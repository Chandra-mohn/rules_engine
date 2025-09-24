from typing import Dict, Any, List, Optional, Tuple
from sqlalchemy import or_
from datetime import datetime
from models import db, Rule, RuleHistory, SchemaEntity, SchemaAttribute
from services.python_rules_engine import PythonRulesEngine
from services.list_cache import ListService
from config import Config
import re
import json

class RuleService:
    """Service class for rule management operations."""
    
    def __init__(self):
        # Use Python ANTLR rules engine instead of Java bridge
        self.rules_engine = PythonRulesEngine()
        self.list_service = ListService()
    
    def parse_rule_name_from_content(self, content: str) -> str:
        """
        Parse rule name from content.
        Supports both quoted and unquoted identifiers:
        - rule "PROMOTION $5%3 @SEARS":
        - rule regularRuleName:

        Args:
            content: Rule content string

        Returns:
            Parsed name or empty string if not found
        """
        if not content:
            return ''

        # Match rules with quoted and unquoted names
        # rule "PROMOTION $5%3 @SEARS":
        # rule regularRuleName:
        pattern = r'^\s*rule\s+(?:"([^"]+)"|([a-zA-Z_][a-zA-Z0-9_]*))\s*:'

        match = re.search(pattern, content, re.MULTILINE)
        if match:
            return match.group(1) or match.group(2) or ''

        return ''


    def get_rules(self, page: int = 1, limit: int = 10, status: Optional[str] = None,
                  search: Optional[str] = None, schema_version: Optional[str] = None,
                  client_id: Optional[int] = None, process_group_id: Optional[int] = None,
                  process_area_id: Optional[int] = None, item_type: Optional[str] = None) -> Tuple[List[Rule], int]:
        """
        Get paginated list of rules with optional filtering.

        Args:
            page: Page number (1-based)
            limit: Number of rules per page
            status: Filter by status (optional)
            search: Search term for name/description (optional)
            schema_version: Filter by schema version (optional)
            client_id: Filter by client (optional)
            process_group_id: Filter by process group (optional)
            process_area_id: Filter by process area (optional)
            item_type: Filter by item type - 'rule', 'actionset', 'action', or None for all types (default: None)

        Returns:
            Tuple of (rules_list, total_count)
        """
        from models import ProcessArea, ProcessGroup, Client
        
        # Start with Rule query and join hierarchy for filtering
        query = Rule.query.join(ProcessArea).join(ProcessGroup).join(Client)

        # Apply item_type filter - if None, show all rule types (exclude actions)
        if item_type:
            query = query.filter(Rule.item_type == item_type)
        else:
            # Default: show all rule types, but not standalone actions
            query = query.filter(Rule.item_type.in_(['rule', 'actionset', 'mon_rule', 'non_mon_rule']))

        # Apply hierarchy filters
        if client_id:
            query = query.filter(Client.id == client_id)
        if process_group_id:
            query = query.filter(ProcessGroup.id == process_group_id)
        if process_area_id:
            query = query.filter(ProcessArea.id == process_area_id)

        # Apply status filter
        if status:
            query = query.filter(Rule.status == status)

        # Apply schema version filter
        if schema_version:
            query = query.filter(Rule.schema_version == schema_version)
        
        # Apply search filter
        if search:
            search_term = f"%{search}%"
            query = query.filter(
                or_(
                    Rule.name.ilike(search_term),
                    Rule.description.ilike(search_term)
                )
            )
        
        # Get total count before pagination
        total = query.count()
        
        # Apply pagination
        rules = query.order_by(Rule.updated_at.desc()).offset((page - 1) * limit).limit(limit).all()
        
        return rules, total
    
    def get_rule_by_id(self, rule_id: int) -> Optional[Rule]:
        """Get a rule by its ID."""
        return Rule.query.get(rule_id)
    
    def get_rule_by_name(self, name: str) -> Optional[Rule]:
        """Get a rule by its name."""
        return Rule.query.filter_by(name=name).first()
    
    def create_rule(self, data: Dict[str, Any], created_by: str = 'system') -> Tuple[Rule, Dict[str, Any]]:
        """
        Create a new rule with validation.
        
        Args:
            data: Rule data (description, content, status, schema_version)
                 Note: 'name' is now parsed from content, not provided separately
            created_by: User who created the rule
            
        Returns:
            Tuple of (created_rule, validation_result)
        """
        # Parse rule name from content
        parsed_name = self.parse_rule_name_from_content(data['content'])
        if not parsed_name:
            return None, {
                'valid': False,
                'message': 'Could not parse name from content. Must start with "rule name:"',
                'errors': ['Rule name not found in content']
            }

        # Validate rule content using Python ANTLR engine
        validation_result = self.rules_engine.validate_rule(data['content'])
        
        # Handle dates
        effective_date = None
        expiry_date = None
        if 'effective_date' in data:
            from datetime import datetime
            effective_date = datetime.fromisoformat(data['effective_date']).date()
        if 'expiry_date' in data and data['expiry_date']:
            from datetime import datetime
            expiry_date = datetime.fromisoformat(data['expiry_date']).date()
        
        # Determine initial status (auto-promote to VALID if validation passes)
        initial_status = 'DRAFT'
        if validation_result.get('valid', False):
            initial_status = 'VALID'
        
        # Create rule with parsed name
        rule = Rule(
            process_area_id=data['process_area_id'],
            name=parsed_name,
            description=data.get('description', ''),
            content=data['content'],
            item_type=data.get('item_type', 'rule'),
            status=initial_status,
            effective_date=effective_date,
            expiry_date=expiry_date,
            schema_version=data.get('schema_version', 'modern'),
            created_by=created_by,
            updated_by=created_by,
            # validation_status removed - status now handles validation + lifecycle
            # Status set to VALID if syntax correct, otherwise stays DRAFT (default)
            validation_message=validation_result['message']
        )
        
        # Set status based on validation result
        if validation_result['valid']:
            rule.status = 'VALID'  # Promote to VALID if syntax is correct
        # else stays DRAFT (default)

        db.session.add(rule)
        db.session.commit()
        
        # Create initial history entry
        self._create_history_entry(rule, created_by, 'Initial creation')
        
        return rule, validation_result
    
    def promote_rule_status(self, rule_id: int, target_status: str, reason: str = '', updated_by: str = 'system') -> bool:
        """
        Promote rule to a new status.
        
        Args:
            rule_id: ID of the rule to promote
            target_status: Target status (DRAFT, VALID, PEND, SCHD, PROD)
            reason: Reason for status change
            updated_by: User who updated the status
            
        Returns:
            True if successful, False otherwise
        """
        try:
            rule = self.get_rule_by_id(rule_id)
            if not rule:
                return False
            
            old_status = rule.status
            rule.status = target_status
            rule.updated_by = updated_by
            rule.version += 1
            
            db.session.commit()
            
            # Create history entry for status change
            self._create_history_entry(
                rule, 
                updated_by, 
                f'Status changed from {old_status} to {target_status}. {reason}'.strip()
            )
            
            return True
            
        except Exception as e:
            db.session.rollback()
            return False
    
    def check_rule_uniqueness(self, process_area_id: int, name: str, effective_date: str, exclude_id: Optional[int] = None) -> bool:
        """
        Check if a rule with the same process_area_id, name, and effective_date already exists.
        
        Args:
            process_area_id: Process area ID
            name: Rule name
            effective_date: Effective date
            exclude_id: Rule ID to exclude from check (for updates)
            
        Returns:
            True if unique, False if duplicate exists
        """
        from datetime import datetime
        
        query = Rule.query.filter(
            Rule.process_area_id == process_area_id,
            Rule.name == name,
            Rule.effective_date == datetime.fromisoformat(effective_date).date()
        )
        
        if exclude_id:
            query = query.filter(Rule.id != exclude_id)
            
        existing = query.first()
        return existing is None
    
    def update_rule(self, rule_id: int, data: Dict[str, Any], updated_by: str = 'system') -> Tuple[Optional[Rule], Dict[str, Any]]:
        """
        Update an existing rule with validation.
        
        Args:
            rule_id: ID of the rule to update
            data: Updated rule data
            updated_by: User who updated the rule
            
        Returns:
            Tuple of (updated_rule, validation_result)
        """
        rule = self.get_rule_by_id(rule_id)
        if not rule:
            return None, {'valid': False, 'message': 'Rule not found', 'errors': ['Rule not found']}
        
        # Store old content for history
        old_content = rule.content
        old_name = rule.name
        
        # Validate content - either new content or current content for DRAFT rules
        validation_result = {'valid': True, 'message': 'No content changes', 'errors': []}
        content_to_validate = None

        if 'content' in data and data['content'] != rule.content:
            # New content provided - validate it
            content_to_validate = data['content']
            validation_result = self.validate_rule_content(data['content'], data.get('schema_version', 'modern'), resolve_lists=True)
        elif rule.status == 'DRAFT' and 'status' not in data:
            # DRAFT rule without explicit status change - validate current content for potential auto-promotion
            content_to_validate = rule.content
            validation_result = self.validate_rule_content(rule.content, rule.schema_version or 'modern', resolve_lists=True)
        
        # Parse name from content if content changed
        if 'content' in data:
            parsed_name = self.parse_rule_name_from_content(data['content'])
            if not parsed_name:
                return None, {
                    'valid': False,
                    'message': 'Could not parse rule name from content. Rule must start with "rule name:"',
                    'errors': ['Rule name not found in content']
                }
            rule.name = parsed_name
            rule.content = data['content']
            
        # Update other rule fields
        if 'description' in data:
            rule.description = data['description']
        if 'effective_date' in data:
            from datetime import datetime
            rule.effective_date = datetime.fromisoformat(data['effective_date']).date()
        if 'expiry_date' in data:
            from datetime import datetime
            rule.expiry_date = datetime.fromisoformat(data['expiry_date']).date() if data['expiry_date'] else None
        if 'process_area_id' in data:
            rule.process_area_id = data['process_area_id']
        if 'schema_version' in data:
            rule.schema_version = data['schema_version']

        # Handle explicit status changes from UI
        if 'status' in data:
            valid_statuses = ['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD', 'deleted']
            if data['status'] in valid_statuses:
                rule.status = data['status']

        # Handle status changes with auto-promotion logic (only if status not explicitly set)
        elif rule.status == 'DRAFT' and validation_result.get('valid', False):
            # Auto-promote to VALID if validation passes (either new content or re-validation of existing content)
            rule.status = 'VALID'
        
        rule.updated_by = updated_by

        # Only override status for validation failures if content was changed (not explicit status updates)
        if 'content' in data and not validation_result['valid'] and 'status' not in data:
            rule.status = 'DRAFT'  # Invalid syntax goes back to DRAFT only if no explicit status change

        rule.validation_message = validation_result['message']
        rule.version += 1
        
        db.session.commit()
        
        # Create history entry if content changed
        if 'content' in data and data['content'] != old_content:
            change_reason = data.get('change_reason', 'Rule content updated')
            self._create_history_entry(rule, updated_by, change_reason)
        
        return rule, validation_result
    
    def delete_rule(self, rule_id: int, deleted_by: str = 'system') -> bool:
        """
        Delete a rule (soft delete by changing status).
        
        Args:
            rule_id: ID of the rule to delete
            deleted_by: User who deleted the rule
            
        Returns:
            True if deleted successfully, False if rule not found
        """
        rule = self.get_rule_by_id(rule_id)
        if not rule:
            return False
        
        # Soft delete by changing status
        rule.status = 'deleted'
        rule.updated_by = deleted_by
        
        db.session.commit()
        
        # Create history entry
        self._create_history_entry(rule, deleted_by, 'Rule deleted')
        
        return True
    
    def validate_rule_content(self, content: str, schema_version: str = 'modern', resolve_lists: bool = False) -> Dict[str, Any]:
        """
        Enhanced validation method for rules with action/ActionSet checking.

        Args:
            content: Rule content to validate
            schema_version: Schema version for list compatibility (default: 'modern')
            resolve_lists: Whether to resolve named lists before validation (default: False)

        Returns:
            Validation result dictionary
        """
        try:
            # Resolve named lists if requested
            content_to_validate = content
            if resolve_lists:
                content_to_validate = self.list_service.resolve_rule_lists(content, schema_version)

            # Enhanced validation with action checking
            validation_errors = []

            # Create validation context for ANTLR
            available_actions = self._get_all_available_actions()
            available_attributes = self._get_all_available_attributes()
            validation_context = {
                'available_actions': available_actions,
                'available_attributes': available_attributes
            }

            # 1. Extract and validate actions from rule content
            action_validation = self._validate_actions_in_rule(content_to_validate)
            if not action_validation['valid']:
                validation_errors.extend(action_validation['errors'])

            # 2. ANTLR-based syntax and semantic validation
            antlr_validation = self.rules_engine.validate_rule(content_to_validate, validation_context)
            if not antlr_validation['valid']:
                # Convert ANTLR error objects to strings for consistent frontend handling
                for error in antlr_validation['errors']:
                    if isinstance(error, dict):
                        validation_errors.append(error.get('message', str(error)))
                    else:
                        validation_errors.append(str(error))
            # Include ANTLR warnings as well (also convert to strings)
            if antlr_validation.get('warnings'):
                for warning in antlr_validation['warnings']:
                    if isinstance(warning, dict):
                        validation_errors.append(warning.get('message', str(warning)))
                    else:
                        validation_errors.append(str(warning))

            # 3. Return combined results with validation details
            is_valid = len(validation_errors) == 0

            # Create detailed validation summary
            validation_details = {
                'syntax_check': 'passed' if antlr_validation.get('valid', False) else 'failed',
                'semantic_check': 'passed' if len(antlr_validation.get('undefined_actions', [])) == 0 and len(antlr_validation.get('undefined_attributes', [])) == 0 else 'warnings',
                'action_validation': 'passed' if action_validation.get('valid', False) else 'failed',
                'rule_info': antlr_validation.get('rule_info', {}),
                'checks_performed': [
                    'ANTLR grammar syntax validation',
                    'Semantic analysis (actions, attributes)',
                    'Action/ActionSet existence validation',
                    'Rule structure validation'
                ]
            }

            success_message = f"✅ Syntax validation: {validation_details['syntax_check']} | ✅ Semantic validation: {validation_details['semantic_check']} | ✅ Action validation: {validation_details['action_validation']}"

            return {
                'valid': is_valid,
                'message': success_message if is_valid else 'Rule validation failed',
                'errors': validation_errors,
                'warnings': [
                    warning.get('message', str(warning)) if isinstance(warning, dict) else str(warning)
                    for warning in antlr_validation.get('warnings', [])
                ],
                'validation_details': validation_details
            }

        except Exception as e:
            return {
                'valid': False,
                'message': f'Validation failed: {str(e)}',
                'errors': [str(e)],
                'warnings': []
            }

    def _validate_actions_in_rule(self, content: str) -> Dict[str, Any]:
        """
        Validate that all actions in rule content exist in rules table.

        Args:
            content: Rule content to validate

        Returns:
            Validation result with any action errors
        """
        try:
            # Extract actions from rule content
            actions = self._extract_actions_from_rule(content)

            # Get known actions and actionsets from rules table
            known_items = self._get_known_actions_and_actionsets()

            # Validate each action
            errors = []
            for action in actions:
                if action not in known_items:
                    errors.append(f"Unknown action/actionset: '{action}'. Available items: {', '.join(sorted(known_items.keys()))}")

            return {
                'valid': len(errors) == 0,
                'errors': errors
            }

        except Exception as e:
            return {
                'valid': False,
                'errors': [f"Action validation failed: {str(e)}"]
            }

    def _extract_actions_from_rule(self, content: str) -> list:
        """Extract action names from rule content."""
        import re
        actions = []

        # Find actions in "then" clauses and standalone actions
        lines = content.split('\n')
        for line in lines:
            stripped = line.strip()
            if not stripped or stripped.startswith('#') or stripped.startswith('rule'):
                continue

            # Extract actions from "then" clauses
            if ' then ' in stripped:
                then_part = stripped.split(' then ', 1)[1]
                action_texts = [a.strip() for a in then_part.split(',')]
                for action_text in action_texts:
                    action_name = self._parse_action_name(action_text)
                    if action_name:
                        actions.append(action_name)

            # Extract actions from "else" clauses
            elif stripped.startswith('else '):
                else_part = stripped[5:]  # Remove 'else '
                action_texts = [a.strip() for a in else_part.split(',')]
                for action_text in action_texts:
                    action_name = self._parse_action_name(action_text)
                    if action_name:
                        actions.append(action_name)

            # Extract standalone actions
            elif not stripped.startswith('if') and not stripped.startswith('else'):
                action_texts = [a.strip() for a in stripped.split(',')]
                for action_text in action_texts:
                    action_name = self._parse_action_name(action_text)
                    if action_name:
                        actions.append(action_name)

        return list(set(actions))  # Remove duplicates

    def _parse_action_name(self, action_text: str) -> str:
        """Parse action name from action text, handling parameters."""
        action_text = action_text.strip().strip('"')

        # Handle parameterized actions: actionName("param1", param2)
        if '(' in action_text:
            return action_text.split('(')[0].strip()

        return action_text

    def _get_known_actions_and_actionsets(self) -> Dict[str, str]:
        """Get all known actions and actionsets from rules table.

        Returns empty dict if database is unavailable - this will cause
        validation warnings which is the correct behavior when actions
        are not properly stored in the database.
        """
        try:
            # Query for actions and actionsets using SQLAlchemy
            results = Rule.query.filter(
                Rule.item_type.in_(['action', 'actionset']),
                Rule.status == 'VALID'
            ).with_entities(Rule.name, Rule.item_type, Rule.description).all()

            return {
                result.name: f"{result.item_type} - {result.description or 'No description'}"
                for result in results
            }

        except Exception as e:
            print(f"Error: Could not load actions from database: {e}")
            print("This will cause validation warnings for all actions - ensure database is accessible")
            return {}  # Return empty dict - this will trigger validation warnings as intended

    def _validate_rule_syntax(self, content: str) -> Dict[str, Any]:
        """
        Validate rule syntax using Python AST-based parsing.

        Args:
            content: Rule content to validate

        Returns:
            Validation result dictionary
        """
        try:
            import re
            errors = []
            warnings = []

            # Parse rule content line by line for basic syntax validation
            lines = content.strip().split('\n')
            if not lines:
                return {'valid': False, 'errors': ['Rule content cannot be empty'], 'warnings': []}

            # Check rule header
            first_line = lines[0].strip()
            if not re.match(r'^rule\s+\w+:', first_line, re.IGNORECASE):
                errors.append("Rule must start with 'rule <name>:'")

            # Track rule structure
            has_if_clause = False
            has_then_clause = False
            in_rule_body = False

            for i, line in enumerate(lines[1:], 2):  # Start from line 2
                stripped = line.strip()

                # Skip empty lines and comments
                if not stripped or stripped.startswith('#'):
                    continue

                in_rule_body = True

                # Check for if clauses
                if re.match(r'^if\s+.+\s+then\s+.+', stripped, re.IGNORECASE):
                    has_if_clause = True
                    has_then_clause = True

                    # Validate if-then structure
                    if ' then ' not in stripped.lower():
                        errors.append(f"Line {i}: 'if' clause missing 'then' keyword")

                # Check for standalone then clauses
                elif re.match(r'^then\s+.+', stripped, re.IGNORECASE):
                    if not has_if_clause:
                        errors.append(f"Line {i}: 'then' clause without preceding 'if' clause")
                    has_then_clause = True

                # Check for else clauses
                elif re.match(r'^else\s+.+', stripped, re.IGNORECASE):
                    if not has_if_clause:
                        errors.append(f"Line {i}: 'else' clause without preceding 'if' clause")

                # Check for standalone actions (should be valid action names)
                elif not any(keyword in stripped.lower() for keyword in ['if ', 'then ', 'else ', 'rule ']):
                    # This could be a standalone action or action list - basic validation
                    # Pattern supports: action, "quoted action", action1, action2, action("param")
                    action_list_pattern = r'^(?:[a-zA-Z_]\w*(?:\s*\([^)]*\))?|"[^"]+")(?:\s*,\s*(?:[a-zA-Z_]\w*(?:\s*\([^)]*\))?|"[^"]+"))*\s*$'
                    if not re.match(action_list_pattern, stripped):
                        warnings.append(f"Line {i}: Possible syntax issue in action: '{stripped}'")

            # Ensure rule has some executable content
            if in_rule_body and not has_then_clause:
                errors.append("Rule must have at least one 'then' clause or standalone action")

            return {
                'valid': len(errors) == 0,
                'errors': errors,
                'warnings': warnings
            }

        except Exception as e:
            return {
                'valid': False,
                'errors': [f"Syntax validation failed: {str(e)}"],
                'warnings': []
            }

    def test_rule(self, content: str, test_data: Dict[str, Any]) -> Dict[str, Any]:
        """Test rule execution with sample data using AST-based hot compilation only."""
        # Use AST-based hot compilation - no fallback to legacy
        hot_result = self.rules_engine.test_rule(content, test_data)

        if hot_result['success']:
            # Transform hot compilation result to match expected format
            result = hot_result['result']
            performance = hot_result.get('performance', {})

            return {
                'success': True,
                'result': {
                    'matched': result.get('matched', False),
                    'actions': result.get('actions', []),
                    'finalAction': result.get('finalAction', None),
                    'executionTimeMs': result.get('executionTimeMs', 0),
                    'ruleCompiled': result.get('ruleCompiled', False),
                    'className': result.get('className', ''),
                    'ruleId': result.get('ruleId', '')
                },
                'performance': {
                    'compilationTimeMs': performance.get('compilationTimeMs', 0),
                    'executionTimeMs': performance.get('executionTimeMs', 0),
                    'totalTimeMs': performance.get('totalTimeMs', 0),
                    'method': 'ast_compilation'
                },
                'errors': []
            }
        else:
            # No fallback - return compilation/execution errors
            return {
                'success': False,
                'result': {},
                'errors': hot_result.get('errors', ['AST compilation failed']),
                'performance': {
                    'compilationTimeMs': hot_result.get('performance', {}).get('compilationTimeMs', 0),
                    'executionTimeMs': 0,
                    'totalTimeMs': hot_result.get('performance', {}).get('totalTimeMs', 0),
                    'method': 'ast_compilation_failed'
                }
            }
    
    def get_rule_history(self, rule_id: int) -> List[RuleHistory]:
        """Get history of changes for a rule."""
        return RuleHistory.query.filter_by(rule_id=rule_id).order_by(RuleHistory.created_at.desc()).all()
    
    def revert_rule(self, rule_id: int, version: int, reverted_by: str = 'system') -> Tuple[Optional[Rule], Dict[str, Any]]:
        """
        Revert a rule to a previous version.
        
        Args:
            rule_id: ID of the rule to revert
            version: Version number to revert to
            reverted_by: User who performed the revert
            
        Returns:
            Tuple of (reverted_rule, validation_result)
        """
        rule = self.get_rule_by_id(rule_id)
        if not rule:
            return None, {'valid': False, 'message': 'Rule not found', 'errors': ['Rule not found']}
        
        # Find the history entry for the target version
        history_entry = RuleHistory.query.filter_by(rule_id=rule_id, version=version).first()
        if not history_entry:
            return None, {'valid': False, 'message': 'Version not found', 'errors': ['Version not found']}
        
        # Validate the historical content
        validation_result = self.rules_engine.validate_rule(history_entry.content)
        
        # Update rule with historical content
        rule.name = history_entry.name
        rule.content = history_entry.content
        rule.updated_by = reverted_by
        # Consolidated: status now handles both validation and lifecycle
        if not validation_result['valid']:
            rule.status = 'DRAFT'  # Invalid syntax goes back to DRAFT
        rule.validation_message = validation_result['message']
        rule.version += 1
        
        db.session.commit()
        
        # Create history entry for the revert
        change_reason = f'Reverted to version {version}'
        self._create_history_entry(rule, reverted_by, change_reason)
        
        return rule, validation_result
    
    def get_all_suggestions(self) -> Dict[str, Any]:
        """
        Get all autocomplete suggestions for caching on the frontend.
        
        Returns:
            Dict with all suggestion categories and metadata
        """
        # Import centralized schema configuration
        from schema.rules_schema import (
            get_all_attributes, get_all_actions, get_all_functions,
            get_attributes_by_entity, KEYWORDS, OPERATORS, TIME_UNITS
        )
        
        # Get data once to avoid duplicates
        all_attributes = get_all_attributes()
        all_actions = get_all_actions()
        all_functions = get_all_functions()
        
        # Build comprehensive suggestions cache
        suggestions_cache = {
            'attributes': {
                'all': all_attributes,
                'by_entity': {
                    'applicant': get_attributes_by_entity('applicant'),
                    'transaction': get_attributes_by_entity('transaction'),
                    'account': get_attributes_by_entity('account')
                }
            },
            'actions': {
                'all': all_actions,
                'modern': [action for action in all_actions if not action['label'].isupper()],
                'legacy': [action for action in all_actions if action['label'].isupper()]
            },
            'functions': {
                'all': all_functions,
                'by_category': {}
            },
            'keywords': [
                {'label': keyword, 'kind': 'keyword', 'detail': 'Keyword'}
                for keyword in KEYWORDS
            ],
            'operators': [
                {'label': op, 'kind': 'operator', 'detail': 'Operator'}
                for op in OPERATORS
            ],
            'time_units': [
                {'label': unit, 'kind': 'keyword', 'detail': 'Time unit'}
                for unit in TIME_UNITS
            ],
            'metadata': {
                'version': '1.0',
                'timestamp': datetime.utcnow().isoformat(),
                'total_count': 0
            }
        }
        
        # Group functions by category
        function_categories = {}
        for func in all_functions:
            category = func.get('category', 'general')
            if category not in function_categories:
                function_categories[category] = []
            function_categories[category].append(func)
        suggestions_cache['functions']['by_category'] = function_categories
        
        # Calculate total count for metadata
        total_count = (
            len(suggestions_cache['attributes']['all']) +
            len(suggestions_cache['actions']['all']) +
            len(suggestions_cache['functions']['all']) +
            len(suggestions_cache['keywords']) +
            len(suggestions_cache['operators']) +
            len(suggestions_cache['time_units'])
        )
        suggestions_cache['metadata']['total_count'] = total_count
        
        return suggestions_cache
    
    def get_autocomplete_suggestions(self, context: str, position: int) -> Dict[str, Any]:
        """
        Get autocomplete suggestions for rule editing.
        
        Args:
            context: The current rule content context
            position: Cursor position in the context
            
        Returns:
            Dict with suggestions: {'suggestions': list}
        """
        return self.rules_engine.get_autocomplete_suggestions(context, position)
    
    def _create_history_entry(self, rule: Rule, created_by: str, change_reason: str):
        """Create a history entry for a rule change."""
        history = RuleHistory(
            rule_id=rule.id,
            name=rule.name,
            content=rule.content,
            version=rule.version,
            created_by=created_by,
            change_reason=change_reason
        )
        
        db.session.add(history)
        db.session.commit()
    
    
    def generate_production_artifacts(self, rule_id: str, rule_name: str, rule_content: str, package_name: str, item_type: str = 'rule') -> Dict[str, Any]:
        """
        Generate production deployment artifacts for a rule.
        Creates a library JAR with Java source, pom.xml, and Dockerfile.
        Saves files to disk for version control.

        Args:
            rule_id: Unique rule identifier
            rule_name: Human-readable rule name
            rule_content: Rule DSL content
            package_name: Java package name for generated code
            item_type: Rule type (rule, actionset, mon_rule, non_mon_rule)

        Returns:
            Dictionary with success status, file paths, and metadata
        """
        import os
        from pathlib import Path
        
        try:
            # Get schema-aware type information
            schema_context = self._get_schema_context()
            
            # Generate schema-aware Java code directly
            try:
                java_code = self._generate_typed_java_code(rule_content, schema_context)
            except Exception as schema_error:
                # Fallback to regular generation if schema-aware generation fails
                java_code_result = self.rules_engine.compile_rule(rule_content)
                if not java_code_result.get('success'):
                    return {
                        'success': False,
                        'message': f'Java code generation failed: {java_code_result.get("message", "Unknown error")}'
                    }
                java_code = java_code_result.get('javaCode', '')
            
            # Generate artifacts in memory first
            artifacts = self._generate_rule_library_artifacts(
                rule_id, rule_name, rule_content, package_name, java_code, item_type
            )
            
            # Create output directory for generated code
            safe_rule_id = str(rule_id).replace('-', '_').replace(' ', '_')
            output_dir = Path.cwd().parent / 'generated-rules' / f'rule-{safe_rule_id}'
            output_dir.mkdir(parents=True, exist_ok=True)
            
            # Write all artifacts to disk
            file_paths = {}
            for relative_path, content in artifacts.items():
                file_path = output_dir / relative_path
                file_path.parent.mkdir(parents=True, exist_ok=True)
                
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                
                # Store absolute path for reference
                file_paths[relative_path] = str(file_path)
            
            # Return artifacts and file locations
            return {
                'success': True,
                'message': f'Production artifacts generated and saved to {output_dir}',
                'files': artifacts,  # File contents for UI display
                'filePaths': file_paths,  # Actual disk locations
                'outputDirectory': str(output_dir),
                'ruleId': rule_id,
                'ruleName': rule_name,
                'packageName': package_name,
                'artifactCount': len(artifacts)
            }
            
        except Exception as e:
            return {
                'success': False,
                'message': f'Production artifact generation failed: {str(e)}'
            }
    
    def _generate_rule_library_artifacts(self, rule_id: str, rule_name: str, rule_content: str,
                                       package_name: str, java_code: str, item_type: str = 'rule') -> Dict[str, str]:
        """Generate all production artifacts for a rule library."""

        # Implement proper naming conventions
        safe_rule_id = str(rule_id).replace('-', '_').replace(' ', '_').lower()
        safe_package_path = package_name.replace('.', '/')

        # Generate PascalCase class name from rule name with type suffix
        class_name = self._to_pascal_case(rule_name) + self._get_class_suffix(item_type)

        artifacts = {}

        # 1. Generated Java source file (rule-type-specific template)
        artifacts[f'src/main/java/{safe_package_path}/{class_name}.java'] = self._generate_rule_type_specific_wrapper(
            package_name, class_name, java_code, rule_name, rule_content, item_type
        )
        
        # 2. Maven pom.xml for library packaging
        artifacts['pom.xml'] = self._generate_rule_library_pom(rule_id, rule_name)
        
        # 3. README.md with usage instructions
        artifacts['README.md'] = self._generate_rule_library_readme(rule_id, rule_name, class_name, package_name)
        
        # 4. Dockerfile for containerizing the rule engine service
        artifacts['Dockerfile'] = self._generate_rule_service_dockerfile()
        
        # 5. Rule metadata file
        artifacts['rule-metadata.json'] = self._generate_rule_metadata(rule_id, rule_name, rule_content)
        
        return artifacts
    
    def _generate_rule_java_wrapper(self, package_name: str, class_name: str, 
                                   java_code: str, rule_name: str, rule_content: str) -> str:
        """Generate Java wrapper class for the rule."""
        return f"""package {package_name};

import com.rules.context.RuleContext;
import com.rules.runtime.RuleResult;

/**
 * Generated rule library for: {rule_name}
 * Rule ID: {class_name}
 * 
 * Original rule content:
 * {rule_content}
 * 
 * This is a library JAR - designed to be loaded into a main rules engine service.
 * Not intended as a standalone microservice.
 */
public class {class_name} {{
    
    private static final String RULE_ID = "{class_name.lower()}";
    private static final String RULE_NAME = "{rule_name}";
    
    /**
     * Execute this rule with the provided context.
     * 
     * @param context Rule execution context
     * @return Rule execution result
     */
    public static RuleResult execute(RuleContext context) {{
        try {{
{self._indent_java_code(java_code, 12)}
        }} catch (Exception e) {{
            return RuleResult.error("Rule execution failed: " + e.getMessage());
        }}
    }}
    
    /**
     * Get the rule ID.
     */
    public static String getRuleId() {{
        return RULE_ID;
    }}
    
    /**
     * Get the human-readable rule name.
     */
    public static String getRuleName() {{
        return RULE_NAME;
    }}
}}"""
    
    def _generate_rule_library_pom(self, rule_id: str, rule_name: str) -> str:
        """Generate Maven pom.xml for rule library."""
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>com.rules.libraries</groupId>
    <artifactId>{str(rule_id).lower().replace('_', '-')}-rule</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>

    <name>{rule_name} Rule Library</name>
    <description>Generated rule library for {rule_name}</description>

    <properties>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <dependencies>
        <!-- Rules engine runtime dependency -->
        <dependency>
            <groupId>com.rules</groupId>
            <artifactId>rules-runtime</artifactId>
            <version>1.0.0</version>
        </dependency>
        
        <!-- JSON processing -->
        <dependency>
            <groupId>org.json</groupId>
            <artifactId>json</artifactId>
            <version>20240303</version>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.11.0</version>
                <configuration>
                    <source>17</source>
                    <target>17</target>
                </configuration>
            </plugin>
            
            <!-- Create JAR with dependencies for easy deployment -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-assembly-plugin</artifactId>
                <version>3.6.0</version>
                <configuration>
                    <descriptorRefs>
                        <descriptorRef>jar-with-dependencies</descriptorRef>
                    </descriptorRefs>
                </configuration>
                <executions>
                    <execution>
                        <id>make-assembly</id>
                        <phase>package</phase>
                        <goals>
                            <goal>single</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>
        </plugins>
    </build>
</project>"""
    
    def _generate_rule_library_readme(self, rule_id: str, rule_name: str, 
                                    class_name: str, package_name: str) -> str:
        """Generate README for the rule library."""
        return f"""# {rule_name} Rule Library

This is a generated rule library JAR for rule: **{rule_name}** (ID: `{rule_id}`)

## Usage

This library is designed to be loaded into a main rules engine service, not run as a standalone microservice.

### Building the Library

```bash
mvn clean package
```

This will create:
- `target/{str(rule_id).lower().replace('_', '-')}-rule-1.0.0.jar` - Main library JAR
- `target/{str(rule_id).lower().replace('_', '-')}-rule-1.0.0-jar-with-dependencies.jar` - JAR with all dependencies

### Integration

Add this library to your main rules engine service classpath and invoke:

```java
import {package_name}.{class_name};
import com.rules.context.RuleContext;
import com.rules.runtime.RuleResult;

// Execute the rule
RuleContext context = new RuleContext(jsonData);
RuleResult result = {class_name}.execute(context);

// Check result
if (result.wasExecuted()) {{
    System.out.println("Rule executed successfully");
    if (result.hasActions()) {{
        System.out.println("Actions: " + result.getActions());
    }}
}}
```

### Deployment

1. Build the JAR: `mvn clean package`
2. Copy the JAR to your rules engine service
3. Add it to the classpath
4. Register the rule with your engine

### Architecture

This follows the **library JAR** pattern rather than microservices:
- ✅ No network overhead between rules
- ✅ Easy to manage thousands of rules
- ✅ Simple deployment and versioning
- ✅ Centralized rule execution engine

## Generated Files

- `src/main/java/.../{class_name}.java` - Rule implementation
- `pom.xml` - Maven build configuration
- `README.md` - This documentation
- `Dockerfile` - Container setup for rules engine service
- `rule-metadata.json` - Rule metadata
"""
    
    def _generate_rule_service_dockerfile(self) -> str:
        """Generate Dockerfile for the rules engine service (not individual rule)."""
        return """# Dockerfile for Rules Engine Service
# This runs the main rules engine that loads library JARs, not individual rule microservices

FROM openjdk:17-jdk-slim

WORKDIR /app

# Copy the main rules engine service JAR
COPY target/rules-engine-service.jar app.jar

# Create directory for rule library JARs
RUN mkdir -p /app/rules

# Copy all rule library JARs
COPY rules/*.jar /app/rules/

# Expose application port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=30s --retries=3 \\
    CMD curl -f http://localhost:8080/health || exit 1

# Run the rules engine service
ENTRYPOINT ["java", "-Drules.library.path=/app/rules", "-jar", "app.jar"]
"""
    
    def _generate_rule_metadata(self, rule_id: str, rule_name: str, rule_content: str) -> str:
        """Generate rule metadata JSON."""
        import json
        from datetime import datetime
        
        metadata = {
            "ruleId": rule_id,
            "ruleName": rule_name,
            "ruleContent": rule_content,
            "generatedAt": datetime.utcnow().isoformat() + "Z",
            "deploymentType": "library",
            "version": "1.0.0",
            "description": f"Production library JAR for rule: {rule_name}"
        }
        
        return json.dumps(metadata, indent=2)
    
    def _get_schema_context(self) -> Dict[str, Any]:
        """Get schema context for type-safe Java code generation."""
        schema_entities = SchemaEntity.query.filter_by(is_active=True).all()
        
        schema_context = {
            'entities': {},
            'type_mappings': {
                'number': 'int',
                'string': 'String', 
                'boolean': 'boolean',
                'date': 'LocalDate',
                'datetime': 'LocalDateTime'
            }
        }
        
        for entity in schema_entities:
            entity_info = {
                'name': entity.name,
                'attributes': {}
            }
            
            for attr in entity.attributes:
                attr_info = {
                    'name': attr.name,
                    'data_type': attr.data_type,
                    'java_type': attr.java_type or schema_context['type_mappings'].get(attr.data_type, 'Object'),
                    'min_value': float(attr.min_value) if attr.min_value is not None else None,
                    'max_value': float(attr.max_value) if attr.max_value is not None else None
                }
                
                if attr.allowed_values:
                    try:
                        attr_info['allowed_values'] = json.loads(attr.allowed_values)
                    except:
                        pass
                
                entity_info['attributes'][attr.name] = attr_info
            
            schema_context['entities'][entity.name] = entity_info
        
        return schema_context
    
    def _generate_typed_java_code(self, rule_content: str, schema_context: Dict[str, Any]) -> str:
        """Generate type-safe Java code using database schema information."""
        import re
        
        # Parse rule content to identify attribute accesses
        attribute_pattern = r'(\w+)\.(\w+)'
        attributes_used = re.findall(attribute_pattern, rule_content)
        
        # Generate typed getters and comparison methods
        typed_methods = []
        imports = set(['java.util.Objects'])
        
        for entity_name, attr_name in attributes_used:
            if entity_name in schema_context['entities']:
                entity = schema_context['entities'][entity_name]
                if attr_name in entity['attributes']:
                    attr = entity['attributes'][attr_name]
                    java_type = attr['java_type']
                    
                    if java_type == 'LocalDate':
                        imports.add('java.time.LocalDate')
                    elif java_type == 'LocalDateTime':
                        imports.add('java.time.LocalDateTime')
                    elif java_type == 'BigDecimal':
                        imports.add('java.math.BigDecimal')
                    
                    # Generate typed getter method
                    method_name = f"get{entity_name.capitalize()}{attr_name.capitalize()}"
                    typed_methods.append(f"""
    private {java_type} {method_name}(RuleContext ctx) {{
        Object value = ctx.getValue("{entity_name}.{attr_name}");
        if (value == null) return null;
        {self._generate_type_conversion(java_type, 'value')}
    }}""")
        
        # Generate the main rule logic with type safety
        import_statements = '\n'.join([f'import {imp};' for imp in sorted(imports)])
        method_declarations = '\n'.join(typed_methods)
        
        rule_logic = self._parse_and_generate_rule_logic(rule_content, schema_context)
        
        return f"""{import_statements}

/**
 * Generated rule class with type-safe attribute access
 * Auto-generated - do not modify manually
 */
public class GeneratedRule implements Rule {{
{method_declarations}

    @Override
    public RuleResult execute(RuleContext ctx) {{
{rule_logic}
    }}

    @Override
    public String getRuleName() {{
        return "{self.parse_rule_name_from_content(rule_content)}";
    }}
}}"""
    
    def _generate_type_conversion(self, java_type: str, value_var: str) -> str:
        """Generate appropriate type conversion code."""
        if java_type == 'int':
            return f"""        if ({value_var} instanceof Number) {{
            return ((Number) {value_var}).intValue();
        }}
        try {{
            return Integer.parseInt({value_var}.toString());
        }} catch (NumberFormatException e) {{
            return 0;
        }}"""
        elif java_type == 'String':
            return f"        return {value_var}.toString();"
        elif java_type == 'boolean':
            return f"""        if ({value_var} instanceof Boolean) {{
            return (Boolean) {value_var};
        }}
        return Boolean.parseBoolean({value_var}.toString());"""
        elif java_type == 'LocalDate':
            return f"""        if ({value_var} instanceof LocalDate) {{
            return (LocalDate) {value_var};
        }}
        try {{
            return LocalDate.parse({value_var}.toString());
        }} catch (Exception e) {{
            return null;
        }}"""
        elif java_type == 'BigDecimal':
            return f"""        if ({value_var} instanceof BigDecimal) {{
            return (BigDecimal) {value_var};
        }}
        if ({value_var} instanceof Number) {{
            return BigDecimal.valueOf(((Number) {value_var}).doubleValue());
        }}
        try {{
            return new BigDecimal({value_var}.toString());
        }} catch (Exception e) {{
            return BigDecimal.ZERO;
        }}"""
        else:
            return f"        return ({java_type}) {value_var};"
    
    def _parse_and_generate_rule_logic(self, rule_content: str, schema_context: Dict[str, Any]) -> str:
        """Parse rule content and generate typed Java logic."""
        lines = rule_content.split('\n')
        java_lines = []
        
        for line in lines:
            stripped = line.strip()
            if not stripped or stripped.startswith('#'):
                continue
                
            if stripped.startswith('if '):
                # Parse if condition and convert to typed Java
                condition = stripped[3:].strip()
                if ' then ' in condition:
                    condition_part, action_part = condition.split(' then ', 1)
                    typed_condition = self._convert_condition_to_typed_java(condition_part.strip(), schema_context)
                    java_lines.append(f"        if ({typed_condition}) {{")
                    java_lines.append(f'            return RuleResult.action("{action_part.strip()}");')
                    java_lines.append('        }')
                    java_lines.append('')
        
        java_lines.append('        return RuleResult.noMatch();')
        return '\n'.join(java_lines)
    
    def _convert_condition_to_typed_java(self, condition: str, schema_context: Dict[str, Any]) -> str:
        """Convert rule condition to type-safe Java code."""
        import re
        
        # Replace attribute references with typed method calls
        attribute_pattern = r'(\w+)\.(\w+)'
        
        def replace_attribute(match):
            entity_name = match.group(1)
            attr_name = match.group(2)
            
            if entity_name in schema_context['entities']:
                entity = schema_context['entities'][entity_name]
                if attr_name in entity['attributes']:
                    method_name = f"get{entity_name.capitalize()}{attr_name.capitalize()}"
                    return f"{method_name}(ctx)"
            
            # Fallback to original ctx.getValue call
            return f'ctx.getValue("{entity_name}.{attr_name}")'  
        
        typed_condition = re.sub(attribute_pattern, replace_attribute, condition)
        
        # Convert operators
        typed_condition = typed_condition.replace(' >= ', ' >= ')
        typed_condition = typed_condition.replace(' <= ', ' <= ')
        typed_condition = typed_condition.replace(' < ', ' < ')
        typed_condition = typed_condition.replace(' > ', ' > ')
        typed_condition = typed_condition.replace(' == ', '.equals(')
        
        # Handle equals properly for non-primitive types
        if '.equals(' in typed_condition and not typed_condition.endswith(')'):
            # Find the value after equals and wrap it properly
            equals_pos = typed_condition.find('.equals(')
            before_equals = typed_condition[:equals_pos]
            after_equals = typed_condition[equals_pos + 8:]
            typed_condition = f"Objects.equals({before_equals}, {after_equals})"
        
        return typed_condition
    
    def _to_pascal_case(self, text: str) -> str:
        """Convert text to PascalCase for Java class names."""
        # Remove special characters and split on word boundaries
        import re
        # Split on camelCase boundaries first
        camel_split = re.sub('([a-z0-9])([A-Z])', r'\1 \2', text)
        # Replace non-alphanumeric with spaces
        cleaned = re.sub(r'[^a-zA-Z0-9]', ' ', camel_split)
        # Split and capitalize each word
        words = cleaned.split()
        return ''.join(word.capitalize() for word in words if word)
    
    def _to_camel_case(self, text: str) -> str:
        """Convert text to camelCase for Java method/variable names."""
        pascal = self._to_pascal_case(text)
        return pascal[0].lower() + pascal[1:] if pascal else ""
    
    def _to_snake_case(self, text: str) -> str:
        """Convert text to snake_case for identifiers."""
        import re
        # Insert underscores before capital letters and convert to lowercase
        s1 = re.sub('(.)([A-Z][a-z]+)', r'\\1_\\2', text)
        return re.sub('([a-z0-9])([A-Z])', r'\\1_\\2', s1).lower()
    
    def _indent_java_code(self, code: str, spaces: int) -> str:
        """Indent Java code by the specified number of spaces."""
        indent = " " * spaces
        lines = code.split('\n')
        indented_lines = [indent + line if line.strip() else line for line in lines]
        return '\n'.join(indented_lines)

    def _get_all_available_actions(self) -> List[str]:
        """Get all available actions from the database."""
        try:
            # Get all rules with item_type='action'
            actions = db.session.query(Rule.name).filter(
                Rule.item_type == 'action'
            ).all()

            # Also get actionsets which can be called as actions
            actionsets = db.session.query(Rule.name).filter(
                Rule.item_type == 'actionset'
            ).all()

            action_names = [action.name for action in actions] + [actionset.name for actionset in actionsets]
            return action_names

        except Exception as e:
            print(f"Error loading available actions: {e}")
            return []

    def _get_all_available_attributes(self) -> List[str]:
        """Get all available attributes from the schema."""
        try:
            # Get all schema attributes
            attributes = db.session.query(
                SchemaEntity.name, SchemaAttribute.name
            ).join(
                SchemaAttribute, SchemaEntity.id == SchemaAttribute.entity_id
            ).all()

            # Format as entity.attribute
            attribute_names = [f"{entity_name}.{attr_name}" for entity_name, attr_name in attributes]
            return attribute_names

        except Exception as e:
            print(f"Error loading available attributes: {e}")
            return []

    # ==== Rule-Type-Specific Template Methods ====

    def _get_class_suffix(self, item_type: str) -> str:
        """Get class name suffix based on rule type."""
        suffixes = {
            'rule': 'Rule',
            'actionset': 'ActionSet',
            'mon_rule': 'MonetaryRule',
            'non_mon_rule': 'NonMonetaryRule'
        }
        return suffixes.get(item_type, 'Rule')

    def _generate_rule_type_specific_wrapper(self, package_name: str, class_name: str, java_code: str,
                                           rule_name: str, rule_content: str, item_type: str) -> str:
        """Generate unified React-like template with conditional capabilities."""

        return self._generate_unified_template(package_name, class_name, java_code, rule_name, rule_content, item_type)

    def _generate_unified_template(self, package_name: str, class_name: str, java_code: str,
                                 rule_name: str, rule_content: str, item_type: str) -> str:
        """Generate unified React-like template with conditional capabilities and high-performance shared data structure."""

        # Determine capability flags
        is_monetary = item_type == 'mon_rule'
        is_validation = item_type == 'non_mon_rule'
        is_actionset = item_type == 'actionset'
        is_standard = item_type == 'rule'

        # Build conditional imports
        imports = ['java.util.*', 'java.time.LocalDateTime']
        if is_monetary:
            imports.extend(['java.math.BigDecimal', 'java.math.RoundingMode'])
        if is_validation:
            imports.append('java.util.regex.Pattern')

        import_statements = '\n'.join([f'import {imp};' for imp in imports])

        # High-performance shared data structure imports
        shared_imports = [
            'java.util.concurrent.ConcurrentHashMap',
            'java.util.concurrent.atomic.AtomicLong',
            'java.lang.ref.WeakReference',
            'java.util.concurrent.Executors',
            'java.util.concurrent.ScheduledExecutorService'
        ]
        import_statements += '\n' + '\n'.join([f'import {imp};' for imp in shared_imports])

        # Generate conditional fields for RuleContext
        context_fields = []
        if is_monetary:
            context_fields.extend([
                '        private final Map<String, BigDecimal> calculations;',
                '        private final List<String> auditTrail;',
                '        private final String baseCurrency;'
            ])
        if is_validation:
            context_fields.extend([
                '        private final List<String> validationErrors;',
                '        private final Map<String, Object> transformedData;'
            ])
        if is_actionset:
            context_fields.append('        private final Map<String, Object> sharedState;')

        # Generate conditional constructor parameters
        constructor_params = ['Map<String, Object> entities']
        if is_monetary:
            constructor_params.append('String baseCurrency')

        # Generate conditional constructor initialization
        constructor_init = [
            '            this.entities = new HashMap<>(entities);',
            '            this.executionTrace = new ArrayList<>();'
        ]
        if is_monetary:
            constructor_init.extend([
                '            this.calculations = new HashMap<>();',
                '            this.auditTrail = new ArrayList<>();',
                '            this.baseCurrency = baseCurrency != null ? baseCurrency : "USD";'
            ])
        if is_validation:
            constructor_init.extend([
                '            this.validationErrors = new ArrayList<>();',
                '            this.transformedData = new HashMap<>();'
            ])
        if is_actionset:
            constructor_init.append('            this.sharedState = new HashMap<>();')

        # Generate conditional methods
        conditional_methods = []

        if is_monetary:
            conditional_methods.append('''
        // Monetary-specific methods
        public BigDecimal getMonetaryValue(String path) {
            Object value = getValue(path);
            if (value instanceof Number) {
                auditTrail.add("RETRIEVE: " + path + " = " + value);
                return new BigDecimal(value.toString()).setScale(2, RoundingMode.HALF_UP);
            }
            return BigDecimal.ZERO;
        }

        public RuleContext withCalculation(String key, BigDecimal value) {
            Map<String, BigDecimal> newCalculations = new HashMap<>(calculations);
            newCalculations.put(key, value.setScale(2, RoundingMode.HALF_UP));
            auditTrail.add("CALCULATE: " + key + " = " + value);
            return new RuleContext(entities, baseCurrency).copyWith(newCalculations, auditTrail, baseCurrency);
        }

        public void addAuditEntry(String entry) {
            auditTrail.add(LocalDateTime.now() + ": " + entry);
        }''')

        if is_validation:
            conditional_methods.append('''
        // Validation-specific methods
        public boolean validate(String field, String pattern, String errorMessage) {
            Object value = getValue(field);
            if (value != null && Pattern.matches(pattern, value.toString())) {
                addExecutionStep("VALIDATE: " + field + " - PASSED");
                return true;
            } else {
                validationErrors.add(errorMessage);
                addExecutionStep("VALIDATE: " + field + " - FAILED: " + errorMessage);
                return false;
            }
        }

        public void transform(String key, Object value) {
            transformedData.put(key, value);
            addExecutionStep("TRANSFORM: " + key + " = " + value);
        }

        public void enrichData(String key, Object enrichedValue) {
            transformedData.put(key, enrichedValue);
            addExecutionStep("ENRICH: " + key + " = " + enrichedValue);
        }''')

        if is_actionset:
            conditional_methods.append('''
        // ActionSet-specific methods
        public void setSharedValue(String key, Object value) {
            sharedState.put(key, value);
            addExecutionStep("SET: " + key + " = " + value);
        }

        public Object getSharedValue(String key) {
            return sharedState.get(key);
        }''')

        # Generate result class based on type
        if is_monetary:
            result_class = '''
    public static class RuleResult {
        private final boolean approved;
        private final Map<String, BigDecimal> calculations;
        private final List<String> actions;
        private final List<String> auditTrail;
        private final String currency;

        public RuleResult(boolean approved, Map<String, BigDecimal> calculations,
                        List<String> actions, List<String> auditTrail, String currency) {
            this.approved = approved;
            this.calculations = calculations;
            this.actions = actions;
            this.auditTrail = auditTrail;
            this.currency = currency;
        }

        public boolean isApproved() { return approved; }
        public Map<String, BigDecimal> getCalculations() { return calculations; }
        public List<String> getActions() { return actions; }
        public List<String> getAuditTrail() { return auditTrail; }
        public String getCurrency() { return currency; }
    }'''
        elif is_validation:
            result_class = '''
    public static class RuleResult {
        private final boolean valid;
        private final Map<String, Object> transformedData;
        private final List<String> validationErrors;
        private final List<String> actions;
        private final List<String> executionTrace;

        public RuleResult(boolean valid, Map<String, Object> transformedData,
                        List<String> validationErrors, List<String> actions,
                        List<String> executionTrace) {
            this.valid = valid;
            this.transformedData = transformedData;
            this.validationErrors = validationErrors;
            this.actions = actions;
            this.executionTrace = executionTrace;
        }

        public boolean isValid() { return valid; }
        public Map<String, Object> getTransformedData() { return transformedData; }
        public List<String> getValidationErrors() { return validationErrors; }
        public List<String> getActions() { return actions; }
        public List<String> getExecutionTrace() { return executionTrace; }
    }'''
        elif is_actionset:
            result_class = '''
    public static class RuleResult {
        private final boolean completed;
        private final List<String> executedActions;
        private final Map<String, Object> finalState;
        private final List<String> executionTrace;

        public RuleResult(boolean completed, List<String> executedActions,
                        Map<String, Object> finalState, List<String> executionTrace) {
            this.completed = completed;
            this.executedActions = executedActions;
            this.finalState = finalState;
            this.executionTrace = executionTrace;
        }

        public boolean isCompleted() { return completed; }
        public List<String> getExecutedActions() { return executedActions; }
        public Map<String, Object> getFinalState() { return finalState; }
        public List<String> getExecutionTrace() { return executionTrace; }
    }'''
        else:  # standard rule
            result_class = '''
    public static class RuleResult {
        private final boolean matched;
        private final List<String> actions;
        private final List<String> executionTrace;

        public RuleResult(boolean matched, List<String> actions, List<String> executionTrace) {
            this.matched = matched;
            this.actions = actions;
            this.executionTrace = executionTrace;
        }

        public boolean isMatched() { return matched; }
        public List<String> getActions() { return actions; }
        public List<String> getExecutionTrace() { return executionTrace; }
    }'''

        # Generate evaluation method signature based on type
        if is_monetary:
            eval_method = 'public static RuleResult evaluate(Map<String, Object> input, String currency)'
            eval_overload = '''
    public static RuleResult evaluate(Map<String, Object> input) {
        return evaluate(input, "USD");
    }'''
            eval_context = 'RuleContext context = new RuleContext(input, currency);'
            eval_return = '''return new RuleResult(true, context.calculations, actions,
                                context.auditTrail, context.baseCurrency);'''
        elif is_validation:
            eval_method = 'public static RuleResult process(Map<String, Object> input)'
            eval_overload = ''
            eval_context = 'RuleContext context = new RuleContext(input);'
            eval_return = '''boolean isValid = context.validationErrors.isEmpty();
        return new RuleResult(isValid, context.transformedData, context.validationErrors,
                            actions, context.executionTrace);'''
        elif is_actionset:
            eval_method = 'public static RuleResult execute(Map<String, Object> input)'
            eval_overload = ''
            eval_context = 'RuleContext context = new RuleContext(input);'
            eval_return = '''return new RuleResult(true, actions, context.sharedState, context.executionTrace);'''
        else:  # standard
            eval_method = 'public static RuleResult evaluate(Map<String, Object> input)'
            eval_overload = ''
            eval_context = 'RuleContext context = new RuleContext(input);'
            eval_return = '''return new RuleResult(true, actions, context.executionTrace);'''

        return f'''package {package_name};

{import_statements}

/**
 * {item_type.title()} Rule: {rule_name}
 * Unified React-like template with conditional capabilities
 * Generated at {datetime.now()}
 */
public class {class_name} {{

    // High-Performance Shared Data Structure - React-like Global State Manager
    public static class SharedRuleManager {{
        // Global shared cache for millions of executions
        private static final ConcurrentHashMap<String, WeakReference<Object>> globalCache = new ConcurrentHashMap<>();
        private static final ConcurrentHashMap<String, AtomicLong> executionCounters = new ConcurrentHashMap<>();
        private static final ConcurrentHashMap<String, Map<String, Object>> ruleMetrics = new ConcurrentHashMap<>();

        // React-like state subscription system
        private static final ConcurrentHashMap<String, List<WeakReference<RuleStateListener>>> stateListeners = new ConcurrentHashMap<>();

        // Background cleanup service
        private static final ScheduledExecutorService cleanupExecutor = Executors.newSingleThreadScheduledExecutor();

        static {{
            // Auto-cleanup weak references every 60 seconds
            cleanupExecutor.scheduleAtFixedRate(() -> {{
                globalCache.entrySet().removeIf(entry -> entry.getValue().get() == null);
                stateListeners.forEach((key, listeners) -> listeners.removeIf(ref -> ref.get() == null));
            }}, 60, 60, java.util.concurrent.TimeUnit.SECONDS);
        }}

        // React-like state management
        public static void setState(String key, Object value) {{
            globalCache.put(key, new WeakReference<>(value));
            notifyStateListeners(key, value);
            incrementCounter("state_updates");
        }}

        public static Object getState(String key) {{
            WeakReference<Object> ref = globalCache.get(key);
            return ref != null ? ref.get() : null;
        }}

        public static void subscribeToState(String key, RuleStateListener listener) {{
            stateListeners.computeIfAbsent(key, k -> new ArrayList<>()).add(new WeakReference<>(listener));
        }}

        private static void notifyStateListeners(String key, Object newValue) {{
            List<WeakReference<RuleStateListener>> listeners = stateListeners.get(key);
            if (listeners != null) {{
                listeners.forEach(ref -> {{
                    RuleStateListener listener = ref.get();
                    if (listener != null) listener.onStateChange(key, newValue);
                }});
            }}
        }}

        // Performance metrics for millions of executions
        public static void incrementCounter(String metric) {{
            executionCounters.computeIfAbsent(metric, k -> new AtomicLong(0)).incrementAndGet();
        }}

        public static long getCounter(String metric) {{
            AtomicLong counter = executionCounters.get(metric);
            return counter != null ? counter.get() : 0;
        }}

        public static void recordRuleMetric(String ruleId, String metric, Object value) {{
            ruleMetrics.computeIfAbsent(ruleId, k -> new ConcurrentHashMap<>()).put(metric, value);
        }}

        public static Map<String, Object> getRuleMetrics(String ruleId) {{
            return ruleMetrics.getOrDefault(ruleId, Collections.emptyMap());
        }}

        // Memory-efficient batch processing
        public static <T> List<T> processBatch(List<Map<String, Object>> inputs, java.util.function.Function<Map<String, Object>, T> processor) {{
            return inputs.parallelStream()
                        .map(processor)
                        .collect(java.util.stream.Collectors.toList());
        }}

        // Hot-path optimization for frequent operations
        public static Object getOrCompute(String key, java.util.function.Supplier<Object> supplier) {{
            Object cached = getState(key);
            if (cached == null) {{
                cached = supplier.get();
                setState(key, cached);
            }}
            return cached;
        }}
    }}

    // State change listener interface for reactive updates
    public interface RuleStateListener {{
        void onStateChange(String key, Object newValue);
    }}

    public static class RuleContext {{
        // Universal core (React-like immutable state)
        private final Map<String, Object> entities;
        private final List<String> executionTrace;

        // Conditional capabilities
{chr(10).join(context_fields)}

        public RuleContext({', '.join(constructor_params)}) {{
{chr(10).join(constructor_init)}
        }}

        // Universal methods (React-like patterns)
        public Object getValue(String path) {{
            String[] parts = path.split("\\.");
            Object current = entities.get(parts[0]);
            for (int i = 1; i < parts.length && current != null; i++) {{
                if (current instanceof Map) {{
                    current = ((Map<String, Object>) current).get(parts[i]);
                }}
            }}
            return current;
        }}

        public void addExecutionStep(String step) {{
            executionTrace.add(step);
        }}

        // React-like immutable update pattern with shared state integration
        public RuleContext withUpdate(String key, Object value) {{
            Map<String, Object> newEntities = new HashMap<>(entities);
            newEntities.put(key, value);
            addExecutionStep("UPDATE: " + key + " = " + value);

            // Integrate with SharedRuleManager for global state visibility
            SharedRuleManager.setState("rule.context." + key, value);
            SharedRuleManager.incrementCounter("context_updates");

            return new RuleContext(newEntities{', baseCurrency' if is_monetary else ''});
        }}

        // High-performance batch update for multiple values
        public RuleContext withBatchUpdate(Map<String, Object> updates) {{
            Map<String, Object> newEntities = new HashMap<>(entities);
            updates.forEach((key, value) -> {{
                newEntities.put(key, value);
                addExecutionStep("BATCH_UPDATE: " + key + " = " + value);
                SharedRuleManager.setState("rule.context." + key, value);
            }});
            SharedRuleManager.incrementCounter("batch_updates");
            return new RuleContext(newEntities{', baseCurrency' if is_monetary else ''});
        }}

        // Subscribe to global state changes for reactive rules
        public void subscribeToGlobalState(String key, RuleStateListener listener) {{
            SharedRuleManager.subscribeToState(key, listener);
        }}

        // Get cached or computed value for hot-path optimization
        public Object getOrCompute(String key, java.util.function.Supplier<Object> supplier) {{
            return SharedRuleManager.getOrCompute("rule.computed." + key, supplier);
        }}
{''.join(conditional_methods)}
    }}
{result_class}

    {eval_method} {{
        // High-performance execution tracking
        long startTime = System.nanoTime();
        String ruleId = "{rule_name}";
        SharedRuleManager.incrementCounter("rule_executions");
        SharedRuleManager.incrementCounter("rule_" + ruleId + "_executions");

        {eval_context}
        List<String> actions = new ArrayList<>();

        try {{
            // Generated rule logic with performance monitoring
            {java_code}

            // Record successful execution metrics
            long executionTime = System.nanoTime() - startTime;
            SharedRuleManager.recordRuleMetric(ruleId, "last_execution_ns", executionTime);
            SharedRuleManager.recordRuleMetric(ruleId, "last_execution_ms", executionTime / 1_000_000.0);
            SharedRuleManager.incrementCounter("successful_executions");

            {eval_return}
        }} catch (Exception e) {{
            // Record failure metrics
            SharedRuleManager.incrementCounter("failed_executions");
            SharedRuleManager.recordRuleMetric(ruleId, "last_error", e.getMessage());
            throw e;
        }}
    }}{eval_overload}
}}'''

    def _generate_standard_rule_template(self, package_name: str, class_name: str, java_code: str,
                                       rule_name: str, rule_content: str) -> str:
        """Generate standard rule template with basic condition/action logic."""
        return f'''package {package_name};

import java.util.*;
import java.time.LocalDateTime;

/**
 * Standard Rule: {rule_name}
 * Generated from rule content at {LocalDateTime.now()}
 */
public class {class_name} {{

    public static class RuleContext {{
        private final Map<String, Object> entities;
        private final List<String> executionPath;

        public RuleContext(Map<String, Object> entities) {{
            this.entities = new HashMap<>(entities);
            this.executionPath = new ArrayList<>();
        }}

        public Object getValue(String path) {{
            String[] parts = path.split("\\.");
            Object current = entities.get(parts[0]);
            for (int i = 1; i < parts.length && current != null; i++) {{
                if (current instanceof Map) {{
                    current = ((Map<String, Object>) current).get(parts[i]);
                }}
            }}
            return current;
        }}

        public void addExecutionStep(String step) {{
            executionPath.add(step);
        }}
    }}

    public static class RuleResult {{
        private final boolean matched;
        private final List<String> actions;
        private final List<String> executionPath;

        public RuleResult(boolean matched, List<String> actions, List<String> executionPath) {{
            this.matched = matched;
            this.actions = actions;
            this.executionPath = executionPath;
        }}

        public boolean isMatched() {{ return matched; }}
        public List<String> getActions() {{ return actions; }}
        public List<String> getExecutionPath() {{ return executionPath; }}
    }}

    public static RuleResult evaluate(Map<String, Object> input) {{
        RuleContext context = new RuleContext(input);
        List<String> actions = new ArrayList<>();

        // Generated rule logic
        {java_code}

        return new RuleResult(true, actions, context.executionPath);
    }}
}}'''

    def _generate_actionset_template(self, package_name: str, class_name: str, java_code: str,
                                   rule_name: str, rule_content: str) -> str:
        """Generate actionset template with sequential action execution."""
        return f'''package {package_name};

import java.util.*;
import java.time.LocalDateTime;

/**
 * Actionset: {rule_name}
 * Sequential action execution with shared context
 * Generated at {datetime.now()}
 */
public class {class_name} {{

    public static class ActionContext {{
        private final Map<String, Object> entities;
        private final Map<String, Object> sharedState;
        private final List<String> executionTrace;

        public ActionContext(Map<String, Object> entities) {{
            this.entities = new HashMap<>(entities);
            this.sharedState = new HashMap<>();
            this.executionTrace = new ArrayList<>();
        }}

        public Object getValue(String path) {{
            String[] parts = path.split("\\.");
            Object current = entities.get(parts[0]);
            for (int i = 1; i < parts.length && current != null; i++) {{
                if (current instanceof Map) {{
                    current = ((Map<String, Object>) current).get(parts[i]);
                }}
            }}
            return current;
        }}

        public void setSharedValue(String key, Object value) {{
            sharedState.put(key, value);
            executionTrace.add("SET: " + key + " = " + value);
        }}

        public Object getSharedValue(String key) {{
            return sharedState.get(key);
        }}

        public void addTrace(String action) {{
            executionTrace.add(action);
        }}
    }}

    public static class ActionSetResult {{
        private final boolean completed;
        private final List<String> executedActions;
        private final Map<String, Object> finalState;
        private final List<String> executionTrace;

        public ActionSetResult(boolean completed, List<String> executedActions,
                             Map<String, Object> finalState, List<String> executionTrace) {{
            this.completed = completed;
            this.executedActions = executedActions;
            this.finalState = finalState;
            this.executionTrace = executionTrace;
        }}

        public boolean isCompleted() {{ return completed; }}
        public List<String> getExecutedActions() {{ return executedActions; }}
        public Map<String, Object> getFinalState() {{ return finalState; }}
        public List<String> getExecutionTrace() {{ return executionTrace; }}
    }}

    public static ActionSetResult execute(Map<String, Object> input) {{
        ActionContext context = new ActionContext(input);
        List<String> executedActions = new ArrayList<>();

        // Generated action set logic
        {java_code}

        return new ActionSetResult(true, executedActions, context.sharedState, context.executionTrace);
    }}
}}'''

    def _generate_monetary_rule_template(self, package_name: str, class_name: str, java_code: str,
                                       rule_name: str, rule_content: str) -> str:
        """Generate Monetary Rule template with BigDecimal precision and audit trail."""
        return f'''package {package_name};

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.time.LocalDateTime;

/**
 * Monetary Rule: {rule_name}
 * High-precision financial calculations with audit trail
 * Generated at {datetime.now()}
 */
public class {class_name} {{

    public static class MonetaryContext {{
        private final Map<String, Object> entities;
        private final Map<String, BigDecimal> monetaryCalculations;
        private final List<String> auditTrail;
        private final String baseCurrency;

        public MonetaryContext(Map<String, Object> entities, String baseCurrency) {{
            this.entities = new HashMap<>(entities);
            this.monetaryCalculations = new HashMap<>();
            this.auditTrail = new ArrayList<>();
            this.baseCurrency = baseCurrency != null ? baseCurrency : "USD";
        }}

        public BigDecimal getMonetaryValue(String path) {{
            Object value = getValue(path);
            if (value instanceof Number) {{
                auditTrail.add("RETRIEVE: " + path + " = " + value);
                return new BigDecimal(value.toString()).setScale(2, RoundingMode.HALF_UP);
            }}
            return BigDecimal.ZERO;
        }}

        public void setCalculation(String key, BigDecimal value) {{
            monetaryCalculations.put(key, value.setScale(2, RoundingMode.HALF_UP));
            auditTrail.add("CALCULATE: " + key + " = " + value);
        }}

        public BigDecimal getCalculation(String key) {{
            return monetaryCalculations.getOrDefault(key, BigDecimal.ZERO);
        }}

        private Object getValue(String path) {{
            String[] parts = path.split("\\.");
            Object current = entities.get(parts[0]);
            for (int i = 1; i < parts.length && current != null; i++) {{
                if (current instanceof Map) {{
                    current = ((Map<String, Object>) current).get(parts[i]);
                }}
            }}
            return current;
        }}

        public void addAuditEntry(String entry) {{
            auditTrail.add(LocalDateTime.now() + ": " + entry);
        }}
    }}

    public static class MonetaryResult {{
        private final boolean approved;
        private final Map<String, BigDecimal> calculations;
        private final List<String> actions;
        private final List<String> auditTrail;
        private final String currency;

        public MonetaryResult(boolean approved, Map<String, BigDecimal> calculations,
                            List<String> actions, List<String> auditTrail, String currency) {{
            this.approved = approved;
            this.calculations = calculations;
            this.actions = actions;
            this.auditTrail = auditTrail;
            this.currency = currency;
        }}

        public boolean isApproved() {{ return approved; }}
        public Map<String, BigDecimal> getCalculations() {{ return calculations; }}
        public List<String> getActions() {{ return actions; }}
        public List<String> getAuditTrail() {{ return auditTrail; }}
        public String getCurrency() {{ return currency; }}
    }}

    public static MonetaryResult evaluate(Map<String, Object> input, String currency) {{
        MonetaryContext context = new MonetaryContext(input, currency);
        List<String> actions = new ArrayList<>();

        // Generated monetary rule logic
        {java_code}

        return new MonetaryResult(true, context.monetaryCalculations, actions,
                                context.auditTrail, context.baseCurrency);
    }}

    public static MonetaryResult evaluate(Map<String, Object> input) {{
        return evaluate(input, "USD");
    }}
}}'''

    def _generate_non_monetary_rule_template(self, package_name: str, class_name: str, java_code: str,
                                           rule_name: str, rule_content: str) -> str:
        """Generate Non-Monetary Rule template with data transformation and validation."""
        return f'''package {package_name};

import java.util.*;
import java.time.LocalDateTime;
import java.util.regex.Pattern;

/**
 * Non-Monetary Rule: {rule_name}
 * Data transformation, validation, and enrichment
 * Generated at {datetime.now()}
 */
public class {class_name} {{

    public static class DataContext {{
        private final Map<String, Object> entities;
        private final Map<String, Object> transformedData;
        private final List<String> validationErrors;
        private final List<String> transformationLog;

        public DataContext(Map<String, Object> entities) {{
            this.entities = new HashMap<>(entities);
            this.transformedData = new HashMap<>();
            this.validationErrors = new ArrayList<>();
            this.transformationLog = new ArrayList<>();
        }}

        public Object getValue(String path) {{
            String[] parts = path.split("\\.");
            Object current = entities.get(parts[0]);
            for (int i = 1; i < parts.length && current != null; i++) {{
                if (current instanceof Map) {{
                    current = ((Map<String, Object>) current).get(parts[i]);
                }}
            }}
            return current;
        }}

        public void transform(String key, Object value) {{
            transformedData.put(key, value);
            transformationLog.add("TRANSFORM: " + key + " = " + value);
        }}

        public boolean validate(String field, String pattern, String errorMessage) {{
            Object value = getValue(field);
            if (value != null && Pattern.matches(pattern, value.toString())) {{
                transformationLog.add("VALIDATE: " + field + " - PASSED");
                return true;
            }} else {{
                validationErrors.add(errorMessage);
                transformationLog.add("VALIDATE: " + field + " - FAILED: " + errorMessage);
                return false;
            }}
        }}

        public void enrichData(String key, Object enrichedValue) {{
            transformedData.put(key, enrichedValue);
            transformationLog.add("ENRICH: " + key + " = " + enrichedValue);
        }}

        public Object getTransformed(String key) {{
            return transformedData.get(key);
        }}
    }}

    public static class DataResult {{
        private final boolean valid;
        private final Map<String, Object> transformedData;
        private final List<String> validationErrors;
        private final List<String> actions;
        private final List<String> transformationLog;

        public DataResult(boolean valid, Map<String, Object> transformedData,
                        List<String> validationErrors, List<String> actions,
                        List<String> transformationLog) {{
            this.valid = valid;
            this.transformedData = transformedData;
            this.validationErrors = validationErrors;
            this.actions = actions;
            this.transformationLog = transformationLog;
        }}

        public boolean isValid() {{ return valid; }}
        public Map<String, Object> getTransformedData() {{ return transformedData; }}
        public List<String> getValidationErrors() {{ return validationErrors; }}
        public List<String> getActions() {{ return actions; }}
        public List<String> getTransformationLog() {{ return transformationLog; }}
    }}

    public static DataResult process(Map<String, Object> input) {{
        DataContext context = new DataContext(input);
        List<String> actions = new ArrayList<>();

        // Generated non-monetary rule logic
        {java_code}

        boolean isValid = context.validationErrors.isEmpty();
        return new DataResult(isValid, context.transformedData, context.validationErrors,
                            actions, context.transformationLog);
    }}
}}'''

    def build_rule_code(self, rule_id: int) -> dict:
        """Build the generated Java code using Maven."""
        try:
            rule_dir = os.path.join(self.generated_rules_dir, f"rule-{rule_id}")

            if not os.path.exists(rule_dir):
                return {
                    'success': False,
                    'error': f'Generated code directory not found for rule {rule_id}',
                    'build_log': ''
                }

            # Change to rule directory and run maven build
            import subprocess
            import time

            start_time = time.time()

            try:
                result = subprocess.run(
                    ['mvn', 'clean', 'compile'],
                    cwd=rule_dir,
                    capture_output=True,
                    text=True,
                    timeout=60  # 60 second timeout
                )

                build_time = time.time() - start_time

                return {
                    'success': result.returncode == 0,
                    'build_log': result.stdout + result.stderr,
                    'return_code': result.returncode,
                    'build_time_ms': round(build_time * 1000, 2),
                    'rule_directory': rule_dir
                }

            except subprocess.TimeoutExpired:
                return {
                    'success': False,
                    'error': 'Build timeout after 60 seconds',
                    'build_log': 'Build process exceeded time limit',
                    'build_time_ms': 60000
                }
            except FileNotFoundError:
                return {
                    'success': False,
                    'error': 'Maven not found. Please ensure Maven is installed and in PATH',
                    'build_log': 'mvn command not found'
                }

        except Exception as e:
            return {
                'success': False,
                'error': f'Build failed: {str(e)}',
                'build_log': f'Exception during build: {str(e)}'
            }

    def test_rule_code(self, rule_id: int, test_data: dict = None) -> dict:
        """Test the generated and compiled Java code using Maven."""
        try:
            rule_dir = os.path.join(self.generated_rules_dir, f"rule-{rule_id}")

            if not os.path.exists(rule_dir):
                return {
                    'success': False,
                    'error': f'Generated code directory not found for rule {rule_id}',
                    'test_log': ''
                }

            # First ensure code is compiled
            build_result = self.build_rule_code(rule_id)
            if not build_result['success']:
                return {
                    'success': False,
                    'error': 'Build failed before testing',
                    'test_log': build_result['build_log'],
                    'build_result': build_result
                }

            import subprocess
            import time

            start_time = time.time()

            try:
                # Run maven test
                result = subprocess.run(
                    ['mvn', 'test'],
                    cwd=rule_dir,
                    capture_output=True,
                    text=True,
                    timeout=90  # 90 second timeout for tests
                )

                test_time = time.time() - start_time

                # Parse test results if available
                test_summary = self._parse_maven_test_output(result.stdout)

                return {
                    'success': result.returncode == 0,
                    'test_log': result.stdout + result.stderr,
                    'return_code': result.returncode,
                    'test_time_ms': round(test_time * 1000, 2),
                    'test_summary': test_summary,
                    'rule_directory': rule_dir,
                    'build_result': build_result
                }

            except subprocess.TimeoutExpired:
                return {
                    'success': False,
                    'error': 'Test timeout after 90 seconds',
                    'test_log': 'Test process exceeded time limit',
                    'test_time_ms': 90000
                }
            except FileNotFoundError:
                return {
                    'success': False,
                    'error': 'Maven not found. Please ensure Maven is installed and in PATH',
                    'test_log': 'mvn command not found'
                }

        except Exception as e:
            return {
                'success': False,
                'error': f'Test failed: {str(e)}',
                'test_log': f'Exception during test: {str(e)}'
            }

    def _parse_maven_test_output(self, output: str) -> dict:
        """Parse Maven test output to extract test summary."""
        summary = {
            'tests_run': 0,
            'failures': 0,
            'errors': 0,
            'skipped': 0,
            'time_seconds': 0.0
        }

        try:
            lines = output.split('\n')
            for line in lines:
                if 'Tests run:' in line:
                    # Example: "Tests run: 3, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 0.123 sec"
                    parts = line.split(',')
                    for part in parts:
                        part = part.strip()
                        if part.startswith('Tests run:'):
                            summary['tests_run'] = int(part.split(':')[1].strip())
                        elif part.startswith('Failures:'):
                            summary['failures'] = int(part.split(':')[1].strip())
                        elif part.startswith('Errors:'):
                            summary['errors'] = int(part.split(':')[1].strip())
                        elif part.startswith('Skipped:'):
                            summary['skipped'] = int(part.split(':')[1].strip())
                        elif part.startswith('Time elapsed:'):
                            time_str = part.split(':')[1].strip().split()[0]
                            summary['time_seconds'] = float(time_str)
        except (ValueError, IndexError):
            # If parsing fails, return default values
            pass

        return summary

    def execute_rule_code(self, rule_id: int, input_data: dict) -> dict:
        """Execute the compiled Java rule with provided input data."""
        try:
            rule_dir = os.path.join(self.generated_rules_dir, f"rule-{rule_id}")

            if not os.path.exists(rule_dir):
                return {
                    'success': False,
                    'error': f'Generated code directory not found for rule {rule_id}',
                    'execution_log': ''
                }

            # First ensure code is compiled
            build_result = self.build_rule_code(rule_id)
            if not build_result['success']:
                return {
                    'success': False,
                    'error': 'Build failed before execution',
                    'execution_log': build_result['build_log'],
                    'build_result': build_result
                }

            import subprocess
            import time
            import json
            import tempfile

            # Create temporary file for input data
            with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
                json.dump(input_data, f)
                input_file = f.name

            start_time = time.time()

            try:
                # Execute using maven exec plugin with input data
                result = subprocess.run(
                    ['mvn', 'exec:java', f'-Dexec.args={input_file}'],
                    cwd=rule_dir,
                    capture_output=True,
                    text=True,
                    timeout=30  # 30 second timeout for execution
                )

                execution_time = time.time() - start_time

                # Parse execution output
                execution_result = self._parse_execution_output(result.stdout)

                return {
                    'success': result.returncode == 0,
                    'execution_log': result.stdout + result.stderr,
                    'return_code': result.returncode,
                    'execution_time_ms': round(execution_time * 1000, 3),
                    'execution_result': execution_result,
                    'rule_directory': rule_dir,
                    'input_data': input_data
                }

            except subprocess.TimeoutExpired:
                return {
                    'success': False,
                    'error': 'Execution timeout after 30 seconds',
                    'execution_log': 'Execution process exceeded time limit',
                    'execution_time_ms': 30000
                }
            finally:
                # Clean up temporary file
                try:
                    os.unlink(input_file)
                except:
                    pass

        except Exception as e:
            return {
                'success': False,
                'error': f'Execution failed: {str(e)}',
                'execution_log': f'Exception during execution: {str(e)}'
            }

    def _parse_execution_output(self, output: str) -> dict:
        """Parse execution output to extract rule result."""
        result = {
            'matched': False,
            'actions': [],
            'execution_path': [],
            'raw_output': output
        }

        try:
            lines = output.split('\n')
            for line in lines:
                line = line.strip()
                if line.startswith('RULE_RESULT:'):
                    # Parse JSON result from Java output
                    json_str = line.replace('RULE_RESULT:', '').strip()
                    parsed = json.loads(json_str)
                    result.update(parsed)
                    break
        except (ValueError, json.JSONDecodeError):
            # If parsing fails, return default values with raw output
            pass

        return result