from typing import Dict, Any, List, Optional, Tuple
from sqlalchemy import or_
from datetime import datetime
from models import db, Rule, RuleHistory, SchemaEntity, SchemaAttribute
from services.java_bridge import JavaBridge
from services.list_cache import ListService
from config import Config
import re
import json

class RuleService:
    """Service class for rule management operations."""
    
    def __init__(self):
        # Use HTTP-based Java bridge instead of subprocess
        self.java_bridge = JavaBridge(server_url="http://localhost:8081")
        self.list_service = ListService()
    
    def parse_rule_name_from_content(self, content: str) -> str:
        """
        Parse rule name from rule content.
        Supports both quoted and unquoted identifiers:
        - rule "PROMOTION $5%3 @SEARS":
        - rule regularRuleName:
        
        Args:
            content: Rule content string
            
        Returns:
            Parsed rule name or empty string if not found
        """
        if not content:
            return ''
        
        # Match both quoted and unquoted rule names
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
                  process_area_id: Optional[int] = None) -> Tuple[List[Rule], int]:
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
            
        Returns:
            Tuple of (rules_list, total_count)
        """
        from models import ProcessArea, ProcessGroup, Client
        
        # Start with Rule query and join hierarchy for filtering
        query = Rule.query.join(ProcessArea).join(ProcessGroup).join(Client)
        
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
                'message': 'Could not parse rule name from content. Rule must start with "rule name:"',
                'errors': ['Rule name not found in content']
            }
        
        # Validate rule content
        validation_result = self.java_bridge.validate_rule(data['content'])
        
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
            validation_result = self.validate_rule_with_lists(data['content'], data.get('schema_version', 'modern'))
        elif rule.status == 'DRAFT' and 'status' not in data:
            # DRAFT rule without explicit status change - validate current content for potential auto-promotion
            content_to_validate = rule.content
            validation_result = self.validate_rule_with_lists(rule.content, rule.schema_version or 'modern')
        
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
    
    def validate_rule_content(self, content: str) -> Dict[str, Any]:
        """Validate rule content without saving."""
        return self.java_bridge.validate_rule(content)
    
    def test_rule(self, content: str, test_data: Dict[str, Any]) -> Dict[str, Any]:
        """Test rule execution with sample data using hot compilation."""
        # Use hot compilation for better performance
        hot_result = self.java_bridge.test_rule_hot(content, test_data)
        
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
                    'method': 'hot_compilation'
                },
                'errors': []
            }
        else:
            # If hot compilation fails, fall back to legacy method
            legacy_result = self.java_bridge.test_rule(content, test_data)
            if legacy_result['success']:
                # Add performance indicator for legacy method
                legacy_result['performance'] = {
                    'method': 'legacy_interpretation',
                    'note': 'Hot compilation failed, used legacy method'
                }
            return legacy_result
    
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
        validation_result = self.java_bridge.validate_rule(history_entry.content)
        
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
        return self.java_bridge.get_autocomplete_suggestions(context, position)
    
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
    
    def validate_rule_with_lists(self, content: str, schema_version: str = 'modern') -> Dict[str, Any]:
        """
        Validate rule content by first resolving named lists.
        
        Args:
            content: Rule content potentially containing named list references
            schema_version: Schema version for list compatibility
            
        Returns:
            Validation result dictionary
        """
        try:
            # Resolve named lists in the rule content
            resolved_content = self.list_service.resolve_rule_lists(content, schema_version)
            
            # Validate the resolved content
            return self.java_bridge.validate_rule(resolved_content)
            
        except Exception as e:
            return {
                'valid': False,
                'message': f'List resolution failed: {str(e)}',
                'errors': [str(e)]
            }
    
    def generate_production_artifacts(self, rule_id: str, rule_name: str, rule_content: str, package_name: str) -> Dict[str, Any]:
        """
        Generate production deployment artifacts for a rule.
        Creates a library JAR with Java source, pom.xml, and Dockerfile.
        Saves files to disk for version control.
        
        Args:
            rule_id: Unique rule identifier
            rule_name: Human-readable rule name
            rule_content: Rule DSL content
            package_name: Java package name for generated code
            
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
                java_code_result = self.java_bridge.generate_java_code(rule_content)
                if not java_code_result.get('success'):
                    return {
                        'success': False,
                        'message': f'Java code generation failed: {java_code_result.get("message", "Unknown error")}'
                    }
                java_code = java_code_result.get('javaCode', '')
            
            # Generate artifacts in memory first
            artifacts = self._generate_rule_library_artifacts(
                rule_id, rule_name, rule_content, package_name, java_code
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
                                       package_name: str, java_code: str) -> Dict[str, str]:
        """Generate all production artifacts for a rule library."""
        
        # Implement proper naming conventions
        safe_rule_id = str(rule_id).replace('-', '_').replace(' ', '_').lower()
        safe_package_path = package_name.replace('.', '/')
        
        # Generate PascalCase class name from rule name
        class_name = self._to_pascal_case(rule_name) + "Rule"
        
        artifacts = {}
        
        # 1. Generated Java source file
        artifacts[f'src/main/java/{safe_package_path}/{class_name}.java'] = self._generate_rule_java_wrapper(
            package_name, class_name, java_code, rule_name, rule_content
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