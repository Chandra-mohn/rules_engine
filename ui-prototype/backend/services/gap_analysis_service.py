"""
Gap Analysis Service

Provides comprehensive analysis of rules, actionsets, and their dependencies.
Identifies missing actions, validates rules, and extracts usage patterns.
"""

import re
from typing import Dict, List, Set, Tuple, Any, Optional
from collections import defaultdict, Counter
from models import db, Rule, ProcessArea
from services.rule_service import RuleService
from grammar_parser.rules_parser import RulesEngineParser, RuleInfoExtractor
from grammar_parser.rule_validator import RuleValidator


class GapAnalysisService:
    """Service for analyzing rules and identifying gaps in implementations."""

    def __init__(self):
        self.rule_service = RuleService()

    def generate_gap_analysis(self, page: int = 1, limit: int = 20,
                            filters: Optional[List[str]] = None) -> Dict[str, Any]:
        """
        Generate comprehensive gap analysis report with pagination.

        Args:
            page: Page number for pagination
            limit: Number of items per page
            filters: List of filters to apply ('valid', 'invalid', 'rule', 'actionset', etc.)

        Returns:
            Dictionary containing aggregates, paginated details, and metadata
        """
        try:
            # Get all rules from database
            all_rules = Rule.query.all()

            # Analyze each rule
            analysis_results = []
            for rule in all_rules:
                result = self._analyze_single_rule(rule)
                analysis_results.append(result)

            # Calculate aggregates
            aggregates = self._calculate_aggregates(analysis_results)

            # Apply filters if provided
            filtered_results = self._apply_filters(analysis_results, filters or [])

            # Apply pagination
            total_filtered = len(filtered_results)
            start_idx = (page - 1) * limit
            end_idx = start_idx + limit
            paginated_results = filtered_results[start_idx:end_idx]

            # Calculate pagination info
            total_pages = (total_filtered + limit - 1) // limit if total_filtered > 0 else 1

            return {
                'aggregates': aggregates,
                'results': paginated_results,
                'pagination': {
                    'page': page,
                    'limit': limit,
                    'total': len(analysis_results),
                    'filtered': total_filtered,
                    'pages': total_pages,
                    'has_next': page < total_pages,
                    'has_prev': page > 1
                },
                'applied_filters': filters or [],
                'success': True
            }

        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'aggregates': {},
                'results': [],
                'pagination': {},
                'applied_filters': []
            }

    def _analyze_single_rule(self, rule: Rule) -> Dict[str, Any]:
        """Analyze a single rule using AST parsing for validation and extraction."""
        result = {
            'rule': rule.to_dict(),
            'validation_status': 'unknown',
            'validation_errors': [],
            'extracted_attributes': [],
            'referenced_actions': [],
            'referenced_actionsets': [],
            'missing_actions': [],
            'builtin_functions': []
        }

        try:
            # Use ANTLR-based validation and extraction via AST
            validator = RuleValidator()

            # Get available actions/attributes for validation context
            available_actions = self._get_existing_actions()
            available_attributes = self._get_defined_attributes()

            validation_context = {
                'available_actions': list(available_actions),
                'available_attributes': list(available_attributes)
            }

            # Validate using AST - this handles comments correctly via grammar
            validation_result = validator.validate_rule(rule.content, validation_context)

            # Extract validation status
            result['validation_status'] = 'valid' if validation_result['valid'] else 'invalid'

            # Extract errors (convert dict errors to strings for frontend compatibility)
            for error in validation_result.get('errors', []):
                if isinstance(error, dict):
                    result['validation_errors'].append(error.get('message', str(error)))
                else:
                    result['validation_errors'].append(str(error))

            # Extract warnings
            for warning in validation_result.get('warnings', []):
                if isinstance(warning, dict):
                    result['validation_errors'].append(f"Warning: {warning.get('message', str(warning))}")
                else:
                    result['validation_errors'].append(f"Warning: {warning}")

            # Extract info from AST parsing
            rule_info = validation_result.get('rule_info', {})

            # Attributes from AST (automatically filters comments)
            result['extracted_attributes'] = sorted(rule_info.get('attributes_used', []))

            # Actions from AST (both regular actions and ActionSet references)
            # The AST extracts all actions - both IDENTIFIER (unquoted) and STRING (quoted)
            # We consider STRING actions as potential ActionSet references
            actions_from_ast = rule_info.get('actions_used', [])

            # For now, treat all actions uniformly - let the missing actions check handle it
            # This is simpler and more accurate than trying to guess ActionSet vs action
            result['referenced_actions'] = sorted(actions_from_ast)

            # ActionSets can be identified later if needed via database lookup
            # For gap analysis, we just want to know what's referenced
            actionset_matches = []
            for action in actions_from_ast:
                matching_actionset = Rule.query.filter_by(
                    name=action,
                    item_type='actionset'
                ).first()
                if matching_actionset:
                    actionset_matches.append(action)

            result['referenced_actionsets'] = sorted(actionset_matches)

            # Functions from AST
            result['builtin_functions'] = sorted(rule_info.get('functions_used', []))

            # Find missing actions (actions referenced but not defined)
            result['missing_actions'] = [
                action for action in actions_from_ast
                if action not in available_actions
            ]

        except Exception as e:
            result['validation_status'] = 'error'
            result['validation_errors'] = [f"Analysis error: {str(e)}"]

        return result

    # REMOVED: _extract_attributes - now using AST-based extraction
    # REMOVED: _extract_actions - now using AST-based extraction
    # REMOVED: _extract_actionsets - now using AST-based extraction

    def _extract_builtin_functions(self, content: str) -> List[str]:
        """Extract builtin function calls from rule content."""
        # Common builtin functions in rules
        builtin_functions = [
            'calculateRiskScore', 'calculateCreditScore', 'getCurrentDate',
            'formatCurrency', 'validateEmail', 'generateId', 'encryptData',
            'getCurrentTime', 'convertToUpper', 'convertToLower', 'mathRound',
            'mathMax', 'mathMin', 'stringLength', 'stringContains'
        ]

        found_functions = []
        for func in builtin_functions:
            if func in content:
                found_functions.append(func)

        return sorted(found_functions)

    # REMOVED: _is_likely_action - obsolete with AST-based extraction
    # REMOVED: _is_likely_actionset_name - obsolete with AST-based extraction

    def _get_existing_actions(self) -> Set[str]:
        """Get all existing actions and actionsets from the rule service."""
        try:
            known_items = self.rule_service._get_known_actions_and_actionsets()
            return set(known_items.keys()) if isinstance(known_items, dict) else set(known_items)
        except:
            # Fallback to direct database query
            existing_actions = set()
            actions = db.session.query(Rule.name).filter(Rule.item_type.in_(['action', 'actionset'])).all()
            existing_actions.update([action.name for action in actions])
            return existing_actions

    def _get_defined_attributes(self) -> Set[str]:
        """Get all attributes defined in schema template contexts (rule_context table)."""
        try:
            # Query to get all schema templates
            query = """
            SELECT context_data
            FROM rule_context
            WHERE is_schema_template = 1
            """
            result = db.session.execute(db.text(query))

            defined_attributes = set()

            # Parse each schema template's context_data
            for row in result.fetchall():
                import json
                context_data = json.loads(row[0])

                # Each top-level key is an entity (e.g., "applicant", "transaction")
                for entity_name, entity_data in context_data.items():
                    if not isinstance(entity_data, dict):
                        continue

                    # Process each attribute (skip _metadata)
                    for attr_name in entity_data.keys():
                        if attr_name != '_metadata':
                            # Add in format 'entity.attribute'
                            defined_attributes.add(f"{entity_name}.{attr_name}")

            return defined_attributes
        except Exception as e:
            # Fallback to empty set if error occurs
            return set()

    def _calculate_aggregates(self, analysis_results: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Calculate aggregate statistics from analysis results."""
        total_rules = len(analysis_results)

        # Count by validation status
        valid_count = sum(1 for r in analysis_results if r['validation_status'] == 'valid')
        invalid_count = sum(1 for r in analysis_results if r['validation_status'] == 'invalid')
        error_count = sum(1 for r in analysis_results if r['validation_status'] == 'error')

        # Count by item type
        type_counts = defaultdict(int)
        status_counts = defaultdict(int)
        for result in analysis_results:
            rule_data = result['rule']
            type_counts[rule_data['item_type']] += 1
            status_counts[rule_data['status']] += 1

        # Count unique attributes and actions
        all_attributes = set()
        all_actions = set()
        all_missing_actions = set()

        for result in analysis_results:
            all_attributes.update(result['extracted_attributes'])
            all_actions.update(result['referenced_actions'])
            all_actions.update(result['referenced_actionsets'])
            all_missing_actions.update(result['missing_actions'])

        # Get defined attributes from schema tables
        defined_attributes = self._get_defined_attributes()

        # Find missing attributes (used in rules but not defined in schema)
        missing_attributes = all_attributes - defined_attributes

        return {
            'total_rules': total_rules,
            'validation_summary': {
                'valid': valid_count,
                'invalid': invalid_count,
                'error': error_count
            },
            'type_distribution': dict(type_counts),
            'status_distribution': dict(status_counts),
            'attribute_analysis': {
                'unique_attributes': len(all_attributes),
                'missing_attributes': len(missing_attributes),
                'most_common_attributes': self._get_top_attributes(analysis_results),
                'critical_missing_attributes': sorted(list(missing_attributes))[:10],  # Top 10 missing
                'all_missing_attributes': sorted(list(missing_attributes))  # Full list for modal
            },
            'action_analysis': {
                'unique_actions': len(all_actions),
                'missing_actions': len(all_missing_actions),
                'most_referenced': self._get_top_actions(analysis_results),
                'critical_missing_actions': sorted(list(all_missing_actions))[:10],  # Top 10 missing
                'all_missing_actions': sorted(list(all_missing_actions))  # Full list for modal
            }
        }

    def _get_top_attributes(self, analysis_results: List[Dict[str, Any]], limit: int = 5) -> List[Dict[str, Any]]:
        """Get most frequently used attributes."""
        attribute_counter = Counter()

        for result in analysis_results:
            attribute_counter.update(result['extracted_attributes'])

        return [
            {'name': attr, 'count': count}
            for attr, count in attribute_counter.most_common(limit)
        ]

    def _get_top_actions(self, analysis_results: List[Dict[str, Any]], limit: int = 5) -> List[Dict[str, Any]]:
        """Get most frequently referenced actions."""
        action_counter = Counter()

        for result in analysis_results:
            action_counter.update(result['referenced_actions'])
            action_counter.update(result['referenced_actionsets'])

        return [
            {'name': action, 'count': count}
            for action, count in action_counter.most_common(limit)
        ]

    def _apply_filters(self, results: List[Dict[str, Any]], filters: List[str]) -> List[Dict[str, Any]]:
        """Apply filters to analysis results."""
        if not filters:
            return results

        filtered = []
        for result in results:
            should_include = True

            for filter_type in filters:
                if filter_type == 'total':
                    # 'total' means show all - no filtering needed for this type
                    continue
                elif filter_type == 'valid' and result['validation_status'] != 'valid':
                    should_include = False
                    break
                elif filter_type == 'invalid' and result['validation_status'] == 'valid':
                    should_include = False
                    break
                elif filter_type in ['rule', 'actionset', 'mon_rule', 'non_mon_rule', 'action']:
                    if result['rule']['item_type'] != filter_type:
                        should_include = False
                        break
                elif filter_type == 'missing_actions' and not result['missing_actions']:
                    should_include = False
                    break
                elif filter_type in ['DRAFT', 'VALID', 'PROD', 'PEND', 'SCHD']:
                    if result['rule']['status'] != filter_type:
                        should_include = False
                        break

            if should_include:
                filtered.append(result)

        return filtered