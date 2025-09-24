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
        """Analyze a single rule for validation, dependencies, and attributes."""
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
            # Use existing rule service validation
            validation_result = self.rule_service.validate_rule_content(rule.content)
            result['validation_status'] = 'valid' if validation_result.get('valid', False) else 'invalid'
            result['validation_errors'] = validation_result.get('errors', [])

            # If there are warnings, note them but still consider valid if no errors
            if validation_result.get('warnings'):
                result['validation_errors'].extend([f"Warning: {w}" for w in validation_result.get('warnings', [])])

            # Extract components using existing rule service methods
            result['extracted_attributes'] = self._extract_attributes(rule.content)

            # Use existing rule service action extraction
            extracted_actions = self.rule_service._extract_actions_from_rule(rule.content)
            result['referenced_actions'] = extracted_actions
            result['referenced_actionsets'] = self._extract_actionsets(rule.content)
            result['builtin_functions'] = self._extract_builtin_functions(rule.content)

            # Find missing actions by comparing with existing actions
            all_referenced = extracted_actions + result['referenced_actionsets']
            existing_actions = self._get_existing_actions()
            result['missing_actions'] = [action for action in all_referenced if action not in existing_actions]

        except Exception as e:
            result['validation_status'] = 'error'
            result['validation_errors'] = [f"Analysis error: {str(e)}"]

        return result

    def _extract_attributes(self, content: str) -> List[str]:
        """Extract variable attributes from rule content (e.g., applicant.creditScore)."""
        # Pattern to match object.attribute references
        attribute_pattern = r'\b([a-zA-Z_][a-zA-Z0-9_]*\.[a-zA-Z_][a-zA-Z0-9_]*)\b'
        attributes = re.findall(attribute_pattern, content)

        # Remove duplicates and sort
        return sorted(list(set(attributes)))

    def _extract_actions(self, content: str) -> List[str]:
        """Extract action calls from rule content."""
        # Pattern to match action calls (function-like calls)
        action_pattern = r'\b([a-zA-Z_][a-zA-Z0-9_]*)\s*(?:\([^)]*\))?'
        potential_actions = re.findall(action_pattern, content)

        # Filter out common keywords and keep likely action names
        keywords_to_exclude = {
            'rule', 'if', 'then', 'else', 'and', 'or', 'not', 'true', 'false',
            'RULE', 'IF', 'THEN', 'ELSE', 'AND', 'OR', 'NOT', 'TRUE', 'FALSE'
        }

        actions = []
        for action in potential_actions:
            if action not in keywords_to_exclude and self._is_likely_action(action):
                actions.append(action)

        return sorted(list(set(actions)))

    def _extract_actionsets(self, content: str) -> List[str]:
        """Extract actionset references from rule content."""
        # Look for quoted strings that might be actionset names
        actionset_pattern = r'"([^"]+)"'
        potential_actionsets = re.findall(actionset_pattern, content)

        # Filter to keep only those that look like actionset names
        actionsets = []
        for name in potential_actionsets:
            if self._is_likely_actionset_name(name):
                actionsets.append(name)

        return sorted(list(set(actionsets)))

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

    def _is_likely_action(self, name: str) -> bool:
        """Determine if a name looks like an action."""
        # Actions typically start with a verb or are camelCase
        action_patterns = [
            r'^(approve|reject|send|log|update|create|delete|validate|calculate|process)',
            r'^[a-z][a-zA-Z]*[A-Z]',  # camelCase
            r'[A-Z][a-z]+[A-Z]'  # PascalCase
        ]

        return any(re.match(pattern, name, re.IGNORECASE) for pattern in action_patterns)

    def _is_likely_actionset_name(self, name: str) -> bool:
        """Determine if a name looks like an actionset."""
        # Actionsets often have descriptive names with spaces or specific patterns
        return (len(name.split()) > 1 or
                'workflow' in name.lower() or
                'process' in name.lower() or
                'approval' in name.lower())

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
        """Get all attributes defined in schema_entities and schema_attributes tables."""
        try:
            # Query to get all defined attributes in format 'entity.attribute'
            query = """
            SELECT e.name || '.' || a.name as full_name
            FROM schema_entities e
            JOIN schema_attributes a ON e.id = a.entity_id
            WHERE e.is_active = 1
            """
            result = db.session.execute(query)
            defined_attributes = {row[0] for row in result.fetchall()}
            return defined_attributes
        except Exception as e:
            # Fallback to empty set if schema tables don't exist or error occurs
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