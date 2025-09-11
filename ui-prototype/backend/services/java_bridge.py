import requests
import json
import platform
from pathlib import Path
from typing import Dict, Any, Tuple

class JavaBridge:
    """Bridge to communicate with Java rules engine for validation and testing."""
    
    def __init__(self, java_rules_path: Path = None, server_url: str = "http://localhost:8081"):
        self.java_rules_path = java_rules_path  # Kept for backward compatibility
        self.server_url = server_url.rstrip('/')
        self.is_windows = platform.system().lower() == 'windows'
        
    def validate_rule(self, rule_content: str) -> Dict[str, Any]:
        """
        Validate rule syntax using Java rules engine via HTTP.
        
        Args:
            rule_content: The rule content to validate
            
        Returns:
            Dict with validation result: {'valid': bool, 'message': str, 'errors': list}
        """
        try:
            response = requests.post(
                f"{self.server_url}/api/rules/validate",
                json={"ruleContent": rule_content},
                timeout=30
            )
            
            if response.status_code == 200:
                result = response.json()
                return {
                    'valid': result.get('valid', False),
                    'message': result.get('message', 'Rule syntax validated'),
                    'errors': result.get('errors', []),
                    'warnings': result.get('warnings', [])
                }
            else:
                error_data = response.json() if response.headers.get('content-type', '').startswith('application/json') else {}
                return {
                    'valid': False,
                    'message': error_data.get('message', f'HTTP {response.status_code}'),
                    'errors': [error_data.get('error', f'Server returned {response.status_code}')],
                    'warnings': []
                }
                
        except requests.exceptions.ConnectionError:
            return {
                'valid': False,
                'message': 'Java rules server not available',
                'errors': ['Cannot connect to Java rules server at ' + self.server_url],
                'warnings': []
            }
        except requests.exceptions.Timeout:
            return {
                'valid': False,
                'message': 'Rule validation timed out',
                'errors': ['Validation request timed out after 30 seconds'],
                'warnings': []
            }
        except Exception as e:
            return {
                'valid': False,
                'message': f'Validation error: {str(e)}',
                'errors': [str(e)],
                'warnings': []
            }
    
    def test_rule(self, rule_content: str, test_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Test rule execution with sample data via HTTP.
        
        Args:
            rule_content: The rule content to test
            test_data: Sample data to test the rule against
            
        Returns:
            Dict with test result: {'success': bool, 'result': dict, 'errors': list}
        """
        try:
            response = requests.post(
                f"{self.server_url}/api/rules/test",
                json={
                    "ruleContent": rule_content,
                    "testData": test_data
                },
                timeout=30
            )
            
            if response.status_code == 200:
                result = response.json()
                return {
                    'success': True,
                    'result': result,
                    'errors': []
                }
            else:
                error_data = response.json() if response.headers.get('content-type', '').startswith('application/json') else {}
                return {
                    'success': False,
                    'result': {},
                    'errors': [error_data.get('error', f'Server returned {response.status_code}')]
                }
                
        except requests.exceptions.ConnectionError:
            return {
                'success': False,
                'result': {},
                'errors': ['Cannot connect to Java rules server at ' + self.server_url]
            }
        except requests.exceptions.Timeout:
            return {
                'success': False,
                'result': {},
                'errors': ['Rule test request timed out after 30 seconds']
            }
        except Exception as e:
            return {
                'success': False,
                'result': {},
                'errors': [str(e)]
            }
    
    def get_autocomplete_suggestions(self, context: str, position: int) -> Dict[str, Any]:
        """
        Get autocomplete suggestions for rule editing.
        
        Args:
            context: The current rule content context
            position: Cursor position in the context
            
        Returns:
            Dict with suggestions: {'suggestions': list}
        """
        # Import centralized schema configuration
        from schema.rules_schema import (
            get_all_attributes, get_all_actions, get_all_functions,
            get_attributes_by_entity, KEYWORDS, OPERATORS, TIME_UNITS
        )
        
        suggestions = []
        
        # Extract the current line and word being typed
        lines = context[:position].split('\n')
        current_line = lines[-1] if lines else ""
        
        # Get suggestions from centralized configuration
        keywords = [
            {'label': keyword, 'kind': 'keyword', 'detail': 'Keyword'}
            for keyword in KEYWORDS
        ]
        
        operators = [
            {'label': op, 'kind': 'operator', 'detail': 'Operator'}
            for op in OPERATORS
        ]
        
        time_units = [
            {'label': unit, 'kind': 'keyword', 'detail': 'Time unit'}
            for unit in TIME_UNITS
        ]
        
        # Get attributes, actions, and functions from schema
        attributes = get_all_attributes()
        actions = get_all_actions()
        functions = get_all_functions()
        
        # Filter based on current context
        if 'applicant.' in current_line:
            suggestions = get_attributes_by_entity('applicant')
        elif 'transaction.' in current_line:
            suggestions = get_attributes_by_entity('transaction')
        elif 'account.' in current_line:
            suggestions = get_attributes_by_entity('account')
        elif current_line.strip().endswith('then'):
            suggestions = actions
        elif any(word in current_line.lower() for word in ['year_of', 'month_of', 'day_of']):
            suggestions = [s for s in functions if s['detail'] in ['datetime', 'date', 'number']]
        else:
            # Combine all suggestions for general context
            suggestions = keywords + operators + attributes + actions + functions + time_units
        
        # Remove duplicates by converting to dict (keeping last occurrence) then back to list
        seen_labels = set()
        unique_suggestions = []
        for suggestion in suggestions:
            if suggestion['label'] not in seen_labels:
                seen_labels.add(suggestion['label'])
                unique_suggestions.append(suggestion)
        
        return {'suggestions': unique_suggestions}