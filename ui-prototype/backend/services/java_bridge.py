import subprocess
import json
import tempfile
import os
from pathlib import Path
from typing import Dict, Any, Tuple

class JavaBridge:
    """Bridge to communicate with Java rules engine for validation and testing."""
    
    def __init__(self, java_rules_path: Path):
        self.java_rules_path = java_rules_path
        self.classpath = java_rules_path / 'target' / 'classes'
        
    def validate_rule(self, rule_content: str) -> Dict[str, Any]:
        """
        Validate rule syntax using Java rules engine.
        
        Args:
            rule_content: The rule content to validate
            
        Returns:
            Dict with validation result: {'valid': bool, 'message': str, 'errors': list}
        """
        try:
            # Create temporary file with rule content
            with tempfile.NamedTemporaryFile(mode='w', suffix='.rules', delete=False) as temp_file:
                temp_file.write(rule_content)
                temp_file_path = temp_file.name
            
            try:
                # Run Java validation command
                cmd = [
                    'java',
                    '-cp', str(self.classpath),
                    'com.rules.cli.RuleValidator',
                    temp_file_path
                ]
                
                result = subprocess.run(
                    cmd,
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=str(self.java_rules_path)
                )
                
                if result.returncode == 0:
                    # Parse successful validation result
                    try:
                        output = json.loads(result.stdout)
                        return {
                            'valid': True,
                            'message': output.get('message', 'Rule syntax is valid'),
                            'errors': [],
                            'warnings': output.get('warnings', [])
                        }
                    except json.JSONDecodeError:
                        return {
                            'valid': True,
                            'message': 'Rule syntax is valid',
                            'errors': [],
                            'warnings': []
                        }
                else:
                    # Parse validation errors
                    error_message = result.stderr.strip() or result.stdout.strip()
                    return {
                        'valid': False,
                        'message': 'Rule validation failed',
                        'errors': [error_message],
                        'warnings': []
                    }
                    
            finally:
                # Clean up temporary file
                os.unlink(temp_file_path)
                
        except subprocess.TimeoutExpired:
            return {
                'valid': False,
                'message': 'Rule validation timed out',
                'errors': ['Validation process timed out after 30 seconds'],
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
        Test rule execution with sample data.
        
        Args:
            rule_content: The rule content to test
            test_data: Sample data to test the rule against
            
        Returns:
            Dict with test result: {'success': bool, 'result': dict, 'errors': list}
        """
        try:
            # Create temporary files for rule and test data
            with tempfile.NamedTemporaryFile(mode='w', suffix='.rules', delete=False) as rule_file:
                rule_file.write(rule_content)
                rule_file_path = rule_file.name
                
            with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as data_file:
                json.dump(test_data, data_file)
                data_file_path = data_file.name
            
            try:
                # Run Java test command
                cmd = [
                    'java',
                    '-cp', str(self.classpath),
                    'com.rules.cli.RuleTester',
                    rule_file_path,
                    data_file_path
                ]
                
                result = subprocess.run(
                    cmd,
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=str(self.java_rules_path)
                )
                
                if result.returncode == 0:
                    # Parse successful test result
                    try:
                        output = json.loads(result.stdout)
                        return {
                            'success': True,
                            'result': output,
                            'errors': []
                        }
                    except json.JSONDecodeError:
                        return {
                            'success': True,
                            'result': {'message': 'Rule executed successfully'},
                            'errors': []
                        }
                else:
                    # Parse test errors
                    error_message = result.stderr.strip() or result.stdout.strip()
                    return {
                        'success': False,
                        'result': {},
                        'errors': [error_message]
                    }
                    
            finally:
                # Clean up temporary files
                os.unlink(rule_file_path)
                os.unlink(data_file_path)
                
        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'result': {},
                'errors': ['Rule test timed out after 30 seconds']
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
        
        # Combine all suggestions
        all_suggestions = keywords + operators + attributes + actions + functions + time_units
        
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
            suggestions = all_suggestions
        
        return {'suggestions': suggestions}