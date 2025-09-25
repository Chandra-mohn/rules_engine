"""
Python Rules Engine
Complete replacement for Java bridge using Python ANTLR implementation.
"""

import tempfile
import subprocess
import time
import json
import os
from pathlib import Path
from typing import Dict, Any, List

from grammar_parser import RulesEngineParser, RuleValidator
from grammar_parser.advanced_java_generator import AdvancedJavaCodeGenerator


class PythonRulesEngine:
    """
    Complete Python implementation of the rules engine.
    Replaces the Java bridge with Python ANTLR parsing and Java code generation.
    """

    def __init__(self):
        self.parser = RulesEngineParser()
        self.code_generator = AdvancedJavaCodeGenerator()
        self.validator = RuleValidator()

        # Cache for compiled rules
        self.compiled_rules_cache = {}

        # Java runtime setup
        self.java_classpath = self._setup_java_classpath()

    def validate_rule(self, rule_content: str, context: Dict[str, Any] = None) -> Dict[str, Any]:
        """
        Validate rule using Python ANTLR parser.

        Args:
            rule_content: Rule content to validate
            context: Validation context with available actions/attributes

        Returns:
            dict: Validation result
        """
        try:
            # Get validation context from database if not provided
            if context is None:
                context = self._get_validation_context()

            # Perform comprehensive validation using Python ANTLR only
            result = self.validator.validate_rule(rule_content, context)

            return {
                'valid': result['valid'],
                'message': 'Rule validation completed' if result['valid'] else 'Rule validation failed',
                'errors': result['errors'],
                'warnings': result['warnings'],
                'rule_info': result.get('rule_info', {}),
                'undefined_actions': result.get('undefined_actions', []),
                'undefined_attributes': result.get('undefined_attributes', [])
            }

        except Exception as e:
            return {
                'valid': False,
                'message': f'Validation failed: {str(e)}',
                'errors': [str(e)],
                'warnings': []
            }

    def compile_rule(self, rule_content: str, rule_id: str = None) -> Dict[str, Any]:
        """
        Compile rule to Java bytecode using Python ANTLR parser.

        Args:
            rule_content: Rule content to compile
            rule_id: Optional rule identifier

        Returns:
            dict: Compilation result
        """
        start_time = time.time()

        if not rule_id:
            rule_id = f"rule_{int(time.time() * 1000)}"

        try:
            # Step 1: Parse rule using Python ANTLR
            tree, error_listener = self.parser.parse(rule_content)

            if tree is None or error_listener.errors:
                return {
                    'success': False,
                    'errors': [error['message'] for error in error_listener.errors],
                    'valid': False,
                    'message': 'Parse errors found'
                }

            # Step 2: Extract rule name
            rule_info = self.parser.extract_rule_info(rule_content)
            rule_name = rule_info.get('name', rule_id)

            # Step 3: Generate Java code using simple generator
            java_code = self.code_generator.generate(rule_content, rule_name)

            # Step 4: Compile Java code
            compilation_result = self._compile_java_code(java_code, rule_name)

            compilation_time = int((time.time() - start_time) * 1000)

            if compilation_result['success']:
                # Cache the compiled rule
                self.compiled_rules_cache[rule_id] = {
                    'java_code': java_code,
                    'class_name': compilation_result['class_name'],
                    'compiled_at': time.time()
                }

                return {
                    'success': True,
                    'valid': True,
                    'ruleId': rule_id,
                    'ruleName': rule_name,
                    'className': compilation_result['class_name'],
                    'compilationTimeMs': compilation_time,
                    'message': 'Rule compiled successfully',
                    'java_code': java_code
                }
            else:
                return {
                    'success': False,
                    'valid': False,
                    'errors': compilation_result['errors'],
                    'message': 'Java compilation failed'
                }

        except Exception as e:
            return {
                'success': False,
                'valid': False,
                'errors': [str(e)],
                'message': f'Compilation failed: {str(e)}'
            }

    def test_rule(self, rule_content: str, test_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Test rule execution with sample data.

        Args:
            rule_content: Rule content to test
            test_data: Test data for rule execution

        Returns:
            dict: Test execution result
        """
        start_time = time.time()

        try:
            # Step 1: Compile rule
            compilation_result = self.compile_rule(rule_content)

            if not compilation_result['success']:
                return {
                    'success': False,
                    'result': {},
                    'errors': compilation_result.get('errors', ['Compilation failed'])
                }

            # Step 2: Execute compiled rule
            execution_result = self._execute_rule(
                compilation_result['className'],
                compilation_result['java_code'],
                test_data
            )

            execution_time = int((time.time() - start_time) * 1000)

            if execution_result['success']:
                return {
                    'success': True,
                    'result': {
                        'matched': execution_result['result'].get('matched', False),
                        'actions': execution_result['result'].get('actions', []),
                        'finalAction': execution_result['result'].get('finalAction'),
                        'executionTimeMs': execution_time,
                        'ruleCompiled': True,
                        'className': compilation_result['className'],
                        'compilationTimeMs': compilation_result['compilationTimeMs']
                    },
                    'errors': []
                }
            else:
                return {
                    'success': False,
                    'result': {},
                    'errors': execution_result.get('errors', ['Execution failed'])
                }

        except Exception as e:
            return {
                'success': False,
                'result': {},
                'errors': [str(e)]
            }

    def get_autocomplete_suggestions(self, context: str, position: int) -> Dict[str, Any]:
        """
        Get intelligent autocomplete suggestions using Python ANTLR.

        Args:
            context: Rule content context
            position: Cursor position

        Returns:
            dict: Autocomplete suggestions
        """
        try:
            # Use Python ANTLR to analyze context
            context_info = self.validator.get_completion_context(context, position)

            suggestions = []

            # If ANTLR context analysis fails, use simple text-based detection
            if context_info['context_type'] == 'unknown':
                context_info = self._simple_context_detection(context, position)

            if context_info['context_type'] == 'condition':
                suggestions.extend(self._get_condition_suggestions(context_info))
            elif context_info['context_type'] == 'action':
                suggestions.extend(self._get_action_suggestions())
            elif context_info['context_type'] == 'attribute':
                suggestions.extend(self._get_attribute_suggestions(context_info))
            else:
                # General suggestions
                suggestions.extend(self._get_general_suggestions())

            return {
                'suggestions': suggestions,
                'context_type': context_info['context_type'],
                'cursor_info': context_info
            }

        except Exception as e:
            return {
                'suggestions': self._get_fallback_suggestions(),
                'context_type': 'error',
                'error': str(e)
            }

    def _test_compilation(self, rule_content: str) -> Dict[str, Any]:
        """Test compilation without caching."""
        try:
            tree, error_listener = self.parser.parse(rule_content)
            if tree is None or error_listener.errors:
                return {
                    'success': False,
                    'errors': [error['message'] for error in error_listener.errors]
                }

            rule_info = self.parser.extract_rule_info(rule_content)
            rule_name = rule_info.get('name', 'TestRule')
            java_code = self.code_generator.generate(rule_content, rule_name)

            return self._compile_java_code(java_code, rule_name)

        except Exception as e:
            return {
                'success': False,
                'errors': [str(e)]
            }

    def _compile_java_code(self, java_code: str, rule_name: str) -> Dict[str, Any]:
        """Compile Java code to bytecode."""
        try:
            with tempfile.TemporaryDirectory() as temp_dir:
                # Create package directory structure
                package_dir = Path(temp_dir) / "com" / "rules"
                package_dir.mkdir(parents=True)

                # Write Java source file
                class_name = f"{self._to_class_name(rule_name)}Rule"
                java_file = package_dir / f"{class_name}.java"
                java_file.write_text(java_code)

                # Compile
                compile_cmd = [
                    'javac',
                    '-cp', self.java_classpath,
                    str(java_file)
                ]

                result = subprocess.run(
                    compile_cmd,
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=temp_dir
                )

                if result.returncode == 0:
                    return {
                        'success': True,
                        'class_name': f"com.rules.{class_name}"
                    }
                else:
                    return {
                        'success': False,
                        'errors': [result.stderr] if result.stderr else ['Compilation failed']
                    }

        except Exception as e:
            return {
                'success': False,
                'errors': [str(e)]
            }

    def _execute_rule(self, class_name: str, java_code: str, test_data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute compiled rule with test data."""
        try:
            with tempfile.TemporaryDirectory() as temp_dir:
                # Recreate and compile the rule
                package_dir = Path(temp_dir) / "com" / "rules"
                package_dir.mkdir(parents=True)

                simple_class_name = class_name.split('.')[-1]
                java_file = package_dir / f"{simple_class_name}.java"
                java_file.write_text(java_code)

                # Compile
                compile_cmd = [
                    'javac',
                    '-cp', self.java_classpath,
                    str(java_file)
                ]

                subprocess.run(compile_cmd, capture_output=True, text=True, timeout=30, cwd=temp_dir)

                # Create test data JSON file
                test_data_file = Path(temp_dir) / "test_data.json"
                test_data_file.write_text(json.dumps(test_data))

                # Execute rule
                execute_cmd = [
                    'java',
                    '-cp', f"{self.java_classpath}{os.pathsep}{temp_dir}",
                    'com.rules.RuleExecutor',
                    class_name,
                    str(test_data_file)
                ]

                result = subprocess.run(
                    execute_cmd,
                    capture_output=True,
                    text=True,
                    timeout=30
                )

                if result.returncode == 0:
                    output = json.loads(result.stdout)
                    return {
                        'success': True,
                        'result': output
                    }
                else:
                    return {
                        'success': False,
                        'errors': [result.stderr] if result.stderr else ['Execution failed']
                    }

        except Exception as e:
            return {
                'success': False,
                'errors': [str(e)]
            }

    def _setup_java_classpath(self) -> str:
        """Setup Java classpath for compilation and execution."""
        # This would need to be implemented based on your Java dependencies
        # For now, return a basic classpath
        base_path = Path(__file__).parent.parent.parent / "java-bridge"
        classpath_file = base_path / "classpath.txt"

        if classpath_file.exists():
            return classpath_file.read_text().strip()
        else:
            return "."

    def _get_validation_context(self) -> Dict[str, Any]:
        """Get validation context from database."""
        try:
            # Import here to avoid circular imports
            from services.rule_service import RuleService

            rule_service = RuleService()

            # Get available actions and attributes
            available_actions = []
            available_attributes = []

            # This would be implemented based on your schema
            # For now, return empty lists

            return {
                'available_actions': available_actions,
                'available_attributes': available_attributes
            }

        except Exception:
            return {
                'available_actions': [],
                'available_attributes': []
            }

    def _get_condition_suggestions(self, context_info: Dict[str, Any]) -> List[Dict[str, str]]:
        """Get suggestions for condition context."""
        suggestions = []

        if context_info.get('current_entity'):
            entity = context_info['current_entity']
            # Add entity-specific attribute suggestions
            suggestions.extend([
                {'label': f'{entity}.amount', 'kind': 'Property'},
                {'label': f'{entity}.status', 'kind': 'Property'},
                {'label': f'{entity}.date', 'kind': 'Property'}
            ])
        else:
            # Add entity suggestions
            suggestions.extend([
                {'label': 'applicant', 'kind': 'Variable'},
                {'label': 'transaction', 'kind': 'Variable'},
                {'label': 'account', 'kind': 'Variable'}
            ])

        # Add operators
        suggestions.extend([
            {'label': '==', 'kind': 'Operator'},
            {'label': '!=', 'kind': 'Operator'},
            {'label': '>', 'kind': 'Operator'},
            {'label': '<', 'kind': 'Operator'},
            {'label': 'contains', 'kind': 'Operator'},
            {'label': 'in', 'kind': 'Operator'}
        ])

        return suggestions

    def _get_action_suggestions(self) -> List[Dict[str, str]]:
        """Get suggestions for action context."""
        return [
            {'label': 'approveApplication', 'kind': 'Function'},
            {'label': 'rejectApplication', 'kind': 'Function'},
            {'label': 'requestDocumentation', 'kind': 'Function'},
            {'label': 'scheduleReview', 'kind': 'Function'},
            {'label': 'assignAgent', 'kind': 'Function'},
            {'label': 'setLimit', 'kind': 'Function'},
            {'label': 'logDecision', 'kind': 'Function'}
        ]

    def _get_general_suggestions(self) -> List[Dict[str, str]]:
        """Get general suggestions."""
        return [
            {'label': 'if', 'kind': 'Keyword'},
            {'label': 'then', 'kind': 'Keyword'},
            {'label': 'else', 'kind': 'Keyword'},
            {'label': 'rule', 'kind': 'Keyword'}
        ]

    def _simple_context_detection(self, context: str, position: int) -> Dict[str, Any]:
        """Simple text-based context detection as fallback."""
        # Get text up to cursor position
        text_before_cursor = context[:position]
        lines = text_before_cursor.split('\n')
        current_line = lines[-1] if lines else ''

        # Simple patterns for context detection

        # Check if we're in an action context (after "then")
        action_context = False
        if 'then' in current_line:
            # "then" is on current line
            then_pos = current_line.find('then')
            cursor_on_line = len(current_line)  # cursor is at end of current line
            if cursor_on_line > then_pos + 4:  # after "then" + space
                action_context = True
        elif len(lines) > 1:
            # Check if previous lines contain "then"
            for line in lines[:-1]:
                if 'then' in line and 'if' in line:
                    action_context = True
                    break

        if action_context:
            return {
                'context_type': 'action',
                'in_action': True,
                'in_condition': False,
                'current_entity': None
            }
        elif any(entity in current_line for entity in ['applicant.', 'transaction.', 'account.']):
            # We're typing an entity attribute
            entity = None
            for ent in ['applicant', 'transaction', 'account']:
                if f'{ent}.' in current_line:
                    entity = ent
                    break
            return {
                'context_type': 'attribute',
                'in_condition': 'if' in current_line,
                'in_action': False,
                'current_entity': entity
            }
        elif 'if' in current_line and 'then' not in current_line:
            # We're in a condition
            return {
                'context_type': 'condition',
                'in_condition': True,
                'in_action': False,
                'current_entity': None
            }
        else:
            # General context
            return {
                'context_type': 'general',
                'in_condition': False,
                'in_action': False,
                'current_entity': None
            }

    def _get_attribute_suggestions(self, context_info: Dict[str, Any]) -> List[Dict[str, str]]:
        """Get suggestions for attribute context."""
        suggestions = []
        entity = context_info.get('current_entity')

        if entity == 'applicant':
            suggestions.extend([
                {'label': 'applicant.creditScore', 'kind': 'Property'},
                {'label': 'applicant.annualIncome', 'kind': 'Property'},
                {'label': 'applicant.age', 'kind': 'Property'},
                {'label': 'applicant.employmentStatus', 'kind': 'Property'},
                {'label': 'applicant.debtToIncomeRatio', 'kind': 'Property'}
            ])
        elif entity == 'transaction':
            suggestions.extend([
                {'label': 'transaction.amount', 'kind': 'Property'},
                {'label': 'transaction.currency', 'kind': 'Property'},
                {'label': 'transaction.type', 'kind': 'Property'},
                {'label': 'transaction.merchantCategory', 'kind': 'Property'}
            ])
        elif entity == 'account':
            suggestions.extend([
                {'label': 'account.balance', 'kind': 'Property'},
                {'label': 'account.type', 'kind': 'Property'},
                {'label': 'account.status', 'kind': 'Property'}
            ])
        else:
            # General entity suggestions
            suggestions.extend([
                {'label': 'applicant', 'kind': 'Variable'},
                {'label': 'transaction', 'kind': 'Variable'},
                {'label': 'account', 'kind': 'Variable'}
            ])

        return suggestions

    def _get_fallback_suggestions(self) -> List[Dict[str, str]]:
        """Get fallback suggestions when context analysis fails."""
        return self._get_general_suggestions() + self._get_action_suggestions()

    def _to_class_name(self, name: str) -> str:
        """Convert rule name to valid Java class name."""
        import re
        name = re.sub(r'[^a-zA-Z0-9_]', '', name)
        if name:
            return name[0].upper() + name[1:]
        return "DefaultRule"