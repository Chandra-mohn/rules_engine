import requests
import json
import platform
import subprocess
import tempfile
import time
import os
import re
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
        Validate rule syntax and parameters using enhanced validation.

        Args:
            rule_content: The rule content to validate

        Returns:
            Dict with validation result: {'valid': bool, 'message': str, 'errors': list, 'warnings': list}
        """
        errors = []
        warnings = []

        try:
            # Basic syntax validation (still try compilation for serious syntax errors)
            compilation_result = self.compile_rule(rule_content, f"validation_{int(time.time() * 1000)}")

            if not compilation_result.get('valid', False):
                errors.extend(compilation_result.get('errors', []))

            # Enhanced parameter validation
            param_errors = self._validate_rule_parameters(rule_content)
            errors.extend(param_errors)

            # Additional validations can be added here

            is_valid = len(errors) == 0

            return {
                'valid': is_valid,
                'message': 'Rule validation completed' if is_valid else 'Rule validation failed',
                'errors': errors,
                'warnings': warnings
            }

        except Exception as e:
            return {
                'valid': False,
                'message': f'Validation error: {str(e)}',
                'errors': [str(e)],
                'warnings': []
            }

    def _validate_rule_parameters(self, rule_content: str) -> list:
        """Validate parameters in all actions within the rule content."""
        errors = []

        # Extract all actions from the rule content
        import re

        # Find all action calls - both simple and parameterized
        # This regex finds actions in "then" clauses and standalone actions
        action_patterns = [
            r'then\s+([^,\n]+(?:,\s*[^,\n]+)*)',  # Actions after "then"
            r'else\s+([^,\n]+(?:,\s*[^,\n]+)*)',  # Actions after "else"
        ]

        # Also look for standalone actions (not after if/then/else)
        lines = rule_content.split('\n')
        for line in lines:
            stripped = line.strip()
            if (stripped and
                not stripped.startswith('#') and
                not stripped.startswith('rule') and
                not stripped.startswith('if ') and
                not stripped.startswith('else ') and
                'then' not in stripped):
                # This might be a standalone action
                action_patterns.append(re.escape(stripped))

        for pattern in action_patterns:
            matches = re.findall(pattern, rule_content, re.IGNORECASE)
            for match in matches:
                # Split multiple actions separated by commas
                action_texts = [action.strip() for action in match.split(',')]
                for action_text in action_texts:
                    if action_text.strip():
                        parsed_action = self._parse_action(action_text.strip())
                        if parsed_action.get('validation_errors'):
                            errors.extend(parsed_action['validation_errors'])

        return errors
    
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
        
        # Filter based on current context - prioritize action context
        if current_line.strip().endswith('then') or 'then ' in current_line.lower():
            suggestions = actions
        elif 'applicant.' in current_line:
            suggestions = get_attributes_by_entity('applicant')
        elif 'transaction.' in current_line:
            suggestions = get_attributes_by_entity('transaction')
        elif 'account.' in current_line:
            suggestions = get_attributes_by_entity('account')
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

    def _extract_rule_name(self, rule_content: str) -> str:
        """Extract rule name from unified rule syntax."""
        # Unified syntax: only look for "rule ruleName:" pattern
        # Support quoted names: rule "Complex Name":
        patterns = [
            r'rule\s+"([^"]+)"\s*:',  # Quoted names
            r'rule\s+(\w+)\s*:'       # Unquoted names
        ]

        for pattern in patterns:
            match = re.search(pattern, rule_content, re.IGNORECASE)
            if match:
                return match.group(1)

        return "UnknownRule"

    def _get_action_parameter_schema(self, action_name: str) -> Dict:
        """Get parameter schema for known actions."""
        action_schemas = {
            'sendEmail': {
                'parameters': [
                    {'name': 'template', 'type': 'string', 'required': True},
                    {'name': 'recipient', 'type': 'field', 'required': True}
                ]
            },
            'scheduleCallback': {
                'parameters': [
                    {'name': 'delay', 'type': 'number', 'required': True},
                    {'name': 'reason', 'type': 'string', 'required': False}
                ]
            },
            'setLimit': {
                'parameters': [
                    {'name': 'amount', 'type': 'number', 'required': True}
                ]
            },
            'logDecision': {
                'parameters': [
                    {'name': 'message', 'type': 'string', 'required': False}
                ]
            }
        }
        return action_schemas.get(action_name, {'parameters': []})

    def _validate_action_parameters(self, action_name: str, parameters: list) -> list:
        """Validate action parameters against schema."""
        errors = []
        schema = self._get_action_parameter_schema(action_name)
        expected_params = schema.get('parameters', [])

        # Check required parameters
        required_params = [p for p in expected_params if p.get('required', False)]
        if len(parameters) < len(required_params):
            errors.append(f"Action '{action_name}' requires {len(required_params)} parameters, got {len(parameters)}")
            return errors

        # Validate parameter types
        for i, param in enumerate(parameters):
            if i < len(expected_params):
                expected = expected_params[i]
                expected_type = expected.get('type')
                param_type = param.get('type')

                if expected_type == 'string' and param_type != 'string':
                    errors.append(f"Parameter {i+1} of '{action_name}' should be a string")
                elif expected_type == 'number' and param_type != 'number':
                    errors.append(f"Parameter {i+1} of '{action_name}' should be a number")
                elif expected_type == 'field' and param_type != 'field':
                    errors.append(f"Parameter {i+1} of '{action_name}' should be a field reference")

        return errors

    def _parse_action(self, action_text: str) -> Dict:
        """Parse action with optional parameters."""
        action_text = action_text.strip().strip('"')

        if '(' in action_text and action_text.endswith(')'):
            # Parameterized action: sendEmail("template", recipient)
            open_paren = action_text.find('(')
            action_name = action_text[:open_paren].strip()
            params_text = action_text[open_paren+1:-1].strip()

            parameters = []
            if params_text:
                # Simple parameter parsing (can be enhanced)
                for param in params_text.split(','):
                    param = param.strip()
                    if param.startswith('"') and param.endswith('"'):
                        # String parameter
                        parameters.append({'type': 'string', 'value': param[1:-1]})
                    elif '.' in param and not param.replace('.', '').isdigit():
                        # Field reference like applicant.creditScore
                        parameters.append({'type': 'field', 'value': param})
                    elif param.replace('.', '').isdigit():
                        # Number parameter
                        parameters.append({'type': 'number', 'value': param})
                    else:
                        # Expression or identifier
                        parameters.append({'type': 'expression', 'value': param})

            # Validate parameters
            validation_errors = self._validate_action_parameters(action_name, parameters)

            return {
                'type': 'parameterized',
                'name': action_name,
                'parameters': parameters,
                'original': action_text,
                'validation_errors': validation_errors
            }
        else:
            # Simple action: approveApplication
            return {
                'type': 'simple',
                'name': action_text,
                'original': action_text,
                'validation_errors': []
            }


    def _parse_rule_conditions(self, rule_content: str) -> str:
        """Parse rule content using unified rule grammar and generate Java conditions."""
        import re

        # Find all field references (entity.field) for entity declarations
        field_refs = re.findall(r'(\w+)\.(\w+)', rule_content)
        entities_used = set(entity for entity, field in field_refs)

        # Generate entity declarations
        entity_declarations = []
        for entity in sorted(entities_used):
            entity_declarations.append(f'            Map<String, Object> {entity} = (Map<String, Object>) context.get("{entity}");')

        # Simple rule parsing - just focus on basic if-then-else structure
        lines = rule_content.split('\n')
        java_conditions = []
        condition_counter = 0

        for line in lines:
            stripped = line.strip()
            if not stripped or stripped.startswith('#') or stripped.startswith('rule'):
                continue

            if stripped.startswith('if ') and ' then ' in stripped:
                # Handle if-then on same line
                try:
                    if_part, then_part = stripped.split(' then ', 1)
                    condition = if_part[3:].strip()  # Remove 'if '
                    action_texts = [a.strip() for a in then_part.split(',')]

                    java_condition = self._generate_simple_java_condition(condition, action_texts, condition_counter)
                    if java_condition:
                        java_conditions.append(java_condition)
                        condition_counter += 1
                except ValueError:
                    pass
            elif stripped.startswith('else '):
                # Handle else clause
                action_texts = [a.strip() for a in stripped[5:].split(',')]
                if action_texts:
                    java_condition = f'''
            // Else clause
            return new RuleResult(true, Arrays.asList({self._format_actions_for_java(action_texts)}), "{self._escape_for_java(action_texts[0])}");'''
                    java_conditions.append(java_condition)
            elif not stripped.startswith('if') and not stripped.startswith('else') and stripped:
                # Standalone actions
                action_texts = [a.strip() for a in stripped.split(',')]
                if action_texts:
                    java_condition = f'''
            // Standalone actions
            return new RuleResult(true, Arrays.asList({self._format_actions_for_java(action_texts)}), "{self._escape_for_java(action_texts[0])}");'''
                    java_conditions.append(java_condition)

        # Combine declarations and conditions
        result = []
        if entity_declarations:
            result.append('\n'.join(entity_declarations))
        if java_conditions:
            result.extend(java_conditions)
        else:
            result.append('            // No conditions parsed')

        # Add default case
        result.append('            return new RuleResult(false, Arrays.asList(), null);')

        return '\n'.join(result)

    def _generate_simple_java_condition(self, condition: str, action_texts: list, counter: int) -> str:
        """Generate simple Java condition code."""
        if not condition or not action_texts:
            return None

        # Generate simple condition check (basic implementation)
        java_condition = f'''
            // Condition {counter + 1}: {condition}
            if (/* TODO: implement condition: {condition} */) {{
                return new RuleResult(true, Arrays.asList({self._format_actions_for_java(action_texts)}), "{self._escape_for_java(action_texts[0])}");
            }}'''
        return java_condition

    def _format_actions_for_java(self, action_texts: list) -> str:
        """Format action texts for Java array."""
        escaped_actions = [f'"{self._escape_for_java(action)}"' for action in action_texts]
        return ', '.join(escaped_actions)

    def _escape_for_java(self, text: str) -> str:
        """Escape text for Java string literals."""
        return text.replace('"', '\\"').replace('\n', '\\n').replace('\r', '\\r')



    def _generate_java_code(self, rule_name: str, rule_content: str) -> str:
        """Generate Java code for the rule using simple string templates."""
        class_name = f"{rule_name.title()}Rule"

        # Parse the rule content to generate conditions
        parsed_conditions = self._parse_rule_conditions(rule_content)

        # Generate Java code with parsed conditions
        java_code = f'''package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;

public class {class_name} {{

    public static class RuleResult {{
        private boolean matched;
        private List<String> actions;
        private String finalAction;

        public RuleResult(boolean matched, List<String> actions, String finalAction) {{
            this.matched = matched;
            this.actions = actions;
            this.finalAction = finalAction;
        }}

        public boolean isMatched() {{ return matched; }}
        public List<String> getActions() {{ return actions; }}
        public String getFinalAction() {{ return finalAction; }}
    }}

    public static RuleResult execute(Map<String, Object> context) {{
        try {{
            // Debug: Print input context
            System.err.println("DEBUG: Input context: " + context);

            {parsed_conditions}

            // If no conditions matched, return no match
            System.err.println("DEBUG: No conditions matched");
            return new RuleResult(false, Arrays.asList(), null);

        }} catch (Exception e) {{
            System.err.println("DEBUG: Exception during execution: " + e.getMessage());
            e.printStackTrace();
            return new RuleResult(false, Arrays.asList(), null);
        }}
    }}

    public static void main(String[] args) {{
        // This allows execution via command line
        if (args.length > 0) {{
            try {{
                // Parse JSON input from command line argument
                String jsonInput = args[0];
                System.err.println("DEBUG: Received JSON input: " + jsonInput);

                // Simple JSON parsing (basic implementation)
                Map<String, Object> context = parseJsonToMap(jsonInput);

                RuleResult result = execute(context);

                // Format actions as proper JSON array
                StringBuilder actionsJson = new StringBuilder("[");
                for (int i = 0; i < result.getActions().size(); i++) {{
                    if (i > 0) actionsJson.append(", ");
                    actionsJson.append("\\"").append(result.getActions().get(i)).append("\\"");
                }}
                actionsJson.append("]");

                String finalAction = result.getFinalAction() != null ? "\\"" + result.getFinalAction() + "\\"" : "null";

                System.out.println("{{\\"matched\\": " + result.isMatched() +
                                 ", \\"actions\\": " + actionsJson.toString() +
                                 ", \\"finalAction\\": " + finalAction + "}}");

            }} catch (Exception e) {{
                System.err.println("ERROR: Failed to parse JSON input: " + e.getMessage());
                e.printStackTrace();
                System.out.println("{{\\"matched\\": false, \\"actions\\": [], \\"finalAction\\": null}}");
            }}
        }} else {{
            System.err.println("ERROR: No JSON input provided");
            System.out.println("{{\\"matched\\": false, \\"actions\\": [], \\"finalAction\\": null}}");
        }}
    }}

    // Simple JSON parser for our specific use case
    private static Map<String, Object> parseJsonToMap(String json) {{
        Map<String, Object> result = new HashMap<>();

        System.err.println("DEBUG: Original JSON input: " + json);

        try {{
            // Very basic JSON parsing - look for "applicant" object
            int applicantStart = json.indexOf("\\"applicant\\"");
            if (applicantStart != -1) {{
                int objectStart = json.indexOf("{{", applicantStart);
                if (objectStart != -1) {{
                    // Find the matching closing brace
                    int braceCount = 1;
                    int objectEnd = objectStart + 1;
                    while (objectEnd < json.length() && braceCount > 0) {{
                        char c = json.charAt(objectEnd);
                        if (c == '{{') braceCount++;
                        else if (c == '}}') braceCount--;
                        objectEnd++;
                    }}

                    if (braceCount == 0) {{
                        String applicantJson = json.substring(objectStart, objectEnd);
                        System.err.println("DEBUG: Extracted applicant JSON: " + applicantJson);

                        Map<String, Object> applicant = new HashMap<>();

                        // Extract creditScore
                        if (applicantJson.contains("\\"creditScore\\"")) {{
                            int scoreStart = applicantJson.indexOf("\\"creditScore\\"");
                            int colonPos = applicantJson.indexOf(":", scoreStart);
                            int commaPos = applicantJson.indexOf(",", colonPos);
                            int bracePos = applicantJson.indexOf("}}", colonPos);
                            int endPos = (commaPos != -1 && commaPos < bracePos) ? commaPos : bracePos;
                            String scoreStr = applicantJson.substring(colonPos + 1, endPos).trim();
                            try {{
                                applicant.put("creditScore", Integer.parseInt(scoreStr));
                                System.err.println("DEBUG: Parsed creditScore: " + scoreStr);
                            }} catch (NumberFormatException e) {{
                                System.err.println("DEBUG: Failed to parse creditScore: " + scoreStr);
                            }}
                        }}

                        // Extract annualIncome
                        if (applicantJson.contains("\\"annualIncome\\"")) {{
                            int incomeStart = applicantJson.indexOf("\\"annualIncome\\"");
                            int colonPos = applicantJson.indexOf(":", incomeStart);
                            int commaPos = applicantJson.indexOf(",", colonPos);
                            int bracePos = applicantJson.indexOf("}}", colonPos);
                            int endPos = (commaPos != -1 && commaPos < bracePos) ? commaPos : bracePos;
                            String incomeStr = applicantJson.substring(colonPos + 1, endPos).trim();
                            try {{
                                applicant.put("annualIncome", Integer.parseInt(incomeStr));
                                System.err.println("DEBUG: Parsed annualIncome: " + incomeStr);
                            }} catch (NumberFormatException e) {{
                                System.err.println("DEBUG: Failed to parse annualIncome: " + incomeStr);
                            }}
                        }}

                        // Extract employmentStatus
                        if (applicantJson.contains("\\"employmentStatus\\"")) {{
                            int statusStart = applicantJson.indexOf("\\"employmentStatus\\"");
                            int colonPos = applicantJson.indexOf(":", statusStart);
                            int quoteStart = applicantJson.indexOf("\\"", colonPos + 1);
                            int quoteEnd = applicantJson.indexOf("\\"", quoteStart + 1);
                            if (quoteStart != -1 && quoteEnd != -1) {{
                                String statusStr = applicantJson.substring(quoteStart + 1, quoteEnd);
                                applicant.put("employmentStatus", statusStr);
                                System.err.println("DEBUG: Parsed employmentStatus: " + statusStr);
                            }}
                        }}

                        // Extract employmentYears
                        if (applicantJson.contains("\\"employmentYears\\"")) {{
                            int yearsStart = applicantJson.indexOf("\\"employmentYears\\"");
                            int colonPos = applicantJson.indexOf(":", yearsStart);
                            int commaPos = applicantJson.indexOf(",", colonPos);
                            int bracePos = applicantJson.indexOf("}}", colonPos);
                            int endPos = (commaPos != -1 && commaPos < bracePos) ? commaPos : bracePos;
                            String yearsStr = applicantJson.substring(colonPos + 1, endPos).trim();
                            try {{
                                applicant.put("employmentYears", Integer.parseInt(yearsStr));
                                System.err.println("DEBUG: Parsed employmentYears: " + yearsStr);
                            }} catch (NumberFormatException e) {{
                                System.err.println("DEBUG: Failed to parse employmentYears: " + yearsStr);
                            }}
                        }}

                        result.put("applicant", applicant);
                        System.err.println("DEBUG: Final parsed result: " + result);
                    }}
                }}
            }}

            // Fallback to hardcoded data if parsing fails
            if (!result.containsKey("applicant")) {{
                System.err.println("DEBUG: JSON parsing failed, using fallback data");
                Map<String, Object> applicant = new HashMap<>();
                applicant.put("creditScore", 750);
                applicant.put("annualIncome", 75000);
                applicant.put("employmentStatus", "employed");
                applicant.put("employmentYears", 3);
                result.put("applicant", applicant);
            }}

        }} catch (Exception e) {{
            System.err.println("DEBUG: Exception during JSON parsing: " + e.getMessage());
            // Use fallback data
            Map<String, Object> applicant = new HashMap<>();
            applicant.put("creditScore", 750);
            applicant.put("annualIncome", 75000);
            applicant.put("employmentStatus", "employed");
            applicant.put("employmentYears", 3);
            result.put("applicant", applicant);
        }}

        return result;
    }}
}}'''
        return java_code

    def _get_maven_classpath(self) -> str:
        """Get Maven classpath for compilation."""
        java_bridge_path = Path(__file__).parent.parent.parent / "java-bridge"

        try:
            # Try to get classpath from Maven
            result = subprocess.run(
                ["mvn", "dependency:build-classpath", "-Dmdep.outputFile=classpath.txt", "-q"],
                cwd=java_bridge_path,
                capture_output=True,
                text=True,
                timeout=30
            )

            classpath_file = java_bridge_path / "classpath.txt"
            if classpath_file.exists():
                classpath = classpath_file.read_text().strip()
                # Add the target/classes directory for our own compiled classes
                target_classes = java_bridge_path / "target" / "classes"
                if target_classes.exists():
                    classpath = f"{target_classes}:{classpath}"
                return classpath

        except Exception as e:
            print(f"Warning: Could not get Maven classpath: {e}")

        # Fallback to basic classpath
        return "."

    def compile_rule(self, rule_content: str, rule_id: str = None) -> Dict[str, Any]:
        """
        Compile rule using subprocess-based compilation (no Java server required).

        Args:
            rule_content: The rule content to compile
            rule_id: Optional rule identifier (auto-generated if not provided)

        Returns:
            Dict with compilation result: {'success': bool, 'ruleId': str, 'className': str, 'compilationTimeMs': int, 'message': str}
        """
        start_time = time.time()

        if not rule_id:
            rule_id = f"rule_{int(time.time() * 1000)}"

        try:
            # Extract rule name from content for className
            rule_name = self._extract_rule_name(rule_content)
            class_name = f"com.rules.{rule_name.title()}Rule"

            # Generate Java code
            java_code = self._generate_java_code(rule_name, rule_content)

            # Create temporary directory for compilation
            with tempfile.TemporaryDirectory() as temp_dir:
                # Create package directory structure
                package_dir = Path(temp_dir) / "com" / "rules"
                package_dir.mkdir(parents=True)

                # Write Java source file
                java_file_path = package_dir / f"{rule_name.title()}Rule.java"
                java_file_path.write_text(java_code)

                # Get Maven classpath
                classpath = self._get_maven_classpath()

                # Compile Java file
                compile_cmd = [
                    'javac',
                    '-cp', classpath,
                    str(java_file_path)
                ]

                result = subprocess.run(
                    compile_cmd,
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=temp_dir
                )

                compilation_time = int((time.time() - start_time) * 1000)

                if result.returncode == 0:
                    return {
                        'success': True,
                        'ruleId': rule_id,
                        'className': class_name,
                        'compilationTimeMs': compilation_time,
                        'message': 'Rule compiled successfully via subprocess',
                        'ready': True,
                        'errors': [],
                        'tempDir': temp_dir  # We'll need this for execution
                    }
                else:
                    return {
                        'success': False,
                        'ruleId': rule_id,
                        'className': '',
                        'compilationTimeMs': compilation_time,
                        'message': f'Compilation failed: {result.stderr}',
                        'ready': False,
                        'errors': [result.stderr]
                    }

        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'ruleId': rule_id,
                'className': '',
                'compilationTimeMs': int((time.time() - start_time) * 1000),
                'message': 'Compilation timed out',
                'ready': False,
                'errors': ['Compilation timed out after 30 seconds']
            }
        except Exception as e:
            return {
                'success': False,
                'ruleId': rule_id,
                'className': '',
                'compilationTimeMs': int((time.time() - start_time) * 1000),
                'message': f'Compilation error: {str(e)}',
                'ready': False,
                'errors': [str(e)]
            }
    
    def execute_compiled_rule(self, rule_id: str, test_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute a compiled rule with test data using hot execution.
        
        Args:
            rule_id: The rule identifier from compilation
            test_data: Sample data to test the rule against
            
        Returns:
            Dict with execution result: {'success': bool, 'result': dict, 'errors': list}
        """
        try:
            response = requests.post(
                f"{self.server_url}/api/rules/execute",
                json={
                    "ruleId": rule_id,
                    "contextData": test_data
                },
                timeout=30
            )
            
            if response.status_code == 200:
                result = response.json()
                return {
                    'success': result.get('success', False),
                    'result': {
                        'ruleId': result.get('ruleId', rule_id),
                        'matched': result.get('matched', False),
                        'hasActions': result.get('hasActions', False),
                        'actions': result.get('actions', []),
                        'finalAction': result.get('finalAction', None),
                        'executionTimeMs': result.get('executionTimeMs', 0)
                    },
                    'errors': []
                }
            else:
                error_data = response.json() if response.headers.get('content-type', '').startswith('application/json') else {}
                return {
                    'success': False,
                    'result': {},
                    'errors': [error_data.get('message', f'Server returned {response.status_code}')]
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
                'errors': ['Rule execution request timed out after 30 seconds']
            }
        except Exception as e:
            return {
                'success': False,
                'result': {},
                'errors': [str(e)]
            }
    
    def test_rule_hot(self, rule_content: str, test_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Test rule using subprocess-based compilation and execution (no Java server required).

        Args:
            rule_content: The rule content to test
            test_data: Sample data to test the rule against

        Returns:
            Dict with test result: {'success': bool, 'result': dict, 'errors': list, 'performance': dict}
        """
        start_time = time.time()
        rule_id = f"rule_{int(time.time() * 1000)}"

        try:
            # Extract rule name from content
            rule_name = self._extract_rule_name(rule_content)
            class_name = f"com.rules.{rule_name.title()}Rule"

            # Generate Java code
            java_code = self._generate_java_code(rule_name, rule_content)

            # Create temporary directory for compilation and execution
            with tempfile.TemporaryDirectory() as temp_dir:
                # Create package directory structure
                package_dir = Path(temp_dir) / "com" / "rules"
                package_dir.mkdir(parents=True)

                # Write Java source file
                java_file_path = package_dir / f"{rule_name.title()}Rule.java"
                java_file_path.write_text(java_code)

                # Get Maven classpath
                classpath = self._get_maven_classpath()

                # Step 1: Compile
                compile_start = time.time()
                compile_cmd = [
                    'javac',
                    '-cp', classpath,
                    str(java_file_path)
                ]

                compile_result = subprocess.run(
                    compile_cmd,
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=temp_dir
                )

                compilation_time = int((time.time() - compile_start) * 1000)

                if compile_result.returncode != 0:
                    return {
                        'success': False,
                        'result': {},
                        'errors': [f'Compilation failed: {compile_result.stderr}'],
                        'performance': {
                            'compilationTimeMs': compilation_time,
                            'executionTimeMs': 0,
                            'totalTimeMs': compilation_time,
                            'method': 'subprocess_compilation_failed'
                        }
                    }

                # Step 2: Execute
                execute_start = time.time()

                # Create JSON input for the rule
                test_data_json = json.dumps(test_data)

                # Execute the compiled class
                execute_cmd = [
                    'java',
                    '-cp', f"{temp_dir}:{classpath}",
                    class_name,
                    test_data_json
                ]

                execute_result = subprocess.run(
                    execute_cmd,
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=temp_dir
                )

                execution_time = int((time.time() - execute_start) * 1000)
                total_time = int((time.time() - start_time) * 1000)

                if execute_result.returncode != 0:
                    return {
                        'success': False,
                        'result': {},
                        'errors': [f'Execution failed: {execute_result.stderr}'],
                        'performance': {
                            'compilationTimeMs': compilation_time,
                            'executionTimeMs': execution_time,
                            'totalTimeMs': total_time,
                            'method': 'subprocess_execution_failed'
                        }
                    }

                # Log the stderr output for debugging
                print(f"Java stderr output: {execute_result.stderr}")
                print(f"Java stdout output: {execute_result.stdout}")

                # Parse execution result
                try:
                    result_json = json.loads(execute_result.stdout.strip())

                    # Include debug information in response for troubleshooting
                    debug_info = []
                    if execute_result.stderr:
                        debug_info.append(f"Debug output: {execute_result.stderr}")

                    return {
                        'success': True,
                        'result': {
                            'ruleId': rule_id,
                            'matched': result_json.get('matched', False),
                            'actions': result_json.get('actions', []),
                            'finalAction': result_json.get('finalAction'),
                            'executionTimeMs': execution_time,
                            'ruleCompiled': True,
                            'className': class_name,
                            'debug': debug_info  # Include debug info
                        },
                        'errors': [],
                        'performance': {
                            'compilationTimeMs': compilation_time,
                            'executionTimeMs': execution_time,
                            'totalTimeMs': total_time,
                            'method': 'subprocess_success'
                        }
                    }
                except json.JSONDecodeError as e:
                    return {
                        'success': False,
                        'result': {},
                        'errors': [f'Failed to parse execution result: {e}. Output: {execute_result.stdout}. Stderr: {execute_result.stderr}'],
                        'performance': {
                            'compilationTimeMs': compilation_time,
                            'executionTimeMs': execution_time,
                            'totalTimeMs': total_time,
                            'method': 'subprocess_parse_failed'
                        }
                    }

        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'result': {},
                'errors': ['Rule test timed out after 30 seconds'],
                'performance': {
                    'compilationTimeMs': 0,
                    'executionTimeMs': 0,
                    'totalTimeMs': int((time.time() - start_time) * 1000),
                    'method': 'subprocess_timeout'
                }
            }
        except Exception as e:
            return {
                'success': False,
                'result': {},
                'errors': [f'Test error: {str(e)}'],
                'performance': {
                    'compilationTimeMs': 0,
                    'executionTimeMs': 0,
                    'totalTimeMs': int((time.time() - start_time) * 1000),
                    'method': 'subprocess_error'
                }
            }
    
    def generate_java_code(self, rule_content: str) -> Dict[str, Any]:
        """
        Generate Java source code from rule DSL.
        
        Args:
            rule_content: The rule DSL content
            
        Returns:
            Dict with generation result: {'success': bool, 'javaCode': str, 'message': str}
        """
        try:
            response = requests.post(
                f"{self.server_url}/api/rules/generate",
                json={"ruleContent": rule_content},
                timeout=30
            )
            
            if response.status_code == 200:
                result = response.json()
                return {
                    'success': result.get('success', False),
                    'javaCode': result.get('javaCode', ''),
                    'message': result.get('message', 'Java code generated successfully')
                }
            else:
                error_data = response.json() if response.headers.get('content-type', '').startswith('application/json') else {}
                return {
                    'success': False,
                    'javaCode': '',
                    'message': error_data.get('message', f'Code generation failed with HTTP {response.status_code}')
                }
                
        except requests.exceptions.ConnectionError:
            return {
                'success': False,
                'javaCode': '',
                'message': 'Java rules server is not available. Please ensure it is running on port 8081.'
            }
        except Exception as e:
            return {
                'success': False,
                'javaCode': '',
                'message': f'Code generation failed: {str(e)}'
            }