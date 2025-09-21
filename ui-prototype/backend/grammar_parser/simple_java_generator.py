"""
Simplified Java Code Generator
Generates basic Java code without complex visitor pattern.
"""

import re
from typing import Dict, Any


class SimpleJavaCodeGenerator:
    """Simple Java code generator using regex parsing."""

    def __init__(self):
        self.rule_name = "UnknownRule"

    def generate(self, rule_content: str, rule_name: str = None) -> str:
        """
        Generate Java code from rule content.

        Args:
            rule_content: Rule content string
            rule_name: Optional rule name

        Returns:
            str: Generated Java code
        """
        if rule_name:
            self.rule_name = rule_name
        else:
            # Extract rule name from content
            name_match = re.search(r'rule\s+(?:"([^"]+)"|(\w+))\s*:', rule_content)
            if name_match:
                self.rule_name = name_match.group(1) or name_match.group(2)

        # Parse rule structure
        rule_info = self._parse_rule_structure(rule_content)

        # Generate Java class
        return self._generate_java_class(rule_info)

    def _parse_rule_structure(self, rule_content: str) -> Dict[str, Any]:
        """Parse rule content into structured information."""
        lines = rule_content.strip().split('\n')

        rule_info = {
            'name': self.rule_name,
            'conditions': [],
            'actions': [],
            'entities_used': set()
        }

        for line in lines:
            line = line.strip()
            if not line or line.startswith('#') or line.startswith('rule'):
                continue

            # Find entity references
            entity_refs = re.findall(r'(\w+)\.(\w+)', line)
            for entity, field in entity_refs:
                rule_info['entities_used'].add(entity)

            # Parse if-then-else structure
            if line.lower().startswith('if '):
                # Extract condition
                condition_match = re.search(r'if\s+(.+?)\s+then\s+(.+)', line, re.IGNORECASE)
                if condition_match:
                    condition = condition_match.group(1)
                    action = condition_match.group(2)
                    rule_info['conditions'].append({
                        'condition': condition,
                        'then_actions': self._parse_actions(action),
                        'else_actions': []
                    })
                else:
                    # Just condition, action on next line
                    condition = re.sub(r'if\s+', '', line, flags=re.IGNORECASE).strip()
                    rule_info['conditions'].append({
                        'condition': condition,
                        'then_actions': [],
                        'else_actions': []
                    })

            elif line.lower().startswith('then '):
                # Actions for the last condition
                action_text = re.sub(r'then\s+', '', line, flags=re.IGNORECASE).strip()
                if rule_info['conditions']:
                    rule_info['conditions'][-1]['then_actions'] = self._parse_actions(action_text)

            elif line.lower().startswith('else '):
                # Else actions
                action_text = re.sub(r'else\s+', '', line, flags=re.IGNORECASE).strip()
                if rule_info['conditions']:
                    rule_info['conditions'][-1]['else_actions'] = self._parse_actions(action_text)

            elif not any(keyword in line.lower() for keyword in ['if ', 'then ', 'else ', 'rule ']):
                # Standalone action
                rule_info['actions'].extend(self._parse_actions(line))

        return rule_info

    def _parse_actions(self, action_text: str) -> list:
        """Parse action text into list of actions."""
        if not action_text:
            return []

        # Split by comma and clean up
        actions = []
        for action in action_text.split(','):
            action = action.strip()
            if action:
                actions.append(action)

        return actions

    def _generate_java_class(self, rule_info: Dict[str, Any]) -> str:
        """Generate complete Java class."""
        class_name = self._to_class_name(rule_info['name'])

        java_code = f'''package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

public class {class_name}Rule {{

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

    public static RuleResult evaluate(Map<String, Object> context) {{
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = false;

{self._generate_entity_declarations(rule_info['entities_used'])}

{self._generate_rule_logic(rule_info)}

        return new RuleResult(matched, actions, finalAction);
    }}

{self._generate_helper_methods()}
}}'''

        return java_code

    def _generate_entity_declarations(self, entities_used: set) -> str:
        """Generate entity variable declarations."""
        if not entities_used:
            return ""

        declarations = []
        for entity in sorted(entities_used):
            declarations.append(f'        Map<String, Object> {entity} = (Map<String, Object>) context.get("{entity}");')

        return '\n'.join(declarations) + '\n'

    def _generate_rule_logic(self, rule_info: Dict[str, Any]) -> str:
        """Generate main rule logic."""
        logic_lines = []

        # Generate conditions
        for condition_info in rule_info['conditions']:
            condition_java = self._convert_condition_to_java(condition_info['condition'])

            logic_lines.append(f'        if ({condition_java}) {{')
            logic_lines.append('            matched = true;')

            # Generate then actions
            for action in condition_info['then_actions']:
                action_java = self._convert_action_to_java(action)
                logic_lines.append(f'            {action_java}')

            logic_lines.append('        }')

            # Generate else actions if present
            if condition_info['else_actions']:
                logic_lines.append('        else {')
                for action in condition_info['else_actions']:
                    action_java = self._convert_action_to_java(action)
                    logic_lines.append(f'            {action_java}')
                logic_lines.append('        }')

        # Generate standalone actions
        if rule_info['actions']:
            logic_lines.append('        matched = true;')
            for action in rule_info['actions']:
                action_java = self._convert_action_to_java(action)
                logic_lines.append(f'        {action_java}')

        return '\n'.join(logic_lines)

    def _convert_condition_to_java(self, condition: str) -> str:
        """Convert rule condition to Java code."""
        # Clean up the condition string
        java_condition = condition.strip()

        # Remove 'then' if it appears at the end
        java_condition = re.sub(r'\s+then\s*$', '', java_condition, flags=re.IGNORECASE)

        # Convert operators
        java_condition = java_condition.replace(' and ', ' && ')
        java_condition = java_condition.replace(' or ', ' || ')
        java_condition = java_condition.replace(' not ', ' !')

        # Convert attribute references to method calls
        java_condition = re.sub(
            r'(\w+)\.(\w+)',
            r'_getFieldValue(\1, "\2")',
            java_condition
        )

        # Convert comparisons to use helper methods
        java_condition = re.sub(
            r'(_getFieldValue\([^)]+\))\s*>\s*(\d+)',
            r'_compareTo(\1, \2) > 0',
            java_condition
        )

        java_condition = re.sub(
            r'(_getFieldValue\([^)]+\))\s*>=\s*(\d+)',
            r'_compareTo(\1, \2) >= 0',
            java_condition
        )

        java_condition = re.sub(
            r'(_getFieldValue\([^)]+\))\s*<\s*(\d+)',
            r'_compareTo(\1, \2) < 0',
            java_condition
        )

        java_condition = re.sub(
            r'(_getFieldValue\([^)]+\))\s*<=\s*(\d+)',
            r'_compareTo(\1, \2) <= 0',
            java_condition
        )

        return java_condition

    def _convert_action_to_java(self, action: str) -> str:
        """Convert rule action to Java code."""
        action = action.strip()

        # Check if action has parameters
        param_match = re.match(r'(\w+)\s*\(([^)]*)\)', action)
        if param_match:
            action_name = param_match.group(1)
            params = param_match.group(2)
            return f'actions.add("{action_name}({params})");'
        else:
            # Simple action name
            return f'actions.add("{action}");'

    def _generate_helper_methods(self) -> str:
        """Generate helper methods for field access."""
        return '''
    private static Object _getFieldValue(Map<String, Object> entity, String fieldName) {
        return entity != null ? entity.get(fieldName) : null;
    }

    private static boolean _equals(Object a, Object b) {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        return a.toString().equals(b.toString());
    }

    private static int _compareTo(Object a, Object b) {
        if (a == null || b == null) return 0;
        try {
            if (a instanceof Number && b instanceof Number) {
                return Double.compare(((Number)a).doubleValue(), ((Number)b).doubleValue());
            }
            return a.toString().compareTo(b.toString());
        } catch (Exception e) {
            return 0;
        }
    }'''

    def _to_class_name(self, name: str) -> str:
        """Convert rule name to valid Java class name."""
        # Remove quotes and special characters
        name = re.sub(r'[^a-zA-Z0-9_]', '', name)
        # Capitalize first letter
        if name:
            return name[0].upper() + name[1:]
        return "DefaultRule"