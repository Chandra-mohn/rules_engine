"""
Test scenario generator for DSL rules.
Analyzes rule conditions to generate comprehensive test cases.
"""

from pathlib import Path
import sys

# Import ANTLR generated classes
sys.path.append(str(Path(__file__).parent.parent / 'java-bridge' / 'src' / 'main' / 'antlr4' / 'com' / 'rules' / 'grammar'))
from RulesParser import RulesParser
from RulesListener import RulesListener


class TestScenarioGenerator(RulesListener):
    """
    ANTLR listener to extract test scenarios from rule parse tree.
    Analyzes conditions and actions to create positive and negative test cases.
    """

    def __init__(self):
        self.rule_name = None
        self.entities = set()
        self.scenarios = []
        self.current_conditions = []
        self.current_actions = []
        self.rule_type = 'standard'  # standard, actionset, or action

    def enterRule(self, ctx):
        """Extract rule name and type."""
        if ctx.ruleName():
            if hasattr(ctx.ruleName(), 'IDENTIFIER') and ctx.ruleName().IDENTIFIER():
                self.rule_name = ctx.ruleName().IDENTIFIER().getText()
            elif hasattr(ctx.ruleName(), 'STRING') and ctx.ruleName().STRING():
                self.rule_name = ctx.ruleName().STRING().getText().strip('"\'')
            else:
                self.rule_name = ctx.ruleName().getText().strip('"\'')

    def enterRuleStep(self, ctx):
        """Detect rule type and extract scenarios."""
        print(f"DEBUG: enterRuleStep called, has IF: {ctx.IF() is not None}")  # DEBUG
        if ctx.IF():
            print(f"DEBUG: Processing as conditional rule")  # DEBUG
            self.rule_type = 'standard'
            self._extract_conditional_scenarios(ctx)
        else:
            print(f"DEBUG: Processing as action rule")  # DEBUG
            self.rule_type = 'action'
            self._extract_action_scenarios(ctx)

    def _extract_conditional_scenarios(self, ctx):
        """Extract test scenarios from conditional rule step."""

        print(f"DEBUG: Extracting conditional scenarios, has condition: {ctx.condition(0) is not None}")  # DEBUG

        # Extract main if clause
        if ctx.condition(0):
            print(f"DEBUG: Analyzing main condition")  # DEBUG
            condition_info = self._analyze_condition(ctx.condition(0))
            action_list = self._extract_action_names(ctx.actionList(0))

            # Positive scenario: condition is true
            positive_scenario = {
                'name': f'test_{self._sanitize_name(condition_info["description"])}_true',
                'description': f'Should match when {condition_info["description"]}',
                'entity_values': condition_info['positive_values'],
                'expected_matched': True,
                'expected_actions': action_list,
                'expected_final_action': None
            }
            self.scenarios.append(positive_scenario)

            # Negative scenario: condition is false
            negative_scenario = {
                'name': f'test_{self._sanitize_name(condition_info["description"])}_false',
                'description': f'Should not match when {condition_info["description"]} is false',
                'entity_values': condition_info['negative_values'],
                'expected_matched': False,
                'expected_actions': [],
                'expected_final_action': None
            }
            self.scenarios.append(negative_scenario)

        # Extract elseif clauses
        num_conditions = len(ctx.condition())
        for i in range(1, num_conditions):
            condition_info = self._analyze_condition(ctx.condition(i))
            action_list = self._extract_action_names(ctx.actionList(i))

            scenario = {
                'name': f'test_elseif_{i}_{self._sanitize_name(condition_info["description"])}',
                'description': f'Should match elseif when {condition_info["description"]}',
                'entity_values': condition_info['positive_values'],
                'expected_matched': True,
                'expected_actions': action_list,
                'expected_final_action': None
            }
            self.scenarios.append(scenario)

        # Extract else clause if present
        if ctx.ELSE():
            else_actions = self._extract_action_names(ctx.actionList(-1))
            scenario = {
                'name': 'test_else_clause',
                'description': 'Should match when all conditions are false',
                'entity_values': self._generate_else_values(),
                'expected_matched': True,
                'expected_actions': else_actions,
                'expected_final_action': None
            }
            self.scenarios.append(scenario)

    def _extract_action_scenarios(self, ctx):
        """Extract test scenarios from direct action rule."""

        action_list = self._extract_action_names(ctx.actionList(0))

        scenario = {
            'name': 'test_direct_action_execution',
            'description': 'Should always execute direct actions',
            'entity_values': self._generate_default_values(),
            'expected_matched': True,
            'expected_actions': action_list,
            'expected_final_action': None
        }
        self.scenarios.append(scenario)

    def _analyze_condition(self, ctx):
        """
        Analyze a condition to extract:
        - Human-readable description
        - Entity values that make it true (positive case)
        - Entity values that make it false (negative case)
        """

        description = self._describe_condition(ctx)

        # Extract field references
        fields = self._extract_field_references(ctx)
        print(f"DEBUG: Extracted {len(fields)} fields from condition")  # DEBUG
        for field in fields:
            print(f"  Field: {field}")  # DEBUG

        positive_values = self._generate_positive_values(ctx)
        negative_values = self._generate_negative_values(ctx)

        return {
            'description': description,
            'positive_values': positive_values,
            'negative_values': negative_values
        }

    def _describe_condition(self, ctx):
        """Generate human-readable description of condition."""

        # Simple text representation for now
        # In production, this would parse the condition tree more intelligently
        text = ctx.getText()

        # Clean up for readability
        text = text.replace('&&', 'AND')
        text = text.replace('||', 'OR')
        text = text.replace('==', 'equals')
        text = text.replace('!=', 'not equals')
        text = text.replace('>=', 'greater than or equal to')
        text = text.replace('<=', 'less than or equal to')
        text = text.replace('>', 'greater than')
        text = text.replace('<', 'less than')

        return text[:80]  # Truncate long conditions

    def _generate_positive_values(self, ctx):
        """Generate entity values that make the condition true."""

        # Extract field references and operators
        fields = self._extract_field_references(ctx)
        values = {}

        for field_info in fields:
            entity = field_info['entity']
            field_path = field_info['field_path']
            operator = field_info['operator']
            comparison_value = field_info['value']

            if entity not in values:
                values[entity] = {}

            # Generate value based on operator
            if operator == '==':
                self._set_nested_value(values[entity], field_path, comparison_value)
            elif operator == '!=':
                # Use different value
                different_val = self._generate_different_value(comparison_value)
                self._set_nested_value(values[entity], field_path, different_val)
            elif operator == '>':
                greater_val = self._generate_greater_value(comparison_value)
                self._set_nested_value(values[entity], field_path, greater_val)
            elif operator == '<':
                lesser_val = self._generate_lesser_value(comparison_value)
                self._set_nested_value(values[entity], field_path, lesser_val)
            elif operator == '>=':
                self._set_nested_value(values[entity], field_path, comparison_value)
            elif operator == '<=':
                self._set_nested_value(values[entity], field_path, comparison_value)
            else:
                # Default: use comparison value
                self._set_nested_value(values[entity], field_path, comparison_value)

        return values

    def _generate_negative_values(self, ctx):
        """Generate entity values that make the condition false."""

        fields = self._extract_field_references(ctx)
        values = {}

        for field_info in fields:
            entity = field_info['entity']
            field_path = field_info['field_path']
            operator = field_info['operator']
            comparison_value = field_info['value']

            if entity not in values:
                values[entity] = {}

            # Generate opposite value based on operator
            if operator == '==':
                different_val = self._generate_different_value(comparison_value)
                self._set_nested_value(values[entity], field_path, different_val)
            elif operator == '!=':
                # Use same value to make != false
                self._set_nested_value(values[entity], field_path, comparison_value)
            elif operator == '>':
                lesser_val = self._generate_lesser_value(comparison_value)
                self._set_nested_value(values[entity], field_path, lesser_val)
            elif operator == '<':
                greater_val = self._generate_greater_value(comparison_value)
                self._set_nested_value(values[entity], field_path, greater_val)
            elif operator == '>=':
                lesser_val = self._generate_lesser_value(comparison_value)
                self._set_nested_value(values[entity], field_path, lesser_val)
            elif operator == '<=':
                greater_val = self._generate_greater_value(comparison_value)
                self._set_nested_value(values[entity], field_path, greater_val)
            else:
                # Default: use different value
                different_val = self._generate_different_value(comparison_value)
                self._set_nested_value(values[entity], field_path, different_val)

        return values

    def _extract_field_references(self, ctx):
        """Extract all field references from condition with their operators and values."""

        fields = []

        # Recursively walk condition tree
        self._walk_condition(ctx, fields)

        return fields

    def _walk_condition(self, ctx, fields):
        """Recursively walk condition tree to find comparisons."""

        # The condition context should have orExpression as a child
        # In ANTLR Python, methods that return rule contexts don't have () - they're direct attributes
        if hasattr(ctx, 'orExpression'):
            # Try calling it to see if it's callable
            or_expr_attr = getattr(ctx, 'orExpression')
            if callable(or_expr_attr):
                or_expr = or_expr_attr()
                if or_expr:
                    self._walk_or_expression(or_expr, fields)
            else:
                # It's a direct attribute
                if or_expr_attr:
                    self._walk_or_expression(or_expr_attr, fields)

    def _walk_or_expression(self, ctx, fields):
        """Walk OR expression nodes."""
        if hasattr(ctx, 'andExpression'):
            and_exprs = ctx.andExpression()
            if and_exprs:
                if hasattr(and_exprs, '__iter__'):
                    for and_expr in and_exprs:
                        self._walk_and_expression(and_expr, fields)
                else:
                    self._walk_and_expression(and_exprs, fields)

    def _walk_and_expression(self, ctx, fields):
        """Walk AND expression nodes."""
        if hasattr(ctx, 'comparisonExpression'):
            comp_exprs = ctx.comparisonExpression()
            if comp_exprs:
                if hasattr(comp_exprs, '__iter__'):
                    for comp_expr in comp_exprs:
                        self._extract_comparison(comp_expr, fields)
                else:
                    self._extract_comparison(comp_exprs, fields)

    def _extract_comparison(self, ctx, fields):
        """Extract comparison from comparisonExpression."""
        # Check for comparison operator
        if hasattr(ctx, 'COMP_OP') and ctx.COMP_OP():
            operator = ctx.COMP_OP().getText()

            # Get left and right expressions
            additive_exprs = ctx.additiveExpression()
            if additive_exprs and len(additive_exprs) >= 2:
                left_expr = additive_exprs[0]
                right_expr = additive_exprs[1]

                # Extract field info from left side
                field_info = self._extract_field_info(left_expr)
                if field_info:
                    field_info['operator'] = operator
                    field_info['value'] = self._extract_value(right_expr)
                    fields.append(field_info)

    def _extract_field_info(self, expr_ctx):
        """Extract entity and field path from expression."""

        # Look for attribute node in expression tree
        if hasattr(expr_ctx, 'multiplicativeExpression'):
            mult_expr = expr_ctx.multiplicativeExpression()
            if mult_expr and hasattr(mult_expr, '__iter__'):
                mult_expr = mult_expr[0]

            if mult_expr and hasattr(mult_expr, 'unaryExpression'):
                unary_expr = mult_expr.unaryExpression()
                if unary_expr and hasattr(unary_expr, '__iter__'):
                    unary_expr = unary_expr[0]

                if unary_expr and hasattr(unary_expr, 'primaryExpression'):
                    primary_expr = unary_expr.primaryExpression()
                    if primary_expr and hasattr(primary_expr, 'attribute'):
                        attr = primary_expr.attribute()
                        if attr:
                            # Parse attribute identifiers
                            attr_ids = attr.attributeIdentifier()
                            if attr_ids and len(attr_ids) >= 2:
                                entity = attr_ids[0].IDENTIFIER().getText() if attr_ids[0].IDENTIFIER() else None
                                field_parts = []
                                for i in range(1, len(attr_ids)):
                                    if attr_ids[i].IDENTIFIER():
                                        field_parts.append(attr_ids[i].IDENTIFIER().getText())

                                if entity and field_parts:
                                    return {
                                        'entity': entity,
                                        'field_path': '.'.join(field_parts),
                                        'operator': None,
                                        'value': None
                                    }

        # Fallback: parse text representation
        text = expr_ctx.getText() if expr_ctx else ''
        parts = text.split('.')
        if len(parts) >= 2:
            return {
                'entity': parts[0],
                'field_path': '.'.join(parts[1:]),
                'operator': None,
                'value': None
            }

        return None

    def _extract_value(self, expr_ctx):
        """Extract literal value from expression."""

        # Try to find literal in parse tree
        if hasattr(expr_ctx, 'multiplicativeExpression'):
            mult_expr = expr_ctx.multiplicativeExpression()
            if mult_expr and hasattr(mult_expr, '__iter__'):
                mult_expr = mult_expr[0]

            if mult_expr and hasattr(mult_expr, 'unaryExpression'):
                unary_expr = mult_expr.unaryExpression()
                if unary_expr and hasattr(unary_expr, '__iter__'):
                    unary_expr = unary_expr[0]

                if unary_expr and hasattr(unary_expr, 'primaryExpression'):
                    primary_expr = unary_expr.primaryExpression()

                    # Check for number
                    if primary_expr and hasattr(primary_expr, 'NUMBER') and primary_expr.NUMBER():
                        num_text = primary_expr.NUMBER().getText()
                        try:
                            return int(num_text) if '.' not in num_text else float(num_text)
                        except ValueError:
                            return num_text

                    # Check for string
                    if primary_expr and hasattr(primary_expr, 'STRING') and primary_expr.STRING():
                        str_text = primary_expr.STRING().getText()
                        return str_text.strip('"\'')

                    # Check for single-quoted string
                    if primary_expr and hasattr(primary_expr, 'SQUOTED_STRING') and primary_expr.SQUOTED_STRING():
                        str_text = primary_expr.SQUOTED_STRING().getText()
                        return str_text.strip('"\'')

        # Fallback: parse text
        text = expr_ctx.getText() if expr_ctx else ''

        # Try to parse as number
        try:
            if '.' in text:
                return float(text)
            else:
                return int(text)
        except ValueError:
            pass

        # Try to parse as string (remove quotes)
        if text.startswith('"') and text.endswith('"'):
            return text[1:-1]
        if text.startswith("'") and text.endswith("'"):
            return text[1:-1]

        # Return as-is
        return text

    def _set_nested_value(self, entity_dict, field_path, value):
        """Set nested value in entity dictionary (e.g., 'location.country' = 'US')."""

        parts = field_path.split('.')
        current = entity_dict

        for part in parts[:-1]:
            if part not in current:
                current[part] = {}
            current = current[part]

        current[parts[-1]] = value

    def _generate_different_value(self, value):
        """Generate a different value of the same type."""

        if isinstance(value, str):
            return 'DIFFERENT_' + value if value else 'DIFFERENT'
        elif isinstance(value, int):
            return value + 100
        elif isinstance(value, float):
            return value + 100.0
        elif isinstance(value, bool):
            return not value
        else:
            return 'DIFFERENT'

    def _generate_greater_value(self, value):
        """Generate a value greater than the given value."""

        if isinstance(value, (int, float)):
            return value + 100
        else:
            return value

    def _generate_lesser_value(self, value):
        """Generate a value less than the given value."""

        if isinstance(value, (int, float)):
            return value - 100 if value > 100 else 0
        else:
            return value

    def _generate_else_values(self):
        """Generate entity values for else clause (all conditions false)."""
        # Return minimal values
        return {entity: {} for entity in self.entities}

    def _generate_default_values(self):
        """Generate default entity values."""
        return {entity: {} for entity in self.entities}

    def _extract_action_names(self, action_list_ctx):
        """Extract action names from action list."""

        if not action_list_ctx:
            return []

        actions = []
        for action_ctx in action_list_ctx.action():
            action_name = None

            if hasattr(action_ctx, 'IDENTIFIER') and action_ctx.IDENTIFIER():
                action_name = action_ctx.IDENTIFIER().getText()
            elif hasattr(action_ctx, 'STRING') and action_ctx.STRING():
                action_name = action_ctx.STRING().getText().strip('"\'')

            # Include parameters if present
            if action_name and hasattr(action_ctx, 'parameterList') and action_ctx.parameterList():
                action_name += '(params)'  # Placeholder for parameterized actions

            if action_name:
                actions.append(action_name)

        return actions

    def _sanitize_name(self, text):
        """Convert text to valid Java method name."""

        # Remove special characters, replace spaces with underscores
        sanitized = ''.join(c if c.isalnum() or c == '_' else '_' for c in text)

        # Remove consecutive underscores
        while '__' in sanitized:
            sanitized = sanitized.replace('__', '_')

        # Ensure it starts with letter or underscore
        if sanitized and sanitized[0].isdigit():
            sanitized = '_' + sanitized

        return sanitized.lower()[:50]  # Limit length

    def enterAttribute(self, ctx):
        """Extract entity names from attributes."""
        if ctx.attributeIdentifier() and len(ctx.attributeIdentifier()) > 0:
            first_attr = ctx.attributeIdentifier(0)
            if hasattr(first_attr, 'IDENTIFIER') and first_attr.IDENTIFIER():
                entity = first_attr.IDENTIFIER().getText()
                self.entities.add(entity)

    def get_scenarios(self):
        """Return generated test scenarios."""
        return self.scenarios
