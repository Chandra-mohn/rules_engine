"""
Template-based Java code generator using plain Python f-strings.
No external template dependencies (no Jinja2).
Walks ANTLR parse tree to extract structured data, then generates via Python templates.
"""

from pathlib import Path
import sys

# Import ANTLR generated classes
sys.path.append(str(Path(__file__).parent.parent / 'java-bridge' / 'src' / 'main' / 'antlr4' / 'com' / 'rules' / 'grammar'))
from RulesParser import RulesParser
from RulesListener import RulesListener

# Import Python template functions
sys.path.append(str(Path(__file__).parent.parent / 'templates' / 'java'))
from standard_rule_template import generate_standard_rule, generate_actionset, generate_action


class RuleDataExtractor(RulesListener):
    """
    ANTLR listener to extract structured rule data for code generation.
    Walks parse tree and builds Python data structures.
    """

    def __init__(self):
        self.rule_name = None
        self.entities = set()
        self.rule_steps = []
        self.complexity_score = 0
        self.performance_category = "hot"
        self.current_step = None
        self.estimated_steps = 0
        self.actions_list = []  # For direct actions

    def enterRule(self, ctx):
        """Extract rule name."""
        if ctx.ruleName():
            if ctx.ruleName().IDENTIFIER():
                self.rule_name = ctx.ruleName().IDENTIFIER().getText()
            elif ctx.ruleName().STRING():
                self.rule_name = ctx.ruleName().STRING().getText().strip('"\'')

    def enterAttribute(self, ctx):
        """Extract entity names from attributes (e.g., 'applicant' from 'applicant.creditScore')."""
        if ctx.attributeIdentifier() and len(ctx.attributeIdentifier()) > 0:
            first_attr = ctx.attributeIdentifier(0)
            if first_attr.IDENTIFIER():
                entity = first_attr.IDENTIFIER().getText()
                self.entities.add(entity)

    def enterRuleStep(self, ctx):
        """Process each rule step (if-then-else or direct actions)."""
        self.estimated_steps += 1
        self.current_step = {
            'type': 'conditional' if ctx.IF() else 'direct',
            'has_else': ctx.ELSE() is not None if ctx.IF() else False,
            'java_code': None
        }

    def exitRuleStep(self, ctx):
        """Convert completed rule step to Java code."""
        if self.current_step:
            java_code = self._convert_rule_step(ctx)
            self.rule_steps.append(java_code)
            self.current_step = None

    def _convert_rule_step(self, ctx):
        """Convert a rule step context to Java code."""
        if ctx.IF():
            # Conditional step: if condition then actions (elseif condition then actions)* (else actions)? endif

            # Main if clause
            condition = self._convert_condition(ctx.condition(0))
            then_actions = self._convert_action_list(ctx.actionList(0))

            java_code = f"if ({condition}) {{\n"
            java_code += "    matched = true;\n"
            for action in then_actions:
                # Fix string escaping: escape inner quotes
                escaped_action = action.replace('"', '\\"')
                java_code += f'    actions.add("{escaped_action}");\n'

            # Handle elseif clauses (multiple possible)
            num_conditions = len(ctx.condition())
            for i in range(1, num_conditions):
                elseif_condition = self._convert_condition(ctx.condition(i))
                elseif_actions = self._convert_action_list(ctx.actionList(i))

                java_code += "} else if (" + elseif_condition + ") {\n"
                java_code += "    matched = true;\n"
                for action in elseif_actions:
                    escaped_action = action.replace('"', '\\"')
                    java_code += f'    actions.add("{escaped_action}");\n'

            # Handle final else clause (optional)
            if ctx.ELSE():
                # Else actions are in the last actionList
                else_actions = self._convert_action_list(ctx.actionList(num_conditions))
                java_code += "} else {\n"
                for action in else_actions:
                    escaped_action = action.replace('"', '\\"')
                    java_code += f'    actions.add("{escaped_action}");\n'

            java_code += "}"
            return java_code
        else:
            # Direct actions (no condition)
            actions = self._convert_action_list(ctx.actionList(0))
            java_code = "matched = true;\n"
            for action in actions:
                self.actions_list.append(action)  # Store for action template
                escaped_action = action.replace('"', '\\"')
                java_code += f'actions.add("{escaped_action}");\n'
            return java_code

    def _convert_condition(self, ctx):
        """Convert condition to Java boolean expression."""
        return self._convert_or_expression(ctx.orExpression())

    def _convert_or_expression(self, ctx):
        """Convert OR expression."""
        if len(ctx.andExpression()) == 1:
            return self._convert_and_expression(ctx.andExpression(0))

        parts = [self._convert_and_expression(and_expr) for and_expr in ctx.andExpression()]
        return " || ".join(f"({part})" for part in parts)

    def _convert_and_expression(self, ctx):
        """Convert AND expression."""
        if len(ctx.notExpression()) == 1:
            return self._convert_not_expression(ctx.notExpression(0))

        parts = [self._convert_not_expression(not_expr) for not_expr in ctx.notExpression()]
        return " && ".join(f"({part})" for part in parts)

    def _convert_not_expression(self, ctx):
        """Convert NOT expression."""
        primary = self._convert_primary_expression(ctx.primaryExpression())
        if ctx.NOT():
            return f"!({primary})"
        return primary

    def _convert_primary_expression(self, ctx):
        """Convert primary expression (comparison or parenthesized)."""
        if ctx.comparison():
            return self._convert_comparison(ctx.comparison())
        else:
            # Parenthesized expression
            return self._convert_or_expression(ctx.orExpression())

    def _convert_comparison(self, ctx):
        """Convert comparison to Java code."""
        left = self._convert_expression(ctx.expression(0))
        right = self._convert_expression(ctx.expression(1))
        operator = ctx.operator().getText()

        # Map DSL operators to Java code
        if operator == '==':
            return f"_equals({left}, {right})"
        elif operator == '!=':
            return f"!_equals({left}, {right})"
        elif operator in ['<', '<=', '>', '>=']:
            op_map = {'<': '<', '<=': '<=', '>': '>', '>=': '>='}
            return f"_compareTo({left}, {right}) {op_map[operator]} 0"
        elif operator == 'in':
            return f"{right}.contains({left})"
        elif operator == 'contains':
            return f"{left}.toString().contains({right}.toString())"
        else:
            return f"{left} {operator} {right}"

    def _convert_expression(self, ctx):
        """Convert arithmetic expression to Java code."""
        if not ctx:
            return "null"

        # Handle term + operators
        result = self._convert_term(ctx.term(0))
        for i in range(1, len(ctx.term())):
            operator = ctx.getChild(2 * i - 1).getText()  # Get operator between terms
            term = self._convert_term(ctx.term(i))
            result = f"({result} {operator} {term})"

        return result

    def _convert_term(self, ctx):
        """Convert term (factor with */% operators)."""
        result = self._convert_factor(ctx.factor(0))
        for i in range(1, len(ctx.factor())):
            operator = ctx.getChild(2 * i - 1).getText()
            factor = self._convert_factor(ctx.factor(i))
            result = f"({result} {operator} {factor})"

        return result

    def _convert_factor(self, ctx):
        """Convert factor (possibly negated atom)."""
        atom = self._convert_atom(ctx.atom())
        if ctx.MINUS():
            return f"(-{atom})"
        return atom

    def _convert_atom(self, ctx):
        """Convert atom (attribute, value, function call, or parenthesized expression)."""
        if ctx.attribute():
            return self._convert_attribute(ctx.attribute())
        elif ctx.value():
            return self._convert_value(ctx.value())
        elif ctx.functionCall():
            return self._convert_function_call(ctx.functionCall())
        elif ctx.expression():
            return f"({self._convert_expression(ctx.expression())})"
        return "null"

    def _convert_attribute(self, ctx):
        """Convert attribute (nested field access) to Java code."""
        parts = []
        for attr_id in ctx.attributeIdentifier():
            if attr_id.IDENTIFIER():
                parts.append(attr_id.IDENTIFIER().getText())
            elif attr_id.STRING():
                parts.append(attr_id.STRING().getText().strip('"\''))

        if len(parts) == 1:
            # Single attribute - could be entity or field
            return f"_getFieldValue(context, \"{parts[0]}\")"

        # Nested attribute: entity.field.subfield
        entity = parts[0]
        result = entity
        for field in parts[1:]:
            result = f"_getFieldValue((Map<String, Object>){result}, \"{field}\")"

        return result

    def _convert_value(self, ctx):
        """Convert value to Java literal."""
        if ctx.STRING():
            # Return string as-is (already quoted)
            return ctx.STRING().getText()
        elif ctx.NUMBER():
            return ctx.NUMBER().getText()
        elif ctx.BOOLEAN():
            return ctx.BOOLEAN().getText()
        elif ctx.NULL():
            return "null"
        elif ctx.list():
            values = [self._convert_value(v) for v in ctx.list().value()]
            return f"Arrays.asList({', '.join(values)})"
        return "null"

    def _convert_function_call(self, ctx):
        """Convert function call to Java code."""
        func_name = ctx.IDENTIFIER().getText()
        args = []
        if ctx.functionArgs():
            for expr in ctx.functionArgs().expression():
                args.append(self._convert_expression(expr))

        args_str = ", ".join(args)
        return f"{func_name}({args_str})"

    def _convert_action_list(self, ctx):
        """Convert action list to array of action strings."""
        actions = []
        for action_ctx in ctx.action():
            action_name = None
            if action_ctx.IDENTIFIER():
                action_name = action_ctx.IDENTIFIER().getText()
            elif action_ctx.STRING():
                action_name = action_ctx.STRING().getText().strip('"\'')

            # Handle parameterized actions
            if action_ctx.parameterList():
                params = []
                for param in action_ctx.parameterList().parameter():
                    params.append(self._convert_expression(param.expression()))
                action_str = f"{action_name}({', '.join(params)})"
            else:
                action_str = action_name

            actions.append(action_str)

        return actions

    def calculate_complexity(self):
        """Calculate complexity score based on extracted data."""
        score = 0
        score += min(self.estimated_steps * 2, 40)  # Steps (max 40 points)
        score += min(len(self.entities) * 5, 30)    # Entities (max 30 points)
        score += min(len(self.rule_steps) * 3, 30)  # Rule steps (max 30 points)

        self.complexity_score = min(score // 10, 10)
        self.performance_category = "hot" if self.complexity_score <= 3 else "warm" if self.complexity_score <= 6 else "cold"


class TemplateCodeGenerator:
    """
    Template-based Java code generator using plain Python f-strings.
    No external dependencies - pure Python code generation.
    """

    def __init__(self):
        """Initialize generator (no external dependencies needed)."""
        pass

    def generate_code(self, parse_tree, rule_type='standard_rule'):
        """
        Generate Java code from ANTLR parse tree using Python templates.

        Args:
            parse_tree: ANTLR parse tree root
            rule_type: Type of rule ('standard_rule', 'actionset', 'action')

        Returns:
            str: Generated Java code
        """
        # Extract data from parse tree
        extractor = RuleDataExtractor()
        from antlr4.tree.Tree import ParseTreeWalker
        walker = ParseTreeWalker()
        walker.walk(extractor, parse_tree)

        # Calculate complexity
        extractor.calculate_complexity()

        # Prepare template data
        class_name = self._to_class_name(extractor.rule_name)

        # Generate code using appropriate template
        if rule_type == 'actionset':
            java_code = generate_actionset(
                rule_name=extractor.rule_name,
                class_name=class_name,
                entities=extractor.entities,
                rule_steps=extractor.rule_steps,
                complexity_score=extractor.complexity_score,
                performance_category=extractor.performance_category
            )
        elif rule_type == 'action':
            java_code = generate_action(
                rule_name=extractor.rule_name,
                class_name=class_name,
                entities=extractor.entities,
                actions_list=extractor.actions_list,
                performance_category=extractor.performance_category
            )
        else:  # standard_rule
            java_code = generate_standard_rule(
                rule_name=extractor.rule_name,
                class_name=class_name,
                entities=extractor.entities,
                rule_steps=extractor.rule_steps,
                complexity_score=extractor.complexity_score,
                performance_category=extractor.performance_category
            )

        return java_code

    def _to_class_name(self, rule_name):
        """Convert rule name to Java class name (PascalCase + 'Rule' suffix)."""
        if not rule_name:
            return "GeneratedRule"

        # Remove quotes if present
        name = rule_name.strip('"\'')

        # Convert to PascalCase
        words = name.replace('_', ' ').replace('-', ' ').split()
        class_name = ''.join(word.capitalize() for word in words)

        # Add 'Rule' suffix if not present
        if not class_name.endswith('Rule'):
            class_name += 'Rule'

        return class_name
