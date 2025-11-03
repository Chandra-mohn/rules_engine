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
from test_template import generate_standard_rule_test, generate_actionset_test, generate_action_test


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
            # Try IDENTIFIER first
            if hasattr(ctx.ruleName(), 'IDENTIFIER') and ctx.ruleName().IDENTIFIER():
                self.rule_name = ctx.ruleName().IDENTIFIER().getText()
            # Try STRING
            elif hasattr(ctx.ruleName(), 'STRING') and ctx.ruleName().STRING():
                self.rule_name = ctx.ruleName().STRING().getText().strip('"\'')
            # Fallback: get all text
            else:
                self.rule_name = ctx.ruleName().getText().strip('"\'')

    def enterAttribute(self, ctx):
        """Extract entity names from attributes (e.g., 'applicant' from 'applicant.creditScore')."""
        if ctx.attributeIdentifier() and len(ctx.attributeIdentifier()) > 0:
            first_attr = ctx.attributeIdentifier(0)
            if hasattr(first_attr, 'IDENTIFIER') and first_attr.IDENTIFIER():
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
            # Conditional step: if condition then block (elseif condition then block)* (else block)? endif

            # Main if clause
            condition = self._convert_condition(ctx.condition(0))
            then_block = self._convert_block(ctx.block(0), indent_level=1)

            java_code = f"if ({condition}) {{\n"
            java_code += then_block

            # Handle elseif clauses (multiple possible)
            num_conditions = len(ctx.condition())
            for i in range(1, num_conditions):
                elseif_condition = self._convert_condition(ctx.condition(i))
                elseif_block = self._convert_block(ctx.block(i), indent_level=1)

                java_code += "} else if (" + elseif_condition + ") {\n"
                java_code += elseif_block

            # Handle final else clause (optional)
            if ctx.ELSE():
                # Else block is in the last block
                else_block = self._convert_block(ctx.block(num_conditions), indent_level=1)
                java_code += "} else {\n"
                java_code += else_block

            java_code += "}"
            return java_code
        else:
            # Direct actions (no condition)
            actions = self._convert_action_list(ctx.actionList(0))
            java_code = "matched = true;\n"
            for action in actions:
                self.actions_list.append(action)  # Store for action template
                java_code += self._generate_action_add(action)
            return java_code

    def _convert_block(self, ctx, indent_level=0):
        """Convert a block (nested rule steps or action list) to Java code.

        Args:
            ctx: Block context from ANTLR parser
            indent_level: Current indentation level for proper code formatting

        Returns:
            str: Java code with proper indentation
        """
        if not ctx:
            return ""

        java_code = ""

        # Check if block contains rule steps (nested if/then/else) or direct actions
        if ctx.ruleStep():
            # Block contains nested rule steps - recursively convert each
            for rule_step_ctx in ctx.ruleStep():
                step_code = self._convert_rule_step(rule_step_ctx)
                # Indent each line of the generated step code
                java_code += self._indent(step_code, indent_level)
        elif ctx.actionList():
            # Block contains direct action list
            java_code += self._indent("matched = true;\n", indent_level)
            actions = self._convert_action_list(ctx.actionList())
            for action in actions:
                action_code = self._generate_action_add(action)
                java_code += self._indent(action_code, indent_level)

        return java_code

    def _indent(self, code, level):
        """Add indentation to code block.

        Args:
            code: Code string to indent
            level: Number of indentation levels (1 level = 4 spaces)

        Returns:
            str: Indented code
        """
        if not code:
            return ""
        indent = "    " * level
        lines = code.split('\n')
        indented_lines = [indent + line if line.strip() else line for line in lines]
        return '\n'.join(indented_lines)

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

        # If we have addition/subtraction operators, we need type-safe arithmetic
        if len(ctx.term()) > 1:
            # Wrap first term in _toNumber if it's a field access
            if '_getFieldValue' in result and '_toNumber' not in result:
                result = f"_toNumber({result})"

        for i in range(1, len(ctx.term())):
            operator = ctx.getChild(2 * i - 1).getText()  # Get operator between terms
            term = self._convert_term(ctx.term(i))

            # Wrap term in _toNumber if it's a field access and not already wrapped
            if '_getFieldValue' in term and '_toNumber' not in term:
                term = f"_toNumber({term})"

            result = f"({result} {operator} {term})"

        return result

    def _convert_term(self, ctx):
        """Convert term (factor with */% operators)."""
        result = self._convert_factor(ctx.factor(0))

        # If we have arithmetic operators, we need type-safe arithmetic
        if len(ctx.factor()) > 1:
            # Wrap first factor in _toNumber if it's a field access
            if '_getFieldValue' in result:
                result = f"_toNumber({result})"

        for i in range(1, len(ctx.factor())):
            operator = ctx.getChild(2 * i - 1).getText()
            factor = self._convert_factor(ctx.factor(i))

            # Wrap factor in _toNumber if it's a field access
            if '_getFieldValue' in factor:
                factor = f"_toNumber({factor})"

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
            if hasattr(attr_id, 'IDENTIFIER') and attr_id.IDENTIFIER():
                parts.append(attr_id.IDENTIFIER().getText())
            elif hasattr(attr_id, 'STRING') and attr_id.STRING():
                parts.append(attr_id.STRING().getText().strip('"\''))
            else:
                # Fallback: try to get text directly
                text = attr_id.getText()
                if text:
                    # Strip quotes if present (for string literals used as attribute names)
                    parts.append(text.strip('"\''))

        if len(parts) == 0:
            # No parts found, return null
            return "null"

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
        # Check for single-quoted string (SQUOTED_STRING)
        if hasattr(ctx, 'SQUOTED_STRING') and ctx.SQUOTED_STRING():
            # Convert single quotes to double quotes for Java
            text = ctx.SQUOTED_STRING().getText()
            # Remove outer single quotes and wrap in double quotes
            return f'"{text[1:-1]}"'
        # Check for double-quoted string (STRING)
        elif hasattr(ctx, 'STRING') and ctx.STRING():
            # Return string as-is (already quoted)
            return ctx.STRING().getText()
        elif hasattr(ctx, 'NUMBER') and ctx.NUMBER():
            return ctx.NUMBER().getText()
        elif hasattr(ctx, 'BOOLEAN') and ctx.BOOLEAN():
            return ctx.BOOLEAN().getText()
        elif hasattr(ctx, 'NULL') and ctx.NULL():
            return "null"
        elif hasattr(ctx, 'list') and ctx.list():
            values = [self._convert_value(v) for v in ctx.list().value()]
            return f"Arrays.asList({', '.join(values)})"
        return "null"

    def _convert_function_call(self, ctx):
        """Convert function call to Java code."""
        if hasattr(ctx, 'IDENTIFIER') and ctx.IDENTIFIER():
            func_name = ctx.IDENTIFIER().getText()
        else:
            func_name = "unknownFunction"

        args = []
        if hasattr(ctx, 'functionArgs') and ctx.functionArgs():
            for expr in ctx.functionArgs().expression():
                args.append(self._convert_expression(expr))

        args_str = ", ".join(args)
        return f"{func_name}({args_str})"

    def _convert_action_list(self, ctx):
        """Convert action list to array of action objects with name and parameters."""
        actions = []
        for action_ctx in ctx.action():
            action_name = None
            if hasattr(action_ctx, 'IDENTIFIER') and action_ctx.IDENTIFIER():
                action_name = action_ctx.IDENTIFIER().getText()
            elif hasattr(action_ctx, 'STRING') and action_ctx.STRING():
                action_name = action_ctx.STRING().getText().strip('"\'')

            # Handle parameterized actions
            params = []
            if hasattr(action_ctx, 'parameterList') and action_ctx.parameterList():
                for param in action_ctx.parameterList().parameter():
                    params.append({
                        'expression': self._convert_expression(param.expression()),
                        'is_literal': self._is_literal_expression(param.expression())
                    })

            actions.append({
                'name': action_name,
                'params': params
            })

        return actions

    def _is_literal_expression(self, expr_ctx):
        """Check if an expression is a literal value (string, number, boolean)."""
        if not expr_ctx:
            return False

        # Check if it's a single term with a single factor with a single atom
        if len(expr_ctx.term()) == 1:
            term = expr_ctx.term(0)
            if len(term.factor()) == 1:
                factor = term.factor(0)
                atom = factor.atom()
                if atom and atom.value():
                    return True

        return False

    def _generate_action_add(self, action):
        """Generate Java code to add an action to the actions list.

        Evaluates parameters at runtime and builds action string with actual values.
        """
        action_name = action['name']
        params = action['params']

        if not params:
            # No parameters - simple action
            return f'    actions.add("{action_name}");\n'

        # Build action string with evaluated parameters
        # Start with action name and opening paren
        java_code = f'    actions.add("{action_name}(" + '

        param_parts = []
        for i, param in enumerate(params):
            expr = param['expression']

            if param['is_literal']:
                # Literal value - include directly in string
                if expr.startswith('"') and expr.endswith('"'):
                    # String literal - escape inner quotes and include in action string
                    inner = expr[1:-1].replace('"', '\\"')
                    param_parts.append(f'"\\\"{inner}\\\""')
                else:
                    # Number or boolean - include as-is
                    param_parts.append(f'"{expr}"')
            else:
                # Expression - check if it looks like a field access that should be evaluated
                # vs a string literal that was incorrectly converted to _getFieldValue
                if '_getFieldValue(context,' in expr and '"' in expr:
                    # This is likely a string literal being incorrectly treated as field access
                    # Extract the string value
                    import re
                    match = re.search(r'_getFieldValue\(context, "([^"]+)"\)', expr)
                    if match:
                        # It's a string literal - include directly
                        string_val = match.group(1).replace('"', '\\"')
                        param_parts.append(f'"\\\"{string_val}\\\""')
                    else:
                        # Fallback - evaluate at runtime
                        param_parts.append(expr)
                else:
                    # Normal expression - evaluate at runtime and concatenate
                    param_parts.append(expr)

        # Join parameters with commas
        java_code += ' + ", " + '.join(param_parts)

        # Close the action string
        java_code += ' + ")");\n'

        return java_code

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

    def generate(self, rule_content: str, rule_name: str = None, item_type: str = 'rule') -> str:
        """
        Convenience wrapper that parses rule content and generates Java code.

        This method is called by PythonRulesEngine.compile_rule() to provide
        a simple interface for code generation from DSL content.

        Args:
            rule_content: DSL rule content to parse
            rule_name: Optional rule name (extracted from DSL if not provided)
            item_type: Rule type ('rule', 'actionset', 'action', 'mon_rule', 'non_mon_rule')

        Returns:
            str: Generated Java code (complete class)
        """
        from grammar_parser import RulesEngineParser

        # Parse rule content
        parser = RulesEngineParser()
        tree, error_listener = parser.parse(rule_content)

        if tree is None or error_listener.errors:
            error_messages = [error['message'] for error in error_listener.errors] if error_listener.errors else ['Unknown parse error']
            raise ValueError(f"Parse errors: {', '.join(error_messages)}")

        # Map item_type to rule_type for generate_code
        rule_type_map = {
            'rule': 'standard_rule',
            'actionset': 'actionset',
            'action': 'action',
            'mon_rule': 'standard_rule',  # monetary rules use standard template
            'non_mon_rule': 'standard_rule'  # validation rules use standard template
        }
        rule_type = rule_type_map.get(item_type, 'standard_rule')

        # Generate code using the working template system
        return self.generate_code(tree, rule_type)

    def generate_tests(self, parse_tree, rule_type='standard_rule'):
        """
        Generate JUnit 5 test code from ANTLR parse tree.

        Args:
            parse_tree: ANTLR parse tree root
            rule_type: Type of rule ('standard_rule', 'actionset', 'action')

        Returns:
            str: Generated JUnit 5 test class code
        """
        from grammar_parser.test_scenario_generator import TestScenarioGenerator
        from antlr4.tree.Tree import ParseTreeWalker

        # Extract test scenarios from parse tree
        scenario_generator = TestScenarioGenerator()
        walker = ParseTreeWalker()
        walker.walk(scenario_generator, parse_tree)

        # Extract rule data for metadata
        data_extractor = RuleDataExtractor()
        walker.walk(data_extractor, parse_tree)
        data_extractor.calculate_complexity()

        # Prepare test template data
        class_name = self._to_class_name(data_extractor.rule_name)
        test_scenarios = scenario_generator.get_scenarios()

        # Generate test code using appropriate template
        if rule_type == 'actionset':
            test_code = generate_actionset_test(
                rule_name=data_extractor.rule_name,
                class_name=class_name,
                entities=list(data_extractor.entities),
                test_scenarios=test_scenarios,
                complexity_score=data_extractor.complexity_score
            )
        elif rule_type == 'action':
            test_code = generate_action_test(
                rule_name=data_extractor.rule_name,
                class_name=class_name,
                entities=list(data_extractor.entities),
                test_scenarios=test_scenarios,
                complexity_score=data_extractor.complexity_score
            )
        else:  # standard_rule
            test_code = generate_standard_rule_test(
                rule_name=data_extractor.rule_name,
                class_name=class_name,
                entities=list(data_extractor.entities),
                test_scenarios=test_scenarios,
                complexity_score=data_extractor.complexity_score
            )

        return test_code

    def generate_with_tests(self, rule_content: str, rule_name: str = None, item_type: str = 'rule'):
        """
        Generate both production code and test code from DSL rule.

        Args:
            rule_content: DSL rule content to parse
            rule_name: Optional rule name
            item_type: Rule type ('rule', 'actionset', 'action', etc.)

        Returns:
            tuple: (production_code, test_code)
        """
        from grammar_parser import RulesEngineParser

        # Parse rule content once
        parser = RulesEngineParser()
        tree, error_listener = parser.parse(rule_content)

        if tree is None or error_listener.errors:
            error_messages = [error['message'] for error in error_listener.errors] if error_listener.errors else ['Unknown parse error']
            raise ValueError(f"Parse errors: {', '.join(error_messages)}")

        # Map item_type to rule_type
        rule_type_map = {
            'rule': 'standard_rule',
            'actionset': 'actionset',
            'action': 'action',
            'mon_rule': 'standard_rule',
            'non_mon_rule': 'standard_rule'
        }
        rule_type = rule_type_map.get(item_type, 'standard_rule')

        # Generate both production and test code
        production_code = self.generate_code(tree, rule_type)
        test_code = self.generate_tests(tree, rule_type)

        return production_code, test_code

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
