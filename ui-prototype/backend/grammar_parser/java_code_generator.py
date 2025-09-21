"""
Python ANTLR-based Java Code Generator
Generates Java code from rules AST using tree walking.
"""

import sys
from pathlib import Path
from antlr4 import *

# Add the ANTLR generated files to the path
antlr_path = Path(__file__).parent.parent / "java-bridge" / "src" / "main" / "antlr4"
sys.path.insert(0, str(antlr_path))

from com.rules.grammar.RulesParser import RulesParser


class JavaCodeGenerator:
    """Generator that produces Java code from rules AST."""

    def __init__(self):
        self.code = []
        self.indent_level = 0
        self.rule_name = "UnknownRule"
        self.entities_used = set()
        self.method_counter = 0

    def generate(self, parse_tree, rule_name: str = None):
        """
        Generate Java code from parse tree.

        Args:
            parse_tree: ANTLR parse tree
            rule_name: Optional rule name override

        Returns:
            str: Generated Java code
        """
        self.code = []
        self.indent_level = 0
        self.entities_used = set()
        self.method_counter = 0

        if rule_name:
            self.rule_name = rule_name

        # Generate the complete Java class
        self._generate_complete_class(parse_tree)

        return '\n'.join(self.code)

    def _emit(self, text: str):
        """Emit code with proper indentation."""
        indent = "    " * self.indent_level
        self.code.append(f"{indent}{text}")

    def _emit_blank(self):
        """Emit a blank line."""
        self.code.append("")

    def visitRuleSet(self, ctx: RulesParser.RuleSetContext):
        """Generate the complete Java class structure."""
        # Generate package and imports
        self._emit("package com.rules;")
        self._emit_blank()
        self._emit("import java.util.*;")
        self._emit("import java.math.BigDecimal;")
        self._emit("import java.time.LocalDate;")
        self._emit("import java.time.LocalDateTime;")
        self._emit("import java.time.format.DateTimeFormatter;")
        self._emit_blank()

        # Generate class header
        class_name = f"{self._to_class_name(self.rule_name)}Rule"
        self._emit(f"public class {class_name} {{")
        self.indent_level += 1

        # Generate RuleResult inner class
        self._generate_rule_result_class()
        self._emit_blank()

        # Visit all definitions to collect entities
        for definition in ctx.definition():
            self._collect_entities(definition)

        # Generate the main evaluate method
        self._emit("public static RuleResult evaluate(Map<String, Object> context) {")
        self.indent_level += 1

        # Generate entity declarations
        self._generate_entity_declarations()
        self._emit_blank()

        # Generate rule logic
        self._emit("List<String> actions = new ArrayList<>();")
        self._emit("String finalAction = null;")
        self._emit("boolean matched = false;")
        self._emit_blank()

        # Visit rule definitions
        for definition in ctx.definition():
            self.visit(definition)

        # Return result
        self._emit_blank()
        self._emit("return new RuleResult(matched, actions, finalAction);")

        self.indent_level -= 1
        self._emit("}")

        # Close class
        self.indent_level -= 1
        self._emit("}")

        return '\n'.join(self.code)

    def visitUnifiedRule(self, ctx: RulesParser.UnifiedRuleContext):
        """Process a unified rule definition."""
        # Extract rule name if not already set
        if ctx.ruleName():
            if ctx.ruleName().IDENTIFIER():
                self.rule_name = ctx.ruleName().IDENTIFIER().getText()
            elif ctx.ruleName().STRING():
                self.rule_name = ctx.ruleName().STRING().getText().strip('"')

        # Visit all rule steps
        for step in ctx.ruleStep():
            self.visit(step)

    def visitRuleStep(self, ctx: RulesParser.RuleStepContext):
        """Process a rule step (if-then-else or standalone action)."""
        if ctx.IF():
            # Generate if-then-else structure
            self._emit("// Rule condition")
            condition_code = self.visit(ctx.condition())
            self._emit(f"if ({condition_code}) {{")
            self.indent_level += 1
            self._emit("matched = true;")

            # Process THEN actions
            if ctx.actionList() and len(ctx.actionList()) > 0:
                self.visit(ctx.actionList(0))

            self.indent_level -= 1
            self._emit("}")

            # Process ELSE actions if present
            if len(ctx.actionList()) > 1:
                self._emit("else {")
                self.indent_level += 1
                self.visit(ctx.actionList(1))
                self.indent_level -= 1
                self._emit("}")

        else:
            # Standalone action list
            if ctx.actionList() and len(ctx.actionList()) > 0:
                self._emit("// Standalone actions")
                self._emit("matched = true;")
                self.visit(ctx.actionList(0))

    def visitCondition(self, ctx: RulesParser.ConditionContext):
        """Generate condition code."""
        return self.visit(ctx.orExpression())

    def visitOrExpression(self, ctx: RulesParser.OrExpressionContext):
        """Generate OR expression code."""
        if len(ctx.andExpression()) == 1:
            return self.visit(ctx.andExpression(0))

        conditions = []
        for expr in ctx.andExpression():
            conditions.append(self.visit(expr))

        return f"({' || '.join(conditions)})"

    def visitAndExpression(self, ctx: RulesParser.AndExpressionContext):
        """Generate AND expression code."""
        if len(ctx.notExpression()) == 1:
            return self.visit(ctx.notExpression(0))

        conditions = []
        for expr in ctx.notExpression():
            conditions.append(self.visit(expr))

        return f"({' && '.join(conditions)})"

    def visitNotExpression(self, ctx: RulesParser.NotExpressionContext):
        """Generate NOT expression code."""
        expr_code = self.visit(ctx.primaryExpression())
        if ctx.NOT():
            return f"!({expr_code})"
        return expr_code

    def visitPrimaryExpression(self, ctx: RulesParser.PrimaryExpressionContext):
        """Generate primary expression code."""
        if ctx.comparison():
            return self.visit(ctx.comparison())
        elif ctx.LPAREN():
            # Parenthesized expression
            return f"({self.visit(ctx.orExpression())})"

    def visitComparison(self, ctx: RulesParser.ComparisonContext):
        """Generate comparison code."""
        left = self.visit(ctx.attribute())
        operator = self.visit(ctx.operator())
        right = self.visit(ctx.operand())

        return f"{left} {operator} {right}"

    def visitAttribute(self, ctx: RulesParser.AttributeContext):
        """Generate attribute access code."""
        parts = []
        for identifier in ctx.attributeIdentifier():
            if identifier.IDENTIFIER():
                parts.append(identifier.IDENTIFIER().getText())
            elif identifier.STRING():
                parts.append(identifier.STRING().getText().strip('"'))

        if len(parts) >= 2:
            entity = parts[0]
            field = parts[1]
            self.entities_used.add(entity)
            return f'_get{entity.title()}Field({entity}, "{field}")'
        elif len(parts) == 1:
            return f'_getContextValue(context, "{parts[0]}")'

    def visitOperator(self, ctx: RulesParser.OperatorContext):
        """Generate operator code."""
        if ctx.EQ():
            return ".equals"
        elif ctx.NE():
            return "!.equals"
        elif ctx.LT():
            return "_compareTo <"
        elif ctx.LE():
            return "_compareTo <="
        elif ctx.GT():
            return "_compareTo >"
        elif ctx.GE():
            return "_compareTo >="
        elif ctx.CONTAINS():
            return "_contains"
        elif ctx.STARTS_WITH():
            return "_startsWith"
        elif ctx.ENDS_WITH():
            return "_endsWith"
        elif ctx.IN():
            return "_in"
        elif ctx.NOT_IN():
            return "_notIn"
        elif ctx.IS_NULL():
            return "_isNull"
        elif ctx.IS_NOT_NULL():
            return "_isNotNull"
        elif ctx.MATCHES():
            return "_matches"
        else:
            return "=="

    def visitOperand(self, ctx: RulesParser.OperandContext):
        """Generate operand code."""
        if ctx.attribute():
            return self.visit(ctx.attribute())
        elif ctx.value():
            return self.visit(ctx.value())

    def visitValue(self, ctx: RulesParser.ValueContext):
        """Generate value code."""
        if ctx.STRING():
            return ctx.STRING().getText()  # Keep quotes
        elif ctx.NUMBER():
            num_text = ctx.NUMBER().getText()
            if '.' in num_text:
                return f'new BigDecimal("{num_text}")'
            else:
                return num_text
        elif ctx.BOOLEAN():
            return ctx.BOOLEAN().getText().lower()
        elif ctx.NULL():
            return "null"
        elif ctx.list_():
            return self.visit(ctx.list_())

    def visitList(self, ctx: RulesParser.ListContext):
        """Generate list code."""
        values = []
        for value in ctx.value():
            values.append(self.visit(value))
        return f"Arrays.asList({', '.join(values)})"

    def visitActionList(self, ctx: RulesParser.ActionListContext):
        """Generate action list code."""
        for action in ctx.action():
            self.visit(action)

    def visitAction(self, ctx: RulesParser.ActionContext):
        """Generate action code."""
        if ctx.IDENTIFIER():
            action_name = ctx.IDENTIFIER().getText()
            if ctx.parameterList():
                params = self.visit(ctx.parameterList())
                self._emit(f'actions.add("{action_name}({params})");')
            else:
                self._emit(f'actions.add("{action_name}");')
        elif ctx.STRING():
            action_name = ctx.STRING().getText().strip('"')
            if ctx.parameterList():
                params = self.visit(ctx.parameterList())
                self._emit(f'actions.add("{action_name}({params})");')
            else:
                self._emit(f'actions.add("{action_name}");')

    def visitParameterList(self, ctx: RulesParser.ParameterListContext):
        """Generate parameter list code."""
        params = []
        for param in ctx.parameter():
            params.append(self.visit(param))
        return ', '.join(params)

    def visitParameter(self, ctx: RulesParser.ParameterContext):
        """Generate parameter code."""
        if ctx.value():
            return self.visit(ctx.value())
        elif ctx.attribute():
            return self.visit(ctx.attribute())

    def _collect_entities(self, ctx):
        """Collect all entities used in the rule for declaration generation."""
        # This is a simplified collection - in practice, you'd walk the tree
        # For now, we'll collect during generation
        pass

    def _generate_entity_declarations(self):
        """Generate entity variable declarations."""
        for entity in sorted(self.entities_used):
            self._emit(f'Map<String, Object> {entity} = (Map<String, Object>) context.get("{entity}");')

    def _generate_rule_result_class(self):
        """Generate the RuleResult inner class."""
        self._emit("public static class RuleResult {")
        self.indent_level += 1

        self._emit("private boolean matched;")
        self._emit("private List<String> actions;")
        self._emit("private String finalAction;")
        self._emit_blank()

        self._emit("public RuleResult(boolean matched, List<String> actions, String finalAction) {")
        self.indent_level += 1
        self._emit("this.matched = matched;")
        self._emit("this.actions = actions;")
        self._emit("this.finalAction = finalAction;")
        self.indent_level -= 1
        self._emit("}")
        self._emit_blank()

        # Getters
        self._emit("public boolean isMatched() { return matched; }")
        self._emit("public List<String> getActions() { return actions; }")
        self._emit("public String getFinalAction() { return finalAction; }")

        self.indent_level -= 1
        self._emit("}")

    def _to_class_name(self, name: str) -> str:
        """Convert rule name to valid Java class name."""
        import re
        # Remove quotes and special characters
        name = re.sub(r'[^a-zA-Z0-9_]', '', name)
        # Capitalize first letter
        if name:
            return name[0].upper() + name[1:]
        return "DefaultRule"