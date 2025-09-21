"""
Python ANTLR-based Rules Engine Parser
Provides parsing, AST generation, and validation for the rules DSL.
"""

import sys
from pathlib import Path
from antlr4 import *
from antlr4.error.ErrorListener import ErrorListener

# Add the ANTLR generated files to the path
antlr_path = Path(__file__).parent.parent / "java-bridge" / "src" / "main" / "antlr4"
sys.path.insert(0, str(antlr_path))

from com.rules.grammar.RulesLexer import RulesLexer
from com.rules.grammar.RulesParser import RulesParser
from com.rules.grammar.RulesListener import RulesListener


class RulesErrorListener(ErrorListener):
    """Custom error listener to collect parsing errors."""

    def __init__(self):
        self.errors = []
        self.warnings = []

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        error_msg = f"Line {line}:{column} - {msg}"
        self.errors.append({
            'line': line,
            'column': column,
            'message': msg,
            'symbol': str(offendingSymbol) if offendingSymbol else None
        })


class RulesEngineParser:
    """Main parser class for the rules engine DSL."""

    def __init__(self):
        self.error_listener = RulesErrorListener()

    def parse(self, rule_content: str):
        """
        Parse rule content into ANTLR AST.

        Args:
            rule_content: String content of the rule

        Returns:
            tuple: (parse_tree, error_listener)
        """
        try:
            # Create input stream
            input_stream = InputStream(rule_content)

            # Create lexer and add error listener
            lexer = RulesLexer(input_stream)
            lexer.removeErrorListeners()
            lexer.addErrorListener(self.error_listener)

            # Create token stream
            token_stream = CommonTokenStream(lexer)

            # Create parser and add error listener
            parser = RulesParser(token_stream)
            parser.removeErrorListeners()
            parser.addErrorListener(self.error_listener)

            # Parse starting from ruleSet (top-level rule)
            tree = parser.ruleSet()

            return tree, self.error_listener

        except Exception as e:
            self.error_listener.errors.append({
                'line': 0,
                'column': 0,
                'message': f"Parser exception: {str(e)}",
                'symbol': None
            })
            return None, self.error_listener

    def validate_syntax(self, rule_content: str):
        """
        Validate rule syntax without generating code.

        Args:
            rule_content: String content of the rule

        Returns:
            dict: Validation result with errors and warnings
        """
        self.error_listener.errors = []
        self.error_listener.warnings = []

        tree, error_listener = self.parse(rule_content)

        return {
            'valid': len(error_listener.errors) == 0,
            'errors': error_listener.errors,
            'warnings': error_listener.warnings,
            'parse_tree': tree
        }

    def extract_rule_info(self, rule_content: str):
        """
        Extract basic information from rule content.

        Args:
            rule_content: String content of the rule

        Returns:
            dict: Rule information (name, type, etc.)
        """
        tree, error_listener = self.parse(rule_content)

        if tree is None or len(error_listener.errors) > 0:
            return {'error': 'Failed to parse rule', 'errors': error_listener.errors}

        # Walk the tree to extract information
        info_extractor = RuleInfoExtractor()
        walker = ParseTreeWalker()
        walker.walk(info_extractor, tree)

        return {
            'name': info_extractor.rule_name,
            'has_conditions': info_extractor.has_conditions,
            'has_actions': info_extractor.has_actions,
            'attributes_used': list(info_extractor.attributes_used),
            'actions_used': list(info_extractor.actions_used),
            'operators_used': list(info_extractor.operators_used)
        }


class RuleInfoExtractor(RulesListener):
    """Listener to extract information from parsed rules."""

    def __init__(self):
        self.rule_name = None
        self.has_conditions = False
        self.has_actions = False
        self.attributes_used = set()
        self.actions_used = set()
        self.operators_used = set()

    def enterUnifiedRule(self, ctx):
        """Extract rule name."""
        if ctx.ruleName():
            if ctx.ruleName().IDENTIFIER():
                self.rule_name = ctx.ruleName().IDENTIFIER().getText()
            elif ctx.ruleName().STRING():
                # Remove quotes from string literal
                self.rule_name = ctx.ruleName().STRING().getText().strip('"')

    def enterRuleStep(self, ctx):
        """Check for conditions and actions."""
        if ctx.IF():
            self.has_conditions = True
        if ctx.actionList():
            self.has_actions = True

    def enterAttribute(self, ctx):
        """Extract attribute references."""
        if ctx.attributeIdentifier():
            attr_parts = []
            for identifier in ctx.attributeIdentifier():
                if identifier.IDENTIFIER():
                    attr_parts.append(identifier.IDENTIFIER().getText())
                elif identifier.STRING():
                    attr_parts.append(identifier.STRING().getText().strip('"'))
            if attr_parts:
                self.attributes_used.add('.'.join(attr_parts))

    def enterAction(self, ctx):
        """Extract action references."""
        if ctx.IDENTIFIER():
            self.actions_used.add(ctx.IDENTIFIER().getText())
        elif ctx.STRING():
            self.actions_used.add(ctx.STRING().getText().strip('"'))

    def enterOperator(self, ctx):
        """Extract operators used."""
        if ctx.EQ():
            self.operators_used.add('==')
        elif ctx.NE():
            self.operators_used.add('!=')
        elif ctx.LT():
            self.operators_used.add('<')
        elif ctx.LE():
            self.operators_used.add('<=')
        elif ctx.GT():
            self.operators_used.add('>')
        elif ctx.GE():
            self.operators_used.add('>=')
        elif ctx.CONTAINS():
            self.operators_used.add('contains')
        elif ctx.STARTS_WITH():
            self.operators_used.add('starts_with')
        elif ctx.ENDS_WITH():
            self.operators_used.add('ends_with')
        elif ctx.IN():
            self.operators_used.add('in')
        elif ctx.NOT_IN():
            self.operators_used.add('not_in')
        elif ctx.IS_NULL():
            self.operators_used.add('is_null')
        elif ctx.IS_NOT_NULL():
            self.operators_used.add('is_not_null')
        elif ctx.MATCHES():
            self.operators_used.add('matches')