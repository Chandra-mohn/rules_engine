"""
Python ANTLR-based Rules Engine Parser
Provides parsing, AST generation, and validation for the rules DSL.
"""

import sys
import re
from pathlib import Path
from typing import List
from antlr4 import *
from antlr4.error.ErrorListener import ErrorListener

# Add the ANTLR generated files to the path
# Extension backend uses ANTLR files from ui-prototype backend
antlr_path = Path(__file__).parent.parent.parent.parent / "ui-prototype" / "backend" / "java-bridge" / "src" / "main" / "antlr4"
sys.path.insert(0, str(antlr_path))

from com.rules.grammar.RulesLexer import RulesLexer
from com.rules.grammar.RulesParser import RulesParser
from com.rules.grammar.RulesListener import RulesListener
from .function_registry import function_registry


class RulesErrorListener(ErrorListener):
    """Custom error listener to collect parsing errors."""

    def __init__(self):
        self.errors = []
        self.warnings = []

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        error_msg = f"Line {line}:{column} - {msg}"

        # Calculate token length for precise underlining
        token_length = 1
        if offendingSymbol:
            token_text = str(offendingSymbol.text) if hasattr(offendingSymbol, 'text') else str(offendingSymbol)
            token_length = len(token_text) if token_text and token_text != '<EOF>' else 1

        self.errors.append({
            'line': line,
            'column': column,
            'message': msg,
            'symbol': str(offendingSymbol) if offendingSymbol else None,
            'length': token_length
        })


class RulesEngineParser:
    """Main parser class for the rules engine DSL."""

    def __init__(self):
        self.error_listener = RulesErrorListener()
        self.function_placeholders = {}
        self.placeholder_counter = 0

    def _preprocess_functions(self, rule_content: str):
        """
        Replace function calls with placeholders that ANTLR can parse.
        Returns: processed_content
        """
        self.function_placeholders = {}
        self.placeholder_counter = 0

        # Find all function calls from innermost to outermost
        processed_content = rule_content

        # Keep processing until no more functions found
        max_iterations = 10  # Prevent infinite loops
        iteration = 0
        while iteration < max_iterations:
            iteration += 1
            found_function = False

            # Look for function calls without nested function calls inside
            i = 0
            while i < len(processed_content):
                # Find potential function start
                match = re.search(r'(\w+)\s*\(', processed_content[i:])
                if not match:
                    break

                function_name = match.group(1)
                start_pos = i + match.start()
                paren_pos = i + match.end() - 1  # Position of opening parenthesis

                # Count parentheses to find the matching closing parenthesis
                paren_count = 1
                j = paren_pos + 1
                args_start = j
                has_nested_functions = False

                while j < len(processed_content) and paren_count > 0:
                    if processed_content[j] == '(':
                        # Check if this is a function call (word followed by ()
                        if j > 0:
                            word_match = re.search(r'(\w+)\s*$', processed_content[:j])
                            if word_match and function_registry.is_function_registered(word_match.group(1)):
                                has_nested_functions = True
                        paren_count += 1
                    elif processed_content[j] == ')':
                        paren_count -= 1
                    j += 1

                if paren_count == 0 and not has_nested_functions:  # Complete function without nested functions
                    if function_registry.is_function_registered(function_name):
                        args_str = processed_content[args_start:j - 1]
                        placeholder = f'"FUNC_PLACEHOLDER_{self.placeholder_counter}"'

                        self.function_placeholders[f"FUNC_PLACEHOLDER_{self.placeholder_counter}"] = {
                            'name': function_name,
                            'args': args_str.strip(),
                            'original': processed_content[start_pos:j]
                        }
                        self.placeholder_counter += 1

                        # Replace the function call with placeholder
                        processed_content = processed_content[:start_pos] + placeholder + processed_content[j:]
                        found_function = True
                        break  # Start over after replacement

                i = start_pos + 1  # Move past current position

            if not found_function:
                break  # No more functions to process

        return processed_content

    def _extract_function_info(self, rule_content: str):
        """
        Extract function usage and validation errors from original content.
        """
        functions_used = set()
        function_errors = []

        # More sophisticated function finding that handles nested parentheses
        def find_function_calls(text):
            """Find all function calls, handling nested parentheses."""
            results = []
            i = 0
            while i < len(text):
                # Look for function name pattern
                match = re.search(r'(\w+)\s*\(', text[i:])
                if not match:
                    break

                function_name = match.group(1)
                start_pos = i + match.start()
                paren_pos = i + match.end() - 1  # Position of opening parenthesis

                # Count parentheses to find the matching closing parenthesis
                paren_count = 1
                j = paren_pos + 1
                while j < len(text) and paren_count > 0:
                    if text[j] == '(':
                        paren_count += 1
                    elif text[j] == ')':
                        paren_count -= 1
                    j += 1

                if paren_count == 0:  # Found matching closing parenthesis
                    args_str = text[paren_pos + 1:j - 1]
                    results.append((function_name, args_str.strip()))
                    i = j
                else:
                    i = paren_pos + 1

            return results

        function_calls = find_function_calls(rule_content)

        for function_name, args_str in function_calls:
            # Check if it's a registered function
            if function_registry.is_function_registered(function_name):
                functions_used.add(function_name)

                # Count parameters by parsing arguments more carefully
                if args_str:
                    # Split by comma, but respect nested parentheses and quotes
                    params = self._parse_function_args(args_str)
                    param_count = len(params)
                else:
                    param_count = 0

                # Validate function call
                is_valid, error_msg = function_registry.validate_function_call(function_name, param_count)
                if not is_valid:
                    function_errors.append(error_msg)
            else:
                # Check if it looks like a function call (not just attribute access)
                function_errors.append(f"Unknown function: {function_name}")

        return functions_used, function_errors

    def _parse_function_args(self, args_str):
        """
        Parse function arguments, respecting nested parentheses and quotes.
        """
        if not args_str.strip():
            return []

        args = []
        current_arg = []
        paren_count = 0
        quote_char = None

        for char in args_str:
            if quote_char:  # Inside quotes
                current_arg.append(char)
                if char == quote_char:
                    quote_char = None
            elif char in ['"', "'"]:  # Start of quoted string
                current_arg.append(char)
                quote_char = char
            elif char == '(':
                current_arg.append(char)
                paren_count += 1
            elif char == ')':
                current_arg.append(char)
                paren_count -= 1
            elif char == ',' and paren_count == 0:
                args.append(''.join(current_arg).strip())
                current_arg = []
            else:
                current_arg.append(char)

        if current_arg:
            args.append(''.join(current_arg).strip())

        return [arg for arg in args if arg]

    def parse(self, rule_content: str):
        """
        Parse rule content into ANTLR AST.

        Args:
            rule_content: String content of the rule

        Returns:
            tuple: (parse_tree, error_listener)
        """
        try:
            # Preprocess functions to work with existing ANTLR parser
            processed_content = self._preprocess_functions(rule_content)

            # Create input stream
            input_stream = InputStream(processed_content)

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

        # Also check function usage
        function_errors = []
        if tree is not None and len(error_listener.errors) == 0:
            rule_info = self.extract_rule_info(rule_content)
            if 'function_errors' in rule_info:
                function_errors = rule_info['function_errors']

        all_errors = error_listener.errors + [{'line': 0, 'column': 0, 'message': err, 'symbol': None} for err in function_errors]

        return {
            'valid': len(all_errors) == 0,
            'errors': all_errors,
            'warnings': error_listener.warnings,
            'parse_tree': tree
        }

    def extract_rule_info(self, rule_content: str, available_actions: List[str] = None):
        """
        Extract basic information from rule content.

        Args:
            rule_content: String content of the rule
            available_actions: List of known action names to exclude from function validation

        Returns:
            dict: Rule information (name, type, etc.)
        """
        # Preprocess and parse with same preprocessing
        processed_content = self._preprocess_functions(rule_content)

        # Now parse with the processed content
        try:
            input_stream = InputStream(processed_content)
            lexer = RulesLexer(input_stream)
            lexer.removeErrorListeners()
            lexer.addErrorListener(self.error_listener)
            token_stream = CommonTokenStream(lexer)
            parser = RulesParser(token_stream)
            parser.removeErrorListeners()
            parser.addErrorListener(self.error_listener)
            tree = parser.ruleSet()
        except Exception as e:
            self.error_listener.errors.append({
                'line': 0,
                'column': 0,
                'message': f"Parser exception: {str(e)}",
                'symbol': None
            })
            tree = None

        if tree is None or len(self.error_listener.errors) > 0:
            return {'error': 'Failed to parse rule', 'errors': self.error_listener.errors}

        # Walk the tree to extract information first (to get actions_used)
        info_extractor = RuleInfoExtractor()
        walker = ParseTreeWalker()
        walker.walk(info_extractor, tree)

        # Create set of known actions (from available_actions parameter + actions found in rule)
        known_action_names = set(available_actions or [])
        known_action_names.update(info_extractor.actions_used)

        # Extract all functions from placeholders
        functions_used = set()
        function_errors = []

        for placeholder_info in self.function_placeholders.values():
            function_name = placeholder_info['name']
            functions_used.add(function_name)

            # Count parameters for validation
            args_str = placeholder_info['args']
            if args_str:
                params = self._parse_function_args(args_str)
                param_count = len(params)
            else:
                param_count = 0

            # Validate function call
            is_valid, error_msg = function_registry.validate_function_call(function_name, param_count)
            if not is_valid:
                function_errors.append(error_msg)

        # Also check for unknown functions that weren't processed
        # BUT exclude known actions to avoid duplicate error messages
        function_pattern = r'(\w+)\s*\('
        for match in re.finditer(function_pattern, rule_content):
            function_name = match.group(1)

            # Skip if this is a known action (to avoid duplicate "Unknown function" + "Unknown action" errors)
            if function_name in known_action_names:
                continue

            if not function_registry.is_function_registered(function_name):
                # Check if it looks like a function call by finding the closing parenthesis
                start_pos = match.end() - 1
                paren_count = 1
                j = start_pos + 1
                while j < len(rule_content) and paren_count > 0:
                    if rule_content[j] == '(':
                        paren_count += 1
                    elif rule_content[j] == ')':
                        paren_count -= 1
                    j += 1

                if paren_count == 0:  # Found complete function call
                    function_errors.append(f"Unknown function: {function_name}")

        return {
            'name': info_extractor.rule_name,
            'has_conditions': info_extractor.has_conditions,
            'has_actions': info_extractor.has_actions,
            'attributes_used': list(info_extractor.attributes_used),
            'actions_used': list(info_extractor.actions_used),
            'operators_used': list(info_extractor.operators_used),
            'functions_used': list(functions_used),  # Use preprocessed function info
            'function_errors': function_errors  # Use preprocessed function errors
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
        self.functions_used = set()
        self.function_errors = []

    def enterRule(self, ctx):
        """Extract rule name."""
        if ctx.ruleName():
            if ctx.ruleName().IDENTIFIER():
                self.rule_name = ctx.ruleName().IDENTIFIER().getText()
            elif ctx.ruleName().DQUOTED_STRING():
                self.rule_name = ctx.ruleName().DQUOTED_STRING().getText().strip('"')
            elif ctx.ruleName().SQUOTED_STRING():
                self.rule_name = ctx.ruleName().SQUOTED_STRING().getText().strip("'")

    def enterRuleStep(self, ctx):
        """Check for conditions and actions."""
        if ctx.IF():
            self.has_conditions = True
        # Actions are now tracked via enterAction(), not from ruleStep

    def enterAttribute(self, ctx):
        """Extract attribute references."""
        if ctx.attributeIdentifier():
            attr_parts = []
            for identifier in ctx.attributeIdentifier():
                if identifier.IDENTIFIER():
                    attr_parts.append(identifier.IDENTIFIER().getText())
                elif identifier.DQUOTED_STRING():
                    attr_parts.append(identifier.DQUOTED_STRING().getText().strip('"'))
                # Note: SQUOTED_STRING not included - single quotes are for literals, not attributes
            if attr_parts:
                self.attributes_used.add('.'.join(attr_parts))

    def enterAction(self, ctx):
        """Extract action references."""
        if ctx.IDENTIFIER():
            self.actions_used.add(ctx.IDENTIFIER().getText())
        elif ctx.DQUOTED_STRING():
            self.actions_used.add(ctx.DQUOTED_STRING().getText().strip('"'))
        elif ctx.SQUOTED_STRING():
            self.actions_used.add(ctx.SQUOTED_STRING().getText().strip("'"))

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

    def enterFunctionCall(self, ctx):
        """Extract function call information and validate."""
        if ctx.IDENTIFIER():
            function_name = ctx.IDENTIFIER().getText()
            self.functions_used.add(function_name)

            # Validate function exists in registry
            if function_registry.is_function_registered(function_name):
                # Count parameters
                param_count = 0
                if ctx.functionArgs() and ctx.functionArgs().operand():
                    param_count = len(ctx.functionArgs().operand())

                # Validate function call
                is_valid, error_msg = function_registry.validate_function_call(function_name, param_count)
                if not is_valid:
                    self.function_errors.append(error_msg)
            else:
                self.function_errors.append(f"Unknown function: {function_name}")