"""
Function Registry for Rules Engine
Manages the core function library with validation and execution.
"""

from typing import Dict, List, Any, Union, Callable
from dataclasses import dataclass
import math
import re


class MathExpressionParser:
    """High-performance math expression parser for direct Java code generation."""

    def __init__(self):
        # Operator precedence (higher number = higher precedence)
        self.precedence = {'+': 1, '-': 1, '*': 2, '/': 2}
        self.operators = set(['+', '-', '*', '/'])

    def parse_to_java(self, expression: str) -> str:
        """Convert math expression to direct Java calculation."""
        # Remove quotes if present
        expression = expression.strip().strip('"\'')

        # Tokenize the expression
        tokens = self._tokenize(expression)

        # Convert infix to postfix using Shunting Yard algorithm
        postfix = self._infix_to_postfix(tokens)

        # Generate Java code from postfix notation
        return self._postfix_to_java(postfix)

    def _tokenize(self, expression: str) -> List[str]:
        """Tokenize expression into numbers, variables, and operators."""
        tokens = []
        current_token = ""

        i = 0
        while i < len(expression):
            char = expression[i]

            if char.isspace():
                if current_token:
                    tokens.append(current_token)
                    current_token = ""
            elif char in self.operators or char in '()':
                if current_token:
                    tokens.append(current_token)
                    current_token = ""
                tokens.append(char)
            else:
                current_token += char

            i += 1

        if current_token:
            tokens.append(current_token)

        return tokens

    def _infix_to_postfix(self, tokens: List[str]) -> List[str]:
        """Convert infix notation to postfix using Shunting Yard algorithm."""
        output = []
        operator_stack = []

        for token in tokens:
            if self._is_number(token) or self._is_variable(token):
                output.append(token)
            elif token == '(':
                operator_stack.append(token)
            elif token == ')':
                while operator_stack and operator_stack[-1] != '(':
                    output.append(operator_stack.pop())
                if operator_stack:  # Remove the '('
                    operator_stack.pop()
            elif token in self.operators:
                while (operator_stack and
                       operator_stack[-1] != '(' and
                       operator_stack[-1] in self.operators and
                       self.precedence.get(operator_stack[-1], 0) >= self.precedence.get(token, 0)):
                    output.append(operator_stack.pop())
                operator_stack.append(token)

        while operator_stack:
            output.append(operator_stack.pop())

        return output

    def _postfix_to_java(self, postfix: List[str]) -> str:
        """Generate Java code from postfix notation."""
        stack = []

        for token in postfix:
            if self._is_number(token) or self._is_variable(token):
                java_value = self._token_to_java(token)
                stack.append(java_value)
            elif token in self.operators:
                if len(stack) < 2:
                    raise ValueError(f"Invalid expression: insufficient operands for {token}")

                right = stack.pop()
                left = stack.pop()

                # Generate Java expression for operation
                java_expr = self._generate_operation_java(left, token, right)
                stack.append(java_expr)

        if len(stack) != 1:
            raise ValueError("Invalid expression: malformed")

        return stack[0]

    def _is_number(self, token: str) -> bool:
        """Check if token is a number."""
        try:
            float(token)
            return True
        except ValueError:
            return False

    def _is_variable(self, token: str) -> bool:
        """Check if token is a variable (attribute reference)."""
        return '.' in token or token.isidentifier()

    def _token_to_java(self, token: str) -> str:
        """Convert token to Java expression."""
        if self._is_number(token):
            return f"((Number){token}).doubleValue()"
        elif '.' in token:
            # Handle attribute references like applicant.income
            parts = token.split('.', 1)
            return f"((Number)_getFieldValue({parts[0]}, \"{parts[1]}\")).doubleValue()"
        else:
            # Simple variable reference
            return f"((Number){token}).doubleValue()"

    def _generate_operation_java(self, left: str, operator: str, right: str) -> str:
        """Generate Java code for binary operation."""
        return f"({left} {operator} {right})"


@dataclass
class FunctionSignature:
    """Function signature definition with parameter validation."""
    name: str
    param_count: int
    param_types: List[str]  # ['string', 'number', 'any']
    return_type: str
    description: str
    java_implementation: str


class FunctionRegistry:
    """Registry for all supported functions in the rules engine."""

    def __init__(self):
        self.functions: Dict[str, FunctionSignature] = {}
        self._register_core_functions()

    def _register_core_functions(self):
        """Register the 4 core MVP functions."""

        # 1. substring(text, start, length) -> String
        self.functions['substring'] = FunctionSignature(
            name='substring',
            param_count=3,
            param_types=['string', 'number', 'number'],
            return_type='string',
            description='Extract substring from text starting at position with specified length',
            java_implementation='String.valueOf({0}).substring(((Number){1}).intValue(), ((Number){1}).intValue() + ((Number){2}).intValue())'
        )

        # 2. length(text) -> Number
        self.functions['length'] = FunctionSignature(
            name='length',
            param_count=1,
            param_types=['string'],
            return_type='number',
            description='Get length of text string',
            java_implementation='String.valueOf({0}).length()'
        )

        # 3. round(number, decimals) -> Number
        self.functions['round'] = FunctionSignature(
            name='round',
            param_count=2,
            param_types=['number', 'number'],
            return_type='number',
            description='Round number to specified decimal places',
            java_implementation='Math.round(((Number){0}).doubleValue() * Math.pow(10, ((Number){1}).intValue())) / Math.pow(10, ((Number){1}).intValue())'
        )

        # 4. percent(part, whole) -> Number
        self.functions['percent'] = FunctionSignature(
            name='percent',
            param_count=2,
            param_types=['number', 'number'],
            return_type='number',
            description='Calculate percentage of part relative to whole',
            java_implementation='(((Number){0}).doubleValue() / ((Number){1}).doubleValue()) * 100.0'
        )

        # 5. math(expression) -> Number - High-performance math processing
        self.functions['math'] = FunctionSignature(
            name='math',
            param_count=1,
            param_types=['string'],
            return_type='number',
            description='High-performance math expression evaluation with direct Java calculation generation',
            java_implementation='MATH_PLACEHOLDER'  # Special handling in get_java_implementation
        )

    def is_function_registered(self, function_name: str) -> bool:
        """Check if function is registered."""
        return function_name.lower() in self.functions

    def get_function_signature(self, function_name: str) -> FunctionSignature:
        """Get function signature for validation."""
        return self.functions.get(function_name.lower())

    def validate_function_call(self, function_name: str, param_count: int) -> tuple[bool, str]:
        """Validate function call against registry."""
        func = self.get_function_signature(function_name)

        if not func:
            return False, f"Unknown function: {function_name}"

        if func.param_count != param_count:
            return False, f"Function {function_name} expects {func.param_count} parameters, got {param_count}"

        return True, ""

    def get_java_implementation(self, function_name: str, params: List[str]) -> str:
        """Get Java code implementation for function call."""
        func = self.get_function_signature(function_name)
        if not func:
            raise ValueError(f"Unknown function: {function_name}")

        # Special handling for math function
        if function_name.lower() == 'math':
            parser = MathExpressionParser()
            return parser.parse_to_java(params[0])

        return func.java_implementation.format(*params)

    def get_registered_functions(self) -> List[str]:
        """Get list of all registered function names."""
        return list(self.functions.keys())

    def execute_function(self, function_name: str, params: List[Any]) -> Any:
        """Execute function for validation/testing purposes."""
        func_name = function_name.lower()

        if func_name == 'substring':
            text, start, length = str(params[0]), int(params[1]), int(params[2])
            return text[start:start + length]

        elif func_name == 'length':
            return len(str(params[0]))

        elif func_name == 'round':
            number, decimals = float(params[0]), int(params[1])
            return round(number, decimals)

        elif func_name == 'percent':
            part, whole = float(params[0]), float(params[1])
            return (part / whole) * 100.0 if whole != 0 else 0.0

        elif func_name == 'math':
            # Simple evaluation for testing (real execution happens in Java)
            expression = str(params[0]).strip().strip('"\'')
            try:
                # Replace variable references with sample values for testing
                test_expression = expression
                test_expression = test_expression.replace('applicant.income', '5000')
                test_expression = test_expression.replace('applicant.debt', '1500')
                test_expression = test_expression.replace('applicant.score', '750')

                # Evaluate safely (only for testing)
                return eval(test_expression)
            except Exception as e:
                raise ValueError(f"Invalid math expression: {expression}")

        else:
            raise ValueError(f"Unknown function: {function_name}")


# Global function registry instance
function_registry = FunctionRegistry()