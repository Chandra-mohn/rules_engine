"""
Function Registry for Rules Engine
Manages the core function library with validation and execution.
"""

from typing import Dict, List, Any, Union, Callable
from dataclasses import dataclass
import math
import re


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

        else:
            raise ValueError(f"Unknown function: {function_name}")


# Global function registry instance
function_registry = FunctionRegistry()