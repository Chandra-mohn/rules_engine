#!/usr/bin/env python3
"""
Test Suite for MVP Function Library
Tests the 4 core functions: substring, length, round, percent
"""

import unittest
from grammar_parser.function_registry import function_registry
from grammar_parser.rules_parser import RulesEngineParser
from grammar_parser.advanced_java_generator import AdvancedJavaCodeGenerator


class TestFunctionRegistry(unittest.TestCase):
    """Test the function registry itself."""

    def test_function_registration(self):
        """Test that all 4 MVP functions are registered."""
        expected_functions = ['substring', 'length', 'round', 'percent']
        registered_functions = function_registry.get_registered_functions()

        for func in expected_functions:
            self.assertIn(func, registered_functions, f"Function {func} not registered")

    def test_function_signatures(self):
        """Test function signatures are correct."""
        # substring(text, start, length) -> String
        substring_sig = function_registry.get_function_signature('substring')
        self.assertEqual(substring_sig.param_count, 3)
        self.assertEqual(substring_sig.param_types, ['string', 'number', 'number'])
        self.assertEqual(substring_sig.return_type, 'string')

        # length(text) -> Number
        length_sig = function_registry.get_function_signature('length')
        self.assertEqual(length_sig.param_count, 1)
        self.assertEqual(length_sig.param_types, ['string'])
        self.assertEqual(length_sig.return_type, 'number')

        # round(number, decimals) -> Number
        round_sig = function_registry.get_function_signature('round')
        self.assertEqual(round_sig.param_count, 2)
        self.assertEqual(round_sig.param_types, ['number', 'number'])
        self.assertEqual(round_sig.return_type, 'number')

        # percent(part, whole) -> Number
        percent_sig = function_registry.get_function_signature('percent')
        self.assertEqual(percent_sig.param_count, 2)
        self.assertEqual(percent_sig.param_types, ['number', 'number'])
        self.assertEqual(percent_sig.return_type, 'number')

    def test_function_validation(self):
        """Test function call validation."""
        # Valid calls
        self.assertTrue(function_registry.validate_function_call('substring', 3)[0])
        self.assertTrue(function_registry.validate_function_call('length', 1)[0])
        self.assertTrue(function_registry.validate_function_call('round', 2)[0])
        self.assertTrue(function_registry.validate_function_call('percent', 2)[0])

        # Invalid calls - wrong parameter count
        self.assertFalse(function_registry.validate_function_call('substring', 2)[0])
        self.assertFalse(function_registry.validate_function_call('length', 2)[0])
        self.assertFalse(function_registry.validate_function_call('round', 1)[0])
        self.assertFalse(function_registry.validate_function_call('percent', 3)[0])

        # Unknown function
        self.assertFalse(function_registry.validate_function_call('unknown_func', 1)[0])

    def test_function_execution(self):
        """Test Python execution of functions."""
        # substring(text, start, length)
        result = function_registry.execute_function('substring', ['Hello World', 0, 5])
        self.assertEqual(result, 'Hello')

        result = function_registry.execute_function('substring', ['1234567890123456', 0, 4])
        self.assertEqual(result, '1234')

        # length(text)
        result = function_registry.execute_function('length', ['Hello'])
        self.assertEqual(result, 5)

        result = function_registry.execute_function('length', ['1234567890123456'])
        self.assertEqual(result, 16)

        # round(number, decimals)
        result = function_registry.execute_function('round', [3.14159, 2])
        self.assertEqual(result, 3.14)

        result = function_registry.execute_function('round', [123.456, 1])
        self.assertEqual(result, 123.5)

        # percent(part, whole)
        result = function_registry.execute_function('percent', [25, 100])
        self.assertEqual(result, 25.0)

        result = function_registry.execute_function('percent', [1, 3])
        self.assertAlmostEqual(result, 33.333333333333336)

    def test_java_implementation_generation(self):
        """Test Java code generation for functions."""
        # Test substring
        java_code = function_registry.get_java_implementation('substring', ['cardNumber', '0', '4'])
        expected = 'String.valueOf(cardNumber).substring(((Number)0).intValue(), ((Number)0).intValue() + ((Number)4).intValue())'
        self.assertEqual(java_code, expected)

        # Test length
        java_code = function_registry.get_java_implementation('length', ['applicant.name'])
        expected = 'String.valueOf(applicant.name).length()'
        self.assertEqual(java_code, expected)

        # Test round
        java_code = function_registry.get_java_implementation('round', ['amount', '2'])
        expected = 'Math.round(((Number)amount).doubleValue() * Math.pow(10, ((Number)2).intValue())) / Math.pow(10, ((Number)2).intValue())'
        self.assertEqual(java_code, expected)

        # Test percent
        java_code = function_registry.get_java_implementation('percent', ['income', 'target'])
        expected = '(((Number)income).doubleValue() / ((Number)target).doubleValue()) * 100.0'
        self.assertEqual(java_code, expected)


class TestFunctionParsing(unittest.TestCase):
    """Test function parsing with the rules parser."""

    def setUp(self):
        self.parser = RulesEngineParser()

    def test_function_call_recognition(self):
        """Test that function calls are recognized in rules."""
        rule_content = '''rule testFunction:
            if substring(applicant.cardNumber, 0, 4) == "1234" then approve'''

        rule_info = self.parser.extract_rule_info(rule_content)

        self.assertIn('functions_used', rule_info)
        self.assertIn('substring', rule_info['functions_used'])
        self.assertEqual(len(rule_info['function_errors']), 0, f"Function errors: {rule_info['function_errors']}")

    def test_function_validation_in_parsing(self):
        """Test function validation during parsing."""
        # Valid function call
        valid_rule = '''rule validFunc:
            if length(applicant.name) > 5 then approve'''

        validation = self.parser.validate_syntax(valid_rule)
        self.assertTrue(validation['valid'], f"Validation errors: {validation['errors']}")

        # Invalid function call - wrong parameter count
        invalid_rule = '''rule invalidFunc:
            if length(applicant.name, 5) > 10 then approve'''

        rule_info = self.parser.extract_rule_info(invalid_rule)
        self.assertGreater(len(rule_info['function_errors']), 0)
        self.assertIn('expects 1 parameters', rule_info['function_errors'][0])

    def test_multiple_functions_in_rule(self):
        """Test rules with multiple function calls."""
        rule_content = '''rule multiFunc:
            if length(substring(applicant.cardNumber, 0, 4)) == 4 then approve'''

        rule_info = self.parser.extract_rule_info(rule_content)

        # Should detect both substring and length
        self.assertIn('substring', rule_info['functions_used'])
        self.assertIn('length', rule_info['functions_used'])
        self.assertEqual(len(rule_info['function_errors']), 0)

    def test_function_in_actions(self):
        """Test functions used in actions."""
        rule_content = '''rule funcInAction:
            if applicant.creditScore > 700 then setLimit(round(applicant.income * 3, 0))'''

        rule_info = self.parser.extract_rule_info(rule_content)

        self.assertIn('round', rule_info['functions_used'])
        self.assertEqual(len(rule_info['function_errors']), 0)


class TestJavaCodeGeneration(unittest.TestCase):
    """Test Java code generation with functions."""

    def setUp(self):
        self.generator = AdvancedJavaCodeGenerator()

    def test_function_conversion_in_conditions(self):
        """Test function calls are converted correctly in conditions."""
        condition = 'substring(applicant.cardNumber, 0, 4) == "1234"'
        java_condition = self.generator._convert_function_calls_to_java(condition)

        self.assertIn('String.valueOf', java_condition)
        self.assertIn('.substring', java_condition)
        self.assertNotIn('substring(', java_condition)

    def test_function_conversion_patterns(self):
        """Test various function conversion patterns."""
        # Test attribute references in functions
        text = 'length(applicant.name)'
        result = self.generator._convert_function_calls_to_java(text)
        expected = 'String.valueOf(_getFieldValue(applicant, "name")).length()'
        self.assertEqual(result, expected)

        # Test numeric parameters
        text = 'round(123.45, 2)'
        result = self.generator._convert_function_calls_to_java(text)
        expected = 'Math.round(((Number)123.45).doubleValue() * Math.pow(10, ((Number)2).intValue())) / Math.pow(10, ((Number)2).intValue())'
        self.assertEqual(result, expected)

        # Test string parameters
        text = 'substring("Hello World", 0, 5)'
        result = self.generator._convert_function_calls_to_java(text)
        expected = 'String.valueOf("Hello World").substring(((Number)0).intValue(), ((Number)0).intValue() + ((Number)5).intValue())'
        self.assertEqual(result, expected)

    def test_full_rule_generation_with_functions(self):
        """Test complete rule generation with functions."""
        rule_content = '''rule cardValidation:
            if length(substring(applicant.cardNumber, 0, 4)) == 4 then approve'''

        java_code = self.generator.generate(rule_content, 'cardValidation')

        # Should contain the function implementations
        self.assertIn('String.valueOf', java_code)
        self.assertIn('.substring', java_code)
        self.assertIn('.length', java_code)
        self.assertIn('class CardValidationRule', java_code)


class TestCardProcessingUseCases(unittest.TestCase):
    """Test specific card processing use cases with functions."""

    def test_card_number_validation(self):
        """Test card number processing functions."""
        # Extract first 4 digits
        first_four = function_registry.execute_function('substring', ['1234567890123456', 0, 4])
        self.assertEqual(first_four, '1234')

        # Check card number length
        card_length = function_registry.execute_function('length', ['1234567890123456'])
        self.assertEqual(card_length, 16)

    def test_financial_calculations(self):
        """Test financial calculation functions."""
        # Round credit limit to nearest dollar
        credit_limit = function_registry.execute_function('round', [5432.67, 0])
        self.assertEqual(credit_limit, 5433.0)

        # Calculate debt-to-income ratio as percentage
        debt_ratio = function_registry.execute_function('percent', [2000, 8000])
        self.assertEqual(debt_ratio, 25.0)

    def test_realistic_rule_scenarios(self):
        """Test realistic rule scenarios using functions."""
        parser = RulesEngineParser()

        # Card type detection rule
        card_rule = '''rule cardTypeDetection:
            if substring(applicant.cardNumber, 0, 1) == "4" then setCardType("Visa")
            else if substring(applicant.cardNumber, 0, 1) == "5" then setCardType("MasterCard")'''

        rule_info = parser.extract_rule_info(card_rule)
        self.assertIn('substring', rule_info['functions_used'])
        self.assertEqual(len(rule_info['function_errors']), 0)

        # Financial ratio rule
        ratio_rule = '''rule debtToIncomeCheck:
            if percent(applicant.monthlyDebt, applicant.monthlyIncome) > 30 then reject'''

        rule_info = parser.extract_rule_info(ratio_rule)
        self.assertIn('percent', rule_info['functions_used'])
        self.assertEqual(len(rule_info['function_errors']), 0)


if __name__ == '__main__':
    print("Testing MVP Function Library (substring, length, round, percent)")
    print("=" * 60)

    # Run tests
    unittest.main(verbosity=2)