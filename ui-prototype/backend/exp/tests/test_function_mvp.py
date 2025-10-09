#!/usr/bin/env python3
"""
MVP Function Library Test Suite
Tests the 4 core functions without requiring ANTLR parser updates.
Focuses on function registry, validation, and Java code generation.
"""

import unittest
from grammar_parser.function_registry import function_registry
from grammar_parser.advanced_java_generator import AdvancedJavaCodeGenerator


class TestMVPFunctionRegistry(unittest.TestCase):
    """Test the MVP function registry."""

    def test_mvp_functions_registered(self):
        """Test that all 4 MVP functions are registered correctly."""
        expected_functions = ['substring', 'length', 'round', 'percent']
        registered_functions = function_registry.get_registered_functions()

        for func in expected_functions:
            self.assertIn(func, registered_functions, f"MVP function {func} not registered")

        # Should have exactly 4 functions
        self.assertEqual(len(registered_functions), 4, f"Expected 4 MVP functions, got {len(registered_functions)}")

    def test_function_signatures(self):
        """Test function signatures match MVP requirements."""
        # substring(text, start, length) -> String
        substring_sig = function_registry.get_function_signature('substring')
        self.assertIsNotNone(substring_sig)
        self.assertEqual(substring_sig.name, 'substring')
        self.assertEqual(substring_sig.param_count, 3)
        self.assertEqual(substring_sig.param_types, ['string', 'number', 'number'])
        self.assertEqual(substring_sig.return_type, 'string')
        self.assertIn('substring', substring_sig.description.lower())

        # length(text) -> Number
        length_sig = function_registry.get_function_signature('length')
        self.assertIsNotNone(length_sig)
        self.assertEqual(length_sig.name, 'length')
        self.assertEqual(length_sig.param_count, 1)
        self.assertEqual(length_sig.param_types, ['string'])
        self.assertEqual(length_sig.return_type, 'number')
        self.assertIn('length', length_sig.description.lower())

        # round(number, decimals) -> Number
        round_sig = function_registry.get_function_signature('round')
        self.assertIsNotNone(round_sig)
        self.assertEqual(round_sig.name, 'round')
        self.assertEqual(round_sig.param_count, 2)
        self.assertEqual(round_sig.param_types, ['number', 'number'])
        self.assertEqual(round_sig.return_type, 'number')
        self.assertIn('round', round_sig.description.lower())

        # percent(part, whole) -> Number
        percent_sig = function_registry.get_function_signature('percent')
        self.assertIsNotNone(percent_sig)
        self.assertEqual(percent_sig.name, 'percent')
        self.assertEqual(percent_sig.param_count, 2)
        self.assertEqual(percent_sig.param_types, ['number', 'number'])
        self.assertEqual(percent_sig.return_type, 'number')
        self.assertIn('percent', percent_sig.description.lower())

    def test_function_validation(self):
        """Test function call validation works correctly."""
        # Valid calls should pass
        valid, msg = function_registry.validate_function_call('substring', 3)
        self.assertTrue(valid, f"substring(3 params) should be valid: {msg}")

        valid, msg = function_registry.validate_function_call('length', 1)
        self.assertTrue(valid, f"length(1 param) should be valid: {msg}")

        valid, msg = function_registry.validate_function_call('round', 2)
        self.assertTrue(valid, f"round(2 params) should be valid: {msg}")

        valid, msg = function_registry.validate_function_call('percent', 2)
        self.assertTrue(valid, f"percent(2 params) should be valid: {msg}")

        # Invalid calls should fail
        valid, msg = function_registry.validate_function_call('substring', 2)
        self.assertFalse(valid, "substring should require 3 parameters")
        self.assertIn('expects 3 parameters', msg)

        valid, msg = function_registry.validate_function_call('unknown_function', 1)
        self.assertFalse(valid, "Unknown function should be invalid")
        self.assertIn('Unknown function', msg)

    def test_function_execution_card_processing(self):
        """Test function execution for card processing use cases."""
        # Card number processing with substring
        card_number = '1234567890123456'

        # Extract first 4 digits for card type detection
        first_four = function_registry.execute_function('substring', [card_number, 0, 4])
        self.assertEqual(first_four, '1234')

        # Extract last 4 digits for display
        last_four = function_registry.execute_function('substring', [card_number, 12, 4])
        self.assertEqual(last_four, '3456')

        # Validate card number length
        card_length = function_registry.execute_function('length', [card_number])
        self.assertEqual(card_length, 16)

        # Test with different length cards
        amex_card = '123456789012345'  # 15 digits
        amex_length = function_registry.execute_function('length', [amex_card])
        self.assertEqual(amex_length, 15)

    def test_function_execution_financial_calculations(self):
        """Test function execution for financial calculations."""
        # Round credit limits
        raw_limit = 5432.67
        rounded_limit = function_registry.execute_function('round', [raw_limit, 0])
        self.assertEqual(rounded_limit, 5433.0)

        # Round APR to 2 decimal places
        raw_apr = 15.789
        rounded_apr = function_registry.execute_function('round', [raw_apr, 2])
        self.assertEqual(rounded_apr, 15.79)

        # Calculate debt-to-income ratio
        monthly_debt = 2000
        monthly_income = 8000
        dti_ratio = function_registry.execute_function('percent', [monthly_debt, monthly_income])
        self.assertEqual(dti_ratio, 25.0)

        # Calculate utilization percentage
        balance = 1500
        limit = 5000
        utilization = function_registry.execute_function('percent', [balance, limit])
        self.assertEqual(utilization, 30.0)

    def test_java_implementation_generation(self):
        """Test Java code generation for each MVP function."""
        # Test substring Java generation
        java_code = function_registry.get_java_implementation('substring', ['cardNumber', '0', '4'])
        self.assertIn('String.valueOf', java_code)
        self.assertIn('.substring', java_code)
        self.assertIn('cardNumber', java_code)

        # Test length Java generation
        java_code = function_registry.get_java_implementation('length', ['applicantName'])
        self.assertIn('String.valueOf', java_code)
        self.assertIn('.length()', java_code)
        self.assertIn('applicantName', java_code)

        # Test round Java generation
        java_code = function_registry.get_java_implementation('round', ['amount', '2'])
        self.assertIn('Math.round', java_code)
        self.assertIn('Math.pow', java_code)
        self.assertIn('amount', java_code)

        # Test percent Java generation
        java_code = function_registry.get_java_implementation('percent', ['debt', 'income'])
        self.assertIn('.doubleValue()', java_code)
        self.assertIn('100.0', java_code)
        self.assertIn('debt', java_code)
        self.assertIn('income', java_code)


class TestJavaCodeGeneratorFunctions(unittest.TestCase):
    """Test Java code generator with function support."""

    def setUp(self):
        self.generator = AdvancedJavaCodeGenerator()

    def test_function_call_conversion(self):
        """Test function call conversion to Java code."""
        # Test substring conversion
        text = 'substring(cardNumber, 0, 4)'
        result = self.generator._convert_function_calls_to_java(text)
        expected = 'String.valueOf(cardNumber).substring(((Number)0).intValue(), ((Number)0).intValue() + ((Number)4).intValue())'
        self.assertEqual(result, expected)

        # Test length conversion
        text = 'length(applicant.name)'
        result = self.generator._convert_function_calls_to_java(text)
        expected = 'String.valueOf(_getFieldValue(applicant, "name")).length()'
        self.assertEqual(result, expected)

        # Test round conversion
        text = 'round(123.45, 2)'
        result = self.generator._convert_function_calls_to_java(text)
        self.assertIn('Math.round', result)
        self.assertIn('123.45', result)

        # Test percent conversion
        text = 'percent(debt, income)'
        result = self.generator._convert_function_calls_to_java(text)
        self.assertIn('.doubleValue()', result)
        self.assertIn('100.0', result)

    def test_function_in_complex_expressions(self):
        """Test functions within complex expressions."""
        # Nested function calls
        text = 'length(substring(cardNumber, 0, 4)) == 4'
        result = self.generator._convert_function_calls_to_java(text)

        # Should contain both function implementations
        self.assertIn('String.valueOf', result)
        self.assertIn('.substring', result)
        self.assertIn('.length()', result)
        self.assertEqual(result.count('String.valueOf'), 2)  # Both functions use String.valueOf

    def test_function_with_attribute_references(self):
        """Test functions with dot notation attribute references."""
        text = 'substring(applicant.cardNumber, 0, 4)'
        result = self.generator._convert_function_calls_to_java(text)

        # Should convert attribute reference properly
        self.assertIn('_getFieldValue(applicant, "cardNumber")', result)
        self.assertIn('.substring', result)

    def test_multiple_different_functions(self):
        """Test multiple different functions in one expression."""
        text = 'round(percent(debt, income), 1)'
        result = self.generator._convert_function_calls_to_java(text)

        # Should contain both function implementations
        self.assertIn('Math.round', result)
        self.assertIn('.doubleValue()', result)
        self.assertIn('100.0', result)


class TestMVPFunctionUseCases(unittest.TestCase):
    """Test specific MVP use cases from user feedback."""

    def test_card_processing_use_cases(self):
        """Test card processing scenarios user explicitly wanted."""
        # Card type detection based on first digit
        card_numbers = {
            '4123456789012345': 'Visa',        # Starts with 4
            '5123456789012345': 'MasterCard',  # Starts with 5
            '3412345678901234': 'Amex'         # Starts with 3
        }

        for card_number, expected_type in card_numbers.items():
            first_digit = function_registry.execute_function('substring', [card_number, 0, 1])

            if first_digit == '4':
                card_type = 'Visa'
            elif first_digit == '5':
                card_type = 'MasterCard'
            elif first_digit == '3':
                card_type = 'Amex'
            else:
                card_type = 'Unknown'

            self.assertEqual(card_type, expected_type)

        # Card number validation by length
        valid_lengths = {16: 'Standard', 15: 'Amex', 14: 'Diners'}

        test_cards = ['1234567890123456', '123456789012345', '12345678901234']
        for i, card in enumerate(test_cards):
            length = function_registry.execute_function('length', [card])
            expected_length = [16, 15, 14][i]
            self.assertEqual(length, expected_length)

    def test_financial_calculations_use_cases(self):
        """Test financial calculations user explicitly wanted."""
        # Credit limit calculation with rounding
        income_multipliers = [
            (50000, 3.0, 150000),  # High income
            (30000, 2.5, 75000),   # Medium income
            (20000, 2.0, 40000)    # Lower income
        ]

        for income, multiplier, expected in income_multipliers:
            raw_limit = income * multiplier
            rounded_limit = function_registry.execute_function('round', [raw_limit, 0])
            self.assertEqual(rounded_limit, expected)

        # Debt-to-income ratios as percentages
        dti_scenarios = [
            (2000, 8000, 25.0),    # Good DTI
            (3000, 6000, 50.0),    # High DTI
            (1500, 10000, 15.0)    # Excellent DTI
        ]

        for debt, income, expected_pct in dti_scenarios:
            dti_ratio = function_registry.execute_function('percent', [debt, income])
            self.assertEqual(dti_ratio, expected_pct)

    def test_mvp_function_combinations(self):
        """Test combinations of MVP functions as user might use them."""
        card_number = '4123456789012345'

        # Get card type and validate
        card_type_digit = function_registry.execute_function('substring', [card_number, 0, 1])
        card_length = function_registry.execute_function('length', [card_number])

        self.assertEqual(card_type_digit, '4')  # Visa
        self.assertEqual(card_length, 16)       # Valid length

        # Financial scenario
        monthly_income = 5000
        monthly_debt = 1200

        # Calculate DTI and round for display
        raw_dti = function_registry.execute_function('percent', [monthly_debt, monthly_income])
        rounded_dti = function_registry.execute_function('round', [raw_dti, 1])

        self.assertEqual(raw_dti, 24.0)
        self.assertEqual(rounded_dti, 24.0)  # Already clean, no rounding needed


if __name__ == '__main__':
    print("MVP Function Library Test Suite")
    print("Testing 4 functions: substring, length, round, percent")
    print("Focus: Card processing & Financial calculations")
    print("=" * 60)

    # Run tests
    unittest.main(verbosity=2)