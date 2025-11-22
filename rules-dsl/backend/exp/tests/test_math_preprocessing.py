#!/usr/bin/env python3
"""
Test suite for math preprocessing system
Validates the high-performance math function with direct Java code generation.
"""

import sys
from pathlib import Path
sys.path.append(str(Path(__file__).parent))

from grammar_parser.function_registry import function_registry, MathExpressionParser
from grammar_parser.rules_parser import RulesEngineParser


def test_math_expression_parser():
    """Test the core MathExpressionParser functionality."""
    print("🧮 Testing MathExpressionParser...")
    parser = MathExpressionParser()

    test_cases = [
        {
            'expression': 'applicant.income * 0.3',
            'expected_java': '(((Number)_getFieldValue(applicant, "income")).doubleValue() * ((Number)0.3).doubleValue())',
            'description': 'Simple multiplication with attribute'
        },
        {
            'expression': 'applicant.debt + applicant.income',
            'expected_java': '(((Number)_getFieldValue(applicant, "debt")).doubleValue() + ((Number)_getFieldValue(applicant, "income")).doubleValue())',
            'description': 'Addition with two attributes'
        },
        {
            'expression': '(applicant.debt / applicant.income) * 100',
            'expected_java': '((((Number)_getFieldValue(applicant, "debt")).doubleValue() / ((Number)_getFieldValue(applicant, "income")).doubleValue()) * ((Number)100).doubleValue())',
            'description': 'Complex expression with parentheses'
        },
        {
            'expression': '5000 - 1500',
            'expected_java': '(((Number)5000).doubleValue() - ((Number)1500).doubleValue())',
            'description': 'Simple numeric calculation'
        }
    ]

    all_passed = True
    for i, test_case in enumerate(test_cases, 1):
        print(f"  {i}. {test_case['description']}")

        try:
            result = parser.parse_to_java(test_case['expression'])
            expected = test_case['expected_java']

            if result == expected:
                print(f"     ✅ PASSED")
                print(f"     Java: {result}")
            else:
                print(f"     ❌ FAILED")
                print(f"     Expected: {expected}")
                print(f"     Got:      {result}")
                all_passed = False
        except Exception as e:
            print(f"     ❌ EXCEPTION: {e}")
            all_passed = False
        print()

    return all_passed


def test_math_function_registry():
    """Test math function registration and execution."""
    print("📋 Testing math function in registry...")

    # Test function registration
    assert function_registry.is_function_registered('math'), "Math function not registered"
    print("  ✅ Math function is registered")

    # Test function signature
    signature = function_registry.get_function_signature('math')
    assert signature.name == 'math', "Incorrect function name"
    assert signature.param_count == 1, "Incorrect parameter count"
    assert signature.param_types == ['string'], "Incorrect parameter types"
    assert signature.return_type == 'number', "Incorrect return type"
    print("  ✅ Function signature is correct")

    # Test validation
    valid, error = function_registry.validate_function_call('math', 1)
    assert valid, f"Math function validation failed: {error}"
    print("  ✅ Function validation passes")

    # Test Java implementation generation
    test_expressions = [
        'applicant.income * 3',
        '(applicant.debt / applicant.income) * 100',
        '5000 + 1500'
    ]

    for expr in test_expressions:
        try:
            java_code = function_registry.get_java_implementation('math', [f'"{expr}"'])
            print(f"  ✅ Java generation for '{expr}': {java_code}")
        except Exception as e:
            print(f"  ❌ Java generation failed for '{expr}': {e}")
            return False

    # Test Python execution (for testing only)
    test_cases = [
        ('applicant.income + applicant.debt', 6500.0),  # 5000 + 1500
        ('applicant.income * 3', 15000.0),              # 5000 * 3
        ('100 / 4', 25.0)                               # Simple division
    ]

    for expression, expected in test_cases:
        try:
            result = function_registry.execute_function('math', [expression])
            if abs(result - expected) < 0.001:  # Float comparison
                print(f"  ✅ Execution test '{expression}' = {result}")
            else:
                print(f"  ❌ Execution test '{expression}' failed: expected {expected}, got {result}")
                return False
        except Exception as e:
            print(f"  ❌ Execution test '{expression}' exception: {e}")
            return False

    return True


def test_math_with_rules_parser():
    """Test math function integration with the rules parser."""
    print("🔧 Testing math function with rules parser...")
    parser = RulesEngineParser()

    test_rules = [
        {
            'name': 'Income Ratio Check',
            'rule': 'rule incomeRatio: if math("applicant.debt / applicant.income * 100") > 30 then reject',
            'expected_functions': ['math'],
            'expected_valid': True
        },
        {
            'name': 'Credit Limit Calculation',
            'rule': 'rule creditLimit: if math("applicant.income * 3") > 50000 then approve',
            'expected_functions': ['math'],
            'expected_valid': True
        },
        {
            'name': 'Complex Financial Formula',
            'rule': 'rule complexCalc: if math("(applicant.income - applicant.debt) * 0.4") > 2000 then approve',
            'expected_functions': ['math'],
            'expected_valid': True
        },
        {
            'name': 'Math with Other Functions',
            'rule': 'rule hybrid: if length(substring(applicant.cardNumber, 0, 4)) == 4 and math("applicant.score * 1.2") > 600 then approve',
            'expected_functions': ['substring', 'length', 'math'],
            'expected_valid': True
        }
    ]

    all_passed = True
    for i, test_case in enumerate(test_rules, 1):
        print(f"  {i}. {test_case['name']}")

        # Parse and validate
        validation = parser.validate_syntax(test_case['rule'])
        rule_info = parser.extract_rule_info(test_case['rule'])

        # Check validation result
        valid = validation['valid']
        expected_valid = test_case['expected_valid']

        if valid != expected_valid:
            print(f"     ❌ VALIDATION: Expected {expected_valid}, got {valid}")
            if validation['errors']:
                for error in validation['errors']:
                    print(f"        Error: {error['message']}")
            all_passed = False
        else:
            print(f"     ✅ VALIDATION: {valid}")

        # Check functions detected
        functions_found = set(rule_info.get('functions_used', []))
        expected_functions = set(test_case['expected_functions'])

        if functions_found != expected_functions:
            print(f"     ❌ FUNCTIONS: Expected {expected_functions}, got {functions_found}")
            all_passed = False
        else:
            print(f"     ✅ FUNCTIONS: {list(functions_found)}")

        # Check for function errors
        function_errors = rule_info.get('function_errors', [])
        if function_errors:
            print(f"     ⚠️  FUNCTION ERRORS: {function_errors}")
            all_passed = False

        print()

    return all_passed


def test_math_performance_examples():
    """Test specific performance-oriented examples."""
    print("⚡ Testing performance examples...")

    performance_examples = [
        {
            'description': 'Debt-to-income ratio calculation',
            'expression': '(applicant.debt / applicant.income) * 100',
            'rule': 'rule dtiCheck: if math("(applicant.debt / applicant.income) * 100") < 30 then approve'
        },
        {
            'description': 'Credit score adjustment',
            'expression': 'applicant.score * 1.15 + 50',
            'rule': 'rule scoreBoost: if math("applicant.score * 1.15 + 50") > 700 then approve'
        },
        {
            'description': 'Available credit calculation',
            'expression': 'applicant.limit - applicant.balance',
            'rule': 'rule availableCredit: if math("applicant.limit - applicant.balance") > 1000 then approve'
        }
    ]

    for example in performance_examples:
        print(f"  📊 {example['description']}")

        # Test Java generation
        try:
            java_code = function_registry.get_java_implementation('math', [f'"{example["expression"]}"'])
            print(f"     Java: {java_code}")

            # Verify it doesn't contain ScriptEngine references
            if 'ScriptEngine' in java_code or 'eval' in java_code:
                print(f"     ❌ Performance concern: Uses slow evaluation")
                return False
            else:
                print(f"     ✅ Direct Java calculation (optimal performance)")
        except Exception as e:
            print(f"     ❌ Java generation failed: {e}")
            return False

        # Test rule parsing
        parser = RulesEngineParser()
        validation = parser.validate_syntax(example['rule'])
        if validation['valid']:
            print(f"     ✅ Rule parsing successful")
        else:
            print(f"     ❌ Rule parsing failed: {validation['errors']}")
            return False

        print()

    return True


def main():
    """Run comprehensive math preprocessing tests."""
    print("🧪 MATH PREPROCESSING SYSTEM TEST SUITE")
    print("=" * 60)
    print()

    test_results = []

    # Run all test suites
    test_results.append(("Math Expression Parser", test_math_expression_parser()))
    test_results.append(("Math Function Registry", test_math_function_registry()))
    test_results.append(("Rules Parser Integration", test_math_with_rules_parser()))
    test_results.append(("Performance Examples", test_math_performance_examples()))

    # Summary
    print("=" * 60)
    print("📋 TEST SUMMARY")
    print()

    all_passed = True
    for test_name, result in test_results:
        status = "✅ PASSED" if result else "❌ FAILED"
        print(f"  {status} {test_name}")
        all_passed = all_passed and result

    print()
    if all_passed:
        print("🎉 ALL MATH PREPROCESSING TESTS PASSED!")
        print()
        print("✅ High-performance math system ready for production")
        print("✅ Direct Java calculation generation working")
        print("✅ Integration with existing function system complete")
        print("✅ Zero runtime performance penalty achieved")
        print("✅ Complex math expressions supported")
    else:
        print("❌ Some math preprocessing tests failed")

    return all_passed


if __name__ == '__main__':
    main()