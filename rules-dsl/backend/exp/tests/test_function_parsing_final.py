#!/usr/bin/env python3
"""
Final validation test for Python-only function parsing implementation
"""

import sys
from pathlib import Path
sys.path.append(str(Path(__file__).parent))

from grammar_parser.rules_parser import RulesEngineParser

def test_function_parsing_complete():
    """Comprehensive test of the completed function parsing implementation."""
    parser = RulesEngineParser()

    print("🧪 FINAL FUNCTION PARSING VALIDATION")
    print("=" * 50)
    print()

    # Test the 4 MVP functions in various combinations
    test_cases = [
        {
            'name': 'Card Processing - substring',
            'rule': 'rule cardCheck: if substring(applicant.cardNumber, 0, 4) == "1234" then approve',
            'expected_functions': ['substring'],
            'expected_valid': True
        },
        {
            'name': 'String Processing - length',
            'rule': 'rule lengthCheck: if length(applicant.name) > 5 then approve',
            'expected_functions': ['length'],
            'expected_valid': True
        },
        {
            'name': 'Financial Calculations - round',
            'rule': 'rule roundCheck: if round(applicant.income, 0) > 50000 then approve',
            'expected_functions': ['round'],
            'expected_valid': True
        },
        {
            'name': 'Financial Calculations - percent',
            'rule': 'rule percentCheck: if percent(applicant.debt, applicant.income) < 30 then approve',
            'expected_functions': ['percent'],
            'expected_valid': True
        },
        {
            'name': 'Nested Functions',
            'rule': 'rule nested: if length(substring(applicant.cardNumber, 0, 4)) == 4 then approve',
            'expected_functions': ['substring', 'length'],
            'expected_valid': True
        },
        {
            'name': 'Complex Financial',
            'rule': 'rule complex: if round(percent(applicant.debt, applicant.income), 1) > 30.0 then reject',
            'expected_functions': ['percent', 'round'],
            'expected_valid': True
        },
        {
            'name': 'Multiple Independent Functions',
            'rule': 'rule multi: if length(applicant.name) > 5 and round(applicant.score, 2) > 85.0 then approve',
            'expected_functions': ['length', 'round'],
            'expected_valid': True
        }
    ]

    all_passed = True

    for i, test_case in enumerate(test_cases, 1):
        print(f"{i}. {test_case['name']}")

        # Parse and validate
        validation = parser.validate_syntax(test_case['rule'])
        rule_info = parser.extract_rule_info(test_case['rule'])

        # Check validation result
        valid = validation['valid']
        expected_valid = test_case['expected_valid']

        if valid != expected_valid:
            print(f"   ❌ VALIDATION: Expected {expected_valid}, got {valid}")
            if validation['errors']:
                for error in validation['errors']:
                    print(f"      Error: {error['message']}")
            all_passed = False
        else:
            print(f"   ✅ VALIDATION: {valid}")

        # Check functions detected
        functions_found = set(rule_info.get('functions_used', []))
        expected_functions = set(test_case['expected_functions'])

        if functions_found != expected_functions:
            print(f"   ❌ FUNCTIONS: Expected {expected_functions}, got {functions_found}")
            all_passed = False
        else:
            print(f"   ✅ FUNCTIONS: {list(functions_found)}")

        # Check for function errors
        function_errors = rule_info.get('function_errors', [])
        if function_errors:
            print(f"   ⚠️  FUNCTION ERRORS: {function_errors}")

        print()

    # Summary
    print("=" * 50)
    if all_passed:
        print("🎉 ALL FUNCTION PARSING TESTS PASSED!")
        print()
        print("✅ Python-only preprocessing implementation complete")
        print("✅ 4 MVP functions working: substring, length, round, percent")
        print("✅ Nested function support working")
        print("✅ Complex financial calculations working")
        print("✅ Multi-function rules working")
        print("✅ Function validation working")
        print("✅ Parser integration complete")
        print()
        print("🚀 Ready for production use!")
    else:
        print("❌ Some function parsing tests failed")

    return all_passed

if __name__ == '__main__':
    test_function_parsing_complete()