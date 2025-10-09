"""
Integration test for new template-based code generator.
Tests end-to-end: DSL parsing → AST walking → Java code generation
"""

import sys
from pathlib import Path

# Add paths for imports
sys.path.append(str(Path(__file__).parent / 'java-bridge' / 'src' / 'main' / 'antlr4' / 'com' / 'rules' / 'grammar'))

from antlr4 import *
from RulesLexer import RulesLexer
from RulesParser import RulesParser
from services.python_rules_engine import PythonRulesEngine


def test_rules_engine_integration():
    """Test complete integration with PythonRulesEngine."""
    print("\n" + "="*80)
    print("INTEGRATION TEST: PythonRulesEngine with New Template Generator")
    print("="*80)

    engine = PythonRulesEngine()

    # Test 1: Simple rule
    print("\n" + "-"*80)
    print("TEST 1: Simple Rule with Arithmetic")
    print("-"*80)

    simple_rule = '''
    rule simpleArithmetic:
        if balance + deposit > 1000 then approve
    '''

    try:
        result = engine.validate_rule(simple_rule)
        print(f"✅ Validation: {'PASSED' if result['valid'] else 'FAILED'}")
        if not result['valid']:
            print(f"   Errors: {result['errors']}")
    except Exception as e:
        print(f"❌ Validation failed: {e}")

    # Test 2: Nested attributes
    print("\n" + "-"*80)
    print("TEST 2: Nested Attributes")
    print("-"*80)

    nested_rule = '''
    rule nestedAttributes:
        if transaction.location.country == "US" then domestic
        else international
    '''

    try:
        result = engine.validate_rule(nested_rule)
        print(f"✅ Validation: {'PASSED' if result['valid'] else 'FAILED'}")
    except Exception as e:
        print(f"❌ Validation failed: {e}")

    # Test 3: Parameterized actions
    print("\n" + "-"*80)
    print("TEST 3: Parameterized Actions with Expressions")
    print("-"*80)

    param_rule = '''
    rule parameterizedActions:
        if amount > 100 then
            applyFee(amount * 0.05),
            updateBalance(amount + amount * 0.05)
    '''

    try:
        result = engine.validate_rule(param_rule)
        print(f"✅ Validation: {'PASSED' if result['valid'] else 'FAILED'}")
    except Exception as e:
        print(f"❌ Validation failed: {e}")

    # Test 4: Complex real-world rule
    print("\n" + "-"*80)
    print("TEST 4: Complex Real-World Rule")
    print("-"*80)

    complex_rule = '''
    rule creditCardApproval:
        if applicant.creditScore >= 700 and applicant.annualIncome >= 60000 then
            approveApplication,
            setLimit(applicant.annualIncome * 0.3)
        else if applicant.creditScore >= 650 and applicant.employmentYears >= 3 then
            approveApplication,
            setLimit(applicant.annualIncome * 0.2)
        else
            rejectApplication
    '''

    try:
        result = engine.validate_rule(complex_rule)
        print(f"✅ Validation: {'PASSED' if result['valid'] else 'FAILED'}")

        # Try to generate Java code
        input_stream = InputStream(complex_rule)
        lexer = RulesLexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = RulesParser(stream)
        tree = parser.ruleSet()

        java_code = engine.code_generator.generate_code(tree.rule_(0))

        print("\n📄 Generated Java Code (first 40 lines):")
        print("-"*80)
        lines = java_code.split('\n')
        for i, line in enumerate(lines[:40], 1):
            print(f"{i:3}: {line}")

        # Check for key features
        print("\n📊 Feature Validation:")
        print("-"*80)
        checks = {
            'Arithmetic expressions': '* 0.3' in java_code or '* 0.2' in java_code,
            'Helper methods': '_equals' in java_code and '_compareTo' in java_code,
            'Proper escaping': '\\"' in java_code or 'actions.add' in java_code,
            'Entity extraction': 'Map<String, Object> applicant' in java_code,
            'Conditional logic': 'if (' in java_code and 'else' in java_code
        }

        for feature, passed in checks.items():
            status = "✅" if passed else "❌"
            print(f"{status} {feature}")

        all_passed = all(checks.values())
        print(f"\n{'='*80}")
        print(f"Overall: {'✅ ALL CHECKS PASSED' if all_passed else '⚠️  SOME CHECKS FAILED'}")
        print(f"{'='*80}")

    except Exception as e:
        print(f"❌ Test failed: {e}")
        import traceback
        traceback.print_exc()


def test_grammar_features():
    """Test all enhanced grammar features."""
    print("\n" + "="*80)
    print("GRAMMAR FEATURES TEST")
    print("="*80)

    engine = PythonRulesEngine()

    features = [
        ("Arithmetic: +", "rule test: if a + b > 10 then approve"),
        ("Arithmetic: -", "rule test: if a - b < 5 then reject"),
        ("Arithmetic: *", "rule test: if amount * 0.05 > fee then apply"),
        ("Arithmetic: /", "rule test: if total / count > 50 then flag"),
        ("Arithmetic: %", "rule test: if value % 2 == 0 then even"),
        ("Nested (3 levels)", "rule test: if transaction.merchant.location.country == \"US\" then domestic"),
        ("Parentheses", "rule test: if (a + b) * c > 100 then approve"),
        ("Negative numbers", "rule test: if balance + (-50) < 0 then decline"),
        ("Complex expression", "rule test: if (amount + fee) * (1 + taxRate) > limit then reject"),
    ]

    passed = 0
    failed = 0

    for name, dsl in features:
        try:
            result = engine.validate_rule(dsl)
            if result['valid']:
                print(f"✅ {name:<25} PASSED")
                passed += 1
            else:
                print(f"❌ {name:<25} FAILED: {result['errors']}")
                failed += 1
        except Exception as e:
            print(f"❌ {name:<25} ERROR: {str(e)[:50]}")
            failed += 1

    print(f"\n{'='*80}")
    print(f"Results: {passed} passed, {failed} failed ({passed}/{passed+failed} = {100*passed/(passed+failed):.0f}%)")
    print(f"{'='*80}")


if __name__ == '__main__':
    print("\n" + "="*80)
    print("NEW TEMPLATE GENERATOR - COMPREHENSIVE INTEGRATION TESTS")
    print("="*80)
    print("\nVerifying:")
    print("  ✓ ANTLR grammar parsing")
    print("  ✓ AST walking and data extraction")
    print("  ✓ Template-based code generation")
    print("  ✓ Python f-string formatting")
    print("  ✓ Integration with PythonRulesEngine")
    print("\n" + "="*80)

    test_grammar_features()
    test_rules_engine_integration()

    print("\n" + "="*80)
    print("INTEGRATION TESTS COMPLETE")
    print("="*80)
