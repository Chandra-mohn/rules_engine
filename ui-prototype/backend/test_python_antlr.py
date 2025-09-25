#!/usr/bin/env python3
"""
Test script for Python ANTLR Rules Engine implementation.
Validates parsing, code generation, and validation functionality.
"""

import sys
import os
from pathlib import Path

# Add backend to Python path
backend_path = Path(__file__).parent
sys.path.insert(0, str(backend_path))

from grammar_parser import RulesEngineParser, RuleValidator
from grammar_parser.advanced_java_generator import AdvancedJavaCodeGenerator
from services.python_rules_engine import PythonRulesEngine


def test_basic_parsing():
    """Test basic ANTLR parsing functionality."""
    print("🔍 Testing basic ANTLR parsing...")

    parser = RulesEngineParser()

    test_rule = '''
rule "Test Rule":
    if applicant.age > 18 then approveApplication
    '''

    result = parser.validate_syntax(test_rule)

    print(f"✅ Parse valid: {result['valid']}")
    print(f"📄 Errors: {result['errors']}")
    print(f"⚠️ Warnings: {result['warnings']}")

    if result['valid']:
        # Test rule info extraction
        info = parser.extract_rule_info(test_rule)
        print(f"📋 Rule info: {info}")

    return result['valid']


def test_java_code_generation():
    """Test Java code generation from parsed AST."""
    print("\n🏗️ Testing Java code generation...")

    parser = RulesEngineParser()
    generator = AdvancedJavaCodeGenerator()

    test_rule = '''
rule "SimpleRule":
    if applicant.age >= 21 and applicant.income > 50000 then
        approveApplication,
        setLimit(10000)
    else
        rejectApplication
    '''

    tree, error_listener = parser.parse(test_rule)

    if tree and not error_listener.errors:
        java_code = generator.generate(test_rule, "SimpleRule")
        print("✅ Java code generated successfully")
        print("📄 Generated code preview:")
        print("=" * 50)
        print(java_code[:500] + "..." if len(java_code) > 500 else java_code)
        print("=" * 50)
        return True
    else:
        print(f"❌ Parse failed: {error_listener.errors}")
        return False


def test_rule_validation():
    """Test comprehensive rule validation."""
    print("\n🔍 Testing rule validation...")

    validator = RuleValidator()

    # Test valid rule
    valid_rule = '''
rule "ValidRule":
    if applicant.age > 18 then approveApplication
    '''

    result = validator.validate_rule(valid_rule)
    print(f"✅ Valid rule validation: {result['valid']}")

    # Test invalid rule
    invalid_rule = '''
rule "InvalidRule":
    if applicant.age > then  # Missing operand
    '''

    result = validator.validate_rule(invalid_rule)
    print(f"❌ Invalid rule validation: {result['valid']} (should be False)")
    print(f"📄 Errors found: {len(result['errors'])}")

    return True


def test_python_rules_engine():
    """Test the complete Python rules engine."""
    print("\n⚙️ Testing Python Rules Engine...")

    engine = PythonRulesEngine()

    test_rule = '''
rule "EngineTest":
    if applicant.creditScore > 700 then
        approveApplication,
        setLimit(5000)
    else
        requestDocumentation
    '''

    # Test validation
    validation_result = engine.validate_rule(test_rule)
    print(f"✅ Engine validation: {validation_result['valid']}")
    if not validation_result['valid']:
        print(f"📄 Validation errors: {validation_result.get('errors', [])}")
        print(f"⚠️ Validation warnings: {validation_result.get('warnings', [])}")

    # Test compilation
    if validation_result['valid']:
        compilation_result = engine.compile_rule(test_rule)
        print(f"🔧 Compilation: {compilation_result['success']}")

        if compilation_result['success']:
            print(f"📋 Class name: {compilation_result['className']}")
            print(f"⏱️ Compilation time: {compilation_result['compilationTimeMs']}ms")

    # Test autocomplete
    autocomplete_result = engine.get_autocomplete_suggestions("if applicant.", 12)
    print(f"💡 Autocomplete suggestions: {len(autocomplete_result['suggestions'])}")

    return validation_result['valid']


def test_context_analysis():
    """Test context analysis for autocomplete."""
    print("\n🧠 Testing context analysis...")

    validator = RuleValidator()

    rule_content = "if applicant.age > 18 then approveApplication"

    # Test different cursor positions
    positions = [3, 11, 15, 25, 30, 45]

    for pos in positions:
        context = validator.get_completion_context(rule_content, pos)
        print(f"📍 Position {pos}: {context['context_type']} (in_condition: {context.get('in_condition', False)})")

    return True


def main():
    """Run all tests."""
    print("🚀 Python ANTLR Rules Engine Test Suite")
    print("=" * 50)

    tests = [
        ("Basic Parsing", test_basic_parsing),
        ("Java Code Generation", test_java_code_generation),
        ("Rule Validation", test_rule_validation),
        ("Python Rules Engine", test_python_rules_engine),
        ("Context Analysis", test_context_analysis)
    ]

    results = []

    for test_name, test_func in tests:
        try:
            print(f"\n🧪 Running {test_name}...")
            result = test_func()
            results.append((test_name, result, None))
            print(f"{'✅' if result else '❌'} {test_name}: {'PASSED' if result else 'FAILED'}")
        except Exception as e:
            results.append((test_name, False, str(e)))
            print(f"💥 {test_name}: ERROR - {str(e)}")

    # Summary
    print("\n" + "=" * 50)
    print("📊 Test Results Summary:")

    passed = 0
    total = len(results)

    for test_name, result, error in results:
        status = "✅ PASSED" if result else "❌ FAILED"
        if error:
            status += f" (Error: {error})"
        print(f"  {status}: {test_name}")
        if result:
            passed += 1

    print(f"\n🎯 Overall: {passed}/{total} tests passed")

    if passed == total:
        print("🎉 All tests passed! Python ANTLR implementation is working correctly.")
        return 0
    else:
        print("⚠️ Some tests failed. Check the implementation.")
        return 1


if __name__ == "__main__":
    sys.exit(main())