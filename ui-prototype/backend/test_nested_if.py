#!/usr/bin/env python3
"""
Test script for nested IF support implementation.
Tests backward compatibility, basic nesting, deep nesting, and depth limit enforcement.
"""

from grammar_parser.rule_validator import RuleValidator
from grammar_parser.template_code_generator import TemplateCodeGenerator


def test_backward_compatibility():
    """Test that existing rules without nesting still work."""
    print("\n" + "="*60)
    print("TEST 1: Backward Compatibility")
    print("="*60)

    rule_content = """
rule "Simple Rule":
    if transaction.amount > 1000 then
        flagForReview
    elseif transaction.amount > 500 then
        requestDocumentation
    else
        autoApprove
    endif
"""

    validator = RuleValidator()
    result = validator.validate_rule(rule_content)

    print(f"Valid: {result['valid']}")
    if result['errors']:
        print(f"Errors: {result['errors']}")
    else:
        print("✅ Backward compatibility maintained")

        # Try code generation
        generator = TemplateCodeGenerator()
        try:
            java_code = generator.generate(rule_content)
            print("✅ Code generation successful")
            print(f"Generated {len(java_code)} characters of Java code")
        except Exception as e:
            print(f"❌ Code generation failed: {e}")

    return result['valid']


def test_basic_nesting():
    """Test basic 2-level nesting."""
    print("\n" + "="*60)
    print("TEST 2: Basic Nesting (2 levels)")
    print("="*60)

    rule_content = """
rule "Nested Rule":
    if transaction.type == 'purchase' then
        if transaction.amount > 1000 then
            flagForReview, notifyManager
        else
            autoApprove
        endif
    else
        standardProcess
    endif
"""

    validator = RuleValidator()
    result = validator.validate_rule(rule_content)

    print(f"Valid: {result['valid']}")
    if result['errors']:
        print(f"Errors: {result['errors']}")
    else:
        print("✅ Basic nesting works")

        # Try code generation
        generator = TemplateCodeGenerator()
        try:
            java_code = generator.generate(rule_content)
            print("✅ Code generation successful")
            print(f"Generated {len(java_code)} characters of Java code")
            # Show a snippet of the generated code
            print("\nGenerated Java code snippet:")
            lines = java_code.split('\n')
            for i, line in enumerate(lines[20:35]):  # Show middle portion
                print(f"  {i+20}: {line}")
        except Exception as e:
            print(f"❌ Code generation failed: {e}")

    return result['valid']


def test_moderate_nesting():
    """Test moderate nesting (5 levels)."""
    print("\n" + "="*60)
    print("TEST 3: Moderate Nesting (5 levels)")
    print("="*60)

    rule_content = """
rule "Deeply Nested Rule":
    if level1 == true then
        if level2 == true then
            if level3 == true then
                if level4 == true then
                    if level5 == true then
                        action5
                    else
                        action5_else
                    endif
                else
                    action4
                endif
            else
                action3
            endif
        else
            action2
        endif
    else
        action1
    endif
"""

    validator = RuleValidator()
    result = validator.validate_rule(rule_content)

    print(f"Valid: {result['valid']}")
    print(f"Max depth reached: {result.get('max_depth_reached', 'N/A')}")
    if result['errors']:
        print(f"Errors: {result['errors']}")
    else:
        print("✅ Moderate nesting works")

        # Try code generation
        generator = TemplateCodeGenerator()
        try:
            java_code = generator.generate(rule_content)
            print("✅ Code generation successful")
            print(f"Generated {len(java_code)} characters of Java code")
        except Exception as e:
            print(f"❌ Code generation failed: {e}")

    return result['valid']


def test_depth_limit():
    """Test that depth limit of 32 is enforced."""
    print("\n" + "="*60)
    print("TEST 4: Depth Limit Enforcement (33 levels)")
    print("="*60)

    # Generate rule with 33 nested levels (exceeds limit of 32)
    rule_lines = ['rule "Excessive Nesting":']

    for i in range(1, 34):  # 33 levels
        indent = "    " * i
        rule_lines.append(f"{indent}if level{i} == true then")
        rule_lines.append(f"{indent}    action{i}")

    # Close all the ifs
    for i in range(33, 0, -1):
        indent = "    " * i
        rule_lines.append(f"{indent}endif")

    rule_content = "\n".join(rule_lines)

    validator = RuleValidator()
    result = validator.validate_rule(rule_content)

    print(f"Valid: {result['valid']}")
    print(f"Max depth reached: {result.get('max_depth_reached', 'N/A')}")
    print(f"Error type: {result.get('error_type', 'N/A')}")

    if result['errors']:
        print(f"✅ Depth limit enforced correctly")
        print(f"Error message: {result['errors'][0].get('message', 'N/A')}")
    else:
        print("❌ Depth limit NOT enforced - should have failed!")

    return not result['valid']  # Should be invalid


def test_within_depth_limit():
    """Test that exactly 32 levels is allowed."""
    print("\n" + "="*60)
    print("TEST 5: Maximum Allowed Depth (32 levels)")
    print("="*60)

    # Generate rule with exactly 32 nested levels
    rule_lines = ['rule "Maximum Nesting":']

    for i in range(1, 33):  # 32 levels
        indent = "    " * i
        rule_lines.append(f"{indent}if level{i} == true then")
        rule_lines.append(f"{indent}    action{i}")

    # Close all the ifs
    for i in range(32, 0, -1):
        indent = "    " * i
        rule_lines.append(f"{indent}endif")

    rule_content = "\n".join(rule_lines)

    validator = RuleValidator()
    result = validator.validate_rule(rule_content)

    print(f"Valid: {result['valid']}")
    print(f"Max depth reached: {result.get('max_depth_reached', 'N/A')}")

    if result['errors']:
        print(f"Errors: {result['errors']}")
        print("❌ Should allow exactly 32 levels!")
    else:
        print("✅ Maximum depth of 32 allowed correctly")

        # Try code generation
        generator = TemplateCodeGenerator()
        try:
            java_code = generator.generate(rule_content)
            print("✅ Code generation successful for max depth")
            print(f"Generated {len(java_code)} characters of Java code")
        except Exception as e:
            print(f"❌ Code generation failed: {e}")

    return result['valid']


def test_mixed_nesting():
    """Test nesting with elseif and else branches."""
    print("\n" + "="*60)
    print("TEST 6: Mixed Nesting with ELSEIF and ELSE")
    print("="*60)

    rule_content = """
rule "Mixed Nested Rule":
    if transaction.type == 'purchase' then
        if transaction.amount > 1000 then
            if transaction.country == 'US' then
                reviewUS
            elseif transaction.country == 'CA' then
                reviewCA
            else
                reviewInternational
            endif
        else
            autoApprove
        endif
    elseif transaction.type == 'refund' then
        if transaction.amount > 500 then
            approveRefund
        else
            autoRefund
        endif
    else
        standardProcess
    endif
"""

    validator = RuleValidator()
    result = validator.validate_rule(rule_content)

    print(f"Valid: {result['valid']}")
    if result['errors']:
        print(f"Errors: {result['errors']}")
    else:
        print("✅ Mixed nesting with elseif/else works")

        # Try code generation
        generator = TemplateCodeGenerator()
        try:
            java_code = generator.generate(rule_content)
            print("✅ Code generation successful")
            print(f"Generated {len(java_code)} characters of Java code")
        except Exception as e:
            print(f"❌ Code generation failed: {e}")

    return result['valid']


def main():
    """Run all tests."""
    print("\n" + "="*60)
    print("NESTED IF SUPPORT - COMPREHENSIVE TEST SUITE")
    print("="*60)

    results = {
        "Backward Compatibility": test_backward_compatibility(),
        "Basic Nesting (2 levels)": test_basic_nesting(),
        "Moderate Nesting (5 levels)": test_moderate_nesting(),
        "Depth Limit Enforcement": test_depth_limit(),
        "Maximum Allowed Depth (32)": test_within_depth_limit(),
        "Mixed Nesting (elseif/else)": test_mixed_nesting()
    }

    print("\n" + "="*60)
    print("TEST SUMMARY")
    print("="*60)

    passed = sum(1 for v in results.values() if v)
    total = len(results)

    for test_name, result in results.items():
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"{status} - {test_name}")

    print(f"\n{passed}/{total} tests passed")

    if passed == total:
        print("\n🎉 All tests passed! Nested IF support is working correctly.")
    else:
        print(f"\n⚠️  {total - passed} test(s) failed. Please review the errors above.")

    return passed == total


if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)
