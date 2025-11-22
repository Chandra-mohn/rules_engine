"""
End-to-End Test: Complete code generation flow
Tests: API → Parser → Generator → Java Code → Quality Review
"""

import sys
from pathlib import Path
from services.python_rules_engine import PythonRulesEngine
from antlr4 import InputStream, CommonTokenStream

# Import ANTLR classes
sys.path.append(str(Path(__file__).parent / 'java-bridge' / 'src' / 'main' / 'antlr4' / 'com' / 'rules' / 'grammar'))
from RulesLexer import RulesLexer
from RulesParser import RulesParser


def test_e2e_standard_rule():
    """E2E test for standard rule with all features."""

    print("\n" + "="*80)
    print("END-TO-END TEST: Standard Rule Code Generation")
    print("="*80)

    # Sample rule using all enhanced features
    dsl_rule = '''
    rule "Credit Card Approval V2":
        if applicant.creditScore >= 700 and applicant.annualIncome >= 60000 then
            approveApplication,
            setLimit(applicant.annualIncome * 0.3),
            applyBonus(applicant.creditScore / 10)
        else if applicant.creditScore >= 650 and applicant.employmentYears >= 3 then
            approveApplication,
            setLimit(applicant.annualIncome * 0.2)
        else
            rejectApplication,
            sendNotification("Credit score too low")
    '''

    print("\n" + "-"*80)
    print("STEP 1: DSL Rule Input")
    print("-"*80)
    print(dsl_rule)

    # Initialize engine
    engine = PythonRulesEngine()

    # Step 2: Validate Rule
    print("\n" + "-"*80)
    print("STEP 2: Rule Validation")
    print("-"*80)

    validation_result = engine.validate_rule(dsl_rule)

    print(f"Valid: {validation_result['valid']}")
    print(f"Errors: {validation_result['errors']}")
    print(f"Warnings: {validation_result['warnings']}")

    if validation_result.get('undefined_actions'):
        print(f"Undefined Actions: {validation_result['undefined_actions']}")

    # Step 3: Parse and Generate Code
    print("\n" + "-"*80)
    print("STEP 3: Code Generation (ANTLR → AST → Java)")
    print("-"*80)

    try:
        # Parse DSL
        input_stream = InputStream(dsl_rule)
        lexer = RulesLexer(input_stream)
        stream = CommonTokenStream(lexer)
        parser = RulesParser(stream)
        tree = parser.ruleSet()

        print("✅ ANTLR parsing successful")
        print(f"   Parse tree: {tree.toStringTree(recog=parser)[:100]}...")

        # Generate Java code
        java_code = engine.code_generator.generate_code(tree.rule_(0), rule_type='standard_rule')

        print("✅ Code generation successful")
        print(f"   Generated code: {len(java_code)} characters, {len(java_code.split(chr(10)))} lines")

    except Exception as e:
        print(f"❌ Code generation failed: {e}")
        import traceback
        traceback.print_exc()
        return False

    # Step 4: Review Generated Code
    print("\n" + "-"*80)
    print("STEP 4: Generated Java Code Review")
    print("-"*80)
    print("\n📄 Full Generated Code:")
    print("="*80)
    print(java_code)
    print("="*80)

    # Step 5: Quality Analysis
    print("\n" + "-"*80)
    print("STEP 5: Code Quality Analysis")
    print("-"*80)

    quality_checks = {
        "Package declaration": "package com.rules;" in java_code,
        "Imports present": "import java.util.*;" in java_code,
        "Class declaration": "public class" in java_code and "Rule {" in java_code,
        "RuleResult inner class": "public static class RuleResult" in java_code,
        "evaluate() method": "public static RuleResult evaluate(Map<String, Object> context)" in java_code,

        # Feature checks
        "Entity extraction": "Map<String, Object> applicant" in java_code,
        "Arithmetic multiplication": "* 0.3" in java_code or "* 0.2" in java_code,
        "Arithmetic division": "/ 10" in java_code,
        "Conditional logic": "if (" in java_code and "else" in java_code,
        "Action execution": "actions.add(" in java_code,
        "Matched flag": "matched = true" in java_code,

        # Helper methods
        "_getFieldValue present": "_getFieldValue" in java_code,
        "_equals present": "_equals" in java_code,
        "_compareTo present": "_compareTo" in java_code,
        "_toNumber present": "_toNumber" in java_code,

        # Code quality
        "Null safety": "!= null" in java_code,
        "Exception handling": "catch (Exception" in java_code or "catch (NumberFormatException" in java_code,
        "Proper formatting": "{" in java_code and "}" in java_code,
        "String escaping": '\\"' in java_code or 'actions.add("' in java_code,
    }

    passed = 0
    failed = 0

    for check, result in quality_checks.items():
        status = "✅" if result else "❌"
        print(f"{status} {check:<35} {'PASS' if result else 'FAIL'}")
        if result:
            passed += 1
        else:
            failed += 1

    # Step 6: Feature Validation
    print("\n" + "-"*80)
    print("STEP 6: Enhanced Features Validation")
    print("-"*80)

    feature_validation = {
        "Arithmetic in actions": java_code.count("* 0.") >= 1,
        "Nested if-else": java_code.count("if (") >= 2 and "else" in java_code,
        "Multiple actions per step": java_code.count("actions.add(") >= 3,
        "Parameterized actions": "setLimit(" in java_code or "applyBonus(" in java_code,
        "String literals in actions": '"Credit score too low"' in java_code or 'Credit score' in java_code,
    }

    for feature, result in feature_validation.items():
        status = "✅" if result else "❌"
        print(f"{status} {feature:<35} {'PRESENT' if result else 'MISSING'}")
        if result:
            passed += 1
        else:
            failed += 1

    # Step 7: Code Metrics
    print("\n" + "-"*80)
    print("STEP 7: Code Metrics")
    print("-"*80)

    lines = java_code.split('\n')
    total_lines = len(lines)
    code_lines = len([l for l in lines if l.strip() and not l.strip().startswith('//')])
    comment_lines = len([l for l in lines if l.strip().startswith('//')])
    blank_lines = len([l for l in lines if not l.strip()])

    print(f"Total lines:        {total_lines}")
    print(f"Code lines:         {code_lines}")
    print(f"Comment lines:      {comment_lines}")
    print(f"Blank lines:        {blank_lines}")
    print(f"Average complexity: Estimated LOW (hot path)")

    # Step 8: Summary
    print("\n" + "="*80)
    print("TEST SUMMARY")
    print("="*80)

    total_checks = passed + failed
    pass_rate = (passed / total_checks * 100) if total_checks > 0 else 0

    print(f"Total checks:  {total_checks}")
    print(f"Passed:        {passed}")
    print(f"Failed:        {failed}")
    print(f"Pass rate:     {pass_rate:.1f}%")

    if pass_rate >= 90:
        grade = "A"
        status = "✅ EXCELLENT"
    elif pass_rate >= 80:
        grade = "B"
        status = "✅ GOOD"
    elif pass_rate >= 70:
        grade = "C"
        status = "⚠️  ACCEPTABLE"
    else:
        grade = "F"
        status = "❌ NEEDS IMPROVEMENT"

    print(f"\nGrade:         {grade}")
    print(f"Status:        {status}")

    # Save generated code
    output_dir = Path(__file__).parent.parent / 'generated-rules' / 'test-e2e'
    output_dir.mkdir(parents=True, exist_ok=True)
    output_file = output_dir / 'CreditCardApprovalV2Rule.java'
    output_file.write_text(java_code)

    print(f"\n💾 Generated code saved to:")
    print(f"   {output_file}")

    print("\n" + "="*80)
    print("E2E TEST COMPLETE")
    print("="*80)

    return pass_rate >= 80


if __name__ == '__main__':
    success = test_e2e_standard_rule()
    sys.exit(0 if success else 1)
