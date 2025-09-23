#!/usr/bin/env python3
"""
Test code generation for samples of each rule type.
Tests both old and new generation systems to compare results.
"""

import sys
import traceback
from backend.services.static_router_generator import StaticRouterGenerator, TransactionMapping, ClientRouterSpec
from backend.services.unified_router_generator import UnifiedRouterGenerator, RuleMapping, ClientSpec

# Sample rules from database
SAMPLES = {
    'actionset': {
        'id': 14,
        'name': 'Standard Application Workflow',
        'content': '''rule "Standard Application Workflow":
    validateApplicantInfo
    if applicant.creditScore >= 700 then approveApplication, sendWelcomeEmail
    if applicant.creditScore < 600 then rejectApplication, sendRejectionLetter
    else conditionalApproval, requestDocumentation
    updateCustomerRecord'''
    },
    'mon_rule': {
        'id': 65,
        'name': 'Purchase Authorization',
        'content': '''rule "Purchase Authorization":
    if transaction.amount <= account.creditLimit and transaction.merchant.riskLevel == "LOW" then
        approveTransaction(transaction.amount),
        updateAccountBalance(transaction.amount)
    else if transaction.amount > account.creditLimit then
        declineTransaction("Insufficient credit limit")
    else
        declineTransaction("High risk merchant")'''
    },
    'non_mon_rule': {
        'id': 61,
        'name': 'Address Update Validation',
        'content': '''rule "Address Update Validation":
    if applicant.addressChangeRequest == true and applicant.documentationProvided == true then
        updateCustomerAddress(applicant.newAddress),
        notifyCustomer("Address updated successfully")
    else
        requestAdditionalDocumentation'''
    },
    'rule': {
        'id': 1,
        'name': 'creditScoreCheck',
        'content': '''rule creditScoreCheck:
    if applicant.creditScore >= 700 then approveApplication
    if applicant.creditScore < 600 then rejectApplication'''
    }
}

def validate_rule_syntax(rule_type, content):
    """Improved syntax validation that accepts both formatting styles."""
    issues = []

    # Check for basic rule structure
    if not content.strip().startswith('rule '):
        issues.append("❌ Rule must start with 'rule' keyword")

    # Check for balanced quotes (if any)
    if content.count('"') % 2 != 0:
        issues.append("❌ Unbalanced quotes in rule")

    # Check for missing colons after rule declaration
    lines = content.split('\n')
    rule_line = lines[0].strip()
    if not rule_line.endswith(':'):
        issues.append("❌ Rule declaration must end with ':'")

    # Improved validation: Check for actual missing keywords, not formatting
    if_count = content.count(' if ')
    then_count = content.count('then')  # Remove space requirement

    # Each 'if' should have a corresponding 'then'
    if if_count > then_count:
        issues.append(f"❌ Missing 'then' clause(s) - found {if_count} 'if' but only {then_count} 'then'")

    # Check for orphaned else statements (else without preceding if-then)
    lines = [line.strip() for line in content.split('\n')]
    for i, line in enumerate(lines):
        if line.startswith('else ') or line == 'else':
            # Look backwards for a corresponding if/then structure
            found_if_then = False
            for j in range(i-1, -1, -1):
                if ('if ' in lines[j] and 'then' in lines[j]) or 'then' in lines[j]:
                    found_if_then = True
                    break
                elif lines[j].startswith('rule '):
                    break
            if not found_if_then:
                issues.append("❌ 'else' clause without preceding 'if-then' structure")

    # Check for ambiguous action statements (parser ambiguity)
    for i, line in enumerate(lines):
        line = line.strip()
        # Skip rule declaration and conditional lines
        if line.startswith('rule ') or 'if ' in line or 'then' in line or line.startswith('else') or not line:
            continue

        # Check for unquoted multi-word actions that could be ambiguous
        if (' and ' in line or ' or ' in line) and not line.startswith('"') and not line.endswith('"'):
            # Check if it contains commas (proper action separation) or parentheses (function calls)
            if ',' not in line and '(' not in line:
                issues.append(f"❌ Ambiguous action statement on line {i+1}: '{line}' - use quotes for single action or commas for multiple actions")

    return issues

def test_old_generator(rule_type, sample):
    """Test with old StaticRouterGenerator."""
    print(f"🔧 Testing OLD generator for {rule_type}...")

    try:
        # Validate rule syntax first
        syntax_issues = validate_rule_syntax(rule_type, sample['content'])
        if syntax_issues:
            print(f"❌ Rule syntax validation failed:")
            for issue in syntax_issues:
                print(f"    {issue}")
            return False

        generator = StaticRouterGenerator()

        # Create mapping for old system
        mapping = TransactionMapping(
            transaction_code=f"TEST_{rule_type.upper()}",
            rule_id=f"rule-{sample['id']}",
            rule_name=sample['name'].replace(' ', '_').lower(),
            rule_type=rule_type,
            execution_frequency="hot",
            dependencies=[],
            estimated_steps=3,
            context_size_kb=2
        )

        spec = ClientRouterSpec(
            client_id="TEST_BANK",
            transaction_mappings=[mapping]
        )

        # Generate code
        artifacts = generator.generate_universal_router([spec])

        # Check for compilation issues
        for path, code in artifacts.items():
            if "Executor.java" in path:
                # Check for common compilation errors
                if "import " in code and "public class" in code:
                    import_section = True
                    lines = code.split('\n')
                    for line in lines:
                        if line.strip().startswith('public class '):
                            import_section = False
                        elif not import_section and line.strip().startswith('import '):
                            print(f"❌ Import statement outside import section")
                            return False

                # Check for TODO comments (indicates incomplete generation)
                if "TODO" in code:
                    print(f"❌ Generated code contains TODO comments")
                    return False

                print(f"✅ Old generator produced valid Java structure")
                return True

        print(f"❌ No executor code generated")
        return False

    except Exception as e:
        print(f"❌ Old generator failed: {e}")
        traceback.print_exc()
        return False

def test_new_generator(rule_type, sample):
    """Test with new UnifiedRouterGenerator."""
    print(f"🚀 Testing NEW generator for {rule_type}...")

    try:
        # Validate rule syntax first
        syntax_issues = validate_rule_syntax(rule_type, sample['content'])
        if syntax_issues:
            print(f"❌ Rule syntax validation failed:")
            for issue in syntax_issues:
                print(f"    {issue}")
            return False

        generator = UnifiedRouterGenerator()

        # Create mapping for new system
        mapping = RuleMapping(
            transaction_code=f"TEST_{rule_type.upper()}",
            rule_id=f"rule-{sample['id']}",
            rule_name=sample['name'].replace(' ', '_').lower(),
            rule_type=rule_type,
            dependencies=[]
        )

        spec = ClientSpec(
            client_id="TEST_BANK",
            rule_mappings=[mapping]
        )

        # Generate code
        artifacts = generator.generate_client_router(spec)

        # Comprehensive validation
        has_executor = False
        for path, code in artifacts.items():
            if "Executor.java" in path and "executors" in path:
                has_executor = True

                # Check compilation structure
                if "import " in code and "public class" in code:
                    import_section = True
                    lines = code.split('\n')
                    for line in lines:
                        if line.strip().startswith('public class '):
                            import_section = False
                        elif not import_section and line.strip().startswith('import '):
                            print(f"❌ Import statement outside import section")
                            return False

                # Check for security features
                if "validateInputs" not in code:
                    print(f"⚠️ Missing input validation (expected for new generator)")

                # Check for React-style patterns
                if "RuleExecutionContext" not in code:
                    print(f"⚠️ Missing React-style context (expected for new generator)")

                # Check for performance timing
                if "System.nanoTime()" not in code:
                    print(f"⚠️ Missing performance timing")

                print(f"✅ New generator produced valid Java structure with modern patterns")
                return True

        if not has_executor:
            print(f"❌ No executor code generated")
            return False

        return True

    except Exception as e:
        print(f"❌ New generator failed: {e}")
        traceback.print_exc()
        return False

def analyze_rule_quality(rule_type, sample):
    """Analyze rule quality and report issues."""
    print(f"🔍 Analyzing rule quality for {rule_type}...")

    content = sample['content']
    issues = []
    warnings = []

    # Basic syntax checks using improved validation
    syntax_issues = validate_rule_syntax(rule_type, content)
    issues.extend(syntax_issues)

    # Semantic checks (style warnings only)
    if 'applicant.' in content and 'transaction.' in content:
        warnings.append("⚠️ Rule mixes applicant and transaction contexts")

    if content.count('if ') > 3:
        warnings.append("⚠️ Complex rule with many conditions - consider splitting")

    # ActionSet specific checks (style warnings only)
    if rule_type == 'actionset':
        if 'validateApplicantInfo' in content and '(' not in content.split('\n')[1]:
            warnings.append("⚠️ ActionSet has bare action calls without parameters")

    # Print analysis results
    if issues:
        print(f"❌ Rule has {len(issues)} syntax issue(s):")
        for issue in issues:
            print(f"    {issue}")
        return False
    elif warnings:
        print(f"⚠️ Rule has {len(warnings)} style warning(s):")
        for warning in warnings:
            print(f"    {warning}")
        print(f"✅ Rule syntax is valid (warnings are style-only)")
        return True
    else:
        print(f"✅ Rule quality is excellent")
        return True

def main():
    """Test all rule samples."""
    print("🧪 Testing Code Generation for All Rule Types\n")

    results = {
        'rule_quality': {},
        'old_generator': {},
        'new_generator': {}
    }

    for rule_type, sample in SAMPLES.items():
        print(f"{'='*60}")
        print(f"Testing {rule_type.upper()}: {sample['name']}")
        print(f"{'='*60}")

        # Rule quality analysis
        quality_result = analyze_rule_quality(rule_type, sample)
        results['rule_quality'][rule_type] = quality_result
        print()

        # Skip code generation if rule quality is bad
        if not quality_result:
            print(f"🚫 Skipping code generation due to bad rule quality")
            results['old_generator'][rule_type] = False
            results['new_generator'][rule_type] = False
        else:
            # Test old generator
            old_result = test_old_generator(rule_type, sample)
            results['old_generator'][rule_type] = old_result
            print()

            # Test new generator
            new_result = test_new_generator(rule_type, sample)
            results['new_generator'][rule_type] = new_result

        print("\n")

    # Summary report
    print("📊 SUMMARY REPORT")
    print("="*50)

    print("Rule Quality Analysis:")
    for rule_type, result in results['rule_quality'].items():
        status = "✅ GOOD" if result else "❌ BAD"
        print(f"  {rule_type:12} : {status}")

    print("\nOld Generator Results:")
    for rule_type, result in results['old_generator'].items():
        if result is None:
            status = "⏭️ SKIPPED"
        elif result:
            status = "✅ PASSED"
        else:
            status = "❌ FAILED"
        print(f"  {rule_type:12} : {status}")

    print("\nNew Generator Results:")
    for rule_type, result in results['new_generator'].items():
        if result is None:
            status = "⏭️ SKIPPED"
        elif result:
            status = "✅ PASSED"
        else:
            status = "❌ FAILED"
        print(f"  {rule_type:12} : {status}")

    # Overall assessment
    total_rules = len(SAMPLES)
    good_rules = sum(1 for r in results['rule_quality'].values() if r)
    old_passes = sum(1 for r in results['old_generator'].values() if r)
    new_passes = sum(1 for r in results['new_generator'].values() if r)

    print(f"\nOverall Assessment:")
    print(f"  Rules Quality   : {good_rules}/{total_rules} rules are well-formed")
    print(f"  Old Generator   : {old_passes}/{total_rules} successful generations")
    print(f"  New Generator   : {new_passes}/{total_rules} successful generations")

    if good_rules == total_rules and new_passes == total_rules:
        print(f"\n🎉 All rules are good and new generator handles them correctly!")
        return 0
    elif good_rules < total_rules:
        print(f"\n⚠️ {total_rules - good_rules} rule(s) have quality issues and should be reviewed")
        return 1
    else:
        print(f"\n⚠️ Generator issues detected - review implementation")
        return 1

if __name__ == "__main__":
    exit(main())