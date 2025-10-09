"""
Demo script to test the unit test generator.
"""

import sys
from pathlib import Path

# Add paths for imports
sys.path.insert(0, str(Path(__file__).parent / 'grammar_parser'))

from template_code_generator import TemplateCodeGenerator

# Test rule: Monthly Fee Application (from database)
MONTHLY_FEE_RULE = """rule "Monthly Fee Application":
    if account.tier == 'PREMIUM' and account.monthlySpend < 1000 then
        applyMonthlyFee(25.00),
        notifyCustomer('Monthly maintenance fee applied')
    elseif account.tier == 'STANDARD' and account.balance < 500 then
        applyMonthlyFee(10.00),
        notifyCustomer('Low balance fee applied')
    else
        waiveFee('Spending threshold met')
    endif"""

# Test rule: Simple Credit Score Check
CREDIT_SCORE_RULE = """rule "Credit Score Check":
    if applicant.creditScore >= 750 then
        approveApplication("Excellent credit")
    elseif applicant.creditScore >= 650 then
        approveApplication("Good credit"),
        requireManualReview("Borderline credit score")
    else
        denyApplication("Insufficient credit score")
    endif"""

def test_rule_generation(rule_content, rule_name):
    """Test generating both production code and tests."""

    print(f"\n{'='*80}")
    print(f"Testing: {rule_name}")
    print(f"{'='*80}\n")

    generator = TemplateCodeGenerator()

    try:
        # Generate both production and test code
        production_code, test_code = generator.generate_with_tests(rule_content, item_type='rule')

        print(f"✅ Production code generated ({len(production_code)} chars)")
        print(f"✅ Test code generated ({len(test_code)} chars)")

        # Show test code
        print(f"\n{'-'*80}")
        print("Generated Test Code:")
        print(f"{'-'*80}\n")
        print(test_code)

        return True

    except Exception as e:
        print(f"❌ Error: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == '__main__':
    print("Unit Test Generator Demo")
    print("=" * 80)

    # Test rules
    test_rule_generation(MONTHLY_FEE_RULE, "Monthly Fee Application")
    test_rule_generation(CREDIT_SCORE_RULE, "Credit Score Check")
