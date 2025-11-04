#!/usr/bin/env python3
"""Test code generation fix"""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent / 'backend'))

from services.python_rules_engine import PythonRulesEngine

# Test rule with unconditional action after conditional block
test_rule = '''rule "Account Settings Update":
    if customer.requestType == "EMAIL_PREFERENCES"
    then
        updateEmailPreferences(customer.preferences),
        sendConfirmationEmail
    elseif customer.requestType == "PAYMENT_METHOD" and customer.verificationComplete == true
    then
        updatePaymentMethod(customer.newPaymentMethod),
        notifyCustomer("Payment method updated")
    else
        requireIdentityVerification
    endif
    "Additional Card Management"'''

engine = PythonRulesEngine()
result = engine.compile_rule(test_rule, "test-rule")

print(f"Success: {result.get('success')}")
print(f"Valid: {result.get('valid')}")

if result.get('success'):
    print(f"\nClass Name: {result.get('className')}")
    print(f"\nGenerated Java Code (first 500 chars):")
    print(result.get('java_code', '')[:500])
else:
    print(f"\nErrors: {result.get('errors')}")
    print(f"Message: {result.get('message')}")
