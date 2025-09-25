#!/usr/bin/env python3
"""Debug what advanced mode returns."""

import sys
from pathlib import Path

# Add backend to path
backend_path = Path(__file__).parent / "backend"
sys.path.insert(0, str(backend_path))

from grammar_parser.unified_java_generator import UnifiedJavaCodeGenerator

simple_rule = '''rule "Basic Credit Check":
if applicant.creditScore >= 700 then approveApplication
else rejectApplication'''

generator = UnifiedJavaCodeGenerator(mode='advanced')
result = generator.generate(simple_rule, "BasicCreditCheck")

print(f"Result type: {type(result)}")
print(f"Result length: {len(result) if result else 'None'}")
print("First 500 chars of result:")
print(result[:500] if result else "None")

print("\n" + "="*50)
print("Looking for class name patterns:")
if result:
    if "BasicCreditCheckRule" in result:
        print("✅ Found BasicCreditCheckRule")
    else:
        print("❌ BasicCreditCheckRule not found")

    if "BasicCreditCheck" in result:
        print("✅ Found BasicCreditCheck")
    else:
        print("❌ BasicCreditCheck not found")

    # Look for any class declarations
    import re
    class_matches = re.findall(r'public class (\w+)', result)
    print(f"Found class declarations: {class_matches}")