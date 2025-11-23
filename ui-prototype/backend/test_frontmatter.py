"""
Test frontmatter handling in semantic validation.
"""

import requests
import json

BACKEND_URL = "http://localhost:5002"
ENDPOINT = f"{BACKEND_URL}/api/validate/semantic"

def test_with_frontmatter():
    """Test validation with YAML frontmatter."""
    print("\n✓ Testing rule WITH frontmatter...")

    rule_content = """---
client_code: DEMO
process_group_code: CC_PREM
process_area_code: CREDIT_LIMITS
---

rule "Cash Advance Processing":
    if applicant.creditScore > 700 then
        approveTransaction()
    endif
"""

    response = requests.post(ENDPOINT, json={"content": rule_content})
    print(f"  Status: {response.status_code}")

    if response.status_code == 200:
        result = response.json()
        print(f"  Valid: {result['valid']}")
        print(f"  Errors: {len(result.get('errors', []))}")
        print(f"  Warnings: {len(result.get('warnings', []))}")

        if result['valid']:
            print("  ✅ Rule with frontmatter validated successfully")
        else:
            print("  ❌ Validation failed")
            for err in result.get('errors', []):
                print(f"     Line {err['line']}: {err['message']}")
    else:
        print(f"  ❌ Request failed: {response.text}")


def test_without_frontmatter():
    """Test validation without frontmatter."""
    print("\n✓ Testing rule WITHOUT frontmatter...")

    rule_content = """rule "Simple Rule":
    if applicant.creditScore > 700 then
        approveTransaction()
    endif
"""

    response = requests.post(ENDPOINT, json={"content": rule_content})
    print(f"  Status: {response.status_code}")

    if response.status_code == 200:
        result = response.json()
        print(f"  Valid: {result['valid']}")
        print(f"  Errors: {len(result.get('errors', []))}")

        if result['valid']:
            print("  ✅ Rule without frontmatter validated successfully")
        else:
            print("  ❌ Validation failed")
    else:
        print(f"  ❌ Request failed: {response.text}")


def test_line_numbers_with_frontmatter():
    """Test that line numbers are correctly adjusted."""
    print("\n✓ Testing line number adjustment...")

    # Error on line 10 of original file (line 4 after stripping frontmatter)
    rule_content = """---
client_code: DEMO
process_group_code: CC_PREM
process_area_code: CREDIT_LIMITS
---

rule "Line Number Test":
    if applicant.creditScore > 700 then
        unknownAction()
    endif
"""

    response = requests.post(ENDPOINT, json={"content": rule_content})
    print(f"  Status: {response.status_code}")

    if response.status_code == 200:
        result = response.json()
        print(f"  Valid: {result['valid']}")

        if result.get('errors'):
            error = result['errors'][0]
            print(f"  Error at line: {error['line']} (should be 9)")
            print(f"  Message: {error['message']}")

            # Line 9 is where unknownAction() appears in original file
            if error['line'] == 9:
                print("  ✅ Line numbers correctly adjusted for frontmatter")
            else:
                print(f"  ❌ Expected line 9, got line {error['line']}")
        else:
            print("  ❌ Expected error but got none")
    else:
        print(f"  ❌ Request failed: {response.text}")


def main():
    print("=" * 60)
    print("Frontmatter Handling Tests")
    print("=" * 60)

    test_with_frontmatter()
    test_without_frontmatter()
    test_line_numbers_with_frontmatter()

    print("\n" + "=" * 60)
    print("Tests completed")
    print("=" * 60)


if __name__ == "__main__":
    main()
