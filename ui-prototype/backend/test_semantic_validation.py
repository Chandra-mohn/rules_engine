"""
Test script for semantic validation endpoint.
This validates the /api/validate/semantic endpoint works correctly.
"""

import requests
import json

# Test configuration
BACKEND_URL = "http://localhost:5002"  # Extension backend port
ENDPOINT = f"{BACKEND_URL}/api/validate/semantic"

def test_valid_rule():
    """Test validation of a valid rule."""
    print("\n1. Testing valid rule...")

    rule_content = """rule "Test Valid Rule":
    if applicant.creditScore > 700 then
        approveTransaction()
    endif
"""

    response = requests.post(ENDPOINT, json={"content": rule_content})
    print(f"   Status: {response.status_code}")

    if response.status_code == 200:
        result = response.json()
        print(f"   Valid: {result['valid']}")
        print(f"   Errors: {len(result.get('errors', []))}")
        print(f"   Warnings: {len(result.get('warnings', []))}")

        if result['valid']:
            print("   ✅ Valid rule passed")
        else:
            print("   ❌ Valid rule should not have errors")
            print(f"   Errors: {result.get('errors')}")
    else:
        print(f"   ❌ Request failed: {response.text}")


def test_undefined_action():
    """Test validation with undefined action."""
    print("\n2. Testing undefined action...")

    rule_content = """rule "Test Undefined Action":
    if applicant.creditScore > 700 then
        unknownAction()
    endif
"""

    response = requests.post(ENDPOINT, json={"content": rule_content})
    print(f"   Status: {response.status_code}")

    if response.status_code == 200:
        result = response.json()
        print(f"   Valid: {result['valid']}")
        print(f"   Errors: {len(result.get('errors', []))}")

        if not result['valid'] and result['errors']:
            error = result['errors'][0]
            print(f"   Error message: {error['message']}")
            if 'suggestion' in error:
                print(f"   Suggestion: {error['suggestion']}")
            print("   ✅ Undefined action detected")
        else:
            print("   ❌ Should have detected undefined action")
    else:
        print(f"   ❌ Request failed: {response.text}")


def test_undefined_attribute():
    """Test validation with undefined attribute."""
    print("\n3. Testing undefined attribute...")

    rule_content = """rule "Test Undefined Attribute":
    if applicant.unknownField > 700 then
        approveTransaction()
    endif
"""

    response = requests.post(ENDPOINT, json={"content": rule_content})
    print(f"   Status: {response.status_code}")

    if response.status_code == 200:
        result = response.json()
        print(f"   Valid: {result['valid']}")
        print(f"   Warnings: {len(result.get('warnings', []))}")

        if result.get('warnings'):
            warning = result['warnings'][0]
            print(f"   Warning message: {warning['message']}")
            if 'suggestion' in warning:
                print(f"   Suggestion: {warning['suggestion']}")
            print("   ✅ Undefined attribute warned")
        else:
            print("   ⚠️  Should have warned about undefined attribute")
    else:
        print(f"   ❌ Request failed: {response.text}")


def test_cyclic_dependency():
    """Test validation with cyclic dependency (requires existing actionsets)."""
    print("\n4. Testing cyclic dependency detection...")
    print("   Note: This test requires actionsets to exist in the system")

    # This would require creating actionsets that reference each other
    # For now, just test that the endpoint handles the request
    rule_content = """actionset "ActionA":
    ActionB()
"""

    response = requests.post(ENDPOINT, json={"content": rule_content})
    print(f"   Status: {response.status_code}")

    if response.status_code == 200:
        result = response.json()
        print(f"   Valid: {result['valid']}")
        print("   ✅ Cyclic dependency check completed")
    else:
        print(f"   ❌ Request failed: {response.text}")


def test_health_check():
    """Test backend health check."""
    print("\n0. Testing backend health...")

    try:
        response = requests.get(f"{BACKEND_URL}/api/health", timeout=2)
        if response.status_code == 200:
            print("   ✅ Backend is running")
            return True
        else:
            print(f"   ❌ Backend returned status {response.status_code}")
            return False
    except requests.exceptions.ConnectionError:
        print("   ❌ Backend is not running")
        print("   Please start the backend: cd backend && python app.py")
        return False
    except Exception as e:
        print(f"   ❌ Error: {e}")
        return False


def main():
    """Run all tests."""
    print("=" * 60)
    print("Semantic Validation Endpoint Tests")
    print("=" * 60)

    if not test_health_check():
        print("\n❌ Cannot proceed - backend not running")
        return

    test_valid_rule()
    test_undefined_action()
    test_undefined_attribute()
    test_cyclic_dependency()

    print("\n" + "=" * 60)
    print("Tests completed")
    print("=" * 60)


if __name__ == "__main__":
    main()
