#!/usr/bin/env python3
"""Debug script to test hot compilation integration."""

from services.java_bridge import JavaBridge
import json

def test_hot_compilation():
    """Test the hot compilation flow step by step."""
    bridge = JavaBridge(server_url="http://localhost:8081")
    
    rule_content = "rule creditCheck: if CREDIT_SCORE >= 750 then APPROVE"
    test_data = {"CREDIT_SCORE": 800}
    
    print("=== Testing Hot Compilation Debug ===")
    print(f"Rule content: {rule_content}")
    print(f"Test data: {test_data}")
    print()
    
    # Step 0: Test basic connectivity
    print("Step 0: Testing basic connectivity...")
    import requests
    try:
        response = requests.get("http://localhost:8081/api/health", timeout=5)
        print(f"Health check response: {response.status_code} - {response.text}")
    except Exception as e:
        print(f"Health check failed: {e}")
    print()
    
    # Step 0.5: Test direct POST to compilation endpoint
    print("Step 0.5: Testing direct POST to compilation endpoint...")
    try:
        response = requests.post(
            "http://localhost:8081/api/rules/compile",
            json={"ruleContent": rule_content, "ruleId": "direct_test"},
            timeout=10
        )
        print(f"Direct POST response: {response.status_code} - {response.text}")
    except Exception as e:
        print(f"Direct POST failed: {e}")
    print()
    
    # Step 1: Test compilation
    print("Step 1: Testing compilation...")
    compile_result = bridge.compile_rule(rule_content, "debug_rule")
    print(f"Compilation result: {json.dumps(compile_result, indent=2)}")
    print()
    
    if not compile_result['success']:
        print("❌ Compilation failed!")
        return
    
    # Step 2: Test execution
    print("Step 2: Testing execution...")
    rule_id = compile_result['ruleId']
    execution_result = bridge.execute_compiled_rule(rule_id, test_data)
    print(f"Execution result: {json.dumps(execution_result, indent=2)}")
    print()
    
    # Step 3: Test combined hot method
    print("Step 3: Testing combined hot method...")
    hot_result = bridge.test_rule_hot(rule_content, test_data)
    print(f"Hot result: {json.dumps(hot_result, indent=2)}")
    
    if hot_result['success']:
        print("✅ Hot compilation working!")
    else:
        print("❌ Hot compilation failed!")
        print(f"Errors: {hot_result.get('errors', [])}")

if __name__ == "__main__":
    test_hot_compilation()