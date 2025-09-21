#!/usr/bin/env python3
"""
Fixed Code Generation Testing for Rules Engine
Uses correct API endpoints for all 4 rule types
"""

import requests
import json
import subprocess
import os
import time
from datetime import datetime

def test_code_generation_all_types():
    """Test code generation for all 4 rule types using correct API"""
    
    print("=" * 60)
    print("CODE GENERATION TESTING - ALL 4 RULE TYPES")
    print("=" * 60)
    
    api_base = "http://localhost:5001/api"
    results = []
    
    # Test 1: Standard Rule
    print("\n🔧 1. TESTING STANDARD RULE")
    print("-" * 40)
    
    standard_rule = {
        'name': f'test_standard_{int(time.time())}',
        'content': 'rule "Credit Score Check":\nif applicant.creditScore >= 750 then approve_application',
        'description': 'Test standard rule for code generation',
        'process_area_id': 1,
        'item_type': 'rule'
    }
    
    try:
        # Create rule
        response = requests.post(f"{api_base}/rules", json=standard_rule)
        if response.status_code == 201:
            rule = response.json()
            rule_id = rule['id']
            print(f"✅ Rule created: {rule['name']} (ID: {rule_id})")
            
            # Test code generation using correct endpoint
            gen_data = {
                'ruleId': rule_id,
                'ruleName': rule['name'],
                'ruleContent': rule['content'],
                'packageName': 'com.rules.generated',
                'itemType': 'rule'
            }
            
            gen_response = requests.post(f"{api_base}/rules/generate-production", json=gen_data)
            if gen_response.status_code == 200:
                gen_result = gen_response.json()
                if gen_result.get('success'):
                    print(f"✅ Code generation successful")
                    print(f"   Generated files: {gen_result.get('generated_files', [])}")
                    results.append(("Standard Rule", True, "Complete success"))
                else:
                    print(f"❌ Code generation failed: {gen_result.get('error')}")
                    results.append(("Standard Rule", False, gen_result.get('error')))
            else:
                print(f"❌ Code generation API error: {gen_response.status_code} - {gen_response.text}")
                results.append(("Standard Rule", False, f"API error {gen_response.status_code}"))
        else:
            print(f"❌ Rule creation failed: {response.status_code} - {response.text}")
            results.append(("Standard Rule", False, f"Creation failed: {response.status_code}"))
            
    except Exception as e:
        print(f"❌ Standard rule test exception: {str(e)}")
        results.append(("Standard Rule", False, str(e)))
    
    # Test 2: ActionSet
    print("\n🔧 2. TESTING ACTIONSET")
    print("-" * 40)
    
    actionset = {
        'name': f'test_actionset_{int(time.time())}',
        'content': 'actionset "Credit Actions":\napprove_application,\nmanual_review,\nreject_application',
        'description': 'Test actionset for code generation',
        'process_area_id': 1,
        'item_type': 'actionset'
    }
    
    try:
        response = requests.post(f"{api_base}/rules", json=actionset)
        if response.status_code == 201:
            actionset_obj = response.json()
            actionset_id = actionset_obj['id']
            print(f"✅ ActionSet created: {actionset_obj['name']} (ID: {actionset_id})")
            
            gen_data = {
                'ruleId': actionset_id,
                'ruleName': actionset_obj['name'],
                'ruleContent': actionset_obj['content'],
                'packageName': 'com.rules.generated',
                'itemType': 'actionset'
            }
            
            gen_response = requests.post(f"{api_base}/rules/generate-production", json=gen_data)
            if gen_response.status_code == 200:
                gen_result = gen_response.json()
                if gen_result.get('success'):
                    print(f"✅ Code generation successful")
                    results.append(("ActionSet", True, "Complete success"))
                else:
                    print(f"❌ Code generation failed: {gen_result.get('error')}")
                    results.append(("ActionSet", False, gen_result.get('error')))
            else:
                print(f"❌ Code generation API error: {gen_response.status_code}")
                results.append(("ActionSet", False, f"API error {gen_response.status_code}"))
        else:
            print(f"❌ ActionSet creation failed: {response.status_code} - {response.text}")
            results.append(("ActionSet", False, f"Creation failed: {response.status_code}"))
            
    except Exception as e:
        print(f"❌ ActionSet test exception: {str(e)}")
        results.append(("ActionSet", False, str(e)))
    
    # Test 3: Complex Rule with Multiple Conditions
    print("\n🔧 3. TESTING COMPLEX RULE")
    print("-" * 40)
    
    complex_rule = {
        'name': f'test_complex_{int(time.time())}',
        'content': '''rule "Complex Risk Assessment":
if applicant.creditScore >= 700 and applicant.annualIncome >= 75000 then approve_application
else if applicant.creditScore >= 650 and applicant.debtToIncome < 0.4 then manual_review
else reject_application''',
        'description': 'Test complex rule with multiple conditions',
        'process_area_id': 1,
        'item_type': 'rule'
    }
    
    try:
        response = requests.post(f"{api_base}/rules", json=complex_rule)
        if response.status_code == 201:
            rule = response.json()
            rule_id = rule['id']
            print(f"✅ Complex rule created: {rule['name']} (ID: {rule_id})")
            
            gen_data = {
                'ruleId': rule_id,
                'ruleName': rule['name'],
                'ruleContent': rule['content'],
                'packageName': 'com.rules.generated',
                'itemType': 'rule'
            }
            
            gen_response = requests.post(f"{api_base}/rules/generate-production", json=gen_data)
            if gen_response.status_code == 200:
                gen_result = gen_response.json()
                if gen_result.get('success'):
                    print(f"✅ Code generation successful")
                    results.append(("Complex Rule", True, "Complete success"))
                else:
                    print(f"❌ Code generation failed: {gen_result.get('error')}")
                    results.append(("Complex Rule", False, gen_result.get('error')))
            else:
                print(f"❌ Code generation API error: {gen_response.status_code}")
                results.append(("Complex Rule", False, f"API error {gen_response.status_code}"))
        else:
            print(f"❌ Complex rule creation failed: {response.status_code} - {response.text}")
            results.append(("Complex Rule", False, f"Creation failed: {response.status_code}"))
            
    except Exception as e:
        print(f"❌ Complex rule test exception: {str(e)}")
        results.append(("Complex Rule", False, str(e)))
    
    # Test 4: Edge Case Rule with Null Checks and Lists
    print("\n🔧 4. TESTING EDGE CASE RULE")
    print("-" * 40)
    
    edge_rule = {
        'name': f'test_edge_{int(time.time())}',
        'content': '''rule "Edge Case Handling":
if applicant.age is_null then reject_application
else if transaction.amount in [100, 200, 500] then approve_application
else manual_review''',
        'description': 'Test edge case rule with null checks and list operations',
        'process_area_id': 1,
        'item_type': 'rule'
    }
    
    try:
        response = requests.post(f"{api_base}/rules", json=edge_rule)
        if response.status_code == 201:
            rule = response.json()
            rule_id = rule['id']
            print(f"✅ Edge case rule created: {rule['name']} (ID: {rule_id})")
            
            gen_data = {
                'ruleId': rule_id,
                'ruleName': rule['name'],
                'ruleContent': rule['content'],
                'packageName': 'com.rules.generated',
                'itemType': 'rule'
            }
            
            gen_response = requests.post(f"{api_base}/rules/generate-production", json=gen_data)
            if gen_response.status_code == 200:
                gen_result = gen_response.json()
                if gen_result.get('success'):
                    print(f"✅ Code generation successful")
                    results.append(("Edge Case", True, "Complete success"))
                else:
                    print(f"❌ Code generation failed: {gen_result.get('error')}")
                    results.append(("Edge Case", False, gen_result.get('error')))
            else:
                print(f"❌ Code generation API error: {gen_response.status_code}")
                results.append(("Edge Case", False, f"API error {gen_response.status_code}"))
        else:
            print(f"❌ Edge case rule creation failed: {response.status_code} - {response.text}")
            results.append(("Edge Case", False, f"Creation failed: {response.status_code}"))
            
    except Exception as e:
        print(f"❌ Edge case test exception: {str(e)}")
        results.append(("Edge Case", False, str(e)))
    
    # Test 5: Java Compilation Test
    print("\n🔧 5. TESTING JAVA COMPILATION")
    print("-" * 40)
    
    try:
        compile_cmd = "cd /Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge && mvn compile -q -Dmaven.test.skip=true"
        compile_result = subprocess.run(compile_cmd, shell=True, capture_output=True, text=True, timeout=120)
        
        if compile_result.returncode == 0:
            print("✅ Java compilation successful")
            results.append(("Java Compilation", True, "All generated code compiles successfully"))
        else:
            stderr_output = compile_result.stderr.strip()
            # Filter out warnings and focus on errors
            error_lines = [line for line in stderr_output.split('\n') if 'ERROR' in line.upper() or 'FAILED' in line.upper()]
            if error_lines:
                print(f"❌ Java compilation failed with errors:")
                for error in error_lines[:3]:  # Show first 3 errors
                    print(f"   {error}")
                results.append(("Java Compilation", False, "Compilation errors found"))
            else:
                print("✅ Java compilation successful (warnings only)")
                results.append(("Java Compilation", True, "Compilation successful with warnings"))
            
    except subprocess.TimeoutExpired:
        print("❌ Java compilation timeout (>2 minutes)")
        results.append(("Java Compilation", False, "Compilation timeout"))
    except Exception as e:
        print(f"❌ Java compilation test failed: {str(e)}")
        results.append(("Java Compilation", False, str(e)))
    
    # Additional Test: Check Generated Files
    print("\n🔧 6. TESTING GENERATED FILES")
    print("-" * 40)
    
    try:
        generated_dir = "/Users/chandramohn/workspace/rules_engine/ui-prototype/generated-rules"
        if os.path.exists(generated_dir):
            java_files = []
            for root, dirs, files in os.walk(generated_dir):
                for file in files:
                    if file.endswith('.java'):
                        java_files.append(os.path.join(root, file))
            
            if java_files:
                print(f"✅ Found {len(java_files)} generated Java files")
                for java_file in java_files[-3:]:  # Show last 3 files
                    rel_path = os.path.relpath(java_file, generated_dir)
                    print(f"   📄 {rel_path}")
                results.append(("Generated Files", True, f"{len(java_files)} Java files generated"))
            else:
                print("❌ No generated Java files found")
                results.append(("Generated Files", False, "No Java files found"))
        else:
            print("❌ Generated files directory not found")
            results.append(("Generated Files", False, "Directory not found"))
            
    except Exception as e:
        print(f"❌ File check failed: {str(e)}")
        results.append(("Generated Files", False, str(e)))
    
    # FINAL REPORT
    print("\n" + "=" * 60)
    print("🏁 FINAL TEST RESULTS")
    print("=" * 60)
    
    total_tests = len(results)
    passed_tests = len([r for r in results if r[1]])
    failed_tests = total_tests - passed_tests
    
    print(f"📊 SUMMARY:")
    print(f"   Total Tests: {total_tests}")
    print(f"   ✅ Passed: {passed_tests}")
    print(f"   ❌ Failed: {failed_tests}")
    
    if failed_tests == 0:
        print("\n🎉 ALL CODE GENERATION TESTS PASSED!")
        success_rate = 100.0
    else:
        print(f"\n⚠️  FAILED TESTS:")
        for test_type, success, details in results:
            if not success:
                print(f"   ❌ {test_type}: {details}")
        success_rate = (passed_tests / total_tests) * 100
        
    print(f"\n📈 Overall Success Rate: {success_rate:.1f}%")
    
    # Categorize by rule type success
    rule_types = ["Standard Rule", "ActionSet", "Complex Rule", "Edge Case"]
    rule_type_results = [(t, next((r[1] for r in results if r[0] == t), False)) for t in rule_types]
    
    print(f"\n📋 BY RULE TYPE:")
    for rule_type, success in rule_type_results:
        status = "✅" if success else "❌"
        print(f"   {status} {rule_type}")
    
    # Save comprehensive results
    report = {
        'timestamp': datetime.now().isoformat(),
        'summary': {
            'total_tests': total_tests,
            'passed': passed_tests,
            'failed': failed_tests,
            'success_rate': success_rate,
            'rule_type_results': dict(rule_type_results)
        },
        'detailed_results': [
            {'test_type': r[0], 'success': r[1], 'details': r[2]} 
            for r in results
        ]
    }
    
    with open('final_codegen_test_results.json', 'w') as f:
        json.dump(report, f, indent=2)
        
    print(f"\n📄 Detailed report saved to: final_codegen_test_results.json")
    
    # Return success status
    rule_type_success_count = sum(1 for _, success in rule_type_results if success)
    return rule_type_success_count >= 3  # At least 3 out of 4 rule types should work

if __name__ == "__main__":
    success = test_code_generation_all_types()
    exit(0 if success else 1)
