#!/usr/bin/env python3
"""
Simplified Code Generation Testing for Rules Engine
Tests code generation for the 4 main rule types using existing data
"""

import requests
import json
import subprocess
import os
import time
from datetime import datetime

def test_code_generation_comprehensive():
    """Test code generation for all rule types"""
    
    print("=" * 60)
    print("COMPREHENSIVE CODE GENERATION TESTING")
    print("=" * 60)
    
    api_base = "http://localhost:5001/api"
    results = []
    
    # Test 1: Create and test a Standard Rule
    print("\n🔧 TESTING STANDARD RULE CODE GENERATION")
    print("-" * 40)
    
    standard_rule = {
        'name': f'test_standard_{int(time.time())}',
        'content': 'rule "Credit Score Check":\nif applicant.creditScore >= 750 then approve_application',
        'description': 'Test standard rule for code generation',
        'process_area_id': 1,  # Use first available process area
        'item_type': 'rule'
    }
    
    try:
        # Create rule
        response = requests.post(f"{api_base}/rules", json=standard_rule)
        if response.status_code == 201:
            rule = response.json()
            rule_id = rule['id']
            print(f"✅ Rule created: {rule['name']} (ID: {rule_id})")
            
            # Test validation
            validation = rule.get('validation', {})
            if validation.get('valid'):
                print("✅ Syntax validation passed")
                
                # Test code generation
                gen_response = requests.post(f"{api_base}/rules/{rule_id}/generate")
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
                    print(f"❌ Code generation API error: {gen_response.status_code}")
                    results.append(("Standard Rule", False, f"API error {gen_response.status_code}"))
            else:
                print(f"❌ Syntax validation failed: {validation.get('error')}")
                results.append(("Standard Rule", False, validation.get('error')))
        else:
            print(f"❌ Rule creation failed: {response.status_code} - {response.text}")
            results.append(("Standard Rule", False, f"Creation failed: {response.status_code}"))
            
    except Exception as e:
        print(f"❌ Standard rule test exception: {str(e)}")
        results.append(("Standard Rule", False, str(e)))
    
    # Test 2: ActionSet
    print("\n🔧 TESTING ACTIONSET CODE GENERATION") 
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
            
            validation = actionset_obj.get('validation', {})
            if validation.get('valid'):
                print("✅ Syntax validation passed")
                
                gen_response = requests.post(f"{api_base}/rules/{actionset_id}/generate")
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
                print(f"❌ Syntax validation failed: {validation.get('error')}")
                results.append(("ActionSet", False, validation.get('error')))
        else:
            print(f"❌ ActionSet creation failed: {response.status_code}")
            results.append(("ActionSet", False, f"Creation failed: {response.status_code}"))
            
    except Exception as e:
        print(f"❌ ActionSet test exception: {str(e)}")
        results.append(("ActionSet", False, str(e)))
    
    # Test 3: Complex Rule
    print("\n🔧 TESTING COMPLEX RULE CODE GENERATION")
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
            
            validation = rule.get('validation', {})
            if validation.get('valid'):
                print("✅ Syntax validation passed")
                
                gen_response = requests.post(f"{api_base}/rules/{rule_id}/generate")
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
                print(f"❌ Syntax validation failed: {validation.get('error')}")
                results.append(("Complex Rule", False, validation.get('error')))
        else:
            print(f"❌ Complex rule creation failed: {response.status_code}")
            results.append(("Complex Rule", False, f"Creation failed: {response.status_code}"))
            
    except Exception as e:
        print(f"❌ Complex rule test exception: {str(e)}")
        results.append(("Complex Rule", False, str(e)))
    
    # Test 4: Edge Case Rule  
    print("\n🔧 TESTING EDGE CASE RULE CODE GENERATION")
    print("-" * 40)
    
    edge_rule = {
        'name': f'test_edge_{int(time.time())}',
        'content': '''rule "Edge Case Handling":
if account.balance is_null then reject_application
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
            
            validation = rule.get('validation', {})
            if validation.get('valid'):
                print("✅ Syntax validation passed")
                
                gen_response = requests.post(f"{api_base}/rules/{rule_id}/generate")
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
                print(f"❌ Syntax validation failed: {validation.get('error')}")
                results.append(("Edge Case", False, validation.get('error')))
        else:
            print(f"❌ Edge case rule creation failed: {response.status_code}")
            results.append(("Edge Case", False, f"Creation failed: {response.status_code}"))
            
    except Exception as e:
        print(f"❌ Edge case test exception: {str(e)}")
        results.append(("Edge Case", False, str(e)))
    
    # Test 5: Compilation Test
    print("\n🔧 TESTING JAVA COMPILATION")
    print("-" * 40)
    
    try:
        compile_cmd = "cd /Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge && mvn compile -q"
        compile_result = subprocess.run(compile_cmd, shell=True, capture_output=True, text=True, timeout=60)
        
        if compile_result.returncode == 0:
            print("✅ Java compilation successful")
            results.append(("Java Compilation", True, "All generated code compiles"))
        else:
            print(f"❌ Java compilation failed:")
            print(f"   STDERR: {compile_result.stderr}")
            results.append(("Java Compilation", False, compile_result.stderr))
            
    except subprocess.TimeoutExpired:
        print("❌ Java compilation timeout")
        results.append(("Java Compilation", False, "Compilation timeout"))
    except Exception as e:
        print(f"❌ Java compilation test failed: {str(e)}")
        results.append(("Java Compilation", False, str(e)))
    
    # Generate Final Report
    print("\n" + "=" * 60)
    print("FINAL TEST REPORT")
    print("=" * 60)
    
    total_tests = len(results)
    passed_tests = len([r for r in results if r[1]])
    failed_tests = total_tests - passed_tests
    
    print(f"📊 SUMMARY:")
    print(f"   Total Tests: {total_tests}")
    print(f"   Passed: {passed_tests}")
    print(f"   Failed: {failed_tests}")
    
    if failed_tests == 0:
        print("\n🎉 ALL CODE GENERATION TESTS PASSED!")
        success_rate = 100.0
    else:
        print(f"\n⚠️  {failed_tests} TEST(S) FAILED:")
        for test_type, success, details in results:
            if not success:
                print(f"   ❌ {test_type}: {details}")
        success_rate = (passed_tests / total_tests) * 100
        
    print(f"\n📈 Success Rate: {success_rate:.1f}%")
    
    # Save results
    report = {
        'timestamp': datetime.now().isoformat(),
        'summary': {
            'total_tests': total_tests,
            'passed': passed_tests,
            'failed': failed_tests,
            'success_rate': success_rate
        },
        'detailed_results': [
            {'test_type': r[0], 'success': r[1], 'details': r[2]} 
            for r in results
        ]
    }
    
    with open('comprehensive_codegen_test_results.json', 'w') as f:
        json.dump(report, f, indent=2)
        
    print(f"\n📄 Detailed report saved to: comprehensive_codegen_test_results.json")
    
    return failed_tests == 0

if __name__ == "__main__":
    success = test_code_generation_comprehensive()
    exit(0 if success else 1)
