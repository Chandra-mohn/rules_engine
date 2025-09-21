#!/usr/bin/env python3
"""
Code Generation Testing Script for Rules Engine
Tests all 4 rule types: Standard Rules, ActionSets, Complex Rules, Edge Cases
"""

import requests
import json
import time
import subprocess
import os
from datetime import datetime

class CodeGenerationTester:
    def __init__(self, api_base="http://localhost:5001/api"):
        self.api_base = api_base
        self.test_results = []
        self.process_area_id = None
        
    def log_result(self, test_type, rule_name, success, details):
        """Log test result"""
        result = {
            'timestamp': datetime.now().isoformat(),
            'test_type': test_type,
            'rule_name': rule_name,
            'success': success,
            'details': details
        }
        self.test_results.append(result)
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status} {test_type}: {rule_name}")
        if not success:
            print(f"   Details: {details}")
            
    def setup_test_environment(self):
        """Get process area for test rules"""
        try:
            response = requests.get(f"{self.api_base}/clients")
            clients = response.json()
            
            for client in clients:
                if client['code'] == 'DEMO':
                    # Get process groups for this client
                    groups_response = requests.get(f"{self.api_base}/clients/{client['id']}/process-groups")
                    process_groups = groups_response.json()
                    
                    for group in process_groups:
                        if group['code'] == 'CC_STD':
                            # Get process areas
                            areas_response = requests.get(f"{self.api_base}/process-groups/{group['id']}/process-areas")
                            process_areas = areas_response.json()
                            
                            if process_areas:
                                self.process_area_id = process_areas[0]['id']
                                print(f"Using process area: {process_areas[0]['name']} (ID: {self.process_area_id})")
                                return True
            
            print("ERROR: Could not find suitable process area for testing")
            return False
            
        except Exception as e:
            print(f"ERROR setting up test environment: {str(e)}")
            return False
    
    def test_rule_creation_and_generation(self, rule_type, rule_content, rule_name):
        """Test rule creation, validation, and code generation"""
        try:
            # Step 1: Create rule via API
            rule_data = {
                'name': rule_name,
                'content': rule_content,
                'description': f'Test {rule_type} for code generation testing',
                'process_area_id': self.process_area_id,
                'item_type': 'actionset' if rule_type == 'ActionSet' else 'rule'
            }
            
            response = requests.post(f"{self.api_base}/rules", json=rule_data)
            
            if response.status_code != 201:
                self.log_result(rule_type, rule_name, False, f"Rule creation failed: {response.status_code} - {response.text}")
                return False
                
            created_rule = response.json()
            rule_id = created_rule['id']
            
            # Step 2: Validate syntax
            validation = created_rule.get('validation', {})
            if not validation.get('valid', False):
                error_msg = validation.get('error', 'Unknown validation error')
                self.log_result(rule_type, rule_name, False, f"Syntax validation failed: {error_msg}")
                return False
                
            # Step 3: Test code generation
            gen_response = requests.post(f"{self.api_base}/rules/{rule_id}/generate")
            
            if gen_response.status_code != 200:
                self.log_result(rule_type, rule_name, False, f"Code generation failed: {gen_response.status_code} - {gen_response.text}")
                return False
                
            gen_result = gen_response.json()
            if not gen_result.get('success', False):
                error_msg = gen_result.get('error', 'Unknown generation error')
                self.log_result(rule_type, rule_name, False, f"Code generation error: {error_msg}")
                return False
                
            # Step 4: Verify generated file exists
            generated_files = gen_result.get('generated_files', [])
            if not generated_files:
                self.log_result(rule_type, rule_name, False, "No generated files reported")
                return False
                
            # Check if generated file actually exists
            java_file_path = generated_files[0]
            full_path = f"/Users/chandramohn/workspace/rules_engine/ui-prototype/{java_file_path}"
            
            if not os.path.exists(full_path):
                self.log_result(rule_type, rule_name, False, f"Generated file does not exist: {full_path}")
                return False
                
            # Step 5: Test compilation
            try:
                compile_cmd = f"cd /Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge && mvn compile -q"
                compile_result = subprocess.run(compile_cmd, shell=True, capture_output=True, text=True)
                
                if compile_result.returncode != 0:
                    self.log_result(rule_type, rule_name, False, f"Java compilation failed: {compile_result.stderr}")
                    return False
                    
            except Exception as compile_error:
                self.log_result(rule_type, rule_name, False, f"Compilation test error: {str(compile_error)}")
                return False
                
            # Step 6: Test execution if possible
            try:
                with open('/Users/chandramohn/workspace/rules_engine/ui-prototype/tests/data/test-data.json', 'r') as f:
                    test_data = json.load(f)
                    
                exec_response = requests.post(f"{self.api_base}/rules/{rule_id}/execute", json=test_data)
                
                if exec_response.status_code == 200:
                    exec_result = exec_response.json()
                    if exec_result.get('success'):
                        execution_details = f"Execution successful: {exec_result.get('result', 'N/A')}"
                    else:
                        execution_details = f"Execution completed with issues: {exec_result.get('error', 'N/A')}"
                else:
                    execution_details = f"Execution API call failed: {exec_response.status_code}"
                    
            except Exception as exec_error:
                execution_details = f"Execution test skipped: {str(exec_error)}"
                
            self.log_result(rule_type, rule_name, True, f"Code generated successfully. File: {java_file_path}. {execution_details}")
            return True
            
        except Exception as e:
            self.log_result(rule_type, rule_name, False, f"Test failed with exception: {str(e)}")
            return False
    
    def run_all_tests(self):
        """Run comprehensive code generation tests"""
        print("=" * 60)
        print("CODE GENERATION TESTING - ALL RULE TYPES")
        print("=" * 60)
        
        if not self.setup_test_environment():
            return False
            
        # Load test rules
        with open('test_rules.json', 'r') as f:
            test_rules = json.load(f)
            
        # Test 1: Standard Rule
        print("\n1. Testing Standard Rule Code Generation...")
        self.test_rule_creation_and_generation(
            "Standard Rule", 
            test_rules['standard_rule'], 
            f"test_standard_rule_{int(time.time())}"
        )
        
        # Test 2: ActionSet
        print("\n2. Testing ActionSet Code Generation...")
        self.test_rule_creation_and_generation(
            "ActionSet", 
            test_rules['actionset'], 
            f"test_actionset_{int(time.time())}"
        )
        
        # Test 3: Complex Rule
        print("\n3. Testing Complex Rule Code Generation...")
        self.test_rule_creation_and_generation(
            "Complex Rule", 
            test_rules['complex_rule'], 
            f"test_complex_rule_{int(time.time())}"
        )
        
        # Test 4: Edge Case Rule
        print("\n4. Testing Edge Case Rule Code Generation...")
        self.test_rule_creation_and_generation(
            "Edge Case", 
            test_rules['edge_case'], 
            f"test_edge_case_{int(time.time())}"
        )
        
        # Generate summary
        print("\n" + "=" * 60)
        print("CODE GENERATION TEST SUMMARY")
        print("=" * 60)
        
        total_tests = len(self.test_results)
        passed_tests = len([r for r in self.test_results if r['success']])
        failed_tests = total_tests - passed_tests
        
        print(f"Total Tests: {total_tests}")
        print(f"Passed: {passed_tests}")
        print(f"Failed: {failed_tests}")
        
        if failed_tests == 0:
            print("\n🎉 ALL CODE GENERATION TESTS PASSED!")
            success_rate = 100.0
        else:
            print(f"\n⚠️  {failed_tests} TEST(S) FAILED")
            success_rate = (passed_tests / total_tests) * 100
            
        print(f"Success Rate: {success_rate:.1f}%")
        
        # Save detailed results
        with open('code_generation_test_results.json', 'w') as f:
            json.dump({
                'timestamp': datetime.now().isoformat(),
                'summary': {
                    'total_tests': total_tests,
                    'passed': passed_tests,
                    'failed': failed_tests,
                    'success_rate': success_rate
                },
                'detailed_results': self.test_results
            }, f, indent=2)
            
        return failed_tests == 0

if __name__ == "__main__":
    tester = CodeGenerationTester()
    success = tester.run_all_tests()
    exit(0 if success else 1)
