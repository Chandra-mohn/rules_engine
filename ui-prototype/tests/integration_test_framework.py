#!/usr/bin/env python3
"""
Integration Testing Framework
Comprehensive testing framework for end-to-end workflows and system integration.

This framework provides:
- Complete workflow testing (frontend -> backend -> java bridge)
- API integration testing
- Database integration testing
- Service communication testing
- Error scenario testing
- Performance integration testing
"""

import requests
import json
import time
import subprocess
import os
import sqlite3
from datetime import datetime
from typing import Dict, List, Tuple, Optional, Any
from pathlib import Path
from dataclasses import dataclass
import concurrent.futures
import threading

@dataclass
class IntegrationTestResult:
    """Represents the result of an integration test"""
    test_name: str
    test_category: str
    status: str  # PASS, FAIL, SKIP
    duration_ms: float
    message: str
    details: Dict[str, Any]
    timestamp: str

class IntegrationTestFramework:
    """Comprehensive integration testing framework"""

    def __init__(self, config: Optional[Dict] = None):
        self.config = config or self._load_default_config()
        self.results: List[IntegrationTestResult] = []
        self.project_root = Path("/Users/chandramohn/workspace/rules_engine/ui-prototype")
        self.test_data = self._load_test_data()

    def _load_default_config(self) -> Dict:
        """Load default test configuration"""
        return {
            'api_base_url': 'http://localhost:5001/api',
            'frontend_url': 'http://localhost:3000',
            'java_bridge_path': '/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge',
            'backend_path': '/Users/chandramohn/workspace/rules_engine/ui-prototype/backend',
            'database_path': '/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/database/rules.db',
            'timeout': 30,
            'retry_count': 3,
            'parallel_tests': 4,
            'performance_thresholds': {
                'api_response_time': 2000,  # ms
                'rule_compilation_time': 5000,  # ms
                'rule_execution_time': 1000,  # ms
                'end_to_end_time': 10000  # ms
            }
        }

    def _load_test_data(self) -> Dict:
        """Load test data for integration tests"""
        timestamp = int(time.time())
        return {
            'valid_rules': [
                {
                    'name': f'creditScoreTest{timestamp}',  # Unique name matching content
                    'description': 'Integration test for credit score validation',
                    'content': f'rule creditScoreTest{timestamp}:\n    if applicant.creditScore >= 700 then approveApplication\n    if applicant.creditScore < 600 then rejectApplication',
                    'process_area_id': 1,
                    'status': 'DRAFT',
                },
                {
                    'name': f'ageTest{timestamp}',  # Unique name matching content
                    'description': 'Integration test for age verification',
                    'content': f'rule ageTest{timestamp}:\n    if applicant.age >= 18 then approveApplication\n    else rejectApplication',
                    'process_area_id': 1,
                    'status': 'DRAFT',
                }
            ],
            'valid_actionsets': [
                {
                    'name': f'integration_test_approval_workflow_{int(time.time())}',
                    'description': 'Integration test for approval workflow',
                    'content': 'ActionSet approvalWorkflow:\n    if applicant.creditScore >= 700 then approveApplication, sendWelcomeEmail\n    if applicant.creditScore < 600 then rejectApplication, sendRejectionLetter\n    else conditionalApproval, requestDocumentation',
                    'process_area_id': 1,
                    'status': 'DRAFT',
                    'item_type': 'actionset',
                }
            ],
            'invalid_rules': [
                {
                    'name': 'integration_test_invalid_syntax',
                    'description': 'Integration test for invalid syntax handling',
                    'content': 'rule invalidSyntax:\n    if applicant.creditScore >= 700 then\n    // Missing action',
                    'process_area_id': 1,
                    'status': 'DRAFT'
                }
            ],
            'test_applicant_data': [
                {
                    'applicant': {
                        'creditScore': 720,
                        'age': 30,
                        'annualIncome': 60000,
                        'employmentStatus': 'employed',
                        'employmentYears': 5
                    },
                    'expected_result': 'approveApplication'
                },
                {
                    'applicant': {
                        'creditScore': 580,
                        'age': 25,
                        'annualIncome': 35000,
                        'employmentStatus': 'employed',
                        'employmentYears': 2
                    },
                    'expected_result': 'rejectApplication'
                },
                {
                    'applicant': {
                        'creditScore': 650,
                        'age': 17,
                        'annualIncome': 25000,
                        'employmentStatus': 'student',
                        'employmentYears': 0
                    },
                    'expected_result': 'rejectApplication'  # Age restriction
                }
            ]
        }

    def log_result(self, test_name: str, test_category: str, status: str,
                   duration_ms: float, message: str, details: Optional[Dict] = None):
        """Log an integration test result"""
        result = IntegrationTestResult(
            test_name=test_name,
            test_category=test_category,
            status=status,
            duration_ms=duration_ms,
            message=message,
            details=details or {},
            timestamp=datetime.now().isoformat()
        )
        self.results.append(result)

        # Print result
        icon = {"PASS": "✅", "FAIL": "❌", "SKIP": "⏭️"}
        print(f"{icon.get(status, '📋')} {test_name}: {message} ({duration_ms:.0f}ms)")

    def test_api_health_and_connectivity(self) -> bool:
        """Test basic API health and connectivity"""
        start_time = time.time()

        try:
            response = requests.get(
                f"{self.config['api_base_url']}/health",
                timeout=self.config['timeout']
            )
            duration = (time.time() - start_time) * 1000

            if response.status_code == 200:
                data = response.json()
                if data.get('status') == 'healthy':
                    self.log_result(
                        "API Health Check", "connectivity", "PASS",
                        duration, "API is healthy and responsive",
                        {'response_data': data}
                    )
                    return True
                else:
                    self.log_result(
                        "API Health Check", "connectivity", "FAIL",
                        duration, f"API returned unhealthy status: {data.get('status')}",
                        {'response_data': data}
                    )
                    return False
            else:
                self.log_result(
                    "API Health Check", "connectivity", "FAIL",
                    duration, f"API returned status {response.status_code}",
                    {'status_code': response.status_code}
                )
                return False

        except Exception as e:
            duration = (time.time() - start_time) * 1000
            self.log_result(
                "API Health Check", "connectivity", "FAIL",
                duration, f"API connectivity failed: {str(e)}",
                {'error': str(e)}
            )
            return False

    def test_database_connectivity(self) -> bool:
        """Test database connectivity and basic operations"""
        start_time = time.time()

        try:
            conn = sqlite3.connect(self.config['database_path'])
            cursor = conn.cursor()

            # Test basic read operation
            cursor.execute("SELECT COUNT(*) FROM rules")
            rule_count = cursor.fetchone()[0]

            # Test basic write operation (create and delete a test record)
            test_name = f"integration_test_{int(time.time())}"
            cursor.execute("""
                INSERT INTO rules (name, content, process_area_id, status)
                VALUES (?, ?, ?, ?)
            """, (test_name, "rule test: if true then pass", 1, "DRAFT"))

            test_id = cursor.lastrowid

            # Verify the record was created
            cursor.execute("SELECT name FROM rules WHERE id = ?", (test_id,))
            result = cursor.fetchone()

            # Clean up test record
            cursor.execute("DELETE FROM rules WHERE id = ?", (test_id,))
            conn.commit()
            conn.close()

            duration = (time.time() - start_time) * 1000

            if result and result[0] == test_name:
                self.log_result(
                    "Database Connectivity", "connectivity", "PASS",
                    duration, f"Database operations successful ({rule_count} rules)",
                    {'rule_count': rule_count, 'test_record_created': True}
                )
                return True
            else:
                self.log_result(
                    "Database Connectivity", "connectivity", "FAIL",
                    duration, "Database write/read test failed",
                    {'rule_count': rule_count, 'test_record_created': False}
                )
                return False

        except Exception as e:
            duration = (time.time() - start_time) * 1000
            self.log_result(
                "Database Connectivity", "connectivity", "FAIL",
                duration, f"Database connectivity failed: {str(e)}",
                {'error': str(e)}
            )
            return False

    def test_java_bridge_compilation(self) -> bool:
        """Test Java bridge compilation and basic functionality"""
        start_time = time.time()

        try:
            # Test Maven compilation
            result = subprocess.run(
                ['mvn', 'compile', '-q'],
                cwd=self.config['java_bridge_path'],
                capture_output=True,
                text=True,
                timeout=60
            )

            duration = (time.time() - start_time) * 1000

            if result.returncode == 0:
                self.log_result(
                    "Java Bridge Compilation", "connectivity", "PASS",
                    duration, "Java bridge compiles successfully",
                    {'stdout': result.stdout[:500], 'stderr': result.stderr[:500]}
                )
                return True
            else:
                self.log_result(
                    "Java Bridge Compilation", "connectivity", "FAIL",
                    duration, f"Java compilation failed (exit code: {result.returncode})",
                    {'stdout': result.stdout[:500], 'stderr': result.stderr[:500]}
                )
                return False

        except subprocess.TimeoutExpired:
            duration = (time.time() - start_time) * 1000
            self.log_result(
                "Java Bridge Compilation", "connectivity", "FAIL",
                duration, "Java compilation timed out",
                {'timeout_seconds': 60}
            )
            return False
        except Exception as e:
            duration = (time.time() - start_time) * 1000
            self.log_result(
                "Java Bridge Compilation", "connectivity", "FAIL",
                duration, f"Java compilation error: {str(e)}",
                {'error': str(e)}
            )
            return False

    def test_rule_crud_workflow(self) -> bool:
        """Test complete Create, Read, Update, Delete workflow for rules"""
        all_tests_passed = True

        for rule_data in self.test_data['valid_rules']:
            start_time = time.time()
            test_name = f"Rule CRUD: {rule_data['name']}"

            try:
                # CREATE
                create_response = requests.post(
                    f"{self.config['api_base_url']}/rules",
                    json=rule_data,
                    timeout=self.config['timeout']
                )

                if create_response.status_code != 201:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "workflow", "FAIL",
                        duration, f"Rule creation failed: {create_response.status_code}",
                        {'response': create_response.text[:500]}
                    )
                    all_tests_passed = False
                    continue

                rule_id = create_response.json()['id']

                # READ
                read_response = requests.get(
                    f"{self.config['api_base_url']}/rules/{rule_id}",
                    timeout=self.config['timeout']
                )

                if read_response.status_code != 200:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "workflow", "FAIL",
                        duration, f"Rule read failed: {read_response.status_code}",
                        {'rule_id': rule_id}
                    )
                    all_tests_passed = False
                    continue

                retrieved_rule = read_response.json()

                # More detailed comparison to identify actual mismatch
                name_match = retrieved_rule.get('name') == rule_data['name']
                content_match = retrieved_rule.get('content') == rule_data['content']
                status_match = retrieved_rule.get('status') == rule_data['status']

                if not name_match:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "workflow", "FAIL",
                        duration, "Retrieved rule name doesn't match created rule",
                        {
                            'expected_name': rule_data['name'],
                            'actual_name': retrieved_rule.get('name'),
                            'rule_id': rule_id
                        }
                    )
                    all_tests_passed = False
                    continue

                if not content_match:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "workflow", "FAIL",
                        duration, "Retrieved rule content doesn't match created rule",
                        {
                            'expected_content_length': len(rule_data['content']),
                            'actual_content_length': len(retrieved_rule.get('content', '')),
                            'rule_id': rule_id
                        }
                    )
                    all_tests_passed = False
                    continue

                # UPDATE
                update_data = {'description': f"Updated description for {rule_data['name']}"}
                update_response = requests.put(
                    f"{self.config['api_base_url']}/rules/{rule_id}",
                    json=update_data,
                    timeout=self.config['timeout']
                )

                if update_response.status_code != 200:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "workflow", "FAIL",
                        duration, f"Rule update failed: {update_response.status_code}",
                        {'rule_id': rule_id}
                    )
                    all_tests_passed = False
                    # Continue to cleanup

                # DELETE
                delete_response = requests.delete(
                    f"{self.config['api_base_url']}/rules/{rule_id}",
                    timeout=self.config['timeout']
                )

                duration = (time.time() - start_time) * 1000

                if delete_response.status_code in [200, 204]:
                    self.log_result(
                        test_name, "workflow", "PASS",
                        duration, "Complete CRUD workflow successful",
                        {'rule_id': rule_id, 'operations': ['create', 'read', 'update', 'delete']}
                    )
                else:
                    self.log_result(
                        test_name, "workflow", "FAIL",
                        duration, f"Rule deletion failed: {delete_response.status_code}",
                        {'rule_id': rule_id}
                    )
                    all_tests_passed = False

            except Exception as e:
                duration = (time.time() - start_time) * 1000
                self.log_result(
                    test_name, "workflow", "FAIL",
                    duration, f"CRUD workflow error: {str(e)}",
                    {'error': str(e)}
                )
                all_tests_passed = False

        return all_tests_passed

    def test_rule_compilation_integration(self) -> bool:
        """Test rule compilation integration with Java bridge"""
        all_tests_passed = True

        for rule_data in self.test_data['valid_rules']:
            start_time = time.time()
            test_name = f"Rule Compilation: {rule_data['name']}"

            try:
                # Create rule
                create_response = requests.post(
                    f"{self.config['api_base_url']}/rules",
                    json=rule_data,
                    timeout=self.config['timeout']
                )

                if create_response.status_code != 201:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "compilation", "FAIL",
                        duration, f"Rule creation failed for compilation test",
                        {'status_code': create_response.status_code}
                    )
                    all_tests_passed = False
                    continue

                rule_id = create_response.json()['id']

                # Test compilation
                compile_start = time.time()
                compile_response = requests.post(
                    f"{self.config['api_base_url']}/rules/{rule_id}/compile",
                    timeout=self.config['timeout']
                )
                compile_duration = (time.time() - compile_start) * 1000

                if compile_response.status_code == 200:
                    compile_result = compile_response.json()
                    is_valid = compile_result.get('valid', False)

                    duration = (time.time() - start_time) * 1000

                    if is_valid:
                        # Check performance threshold
                        if compile_duration <= self.config['performance_thresholds']['rule_compilation_time']:
                            self.log_result(
                                test_name, "compilation", "PASS",
                                duration, f"Rule compiled successfully ({compile_duration:.0f}ms)",
                                {
                                    'rule_id': rule_id,
                                    'compile_duration_ms': compile_duration,
                                    'validation_result': compile_result
                                }
                            )
                        else:
                            self.log_result(
                                test_name, "compilation", "FAIL",
                                duration, f"Rule compilation too slow ({compile_duration:.0f}ms)",
                                {
                                    'rule_id': rule_id,
                                    'compile_duration_ms': compile_duration,
                                    'threshold_ms': self.config['performance_thresholds']['rule_compilation_time']
                                }
                            )
                            all_tests_passed = False
                    else:
                        self.log_result(
                            test_name, "compilation", "FAIL",
                            duration, "Rule compilation failed - invalid syntax",
                            {
                                'rule_id': rule_id,
                                'validation_result': compile_result
                            }
                        )
                        all_tests_passed = False
                else:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "compilation", "FAIL",
                        duration, f"Compilation endpoint failed: {compile_response.status_code}",
                        {'rule_id': rule_id, 'response': compile_response.text[:500]}
                    )
                    all_tests_passed = False

                # Cleanup
                requests.delete(f"{self.config['api_base_url']}/rules/{rule_id}")

            except Exception as e:
                duration = (time.time() - start_time) * 1000
                self.log_result(
                    test_name, "compilation", "FAIL",
                    duration, f"Compilation integration error: {str(e)}",
                    {'error': str(e)}
                )
                all_tests_passed = False

        return all_tests_passed

    def test_rule_execution_integration(self) -> bool:
        """Test complete rule execution workflow"""
        all_tests_passed = True

        for i, rule_data in enumerate(self.test_data['valid_rules']):
            start_time = time.time()
            test_name = f"Rule Execution: {rule_data['name']}"

            try:
                # Create and compile rule
                create_response = requests.post(
                    f"{self.config['api_base_url']}/rules",
                    json=rule_data,
                    timeout=self.config['timeout']
                )

                if create_response.status_code != 201:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "execution", "FAIL",
                        duration, "Rule creation failed for execution test",
                        {'status_code': create_response.status_code}
                    )
                    all_tests_passed = False
                    continue

                rule_id = create_response.json()['id']

                # Compile rule
                compile_response = requests.post(
                    f"{self.config['api_base_url']}/rules/{rule_id}/compile",
                    timeout=self.config['timeout']
                )

                if compile_response.status_code != 200 or not compile_response.json().get('valid'):
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "execution", "FAIL",
                        duration, "Rule compilation failed - cannot test execution",
                        {'rule_id': rule_id}
                    )
                    requests.delete(f"{self.config['api_base_url']}/rules/{rule_id}")
                    all_tests_passed = False
                    continue

                # Test execution with different test data
                test_applicant = self.test_data['test_applicant_data'][i % len(self.test_data['test_applicant_data'])]

                execute_start = time.time()
                execute_response = requests.post(
                    f"{self.config['api_base_url']}/rules/{rule_id}/execute",
                    json=test_applicant,
                    timeout=self.config['timeout']
                )
                execute_duration = (time.time() - execute_start) * 1000

                duration = (time.time() - start_time) * 1000

                if execute_response.status_code == 200:
                    execute_result = execute_response.json()

                    # Check performance threshold
                    if execute_duration <= self.config['performance_thresholds']['rule_execution_time']:
                        self.log_result(
                            test_name, "execution", "PASS",
                            duration, f"Rule executed successfully ({execute_duration:.0f}ms)",
                            {
                                'rule_id': rule_id,
                                'execute_duration_ms': execute_duration,
                                'execution_result': execute_result,
                                'test_data': test_applicant
                            }
                        )
                    else:
                        self.log_result(
                            test_name, "execution", "FAIL",
                            duration, f"Rule execution too slow ({execute_duration:.0f}ms)",
                            {
                                'rule_id': rule_id,
                                'execute_duration_ms': execute_duration,
                                'threshold_ms': self.config['performance_thresholds']['rule_execution_time']
                            }
                        )
                        all_tests_passed = False
                else:
                    self.log_result(
                        test_name, "execution", "FAIL",
                        duration, f"Rule execution failed: {execute_response.status_code}",
                        {
                            'rule_id': rule_id,
                            'response': execute_response.text[:500],
                            'test_data': test_applicant
                        }
                    )
                    all_tests_passed = False

                # Cleanup
                requests.delete(f"{self.config['api_base_url']}/rules/{rule_id}")

            except Exception as e:
                duration = (time.time() - start_time) * 1000
                self.log_result(
                    test_name, "execution", "FAIL",
                    duration, f"Execution integration error: {str(e)}",
                    {'error': str(e)}
                )
                all_tests_passed = False

        return all_tests_passed

    def test_actionset_integration(self) -> bool:
        """Test ActionSet creation, compilation, and execution"""
        all_tests_passed = True

        for actionset_data in self.test_data['valid_actionsets']:
            start_time = time.time()
            test_name = f"ActionSet Integration: {actionset_data['name']}"

            try:
                # Create ActionSet
                create_response = requests.post(
                    f"{self.config['api_base_url']}/rules",
                    json=actionset_data,
                    timeout=self.config['timeout']
                )

                if create_response.status_code != 201:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "actionset", "FAIL",
                        duration, f"ActionSet creation failed: {create_response.status_code}",
                        {'response': create_response.text[:500]}
                    )
                    all_tests_passed = False
                    continue

                actionset_id = create_response.json()['id']

                # Verify item_type is set correctly
                get_response = requests.get(
                    f"{self.config['api_base_url']}/rules/{actionset_id}",
                    timeout=self.config['timeout']
                )

                if get_response.status_code == 200:
                    actionset = get_response.json()
                    if actionset.get('item_type') != 'actionset':
                        duration = (time.time() - start_time) * 1000
                        self.log_result(
                            test_name, "actionset", "FAIL",
                            duration, f"ActionSet item_type incorrect: {actionset.get('item_type')}",
                            {'expected': 'actionset', 'actual': actionset.get('item_type')}
                        )
                        all_tests_passed = False
                        requests.delete(f"{self.config['api_base_url']}/rules/{actionset_id}")
                        continue

                # Test compilation
                compile_response = requests.post(
                    f"{self.config['api_base_url']}/rules/{actionset_id}/compile",
                    timeout=self.config['timeout']
                )

                if compile_response.status_code == 200:
                    compile_result = compile_response.json()
                    if compile_result.get('valid'):
                        # Test execution
                        test_data = self.test_data['test_applicant_data'][0]
                        execute_response = requests.post(
                            f"{self.config['api_base_url']}/rules/{actionset_id}/execute",
                            json=test_data,
                            timeout=self.config['timeout']
                        )

                        duration = (time.time() - start_time) * 1000

                        if execute_response.status_code == 200:
                            self.log_result(
                                test_name, "actionset", "PASS",
                                duration, "ActionSet integration workflow successful",
                                {
                                    'actionset_id': actionset_id,
                                    'compilation_result': compile_result,
                                    'execution_result': execute_response.json()
                                }
                            )
                        else:
                            self.log_result(
                                test_name, "actionset", "FAIL",
                                duration, f"ActionSet execution failed: {execute_response.status_code}",
                                {'actionset_id': actionset_id}
                            )
                            all_tests_passed = False
                    else:
                        duration = (time.time() - start_time) * 1000
                        self.log_result(
                            test_name, "actionset", "FAIL",
                            duration, "ActionSet compilation failed",
                            {
                                'actionset_id': actionset_id,
                                'compilation_result': compile_result
                            }
                        )
                        all_tests_passed = False
                else:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "actionset", "FAIL",
                        duration, f"ActionSet compilation endpoint failed: {compile_response.status_code}",
                        {'actionset_id': actionset_id}
                    )
                    all_tests_passed = False

                # Cleanup
                requests.delete(f"{self.config['api_base_url']}/rules/{actionset_id}")

            except Exception as e:
                duration = (time.time() - start_time) * 1000
                self.log_result(
                    test_name, "actionset", "FAIL",
                    duration, f"ActionSet integration error: {str(e)}",
                    {'error': str(e)}
                )
                all_tests_passed = False

        return all_tests_passed

    def test_error_handling_integration(self) -> bool:
        """Test error handling across the entire system"""
        all_tests_passed = True

        for invalid_rule in self.test_data['invalid_rules']:
            start_time = time.time()
            test_name = f"Error Handling: {invalid_rule['name']}"

            try:
                # Create invalid rule
                create_response = requests.post(
                    f"{self.config['api_base_url']}/rules",
                    json=invalid_rule,
                    timeout=self.config['timeout']
                )

                if create_response.status_code != 201:
                    duration = (time.time() - start_time) * 1000
                    self.log_result(
                        test_name, "error_handling", "FAIL",
                        duration, "Invalid rule creation failed (should succeed for testing)",
                        {'status_code': create_response.status_code}
                    )
                    all_tests_passed = False
                    continue

                rule_id = create_response.json()['id']

                # Test compilation - should fail gracefully
                compile_response = requests.post(
                    f"{self.config['api_base_url']}/rules/{rule_id}/compile",
                    timeout=self.config['timeout']
                )

                duration = (time.time() - start_time) * 1000

                if compile_response.status_code == 200:
                    compile_result = compile_response.json()
                    # Should return valid=False with error details
                    if not compile_result.get('valid') and compile_result.get('validation', {}).get('errors'):
                        self.log_result(
                            test_name, "error_handling", "PASS",
                            duration, "Error handling works correctly - compilation failed gracefully",
                            {
                                'rule_id': rule_id,
                                'compilation_result': compile_result
                            }
                        )
                    else:
                        self.log_result(
                            test_name, "error_handling", "FAIL",
                            duration, "Invalid rule compiled successfully (should fail)",
                            {
                                'rule_id': rule_id,
                                'compilation_result': compile_result
                            }
                        )
                        all_tests_passed = False
                else:
                    self.log_result(
                        test_name, "error_handling", "FAIL",
                        duration, f"Compilation endpoint error: {compile_response.status_code}",
                        {'rule_id': rule_id}
                    )
                    all_tests_passed = False

                # Cleanup
                requests.delete(f"{self.config['api_base_url']}/rules/{rule_id}")

            except Exception as e:
                duration = (time.time() - start_time) * 1000
                self.log_result(
                    test_name, "error_handling", "FAIL",
                    duration, f"Error handling test error: {str(e)}",
                    {'error': str(e)}
                )
                all_tests_passed = False

        return all_tests_passed

    def test_concurrent_operations(self) -> bool:
        """Test system behavior under concurrent load"""
        start_time = time.time()
        test_name = "Concurrent Operations"

        try:
            def create_and_delete_rule(thread_id: int) -> bool:
                try:
                    rule_data = {
                        'name': f'concurrent_test_rule_{thread_id}_{int(time.time())}',
                        'description': f'Concurrent test rule for thread {thread_id}',
                        'content': f'rule concurrentTest{thread_id}:\n    if applicant.age >= 18 then approveApplication',
                        'process_area_id': 1,
                        'status': 'DRAFT'
                    }

                    # Create
                    create_response = requests.post(
                        f"{self.config['api_base_url']}/rules",
                        json=rule_data,
                        timeout=10
                    )

                    if create_response.status_code != 201:
                        return False

                    rule_id = create_response.json()['id']

                    # Read
                    read_response = requests.get(
                        f"{self.config['api_base_url']}/rules/{rule_id}",
                        timeout=10
                    )

                    if read_response.status_code != 200:
                        return False

                    # Delete
                    delete_response = requests.delete(
                        f"{self.config['api_base_url']}/rules/{rule_id}",
                        timeout=10
                    )

                    return delete_response.status_code in [200, 204]

                except Exception:
                    return False

            # Run concurrent operations
            num_threads = self.config['parallel_tests']
            with concurrent.futures.ThreadPoolExecutor(max_workers=num_threads) as executor:
                futures = [executor.submit(create_and_delete_rule, i) for i in range(num_threads)]
                results = [future.result() for future in concurrent.futures.as_completed(futures)]

            duration = (time.time() - start_time) * 1000
            success_count = sum(results)

            if success_count == num_threads:
                self.log_result(
                    test_name, "concurrency", "PASS",
                    duration, f"All {num_threads} concurrent operations successful",
                    {'successful_operations': success_count, 'total_operations': num_threads}
                )
                return True
            else:
                self.log_result(
                    test_name, "concurrency", "FAIL",
                    duration, f"Only {success_count}/{num_threads} concurrent operations successful",
                    {'successful_operations': success_count, 'total_operations': num_threads}
                )
                return False

        except Exception as e:
            duration = (time.time() - start_time) * 1000
            self.log_result(
                test_name, "concurrency", "FAIL",
                duration, f"Concurrent operations test error: {str(e)}",
                {'error': str(e)}
            )
            return False

    def test_end_to_end_performance(self) -> bool:
        """Test complete end-to-end performance"""
        start_time = time.time()
        test_name = "End-to-End Performance"

        try:
            rule_data = self.test_data['valid_rules'][0]
            test_data = self.test_data['test_applicant_data'][0]

            # Create rule
            create_response = requests.post(
                f"{self.config['api_base_url']}/rules",
                json=rule_data,
                timeout=self.config['timeout']
            )

            if create_response.status_code != 201:
                duration = (time.time() - start_time) * 1000
                self.log_result(
                    test_name, "performance", "FAIL",
                    duration, "Rule creation failed in performance test",
                    {'status_code': create_response.status_code}
                )
                return False

            rule_id = create_response.json()['id']

            # Compile rule
            compile_response = requests.post(
                f"{self.config['api_base_url']}/rules/{rule_id}/compile",
                timeout=self.config['timeout']
            )

            if compile_response.status_code != 200 or not compile_response.json().get('valid'):
                duration = (time.time() - start_time) * 1000
                self.log_result(
                    test_name, "performance", "FAIL",
                    duration, "Rule compilation failed in performance test",
                    {'rule_id': rule_id}
                )
                requests.delete(f"{self.config['api_base_url']}/rules/{rule_id}")
                return False

            # Execute rule
            execute_response = requests.post(
                f"{self.config['api_base_url']}/rules/{rule_id}/execute",
                json=test_data,
                timeout=self.config['timeout']
            )

            duration = (time.time() - start_time) * 1000

            if execute_response.status_code == 200:
                threshold = self.config['performance_thresholds']['end_to_end_time']
                if duration <= threshold:
                    self.log_result(
                        test_name, "performance", "PASS",
                        duration, f"End-to-end workflow completed in {duration:.0f}ms",
                        {
                            'rule_id': rule_id,
                            'threshold_ms': threshold,
                            'execution_result': execute_response.json()
                        }
                    )
                    success = True
                else:
                    self.log_result(
                        test_name, "performance", "FAIL",
                        duration, f"End-to-end workflow too slow ({duration:.0f}ms > {threshold}ms)",
                        {
                            'rule_id': rule_id,
                            'threshold_ms': threshold
                        }
                    )
                    success = False
            else:
                self.log_result(
                    test_name, "performance", "FAIL",
                    duration, f"Rule execution failed: {execute_response.status_code}",
                    {'rule_id': rule_id}
                )
                success = False

            # Cleanup
            requests.delete(f"{self.config['api_base_url']}/rules/{rule_id}")
            return success

        except Exception as e:
            duration = (time.time() - start_time) * 1000
            self.log_result(
                test_name, "performance", "FAIL",
                duration, f"End-to-end performance test error: {str(e)}",
                {'error': str(e)}
            )
            return False

    def generate_integration_report(self, output_file: Optional[str] = None) -> Dict[str, Any]:
        """Generate comprehensive integration test report"""
        # Categorize results
        by_category = {}
        by_status = {'PASS': 0, 'FAIL': 0, 'SKIP': 0}

        for result in self.results:
            # By category
            if result.test_category not in by_category:
                by_category[result.test_category] = {'PASS': 0, 'FAIL': 0, 'SKIP': 0}
            by_category[result.test_category][result.status] += 1

            # By status
            by_status[result.status] += 1

        # Calculate performance metrics
        performance_results = [r for r in self.results if r.test_category == 'performance']
        avg_performance = sum(r.duration_ms for r in performance_results) / len(performance_results) if performance_results else 0

        report = {
            'timestamp': datetime.now().isoformat(),
            'total_tests': len(self.results),
            'summary': {
                'passed': by_status['PASS'],
                'failed': by_status['FAIL'],
                'skipped': by_status['SKIP'],
                'success_rate': (by_status['PASS'] / len(self.results) * 100) if self.results else 0
            },
            'by_category': by_category,
            'performance_metrics': {
                'average_test_duration_ms': avg_performance,
                'total_test_duration_ms': sum(r.duration_ms for r in self.results),
                'performance_threshold_violations': len([
                    r for r in self.results
                    if 'threshold' in r.details and r.duration_ms > r.details.get('threshold_ms', float('inf'))
                ])
            },
            'detailed_results': [
                {
                    'test_name': r.test_name,
                    'test_category': r.test_category,
                    'status': r.status,
                    'duration_ms': r.duration_ms,
                    'message': r.message,
                    'details': r.details,
                    'timestamp': r.timestamp
                }
                for r in self.results
            ]
        }

        if output_file:
            with open(output_file, 'w') as f:
                json.dump(report, f, indent=2)

        return report

    def run_integration_tests(self, test_categories: Optional[List[str]] = None) -> bool:
        """Run integration tests based on specified categories"""
        print("=" * 60)
        print("INTEGRATION TEST FRAMEWORK - STARTING")
        print("=" * 60)

        all_categories = [
            'connectivity', 'workflow', 'compilation', 'execution',
            'actionset', 'error_handling', 'concurrency', 'performance'
        ]

        if test_categories is None:
            test_categories = all_categories

        # Define test suites
        test_suites = {
            'connectivity': [
                ("API Health and Connectivity", self.test_api_health_and_connectivity),
                ("Database Connectivity", self.test_database_connectivity),
                ("Java Bridge Compilation", self.test_java_bridge_compilation)
            ],
            'workflow': [
                ("Rule CRUD Workflow", self.test_rule_crud_workflow)
            ],
            'compilation': [
                ("Rule Compilation Integration", self.test_rule_compilation_integration)
            ],
            'execution': [
                ("Rule Execution Integration", self.test_rule_execution_integration)
            ],
            'actionset': [
                ("ActionSet Integration", self.test_actionset_integration)
            ],
            'error_handling': [
                ("Error Handling Integration", self.test_error_handling_integration)
            ],
            'concurrency': [
                ("Concurrent Operations", self.test_concurrent_operations)
            ],
            'performance': [
                ("End-to-End Performance", self.test_end_to_end_performance)
            ]
        }

        overall_success = True

        for category in test_categories:
            if category in test_suites:
                print(f"\n=== {category.upper()} TESTS ===")
                for test_name, test_func in test_suites[category]:
                    try:
                        if not test_func():
                            overall_success = False
                    except Exception as e:
                        print(f"❌ {test_name}: EXCEPTION - {str(e)}")
                        overall_success = False

        # Generate report
        report_file = f"integration_test_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        report = self.generate_integration_report(report_file)

        print(f"\n{'='*60}")
        print("INTEGRATION TEST FRAMEWORK - SUMMARY")
        print(f"{'='*60}")
        print(f"Total Tests: {report['total_tests']}")
        print(f"Passed: {report['summary']['passed']}")
        print(f"Failed: {report['summary']['failed']}")
        print(f"Skipped: {report['summary']['skipped']}")
        print(f"Success Rate: {report['summary']['success_rate']:.1f}%")
        print(f"Average Duration: {report['performance_metrics']['average_test_duration_ms']:.0f}ms")
        print(f"Report saved to: {report_file}")

        return overall_success

def main():
    """Main entry point"""
    import argparse

    parser = argparse.ArgumentParser(description="Integration Test Framework")
    parser.add_argument(
        '--categories',
        nargs='+',
        choices=['connectivity', 'workflow', 'compilation', 'execution',
                'actionset', 'error_handling', 'concurrency', 'performance'],
        help='Test categories to run (default: all)'
    )

    args = parser.parse_args()

    framework = IntegrationTestFramework()
    success = framework.run_integration_tests(args.categories)

    return 0 if success else 1

if __name__ == "__main__":
    exit(main())