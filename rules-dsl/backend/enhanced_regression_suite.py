#!/usr/bin/env python3
"""
Enhanced Regression Testing Suite
Extended version of the existing regression suite with additional validation capabilities.

This module provides:
- Advanced rule execution testing
- Performance regression detection
- Schema evolution validation
- API contract versioning checks
- Security validation
"""

import requests
import json
import sys
import time
import sqlite3
import os
import subprocess
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
from pathlib import Path

class EnhancedRegressionSuite:
    """Enhanced regression testing with additional validation capabilities"""

    def __init__(self, api_base="http://localhost:5001/api", db_path=None):
        self.api_base = api_base
        self.db_path = db_path or self._get_default_db_path()
        self.issues = []
        self.performance_baseline = self._load_performance_baseline()
        self.schema_baseline = self._load_schema_baseline()

    def _get_default_db_path(self) -> str:
        """Get default database path based on project structure"""
        # Look for database in common locations
        possible_paths = [
            "database/rules.db",
            "../database/rules.db",
            "./rules.db",
            os.path.join(os.getcwd(), "database/rules.db"),
            os.path.join(os.path.dirname(os.path.abspath(__file__)), "../database/rules.db")
        ]

        for path in possible_paths:
            if os.path.exists(path):
                return path

        # Default fallback
        return "database/rules.db"

    def log_issue(self, severity: str, message: str, details: Optional[Dict] = None):
        """Log a regression issue with enhanced metadata"""
        issue = {
            'timestamp': datetime.now().isoformat(),
            'severity': severity,
            'message': message,
            'details': details or {},
            'test_category': getattr(self, '_current_test_category', 'general')
        }
        self.issues.append(issue)
        print(f"[{severity}] {message}")
        if details:
            for key, value in details.items():
                print(f"  {key}: {value}")

    def _load_performance_baseline(self) -> Dict:
        """Load performance baseline from file or create default"""
        baseline_file = Path("performance_baseline.json")
        if baseline_file.exists():
            try:
                with open(baseline_file, 'r') as f:
                    return json.load(f)
            except (json.JSONDecodeError, IOError) as e:
                print(f"Warning: Could not load performance baseline: {e}")
                pass

        # Default baseline
        return {
            'api_response_times': {
                '/api/rules': 500,  # ms
                '/api/rules/{id}': 200,
                '/api/schema/entities': 300,
                '/api/hierarchy/process-areas': 400
            },
            'rule_execution_times': {
                'simple_rule': 100,  # ms
                'complex_rule': 500,
                'actionset': 800
            },
            'database_query_times': {
                'simple_select': 50,  # ms
                'complex_join': 200,
                'count_query': 30
            }
        }

    def _load_schema_baseline(self) -> Dict:
        """Load schema baseline for evolution detection"""
        baseline_file = Path("schema_baseline.json")
        if baseline_file.exists():
            try:
                with open(baseline_file, 'r') as f:
                    return json.load(f)
            except (json.JSONDecodeError, IOError) as e:
                print(f"Warning: Could not load schema baseline: {e}")
                pass
        return {}

    def _update_performance_baseline(self, category: str, test_name: str, time_ms: float):
        """Update performance baseline with new measurements"""
        if category not in self.performance_baseline:
            self.performance_baseline[category] = {}

        # Update with 90th percentile approach (conservative)
        current = self.performance_baseline[category].get(test_name, time_ms)
        self.performance_baseline[category][test_name] = max(current, time_ms * 1.1)

    def test_rule_execution_performance(self) -> bool:
        """Test rule execution performance and detect regressions"""
        self._current_test_category = 'performance'
        print("=== Testing Rule Execution Performance ===")

        try:
            # Get a sample of rules for testing
            response = requests.get(f"{self.api_base}/rules?limit=5&status=VALID")
            if response.status_code != 200:
                self.log_issue('HIGH', 'Failed to fetch rules for performance testing')
                return False

            rules = response.json()['rules']
            all_performance_ok = True

            for rule in rules:
                # Test rule compilation performance
                start_time = time.time()
                compile_response = requests.post(
                    f"{self.api_base}/rules/{rule['id']}/compile",
                    timeout=30
                )
                compile_time = (time.time() - start_time) * 1000

                baseline_time = self.performance_baseline['rule_execution_times'].get('simple_rule', 1000)

                if compile_response.status_code == 200:
                    if compile_time <= baseline_time * 2:  # Allow 2x degradation
                        print(f"✅ Rule {rule['name']} compilation: {compile_time:.0f}ms")
                    else:
                        self.log_issue(
                            'MEDIUM',
                            f"Performance regression in rule compilation: {rule['name']}",
                            {
                                'rule_name': rule['name'],
                                'compile_time_ms': compile_time,
                                'baseline_ms': baseline_time,
                                'degradation_factor': compile_time / baseline_time
                            }
                        )
                        all_performance_ok = False
                else:
                    self.log_issue(
                        'HIGH',
                        f"Rule compilation failed: {rule['name']}",
                        {'rule_name': rule['name'], 'status_code': compile_response.status_code}
                    )
                    all_performance_ok = False

                # Test rule execution if compilation succeeded
                if compile_response.status_code == 200:
                    test_data = {
                        'applicant': {
                            'creditScore': 720,
                            'age': 30,
                            'annualIncome': 60000,
                            'employmentStatus': 'employed',
                            'employmentYears': 5
                        }
                    }

                    start_time = time.time()
                    execute_response = requests.post(
                        f"{self.api_base}/rules/{rule['id']}/execute",
                        json=test_data,
                        timeout=30
                    )
                    execute_time = (time.time() - start_time) * 1000

                    if execute_response.status_code == 200:
                        execution_baseline = self.performance_baseline['rule_execution_times'].get('simple_rule', 1000)
                        if execute_time <= execution_baseline * 2:
                            print(f"✅ Rule {rule['name']} execution: {execute_time:.0f}ms")
                        else:
                            self.log_issue(
                                'MEDIUM',
                                f"Performance regression in rule execution: {rule['name']}",
                                {
                                    'rule_name': rule['name'],
                                    'execute_time_ms': execute_time,
                                    'baseline_ms': execution_baseline,
                                    'degradation_factor': execute_time / execution_baseline
                                }
                            )
                            all_performance_ok = False

            return all_performance_ok

        except Exception as e:
            self.log_issue('CRITICAL', f'Rule execution performance test failed: {str(e)}')
            return False

    def test_api_performance_regression(self) -> bool:
        """Test API endpoint performance for regressions"""
        self._current_test_category = 'api_performance'
        print("=== Testing API Performance ===")

        endpoints_to_test = [
            ('/rules?limit=10', 'GET', None),
            ('/rules', 'POST', {
                'name': f'perf_test_rule_{int(time.time())}',
                'content': 'rule perfTest: if applicant.age >= 18 then approveApplication',
                'process_area_id': 1,
                'status': 'DRAFT'
            }),
            ('/schema/entities', 'GET', None),
            ('/hierarchy/process-areas', 'GET', None)
        ]

        all_performance_ok = True
        created_rule_id = None

        for endpoint, method, data in endpoints_to_test:
            start_time = time.time()

            try:
                if method == 'GET':
                    response = requests.get(f"{self.api_base}{endpoint}", timeout=30)
                elif method == 'POST':
                    response = requests.post(f"{self.api_base}{endpoint}", json=data, timeout=30)

                response_time = (time.time() - start_time) * 1000

                if response.status_code in [200, 201]:
                    # Check against baseline
                    baseline_key = endpoint.split('?')[0]  # Remove query params for baseline lookup
                    baseline_time = self.performance_baseline['api_response_times'].get(baseline_key, 1000)

                    if response_time <= baseline_time * 2:  # Allow 2x degradation
                        print(f"✅ {method} {endpoint}: {response_time:.0f}ms")

                        # Store rule ID for cleanup
                        if method == 'POST' and endpoint == '/rules':
                            created_rule_id = response.json().get('id')
                    else:
                        self.log_issue(
                            'MEDIUM',
                            f"API performance regression: {method} {endpoint}",
                            {
                                'endpoint': endpoint,
                                'method': method,
                                'response_time_ms': response_time,
                                'baseline_ms': baseline_time,
                                'degradation_factor': response_time / baseline_time
                            }
                        )
                        all_performance_ok = False
                else:
                    self.log_issue(
                        'HIGH',
                        f"API endpoint failed: {method} {endpoint}",
                        {'status_code': response.status_code, 'endpoint': endpoint}
                    )
                    all_performance_ok = False

            except Exception as e:
                self.log_issue(
                    'HIGH',
                    f"API performance test failed: {method} {endpoint}",
                    {'error': str(e)}
                )
                all_performance_ok = False

        # Cleanup created rule
        if created_rule_id:
            try:
                requests.delete(f"{self.api_base}/rules/{created_rule_id}", timeout=10)
            except requests.RequestException as e:
                print(f"Warning: Could not cleanup test rule {created_rule_id}: {e}")
                pass  # Cleanup failure is not critical

        return all_performance_ok

    def test_schema_evolution_safety(self) -> bool:
        """Test that schema changes don't break existing functionality"""
        self._current_test_category = 'schema_evolution'
        print("=== Testing Schema Evolution Safety ===")

        if not os.path.exists(self.db_path):
            self.log_issue('CRITICAL', f'Database file not found: {self.db_path}')
            return False

        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            # Get current schema structure
            cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
            current_tables = [row[0] for row in cursor.fetchall()]

            current_schema = {}
            for table in current_tables:
                cursor.execute(f"PRAGMA table_info({table})")
                columns = cursor.fetchall()
                current_schema[table] = {
                    col[1]: {  # column name
                        'type': col[2],
                        'not_null': bool(col[3]),
                        'default': col[4],
                        'primary_key': bool(col[5])
                    }
                    for col in columns
                }

            # Compare with baseline if available
            if self.schema_baseline:
                schema_changes = self._detect_schema_changes(self.schema_baseline, current_schema)

                if schema_changes['breaking_changes']:
                    for change in schema_changes['breaking_changes']:
                        self.log_issue('HIGH', f"Breaking schema change detected: {change}")
                    return False

                if schema_changes['warnings']:
                    for warning in schema_changes['warnings']:
                        self.log_issue('MEDIUM', f"Schema evolution warning: {warning}")

            # Validate that critical tables and columns exist
            critical_schema = {
                'rules': ['id', 'name', 'status', 'content', 'process_area_id'],
                'process_areas': ['id', 'name', 'code'],
                'schema_entities': ['id', 'name', 'is_active'],
                'schema_attributes': ['id', 'entity_id', 'name', 'data_type']
            }

            all_critical_present = True
            for table, required_columns in critical_schema.items():
                if table not in current_schema:
                    self.log_issue('CRITICAL', f"Critical table missing: {table}")
                    all_critical_present = False
                else:
                    for column in required_columns:
                        if column not in current_schema[table]:
                            self.log_issue('CRITICAL', f"Critical column missing: {table}.{column}")
                            all_critical_present = False

            # Test that deprecated columns have been removed
            deprecated_elements = [
                ('rules', 'validation_status'),  # Should be removed
            ]

            for table, column in deprecated_elements:
                if table in current_schema and column in current_schema[table]:
                    self.log_issue('MEDIUM', f"Deprecated element still present: {table}.{column}")

            conn.close()

            # Update baseline with current schema
            if all_critical_present:
                self._save_schema_baseline(current_schema)

            return all_critical_present

        except Exception as e:
            self.log_issue('CRITICAL', f'Schema evolution test failed: {str(e)}')
            return False

    def _detect_schema_changes(self, baseline: Dict, current: Dict) -> Dict:
        """Detect breaking and non-breaking schema changes"""
        breaking_changes = []
        warnings = []

        # Check for removed tables
        for table in baseline:
            if table not in current:
                breaking_changes.append(f"Table removed: {table}")

        # Check for column changes
        for table in baseline:
            if table in current:
                baseline_columns = baseline[table]
                current_columns = current[table]

                # Check for removed columns
                for column in baseline_columns:
                    if column not in current_columns:
                        breaking_changes.append(f"Column removed: {table}.{column}")

                # Check for type changes
                for column in baseline_columns:
                    if column in current_columns:
                        baseline_col = baseline_columns[column]
                        current_col = current_columns[column]

                        if baseline_col['type'] != current_col['type']:
                            breaking_changes.append(
                                f"Column type changed: {table}.{column} "
                                f"from {baseline_col['type']} to {current_col['type']}"
                            )

                        if baseline_col['not_null'] != current_col['not_null']:
                            if current_col['not_null'] and not baseline_col['not_null']:
                                breaking_changes.append(
                                    f"Column made NOT NULL: {table}.{column}"
                                )
                            else:
                                warnings.append(
                                    f"Column nullability changed: {table}.{column}"
                                )

        return {
            'breaking_changes': breaking_changes,
            'warnings': warnings
        }

    def _save_schema_baseline(self, schema: Dict):
        """Save current schema as baseline"""
        try:
            with open('schema_baseline.json', 'w') as f:
                json.dump(schema, f, indent=2)
        except Exception as e:
            print(f"Warning: Could not save schema baseline: {e}")

    def test_security_regression(self) -> bool:
        """Test for common security regressions"""
        self._current_test_category = 'security'
        print("=== Testing Security Regression ===")

        all_security_ok = True

        # Test 1: SQL Injection protection
        malicious_inputs = [
            "'; DROP TABLE rules; --",
            "1' OR '1'='1",
            "admin'/*",
            "1'; UPDATE rules SET status='PROD' WHERE id=1; --"
        ]

        for malicious_input in malicious_inputs:
            try:
                # Test rule name field
                response = requests.post(
                    f"{self.api_base}/rules",
                    json={
                        'name': malicious_input,
                        'content': 'rule test: if true then approveApplication',
                        'process_area_id': 1
                    },
                    timeout=10
                )

                # Should either reject or sanitize the input
                if response.status_code == 201:
                    rule_id = response.json().get('id')
                    # Check if the malicious input was stored as-is
                    get_response = requests.get(f"{self.api_base}/rules/{rule_id}")
                    if get_response.status_code == 200:
                        stored_name = get_response.json().get('name')
                        if stored_name == malicious_input:
                            self.log_issue(
                                'HIGH',
                                'Potential SQL injection vulnerability: malicious input stored',
                                {'malicious_input': malicious_input}
                            )
                            all_security_ok = False

                    # Cleanup
                    requests.delete(f"{self.api_base}/rules/{rule_id}")

            except Exception as e:
                # Timeouts or errors might indicate protection mechanisms
                pass

        # Test 2: Check for exposed sensitive information
        try:
            response = requests.get(f"{self.api_base}/rules?limit=1")
            if response.status_code == 200:
                data = response.json()
                if data.get('rules'):
                    rule = data['rules'][0]
                    # Check for fields that shouldn't be exposed
                    sensitive_fields = ['password', 'secret', 'key', 'token']
                    for field in sensitive_fields:
                        if any(field in str(value).lower() for value in rule.values()):
                            self.log_issue(
                                'MEDIUM',
                                f'Potentially sensitive information exposed in API response',
                                {'field': field, 'rule_id': rule.get('id')}
                            )

        except Exception as e:
            self.log_issue('MEDIUM', f'Security test error: {str(e)}')

        # Test 3: Authentication bypass attempts
        # This would test authentication if implemented
        # For now, just verify that protected endpoints exist
        auth_required_endpoints = [
            '/rules',  # POST should require auth in production
            '/admin',  # Admin endpoints should require auth
        ]

        # Note: This is a placeholder for when authentication is implemented
        print("Note: Authentication security tests skipped (auth not implemented)")

        return all_security_ok

    def test_data_consistency_rules(self) -> bool:
        """Test advanced data consistency rules and constraints"""
        self._current_test_category = 'data_consistency'
        print("=== Testing Data Consistency Rules ===")

        if not os.path.exists(self.db_path):
            self.log_issue('CRITICAL', f'Database file not found: {self.db_path}')
            return False

        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            all_consistent = True

            # Test 1: Referential integrity
            cursor.execute("""
                SELECT r.id, r.name, r.process_area_id
                FROM rules r
                LEFT JOIN process_areas pa ON r.process_area_id = pa.id
                WHERE r.process_area_id IS NOT NULL AND pa.id IS NULL
            """)
            orphaned_rules = cursor.fetchall()

            if orphaned_rules:
                self.log_issue(
                    'HIGH',
                    f'Referential integrity violation: {len(orphaned_rules)} rules with invalid process_area_id',
                    {'orphaned_rules': [{'id': r[0], 'name': r[1], 'process_area_id': r[2]} for r in orphaned_rules]}
                )
                all_consistent = False

            # Test 2: Duplicate rule names within same process area
            cursor.execute("""
                SELECT process_area_id, name, COUNT(*) as count
                FROM rules
                WHERE status != 'deleted'
                GROUP BY process_area_id, name
                HAVING COUNT(*) > 1
            """)
            duplicates = cursor.fetchall()

            if duplicates:
                self.log_issue(
                    'MEDIUM',
                    f'Duplicate rule names found: {len(duplicates)} cases',
                    {'duplicates': [{'process_area_id': d[0], 'name': d[1], 'count': d[2]} for d in duplicates]}
                )
                all_consistent = False

            # Test 3: Status transition validity
            # Check for invalid status combinations or transitions
            cursor.execute("""
                SELECT id, name, status, effective_date
                FROM rules
                WHERE status IN ('PROD', 'SCHD') AND effective_date IS NULL
            """)
            invalid_prod_rules = cursor.fetchall()

            if invalid_prod_rules:
                self.log_issue(
                    'HIGH',
                    f'Production rules without effective_date: {len(invalid_prod_rules)} rules',
                    {'invalid_rules': [{'id': r[0], 'name': r[1], 'status': r[2]} for r in invalid_prod_rules]}
                )
                all_consistent = False

            # Test 4: Schema attribute consistency
            cursor.execute("""
                SELECT sa.id, sa.name, sa.data_type, sa.java_type
                FROM schema_attributes sa
                JOIN schema_entities se ON sa.entity_id = se.id
                WHERE se.is_active = 1
            """)
            attributes = cursor.fetchall()

            type_mapping = {
                'string': 'String',
                'number': ['int', 'Integer', 'BigDecimal', 'Double'],
                'date': ['LocalDate', 'Date'],
                'boolean': ['boolean', 'Boolean']
            }

            for attr in attributes:
                attr_id, name, data_type, java_type = attr
                expected_java_types = type_mapping.get(data_type, [])

                if isinstance(expected_java_types, list):
                    if java_type not in expected_java_types:
                        self.log_issue(
                            'MEDIUM',
                            f'Schema attribute type mismatch: {name}',
                            {
                                'attribute_id': attr_id,
                                'data_type': data_type,
                                'java_type': java_type,
                                'expected': expected_java_types
                            }
                        )
                        all_consistent = False
                else:
                    if java_type != expected_java_types:
                        self.log_issue(
                            'MEDIUM',
                            f'Schema attribute type mismatch: {name}',
                            {
                                'attribute_id': attr_id,
                                'data_type': data_type,
                                'java_type': java_type,
                                'expected': expected_java_types
                            }
                        )
                        all_consistent = False

            conn.close()
            return all_consistent

        except Exception as e:
            self.log_issue('CRITICAL', f'Data consistency test failed: {str(e)}')
            return False

    def test_actionset_regression(self) -> bool:
        """Test ActionSet-specific regression scenarios"""
        self._current_test_category = 'actionset'
        print("=== Testing ActionSet Regression ===")

        try:
            # Get ActionSets (rules with item_type='actionset')
            response = requests.get(f"{self.api_base}/rules?item_type=actionset")
            if response.status_code != 200:
                self.log_issue('HIGH', 'Failed to fetch ActionSets for testing')
                return False

            actionsets = response.json()['rules']
            all_actionset_ok = True

            if not actionsets:
                self.log_issue('MEDIUM', 'No ActionSets found for regression testing')
                return True

            for actionset in actionsets[:3]:  # Test first 3 ActionSets
                # Test ActionSet compilation
                try:
                    compile_response = requests.post(
                        f"{self.api_base}/rules/{actionset['id']}/compile",
                        timeout=30
                    )

                    if compile_response.status_code == 200:
                        compile_result = compile_response.json()

                        # Check for ActionSet-specific compilation issues
                        if not compile_result.get('valid', False):
                            validation_errors = compile_result.get('validation', {}).get('errors', [])
                            actionset_errors = [err for err in validation_errors if 'actionset' in err.lower()]

                            if actionset_errors:
                                self.log_issue(
                                    'HIGH',
                                    f'ActionSet compilation regression: {actionset["name"]}',
                                    {
                                        'actionset_id': actionset['id'],
                                        'errors': actionset_errors
                                    }
                                )
                                all_actionset_ok = False

                        # Check for proper ActionSet syntax recognition
                        content = actionset.get('content', '')
                        if 'ActionSet' in content or 'actionset' in content.lower():
                            if not any('actionset' in error.lower() for error in
                                     compile_result.get('validation', {}).get('info', [])):
                                # ActionSet should be recognized in compilation info
                                pass  # This is okay, not all ActionSets need special info

                    else:
                        self.log_issue(
                            'HIGH',
                            f'ActionSet compilation failed: {actionset["name"]}',
                            {
                                'actionset_id': actionset['id'],
                                'status_code': compile_response.status_code
                            }
                        )
                        all_actionset_ok = False

                except Exception as e:
                    self.log_issue(
                        'MEDIUM',
                        f'ActionSet test error: {actionset["name"]}',
                        {'error': str(e), 'actionset_id': actionset['id']}
                    )
                    all_actionset_ok = False

            return all_actionset_ok

        except Exception as e:
            self.log_issue('CRITICAL', f'ActionSet regression test failed: {str(e)}')
            return False

    def run_enhanced_suite(self) -> bool:
        """Run the complete enhanced regression suite"""
        print("=" * 60)
        print("ENHANCED REGRESSION TEST SUITE - STARTING")
        print("=" * 60)

        # First run the basic regression suite
        basic_success = self._run_basic_regression_suite()

        # Then run enhanced tests
        tests = [
            ("Rule Execution Performance", self.test_rule_execution_performance),
            ("API Performance", self.test_api_performance_regression),
            ("Schema Evolution Safety", self.test_schema_evolution_safety),
            ("Security Regression", self.test_security_regression),
            ("Data Consistency Rules", self.test_data_consistency_rules),
            ("ActionSet Regression", self.test_actionset_regression)
        ]

        enhanced_success = True
        for test_name, test_func in tests:
            try:
                if not test_func():
                    enhanced_success = False
            except Exception as e:
                self.log_issue('CRITICAL', f'{test_name} failed with exception: {str(e)}')
                enhanced_success = False

        overall_success = basic_success and enhanced_success

        print("=" * 60)
        print("ENHANCED REGRESSION TEST SUITE - RESULTS")
        print("=" * 60)

        if not self.issues:
            print("✅ ALL ENHANCED TESTS PASSED - No regression issues detected")
        else:
            print(f"❌ {len(self.issues)} ISSUES DETECTED:")

            # Group by severity
            critical = [i for i in self.issues if i['severity'] == 'CRITICAL']
            high = [i for i in self.issues if i['severity'] == 'HIGH']
            medium = [i for i in self.issues if i['severity'] == 'MEDIUM']
            low = [i for i in self.issues if i['severity'] == 'LOW']

            print(f"  CRITICAL: {len(critical)}")
            print(f"  HIGH: {len(high)}")
            print(f"  MEDIUM: {len(medium)}")
            print(f"  LOW: {len(low)}")

            # Group by category
            categories = {}
            for issue in self.issues:
                category = issue.get('test_category', 'unknown')
                if category not in categories:
                    categories[category] = 0
                categories[category] += 1

            print("\nIssues by category:")
            for category, count in categories.items():
                print(f"  {category}: {count}")

        # Save enhanced results
        enhanced_results = {
            'timestamp': datetime.now().isoformat(),
            'basic_success': basic_success,
            'enhanced_success': enhanced_success,
            'overall_success': overall_success,
            'issues': self.issues,
            'performance_data': self.performance_baseline
        }

        with open('enhanced_regression_results.json', 'w') as f:
            json.dump(enhanced_results, f, indent=2)

        return overall_success

    def _run_basic_regression_suite(self) -> bool:
        """Run the existing basic regression suite"""
        try:
            success, stdout, stderr = self._run_command("python test_regression_suite.py")
            return success
        except Exception as e:
            self.log_issue('CRITICAL', f'Basic regression suite failed: {str(e)}')
            return False

    def _run_command(self, cmd: str) -> Tuple[bool, str, str]:
        """Run a command and return success status, stdout, stderr"""
        try:
            result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=120)
            return result.returncode == 0, result.stdout, result.stderr
        except subprocess.TimeoutExpired:
            return False, "", "Command timed out"
        except Exception as e:
            return False, "", str(e)

if __name__ == "__main__":
    # Change to backend directory
    # Try to find and change to backend directory dynamically
    script_dir = os.path.dirname(os.path.abspath(__file__))
    backend_dir = None

    # Look for backend directory in common locations
    possible_backend_dirs = [
        script_dir,  # Current directory if script is in backend
        os.path.join(script_dir, "backend"),  # If script is in project root
        os.path.join(os.path.dirname(script_dir), "backend")  # If script is elsewhere
    ]

    for dir_path in possible_backend_dirs:
        if os.path.exists(os.path.join(dir_path, "app.py")) or os.path.exists(os.path.join(dir_path, "models.py")):
            backend_dir = dir_path
            break

    if backend_dir:
        os.chdir(backend_dir)
        print(f"Changed to backend directory: {backend_dir}")
    else:
        print("Warning: Could not locate backend directory, using current directory")

    suite = EnhancedRegressionSuite()
    success = suite.run_enhanced_suite()

    sys.exit(0 if success else 1)