#!/usr/bin/env python3
"""
Comprehensive Validation Agent System
Prevents regression issues and ensures code quality across the entire rules engine.

This agent provides:
1. Automated Regression Detection
2. Code Quality Validation
3. Data Integrity Validation
4. System Health Validation
5. Architecture Compliance Validation
6. Integration Testing

Usage:
    python validation_agent.py [--mode full|quick|health|regression]
"""

import argparse
import sys
import os
import json
import subprocess
import time
import sqlite3
import requests
from datetime import datetime
from typing import Dict, List, Tuple, Optional
from pathlib import Path

class ValidationResult:
    """Represents the result of a validation check"""

    def __init__(self, check_name: str, status: str, message: str,
                 severity: str = "INFO", details: Optional[Dict] = None):
        self.check_name = check_name
        self.status = status  # PASS, FAIL, WARN, SKIP
        self.message = message
        self.severity = severity  # CRITICAL, HIGH, MEDIUM, LOW, INFO
        self.details = details or {}
        self.timestamp = datetime.now().isoformat()

class ValidationAgent:
    """Comprehensive validation agent for the rules engine"""

    def __init__(self, config: Optional[Dict] = None):
        self.config = config or self._load_default_config()
        self.results: List[ValidationResult] = []
        self.start_time = datetime.now()

        # Project paths
        self.project_root = Path("/Users/chandramohn/workspace/rules_engine/ui-prototype")
        self.backend_path = self.project_root / "backend"
        self.frontend_path = self.project_root / "frontend"
        self.java_bridge_path = self.project_root / "java-bridge"
        self.db_path = self.backend_path / "database" / "rules.db"

    def _load_default_config(self) -> Dict:
        """Load default configuration"""
        return {
            'api_base_url': 'http://localhost:5001/api',
            'frontend_url': 'http://localhost:3000',
            'timeout': 30,
            'required_services': ['backend', 'frontend'],
            'performance_thresholds': {
                'api_response_time': 1000,  # ms
                'rule_execution_time': 1000,  # ms
                'page_load_time': 3000  # ms
            },
            'critical_endpoints': [
                '/api/health',
                '/api/rules',
                '/api/schema/entities',
                '/api/hierarchy/process-areas'
            ]
        }

    def log_result(self, check_name: str, status: str, message: str,
                   severity: str = "INFO", details: Optional[Dict] = None):
        """Log a validation result"""
        result = ValidationResult(check_name, status, message, severity, details)
        self.results.append(result)

        # Print result with appropriate formatting
        icon = {"PASS": "✅", "FAIL": "❌", "WARN": "⚠️", "SKIP": "⏭️"}
        print(f"{icon.get(status, '📋')} [{severity}] {check_name}: {message}")

        if details:
            for key, value in details.items():
                print(f"   {key}: {value}")

    def run_command(self, cmd: str, timeout: int = 30, cwd: Optional[str] = None) -> Tuple[bool, str, str]:
        """Run a shell command and return success status, stdout, stderr"""
        try:
            result = subprocess.run(
                cmd, shell=True, capture_output=True, text=True,
                timeout=timeout, cwd=cwd
            )
            return result.returncode == 0, result.stdout, result.stderr
        except subprocess.TimeoutExpired:
            return False, "", f"Command timed out after {timeout}s"
        except Exception as e:
            return False, "", str(e)

    def check_api_health(self) -> bool:
        """Check if backend API is healthy and responsive"""
        try:
            start_time = time.time()
            response = requests.get(
                f"{self.config['api_base_url']}/health",
                timeout=self.config['timeout']
            )
            response_time = (time.time() - start_time) * 1000

            if response.status_code == 200:
                data = response.json()
                if data.get('status') == 'healthy':
                    self.log_result(
                        "API Health Check", "PASS",
                        f"Backend API is healthy (response time: {response_time:.0f}ms)",
                        details={'response_time_ms': response_time, 'status': data.get('status')}
                    )
                    return True
                else:
                    self.log_result(
                        "API Health Check", "FAIL",
                        f"API returned unhealthy status: {data.get('status')}",
                        "HIGH"
                    )
                    return False
            else:
                self.log_result(
                    "API Health Check", "FAIL",
                    f"API returned status {response.status_code}",
                    "CRITICAL"
                )
                return False

        except requests.exceptions.RequestException as e:
            self.log_result(
                "API Health Check", "FAIL",
                f"Failed to connect to API: {str(e)}",
                "CRITICAL"
            )
            return False

    def validate_system_health(self) -> bool:
        """Validate overall system health"""
        print("\n=== SYSTEM HEALTH VALIDATION ===")

        all_healthy = True

        # Check backend service
        if not self.check_api_health():
            all_healthy = False

        # Check critical API endpoints
        for endpoint in self.config['critical_endpoints']:
            try:
                start_time = time.time()
                response = requests.get(
                    f"{self.config['api_base_url'].rstrip('/api')}{endpoint}",
                    timeout=self.config['timeout']
                )
                response_time = (time.time() - start_time) * 1000

                if response.status_code == 200:
                    self.log_result(
                        f"Endpoint {endpoint}", "PASS",
                        f"Endpoint responsive ({response_time:.0f}ms)",
                        details={'response_time_ms': response_time}
                    )
                else:
                    self.log_result(
                        f"Endpoint {endpoint}", "FAIL",
                        f"Endpoint returned {response.status_code}",
                        "HIGH"
                    )
                    all_healthy = False

            except Exception as e:
                self.log_result(
                    f"Endpoint {endpoint}", "FAIL",
                    f"Endpoint check failed: {str(e)}",
                    "HIGH"
                )
                all_healthy = False

        # Check database connectivity
        if self.db_path.exists():
            try:
                conn = sqlite3.connect(str(self.db_path))
                cursor = conn.cursor()
                cursor.execute("SELECT COUNT(*) FROM rules")
                rule_count = cursor.fetchone()[0]
                conn.close()

                if rule_count > 0:
                    self.log_result(
                        "Database Connectivity", "PASS",
                        f"Database accessible with {rule_count} rules",
                        details={'rule_count': rule_count}
                    )
                else:
                    self.log_result(
                        "Database Connectivity", "WARN",
                        "Database accessible but no rules found",
                        "MEDIUM"
                    )

            except Exception as e:
                self.log_result(
                    "Database Connectivity", "FAIL",
                    f"Database access failed: {str(e)}",
                    "CRITICAL"
                )
                all_healthy = False
        else:
            self.log_result(
                "Database Connectivity", "FAIL",
                f"Database file not found at {self.db_path}",
                "CRITICAL"
            )
            all_healthy = False

        # Check Java bridge compilation
        java_success, java_out, java_err = self.run_command(
            "mvn compile -q",
            cwd=str(self.java_bridge_path),
            timeout=60
        )

        if java_success:
            self.log_result(
                "Java Bridge Compilation", "PASS",
                "Java bridge compiles successfully"
            )
        else:
            self.log_result(
                "Java Bridge Compilation", "FAIL",
                f"Java compilation failed: {java_err}",
                "HIGH"
            )
            all_healthy = False

        return all_healthy

    def validate_regression_suite(self) -> bool:
        """Run the existing regression test suite"""
        print("\n=== REGRESSION DETECTION ===")

        # Change to backend directory and run regression tests
        success, stdout, stderr = self.run_command(
            "python test_regression_suite.py",
            cwd=str(self.backend_path),
            timeout=120
        )

        if success:
            # Try to read the results file
            results_file = self.backend_path / "regression_test_results.json"
            if results_file.exists():
                try:
                    with open(results_file, 'r') as f:
                        results = json.load(f)

                    if results.get('success', False):
                        issue_count = len(results.get('issues', []))
                        if issue_count == 0:
                            self.log_result(
                                "Regression Test Suite", "PASS",
                                "All regression tests passed with no issues"
                            )
                            return True
                        else:
                            # Check severity of issues
                            critical_issues = [i for i in results['issues'] if i.get('severity') == 'CRITICAL']
                            high_issues = [i for i in results['issues'] if i.get('severity') == 'HIGH']

                            if critical_issues or high_issues:
                                self.log_result(
                                    "Regression Test Suite", "FAIL",
                                    f"Found {len(critical_issues)} critical and {len(high_issues)} high-severity issues",
                                    "HIGH",
                                    details={'issues': results['issues']}
                                )
                                return False
                            else:
                                self.log_result(
                                    "Regression Test Suite", "WARN",
                                    f"Found {issue_count} medium/low-severity issues",
                                    "MEDIUM",
                                    details={'issues': results['issues']}
                                )
                                return True
                    else:
                        self.log_result(
                            "Regression Test Suite", "FAIL",
                            "Regression tests completed but reported failures",
                            "HIGH"
                        )
                        return False

                except Exception as e:
                    self.log_result(
                        "Regression Test Suite", "FAIL",
                        f"Failed to parse regression test results: {str(e)}",
                        "MEDIUM"
                    )
                    return False
            else:
                self.log_result(
                    "Regression Test Suite", "WARN",
                    "Regression tests completed but no results file found",
                    "MEDIUM"
                )
                return True
        else:
            self.log_result(
                "Regression Test Suite", "FAIL",
                f"Regression tests failed to run: {stderr}",
                "HIGH"
            )
            return False

    def validate_data_integrity(self) -> bool:
        """Validate data integrity and consistency"""
        print("\n=== DATA INTEGRITY VALIDATION ===")

        if not self.db_path.exists():
            self.log_result(
                "Data Integrity", "FAIL",
                f"Database file not found at {self.db_path}",
                "CRITICAL"
            )
            return False

        try:
            conn = sqlite3.connect(str(self.db_path))
            cursor = conn.cursor()

            all_valid = True

            # Check 1: Validate rule status values
            cursor.execute("""
                SELECT COUNT(*) FROM rules
                WHERE status NOT IN ('DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD', 'deleted')
            """)
            invalid_status_count = cursor.fetchone()[0]

            if invalid_status_count > 0:
                self.log_result(
                    "Rule Status Validation", "FAIL",
                    f"Found {invalid_status_count} rules with invalid status values",
                    "HIGH"
                )
                all_valid = False
            else:
                self.log_result(
                    "Rule Status Validation", "PASS",
                    "All rule status values are valid"
                )

            # Check 2: Check for missing required fields
            cursor.execute("SELECT COUNT(*) FROM rules WHERE name IS NULL OR name = ''")
            missing_names = cursor.fetchone()[0]

            if missing_names > 0:
                self.log_result(
                    "Required Fields Check", "FAIL",
                    f"Found {missing_names} rules with missing names",
                    "HIGH"
                )
                all_valid = False
            else:
                self.log_result(
                    "Required Fields Check", "PASS",
                    "All rules have required fields"
                )

            # Check 3: Validate foreign key relationships
            cursor.execute("""
                SELECT COUNT(*) FROM rules r
                LEFT JOIN process_areas pa ON r.process_area_id = pa.id
                WHERE r.process_area_id IS NOT NULL AND pa.id IS NULL
            """)
            orphaned_rules = cursor.fetchone()[0]

            if orphaned_rules > 0:
                self.log_result(
                    "Foreign Key Integrity", "FAIL",
                    f"Found {orphaned_rules} rules with invalid process_area_id",
                    "HIGH"
                )
                all_valid = False
            else:
                self.log_result(
                    "Foreign Key Integrity", "PASS",
                    "All foreign key relationships are valid"
                )

            # Check 4: Validate schema consistency
            cursor.execute("PRAGMA table_info(rules)")
            columns = [col[1] for col in cursor.fetchall()]

            required_columns = ['id', 'name', 'status', 'content', 'process_area_id']
            missing_columns = [col for col in required_columns if col not in columns]

            if missing_columns:
                self.log_result(
                    "Schema Consistency", "FAIL",
                    f"Missing required columns: {missing_columns}",
                    "CRITICAL"
                )
                all_valid = False
            else:
                self.log_result(
                    "Schema Consistency", "PASS",
                    "Database schema has all required columns"
                )

            # Check 5: Check for deprecated validation_status column
            if 'validation_status' in columns:
                self.log_result(
                    "Schema Migration", "FAIL",
                    "Deprecated validation_status column still exists",
                    "MEDIUM"
                )
                all_valid = False
            else:
                self.log_result(
                    "Schema Migration", "PASS",
                    "Deprecated columns have been properly removed"
                )

            conn.close()
            return all_valid

        except Exception as e:
            self.log_result(
                "Data Integrity", "FAIL",
                f"Data integrity check failed: {str(e)}",
                "CRITICAL"
            )
            return False

    def validate_code_quality(self) -> bool:
        """Validate code quality and adherence to patterns"""
        print("\n=== CODE QUALITY VALIDATION ===")

        all_valid = True

        # Check 1: Python code quality (backend)
        python_files = list(self.backend_path.glob("**/*.py"))
        for py_file in python_files[:5]:  # Sample check
            # Check for common anti-patterns
            try:
                with open(py_file, 'r') as f:
                    content = f.read()

                issues = []

                # Check for old validation_status references
                if 'validation_status' in content and 'removed' not in content.lower():
                    issues.append("Contains deprecated validation_status references")

                # Check for proper error handling
                if 'except:' in content:
                    issues.append("Contains bare except clauses")

                # Check for hardcoded paths
                if '/Users/' in content and str(py_file) not in [
                    str(self.backend_path / "test_regression_suite.py")
                ]:
                    issues.append("Contains hardcoded paths")

                if issues:
                    self.log_result(
                        f"Code Quality: {py_file.name}", "WARN",
                        f"Found issues: {', '.join(issues)}",
                        "MEDIUM"
                    )
                else:
                    self.log_result(
                        f"Code Quality: {py_file.name}", "PASS",
                        "No code quality issues found"
                    )

            except Exception as e:
                self.log_result(
                    f"Code Quality: {py_file.name}", "FAIL",
                    f"Failed to analyze file: {str(e)}",
                    "LOW"
                )

        # Check 2: Frontend code consistency
        frontend_components = list(self.frontend_path.glob("src/components/*.jsx"))
        for component in frontend_components[:3]:  # Sample check
            try:
                with open(component, 'r') as f:
                    content = f.read()

                issues = []

                # Check for deprecated status values
                old_statuses = ['value="draft"', 'value="active"', 'value="inactive"']
                for old_status in old_statuses:
                    if old_status in content:
                        issues.append(f"Contains deprecated status value: {old_status}")

                # Check for proper error handling
                if 'catch' in content and 'console.log' in content:
                    issues.append("May have console.log statements in error handling")

                if issues:
                    self.log_result(
                        f"Frontend Quality: {component.name}", "WARN",
                        f"Found issues: {', '.join(issues)}",
                        "MEDIUM"
                    )
                else:
                    self.log_result(
                        f"Frontend Quality: {component.name}", "PASS",
                        "No code quality issues found"
                    )

            except Exception as e:
                self.log_result(
                    f"Frontend Quality: {component.name}", "FAIL",
                    f"Failed to analyze file: {str(e)}",
                    "LOW"
                )

        return all_valid

    def validate_architecture_compliance(self) -> bool:
        """Validate architecture compliance and separation of concerns"""
        print("\n=== ARCHITECTURE COMPLIANCE VALIDATION ===")

        all_compliant = True

        # Check 1: Three-tier architecture validation
        required_dirs = {
            'frontend': self.frontend_path,
            'backend': self.backend_path,
            'java-bridge': self.java_bridge_path
        }

        for tier, path in required_dirs.items():
            if path.exists():
                self.log_result(
                    f"Architecture: {tier}", "PASS",
                    f"{tier} tier exists and is properly structured"
                )
            else:
                self.log_result(
                    f"Architecture: {tier}", "FAIL",
                    f"{tier} tier directory not found",
                    "CRITICAL"
                )
                all_compliant = False

        # Check 2: Proper separation of concerns
        # Backend should not have frontend dependencies
        backend_package_file = self.backend_path / "requirements.txt"
        if backend_package_file.exists():
            try:
                with open(backend_package_file, 'r') as f:
                    backend_deps = f.read()

                frontend_deps = ['react', 'vue', 'angular', 'node']
                mixed_deps = [dep for dep in frontend_deps if dep in backend_deps.lower()]

                if mixed_deps:
                    self.log_result(
                        "Separation of Concerns", "FAIL",
                        f"Backend has frontend dependencies: {mixed_deps}",
                        "HIGH"
                    )
                    all_compliant = False
                else:
                    self.log_result(
                        "Separation of Concerns", "PASS",
                        "Backend dependencies are properly separated"
                    )
            except Exception as e:
                self.log_result(
                    "Separation of Concerns", "WARN",
                    f"Could not validate backend dependencies: {str(e)}",
                    "LOW"
                )

        # Check 3: API layer consistency
        api_files = list(self.backend_path.glob("api/*.py"))
        if len(api_files) >= 3:  # Expect rules, schema, hierarchy APIs
            self.log_result(
                "API Layer Structure", "PASS",
                f"API layer properly structured with {len(api_files)} modules"
            )
        else:
            self.log_result(
                "API Layer Structure", "WARN",
                f"Expected at least 3 API modules, found {len(api_files)}",
                "MEDIUM"
            )

        return all_compliant

    def validate_integration_workflows(self) -> bool:
        """Test complete integration workflows"""
        print("\n=== INTEGRATION TESTING ===")

        all_workflows_pass = True

        # Workflow 1: Rule Creation and Retrieval
        try:
            # Create a test rule
            test_rule_data = {
                'name': f'test_rule_{int(time.time())}',
                'description': 'Validation agent test rule',
                'content': 'rule testRule:\n    if applicant.age >= 18 then approveApplication',
                'status': 'DRAFT',
                'process_area_id': 1
            }

            create_response = requests.post(
                f"{self.config['api_base_url']}/rules",
                json=test_rule_data,
                timeout=self.config['timeout']
            )

            if create_response.status_code == 201:
                rule_id = create_response.json()['id']

                # Retrieve the rule
                get_response = requests.get(
                    f"{self.config['api_base_url']}/rules/{rule_id}",
                    timeout=self.config['timeout']
                )

                if get_response.status_code == 200:
                    retrieved_rule = get_response.json()
                    if retrieved_rule['name'] == test_rule_data['name']:
                        self.log_result(
                            "Integration: Rule CRUD", "PASS",
                            "Rule creation and retrieval workflow successful"
                        )

                        # Clean up: delete the test rule
                        requests.delete(
                            f"{self.config['api_base_url']}/rules/{rule_id}",
                            timeout=self.config['timeout']
                        )
                    else:
                        self.log_result(
                            "Integration: Rule CRUD", "FAIL",
                            "Retrieved rule data doesn't match created rule",
                            "HIGH"
                        )
                        all_workflows_pass = False
                else:
                    self.log_result(
                        "Integration: Rule CRUD", "FAIL",
                        f"Failed to retrieve created rule: {get_response.status_code}",
                        "HIGH"
                    )
                    all_workflows_pass = False
            else:
                self.log_result(
                    "Integration: Rule CRUD", "FAIL",
                    f"Failed to create test rule: {create_response.status_code}",
                    "HIGH"
                )
                all_workflows_pass = False

        except Exception as e:
            self.log_result(
                "Integration: Rule CRUD", "FAIL",
                f"Rule CRUD workflow failed: {str(e)}",
                "HIGH"
            )
            all_workflows_pass = False

        # Workflow 2: Schema Validation
        try:
            schema_response = requests.get(
                f"{self.config['api_base_url']}/schema/entities",
                timeout=self.config['timeout']
            )

            if schema_response.status_code == 200:
                entities = schema_response.json()
                if len(entities) > 0:
                    self.log_result(
                        "Integration: Schema Access", "PASS",
                        f"Schema entities accessible ({len(entities)} entities)"
                    )
                else:
                    self.log_result(
                        "Integration: Schema Access", "WARN",
                        "Schema entities endpoint accessible but no entities found",
                        "MEDIUM"
                    )
            else:
                self.log_result(
                    "Integration: Schema Access", "FAIL",
                    f"Schema entities endpoint failed: {schema_response.status_code}",
                    "HIGH"
                )
                all_workflows_pass = False

        except Exception as e:
            self.log_result(
                "Integration: Schema Access", "FAIL",
                f"Schema access workflow failed: {str(e)}",
                "HIGH"
            )
            all_workflows_pass = False

        return all_workflows_pass

    def run_performance_benchmarks(self) -> bool:
        """Run performance validation benchmarks"""
        print("\n=== PERFORMANCE VALIDATION ===")

        all_benchmarks_pass = True

        # Benchmark 1: API Response Time
        try:
            start_time = time.time()
            response = requests.get(
                f"{self.config['api_base_url']}/rules?limit=10",
                timeout=self.config['timeout']
            )
            response_time = (time.time() - start_time) * 1000

            threshold = self.config['performance_thresholds']['api_response_time']
            if response_time <= threshold:
                self.log_result(
                    "Performance: API Response", "PASS",
                    f"API response time {response_time:.0f}ms <= {threshold}ms",
                    details={'response_time_ms': response_time, 'threshold_ms': threshold}
                )
            else:
                self.log_result(
                    "Performance: API Response", "FAIL",
                    f"API response time {response_time:.0f}ms > {threshold}ms",
                    "MEDIUM",
                    details={'response_time_ms': response_time, 'threshold_ms': threshold}
                )
                all_benchmarks_pass = False

        except Exception as e:
            self.log_result(
                "Performance: API Response", "FAIL",
                f"API performance test failed: {str(e)}",
                "MEDIUM"
            )
            all_benchmarks_pass = False

        # Benchmark 2: Database Query Performance
        try:
            conn = sqlite3.connect(str(self.db_path))
            cursor = conn.cursor()

            start_time = time.time()
            cursor.execute("SELECT COUNT(*) FROM rules WHERE status = 'VALID'")
            query_time = (time.time() - start_time) * 1000
            conn.close()

            if query_time <= 100:  # 100ms threshold
                self.log_result(
                    "Performance: Database Query", "PASS",
                    f"Database query time {query_time:.1f}ms <= 100ms"
                )
            else:
                self.log_result(
                    "Performance: Database Query", "WARN",
                    f"Database query time {query_time:.1f}ms > 100ms",
                    "LOW"
                )

        except Exception as e:
            self.log_result(
                "Performance: Database Query", "FAIL",
                f"Database performance test failed: {str(e)}",
                "MEDIUM"
            )
            all_benchmarks_pass = False

        return all_benchmarks_pass

    def generate_report(self, output_file: Optional[str] = None) -> Dict:
        """Generate comprehensive validation report"""
        end_time = datetime.now()
        duration = (end_time - self.start_time).total_seconds()

        # Categorize results
        passed = [r for r in self.results if r.status == "PASS"]
        failed = [r for r in self.results if r.status == "FAIL"]
        warnings = [r for r in self.results if r.status == "WARN"]
        skipped = [r for r in self.results if r.status == "SKIP"]

        # Categorize by severity
        critical = [r for r in self.results if r.severity == "CRITICAL"]
        high = [r for r in self.results if r.severity == "HIGH"]
        medium = [r for r in self.results if r.severity == "MEDIUM"]

        overall_status = "PASS"
        if critical or high:
            overall_status = "FAIL"
        elif medium or warnings:
            overall_status = "WARN"

        report = {
            'validation_summary': {
                'overall_status': overall_status,
                'start_time': self.start_time.isoformat(),
                'end_time': end_time.isoformat(),
                'duration_seconds': duration,
                'total_checks': len(self.results)
            },
            'result_counts': {
                'passed': len(passed),
                'failed': len(failed),
                'warnings': len(warnings),
                'skipped': len(skipped)
            },
            'severity_counts': {
                'critical': len(critical),
                'high': len(high),
                'medium': len(medium),
                'low': len([r for r in self.results if r.severity == "LOW"]),
                'info': len([r for r in self.results if r.severity == "INFO"])
            },
            'detailed_results': [
                {
                    'check_name': r.check_name,
                    'status': r.status,
                    'message': r.message,
                    'severity': r.severity,
                    'timestamp': r.timestamp,
                    'details': r.details
                }
                for r in self.results
            ]
        }

        if output_file:
            with open(output_file, 'w') as f:
                json.dump(report, f, indent=2)

        return report

    def run_validation(self, mode: str = "full") -> bool:
        """Run validation based on mode"""
        print(f"\n{'='*60}")
        print(f"VALIDATION AGENT - {mode.upper()} MODE")
        print(f"Started at: {self.start_time.isoformat()}")
        print(f"{'='*60}")

        success = True

        if mode in ["full", "health", "quick"]:
            if not self.validate_system_health():
                success = False
                if mode == "health":
                    return success

        if mode in ["full", "regression"]:
            if not self.validate_regression_suite():
                success = False

        if mode in ["full"]:
            if not self.validate_data_integrity():
                success = False

            if not self.validate_code_quality():
                success = False

            if not self.validate_architecture_compliance():
                success = False

            if not self.validate_integration_workflows():
                success = False

            if not self.run_performance_benchmarks():
                success = False

        # Generate and save report
        report_file = f"validation_report_{mode}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        report = self.generate_report(report_file)

        print(f"\n{'='*60}")
        print("VALIDATION SUMMARY")
        print(f"{'='*60}")
        print(f"Overall Status: {report['validation_summary']['overall_status']}")
        print(f"Duration: {report['validation_summary']['duration_seconds']:.1f}s")
        print(f"Total Checks: {report['validation_summary']['total_checks']}")
        print(f"Passed: {report['result_counts']['passed']}")
        print(f"Failed: {report['result_counts']['failed']}")
        print(f"Warnings: {report['result_counts']['warnings']}")
        print(f"Critical Issues: {report['severity_counts']['critical']}")
        print(f"High Issues: {report['severity_counts']['high']}")
        print(f"Report saved to: {report_file}")

        return success

def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description="Comprehensive Validation Agent for Rules Engine")
    parser.add_argument(
        '--mode',
        choices=['full', 'quick', 'health', 'regression'],
        default='full',
        help='Validation mode (default: full)'
    )
    parser.add_argument(
        '--config',
        help='Path to configuration file'
    )

    args = parser.parse_args()

    # Load configuration if provided
    config = None
    if args.config:
        try:
            with open(args.config, 'r') as f:
                config = json.load(f)
        except Exception as e:
            print(f"Failed to load config: {e}")
            sys.exit(1)

    # Create and run validation agent
    agent = ValidationAgent(config)
    success = agent.run_validation(args.mode)

    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()