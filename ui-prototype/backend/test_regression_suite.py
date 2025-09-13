#!/usr/bin/env python3
"""
Comprehensive Regression Testing Suite
Prevents data loss and field corruption issues
"""

import requests
import json
import sys
from datetime import datetime
import sqlite3
import os

class RegressionTestSuite:
    def __init__(self, api_base="http://localhost:5001/api", db_path="database/rules.db"):
        self.api_base = api_base
        self.db_path = db_path
        self.issues = []

    def log_issue(self, severity, message):
        """Log a regression issue"""
        issue = {
            'timestamp': datetime.now().isoformat(),
            'severity': severity,
            'message': message
        }
        self.issues.append(issue)
        print(f"[{severity}] {message}")

    def test_api_availability(self):
        """Test if API is responding"""
        try:
            response = requests.get(f"{self.api_base}/rules?limit=1", timeout=5)
            if response.status_code != 200:
                self.log_issue('CRITICAL', f'API not responding: {response.status_code}')
                return False
            return True
        except Exception as e:
            self.log_issue('CRITICAL', f'API connection failed: {str(e)}')
            return False

    def test_data_integrity(self):
        """Test core data integrity"""
        print("=== Testing Data Integrity ===")

        try:
            response = requests.get(f"{self.api_base}/rules")
            data = response.json()

            # Test 1: Check if we have expected number of core rules (should be >= 8 active)
            active_rules = [r for r in data['rules'] if r['status'] not in ['deleted']]
            if len(active_rules) < 8:
                self.log_issue('HIGH', f'Data loss detected: Only {len(active_rules)} active rules (expected >= 8)')

            # Test 2: Check required fields exist
            required_fields = ['id', 'name', 'status', 'schema_version', 'effective_date']
            for rule in data['rules'][:3]:  # Sample check
                for field in required_fields:
                    if field not in rule:
                        self.log_issue('HIGH', f'Missing field {field} in rule {rule.get("name", "unknown")}')

            # Test 3: Check status field values are correct format
            valid_statuses = {'DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD', 'deleted'}
            for rule in data['rules']:
                if rule['status'] not in valid_statuses:
                    self.log_issue('MEDIUM', f'Invalid status "{rule["status"]}" in rule {rule["name"]}')

            # Test 4: Check for lowercase regression in status
            for rule in data['rules']:
                if rule['status'] in ['active', 'draft', 'inactive']:
                    self.log_issue('HIGH', f'Status regression: lowercase "{rule["status"]}" in rule {rule["name"]}')

            # Test 5: Check effective_date field not null for active rules
            for rule in active_rules:
                if not rule.get('effective_date'):
                    self.log_issue('MEDIUM', f'Missing effective_date for active rule {rule["name"]}')

        except Exception as e:
            self.log_issue('CRITICAL', f'Data integrity test failed: {str(e)}')

    def test_schema_consistency(self):
        """Test database schema consistency"""
        print("=== Testing Schema Consistency ===")

        if not os.path.exists(self.db_path):
            self.log_issue('CRITICAL', f'Database file not found: {self.db_path}')
            return

        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            # Test 1: Check if validation_status column was properly removed
            cursor.execute("PRAGMA table_info(rules)")
            columns = [col[1] for col in cursor.fetchall()]

            if 'validation_status' in columns:
                self.log_issue('HIGH', 'Schema regression: validation_status column still exists')

            # Test 2: Check required columns exist
            required_columns = ['id', 'name', 'status', 'effective_date', 'schema_version']
            for col in required_columns:
                if col not in columns:
                    self.log_issue('CRITICAL', f'Missing required column: {col}')

            # Test 3: Check for orphaned data
            cursor.execute("SELECT COUNT(*) FROM rules WHERE process_area_id IS NULL")
            orphaned = cursor.fetchone()[0]
            if orphaned > 0:
                self.log_issue('MEDIUM', f'{orphaned} rules have null process_area_id (orphaned data)')

            conn.close()

        except Exception as e:
            self.log_issue('CRITICAL', f'Schema consistency test failed: {str(e)}')

    def test_api_contracts(self):
        """Test API contract consistency"""
        print("=== Testing API Contracts ===")

        try:
            # Test rules endpoint structure
            response = requests.get(f"{self.api_base}/rules?limit=1")
            data = response.json()

            expected_structure = {
                'rules': list,
                'total': int,
                'page': int,
                'limit': int
            }

            for key, expected_type in expected_structure.items():
                if key not in data:
                    self.log_issue('HIGH', f'API contract broken: missing {key} in rules response')
                elif not isinstance(data[key], expected_type):
                    self.log_issue('MEDIUM', f'API contract changed: {key} type is {type(data[key])}, expected {expected_type}')

            # Test individual rule structure
            if data['rules']:
                rule = data['rules'][0]
                expected_rule_fields = ['id', 'name', 'status', 'content', 'schema_version']
                for field in expected_rule_fields:
                    if field not in rule:
                        self.log_issue('HIGH', f'Rule API contract broken: missing {field}')

                # Test that validation_status field is NOT returned (regression check)
                if 'validation_status' in rule:
                    self.log_issue('HIGH', 'API regression: validation_status field returned (should be removed)')

            # Test status update functionality (regression check)
            if data['rules']:
                test_rule = data['rules'][0]
                original_status = test_rule['status']
                test_status = 'DRAFT' if original_status != 'DRAFT' else 'VALID'

                try:
                    # Test explicit status update
                    update_response = requests.put(
                        f"{self.api_base}/rules/{test_rule['id']}",
                        json={'status': test_status}
                    )

                    if update_response.status_code == 200:
                        updated_rule = update_response.json()
                        if updated_rule['status'] != test_status:
                            self.log_issue('HIGH', f'Status update regression: sent {test_status}, got {updated_rule["status"]}')

                        # Test auto-promotion from DRAFT to VALID (regression check)
                        if test_status == 'DRAFT':
                            # Save without explicit status - should auto-promote to VALID if valid
                            auto_promo_response = requests.put(
                                f"{self.api_base}/rules/{test_rule['id']}",
                                json={'description': 'Test auto-promotion'}
                            )

                            if auto_promo_response.status_code == 200:
                                auto_promo_rule = auto_promo_response.json()
                                validation = auto_promo_rule.get('validation', {})
                                if validation.get('valid') and auto_promo_rule['status'] != 'VALID':
                                    self.log_issue('HIGH', f'Auto-promotion regression: DRAFT rule with valid syntax should auto-promote to VALID, got {auto_promo_rule["status"]}')

                        # Restore original status
                        requests.put(
                            f"{self.api_base}/rules/{test_rule['id']}",
                            json={'status': original_status}
                        )
                    else:
                        self.log_issue('MEDIUM', f'Status update failed with status {update_response.status_code}')

                except Exception as update_error:
                    self.log_issue('MEDIUM', f'Status update test failed: {str(update_error)}')

        except Exception as e:
            self.log_issue('CRITICAL', f'API contract test failed: {str(e)}')

    def test_frontend_component_consistency(self):
        """Test frontend component consistency"""
        print("=== Testing Frontend Component Consistency ===")

        frontend_path = "/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/components"

        try:
            # Check RuleEditor.jsx for correct status options
            rule_editor_path = f"{frontend_path}/RuleEditor.jsx"
            if os.path.exists(rule_editor_path):
                with open(rule_editor_path, 'r') as f:
                    content = f.read()

                    # Check for old lowercase status values (regression indicators)
                    old_statuses = ['value="draft"', 'value="active"', 'value="inactive"']
                    for old_status in old_statuses:
                        if old_status in content:
                            self.log_issue('HIGH', f'RuleEditor regression: Found old status value {old_status}')

                    # Check for correct uppercase status values
                    correct_statuses = ['value="DRAFT"', 'value="VALID"', 'value="PEND"', 'value="SCHD"', 'value="PROD"']
                    missing_statuses = []
                    for correct_status in correct_statuses:
                        if correct_status not in content:
                            missing_statuses.append(correct_status)

                    if missing_statuses:
                        self.log_issue('MEDIUM', f'RuleEditor missing status values: {missing_statuses}')

                    # Check Execute/Generate buttons are enabled for VALID+ statuses (regression check)
                    if 'isExecutableStatus' not in content:
                        self.log_issue('MEDIUM', 'RuleEditor missing isExecutableStatus helper function')

                    # Check for old PROD-only restrictions (regression indicators)
                    prod_only_restrictions = ['rule.status !== \'PROD\'', 'status === \'PROD\'']
                    for restriction in prod_only_restrictions:
                        if restriction in content:
                            self.log_issue('HIGH', f'RuleEditor regression: Found old PROD-only restriction: {restriction}')

        except Exception as e:
            self.log_issue('MEDIUM', f'Frontend consistency test failed: {str(e)}')

    def run_all_tests(self):
        """Run complete regression test suite"""
        print("=" * 50)
        print("REGRESSION TEST SUITE - STARTING")
        print("=" * 50)

        if not self.test_api_availability():
            print("CRITICAL: API not available, stopping tests")
            return False

        self.test_data_integrity()
        self.test_schema_consistency()
        self.test_api_contracts()
        self.test_frontend_component_consistency()

        print("=" * 50)
        print("REGRESSION TEST SUITE - RESULTS")
        print("=" * 50)

        if not self.issues:
            print("✅ ALL TESTS PASSED - No regression issues detected")
            return True
        else:
            print(f"❌ {len(self.issues)} ISSUES DETECTED:")

            # Group by severity
            critical = [i for i in self.issues if i['severity'] == 'CRITICAL']
            high = [i for i in self.issues if i['severity'] == 'HIGH']
            medium = [i for i in self.issues if i['severity'] == 'MEDIUM']

            print(f"  CRITICAL: {len(critical)}")
            print(f"  HIGH: {len(high)}")
            print(f"  MEDIUM: {len(medium)}")

            return len(critical) == 0 and len(high) == 0

if __name__ == "__main__":
    # Change to backend directory
    os.chdir('/Users/chandramohn/workspace/rules_engine/ui-prototype/backend')

    suite = RegressionTestSuite()
    success = suite.run_all_tests()

    # Save results
    with open('regression_test_results.json', 'w') as f:
        json.dump({
            'timestamp': datetime.now().isoformat(),
            'success': success,
            'issues': suite.issues
        }, f, indent=2)

    sys.exit(0 if success else 1)