#!/usr/bin/env python3
"""
Pre-commit regression checks
Run this before any code changes to ensure system health
"""

import subprocess
import sys
import os
from datetime import datetime

def run_command(cmd):
    """Run a command and return success status"""
    try:
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=30)
        return result.returncode == 0, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return False, "", "Command timed out"

def check_backend_health():
    """Check if backend is running and healthy"""
    print("🔍 Checking backend health...")

    # Check if Flask app is running
    success, stdout, stderr = run_command("curl -s http://localhost:5001/api/rules?limit=1")
    if not success:
        print("❌ Backend not running or not responding")
        return False

    print("✅ Backend is healthy")
    return True

def run_regression_tests():
    """Run the regression test suite"""
    print("🧪 Running regression tests...")

    success, stdout, stderr = run_command("python test_regression_suite.py")
    if not success:
        print("❌ Regression tests failed:")
        print(stderr)
        return False

    print("✅ All regression tests passed")
    return True

def check_data_backup():
    """Ensure data backup exists"""
    print("💾 Checking data backup...")

    db_path = "database/rules.db"
    backup_path = "database/rules.db.backup"

    if not os.path.exists(db_path):
        print("❌ Main database not found")
        return False

    if not os.path.exists(backup_path):
        print("⚠️  Creating database backup...")
        success, _, _ = run_command(f"cp {db_path} {backup_path}")
        if not success:
            print("❌ Failed to create backup")
            return False

    print("✅ Data backup verified")
    return True

def main():
    print("=" * 60)
    print("PRE-COMMIT REGRESSION PREVENTION CHECKS")
    print(f"Timestamp: {datetime.now().isoformat()}")
    print("=" * 60)

    checks = [
        ("Backend Health", check_backend_health),
        ("Data Backup", check_data_backup),
        ("Regression Tests", run_regression_tests)
    ]

    all_passed = True

    for check_name, check_func in checks:
        try:
            if not check_func():
                all_passed = False
        except Exception as e:
            print(f"❌ {check_name} check failed with error: {str(e)}")
            all_passed = False

    print("=" * 60)
    if all_passed:
        print("🎉 ALL PRE-COMMIT CHECKS PASSED")
        print("Safe to proceed with changes")
        sys.exit(0)
    else:
        print("🚨 SOME CHECKS FAILED")
        print("DO NOT PROCEED - Fix issues first")
        sys.exit(1)

if __name__ == "__main__":
    main()