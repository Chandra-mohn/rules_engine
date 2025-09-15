#!/usr/bin/env python3
"""
System validation script for code reduction safety
Run after each change to ensure system integrity
"""

import subprocess
import sys
import time
import json
from pathlib import Path

def validate_imports():
    """Test all critical imports work"""
    print("🔍 Validating imports...")
    try:
        import app
        from services.rule_service import RuleService
        from services.list_cache import ListService
        from models import Rule, db, Client, ProcessGroup, ProcessArea
        from api.rules import rules_bp
        from api.hierarchy import hierarchy_bp
        from api.schema import schema_bp
        from api.lists import lists_bp
        print("✅ All imports successful")
        return True
    except ImportError as e:
        print(f"❌ Import error: {e}")
        return False

def validate_syntax():
    """Test all Python files have valid syntax"""
    print("🔍 Validating Python syntax...")
    try:
        result = subprocess.run(['python', '-m', 'py_compile'] + list(Path('.').glob('*.py')),
                              capture_output=True, text=True)
        if result.returncode != 0:
            print(f"❌ Syntax error: {result.stderr}")
            return False

        # Check services and api directories
        for directory in ['services', 'api']:
            for py_file in Path(directory).glob('*.py'):
                result = subprocess.run(['python', '-m', 'py_compile', str(py_file)],
                                      capture_output=True, text=True)
                if result.returncode != 0:
                    print(f"❌ Syntax error in {py_file}: {result.stderr}")
                    return False

        print("✅ All Python syntax valid")
        return True
    except Exception as e:
        print(f"❌ Syntax validation error: {e}")
        return False

def validate_core_functionality():
    """Test core rule operations"""
    print("🔍 Validating core functionality...")
    try:
        from services.rule_service import RuleService
        from services.list_cache import ListService

        # Test RuleService instantiation
        rs = RuleService()

        # Test validation method exists and works
        test_rule = "rule test: if amount > 100 then APPROVE"
        result = rs.validate_rule_content(test_rule)
        if not isinstance(result, dict) or 'valid' not in result:
            print("❌ Validation method broken - wrong return type")
            return False

        # Test autocomplete
        suggestions = rs.get_autocomplete_suggestions("if applicant.", 12)
        if not isinstance(suggestions, dict):
            print("❌ Autocomplete method broken")
            return False

        # Test ListService
        ls = ListService()
        lists = ls.get_all_lists()
        if not isinstance(lists, dict):
            print("❌ ListService broken")
            return False

        print("✅ Core functionality working")
        return True
    except Exception as e:
        print(f"❌ Core functionality error: {e}")
        return False

def validate_file_references():
    """Check for broken file references"""
    print("🔍 Checking for broken file references...")
    try:
        # Files we plan to remove - check they're not imported
        removed_files = ['debug_hot_compilation', 'migrate_actionsets']

        for removed_file in removed_files:
            # Check if file still exists
            if Path(f"{removed_file}.py").exists():
                continue  # Not removed yet, skip check

            # Check for imports of removed file
            result = subprocess.run(['grep', '-r', f'import {removed_file}', '.'],
                                  capture_output=True, text=True)
            if result.returncode == 0 and result.stdout.strip():
                print(f"❌ Found imports of removed file {removed_file}:")
                print(result.stdout)
                return False

            result = subprocess.run(['grep', '-r', f'from {removed_file}', '.'],
                                  capture_output=True, text=True)
            if result.returncode == 0 and result.stdout.strip():
                print(f"❌ Found imports from removed file {removed_file}:")
                print(result.stdout)
                return False

        print("✅ No broken file references found")
        return True
    except Exception as e:
        print(f"❌ File reference check error: {e}")
        return False

def create_baseline():
    """Create baseline test results for comparison"""
    print("📊 Creating baseline...")
    try:
        baseline = {
            'timestamp': time.time(),
            'imports_ok': validate_imports(),
            'syntax_ok': validate_syntax(),
            'core_functionality_ok': validate_core_functionality(),
            'file_references_ok': validate_file_references()
        }

        with open('validation_baseline.json', 'w') as f:
            json.dump(baseline, f, indent=2)

        print("✅ Baseline created")
        return baseline
    except Exception as e:
        print(f"❌ Baseline creation error: {e}")
        return None

def main():
    """Main validation routine"""
    print("🛡️  System Validation Starting...")
    print("=" * 50)

    # Create baseline if it doesn't exist
    baseline_file = Path('validation_baseline.json')
    if not baseline_file.exists():
        print("📊 No baseline found, creating one...")
        baseline = create_baseline()
        if not baseline:
            print("❌ Failed to create baseline")
            sys.exit(1)
        print("✅ Baseline created successfully")
        return

    # Run validation checks
    checks = [
        ('Imports', validate_imports),
        ('Syntax', validate_syntax),
        ('Core Functionality', validate_core_functionality),
        ('File References', validate_file_references)
    ]

    all_passed = True
    for name, check_func in checks:
        if not check_func():
            all_passed = False
            break

    if all_passed:
        print("=" * 50)
        print("🎉 All validations passed!")
        print("✅ System is stable after changes")
    else:
        print("=" * 50)
        print("❌ Validation failed!")
        print("🚨 System may be unstable - consider rollback")
        sys.exit(1)

if __name__ == "__main__":
    main()