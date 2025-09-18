#!/usr/bin/env python3
"""
Validation Agent System Demo
Demonstrates the comprehensive validation system capabilities.

This script runs a subset of validation checks to showcase the system.
"""

import os
import sys
import time
import subprocess
from datetime import datetime
from pathlib import Path

def print_banner(title):
    """Print a formatted banner"""
    print(f"\n{'='*60}")
    print(f"{title:^60}")
    print(f"{'='*60}")

def print_section(title):
    """Print a section header"""
    print(f"\n{'-'*40}")
    print(f"{title}")
    print(f"{'-'*40}")

def run_command(cmd, description, timeout=60):
    """Run a command and show results"""
    print(f"\n🔍 {description}")
    print(f"Command: {cmd}")
    print("Running...", end="", flush=True)

    start_time = time.time()

    try:
        result = subprocess.run(
            cmd, shell=True, capture_output=True, text=True, timeout=timeout
        )
        duration = time.time() - start_time

        if result.returncode == 0:
            print(f" ✅ PASSED ({duration:.1f}s)")
            return True
        else:
            print(f" ❌ FAILED ({duration:.1f}s)")
            if result.stderr:
                print(f"Error: {result.stderr[:200]}...")
            return False
    except subprocess.TimeoutExpired:
        print(f" ⏰ TIMEOUT ({timeout}s)")
        return False
    except Exception as e:
        print(f" ❌ ERROR: {str(e)}")
        return False

def check_prerequisites():
    """Check if system is ready for validation"""
    print_section("Checking Prerequisites")

    checks = [
        ("python --version", "Python installation"),
        ("curl --version", "Curl installation"),
        ("ls backend/database/rules.db", "Database file exists"),
    ]

    all_good = True
    for cmd, desc in checks:
        if not run_command(cmd, desc, timeout=10):
            all_good = False

    return all_good

def demo_system_health():
    """Demonstrate system health validation"""
    print_section("System Health Validation Demo")

    # Check if backend is running
    backend_running = run_command(
        "curl -s http://localhost:5001/api/health",
        "Backend API Health Check",
        timeout=10
    )

    if not backend_running:
        print("\n⚠️  Backend not running. Starting backend for demo...")
        # Try to start backend in background
        try:
            subprocess.Popen(
                ["python", "app.py"],
                cwd="backend",
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )
            print("Backend started. Waiting 5 seconds...")
            time.sleep(5)

            # Try health check again
            backend_running = run_command(
                "curl -s http://localhost:5001/api/health",
                "Backend API Health Check (retry)",
                timeout=10
            )
        except Exception as e:
            print(f"Could not start backend: {e}")

    # Run health mode validation
    run_command(
        "python validation_agent.py --mode health",
        "Comprehensive Health Validation",
        timeout=120
    )

def demo_code_quality():
    """Demonstrate code quality validation"""
    print_section("Code Quality Validation Demo")

    run_command(
        "python code_quality_validator.py",
        "Code Quality Analysis",
        timeout=60
    )

def demo_data_integrity():
    """Demonstrate data integrity monitoring"""
    print_section("Data Integrity Monitoring Demo")

    run_command(
        "cd backend && python data_integrity_monitor.py",
        "Data Integrity Validation",
        timeout=90
    )

def demo_regression_testing():
    """Demonstrate regression testing"""
    print_section("Regression Testing Demo")

    # Run basic regression suite first
    run_command(
        "cd backend && python test_regression_suite.py",
        "Basic Regression Test Suite",
        timeout=120
    )

def demo_integration_testing():
    """Demonstrate integration testing"""
    print_section("Integration Testing Demo")

    # Run connectivity tests only (faster)
    run_command(
        "python integration_test_framework.py --categories connectivity",
        "Integration Connectivity Tests",
        timeout=60
    )

def show_reports():
    """Show generated reports"""
    print_section("Generated Reports")

    # Find recent report files
    report_patterns = [
        "*report*.json",
        "*results*.json"
    ]

    reports_found = []
    for pattern in report_patterns:
        for report_file in Path(".").rglob(pattern):
            if report_file.stat().st_mtime > time.time() - 3600:  # Last hour
                reports_found.append(report_file)

    if reports_found:
        print("Recent validation reports:")
        for report in sorted(reports_found, key=lambda x: x.stat().st_mtime, reverse=True)[:5]:
            size_kb = report.stat().st_size / 1024
            mtime = datetime.fromtimestamp(report.stat().st_mtime).strftime("%H:%M:%S")
            print(f"  📄 {report} ({size_kb:.1f}KB, {mtime})")
    else:
        print("No recent reports found.")

def cleanup_demo():
    """Cleanup any demo artifacts"""
    print_section("Cleanup")

    # Remove any test rules created during demo
    run_command(
        "curl -s -X DELETE http://localhost:5001/api/rules/999999 || true",
        "Cleanup test data",
        timeout=10
    )

def main():
    """Main demo function"""
    print_banner("VALIDATION AGENT SYSTEM DEMO")
    print(f"Demo started at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("\nThis demo showcases the comprehensive validation system capabilities.")
    print("Note: Some tests may fail if services are not running - this is expected.")

    # Change to project root
    os.chdir(Path(__file__).parent)

    demos = [
        ("Prerequisites Check", check_prerequisites),
        ("System Health", demo_system_health),
        ("Code Quality", demo_code_quality),
        ("Data Integrity", demo_data_integrity),
        ("Regression Testing", demo_regression_testing),
        ("Integration Testing", demo_integration_testing),
    ]

    results = {}
    total_start = time.time()

    for demo_name, demo_func in demos:
        print_banner(f"DEMO: {demo_name}")
        start_time = time.time()

        try:
            result = demo_func()
            results[demo_name] = result
        except Exception as e:
            print(f"❌ Demo failed with error: {str(e)}")
            results[demo_name] = False

        duration = time.time() - start_time
        print(f"\nDemo completed in {duration:.1f} seconds")

    # Show results summary
    total_duration = time.time() - total_start

    print_banner("DEMO SUMMARY")
    print(f"Total demo duration: {total_duration:.1f} seconds")
    print(f"Demos completed: {len(results)}")

    for demo_name, result in results.items():
        status = "✅ PASSED" if result else "❌ FAILED"
        print(f"  {status} {demo_name}")

    # Show reports
    show_reports()

    # Cleanup
    cleanup_demo()

    print_banner("DEMO COMPLETE")
    print("Thank you for trying the Validation Agent System!")
    print("\nTo run individual components:")
    print("  python validation_agent.py --mode full")
    print("  python code_quality_validator.py")
    print("  python integration_test_framework.py")
    print("\nFor more information, see VALIDATION_AGENT_GUIDE.md")

if __name__ == "__main__":
    main()