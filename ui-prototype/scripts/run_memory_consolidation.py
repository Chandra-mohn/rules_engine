#!/usr/bin/env python3
"""
Memory Consolidation Orchestrator

Main entry point for running the complete memory consolidation system.
This script provides a unified interface for all memory management operations.

Created: September 17, 2025
Version: 1.0
Purpose: Single entry point for memory consolidation operations
"""

import os
import sys
import argparse
import datetime
import json
from pathlib import Path

# Add current directory to path
sys.path.append(os.path.dirname(__file__))

from integrated_memory_system import IntegratedMemorySystem
from memory_system_tests import run_comprehensive_test_suite

def print_banner():
    """Print system banner"""
    print("🧠 Memory Consolidation System v1.0")
    print("=" * 50)
    print("Systematic Knowledge Management & Context Preservation")
    print("for Rules Engine Project")
    print("=" * 50)
    print()

def run_pre_compaction(project_root: str):
    """Run pre-compaction preparation"""
    print("🚀 RUNNING PRE-COMPACTION PREPARATION")
    print("-" * 40)

    system = IntegratedMemorySystem(project_root)
    result = system.pre_compaction_preparation()

    if result["success"]:
        print("\n✅ PRE-COMPACTION PREPARATION COMPLETED SUCCESSFULLY")
        print(f"📋 Steps completed: {len(result['steps_completed'])}")
        print(f"📦 Files backed up: {len(result['critical_files_backed_up'])}")

        if result["warnings"]:
            print(f"⚠️ Warnings: {len(result['warnings'])}")
            for warning in result["warnings"]:
                print(f"  - {warning}")
    else:
        print("\n❌ PRE-COMPACTION PREPARATION FAILED")
        for error in result.get("errors", []):
            print(f"  - {error}")

    return result["success"]

def run_post_compaction(project_root: str):
    """Run post-compaction recovery"""
    print("🔄 RUNNING POST-COMPACTION RECOVERY")
    print("-" * 40)

    system = IntegratedMemorySystem(project_root)
    result = system.post_compaction_recovery()

    if result["success"]:
        print("\n✅ POST-COMPACTION RECOVERY COMPLETED SUCCESSFULLY")
        print(f"📋 Steps completed: {len(result['steps_completed'])}")
        print(f"🌉 Context recovered: {'Yes' if result['context_recovered'] else 'No'}")
        print(f"🔧 Services restored: {'Yes' if result['services_restored'] else 'No'}")

        if result["warnings"]:
            print(f"⚠️ Warnings: {len(result['warnings'])}")
            for warning in result["warnings"]:
                print(f"  - {warning}")
    else:
        print("\n❌ POST-COMPACTION RECOVERY FAILED")
        for error in result.get("errors", []):
            print(f"  - {error}")

    return result["success"]

def run_health_check(project_root: str):
    """Run system health check"""
    print("🏥 RUNNING SYSTEM HEALTH CHECK")
    print("-" * 40)

    system = IntegratedMemorySystem(project_root)
    health = system.run_health_check()

    print(f"\nOverall Health: {health['overall_health'].upper()}")
    print("\nComponent Status:")

    for component, status in health["component_health"].items():
        status_icon = "✅" if status == "healthy" else "⚠️" if "issues" in status else "❌"
        component_name = component.replace("_", " ").title()
        print(f"  {status_icon} {component_name}: {status}")

    if health["recommendations"]:
        print("\nRecommendations:")
        for rec in health["recommendations"]:
            print(f"  💡 {rec}")

    return health["overall_health"] in ["healthy", "degraded"]

def run_tests(project_root: str):
    """Run comprehensive test suite"""
    print("🧪 RUNNING COMPREHENSIVE TEST SUITE")
    print("-" * 40)

    success = run_comprehensive_test_suite()
    return success

def run_interactive_mode(project_root: str):
    """Run interactive mode"""
    print("🔧 INTERACTIVE MEMORY CONSOLIDATION MODE")
    print("-" * 40)

    while True:
        print("\nAvailable operations:")
        print("1. Pre-compaction preparation")
        print("2. Post-compaction recovery")
        print("3. Health check")
        print("4. Run tests")
        print("5. Generate memory report")
        print("6. Exit")

        choice = input("\nSelect operation (1-6): ").strip()

        if choice == "1":
            run_pre_compaction(project_root)
        elif choice == "2":
            run_post_compaction(project_root)
        elif choice == "3":
            run_health_check(project_root)
        elif choice == "4":
            run_tests(project_root)
        elif choice == "5":
            generate_memory_report(project_root)
        elif choice == "6":
            print("👋 Goodbye!")
            break
        else:
            print("❌ Invalid choice. Please select 1-6.")

def generate_memory_report(project_root: str):
    """Generate comprehensive memory report"""
    print("📊 GENERATING MEMORY REPORT")
    print("-" * 40)

    from memory_consolidation_agent import MemoryConsolidationAgent

    agent = MemoryConsolidationAgent(project_root)
    report = agent.generate_memory_report()

    report_file = os.path.join(project_root, f"memory_report_{datetime.datetime.now().strftime('%Y%m%d_%H%M%S')}.md")

    with open(report_file, 'w') as f:
        f.write(report)

    print(f"\n📄 Memory report generated: {report_file}")
    print("\nReport summary:")
    lines = report.split('\n')
    for line in lines[:20]:  # Show first 20 lines
        if line.strip():
            print(f"  {line}")

    return report_file

def verify_environment(project_root: str) -> bool:
    """Verify environment is suitable for memory operations"""
    print("🔍 VERIFYING ENVIRONMENT")
    print("-" * 40)

    issues = []

    # Check working directory
    expected_dir = "/Users/chandramohn/workspace/rules_engine/ui-prototype"
    if project_root != expected_dir:
        print(f"⚠️ Non-standard project root: {project_root}")

    # Check critical files
    critical_files = [
        "CLAUDE.md",
        "backend/models.py",
        "frontend/src/components/RuleEditor.jsx"
    ]

    for file_path in critical_files:
        full_path = os.path.join(project_root, file_path)
        if os.path.exists(full_path):
            print(f"✅ {file_path}")
        else:
            print(f"❌ Missing: {file_path}")
            issues.append(f"Missing critical file: {file_path}")

    # Check git repository
    git_dir = os.path.join(project_root, ".git")
    if os.path.exists(git_dir):
        print("✅ Git repository")
    else:
        print("❌ Git repository not found")
        issues.append("Git repository not found")

    # Check Python dependencies
    try:
        import requests
        print("✅ Python requests library")
    except ImportError:
        print("⚠️ Python requests library not available")

    if issues:
        print(f"\n⚠️ Environment issues detected ({len(issues)}):")
        for issue in issues:
            print(f"  - {issue}")
        return False
    else:
        print("\n✅ Environment verification passed")
        return True

def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="Memory Consolidation System for Rules Engine Project",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s --mode pre-compaction          # Prepare for auto-compaction
  %(prog)s --mode post-compaction         # Recover after auto-compaction
  %(prog)s --mode health-check            # Check system health
  %(prog)s --mode test                    # Run test suite
  %(prog)s --mode interactive             # Interactive mode
  %(prog)s --mode report                  # Generate memory report
        """
    )

    parser.add_argument(
        "--mode",
        choices=["pre-compaction", "post-compaction", "health-check", "test", "interactive", "report"],
        default="interactive",
        help="Operation mode (default: interactive)"
    )

    parser.add_argument(
        "--project-root",
        default="/Users/chandramohn/workspace/rules_engine/ui-prototype",
        help="Project root directory"
    )

    parser.add_argument(
        "--skip-verification",
        action="store_true",
        help="Skip environment verification"
    )

    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Enable verbose output"
    )

    args = parser.parse_args()

    # Print banner
    print_banner()

    # Verify environment unless skipped
    if not args.skip_verification:
        if not verify_environment(args.project_root):
            print("\n❌ Environment verification failed.")
            print("Use --skip-verification to proceed anyway, or fix the issues above.")
            return 1

    print()  # Add spacing

    # Execute requested operation
    success = True

    try:
        if args.mode == "pre-compaction":
            success = run_pre_compaction(args.project_root)

        elif args.mode == "post-compaction":
            success = run_post_compaction(args.project_root)

        elif args.mode == "health-check":
            success = run_health_check(args.project_root)

        elif args.mode == "test":
            success = run_tests(args.project_root)

        elif args.mode == "report":
            generate_memory_report(args.project_root)

        elif args.mode == "interactive":
            run_interactive_mode(args.project_root)

    except KeyboardInterrupt:
        print("\n\n⏹️ Operation cancelled by user")
        return 1

    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1

    # Final status
    if args.mode != "interactive":
        print("\n" + "=" * 50)
        if success:
            print("✅ Operation completed successfully")
            print("Memory consolidation system is ready for use")
        else:
            print("❌ Operation completed with issues")
            print("Review the output above for details")

    return 0 if success else 1

if __name__ == "__main__":
    exit(main())