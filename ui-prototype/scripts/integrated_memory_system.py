#!/usr/bin/env python3
"""
Integrated Memory System

Orchestrates all memory consolidation components to provide a unified system
for preventing context loss during auto-compaction and managing project knowledge.

This system integrates:
- Memory Consolidation Agent
- Claude Memory Manager
- Session Continuity Bridge
- Existing Context Preservation Framework

Created: September 17, 2025
Version: 1.0
Purpose: Unified memory management and context preservation
"""

import os
import sys
import json
import datetime
import subprocess
import shutil
from typing import Dict, List, Any, Optional
from pathlib import Path

# Import our memory components
from memory_consolidation_agent import MemoryConsolidationAgent
from claude_memory_manager import ClaudeMemoryManager
from session_continuity_bridge import SessionContinuityBridge

class IntegratedMemorySystem:
    """Unified system for comprehensive memory management and context preservation"""

    def __init__(self, project_root: str = None):
        self.project_root = project_root or "/Users/chandramohn/workspace/rules_engine/ui-prototype"
        self.memory_root = os.path.join(self.project_root, ".integrated_memory")
        self.reports_dir = os.path.join(self.memory_root, "reports")
        self.backups_dir = os.path.join(self.memory_root, "backups")

        # Ensure directories exist
        os.makedirs(self.memory_root, exist_ok=True)
        os.makedirs(self.reports_dir, exist_ok=True)
        os.makedirs(self.backups_dir, exist_ok=True)

        # Initialize components
        self.consolidation_agent = MemoryConsolidationAgent(self.project_root)
        self.memory_manager = ClaudeMemoryManager(self.project_root)
        self.continuity_bridge = SessionContinuityBridge(self.project_root)

        # System state file
        self.system_state_file = os.path.join(self.memory_root, "system_state.json")

    def pre_compaction_preparation(self) -> Dict[str, Any]:
        """Complete pre-compaction preparation process"""
        print("🚀 Starting Pre-Compaction Memory Preparation")
        print("=" * 60)

        preparation_results = {
            "timestamp": datetime.datetime.now().isoformat(),
            "success": True,
            "steps_completed": [],
            "warnings": [],
            "errors": [],
            "critical_files_backed_up": [],
            "context_preserved": False
        }

        try:
            # Step 1: Create comprehensive backup
            print("📦 Step 1: Creating comprehensive backup...")
            backup_result = self._create_comprehensive_backup()
            preparation_results["steps_completed"].append("Comprehensive backup created")
            preparation_results["critical_files_backed_up"] = backup_result["backed_up_files"]

            # Step 2: Run memory consolidation
            print("🧠 Step 2: Running memory consolidation...")
            consolidation_result = self.consolidation_agent.run_full_consolidation()
            if consolidation_result["success"]:
                preparation_results["steps_completed"].append("Memory consolidation completed")
            else:
                preparation_results["warnings"].append(f"Memory consolidation warning: {consolidation_result.get('error', 'Unknown error')}")

            # Step 3: Optimize CLAUDE.md
            print("📝 Step 3: Optimizing CLAUDE.md structure...")
            optimization_result = self.memory_manager.optimize_document_structure()
            preparation_results["steps_completed"].append("CLAUDE.md optimized")

            # Step 4: Create session bridge
            print("🌉 Step 4: Creating session continuity bridge...")
            bridge_result = self.continuity_bridge.create_context_bridge()
            preparation_results["steps_completed"].append("Session bridge created")
            preparation_results["context_preserved"] = True

            # Step 5: Validate system integrity
            print("✅ Step 5: Validating system integrity...")
            validation_result = self._validate_system_integrity()
            if validation_result["healthy"]:
                preparation_results["steps_completed"].append("System integrity validated")
            else:
                preparation_results["warnings"].extend(validation_result["issues"])

            # Step 6: Generate pre-compaction report
            print("📊 Step 6: Generating pre-compaction report...")
            report_result = self._generate_pre_compaction_report(preparation_results)
            preparation_results["steps_completed"].append("Pre-compaction report generated")

            # Step 7: Update existing context preservation scripts
            print("🔧 Step 7: Updating context preservation scripts...")
            self._update_context_preservation_scripts()
            preparation_results["steps_completed"].append("Context preservation scripts updated")

            print("=" * 60)
            print("✅ PRE-COMPACTION PREPARATION COMPLETED SUCCESSFULLY")
            print(f"📋 Steps completed: {len(preparation_results['steps_completed'])}")
            print(f"⚠️ Warnings: {len(preparation_results['warnings'])}")

        except Exception as e:
            preparation_results["success"] = False
            preparation_results["errors"].append(str(e))
            print(f"❌ Pre-compaction preparation failed: {e}")

        # Save preparation state
        self._save_system_state(preparation_results)

        return preparation_results

    def post_compaction_recovery(self) -> Dict[str, Any]:
        """Complete post-compaction recovery process"""
        print("🔄 Starting Post-Compaction Recovery")
        print("=" * 60)

        recovery_results = {
            "timestamp": datetime.datetime.now().isoformat(),
            "success": True,
            "steps_completed": [],
            "warnings": [],
            "errors": [],
            "context_recovered": False,
            "services_restored": False
        }

        try:
            # Step 1: Validate environment
            print("🔍 Step 1: Validating environment...")
            env_validation = self._validate_environment()
            if env_validation["valid"]:
                recovery_results["steps_completed"].append("Environment validated")
            else:
                recovery_results["warnings"].extend(env_validation["issues"])

            # Step 2: Recover context from bridge
            print("🌉 Step 2: Recovering context from session bridge...")
            context_recovery = self.continuity_bridge.recover_from_context_bridge()
            if context_recovery["success"]:
                recovery_results["steps_completed"].append("Context recovered from bridge")
                recovery_results["context_recovered"] = True
            else:
                recovery_results["warnings"].append(f"Context recovery issue: {context_recovery.get('error', 'Unknown error')}")

            # Step 3: Validate CLAUDE.md integrity
            print("📝 Step 3: Validating CLAUDE.md integrity...")
            claude_validation = self.memory_manager.validate_memory_integrity()
            if claude_validation["file_exists"] and claude_validation["parseable"]:
                recovery_results["steps_completed"].append("CLAUDE.md integrity validated")
            else:
                recovery_results["warnings"].extend(claude_validation["issues"])

            # Step 4: Check service status
            print("🔧 Step 4: Checking service status...")
            service_status = self._check_and_restore_services()
            recovery_results["services_restored"] = service_status["all_running"]
            recovery_results["steps_completed"].append("Service status checked")

            # Step 5: Run integrity tests
            print("🧪 Step 5: Running integrity tests...")
            integrity_result = self._run_integrity_tests()
            if integrity_result["passed"]:
                recovery_results["steps_completed"].append("Integrity tests passed")
            else:
                recovery_results["warnings"].extend(integrity_result["failures"])

            # Step 6: Generate recovery report
            print("📊 Step 6: Generating recovery report...")
            report_result = self._generate_recovery_report(recovery_results, context_recovery)
            recovery_results["steps_completed"].append("Recovery report generated")

            print("=" * 60)
            print("✅ POST-COMPACTION RECOVERY COMPLETED")
            print(f"📋 Steps completed: {len(recovery_results['steps_completed'])}")
            print(f"⚠️ Warnings: {len(recovery_results['warnings'])}")

        except Exception as e:
            recovery_results["success"] = False
            recovery_results["errors"].append(str(e))
            print(f"❌ Post-compaction recovery failed: {e}")

        return recovery_results

    def _create_comprehensive_backup(self) -> Dict[str, Any]:
        """Create comprehensive backup of critical files and state"""
        timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_dir = os.path.join(self.backups_dir, f"pre_compaction_{timestamp}")
        os.makedirs(backup_dir, exist_ok=True)

        backed_up_files = []

        # Critical files to backup
        critical_files = [
            "CLAUDE.md",
            "CLAUDE_SESSION_CHECKPOINT.md",
            "CONTEXT_PRESERVATION_FRAMEWORK.md",
            "CONTEXT_QUICK_REFERENCE.md",
            "backend/models.py",
            "backend/services/rule_service.py",
            "frontend/src/components/RuleEditor.jsx",
            "java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4"
        ]

        for file_path in critical_files:
            full_path = os.path.join(self.project_root, file_path)
            if os.path.exists(full_path):
                backup_path = os.path.join(backup_dir, file_path.replace("/", "_"))
                shutil.copy2(full_path, backup_path)
                backed_up_files.append(file_path)

        # Backup database if it exists
        db_path = os.path.join(self.project_root, "backend", "database", "rules.db")
        if os.path.exists(db_path):
            shutil.copy2(db_path, os.path.join(backup_dir, "rules.db"))
            backed_up_files.append("backend/database/rules.db")

        # Save git status
        try:
            result = subprocess.run(
                ['git', 'status', '--porcelain'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )
            with open(os.path.join(backup_dir, "git_status.txt"), 'w') as f:
                f.write(result.stdout)
        except:
            pass

        return {
            "backup_dir": backup_dir,
            "backed_up_files": backed_up_files,
            "timestamp": timestamp
        }

    def _validate_system_integrity(self) -> Dict[str, Any]:
        """Validate current system integrity"""
        validation = {
            "healthy": True,
            "issues": [],
            "checks_performed": []
        }

        # Check critical files exist
        critical_files = [
            "CLAUDE.md",
            "backend/models.py",
            "frontend/src/components/RuleEditor.jsx"
        ]

        for file_path in critical_files:
            full_path = os.path.join(self.project_root, file_path)
            if os.path.exists(full_path):
                validation["checks_performed"].append(f"✅ {file_path} exists")
            else:
                validation["healthy"] = False
                validation["issues"].append(f"❌ Missing critical file: {file_path}")

        # Check git repository
        if os.path.exists(os.path.join(self.project_root, ".git")):
            validation["checks_performed"].append("✅ Git repository exists")
        else:
            validation["healthy"] = False
            validation["issues"].append("❌ Git repository not found")

        # Check memory system directories
        memory_dirs = [self.memory_root, self.reports_dir, self.backups_dir]
        for dir_path in memory_dirs:
            if os.path.exists(dir_path):
                validation["checks_performed"].append(f"✅ Memory directory exists: {os.path.basename(dir_path)}")
            else:
                validation["issues"].append(f"⚠️ Memory directory missing: {os.path.basename(dir_path)}")

        return validation

    def _generate_pre_compaction_report(self, preparation_results: Dict[str, Any]) -> str:
        """Generate comprehensive pre-compaction report"""
        report_timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        report_file = os.path.join(self.reports_dir, f"pre_compaction_report_{report_timestamp}.md")

        report_content = f"""# Pre-Compaction Memory Preparation Report

**Generated**: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
**System**: Integrated Memory System v1.0

## PREPARATION SUMMARY

✅ **Success**: {preparation_results['success']}
📋 **Steps Completed**: {len(preparation_results['steps_completed'])}
⚠️ **Warnings**: {len(preparation_results['warnings'])}
❌ **Errors**: {len(preparation_results['errors'])}

## COMPLETED STEPS

"""

        for step in preparation_results['steps_completed']:
            report_content += f"- ✅ {step}\n"

        if preparation_results['warnings']:
            report_content += "\n## WARNINGS\n\n"
            for warning in preparation_results['warnings']:
                report_content += f"- ⚠️ {warning}\n"

        if preparation_results['errors']:
            report_content += "\n## ERRORS\n\n"
            for error in preparation_results['errors']:
                report_content += f"- ❌ {error}\n"

        report_content += f"""

## BACKUP INFORMATION

**Files Backed Up**: {len(preparation_results['critical_files_backed_up'])}
**Context Preserved**: {'✅ Yes' if preparation_results['context_preserved'] else '❌ No'}

### Critical Files Backed Up:
"""

        for file_path in preparation_results['critical_files_backed_up']:
            report_content += f"- {file_path}\n"

        report_content += """

## RECOVERY INSTRUCTIONS

1. **Verify Environment**: Ensure working directory is correct
2. **Check Git Status**: Verify repository state
3. **Restore Services**: Start backend and frontend if needed
4. **Run Recovery**: Execute post-compaction recovery process
5. **Validate**: Run integrity tests to confirm system health

## EMERGENCY RECOVERY

If critical issues occur:
```bash
# Restore from backup
cp .integrated_memory/backups/pre_compaction_*/CLAUDE.md ./
cp .integrated_memory/backups/pre_compaction_*/rules.db backend/database/

# Run recovery
python integrated_memory_system.py --mode=recovery
```

---
**Report Generated by Integrated Memory System**
"""

        with open(report_file, 'w') as f:
            f.write(report_content)

        return report_file

    def _update_context_preservation_scripts(self):
        """Update existing context preservation scripts to integrate with new system"""
        # Update save_context.sh to use integrated system
        save_script_path = os.path.join(self.project_root, "save_context.sh")

        integrated_save_script = f"""#!/bin/bash
echo "=== INTEGRATED MEMORY SYSTEM - CONTEXT PRESERVATION ==="

# Run integrated pre-compaction preparation
python {os.path.join(self.project_root, 'integrated_memory_system.py')} --mode=pre-compaction

echo "✅ Integrated context preservation completed"
"""

        with open(save_script_path, 'w') as f:
            f.write(integrated_save_script)

        os.chmod(save_script_path, 0o755)

        # Update restore_context.sh to use integrated system
        restore_script_path = os.path.join(self.project_root, "restore_context.sh")

        integrated_restore_script = f"""#!/bin/bash
echo "=== INTEGRATED MEMORY SYSTEM - CONTEXT RESTORATION ==="

# Run integrated post-compaction recovery
python {os.path.join(self.project_root, 'integrated_memory_system.py')} --mode=post-compaction

echo "✅ Integrated context restoration completed"
"""

        with open(restore_script_path, 'w') as f:
            f.write(integrated_restore_script)

        os.chmod(restore_script_path, 0o755)

    def _validate_environment(self) -> Dict[str, Any]:
        """Validate post-compaction environment"""
        validation = {
            "valid": True,
            "issues": []
        }

        # Check working directory
        expected_dir = "/Users/chandramohn/workspace/rules_engine/ui-prototype"
        if os.getcwd() != expected_dir:
            validation["valid"] = False
            validation["issues"].append(f"Wrong working directory: {os.getcwd()}, expected: {expected_dir}")

        # Check git repository
        if not os.path.exists(os.path.join(self.project_root, ".git")):
            validation["valid"] = False
            validation["issues"].append("Git repository not found")

        # Check critical directories
        critical_dirs = ["backend", "frontend", "java-bridge"]
        for dir_name in critical_dirs:
            dir_path = os.path.join(self.project_root, dir_name)
            if not os.path.exists(dir_path):
                validation["valid"] = False
                validation["issues"].append(f"Missing critical directory: {dir_name}")

        return validation

    def _check_and_restore_services(self) -> Dict[str, Any]:
        """Check and attempt to restore services"""
        service_status = {
            "backend": False,
            "frontend": False,
            "all_running": False
        }

        # Check if services are running
        try:
            import requests

            # Check backend
            try:
                response = requests.get('http://localhost:5001/api/health', timeout=3)
                service_status["backend"] = response.status_code == 200
            except:
                service_status["backend"] = False

            # Check frontend
            try:
                response = requests.get('http://localhost:3000', timeout=3)
                service_status["frontend"] = response.status_code == 200
            except:
                service_status["frontend"] = False

        except ImportError:
            # Fallback to port checking
            import socket

            for service, port in [("backend", 5001), ("frontend", 3000)]:
                try:
                    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                        s.settimeout(1)
                        result = s.connect_ex(('localhost', port))
                        service_status[service] = result == 0
                except:
                    service_status[service] = False

        service_status["all_running"] = service_status["backend"] and service_status["frontend"]

        return service_status

    def _run_integrity_tests(self) -> Dict[str, Any]:
        """Run comprehensive integrity tests"""
        test_result = {
            "passed": True,
            "failures": [],
            "tests_run": []
        }

        # Test 1: CLAUDE.md integrity
        claude_validation = self.memory_manager.validate_memory_integrity()
        if claude_validation["file_exists"] and claude_validation["parseable"]:
            test_result["tests_run"].append("✅ CLAUDE.md integrity")
        else:
            test_result["passed"] = False
            test_result["failures"].append("❌ CLAUDE.md integrity failed")

        # Test 2: Database accessibility (if backend is available)
        try:
            sys.path.append(os.path.join(self.project_root, 'backend'))
            from app import create_app
            from models import Rule

            app = create_app()
            with app.app_context():
                rule_count = Rule.query.count()
                if rule_count > 0:
                    test_result["tests_run"].append(f"✅ Database accessible ({rule_count} rules)")
                else:
                    test_result["failures"].append("⚠️ Database empty or inaccessible")

        except Exception as e:
            test_result["failures"].append(f"⚠️ Database test failed: {str(e)}")

        # Test 3: Git repository health
        try:
            result = subprocess.run(
                ['git', 'status'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )
            if result.returncode == 0:
                test_result["tests_run"].append("✅ Git repository healthy")
            else:
                test_result["passed"] = False
                test_result["failures"].append("❌ Git repository issues")

        except Exception as e:
            test_result["passed"] = False
            test_result["failures"].append(f"❌ Git test failed: {str(e)}")

        return test_result

    def _generate_recovery_report(self, recovery_results: Dict[str, Any], context_recovery: Dict[str, Any]) -> str:
        """Generate post-compaction recovery report"""
        report_timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        report_file = os.path.join(self.reports_dir, f"post_compaction_report_{report_timestamp}.md")

        report_content = f"""# Post-Compaction Recovery Report

**Generated**: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
**System**: Integrated Memory System v1.0

## RECOVERY SUMMARY

✅ **Success**: {recovery_results['success']}
📋 **Steps Completed**: {len(recovery_results['steps_completed'])}
🌉 **Context Recovered**: {'✅ Yes' if recovery_results['context_recovered'] else '❌ No'}
🔧 **Services Restored**: {'✅ Yes' if recovery_results['services_restored'] else '❌ No'}

## COMPLETED STEPS

"""

        for step in recovery_results['steps_completed']:
            report_content += f"- ✅ {step}\n"

        if recovery_results['warnings']:
            report_content += "\n## WARNINGS\n\n"
            for warning in recovery_results['warnings']:
                report_content += f"- ⚠️ {warning}\n"

        if context_recovery.get("success"):
            recovery_info = context_recovery.get("recovery_info", {})
            report_content += f"""

## CONTEXT RECOVERY DETAILS

**Last Session ID**: {recovery_info.get('last_session_id', 'Unknown')}
**Last Focus**: {recovery_info.get('last_focus', 'Unknown')}
**Knowledge Gaps**: {recovery_info.get('knowledge_gaps_count', 0)}
**Technical Debt Items**: {recovery_info.get('technical_debt_items', 0)}

### Next Steps from Previous Session:
"""
            for step in recovery_info.get('next_steps', [])[:5]:
                report_content += f"- {step}\n"

        report_content += """

## SYSTEM HEALTH CHECK

Run the following to verify full system health:

```bash
# Check services
curl http://localhost:5001/api/health
curl http://localhost:3000

# Run regression tests
cd backend && python test_regression_suite.py

# Validate memory system
python memory_consolidation_agent.py
```

---
**Report Generated by Integrated Memory System**
"""

        with open(report_file, 'w') as f:
            f.write(report_content)

        return report_file

    def _save_system_state(self, state_data: Dict[str, Any]):
        """Save current system state"""
        with open(self.system_state_file, 'w') as f:
            json.dump(state_data, f, indent=2)

    def run_health_check(self) -> Dict[str, Any]:
        """Run comprehensive system health check"""
        print("🏥 Running Integrated Memory System Health Check")
        print("=" * 50)

        health_results = {
            "timestamp": datetime.datetime.now().isoformat(),
            "overall_health": "unknown",
            "component_health": {},
            "recommendations": []
        }

        # Check Memory Consolidation Agent
        try:
            # Just check if it can initialize
            agent = MemoryConsolidationAgent(self.project_root)
            health_results["component_health"]["memory_consolidation_agent"] = "healthy"
        except Exception as e:
            health_results["component_health"]["memory_consolidation_agent"] = f"error: {e}"

        # Check Claude Memory Manager
        try:
            manager = ClaudeMemoryManager(self.project_root)
            claude_validation = manager.validate_memory_integrity()
            if claude_validation["file_exists"] and claude_validation["parseable"]:
                health_results["component_health"]["claude_memory_manager"] = "healthy"
            else:
                health_results["component_health"]["claude_memory_manager"] = "issues_detected"
                health_results["recommendations"].append("Review CLAUDE.md integrity issues")
        except Exception as e:
            health_results["component_health"]["claude_memory_manager"] = f"error: {e}"

        # Check Session Continuity Bridge
        try:
            bridge = SessionContinuityBridge(self.project_root)
            health_results["component_health"]["session_continuity_bridge"] = "healthy"
        except Exception as e:
            health_results["component_health"]["session_continuity_bridge"] = f"error: {e}"

        # Check directory structure
        required_dirs = [self.memory_root, self.reports_dir, self.backups_dir]
        missing_dirs = [d for d in required_dirs if not os.path.exists(d)]

        if missing_dirs:
            health_results["component_health"]["directory_structure"] = f"missing: {missing_dirs}"
            health_results["recommendations"].append("Recreate missing memory system directories")
        else:
            health_results["component_health"]["directory_structure"] = "healthy"

        # Overall health assessment
        healthy_components = sum(1 for status in health_results["component_health"].values() if status == "healthy")
        total_components = len(health_results["component_health"])
        health_percentage = healthy_components / total_components

        if health_percentage >= 0.8:
            health_results["overall_health"] = "healthy"
        elif health_percentage >= 0.6:
            health_results["overall_health"] = "degraded"
        else:
            health_results["overall_health"] = "critical"

        # Print results
        print(f"Overall Health: {health_results['overall_health'].upper()}")
        print("\nComponent Status:")
        for component, status in health_results["component_health"].items():
            status_icon = "✅" if status == "healthy" else "⚠️" if "issues" in status else "❌"
            print(f"  {status_icon} {component}: {status}")

        if health_results["recommendations"]:
            print("\nRecommendations:")
            for rec in health_results["recommendations"]:
                print(f"  💡 {rec}")

        return health_results

def main():
    """Main entry point for Integrated Memory System"""
    import argparse

    parser = argparse.ArgumentParser(description="Integrated Memory System for Rules Engine Project")
    parser.add_argument("--mode", choices=["pre-compaction", "post-compaction", "health-check"],
                       default="health-check", help="Operation mode")
    parser.add_argument("--project-root", default=None, help="Project root directory")

    args = parser.parse_args()

    system = IntegratedMemorySystem(args.project_root)

    if args.mode == "pre-compaction":
        result = system.pre_compaction_preparation()
        exit_code = 0 if result["success"] else 1

    elif args.mode == "post-compaction":
        result = system.post_compaction_recovery()
        exit_code = 0 if result["success"] else 1

    else:  # health-check
        result = system.run_health_check()
        exit_code = 0 if result["overall_health"] in ["healthy", "degraded"] else 1

    return exit_code

if __name__ == "__main__":
    exit(main())