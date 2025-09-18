#!/usr/bin/env python3
"""
Session Continuity Bridge

Creates seamless bridges between development sessions to prevent knowledge loss
and ensure consistent development context across auto-compaction events.

Created: September 17, 2025
Version: 1.0
Purpose: Maintain development continuity across session boundaries
"""

import os
import sys
import json
import datetime
import subprocess
import hashlib
from typing import Dict, List, Set, Tuple, Optional, Any
from dataclasses import dataclass, asdict
from pathlib import Path
import difflib

@dataclass
class SessionState:
    """Captures the complete state of a development session"""
    timestamp: str
    session_id: str
    working_directory: str
    git_status: Dict[str, Any]
    modified_files: List[str]
    active_services: Dict[str, bool]
    current_focus: str
    development_momentum: Dict[str, Any]
    critical_context: Dict[str, Any]
    next_steps: List[str]

@dataclass
class KnowledgeGap:
    """Represents identified knowledge gaps"""
    id: str
    area: str
    description: str
    severity: str  # critical, high, medium, low
    evidence: List[str]
    impact: str
    suggested_resolution: str
    related_files: List[str]
    discovered_date: str
    resolved_date: Optional[str] = None

@dataclass
class ContextSnapshot:
    """Comprehensive context snapshot for recovery"""
    session_state: SessionState
    knowledge_gaps: List[KnowledgeGap]
    active_patterns: List[str]
    recent_decisions: List[str]
    technical_debt: List[str]
    documentation_state: Dict[str, Any]

class SessionContinuityBridge:
    """Manages session continuity and knowledge preservation"""

    def __init__(self, project_root: str = None):
        self.project_root = project_root or "/Users/chandramohn/workspace/rules_engine/ui-prototype"
        self.continuity_dir = os.path.join(self.project_root, ".session_continuity")
        self.snapshots_dir = os.path.join(self.continuity_dir, "snapshots")
        self.knowledge_gaps_file = os.path.join(self.continuity_dir, "knowledge_gaps.json")
        self.session_history_file = os.path.join(self.continuity_dir, "session_history.json")
        self.context_bridge_file = os.path.join(self.continuity_dir, "context_bridge.json")

        # Ensure directories exist
        os.makedirs(self.snapshots_dir, exist_ok=True)
        os.makedirs(self.continuity_dir, exist_ok=True)

        # Initialize knowledge gaps database
        self._initialize_knowledge_gaps()

    def _initialize_knowledge_gaps(self):
        """Initialize knowledge gaps database if it doesn't exist"""
        if not os.path.exists(self.knowledge_gaps_file):
            with open(self.knowledge_gaps_file, 'w') as f:
                json.dump([], f, indent=2)

    def capture_session_state(self) -> SessionState:
        """Capture comprehensive current session state"""
        print("📸 Capturing current session state...")

        session_id = self._generate_session_id()
        timestamp = datetime.datetime.now().isoformat()

        # Git status
        git_status = self._get_detailed_git_status()

        # Modified files
        modified_files = self._get_modified_files_detailed()

        # Service status
        active_services = self._check_service_status()

        # Development focus
        current_focus = self._analyze_current_focus()

        # Development momentum
        momentum = self._assess_development_momentum()

        # Critical context
        critical_context = self._extract_critical_context()

        # Next steps
        next_steps = self._suggest_next_steps()

        session_state = SessionState(
            timestamp=timestamp,
            session_id=session_id,
            working_directory=self.project_root,
            git_status=git_status,
            modified_files=modified_files,
            active_services=active_services,
            current_focus=current_focus,
            development_momentum=momentum,
            critical_context=critical_context,
            next_steps=next_steps
        )

        # Save session state
        self._save_session_state(session_state)

        return session_state

    def _generate_session_id(self) -> str:
        """Generate unique session identifier"""
        timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        git_hash = self._get_current_git_hash()[:8]
        return f"{timestamp}_{git_hash}"

    def _get_current_git_hash(self) -> str:
        """Get current git commit hash"""
        try:
            result = subprocess.run(
                ['git', 'rev-parse', 'HEAD'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )
            return result.stdout.strip()
        except:
            return "unknown"

    def _get_detailed_git_status(self) -> Dict[str, Any]:
        """Get detailed git status information"""
        try:
            # Git status
            status_result = subprocess.run(
                ['git', 'status', '--porcelain'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )

            # Git branch
            branch_result = subprocess.run(
                ['git', 'branch', '--show-current'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )

            # Recent commits
            log_result = subprocess.run(
                ['git', 'log', '--oneline', '-5'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )

            # Uncommitted changes
            diff_result = subprocess.run(
                ['git', 'diff', '--name-only'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )

            return {
                "branch": branch_result.stdout.strip(),
                "status_output": status_result.stdout,
                "recent_commits": log_result.stdout.strip().split('\n'),
                "uncommitted_files": diff_result.stdout.strip().split('\n') if diff_result.stdout.strip() else [],
                "clean": len(status_result.stdout.strip()) == 0
            }
        except Exception as e:
            return {"error": str(e)}

    def _get_modified_files_detailed(self) -> List[Dict[str, Any]]:
        """Get detailed information about modified files"""
        modified_files = []

        try:
            result = subprocess.run(
                ['git', 'status', '--porcelain'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )

            for line in result.stdout.strip().split('\n'):
                if line:
                    status = line[:2]
                    file_path = line[3:].strip()
                    full_path = os.path.join(self.project_root, file_path)

                    file_info = {
                        "path": file_path,
                        "status": status,
                        "exists": os.path.exists(full_path),
                        "size": os.path.getsize(full_path) if os.path.exists(full_path) else 0,
                        "extension": os.path.splitext(file_path)[1],
                        "category": self._categorize_file(file_path)
                    }

                    # Add modification time
                    if os.path.exists(full_path):
                        file_info["modified_time"] = datetime.datetime.fromtimestamp(
                            os.path.getmtime(full_path)
                        ).isoformat()

                    modified_files.append(file_info)

        except Exception as e:
            print(f"Warning: Could not get detailed file information: {e}")

        return modified_files

    def _categorize_file(self, file_path: str) -> str:
        """Categorize file by its role in the project"""
        if file_path.startswith('backend/'):
            if file_path.endswith('.py'):
                return "backend_code"
            elif file_path.endswith('.txt') or file_path.endswith('.md'):
                return "backend_docs"
        elif file_path.startswith('frontend/'):
            if file_path.endswith(('.js', '.jsx')):
                return "frontend_code"
            elif file_path.endswith('.json'):
                return "frontend_config"
        elif file_path.startswith('java-bridge/'):
            if file_path.endswith('.java'):
                return "java_code"
            elif file_path.endswith('.g4'):
                return "grammar"
        elif file_path.endswith('.md'):
            return "documentation"
        elif file_path.endswith(('.json', '.yaml', '.yml')):
            return "configuration"

        return "other"

    def _check_service_status(self) -> Dict[str, bool]:
        """Check status of key project services"""
        services = {}

        try:
            import requests

            # Backend health check
            try:
                response = requests.get('http://localhost:5001/api/health', timeout=2)
                services["backend"] = response.status_code == 200
            except:
                services["backend"] = False

            # Frontend health check
            try:
                response = requests.get('http://localhost:3000', timeout=2)
                services["frontend"] = response.status_code == 200
            except:
                services["frontend"] = False

        except ImportError:
            # Fallback to basic port checking
            services["backend"] = self._check_port(5001)
            services["frontend"] = self._check_port(3000)

        return services

    def _check_port(self, port: int) -> bool:
        """Check if a port is accessible"""
        import socket
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.settimeout(1)
                result = s.connect_ex(('localhost', port))
                return result == 0
        except:
            return False

    def _analyze_current_focus(self) -> str:
        """Analyze what the current development focus is"""
        modified_files = self._get_modified_files_detailed()

        # Analyze file patterns to determine focus
        categories = {}
        for file_info in modified_files:
            category = file_info["category"]
            categories[category] = categories.get(category, 0) + 1

        # Determine primary focus
        if categories.get("documentation", 0) > 3:
            return "Documentation and Knowledge Management"
        elif categories.get("backend_code", 0) > categories.get("frontend_code", 0):
            return "Backend Development and API Design"
        elif categories.get("frontend_code", 0) > 0:
            return "Frontend Development and UI/UX"
        elif categories.get("java_code", 0) > 0 or categories.get("grammar", 0) > 0:
            return "Java Bridge and Grammar Development"
        else:
            return "General System Maintenance"

    def _assess_development_momentum(self) -> Dict[str, Any]:
        """Assess current development momentum and velocity"""
        try:
            # Get recent commit activity
            result = subprocess.run(
                ['git', 'log', '--since="1 day ago"', '--oneline'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )

            recent_commits = len(result.stdout.strip().split('\n')) if result.stdout.strip() else 0

            # Get file change frequency
            modified_files = self._get_modified_files_detailed()
            files_changed = len(modified_files)

            # Assess momentum
            momentum_score = min(recent_commits * 2 + files_changed, 10)

            return {
                "momentum_score": momentum_score,
                "recent_commits_24h": recent_commits,
                "files_currently_modified": files_changed,
                "velocity": "high" if momentum_score > 7 else "medium" if momentum_score > 3 else "low",
                "trajectory": self._assess_trajectory()
            }

        except Exception as e:
            return {"error": str(e)}

    def _assess_trajectory(self) -> str:
        """Assess development trajectory"""
        # This is a simplified assessment
        # In practice, you could analyze commit messages, file changes, etc.
        modified_files = self._get_modified_files_detailed()

        backend_changes = sum(1 for f in modified_files if f["category"] == "backend_code")
        frontend_changes = sum(1 for f in modified_files if f["category"] == "frontend_code")
        doc_changes = sum(1 for f in modified_files if f["category"] == "documentation")

        if doc_changes > backend_changes + frontend_changes:
            return "consolidation_and_documentation"
        elif backend_changes > frontend_changes:
            return "backend_focused_development"
        elif frontend_changes > 0:
            return "frontend_focused_development"
        else:
            return "exploratory_development"

    def _extract_critical_context(self) -> Dict[str, Any]:
        """Extract critical context that must be preserved"""
        context = {}

        # Check for critical files
        critical_files = [
            "CLAUDE.md",
            "CLAUDE_SESSION_CHECKPOINT.md",
            "backend/models.py",
            "frontend/src/components/RuleEditor.jsx",
            "java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4"
        ]

        context["critical_files_status"] = {}
        for file_path in critical_files:
            full_path = os.path.join(self.project_root, file_path)
            context["critical_files_status"][file_path] = {
                "exists": os.path.exists(full_path),
                "size": os.path.getsize(full_path) if os.path.exists(full_path) else 0,
                "last_modified": datetime.datetime.fromtimestamp(
                    os.path.getmtime(full_path)
                ).isoformat() if os.path.exists(full_path) else None
            }

        # Extract recent decisions from session checkpoint
        context["recent_decisions"] = self._extract_recent_decisions()

        # Current system state
        context["system_state"] = {
            "database_accessible": self._check_database_accessibility(),
            "services_running": self._check_service_status(),
            "git_clean": self._get_detailed_git_status().get("clean", False)
        }

        return context

    def _extract_recent_decisions(self) -> List[str]:
        """Extract recent decisions from session checkpoint"""
        checkpoint_path = os.path.join(self.project_root, "CLAUDE_SESSION_CHECKPOINT.md")

        if not os.path.exists(checkpoint_path):
            return []

        try:
            with open(checkpoint_path, 'r') as f:
                content = f.read()

            # Extract completed work items
            decisions = []
            in_completed_section = False

            for line in content.split('\n'):
                if '## ✅ COMPLETED WORK' in line:
                    in_completed_section = True
                elif line.startswith('## ') and '✅' not in line:
                    in_completed_section = False
                elif in_completed_section and line.strip().startswith('-'):
                    decisions.append(line.strip())

            return decisions[-5:]  # Return last 5 decisions

        except Exception as e:
            print(f"Warning: Could not extract recent decisions: {e}")
            return []

    def _check_database_accessibility(self) -> bool:
        """Check if database is accessible"""
        try:
            # Add backend to path
            sys.path.append(os.path.join(self.project_root, 'backend'))

            from app import create_app
            from models import Rule

            app = create_app()
            with app.app_context():
                count = Rule.query.count()
                return count > 0

        except Exception:
            return False

    def _suggest_next_steps(self) -> List[str]:
        """Suggest next development steps based on current state"""
        next_steps = []

        # Analyze current focus and suggest continuations
        current_focus = self._analyze_current_focus()

        if "Documentation" in current_focus:
            next_steps.extend([
                "Complete documentation consolidation",
                "Update CLAUDE.md with latest changes",
                "Run memory consolidation agent"
            ])
        elif "Backend" in current_focus:
            next_steps.extend([
                "Run regression test suite",
                "Validate API endpoints",
                "Check database integrity"
            ])
        elif "Frontend" in current_focus:
            next_steps.extend([
                "Test UI components",
                "Validate user workflows",
                "Check responsive design"
            ])

        # Always add general maintenance steps
        next_steps.extend([
            "Verify all services are running",
            "Create git commit if changes are ready",
            "Update session checkpoint"
        ])

        return next_steps[:5]  # Return top 5 steps

    def _save_session_state(self, session_state: SessionState):
        """Save session state to history"""
        # Save individual session state
        session_file = os.path.join(self.snapshots_dir, f"session_{session_state.session_id}.json")
        with open(session_file, 'w') as f:
            json.dump(asdict(session_state), f, indent=2)

        # Update session history
        history = []
        if os.path.exists(self.session_history_file):
            with open(self.session_history_file, 'r') as f:
                history = json.load(f)

        history.append({
            "session_id": session_state.session_id,
            "timestamp": session_state.timestamp,
            "focus": session_state.current_focus,
            "momentum": session_state.development_momentum.get("velocity", "unknown"),
            "files_modified": len(session_state.modified_files)
        })

        # Keep only last 20 sessions
        history = history[-20:]

        with open(self.session_history_file, 'w') as f:
            json.dump(history, f, indent=2)

    def identify_knowledge_gaps(self) -> List[KnowledgeGap]:
        """Systematically identify knowledge gaps in the project"""
        print("🔍 Identifying knowledge gaps...")

        gaps = []

        # Load existing gaps
        existing_gaps = self._load_existing_gaps()

        # Check for missing documentation
        gaps.extend(self._check_documentation_gaps())

        # Check for architectural inconsistencies
        gaps.extend(self._check_architectural_gaps())

        # Check for process gaps
        gaps.extend(self._check_process_gaps())

        # Check for technical debt
        gaps.extend(self._check_technical_debt_gaps())

        # Remove duplicates and merge with existing
        merged_gaps = self._merge_knowledge_gaps(existing_gaps, gaps)

        # Save updated gaps
        self._save_knowledge_gaps(merged_gaps)

        return merged_gaps

    def _load_existing_gaps(self) -> List[KnowledgeGap]:
        """Load existing knowledge gaps"""
        try:
            with open(self.knowledge_gaps_file, 'r') as f:
                gaps_data = json.load(f)
                return [KnowledgeGap(**gap) for gap in gaps_data]
        except:
            return []

    def _check_documentation_gaps(self) -> List[KnowledgeGap]:
        """Check for documentation gaps"""
        gaps = []

        # Define critical documentation areas
        critical_docs = {
            "API Documentation": {
                "files": ["api_design.md", "ATTRIBUTES_AND_ACTIONS_REFERENCE.md"],
                "indicators": ["endpoint", "api", "response"]
            },
            "Database Schema": {
                "files": ["SCHEMA_CONFIGURATION.md"],
                "indicators": ["table", "column", "relationship"]
            },
            "Setup Instructions": {
                "files": ["README.md", "WINDOWS_SETUP.md"],
                "indicators": ["install", "setup", "requirements"]
            },
            "Testing Guidelines": {
                "files": ["VALIDATION_AGENT_GUIDE.md"],
                "indicators": ["test", "validation", "regression"]
            }
        }

        for area, config in critical_docs.items():
            adequacy_score = self._assess_documentation_adequacy(area, config)

            if adequacy_score < 0.7:  # Less than 70% adequate
                gaps.append(KnowledgeGap(
                    id=f"doc_gap_{area.lower().replace(' ', '_')}",
                    area=f"Documentation - {area}",
                    description=f"Inadequate documentation for {area}",
                    severity="medium" if adequacy_score > 0.3 else "high",
                    evidence=[f"Adequacy score: {adequacy_score:.2%}"],
                    impact=f"Developers may struggle with {area.lower()}",
                    suggested_resolution=f"Expand and improve {area} documentation",
                    related_files=config["files"],
                    discovered_date=datetime.datetime.now().isoformat()
                ))

        return gaps

    def _assess_documentation_adequacy(self, area: str, config: Dict) -> float:
        """Assess how adequate documentation is for a given area"""
        score = 0.0
        total_checks = 0

        for file_name in config["files"]:
            file_path = os.path.join(self.project_root, file_name)
            total_checks += 1

            if os.path.exists(file_path):
                score += 0.3  # File exists

                try:
                    with open(file_path, 'r') as f:
                        content = f.read()

                    # Check content length
                    if len(content) > 1000:
                        score += 0.2

                    # Check for key indicators
                    indicators_found = sum(1 for indicator in config["indicators"]
                                         if indicator.lower() in content.lower())

                    score += min(indicators_found / len(config["indicators"]) * 0.5, 0.5)

                except:
                    pass

        return score / total_checks if total_checks > 0 else 0.0

    def _check_architectural_gaps(self) -> List[KnowledgeGap]:
        """Check for architectural knowledge gaps"""
        gaps = []

        # Check for missing architectural documentation
        architecture_files = [
            "ARCHITECTURE_CONSOLIDATION.md",
            "SOLUTION_DESIGN.md"
        ]

        architecture_adequacy = 0
        for file_name in architecture_files:
            file_path = os.path.join(self.project_root, file_name)
            if os.path.exists(file_path):
                with open(file_path, 'r') as f:
                    content = f.read()
                    if len(content) > 2000:
                        architecture_adequacy += 1

        if architecture_adequacy < len(architecture_files) * 0.8:
            gaps.append(KnowledgeGap(
                id="arch_gap_documentation",
                area="Architecture Documentation",
                description="Insufficient architectural documentation",
                severity="medium",
                evidence=["Missing or incomplete architecture files"],
                impact="New developers may not understand system design",
                suggested_resolution="Create comprehensive architecture documentation",
                related_files=architecture_files,
                discovered_date=datetime.datetime.now().isoformat()
            ))

        return gaps

    def _check_process_gaps(self) -> List[KnowledgeGap]:
        """Check for development process gaps"""
        gaps = []

        # Check for missing process documentation
        process_areas = {
            "Development Workflow": ["workflow", "development", "process"],
            "Deployment Process": ["deploy", "deployment", "production"],
            "Testing Strategy": ["test", "testing", "quality"],
            "Code Review Guidelines": ["review", "guidelines", "standards"]
        }

        for area, keywords in process_areas.items():
            has_documentation = False

            # Check all markdown files for process documentation
            for file_name in os.listdir(self.project_root):
                if file_name.endswith('.md'):
                    file_path = os.path.join(self.project_root, file_name)
                    try:
                        with open(file_path, 'r') as f:
                            content = f.read().lower()
                            if any(keyword in content for keyword in keywords):
                                if len(content) > 500:  # Substantial content
                                    has_documentation = True
                                    break
                    except:
                        pass

            if not has_documentation:
                gaps.append(KnowledgeGap(
                    id=f"process_gap_{area.lower().replace(' ', '_')}",
                    area=f"Process - {area}",
                    description=f"Missing {area.lower()} documentation",
                    severity="low",
                    evidence=[f"No substantial {area.lower()} documentation found"],
                    impact=f"Team may lack clarity on {area.lower()}",
                    suggested_resolution=f"Document {area.lower()} procedures",
                    related_files=[],
                    discovered_date=datetime.datetime.now().isoformat()
                ))

        return gaps

    def _check_technical_debt_gaps(self) -> List[KnowledgeGap]:
        """Check for technical debt and maintenance gaps"""
        gaps = []

        # Check for TODO comments
        todo_count = 0
        files_with_todos = []

        for root, dirs, files in os.walk(self.project_root):
            # Skip hidden directories and node_modules
            dirs[:] = [d for d in dirs if not d.startswith('.') and d != 'node_modules']

            for file in files:
                if file.endswith(('.py', '.js', '.jsx', '.java')):
                    file_path = os.path.join(root, file)
                    try:
                        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                            content = f.read()
                            if 'TODO' in content or 'FIXME' in content:
                                todo_count += content.count('TODO') + content.count('FIXME')
                                files_with_todos.append(os.path.relpath(file_path, self.project_root))
                    except:
                        pass

        if todo_count > 10:
            gaps.append(KnowledgeGap(
                id="tech_debt_todos",
                area="Technical Debt - TODOs",
                description=f"High number of TODO/FIXME comments ({todo_count})",
                severity="medium",
                evidence=[f"Found {todo_count} TODO/FIXME comments in {len(files_with_todos)} files"],
                impact="Unresolved technical debt may impact code quality",
                suggested_resolution="Review and address TODO/FIXME comments",
                related_files=files_with_todos,
                discovered_date=datetime.datetime.now().isoformat()
            ))

        return gaps

    def _merge_knowledge_gaps(self, existing: List[KnowledgeGap], new: List[KnowledgeGap]) -> List[KnowledgeGap]:
        """Merge existing and new knowledge gaps"""
        existing_map = {gap.id: gap for gap in existing}

        for new_gap in new:
            if new_gap.id in existing_map:
                # Update existing gap
                existing_gap = existing_map[new_gap.id]
                existing_gap.evidence.extend(new_gap.evidence)
                existing_gap.evidence = list(set(existing_gap.evidence))  # Remove duplicates
            else:
                # Add new gap
                existing_map[new_gap.id] = new_gap

        return list(existing_map.values())

    def _save_knowledge_gaps(self, gaps: List[KnowledgeGap]):
        """Save knowledge gaps to file"""
        with open(self.knowledge_gaps_file, 'w') as f:
            json.dump([asdict(gap) for gap in gaps], f, indent=2)

    def create_context_bridge(self) -> Dict[str, Any]:
        """Create comprehensive context bridge for next session"""
        print("🌉 Creating context bridge for session continuity...")

        # Capture current session state
        session_state = self.capture_session_state()

        # Identify knowledge gaps
        knowledge_gaps = self.identify_knowledge_gaps()

        # Extract active patterns (simplified for now)
        active_patterns = self._extract_active_patterns()

        # Extract recent decisions
        recent_decisions = self._extract_recent_decisions()

        # Identify technical debt
        technical_debt = self._identify_technical_debt()

        # Assess documentation state
        documentation_state = self._assess_documentation_state()

        # Create comprehensive snapshot
        context_snapshot = ContextSnapshot(
            session_state=session_state,
            knowledge_gaps=knowledge_gaps,
            active_patterns=active_patterns,
            recent_decisions=recent_decisions,
            technical_debt=technical_debt,
            documentation_state=documentation_state
        )

        # Save context bridge
        bridge_data = asdict(context_snapshot)
        with open(self.context_bridge_file, 'w') as f:
            json.dump(bridge_data, f, indent=2)

        print(f"✅ Context bridge created with {len(knowledge_gaps)} knowledge gaps identified")

        return bridge_data

    def _extract_active_patterns(self) -> List[str]:
        """Extract currently active development patterns"""
        patterns = []

        # Simple pattern detection based on recent file changes
        modified_files = self._get_modified_files_detailed()

        backend_files = [f for f in modified_files if f["category"] == "backend_code"]
        frontend_files = [f for f in modified_files if f["category"] == "frontend_code"]

        if backend_files:
            patterns.append("Backend API Development Pattern")

        if frontend_files:
            patterns.append("React Component Development Pattern")

        if any(f["category"] == "documentation" for f in modified_files):
            patterns.append("Documentation-First Development Pattern")

        return patterns

    def _identify_technical_debt(self) -> List[str]:
        """Identify areas of technical debt"""
        debt_items = []

        # Check for outdated dependencies (simplified check)
        package_json_path = os.path.join(self.project_root, "frontend", "package.json")
        if os.path.exists(package_json_path):
            try:
                with open(package_json_path, 'r') as f:
                    content = f.read()
                    if '"react": "18.' in content:
                        debt_items.append("Frontend using React 18 - consider upgrade strategy")
            except:
                pass

        # Check for large files that might need refactoring
        large_files = []
        for root, dirs, files in os.walk(self.project_root):
            dirs[:] = [d for d in dirs if not d.startswith('.') and d != 'node_modules']

            for file in files:
                if file.endswith(('.py', '.js', '.jsx')):
                    file_path = os.path.join(root, file)
                    try:
                        if os.path.getsize(file_path) > 10000:  # Files larger than 10KB
                            large_files.append(os.path.relpath(file_path, self.project_root))
                    except:
                        pass

        if large_files:
            debt_items.append(f"Large files may need refactoring: {', '.join(large_files[:3])}")

        return debt_items

    def _assess_documentation_state(self) -> Dict[str, Any]:
        """Assess current documentation state"""
        doc_files = [f for f in os.listdir(self.project_root) if f.endswith('.md')]

        total_words = 0
        doc_health = {}

        for doc_file in doc_files:
            file_path = os.path.join(self.project_root, doc_file)
            try:
                with open(file_path, 'r') as f:
                    content = f.read()
                    word_count = len(content.split())
                    total_words += word_count

                    doc_health[doc_file] = {
                        "word_count": word_count,
                        "last_modified": datetime.datetime.fromtimestamp(
                            os.path.getmtime(file_path)
                        ).isoformat(),
                        "health": "good" if word_count > 500 else "needs_attention"
                    }
            except:
                doc_health[doc_file] = {"error": "Could not read file"}

        return {
            "total_documentation_files": len(doc_files),
            "total_words": total_words,
            "files": doc_health,
            "overall_health": "good" if total_words > 10000 else "needs_improvement"
        }

    def recover_from_context_bridge(self) -> Dict[str, Any]:
        """Recover context from the last session bridge"""
        print("🔄 Recovering context from session bridge...")

        if not os.path.exists(self.context_bridge_file):
            return {"success": False, "error": "No context bridge found"}

        try:
            with open(self.context_bridge_file, 'r') as f:
                bridge_data = json.load(f)

            # Extract key information for recovery
            recovery_info = {
                "last_session_id": bridge_data.get("session_state", {}).get("session_id"),
                "last_focus": bridge_data.get("session_state", {}).get("current_focus"),
                "knowledge_gaps_count": len(bridge_data.get("knowledge_gaps", [])),
                "critical_gaps": [
                    gap for gap in bridge_data.get("knowledge_gaps", [])
                    if gap.get("severity") == "critical"
                ],
                "next_steps": bridge_data.get("session_state", {}).get("next_steps", []),
                "services_were_running": bridge_data.get("session_state", {}).get("active_services", {}),
                "documentation_health": bridge_data.get("documentation_state", {}).get("overall_health"),
                "technical_debt_items": len(bridge_data.get("technical_debt", []))
            }

            print(f"✅ Context recovered from session {recovery_info['last_session_id']}")
            print(f"🎯 Last focus: {recovery_info['last_focus']}")
            print(f"⚠️ Knowledge gaps: {recovery_info['knowledge_gaps_count']}")

            return {"success": True, "recovery_info": recovery_info}

        except Exception as e:
            return {"success": False, "error": str(e)}

def main():
    """Main entry point for Session Continuity Bridge"""
    print("Session Continuity Bridge v1.0")
    print("Ensuring seamless development continuity across sessions")
    print()

    bridge = SessionContinuityBridge()

    # Create context bridge
    context_bridge = bridge.create_context_bridge()

    print("\n📊 Session State Summary:")
    print(f"- Session ID: {context_bridge['session_state']['session_id']}")
    print(f"- Current Focus: {context_bridge['session_state']['current_focus']}")
    print(f"- Modified Files: {len(context_bridge['session_state']['modified_files'])}")
    print(f"- Knowledge Gaps: {len(context_bridge['knowledge_gaps'])}")
    print(f"- Next Steps: {len(context_bridge['session_state']['next_steps'])}")

    print("\n🔍 Critical Knowledge Gaps:")
    critical_gaps = [gap for gap in context_bridge['knowledge_gaps'] if gap['severity'] == 'critical']
    if critical_gaps:
        for gap in critical_gaps[:3]:
            print(f"  - {gap['area']}: {gap['description']}")
    else:
        print("  - No critical knowledge gaps identified")

    print("\n🎯 Suggested Next Steps:")
    for i, step in enumerate(context_bridge['session_state']['next_steps'][:3], 1):
        print(f"  {i}. {step}")

    return 0

if __name__ == "__main__":
    exit(main())