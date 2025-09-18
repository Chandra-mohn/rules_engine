#!/usr/bin/env python3
"""
Memory Consolidation Agent for Rules Engine Project

This agent systematically manages and consolidates project knowledge to prevent
context loss during auto-compaction and provides seamless cross-session continuity.

Created: September 17, 2025
Version: 1.0
Purpose: Prevent knowledge loss and maintain project memory across sessions
"""

import os
import sys
import json
import re
import datetime
import subprocess
import hashlib
from pathlib import Path
from typing import Dict, List, Set, Tuple, Optional, Any
from dataclasses import dataclass, asdict
from collections import defaultdict, Counter
import difflib

# Add backend to path for database access
sys.path.append(os.path.join(os.path.dirname(__file__), 'backend'))

@dataclass
class SessionLearning:
    """Captures learning from a development session"""
    timestamp: str
    modified_files: List[str]
    new_patterns: List[str]
    decisions_made: List[str]
    knowledge_gaps_filled: List[str]
    new_conventions: List[str]
    critical_insights: List[str]

@dataclass
class PatternDefinition:
    """Defines a discovered pattern in the codebase"""
    name: str
    description: str
    files_involved: List[str]
    examples: List[str]
    frequency: int
    last_seen: str
    category: str

@dataclass
class KnowledgeGap:
    """Identifies missing or incomplete knowledge"""
    area: str
    description: str
    severity: str  # critical, high, medium, low
    suggested_documentation: str
    related_files: List[str]
    discovered_date: str

class MemoryConsolidationAgent:
    """Main agent for memory consolidation and knowledge management"""

    def __init__(self, project_root: str = None):
        self.project_root = project_root or "/Users/chandramohn/workspace/rules_engine/ui-prototype"
        self.memory_dir = os.path.join(self.project_root, ".memory_consolidation")
        self.claude_md_path = os.path.join(self.project_root, "CLAUDE.md")
        self.session_checkpoint_path = os.path.join(self.project_root, "CLAUDE_SESSION_CHECKPOINT.md")

        # Ensure memory directory exists
        os.makedirs(self.memory_dir, exist_ok=True)

        # Core knowledge files
        self.patterns_db = os.path.join(self.memory_dir, "patterns_database.json")
        self.knowledge_gaps_db = os.path.join(self.memory_dir, "knowledge_gaps.json")
        self.session_learnings_db = os.path.join(self.memory_dir, "session_learnings.json")
        self.consolidated_memory = os.path.join(self.memory_dir, "consolidated_memory.json")

        # Initialize databases
        self._initialize_databases()

    def _initialize_databases(self):
        """Initialize empty databases if they don't exist"""
        default_data = {
            self.patterns_db: [],
            self.knowledge_gaps_db: [],
            self.session_learnings_db: [],
            self.consolidated_memory: {
                "last_updated": "",
                "key_decisions": [],
                "established_patterns": [],
                "critical_conventions": [],
                "architecture_evolution": [],
                "context_snapshots": []
            }
        }

        for db_path, default_content in default_data.items():
            if not os.path.exists(db_path):
                with open(db_path, 'w') as f:
                    json.dump(default_content, f, indent=2)

    def analyze_current_session(self) -> SessionLearning:
        """Analyze current session to extract learnings"""
        print("🔍 Analyzing current session for learnings...")

        timestamp = datetime.datetime.now().isoformat()

        # Get git status for modified files
        modified_files = self._get_modified_files()

        # Analyze file changes for patterns
        new_patterns = self._identify_new_patterns(modified_files)

        # Extract decisions from recent commits
        decisions_made = self._extract_decisions_from_commits()

        # Identify knowledge gaps that were filled
        knowledge_gaps_filled = self._identify_filled_knowledge_gaps()

        # Discover new conventions
        new_conventions = self._discover_new_conventions(modified_files)

        # Extract critical insights
        critical_insights = self._extract_critical_insights()

        session_learning = SessionLearning(
            timestamp=timestamp,
            modified_files=modified_files,
            new_patterns=new_patterns,
            decisions_made=decisions_made,
            knowledge_gaps_filled=knowledge_gaps_filled,
            new_conventions=new_conventions,
            critical_insights=critical_insights
        )

        # Store session learning
        self._store_session_learning(session_learning)

        return session_learning

    def _get_modified_files(self) -> List[str]:
        """Get list of modified files from git status"""
        try:
            result = subprocess.run(
                ['git', 'status', '--porcelain'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )

            modified_files = []
            for line in result.stdout.strip().split('\n'):
                if line and line.startswith(' M'):
                    file_path = line[3:].strip()
                    modified_files.append(file_path)

            return modified_files
        except Exception as e:
            print(f"Warning: Could not get git status: {e}")
            return []

    def _identify_new_patterns(self, modified_files: List[str]) -> List[str]:
        """Identify new patterns in modified files"""
        patterns = []

        for file_path in modified_files:
            full_path = os.path.join(self.project_root, file_path)
            if os.path.exists(full_path):
                # Analyze file for patterns
                file_patterns = self._analyze_file_patterns(full_path, file_path)
                patterns.extend(file_patterns)

        return list(set(patterns))  # Remove duplicates

    def _analyze_file_patterns(self, full_path: str, relative_path: str) -> List[str]:
        """Analyze a single file for patterns"""
        patterns = []

        try:
            with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()

            # Pattern detection rules
            if relative_path.endswith('.py'):
                patterns.extend(self._detect_python_patterns(content, relative_path))
            elif relative_path.endswith('.jsx') or relative_path.endswith('.js'):
                patterns.extend(self._detect_javascript_patterns(content, relative_path))
            elif relative_path.endswith('.md'):
                patterns.extend(self._detect_documentation_patterns(content, relative_path))

        except Exception as e:
            print(f"Warning: Could not analyze {relative_path}: {e}")

        return patterns

    def _detect_python_patterns(self, content: str, file_path: str) -> List[str]:
        """Detect patterns in Python files"""
        patterns = []

        # Service layer pattern
        if 'class' in content and 'Service' in content:
            patterns.append(f"Service Layer Pattern in {file_path}")

        # API Blueprint pattern
        if 'Blueprint' in content and '@bp.route' in content:
            patterns.append(f"Flask Blueprint Pattern in {file_path}")

        # SQLAlchemy model pattern
        if 'db.Model' in content and 'class' in content:
            patterns.append(f"SQLAlchemy Model Pattern in {file_path}")

        # Validation pattern
        if 'validate' in content.lower() and 'def' in content:
            patterns.append(f"Validation Pattern in {file_path}")

        return patterns

    def _detect_javascript_patterns(self, content: str, file_path: str) -> List[str]:
        """Detect patterns in JavaScript/JSX files"""
        patterns = []

        # React functional component pattern
        if 'const' in content and '= (' in content and 'return' in content:
            patterns.append(f"React Functional Component Pattern in {file_path}")

        # Hooks pattern
        if 'useState' in content or 'useEffect' in content:
            patterns.append(f"React Hooks Pattern in {file_path}")

        # Antd components pattern
        if 'import' in content and 'antd' in content:
            patterns.append(f"Antd UI Components Pattern in {file_path}")

        # API client pattern
        if 'axios' in content or 'fetch' in content:
            patterns.append(f"API Client Pattern in {file_path}")

        return patterns

    def _detect_documentation_patterns(self, content: str, file_path: str) -> List[str]:
        """Detect patterns in documentation files"""
        patterns = []

        # Architecture documentation pattern
        if '##' in content and ('architecture' in content.lower() or 'design' in content.lower()):
            patterns.append(f"Architecture Documentation Pattern in {file_path}")

        # API documentation pattern
        if 'endpoint' in content.lower() or 'api' in content.lower():
            patterns.append(f"API Documentation Pattern in {file_path}")

        return patterns

    def _extract_decisions_from_commits(self) -> List[str]:
        """Extract architectural decisions from recent commits"""
        decisions = []

        try:
            # Get recent commits
            result = subprocess.run(
                ['git', 'log', '--oneline', '-10'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )

            for line in result.stdout.strip().split('\n'):
                if line:
                    commit_msg = line.split(' ', 1)[1] if ' ' in line else line

                    # Look for decision keywords
                    decision_keywords = ['implement', 'add', 'refactor', 'consolidate', 'fix', 'enhance']
                    for keyword in decision_keywords:
                        if keyword.lower() in commit_msg.lower():
                            decisions.append(f"Decision: {commit_msg}")
                            break

        except Exception as e:
            print(f"Warning: Could not extract commit decisions: {e}")

        return decisions

    def _identify_filled_knowledge_gaps(self) -> List[str]:
        """Identify knowledge gaps that were filled in this session"""
        filled_gaps = []

        # This would compare against existing knowledge gaps
        # For now, we'll identify common areas where gaps are typically filled
        common_gap_areas = [
            "ActionSet implementation",
            "Status validation",
            "UI component behavior",
            "Database schema",
            "API integration"
        ]

        # Check if any modified files relate to these areas
        modified_files = self._get_modified_files()
        for gap_area in common_gap_areas:
            for file_path in modified_files:
                if any(keyword in file_path.lower() for keyword in gap_area.lower().split()):
                    filled_gaps.append(f"Knowledge gap filled in {gap_area}")
                    break

        return filled_gaps

    def _discover_new_conventions(self, modified_files: List[str]) -> List[str]:
        """Discover new coding conventions from modified files"""
        conventions = []

        # Analyze naming patterns
        for file_path in modified_files:
            if file_path.endswith('.py'):
                conventions.append("Python snake_case naming convention")
            elif file_path.endswith('.jsx'):
                conventions.append("React PascalCase component naming convention")
            elif file_path.endswith('.js'):
                conventions.append("JavaScript camelCase utility naming convention")

        # Check for status value conventions
        full_paths = [os.path.join(self.project_root, f) for f in modified_files]
        for full_path in full_paths:
            if os.path.exists(full_path):
                try:
                    with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                        if any(status in content for status in ['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD']):
                            conventions.append("Uppercase status value convention")
                            break
                except:
                    pass

        return list(set(conventions))

    def _extract_critical_insights(self) -> List[str]:
        """Extract critical insights from the current session"""
        insights = []

        # Look for insights in session checkpoint if it exists
        if os.path.exists(self.session_checkpoint_path):
            try:
                with open(self.session_checkpoint_path, 'r') as f:
                    checkpoint_content = f.read()

                # Extract insights from completed work sections
                if '## ✅ COMPLETED WORK' in checkpoint_content:
                    insights.append("Successfully completed regression fixes")

                if 'Zero Regression Policy' in checkpoint_content:
                    insights.append("Zero regression policy implementation")

                if 'ActionSet' in checkpoint_content:
                    insights.append("ActionSet implementation with unified table approach")

            except Exception as e:
                print(f"Warning: Could not extract insights from checkpoint: {e}")

        return insights

    def _store_session_learning(self, learning: SessionLearning):
        """Store session learning in database"""
        try:
            with open(self.session_learnings_db, 'r') as f:
                learnings = json.load(f)

            learnings.append(asdict(learning))

            with open(self.session_learnings_db, 'w') as f:
                json.dump(learnings, f, indent=2)

        except Exception as e:
            print(f"Warning: Could not store session learning: {e}")

    def consolidate_patterns(self) -> List[PatternDefinition]:
        """Consolidate and update pattern database"""
        print("🔄 Consolidating patterns from project codebase...")

        patterns = []

        # Load existing patterns
        try:
            with open(self.patterns_db, 'r') as f:
                existing_patterns = json.load(f)
                patterns = [PatternDefinition(**p) for p in existing_patterns]
        except:
            patterns = []

        # Analyze entire codebase for patterns
        new_patterns = self._scan_codebase_for_patterns()

        # Merge new patterns with existing ones
        pattern_map = {p.name: p for p in patterns}

        for new_pattern in new_patterns:
            if new_pattern.name in pattern_map:
                # Update existing pattern
                pattern_map[new_pattern.name].frequency += 1
                pattern_map[new_pattern.name].last_seen = datetime.datetime.now().isoformat()
                if new_pattern.examples not in pattern_map[new_pattern.name].examples:
                    pattern_map[new_pattern.name].examples.extend(new_pattern.examples)
            else:
                # Add new pattern
                pattern_map[new_pattern.name] = new_pattern

        # Save updated patterns
        updated_patterns = list(pattern_map.values())
        self._save_patterns(updated_patterns)

        return updated_patterns

    def _scan_codebase_for_patterns(self) -> List[PatternDefinition]:
        """Scan entire codebase for architectural patterns"""
        patterns = []

        # Define pattern detection rules
        pattern_rules = {
            'Service Layer Architecture': {
                'extensions': ['.py'],
                'indicators': ['class', 'Service', 'def'],
                'category': 'Architecture'
            },
            'React Functional Components': {
                'extensions': ['.jsx'],
                'indicators': ['const', '= (', 'return'],
                'category': 'Frontend'
            },
            'Flask Blueprint Pattern': {
                'extensions': ['.py'],
                'indicators': ['Blueprint', '@bp.route'],
                'category': 'Backend'
            },
            'SQLAlchemy Models': {
                'extensions': ['.py'],
                'indicators': ['db.Model', 'class'],
                'category': 'Database'
            },
            'ANTLR Grammar': {
                'extensions': ['.g4'],
                'indicators': ['grammar', 'rule'],
                'category': 'Parser'
            }
        }

        # Scan files
        for root, dirs, files in os.walk(self.project_root):
            # Skip node_modules, .git, etc.
            dirs[:] = [d for d in dirs if not d.startswith('.') and d != 'node_modules']

            for file in files:
                file_path = os.path.join(root, file)
                relative_path = os.path.relpath(file_path, self.project_root)

                for pattern_name, rule in pattern_rules.items():
                    if any(file.endswith(ext) for ext in rule['extensions']):
                        if self._file_matches_pattern(file_path, rule['indicators']):
                            patterns.append(PatternDefinition(
                                name=pattern_name,
                                description=f"Pattern found in {rule['category']} layer",
                                files_involved=[relative_path],
                                examples=[f"Found in {relative_path}"],
                                frequency=1,
                                last_seen=datetime.datetime.now().isoformat(),
                                category=rule['category']
                            ))

        return patterns

    def _file_matches_pattern(self, file_path: str, indicators: List[str]) -> bool:
        """Check if a file matches pattern indicators"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                return all(indicator in content for indicator in indicators)
        except:
            return False

    def _save_patterns(self, patterns: List[PatternDefinition]):
        """Save patterns to database"""
        try:
            with open(self.patterns_db, 'w') as f:
                json.dump([asdict(p) for p in patterns], f, indent=2)
        except Exception as e:
            print(f"Warning: Could not save patterns: {e}")

    def identify_knowledge_gaps(self) -> List[KnowledgeGap]:
        """Identify areas where project knowledge is incomplete"""
        print("🔍 Identifying knowledge gaps...")

        gaps = []

        # Check for missing documentation
        critical_areas = [
            'API Documentation',
            'Database Schema Documentation',
            'Frontend Component Documentation',
            'Java Bridge Documentation',
            'Deployment Documentation',
            'Testing Documentation'
        ]

        for area in critical_areas:
            if not self._has_adequate_documentation(area):
                gaps.append(KnowledgeGap(
                    area=area,
                    description=f"Missing or incomplete documentation for {area}",
                    severity="medium",
                    suggested_documentation=f"Create comprehensive {area.lower()}",
                    related_files=[],
                    discovered_date=datetime.datetime.now().isoformat()
                ))

        # Check for inconsistent patterns
        gaps.extend(self._identify_pattern_inconsistencies())

        # Save gaps
        self._save_knowledge_gaps(gaps)

        return gaps

    def _has_adequate_documentation(self, area: str) -> bool:
        """Check if area has adequate documentation"""
        doc_files = [f for f in os.listdir(self.project_root) if f.endswith('.md')]

        area_keywords = {
            'API Documentation': ['api', 'endpoint'],
            'Database Schema Documentation': ['schema', 'database', 'model'],
            'Frontend Component Documentation': ['component', 'frontend', 'ui'],
            'Java Bridge Documentation': ['java', 'bridge', 'antlr'],
            'Deployment Documentation': ['deploy', 'setup', 'install'],
            'Testing Documentation': ['test', 'testing', 'validation']
        }

        keywords = area_keywords.get(area, [])

        for doc_file in doc_files:
            doc_path = os.path.join(self.project_root, doc_file)
            try:
                with open(doc_path, 'r') as f:
                    content = f.read().lower()
                    if any(keyword in content for keyword in keywords):
                        if len(content) > 1000:  # Substantial documentation
                            return True
            except:
                pass

        return False

    def _identify_pattern_inconsistencies(self) -> List[KnowledgeGap]:
        """Identify inconsistencies in established patterns"""
        gaps = []

        # Load patterns
        try:
            with open(self.patterns_db, 'r') as f:
                patterns_data = json.load(f)
                patterns = [PatternDefinition(**p) for p in patterns_data]
        except:
            patterns = []

        # Look for pattern violations
        for pattern in patterns:
            if pattern.frequency < 2:  # Pattern not well established
                gaps.append(KnowledgeGap(
                    area=f"Pattern Consistency - {pattern.name}",
                    description=f"Pattern '{pattern.name}' is not consistently applied",
                    severity="low",
                    suggested_documentation=f"Document and standardize {pattern.name} pattern",
                    related_files=pattern.files_involved,
                    discovered_date=datetime.datetime.now().isoformat()
                ))

        return gaps

    def _save_knowledge_gaps(self, gaps: List[KnowledgeGap]):
        """Save knowledge gaps to database"""
        try:
            with open(self.knowledge_gaps_db, 'w') as f:
                json.dump([asdict(g) for g in gaps], f, indent=2)
        except Exception as e:
            print(f"Warning: Could not save knowledge gaps: {e}")

    def update_claude_md(self, session_learning: SessionLearning, patterns: List[PatternDefinition]):
        """Update CLAUDE.md with consolidated knowledge"""
        print("📝 Updating CLAUDE.md with consolidated knowledge...")

        if not os.path.exists(self.claude_md_path):
            print("Warning: CLAUDE.md not found, skipping update")
            return

        # Read current CLAUDE.md
        with open(self.claude_md_path, 'r') as f:
            content = f.read()

        # Add session learning section
        timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")

        session_section = f"""

## SESSION CONSOLIDATION - {timestamp}

### Session Learning Summary
- **Modified Files**: {len(session_learning.modified_files)} files changed
- **New Patterns Identified**: {len(session_learning.new_patterns)}
- **Decisions Made**: {len(session_learning.decisions_made)}
- **Knowledge Gaps Filled**: {len(session_learning.knowledge_gaps_filled)}

### Key Patterns Discovered
"""

        for pattern in patterns[-5:]:  # Show last 5 patterns
            session_section += f"- **{pattern.name}** ({pattern.category}): {pattern.description}\n"

        session_section += f"""
### Recent Decisions
"""
        for decision in session_learning.decisions_made[-3:]:  # Show last 3 decisions
            session_section += f"- {decision}\n"

        session_section += f"""
### Critical Insights
"""
        for insight in session_learning.critical_insights:
            session_section += f"- {insight}\n"

        # Append to CLAUDE.md
        updated_content = content + session_section

        with open(self.claude_md_path, 'w') as f:
            f.write(updated_content)

        print(f"✅ CLAUDE.md updated with session consolidation")

    def optimize_for_context_recovery(self):
        """Optimize CLAUDE.md structure for quick context recovery"""
        print("⚡ Optimizing CLAUDE.md for context recovery...")

        if not os.path.exists(self.claude_md_path):
            return

        with open(self.claude_md_path, 'r') as f:
            content = f.read()

        # Create quick reference section at the top if it doesn't exist
        quick_ref_marker = "## QUICK CONTEXT RECOVERY"

        if quick_ref_marker not in content:
            # Extract key information for quick reference
            quick_ref = f"""

{quick_ref_marker}

### 🚀 Essential Information (Last Updated: {datetime.datetime.now().strftime("%Y-%m-%d %H:%M")})

**Project**: Credit Card Processing Rules Engine
**Architecture**: React (3000) → Flask (5001) → Java Engine
**Database**: SQLite with unified Rules/ActionSets table
**Status Values**: DRAFT, VALID, PEND, SCHD, PROD (uppercase only)

**Critical Files**:
- `/backend/models.py` - Database models (unified table)
- `/backend/services/rule_service.py` - Core business logic
- `/frontend/src/components/RuleEditor.jsx` - Main UI component
- `/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4` - Grammar definition

**Key Patterns**:
- Service Layer: API → Service → Model architecture
- Status Auto-promotion: DRAFT → VALID on successful validation
- File Paths: Always use absolute paths
- UI Components: Functional components with hooks pattern

**Emergency Commands**:
```bash
# Start services
cd backend && python app.py &
cd frontend && npm start &

# Health check
curl http://localhost:5001/api/health
curl http://localhost:3000

# Run tests
python backend/test_regression_suite.py
```

---
"""

            # Insert at the beginning after the title
            lines = content.split('\n')
            title_end = 0
            for i, line in enumerate(lines):
                if line.startswith('---') and i > 5:
                    title_end = i + 1
                    break

            if title_end > 0:
                new_content = '\n'.join(lines[:title_end]) + quick_ref + '\n'.join(lines[title_end:])

                with open(self.claude_md_path, 'w') as f:
                    f.write(new_content)

                print("✅ Quick context recovery section added to CLAUDE.md")

    def create_cross_session_bridge(self):
        """Create bridge information for session continuity"""
        print("🌉 Creating cross-session continuity bridge...")

        bridge_file = os.path.join(self.memory_dir, "session_bridge.json")

        # Get current state
        current_state = {
            "timestamp": datetime.datetime.now().isoformat(),
            "git_status": self._get_git_status_summary(),
            "active_patterns": self._get_active_patterns_summary(),
            "current_focus": self._determine_current_focus(),
            "next_priorities": self._suggest_next_priorities(),
            "context_health": self._assess_context_health()
        }

        with open(bridge_file, 'w') as f:
            json.dump(current_state, f, indent=2)

        print(f"✅ Session bridge created: {bridge_file}")

        return current_state

    def _get_git_status_summary(self) -> Dict[str, Any]:
        """Get summary of git status"""
        try:
            result = subprocess.run(
                ['git', 'status', '--porcelain'],
                cwd=self.project_root,
                capture_output=True,
                text=True
            )

            modified = len([line for line in result.stdout.split('\n') if line.startswith(' M')])
            untracked = len([line for line in result.stdout.split('\n') if line.startswith('??')])

            return {
                "modified_files": modified,
                "untracked_files": untracked,
                "clean": modified == 0 and untracked == 0
            }
        except:
            return {"error": "Could not get git status"}

    def _get_active_patterns_summary(self) -> List[str]:
        """Get summary of currently active patterns"""
        try:
            with open(self.patterns_db, 'r') as f:
                patterns_data = json.load(f)

            # Get most frequent patterns
            patterns = sorted(patterns_data, key=lambda x: x.get('frequency', 0), reverse=True)
            return [p['name'] for p in patterns[:5]]
        except:
            return []

    def _determine_current_focus(self) -> str:
        """Determine what the current development focus is"""
        # Analyze recent commits and modified files
        modified_files = self._get_modified_files()

        if any('actionset' in f.lower() for f in modified_files):
            return "ActionSet implementation and testing"
        elif any('validation' in f.lower() for f in modified_files):
            return "Validation system improvements"
        elif any('ui' in f.lower() or 'component' in f.lower() for f in modified_files):
            return "UI/UX enhancements"
        elif any('api' in f.lower() for f in modified_files):
            return "API development and integration"
        else:
            return "General system maintenance and improvements"

    def _suggest_next_priorities(self) -> List[str]:
        """Suggest next development priorities based on current state"""
        priorities = []

        # Check knowledge gaps
        try:
            with open(self.knowledge_gaps_db, 'r') as f:
                gaps = json.load(f)

            critical_gaps = [g for g in gaps if g.get('severity') == 'critical']
            if critical_gaps:
                priorities.append("Address critical knowledge gaps")
        except:
            pass

        # Check for incomplete patterns
        try:
            with open(self.patterns_db, 'r') as f:
                patterns = json.load(f)

            incomplete_patterns = [p for p in patterns if p.get('frequency', 0) < 2]
            if incomplete_patterns:
                priorities.append("Standardize incomplete patterns")
        except:
            pass

        # Default priorities
        if not priorities:
            priorities = [
                "Continue current development focus",
                "Run regression test suite",
                "Update documentation"
            ]

        return priorities

    def _assess_context_health(self) -> Dict[str, Any]:
        """Assess the health of the current context"""
        health = {
            "claude_md_exists": os.path.exists(self.claude_md_path),
            "session_checkpoint_exists": os.path.exists(self.session_checkpoint_path),
            "memory_db_initialized": all(os.path.exists(db) for db in [
                self.patterns_db, self.knowledge_gaps_db, self.session_learnings_db
            ]),
            "git_repository": os.path.exists(os.path.join(self.project_root, '.git')),
            "services_accessible": self._check_services_health()
        }

        health["overall_score"] = sum(health.values()) / len(health)

        return health

    def _check_services_health(self) -> bool:
        """Check if key services are accessible"""
        try:
            import requests

            # Check backend
            backend_health = requests.get('http://localhost:5001/api/health', timeout=2)
            backend_ok = backend_health.status_code == 200

            # Check frontend (just connection, not full health)
            frontend_health = requests.get('http://localhost:3000', timeout=2)
            frontend_ok = frontend_health.status_code == 200

            return backend_ok and frontend_ok
        except:
            return False

    def generate_memory_report(self) -> str:
        """Generate comprehensive memory consolidation report"""
        print("📊 Generating memory consolidation report...")

        report_lines = [
            "# MEMORY CONSOLIDATION REPORT",
            f"Generated: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
            "",
            "## SUMMARY",
            ""
        ]

        # Session learning summary
        try:
            with open(self.session_learnings_db, 'r') as f:
                learnings = json.load(f)

            report_lines.extend([
                f"**Total Sessions Analyzed**: {len(learnings)}",
                f"**Latest Session**: {learnings[-1]['timestamp'] if learnings else 'None'}",
                ""
            ])
        except:
            report_lines.extend(["**Session Data**: Not available", ""])

        # Patterns summary
        try:
            with open(self.patterns_db, 'r') as f:
                patterns = json.load(f)

            categories = defaultdict(int)
            for pattern in patterns:
                categories[pattern.get('category', 'Unknown')] += 1

            report_lines.extend([
                "## PATTERNS IDENTIFIED",
                f"**Total Patterns**: {len(patterns)}",
                ""
            ])

            for category, count in categories.items():
                report_lines.append(f"- **{category}**: {count} patterns")

            report_lines.append("")

        except:
            report_lines.extend(["## PATTERNS", "Not available", ""])

        # Knowledge gaps summary
        try:
            with open(self.knowledge_gaps_db, 'r') as f:
                gaps = json.load(f)

            severity_counts = Counter(gap.get('severity', 'unknown') for gap in gaps)

            report_lines.extend([
                "## KNOWLEDGE GAPS",
                f"**Total Gaps**: {len(gaps)}",
                ""
            ])

            for severity, count in severity_counts.items():
                report_lines.append(f"- **{severity.title()}**: {count} gaps")

            report_lines.append("")

        except:
            report_lines.extend(["## KNOWLEDGE GAPS", "Not available", ""])

        # Context health
        health = self._assess_context_health()
        report_lines.extend([
            "## CONTEXT HEALTH",
            f"**Overall Score**: {health['overall_score']:.2%}",
            ""
        ])

        for component, status in health.items():
            if component != 'overall_score':
                status_icon = "✅" if status else "❌"
                report_lines.append(f"- {status_icon} {component.replace('_', ' ').title()}")

        report_lines.append("")

        # Recommendations
        report_lines.extend([
            "## RECOMMENDATIONS",
            ""
        ])

        priorities = self._suggest_next_priorities()
        for i, priority in enumerate(priorities, 1):
            report_lines.append(f"{i}. {priority}")

        return '\n'.join(report_lines)

    def run_full_consolidation(self):
        """Run complete memory consolidation process"""
        print("🚀 Starting full memory consolidation process...")
        print("=" * 60)

        try:
            # 1. Analyze current session
            session_learning = self.analyze_current_session()

            # 2. Consolidate patterns
            patterns = self.consolidate_patterns()

            # 3. Identify knowledge gaps
            gaps = self.identify_knowledge_gaps()

            # 4. Update CLAUDE.md
            self.update_claude_md(session_learning, patterns)

            # 5. Optimize for context recovery
            self.optimize_for_context_recovery()

            # 6. Create cross-session bridge
            bridge_state = self.create_cross_session_bridge()

            # 7. Generate report
            report = self.generate_memory_report()

            # Save report
            report_file = os.path.join(self.memory_dir, f"consolidation_report_{datetime.datetime.now().strftime('%Y%m%d_%H%M%S')}.md")
            with open(report_file, 'w') as f:
                f.write(report)

            print("=" * 60)
            print("✅ MEMORY CONSOLIDATION COMPLETED SUCCESSFULLY")
            print(f"📊 Report saved: {report_file}")
            print(f"🔍 Patterns identified: {len(patterns)}")
            print(f"📋 Knowledge gaps: {len(gaps)}")
            print(f"🌉 Session bridge created")
            print(f"📝 CLAUDE.md updated")

            return {
                "success": True,
                "session_learning": session_learning,
                "patterns_count": len(patterns),
                "knowledge_gaps_count": len(gaps),
                "report_file": report_file,
                "bridge_state": bridge_state
            }

        except Exception as e:
            print(f"❌ Error during consolidation: {e}")
            return {"success": False, "error": str(e)}

def main():
    """Main entry point for the memory consolidation agent"""
    print("Memory Consolidation Agent v1.0")
    print("Preventing context loss through systematic knowledge management")
    print()

    agent = MemoryConsolidationAgent()
    result = agent.run_full_consolidation()

    if result["success"]:
        print("\n🎉 Memory consolidation completed successfully!")
        print("Your project knowledge has been systematically preserved and organized.")
        print("CLAUDE.md has been updated with the latest consolidation.")
    else:
        print(f"\n❌ Consolidation failed: {result['error']}")
        return 1

    return 0

if __name__ == "__main__":
    exit(main())