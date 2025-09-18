#!/usr/bin/env python3
"""
CLAUDE.md Memory Manager

Specialized component for managing CLAUDE.md as the single source of truth for project memory.
This module handles automated updates, conflict resolution, and structure optimization.

Created: September 17, 2025
Version: 1.0
Purpose: Manage CLAUDE.md as authoritative project memory document
"""

import os
import re
import json
import datetime
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass
import difflib
import hashlib

@dataclass
class DocumentSection:
    """Represents a section in CLAUDE.md"""
    title: str
    level: int
    content: str
    line_start: int
    line_end: int
    last_updated: Optional[str] = None

@dataclass
class MemoryUpdate:
    """Represents an update to be made to project memory"""
    section: str
    content: str
    priority: str  # critical, high, medium, low
    source: str  # session, git, pattern_analysis, etc.
    timestamp: str

class ClaudeMemoryManager:
    """Manages CLAUDE.md as the authoritative project memory document"""

    def __init__(self, project_root: str = None):
        self.project_root = project_root or "/Users/chandramohn/workspace/rules_engine/ui-prototype"
        self.claude_md_path = os.path.join(self.project_root, "CLAUDE.md")
        self.backup_dir = os.path.join(self.project_root, ".memory_consolidation", "claude_backups")
        self.pending_updates_file = os.path.join(self.project_root, ".memory_consolidation", "pending_updates.json")

        # Ensure backup directory exists
        os.makedirs(self.backup_dir, exist_ok=True)

        # Section priorities for conflict resolution
        self.section_priorities = {
            "QUICK CONTEXT RECOVERY": "critical",
            "PROJECT STRUCTURE ANALYSIS": "critical",
            "TECHNOLOGY STACK": "high",
            "CORE FUNCTIONALITY": "high",
            "DEVELOPMENT PATTERNS": "high",
            "RECENT CHANGES & DEVELOPMENT FOCUS": "critical",
            "PATTERNS TO PRESERVE": "critical",
            "PERFORMANCE CHARACTERISTICS": "medium",
            "BUILD AND DEPLOYMENT PROCESSES": "medium",
            "CRITICAL FILE LOCATIONS": "high"
        }

    def create_backup(self) -> str:
        """Create timestamped backup of CLAUDE.md"""
        if not os.path.exists(self.claude_md_path):
            return None

        timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_file = os.path.join(self.backup_dir, f"CLAUDE_backup_{timestamp}.md")

        with open(self.claude_md_path, 'r') as source:
            with open(backup_file, 'w') as backup:
                backup.write(source.read())

        return backup_file

    def parse_document_structure(self) -> List[DocumentSection]:
        """Parse CLAUDE.md into structured sections"""
        if not os.path.exists(self.claude_md_path):
            return []

        with open(self.claude_md_path, 'r') as f:
            lines = f.readlines()

        sections = []
        current_section = None

        for i, line in enumerate(lines):
            # Detect header lines
            if line.startswith('#'):
                # Save previous section
                if current_section:
                    current_section.line_end = i - 1
                    current_section.content = ''.join(lines[current_section.line_start:i])
                    sections.append(current_section)

                # Start new section
                level = len(line) - len(line.lstrip('#'))
                title = line.strip('#').strip()

                current_section = DocumentSection(
                    title=title,
                    level=level,
                    content="",
                    line_start=i,
                    line_end=len(lines) - 1
                )

        # Add the last section
        if current_section:
            current_section.content = ''.join(lines[current_section.line_start:])
            sections.append(current_section)

        return sections

    def update_section(self, section_title: str, new_content: str, merge_strategy: str = "append") -> bool:
        """Update a specific section in CLAUDE.md"""
        backup_file = self.create_backup()

        try:
            sections = self.parse_document_structure()

            # Find target section
            target_section = None
            for section in sections:
                if section.title.upper() == section_title.upper():
                    target_section = section
                    break

            if not target_section:
                # Create new section
                return self._add_new_section(section_title, new_content)

            # Update existing section
            if merge_strategy == "replace":
                target_section.content = f"## {section_title}\n\n{new_content}\n\n"
            elif merge_strategy == "append":
                # Remove existing header from new content if present
                clean_content = re.sub(rf'^#+\s*{re.escape(section_title)}\s*$', '', new_content, flags=re.MULTILINE).strip()
                target_section.content += f"\n{clean_content}\n"
            elif merge_strategy == "prepend":
                clean_content = re.sub(rf'^#+\s*{re.escape(section_title)}\s*$', '', new_content, flags=re.MULTILINE).strip()
                header_line = f"## {section_title}\n\n"
                content_without_header = target_section.content.replace(header_line, "")
                target_section.content = f"{header_line}{clean_content}\n\n{content_without_header}"

            # Mark as updated
            target_section.last_updated = datetime.datetime.now().isoformat()

            # Rebuild document
            self._rebuild_document(sections)
            return True

        except Exception as e:
            # Restore from backup on error
            if backup_file and os.path.exists(backup_file):
                with open(backup_file, 'r') as backup:
                    with open(self.claude_md_path, 'w') as main:
                        main.write(backup.read())

            print(f"Error updating section {section_title}: {e}")
            return False

    def _add_new_section(self, section_title: str, content: str) -> bool:
        """Add a new section to CLAUDE.md"""
        try:
            with open(self.claude_md_path, 'a') as f:
                f.write(f"\n\n## {section_title}\n\n{content}\n")
            return True
        except Exception as e:
            print(f"Error adding new section {section_title}: {e}")
            return False

    def _rebuild_document(self, sections: List[DocumentSection]):
        """Rebuild CLAUDE.md from sections"""
        with open(self.claude_md_path, 'w') as f:
            for section in sections:
                f.write(section.content)

    def consolidate_duplicate_sections(self) -> List[str]:
        """Identify and consolidate duplicate or similar sections"""
        sections = self.parse_document_structure()

        # Group sections by similarity
        similar_groups = self._find_similar_sections(sections)
        consolidated = []

        for group in similar_groups:
            if len(group) > 1:
                # Merge similar sections
                merged_section = self._merge_sections(group)
                consolidated.append(f"Consolidated {len(group)} sections into: {merged_section.title}")

        return consolidated

    def _find_similar_sections(self, sections: List[DocumentSection]) -> List[List[DocumentSection]]:
        """Find groups of similar sections"""
        groups = []
        processed = set()

        for i, section1 in enumerate(sections):
            if i in processed:
                continue

            current_group = [section1]
            processed.add(i)

            for j, section2 in enumerate(sections[i+1:], i+1):
                if j in processed:
                    continue

                # Check similarity
                if self._sections_are_similar(section1, section2):
                    current_group.append(section2)
                    processed.add(j)

            groups.append(current_group)

        return [group for group in groups if len(group) > 1]

    def _sections_are_similar(self, section1: DocumentSection, section2: DocumentSection) -> bool:
        """Check if two sections are similar enough to merge"""
        # Check title similarity
        title_similarity = difflib.SequenceMatcher(None, section1.title.lower(), section2.title.lower()).ratio()

        # Check content similarity
        content_similarity = difflib.SequenceMatcher(None, section1.content, section2.content).ratio()

        # Consider similar if titles are very similar or content overlaps significantly
        return title_similarity > 0.8 or content_similarity > 0.6

    def _merge_sections(self, sections: List[DocumentSection]) -> DocumentSection:
        """Merge multiple sections into one"""
        # Use the longest title as the base
        primary_section = max(sections, key=lambda s: len(s.title))

        # Combine content, removing duplicates
        combined_content = f"## {primary_section.title}\n\n"

        content_lines = set()
        for section in sections:
            # Extract content without header
            clean_content = re.sub(r'^#+.*$', '', section.content, flags=re.MULTILINE)
            for line in clean_content.split('\n'):
                if line.strip():
                    content_lines.add(line.strip())

        combined_content += '\n'.join(sorted(content_lines)) + '\n'

        primary_section.content = combined_content
        primary_section.last_updated = datetime.datetime.now().isoformat()

        return primary_section

    def optimize_document_structure(self) -> Dict[str, Any]:
        """Optimize CLAUDE.md structure for better organization and searchability"""
        optimization_results = {
            "sections_reorganized": 0,
            "duplicates_removed": 0,
            "quick_ref_updated": False,
            "toc_created": False
        }

        try:
            # 1. Create/update table of contents
            self._create_table_of_contents()
            optimization_results["toc_created"] = True

            # 2. Ensure quick reference section exists and is at the top
            self._ensure_quick_reference_section()
            optimization_results["quick_ref_updated"] = True

            # 3. Consolidate duplicate sections
            duplicates = self.consolidate_duplicate_sections()
            optimization_results["duplicates_removed"] = len(duplicates)

            # 4. Reorganize sections in logical order
            self._reorganize_sections()
            optimization_results["sections_reorganized"] = 1

            # 5. Clean up formatting
            self._clean_formatting()

        except Exception as e:
            print(f"Error during optimization: {e}")

        return optimization_results

    def _create_table_of_contents(self):
        """Create or update table of contents"""
        sections = self.parse_document_structure()

        # Generate TOC
        toc_lines = ["## TABLE OF CONTENTS\n"]

        for section in sections:
            if section.title not in ["TABLE OF CONTENTS", "QUICK CONTEXT RECOVERY"]:
                indent = "  " * (section.level - 2) if section.level > 2 else ""
                toc_lines.append(f"{indent}- [{section.title}](#{section.title.lower().replace(' ', '-')})")

        toc_content = '\n'.join(toc_lines) + '\n\n'

        # Insert TOC after the title
        self.update_section("TABLE OF CONTENTS", toc_content, "replace")

    def _ensure_quick_reference_section(self):
        """Ensure quick reference section exists at the top"""
        sections = self.parse_document_structure()

        # Check if quick reference exists
        has_quick_ref = any(s.title.upper() == "QUICK CONTEXT RECOVERY" for s in sections)

        if not has_quick_ref:
            # Create quick reference section
            quick_ref_content = self._generate_quick_reference_content()

            # Insert at the beginning (after title and basic info)
            with open(self.claude_md_path, 'r') as f:
                content = f.read()

            # Find insertion point (after first horizontal rule)
            insertion_point = content.find('---', content.find('---') + 3)
            if insertion_point == -1:
                insertion_point = content.find('\n## ')

            if insertion_point != -1:
                new_content = (content[:insertion_point] +
                             f"\n\n{quick_ref_content}\n" +
                             content[insertion_point:])

                with open(self.claude_md_path, 'w') as f:
                    f.write(new_content)

    def _generate_quick_reference_content(self) -> str:
        """Generate quick reference content from current state"""
        return """## QUICK CONTEXT RECOVERY

### 🚀 Essential Information (Auto-Generated)

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
# Verify environment
pwd  # Should be: /Users/chandramohn/workspace/rules_engine/ui-prototype

# Start services
cd backend && python app.py &
cd frontend && npm start &

# Health check
curl http://localhost:5001/api/health
curl http://localhost:3000

# Run tests
python backend/test_regression_suite.py
```

---"""

    def _reorganize_sections(self):
        """Reorganize sections in logical order"""
        sections = self.parse_document_structure()

        # Define ideal section order
        ideal_order = [
            "QUICK CONTEXT RECOVERY",
            "TABLE OF CONTENTS",
            "PROJECT STRUCTURE ANALYSIS",
            "TECHNOLOGY STACK",
            "CORE FUNCTIONALITY",
            "DEVELOPMENT PATTERNS",
            "RECENT CHANGES & DEVELOPMENT FOCUS",
            "BUILD AND DEPLOYMENT PROCESSES",
            "PERFORMANCE CHARACTERISTICS",
            "PATTERNS TO PRESERVE",
            "CRITICAL FILE LOCATIONS"
        ]

        # Reorder sections
        ordered_sections = []
        remaining_sections = sections.copy()

        # Add sections in ideal order
        for ideal_title in ideal_order:
            for section in sections:
                if section.title.upper() == ideal_title.upper():
                    ordered_sections.append(section)
                    remaining_sections.remove(section)
                    break

        # Add any remaining sections
        ordered_sections.extend(remaining_sections)

        # Rebuild document with new order
        self._rebuild_document(ordered_sections)

    def _clean_formatting(self):
        """Clean up document formatting"""
        with open(self.claude_md_path, 'r') as f:
            content = f.read()

        # Remove excessive blank lines
        content = re.sub(r'\n\n\n+', '\n\n', content)

        # Ensure proper spacing around headers
        content = re.sub(r'\n(#+\s+[^\n]+)\n([^\n])', r'\n\1\n\n\2', content)

        # Clean up end of file
        content = content.rstrip() + '\n'

        with open(self.claude_md_path, 'w') as f:
            f.write(content)

    def queue_memory_update(self, update: MemoryUpdate):
        """Queue a memory update for later processing"""
        pending_updates = []

        if os.path.exists(self.pending_updates_file):
            with open(self.pending_updates_file, 'r') as f:
                pending_updates = json.load(f)

        pending_updates.append({
            "section": update.section,
            "content": update.content,
            "priority": update.priority,
            "source": update.source,
            "timestamp": update.timestamp
        })

        with open(self.pending_updates_file, 'w') as f:
            json.dump(pending_updates, f, indent=2)

    def process_pending_updates(self) -> List[str]:
        """Process all pending memory updates"""
        if not os.path.exists(self.pending_updates_file):
            return []

        with open(self.pending_updates_file, 'r') as f:
            pending_updates = json.load(f)

        processed = []

        # Sort by priority
        priority_order = {"critical": 0, "high": 1, "medium": 2, "low": 3}
        pending_updates.sort(key=lambda x: priority_order.get(x["priority"], 3))

        for update in pending_updates:
            try:
                success = self.update_section(
                    update["section"],
                    update["content"],
                    "append"
                )
                if success:
                    processed.append(f"Updated {update['section']} from {update['source']}")
                else:
                    processed.append(f"Failed to update {update['section']}")
            except Exception as e:
                processed.append(f"Error updating {update['section']}: {e}")

        # Clear pending updates
        with open(self.pending_updates_file, 'w') as f:
            json.dump([], f)

        return processed

    def validate_memory_integrity(self) -> Dict[str, Any]:
        """Validate the integrity of the memory document"""
        validation_results = {
            "file_exists": os.path.exists(self.claude_md_path),
            "parseable": True,
            "has_essential_sections": True,
            "no_corruption": True,
            "word_count": 0,
            "section_count": 0,
            "last_modified": None,
            "issues": []
        }

        if not validation_results["file_exists"]:
            validation_results["issues"].append("CLAUDE.md file does not exist")
            return validation_results

        try:
            # Basic file stats
            stat = os.stat(self.claude_md_path)
            validation_results["last_modified"] = datetime.datetime.fromtimestamp(stat.st_mtime).isoformat()

            with open(self.claude_md_path, 'r') as f:
                content = f.read()
                validation_results["word_count"] = len(content.split())

            # Parse structure
            sections = self.parse_document_structure()
            validation_results["section_count"] = len(sections)

            # Check for essential sections
            essential_sections = [
                "PROJECT STRUCTURE ANALYSIS",
                "TECHNOLOGY STACK",
                "CORE FUNCTIONALITY",
                "DEVELOPMENT PATTERNS"
            ]

            existing_sections = [s.title.upper() for s in sections]
            missing_sections = [s for s in essential_sections if s not in existing_sections]

            if missing_sections:
                validation_results["has_essential_sections"] = False
                validation_results["issues"].extend([f"Missing section: {s}" for s in missing_sections])

            # Check for corruption signs
            corruption_signs = [
                (len(content) < 1000, "Document too short"),
                (content.count('##') < 5, "Too few sections"),
                ('�' in content, "Contains replacement characters"),
                (content.count('\n\n\n\n') > 5, "Excessive blank lines")
            ]

            for condition, message in corruption_signs:
                if condition:
                    validation_results["no_corruption"] = False
                    validation_results["issues"].append(message)

        except Exception as e:
            validation_results["parseable"] = False
            validation_results["issues"].append(f"Parse error: {e}")

        return validation_results

    def generate_memory_health_report(self) -> str:
        """Generate a comprehensive memory health report"""
        validation = self.validate_memory_integrity()

        report_lines = [
            "# CLAUDE.md MEMORY HEALTH REPORT",
            f"Generated: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
            "",
            "## HEALTH STATUS",
            ""
        ]

        # Overall health score
        health_score = sum([
            validation["file_exists"],
            validation["parseable"],
            validation["has_essential_sections"],
            validation["no_corruption"]
        ]) / 4

        health_emoji = "🟢" if health_score >= 0.75 else "🟡" if health_score >= 0.5 else "🔴"
        report_lines.append(f"**Overall Health**: {health_emoji} {health_score:.0%}")
        report_lines.append("")

        # Detailed metrics
        report_lines.extend([
            "## METRICS",
            f"- **Word Count**: {validation['word_count']:,}",
            f"- **Section Count**: {validation['section_count']}",
            f"- **Last Modified**: {validation['last_modified'] or 'Unknown'}",
            ""
        ])

        # Issues
        if validation["issues"]:
            report_lines.extend([
                "## ISSUES DETECTED",
                ""
            ])
            for issue in validation["issues"]:
                report_lines.append(f"⚠️ {issue}")
            report_lines.append("")

        # Recommendations
        report_lines.extend([
            "## RECOMMENDATIONS",
            ""
        ])

        if not validation["file_exists"]:
            report_lines.append("🔧 Create CLAUDE.md file immediately")
        elif not validation["has_essential_sections"]:
            report_lines.append("🔧 Add missing essential sections")
        elif not validation["no_corruption"]:
            report_lines.append("🔧 Fix document corruption issues")
        else:
            report_lines.append("✅ Memory document is healthy")

        if validation["word_count"] < 5000:
            report_lines.append("📝 Consider expanding documentation")
        elif validation["word_count"] > 50000:
            report_lines.append("✂️ Consider splitting into multiple documents")

        return '\n'.join(report_lines)

def main():
    """Main entry point for Claude Memory Manager"""
    print("Claude Memory Manager v1.0")
    print("Managing CLAUDE.md as the authoritative project memory")
    print()

    manager = ClaudeMemoryManager()

    # Validate current state
    validation = manager.validate_memory_integrity()
    print(f"Memory validation: {'✅' if validation['file_exists'] and validation['parseable'] else '❌'}")

    # Process any pending updates
    processed_updates = manager.process_pending_updates()
    if processed_updates:
        print(f"Processed {len(processed_updates)} pending updates")

    # Optimize document structure
    optimization = manager.optimize_document_structure()
    print(f"Optimization complete: {optimization}")

    # Generate health report
    health_report = manager.generate_memory_health_report()
    print("\n" + health_report)

    return 0

if __name__ == "__main__":
    exit(main())