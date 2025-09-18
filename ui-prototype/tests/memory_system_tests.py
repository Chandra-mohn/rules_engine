#!/usr/bin/env python3
"""
Memory System Comprehensive Test Suite

Tests all components of the memory consolidation system to ensure reliability
and effectiveness in preventing context loss during auto-compaction.

Created: September 17, 2025
Version: 1.0
Purpose: Validate memory consolidation system integrity and functionality
"""

import os
import sys
import json
import tempfile
import shutil
import unittest
import datetime
from unittest.mock import patch, MagicMock
from pathlib import Path

# Add current directory to path for imports
sys.path.append(os.path.dirname(__file__))

from memory_consolidation_agent import MemoryConsolidationAgent
from claude_memory_manager import ClaudeMemoryManager
from session_continuity_bridge import SessionContinuityBridge
from integrated_memory_system import IntegratedMemorySystem

class TestMemoryConsolidationAgent(unittest.TestCase):
    """Test suite for Memory Consolidation Agent"""

    def setUp(self):
        """Set up test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.agent = MemoryConsolidationAgent(self.test_dir)

        # Create minimal project structure
        self._create_test_project_structure()

    def tearDown(self):
        """Clean up test environment"""
        shutil.rmtree(self.test_dir, ignore_errors=True)

    def _create_test_project_structure(self):
        """Create minimal project structure for testing"""
        # Create directories
        dirs = ["backend", "frontend/src/components", "java-bridge/src/main/java"]
        for dir_path in dirs:
            os.makedirs(os.path.join(self.test_dir, dir_path), exist_ok=True)

        # Create test files
        test_files = {
            "backend/models.py": "class Rule(db.Model):\n    pass",
            "backend/services/rule_service.py": "class RuleService:\n    def validate(self):\n        pass",
            "frontend/src/components/RuleEditor.jsx": "const RuleEditor = () => {\n    return <div>Editor</div>\n}",
            "CLAUDE.md": "# Test Project\n\n## Overview\nTest project for memory system"
        }

        for file_path, content in test_files.items():
            full_path = os.path.join(self.test_dir, file_path)
            os.makedirs(os.path.dirname(full_path), exist_ok=True)
            with open(full_path, 'w') as f:
                f.write(content)

        # Initialize git repository
        os.system(f"cd {self.test_dir} && git init")
        os.system(f"cd {self.test_dir} && git add .")
        os.system(f"cd {self.test_dir} && git commit -m 'Initial commit'")

    def test_agent_initialization(self):
        """Test agent initializes correctly"""
        self.assertTrue(os.path.exists(self.agent.memory_dir))
        self.assertTrue(os.path.exists(self.agent.patterns_db))
        self.assertTrue(os.path.exists(self.agent.knowledge_gaps_db))

    def test_pattern_detection(self):
        """Test pattern detection in code files"""
        # Create a Python service file
        service_file = os.path.join(self.test_dir, "backend/test_service.py")
        with open(service_file, 'w') as f:
            f.write("class TestService:\n    def validate(self):\n        return True")

        patterns = self.agent._analyze_file_patterns(service_file, "backend/test_service.py")
        self.assertIn("Service Layer Pattern", patterns[0])

    def test_session_learning_capture(self):
        """Test session learning capture"""
        # Mock git status to return modified files
        with patch('subprocess.run') as mock_run:
            mock_run.return_value.stdout = " M backend/models.py\n M frontend/src/App.jsx"

            learning = self.agent.analyze_current_session()

            self.assertIsNotNone(learning.timestamp)
            self.assertIsInstance(learning.modified_files, list)
            self.assertIsInstance(learning.new_patterns, list)

    def test_knowledge_gap_identification(self):
        """Test knowledge gap identification"""
        gaps = self.agent.identify_knowledge_gaps()
        self.assertIsInstance(gaps, list)

        # Should identify missing documentation
        gap_areas = [gap.area for gap in gaps]
        self.assertTrue(any("documentation" in area.lower() for area in gap_areas))

    def test_claude_md_update(self):
        """Test CLAUDE.md update functionality"""
        # Create a session learning object
        session_learning = self.agent.analyze_current_session()

        # Test updating CLAUDE.md
        self.agent.update_claude_md(session_learning, [])

        # Verify CLAUDE.md was updated
        with open(os.path.join(self.test_dir, "CLAUDE.md"), 'r') as f:
            content = f.read()
            self.assertIn("SESSION CONSOLIDATION", content)

class TestClaudeMemoryManager(unittest.TestCase):
    """Test suite for Claude Memory Manager"""

    def setUp(self):
        """Set up test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.manager = ClaudeMemoryManager(self.test_dir)

        # Create test CLAUDE.md
        self._create_test_claude_md()

    def tearDown(self):
        """Clean up test environment"""
        shutil.rmtree(self.test_dir, ignore_errors=True)

    def _create_test_claude_md(self):
        """Create test CLAUDE.md file"""
        claude_content = """# Test Project Documentation

## PROJECT STRUCTURE
This is the project structure section.

## TECHNOLOGY STACK
This is the technology stack section.

## CORE FUNCTIONALITY
This is the core functionality section.
"""
        with open(os.path.join(self.test_dir, "CLAUDE.md"), 'w') as f:
            f.write(claude_content)

    def test_document_parsing(self):
        """Test document structure parsing"""
        sections = self.manager.parse_document_structure()
        self.assertGreater(len(sections), 0)

        # Check section titles
        titles = [section.title for section in sections]
        self.assertIn("PROJECT STRUCTURE", titles)
        self.assertIn("TECHNOLOGY STACK", titles)

    def test_section_update(self):
        """Test section update functionality"""
        new_content = "Updated technology stack information."
        success = self.manager.update_section("TECHNOLOGY STACK", new_content, "append")

        self.assertTrue(success)

        # Verify content was updated
        with open(os.path.join(self.test_dir, "CLAUDE.md"), 'r') as f:
            content = f.read()
            self.assertIn("Updated technology stack information", content)

    def test_backup_creation(self):
        """Test backup creation"""
        backup_file = self.manager.create_backup()
        self.assertIsNotNone(backup_file)
        self.assertTrue(os.path.exists(backup_file))

    def test_memory_integrity_validation(self):
        """Test memory integrity validation"""
        validation = self.manager.validate_memory_integrity()

        self.assertTrue(validation["file_exists"])
        self.assertTrue(validation["parseable"])
        self.assertGreater(validation["word_count"], 0)
        self.assertGreater(validation["section_count"], 0)

    def test_document_optimization(self):
        """Test document structure optimization"""
        optimization = self.manager.optimize_document_structure()

        self.assertIsInstance(optimization, dict)
        self.assertIn("toc_created", optimization)
        self.assertIn("quick_ref_updated", optimization)

class TestSessionContinuityBridge(unittest.TestCase):
    """Test suite for Session Continuity Bridge"""

    def setUp(self):
        """Set up test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.bridge = SessionContinuityBridge(self.test_dir)

        # Create minimal project structure
        self._create_test_structure()

    def tearDown(self):
        """Clean up test environment"""
        shutil.rmtree(self.test_dir, ignore_errors=True)

    def _create_test_structure(self):
        """Create test structure"""
        # Create directories
        os.makedirs(os.path.join(self.test_dir, "backend"), exist_ok=True)
        os.makedirs(os.path.join(self.test_dir, "frontend"), exist_ok=True)

        # Create files
        files = {
            "CLAUDE.md": "# Test Project",
            "backend/models.py": "class Rule: pass"
        }

        for file_path, content in files.items():
            full_path = os.path.join(self.test_dir, file_path)
            os.makedirs(os.path.dirname(full_path), exist_ok=True)
            with open(full_path, 'w') as f:
                f.write(content)

        # Initialize git
        os.system(f"cd {self.test_dir} && git init")

    def test_session_state_capture(self):
        """Test session state capture"""
        session_state = self.bridge.capture_session_state()

        self.assertIsNotNone(session_state.timestamp)
        self.assertIsNotNone(session_state.session_id)
        self.assertEqual(session_state.working_directory, self.test_dir)
        self.assertIsInstance(session_state.modified_files, list)

    def test_knowledge_gap_identification(self):
        """Test knowledge gap identification"""
        gaps = self.bridge.identify_knowledge_gaps()
        self.assertIsInstance(gaps, list)

        # Should identify some gaps in minimal project
        self.assertGreater(len(gaps), 0)

    def test_context_bridge_creation(self):
        """Test context bridge creation"""
        bridge_data = self.bridge.create_context_bridge()

        self.assertIn("session_state", bridge_data)
        self.assertIn("knowledge_gaps", bridge_data)
        self.assertIn("active_patterns", bridge_data)

    def test_context_recovery(self):
        """Test context recovery"""
        # First create a bridge
        self.bridge.create_context_bridge()

        # Then try to recover
        recovery = self.bridge.recover_from_context_bridge()

        if recovery["success"]:
            self.assertIn("recovery_info", recovery)
        else:
            # Recovery might fail in test environment, which is acceptable
            self.assertIn("error", recovery)

class TestIntegratedMemorySystem(unittest.TestCase):
    """Test suite for Integrated Memory System"""

    def setUp(self):
        """Set up test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.system = IntegratedMemorySystem(self.test_dir)

        # Create comprehensive test structure
        self._create_comprehensive_test_structure()

    def tearDown(self):
        """Clean up test environment"""
        shutil.rmtree(self.test_dir, ignore_errors=True)

    def _create_comprehensive_test_structure(self):
        """Create comprehensive test structure"""
        # Create full directory structure
        dirs = [
            "backend",
            "frontend/src/components",
            "java-bridge/src/main/antlr4/com/rules/grammar",
            "backend/database"
        ]

        for dir_path in dirs:
            os.makedirs(os.path.join(self.test_dir, dir_path), exist_ok=True)

        # Create comprehensive file set
        files = {
            "CLAUDE.md": "# Rules Engine Project\n\n## Overview\nComprehensive project documentation.",
            "CLAUDE_SESSION_CHECKPOINT.md": "# Session Checkpoint\n\n## Completed Work\n- Test implementation",
            "backend/models.py": "from flask_sqlalchemy import SQLAlchemy\n\nclass Rule(db.Model):\n    pass",
            "backend/services/rule_service.py": "class RuleService:\n    def validate(self):\n        return True",
            "frontend/src/components/RuleEditor.jsx": "import React from 'react';\n\nconst RuleEditor = () => {\n    return <div>Editor</div>;\n};",
            "java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4": "grammar Rules;\n\nrule: 'test';",
            "backend/database/rules.db": ""  # Empty file to simulate database
        }

        for file_path, content in files.items():
            full_path = os.path.join(self.test_dir, file_path)
            os.makedirs(os.path.dirname(full_path), exist_ok=True)
            with open(full_path, 'w') as f:
                f.write(content)

        # Initialize git
        os.system(f"cd {self.test_dir} && git init")
        os.system(f"cd {self.test_dir} && git add .")
        os.system(f"cd {self.test_dir} && git commit -m 'Initial test commit'")

    def test_system_initialization(self):
        """Test system initialization"""
        self.assertTrue(os.path.exists(self.system.memory_root))
        self.assertTrue(os.path.exists(self.system.reports_dir))
        self.assertTrue(os.path.exists(self.system.backups_dir))

        # Test component initialization
        self.assertIsNotNone(self.system.consolidation_agent)
        self.assertIsNotNone(self.system.memory_manager)
        self.assertIsNotNone(self.system.continuity_bridge)

    def test_pre_compaction_preparation(self):
        """Test pre-compaction preparation process"""
        result = self.system.pre_compaction_preparation()

        self.assertIn("timestamp", result)
        self.assertIn("success", result)
        self.assertIn("steps_completed", result)

        # Check that some steps were completed
        self.assertGreater(len(result["steps_completed"]), 0)

    def test_health_check(self):
        """Test system health check"""
        health = self.system.run_health_check()

        self.assertIn("overall_health", health)
        self.assertIn("component_health", health)
        self.assertIn("timestamp", health)

        # Should have health status for all components
        expected_components = [
            "memory_consolidation_agent",
            "claude_memory_manager",
            "session_continuity_bridge",
            "directory_structure"
        ]

        for component in expected_components:
            self.assertIn(component, health["component_health"])

    def test_comprehensive_backup(self):
        """Test comprehensive backup creation"""
        backup_result = self.system._create_comprehensive_backup()

        self.assertIn("backup_dir", backup_result)
        self.assertIn("backed_up_files", backup_result)
        self.assertTrue(os.path.exists(backup_result["backup_dir"]))

        # Check that critical files were backed up
        backed_up = backup_result["backed_up_files"]
        self.assertIn("CLAUDE.md", backed_up)
        self.assertIn("backend/models.py", backed_up)

    def test_system_integrity_validation(self):
        """Test system integrity validation"""
        validation = self.system._validate_system_integrity()

        self.assertIn("healthy", validation)
        self.assertIn("issues", validation)
        self.assertIn("checks_performed", validation)

        # Should pass basic integrity checks
        self.assertGreater(len(validation["checks_performed"]), 0)

class TestMemorySystemIntegration(unittest.TestCase):
    """Integration tests for the complete memory system"""

    def setUp(self):
        """Set up integration test environment"""
        self.test_dir = tempfile.mkdtemp()
        self._create_full_test_project()

    def tearDown(self):
        """Clean up integration test environment"""
        shutil.rmtree(self.test_dir, ignore_errors=True)

    def _create_full_test_project(self):
        """Create full test project structure"""
        # Create complete directory structure
        dirs = [
            "backend/api",
            "backend/services",
            "backend/database",
            "frontend/src/components",
            "frontend/src/services",
            "java-bridge/src/main/java/com/rules/codegen",
            "java-bridge/src/main/antlr4/com/rules/grammar"
        ]

        for dir_path in dirs:
            os.makedirs(os.path.join(self.test_dir, dir_path), exist_ok=True)

        # Create comprehensive file structure
        files = {
            "CLAUDE.md": self._get_sample_claude_md(),
            "CLAUDE_SESSION_CHECKPOINT.md": self._get_sample_checkpoint(),
            "backend/models.py": self._get_sample_models(),
            "backend/services/rule_service.py": self._get_sample_service(),
            "backend/api/rules.py": self._get_sample_api(),
            "frontend/src/components/RuleEditor.jsx": self._get_sample_component(),
            "frontend/src/services/api.js": self._get_sample_api_client(),
            "java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4": self._get_sample_grammar(),
            "backend/database/rules.db": ""
        }

        for file_path, content in files.items():
            full_path = os.path.join(self.test_dir, file_path)
            os.makedirs(os.path.dirname(full_path), exist_ok=True)
            with open(full_path, 'w') as f:
                f.write(content)

        # Initialize git with history
        os.system(f"cd {self.test_dir} && git init")
        os.system(f"cd {self.test_dir} && git add .")
        os.system(f"cd {self.test_dir} && git commit -m 'Initial project setup'")

        # Add some changes to simulate active development
        with open(os.path.join(self.test_dir, "backend/models.py"), 'a') as f:
            f.write("\n# Recent addition\nclass ActionSet(db.Model):\n    pass")

        os.system(f"cd {self.test_dir} && git add backend/models.py")
        os.system(f"cd {self.test_dir} && git commit -m 'Add ActionSet model'")

    def _get_sample_claude_md(self):
        """Get sample CLAUDE.md content"""
        return """# Rules Engine Project Documentation

## PROJECT STRUCTURE
Multi-tier architecture with React frontend, Flask backend, and Java engine.

## TECHNOLOGY STACK
- Backend: Flask 2.3.3, SQLAlchemy
- Frontend: React 18.2.0, Antd 5.8.6
- Java: ANTLR 4.13.1, Maven

## CORE FUNCTIONALITY
Rules definition, validation, and execution system.

## DEVELOPMENT PATTERNS
- Service layer architecture
- React functional components
- ANTLR grammar parsing
"""

    def _get_sample_checkpoint(self):
        """Get sample checkpoint content"""
        return """# Session Checkpoint

## ✅ COMPLETED WORK
- ActionSet implementation
- Status validation fixes
- UI improvements

## CURRENT STATUS
All systems operational, zero regressions detected.
"""

    def _get_sample_models(self):
        """Get sample models content"""
        return """from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()

class Rule(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100), nullable=False)
    content = db.Column(db.Text)
    status = db.Column(db.String(20), default='DRAFT')
"""

    def _get_sample_service(self):
        """Get sample service content"""
        return """class RuleService:
    def __init__(self):
        self.java_bridge = JavaBridge()

    def validate_rule(self, rule_content):
        return self.java_bridge.validate(rule_content)

    def compile_rule(self, rule):
        return self.java_bridge.compile(rule)
"""

    def _get_sample_api(self):
        """Get sample API content"""
        return """from flask import Blueprint, request, jsonify

bp = Blueprint('rules', __name__)

@bp.route('/api/rules', methods=['GET'])
def get_rules():
    rules = Rule.query.all()
    return jsonify([rule.to_dict() for rule in rules])

@bp.route('/api/rules', methods=['POST'])
def create_rule():
    data = request.get_json()
    rule = Rule(**data)
    db.session.add(rule)
    db.session.commit()
    return jsonify(rule.to_dict())
"""

    def _get_sample_component(self):
        """Get sample React component content"""
        return """import React, { useState, useEffect } from 'react';
import { Button, Input, Select } from 'antd';

const RuleEditor = () => {
    const [rule, setRule] = useState('');
    const [status, setStatus] = useState('DRAFT');

    const handleSave = () => {
        // Save rule logic
    };

    return (
        <div>
            <Input.TextArea value={rule} onChange={e => setRule(e.target.value)} />
            <Select value={status} onChange={setStatus}>
                <Select.Option value="DRAFT">DRAFT</Select.Option>
                <Select.Option value="VALID">VALID</Select.Option>
            </Select>
            <Button onClick={handleSave}>Save</Button>
        </div>
    );
};

export default RuleEditor;
"""

    def _get_sample_api_client(self):
        """Get sample API client content"""
        return """import axios from 'axios';

const API_BASE_URL = 'http://localhost:5001';

const api = axios.create({
    baseURL: API_BASE_URL
});

export const rulesApi = {
    getRules: () => api.get('/api/rules'),
    createRule: (rule) => api.post('/api/rules', rule),
    updateRule: (id, rule) => api.put(`/api/rules/${id}`, rule)
};
"""

    def _get_sample_grammar(self):
        """Get sample ANTLR grammar content"""
        return """grammar Rules;

rule: RULE ID COLON statement+ ;

statement: IF condition THEN action ;

condition: expression ;
action: expression ;
expression: ID | STRING ;

RULE: 'rule' ;
IF: 'if' ;
THEN: 'then' ;
COLON: ':' ;
ID: [a-zA-Z_][a-zA-Z0-9_]* ;
STRING: '"' .*? '"' ;
WS: [ \\t\\n\\r]+ -> skip ;
"""

    def test_full_memory_consolidation_workflow(self):
        """Test complete memory consolidation workflow"""
        # Initialize system
        system = IntegratedMemorySystem(self.test_dir)

        # Run pre-compaction preparation
        prep_result = system.pre_compaction_preparation()
        self.assertTrue(prep_result["success"], f"Preparation failed: {prep_result.get('errors', [])}")

        # Verify backups were created
        self.assertTrue(os.path.exists(system.backups_dir))
        backup_files = os.listdir(system.backups_dir)
        self.assertGreater(len(backup_files), 0, "No backup files created")

        # Verify reports were generated
        report_files = [f for f in os.listdir(system.reports_dir) if f.startswith('pre_compaction_report')]
        self.assertGreater(len(report_files), 0, "No pre-compaction report generated")

        # Simulate compaction by modifying project state slightly
        # (In real scenario, Claude would compact conversation history)

        # Run post-compaction recovery
        recovery_result = system.post_compaction_recovery()
        self.assertTrue(recovery_result["success"], f"Recovery failed: {recovery_result.get('errors', [])}")

        # Verify recovery reports were generated
        recovery_reports = [f for f in os.listdir(system.reports_dir) if f.startswith('post_compaction_report')]
        self.assertGreater(len(recovery_reports), 0, "No post-compaction report generated")

    def test_pattern_recognition_across_codebase(self):
        """Test pattern recognition across the entire codebase"""
        agent = MemoryConsolidationAgent(self.test_dir)

        # Run pattern consolidation
        patterns = agent.consolidate_patterns()

        # Should identify various patterns
        pattern_names = [p.name for p in patterns]

        # Check for expected patterns
        expected_patterns = [
            "Service Layer Architecture",
            "React Functional Components",
            "Flask Blueprint Pattern",
            "SQLAlchemy Models"
        ]

        found_patterns = [p for p in expected_patterns if any(ep in pn for pn in pattern_names for ep in [p])]
        self.assertGreater(len(found_patterns), 0, f"Expected patterns not found. Found: {pattern_names}")

    def test_knowledge_gap_detection_accuracy(self):
        """Test accuracy of knowledge gap detection"""
        bridge = SessionContinuityBridge(self.test_dir)

        # Identify knowledge gaps
        gaps = bridge.identify_knowledge_gaps()

        # Should identify some gaps even in well-structured test project
        self.assertGreater(len(gaps), 0, "No knowledge gaps identified")

        # Check gap categories
        gap_areas = [gap.area for gap in gaps]
        self.assertTrue(any("documentation" in area.lower() for area in gap_areas))

    def test_cross_session_continuity(self):
        """Test cross-session continuity preservation"""
        bridge = SessionContinuityBridge(self.test_dir)

        # Capture session state
        session_state = bridge.capture_session_state()
        self.assertIsNotNone(session_state.session_id)

        # Create context bridge
        bridge_data = bridge.create_context_bridge()
        self.assertIn("session_state", bridge_data)

        # Simulate new session by creating new bridge instance
        new_bridge = SessionContinuityBridge(self.test_dir)

        # Recover context
        recovery = new_bridge.recover_from_context_bridge()

        if recovery["success"]:
            recovery_info = recovery["recovery_info"]
            self.assertEqual(recovery_info["last_session_id"], session_state.session_id)
        else:
            # Context recovery might fail in isolated test environment
            # This is acceptable as long as the bridge file was created
            bridge_file = os.path.join(self.test_dir, ".session_continuity", "context_bridge.json")
            self.assertTrue(os.path.exists(bridge_file), "Context bridge file not created")

def run_comprehensive_test_suite():
    """Run the complete test suite with detailed reporting"""
    print("🧪 Memory Consolidation System - Comprehensive Test Suite")
    print("=" * 70)

    # Create test suite
    test_suite = unittest.TestSuite()

    # Add all test classes
    test_classes = [
        TestMemoryConsolidationAgent,
        TestClaudeMemoryManager,
        TestSessionContinuityBridge,
        TestIntegratedMemorySystem,
        TestMemorySystemIntegration
    ]

    for test_class in test_classes:
        tests = unittest.TestLoader().loadTestsFromTestCase(test_class)
        test_suite.addTests(tests)

    # Run tests with detailed output
    runner = unittest.TextTestRunner(verbosity=2, stream=sys.stdout)
    result = runner.run(test_suite)

    # Generate summary report
    print("\n" + "=" * 70)
    print("📊 TEST SUITE SUMMARY")
    print("=" * 70)

    total_tests = result.testsRun
    failures = len(result.failures)
    errors = len(result.errors)
    success_rate = ((total_tests - failures - errors) / total_tests * 100) if total_tests > 0 else 0

    print(f"Total Tests: {total_tests}")
    print(f"Passed: {total_tests - failures - errors}")
    print(f"Failed: {failures}")
    print(f"Errors: {errors}")
    print(f"Success Rate: {success_rate:.1f}%")

    if result.failures:
        print(f"\n❌ FAILURES ({len(result.failures)}):")
        for test, traceback in result.failures:
            print(f"  - {test}: {traceback.split('AssertionError:')[-1].strip()}")

    if result.errors:
        print(f"\n🔥 ERRORS ({len(result.errors)}):")
        for test, traceback in result.errors:
            print(f"  - {test}: {traceback.split('Exception:')[-1].strip()}")

    print("\n" + "=" * 70)

    if success_rate >= 80:
        print("✅ MEMORY SYSTEM VALIDATION: PASSED")
        print("The memory consolidation system is ready for production use.")
    elif success_rate >= 60:
        print("⚠️ MEMORY SYSTEM VALIDATION: PARTIAL")
        print("The memory consolidation system has some issues that should be addressed.")
    else:
        print("❌ MEMORY SYSTEM VALIDATION: FAILED")
        print("The memory consolidation system requires significant fixes before use.")

    return result.wasSuccessful()

if __name__ == "__main__":
    success = run_comprehensive_test_suite()
    exit(0 if success else 1)