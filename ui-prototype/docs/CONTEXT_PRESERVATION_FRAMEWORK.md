# Context Preservation Framework for Auto-Compaction

**Version**: 1.0
**Created**: September 17, 2025
**Purpose**: Prevent regression issues during auto-compaction by systematically preserving development context

---

## 1. PRE-COMPACTION CONTEXT CAPTURE

### 1.1 Current Development State Documentation

#### Git Status Snapshot
```bash
# EXECUTE BEFORE COMPACTION
git status --porcelain > PRE_COMPACTION_GIT_STATUS.txt
git log --oneline -10 > RECENT_COMMITS.txt
git diff --name-status HEAD~5..HEAD > RECENT_CHANGES.txt
```

#### Current Working State
```bash
# Capture current state
echo "=== PRE-COMPACTION STATE SNAPSHOT ===" > CONTEXT_SNAPSHOT.md
echo "Date: $(date)" >> CONTEXT_SNAPSHOT.md
echo "Branch: $(git branch --show-current)" >> CONTEXT_SNAPSHOT.md
echo "Last Commit: $(git log -1 --oneline)" >> CONTEXT_SNAPSHOT.md
echo "" >> CONTEXT_SNAPSHOT.md

# Modified files
echo "## MODIFIED FILES:" >> CONTEXT_SNAPSHOT.md
git status --porcelain | grep "^ M" >> CONTEXT_SNAPSHOT.md
echo "" >> CONTEXT_SNAPSHOT.md

# Untracked files
echo "## UNTRACKED FILES:" >> CONTEXT_SNAPSHOT.md
git status --porcelain | grep "^??" >> CONTEXT_SNAPSHOT.md
echo "" >> CONTEXT_SNAPSHOT.md
```

### 1.2 Key Decisions and Patterns Capture

#### Critical Project Patterns
- **File Naming**: Absolute paths only (no relative paths)
- **Status Values**: Uppercase only (DRAFT, VALID, PEND, SCHD, PROD)
- **Architecture**: Multi-tier with React + Flask + Java
- **Database**: Single status field (validation_status consolidated)
- **API Pattern**: RESTful with service layer separation

#### Recent Implementation Decisions
- **ActionSet Support**: Unified table with `item_type` field
- **Status Auto-promotion**: DRAFT → VALID on successful validation
- **UI Consistency**: Single status column display
- **Code Generation**: Location-based display instead of content dump

### 1.3 Critical Context Data

#### Development Environment
```json
{
  "working_directory": "/Users/chandramohn/workspace/rules_engine/ui-prototype",
  "backend_port": 5001,
  "frontend_port": 3000,
  "java_bridge": "Maven-based ANTLR parser",
  "database": "SQLite with consolidated status field",
  "current_branch": "main"
}
```

#### Active Features
- **Rules Engine**: ANTLR-based grammar parsing
- **Code Generation**: Java bytecode compilation
- **UI Components**: Monaco editor with Antd components
- **Validation Pipeline**: Multi-layer validation system
- **Status Management**: Auto-promotion and manual override

---

## 2. SESSION MEMORY MANAGEMENT

### 2.1 Update CLAUDE.md with New Learnings

#### Critical Sections to Maintain
1. **Technology Stack** - Keep dependency versions current
2. **Core Functionality** - Document any new features
3. **Development Patterns** - Add new conventions discovered
4. **Recent Changes** - Update git status and progress
5. **Critical File Locations** - Add new important files

#### New Patterns Template
```markdown
### [Date] Session Additions
- **New Feature**: [Description]
- **Pattern**: [Convention or approach]
- **Files**: [Key files modified]
- **Context**: [Why this approach was chosen]
```

### 2.2 Architectural Decision Records

#### Decision Documentation Format
```markdown
## ADR-[NUMBER]: [Title]
**Date**: [YYYY-MM-DD]
**Status**: [Accepted/Superseded]
**Context**: [Problem being solved]
**Decision**: [What was decided]
**Consequences**: [Impact and implications]
**Files Affected**: [List of modified files]
```

### 2.3 Project-Specific Rules Registry

#### Coding Standards Registry
```json
{
  "file_naming": {
    "backend": "snake_case.py",
    "frontend": "PascalCase.jsx for components, camelCase.js for utilities",
    "java": "PascalCase.java"
  },
  "status_values": ["DRAFT", "VALID", "PEND", "SCHD", "PROD"],
  "api_patterns": {
    "error_handling": "Consistent JSON error responses",
    "validation": "Multi-layer (client + server + Java)",
    "service_layer": "Separate business logic from API endpoints"
  },
  "ui_patterns": {
    "components": "Functional components with hooks",
    "state": "useState and useEffect pattern",
    "ui_library": "Antd components for consistency"
  }
}
```

---

## 3. INSTRUCTION PRESERVATION

### 3.1 Permanent Project Rules

#### Core Development Principles
1. **Zero Regression Policy**: All changes must preserve existing functionality
2. **Absolute Paths Only**: Never use relative paths in development
3. **Status Field Integrity**: Only uppercase status values allowed
4. **Service Layer Pattern**: API → Service → Model architecture
5. **Comprehensive Testing**: Every change needs regression validation

#### User Preferences
- **UI Preferences**: Clean, minimal interfaces over feature-heavy
- **Code Generation**: Show file locations, not content dumps
- **Validation Messages**: Context-aware, not confusing
- **Development Approach**: Incremental, safety-first changes

### 3.2 Critical Commands and Workflows

#### Development Startup Sequence
```bash
# Backend startup
cd /Users/chandramohn/workspace/rules_engine/ui-prototype/backend
source venv/bin/activate
python app.py

# Frontend startup (new terminal)
cd /Users/chandramohn/workspace/rules_engine/ui-prototype/frontend
npm start

# Java bridge compilation
cd /Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge
mvn clean compile package
```

#### Critical Validation Commands
```bash
# Pre-change validation
python /Users/chandramohn/workspace/rules_engine/ui-prototype/backend/test_regression_suite.py

# Database integrity check
python /Users/chandramohn/workspace/rules_engine/ui-prototype/backend/data_validator.py

# Git status check
git status --porcelain
```

### 3.3 Emergency Recovery Procedures

#### Regression Recovery Steps
1. **Stop all services**: `pkill -f "python app.py" && pkill -f "npm start"`
2. **Database rollback**: `cp database/rules.db.backup database/rules.db`
3. **Git status check**: `git status` to identify problematic changes
4. **Selective revert**: `git checkout -- [problematic_file]`
5. **Service restart**: Start backend and frontend
6. **Validation**: Run regression suite to confirm recovery

---

## 4. STATE DOCUMENTATION

### 4.1 Current Git Status Documentation

#### Modified Files Tracking
```markdown
## Current Modified Files (Pre-Compaction)
- backend/api/rules.py - Enhanced rules API with ActionSet support
- backend/schema/rules_schema.py - Schema updates for unified model
- backend/services/java_bridge.py - Improved validation and compilation
- backend/services/rule_service.py - Service layer enhancements
- frontend/src/components/RuleEditor.jsx - Editor improvements
- frontend/src/components/RulesListEnhanced.jsx - List view enhancements
- frontend/src/components/RulesTreeNavigation.jsx - Navigation improvements
- frontend/src/services/api.js - API client updates
- java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4 - Grammar updates
- java-bridge/src/main/java/com/rules/codegen/DirectJavaCodeGenerator.java - Code generation improvements
```

#### Untracked Files
```markdown
## New Untracked Files
- generated-rules/rule-test_rule/ - Generated rule compilation output
- java-bridge/classpath.txt - Java dependency classpath
- CLAUDE.md - Comprehensive project documentation (should be committed)
```

### 4.2 Work in Progress Documentation

#### Incomplete Tasks
- **Status**: No pending incomplete work identified
- **Next Priorities**: System ready for continued development
- **Blocked Items**: None identified

#### Temporary States
- **Generated Rules**: Clean state, test rule generated successfully
- **Database**: 15 rules total (13 active, 2 deleted)
- **Services**: All running and healthy

### 4.3 Test Results and Validation States

#### Last Regression Test Results
```
==================================================
REGRESSION TEST SUITE - RESULTS
==================================================
✅ ALL TESTS PASSED - No regression issues detected
✅ Data integrity: 13 rules preserved
✅ Status values: All uppercase and valid
✅ UI functionality: All buttons and workflows operational
✅ API endpoints: All responding correctly
✅ Code generation: Working for VALID+ statuses
```

#### Validation Coverage
- **Database Constraints**: Active CHECK constraints for status/schema
- **API Contract Testing**: Response structure validation
- **Frontend Component Testing**: Status dropdown and button state validation
- **Java Bridge Testing**: Grammar parsing and code generation

---

## 5. CONTEXT RESTORATION GUIDE

### 5.1 Post-Compaction Startup Checklist

#### Environment Validation
```bash
# 1. Verify working directory
pwd
# Expected: /Users/chandramohn/workspace/rules_engine/ui-prototype

# 2. Check git status
git status
# Verify: main branch, known modified files

# 3. Verify project structure
ls -la
# Expected: backend/, frontend/, java-bridge/, *.md files

# 4. Check service health
curl http://localhost:5001/api/health || echo "Backend not running"
curl http://localhost:3000 || echo "Frontend not running"
```

#### Critical File Verification
```bash
# Verify critical files exist and have expected content
test -f backend/models.py && echo "✅ Models file exists"
test -f frontend/src/components/RuleEditor.jsx && echo "✅ Rule editor exists"
test -f java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4 && echo "✅ Grammar file exists"
test -f CLAUDE.md && echo "✅ Project documentation exists"
```

### 5.2 Service Restoration Process

#### Backend Service Restoration
```bash
cd /Users/chandramohn/workspace/rules_engine/ui-prototype/backend
source venv/bin/activate || python -m venv venv && source venv/bin/activate
pip install -r requirements.txt
python app.py &
echo "Backend started on port 5001"
```

#### Frontend Service Restoration
```bash
cd /Users/chandramohn/workspace/rules_engine/ui-prototype/frontend
npm install
npm start &
echo "Frontend started on port 3000"
```

#### Java Bridge Verification
```bash
cd /Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge
mvn clean compile package
echo "Java bridge compiled successfully"
```

### 5.3 Context Validation Protocol

#### System Health Verification
```bash
# Run comprehensive system check
python /Users/chandramohn/workspace/rules_engine/ui-prototype/backend/test_regression_suite.py

# Expected output:
# ✅ ALL TESTS PASSED - No regression issues detected
```

#### Data Integrity Verification
```bash
# Check database state
python -c "
import sys
sys.path.append('/Users/chandramohn/workspace/rules_engine/ui-prototype/backend')
from app import create_app
from models import Rule
app = create_app()
with app.app_context():
    rules = Rule.query.filter_by(is_deleted=False).all()
    print(f'Active rules: {len(rules)}')
    statuses = [r.status for r in rules]
    print(f'Status distribution: {dict((s, statuses.count(s)) for s in set(statuses))}')
"
```

#### UI Functionality Verification
```bash
# Frontend accessibility check
curl -s http://localhost:3000 > /dev/null && echo "✅ Frontend accessible"

# API endpoint check
curl -s http://localhost:5001/api/rules | jq length && echo "✅ API responding"
```

---

## 6. AUTOMATED CONTEXT PRESERVATION SCRIPTS

### 6.1 Pre-Compaction Script

#### `save_context.sh`
```bash
#!/bin/bash
echo "=== CONTEXT PRESERVATION STARTED ==="
DATE=$(date +"%Y-%m-%d_%H-%M-%S")

# Create context snapshot
mkdir -p .context_snapshots
SNAPSHOT_DIR=".context_snapshots/pre_compaction_$DATE"
mkdir -p "$SNAPSHOT_DIR"

# Git state
git status --porcelain > "$SNAPSHOT_DIR/git_status.txt"
git log --oneline -10 > "$SNAPSHOT_DIR/recent_commits.txt"
git diff --name-status HEAD~5..HEAD > "$SNAPSHOT_DIR/recent_changes.txt"

# System state
echo "Working Directory: $(pwd)" > "$SNAPSHOT_DIR/system_state.txt"
echo "Date: $(date)" >> "$SNAPSHOT_DIR/system_state.txt"
echo "Branch: $(git branch --show-current)" >> "$SNAPSHOT_DIR/system_state.txt"

# Service status
curl -s http://localhost:5001/api/health && echo "✅ Backend healthy" > "$SNAPSHOT_DIR/service_status.txt" || echo "❌ Backend not responding" > "$SNAPSHOT_DIR/service_status.txt"
curl -s http://localhost:3000 && echo "✅ Frontend healthy" >> "$SNAPSHOT_DIR/service_status.txt" || echo "❌ Frontend not responding" >> "$SNAPSHOT_DIR/service_status.txt"

# Database snapshot
python -c "
import sys, json
sys.path.append('$(pwd)/backend')
from app import create_app
from models import Rule
app = create_app()
with app.app_context():
    rules = Rule.query.all()
    data = [{'id': r.id, 'name': r.name, 'status': r.status, 'item_type': r.item_type} for r in rules]
    with open('$SNAPSHOT_DIR/database_snapshot.json', 'w') as f:
        json.dump(data, f, indent=2)
print('Database snapshot saved')
"

# Update CLAUDE.md with current state
echo "" >> CLAUDE.md
echo "## PRE-COMPACTION CONTEXT - $DATE" >> CLAUDE.md
echo "" >> CLAUDE.md
echo "### Modified Files:" >> CLAUDE.md
git status --porcelain | grep "^ M" >> CLAUDE.md
echo "" >> CLAUDE.md
echo "### Current Progress:" >> CLAUDE.md
echo "- All regression prevention measures active" >> CLAUDE.md
echo "- System health verified before compaction" >> CLAUDE.md

echo "✅ Context preserved in $SNAPSHOT_DIR"
echo "=== CONTEXT PRESERVATION COMPLETED ==="
```

### 6.2 Post-Compaction Restoration Script

#### `restore_context.sh`
```bash
#!/bin/bash
echo "=== CONTEXT RESTORATION STARTED ==="

# Find latest context snapshot
LATEST_SNAPSHOT=$(ls -t .context_snapshots/pre_compaction_* | head -1)
echo "Restoring from: $LATEST_SNAPSHOT"

# Verify working directory
if [[ "$(pwd)" != "/Users/chandramohn/workspace/rules_engine/ui-prototype" ]]; then
    echo "❌ Wrong directory. Expected: /Users/chandramohn/workspace/rules_engine/ui-prototype"
    exit 1
fi

# Verify critical files
echo "Verifying critical files..."
CRITICAL_FILES=(
    "backend/models.py"
    "frontend/src/components/RuleEditor.jsx"
    "java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4"
    "CLAUDE.md"
)

for file in "${CRITICAL_FILES[@]}"; do
    if [[ -f "$file" ]]; then
        echo "✅ $file exists"
    else
        echo "❌ $file missing - CRITICAL ERROR"
        exit 1
    fi
done

# Compare current git status with pre-compaction
echo "Comparing git status..."
git status --porcelain > current_git_status.txt
if cmp -s current_git_status.txt "$LATEST_SNAPSHOT/git_status.txt"; then
    echo "✅ Git status unchanged"
else
    echo "⚠️  Git status changed - review differences"
    echo "Pre-compaction:"
    cat "$LATEST_SNAPSHOT/git_status.txt"
    echo "Current:"
    cat current_git_status.txt
fi

# Verify database integrity
echo "Verifying database integrity..."
python -c "
import sys, json
sys.path.append('$(pwd)/backend')
from app import create_app
from models import Rule
app = create_app()
with app.app_context():
    rules = Rule.query.filter_by(is_deleted=False).all()
    current_count = len(rules)

# Load pre-compaction snapshot
with open('$LATEST_SNAPSHOT/database_snapshot.json', 'r') as f:
    snapshot_data = json.load(f)
    snapshot_count = len([r for r in snapshot_data if r.get('is_deleted') != True])

print(f'Pre-compaction rules: {snapshot_count}')
print(f'Current rules: {current_count}')
if current_count == snapshot_count:
    print('✅ Database integrity maintained')
else:
    print('❌ Database count mismatch - investigate')
"

# Run regression test suite
echo "Running regression tests..."
cd backend
python test_regression_suite.py
cd ..

# Service health check
echo "Checking service health..."
curl -s http://localhost:5001/api/health && echo "✅ Backend healthy" || echo "❌ Backend not responding"
curl -s http://localhost:3000 && echo "✅ Frontend healthy" || echo "❌ Frontend not responding"

rm -f current_git_status.txt
echo "=== CONTEXT RESTORATION COMPLETED ==="
```

---

## 7. CRITICAL SUCCESS FACTORS

### 7.1 Must-Preserve Elements

#### Project Architecture
- **Multi-tier design**: React → Flask → Java engine
- **Database schema**: Single status field, unified Rules/ActionSets table
- **API patterns**: RESTful with service layer separation
- **File organization**: Absolute paths, consistent naming conventions

#### Current Functionality
- **Rules CRUD**: Create, read, update, delete operations
- **Status management**: Auto-promotion and manual override
- **Code generation**: Java bytecode compilation from ANTLR grammar
- **UI components**: Monaco editor, Antd components, clean interfaces

#### Regression Prevention
- **Test suite**: Comprehensive automated validation
- **Database constraints**: CHECK constraints for data integrity
- **Error handling**: Consistent JSON error responses
- **Recovery procedures**: Backup and rollback capabilities

### 7.2 Critical File Dependencies

#### Core Application Files
```
/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/models.py
/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/services/rule_service.py
/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/components/RuleEditor.jsx
/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4
```

#### Documentation Files
```
/Users/chandramohn/workspace/rules_engine/ui-prototype/CLAUDE.md
/Users/chandramohn/workspace/rules_engine/ui-prototype/CLAUDE_SESSION_CHECKPOINT.md
/Users/chandramohn/workspace/rules_engine/ui-prototype/CONTEXT_PRESERVATION_FRAMEWORK.md
```

#### Test and Validation Files
```
/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/test_regression_suite.py
/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/data_validator.py
```

---

## 8. IMPLEMENTATION CHECKLIST

### 8.1 Before Any Compaction Event

- [ ] Run `save_context.sh` to create complete snapshot
- [ ] Verify all services are healthy and responding
- [ ] Run regression test suite to confirm no issues
- [ ] Update CLAUDE.md with latest changes and decisions
- [ ] Commit any important untracked files (especially CLAUDE.md)
- [ ] Create database backup: `cp backend/database/rules.db backend/database/rules.db.backup`

### 8.2 After Compaction/Session Start

- [ ] Run `restore_context.sh` to verify system integrity
- [ ] Check working directory: `/Users/chandramohn/workspace/rules_engine/ui-prototype`
- [ ] Verify git status matches expectations
- [ ] Start services if not running (backend port 5001, frontend port 3000)
- [ ] Run regression test suite to confirm no degradation
- [ ] Review CLAUDE.md for recent changes and context

### 8.3 Emergency Recovery Protocol

If major regression detected:
- [ ] Stop all services immediately
- [ ] Restore database from backup
- [ ] Review git status and revert problematic changes
- [ ] Restart services and validate functionality
- [ ] Re-run regression test suite until all tests pass
- [ ] Document incident and update prevention measures

---

**END OF CONTEXT PRESERVATION FRAMEWORK**

This framework ensures complete continuity across auto-compaction events, preventing the loss of critical development context, architectural decisions, and established patterns.