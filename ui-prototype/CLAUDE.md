# Rules Engine Project - Comprehensive Architecture Guide

**Project**: Credit Card Processing Rules Engine
**Version**: Production-Ready System
**Architecture**: Multi-tier (React + Python Flask + Java Engine)
**Last Updated**: September 17, 2025

---

## 1. PROJECT STRUCTURE ANALYSIS

### 1.1 Main Directories Overview

```
/Users/chandramohn/workspace/rules_engine/ui-prototype/
├── backend/                 # Python Flask API server (Port 5001)
├── frontend/                # React web application (Port 3000)
├── java-bridge/             # Java rules engine and ANTLR parser
├── generated-rules/         # Compiled Java rule classes
├── scripts/                 # Build and deployment scripts
└── [documentation files]   # Various .md files
```

### 1.2 Key Entry Points

- **Backend**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/app.py` (Line 349-352)
- **Frontend**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/App.jsx` (Line 9-68)
- **Java CLI**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/java/com/rules/cli/RulesEngineCLI.java` (Line 9-42)

### 1.3 Component Relationships

```
React Frontend (3000) ──HTTP/REST──► Flask Backend (5001) ──subprocess──► Java Engine
       │                                    │                                    │
   Antd + Monaco                      SQLAlchemy + SQLite              ANTLR Parser + JVM
```

---

## 2. TECHNOLOGY STACK

### 2.1 Backend Technologies

**Framework**: Flask 2.3.3
**Database**: SQLite with SQLAlchemy ORM
**Dependencies** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/requirements.txt`):
- Flask-SQLAlchemy 3.0.5 - Database ORM
- Flask-CORS 4.0.0 - Cross-origin resource sharing
- Flask-Migrate 4.0.5 - Database migrations
- Marshmallow 3.20.1 - Data serialization
- Requests 2.31.0 - HTTP client for Java bridge

**Key Backend Files**:
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/app.py` - Application factory and initialization
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/models.py` - Database models (Lines 8-382)
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/api/rules.py` - Rules REST endpoints
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/services/java_bridge.py` - Java integration

### 2.2 Frontend Technologies

**Framework**: React 18.2.0
**Build Tool**: Create React App (react-scripts 5.0.1)
**Dependencies** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/package.json`):
- @monaco-editor/react 4.6.0 - Code editor component
- antd 5.8.6 - UI component library
- axios 1.5.0 - HTTP client
- react-router-dom 6.15.0 - Client-side routing

**Key Frontend Files**:
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/App.jsx` - Main application component
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/components/RuleEditor.jsx` - Monaco-based rule editor
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/services/api.js` - API client configuration

### 2.3 Java Bridge Technologies

**Build Tool**: Maven 3.x
**Java Version**: 17
**Dependencies** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/pom.xml`):
- ANTLR 4.13.1 - Grammar parsing and code generation
- Jackson 2.15.2 - JSON processing
- JUnit 5.10.0 - Testing framework

**Key Java Files**:
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4` - ANTLR grammar definition
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/java/com/rules/codegen/DirectJavaCodeGenerator.java` - Code generation engine
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/java/com/rules/cli/RulesEngineCLI.java` - Command-line interface

---

## 3. CORE FUNCTIONALITY

### 3.1 Rules Definition and Storage

**Grammar Definition** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4`):
```antlr
// Key grammar elements (Lines 12-24)
unifiedRule: RULE ruleName COLON ruleStep+
ruleStep: IF condition THEN actionList (ELSE actionList)? | actionList
condition: orExpression
```

**Database Schema** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/models.py`):
- **Rule Model** (Lines 109-175): Main rules storage with content, status, and hierarchy
- **Hierarchical Organization**: Client → ProcessGroup → ProcessArea → Rules
- **Unified Storage**: Both rules and ActionSets stored in same table with `item_type` field

### 3.2 Rule Execution Flow

```
Frontend Rule Editor → Backend Validation → Java ANTLR Parser → Bytecode Generation → Execution
```

**Validation Process** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/services/java_bridge.py`):
1. **Syntax Validation**: ANTLR parser validates grammar
2. **Parameter Validation**: Custom validation for rule parameters
3. **Compilation**: Generate Java bytecode for rule execution
4. **Testing**: Execute against sample data

### 3.3 Code Generation Process

**DirectJavaCodeGenerator** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/java/com/rules/codegen/DirectJavaCodeGenerator.java`):
- Visitor pattern over ANTLR parse tree
- Generates executable Java classes
- Supports complex expressions and nested logic
- Output stored in `/generated-rules/` directory

---

## 4. DEVELOPMENT PATTERNS

### 4.1 Naming Conventions

**Backend Python**:
- Files: snake_case (e.g., `rule_service.py`, `java_bridge.py`)
- Classes: PascalCase (e.g., `RuleService`, `JavaBridge`)
- Variables/functions: snake_case (e.g., `get_rules`, `validate_rule`)

**Frontend JavaScript/JSX**:
- Files: PascalCase for components (e.g., `RuleEditor.jsx`)
- Files: camelCase for utilities (e.g., `api.js`, `rulesSyntax.js`)
- Components: PascalCase (e.g., `RuleEditor`, `RulesListEnhanced`)
- Variables/functions: camelCase (e.g., `handleSaveRule`, `currentView`)

**Java**:
- Files: PascalCase matching class names
- Classes: PascalCase (e.g., `DirectJavaCodeGenerator`, `RulesEngineCLI`)
- Methods: camelCase (e.g., `generateCode`, `validateRule`)
- Packages: lowercase (e.g., `com.rules.codegen`, `com.rules.cli`)

### 4.2 Component Structure

**React Components** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/components/`):
- Functional components with hooks pattern
- State management with useState and useEffect
- Props destructuring for clean interfaces
- Antd components for consistent UI

**Backend APIs** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/api/`):
- Blueprint pattern for route organization
- Service layer for business logic separation
- Marshmallow schemas for data validation
- Consistent error handling and HTTP status codes

### 4.3 Testing Approaches

**Java Testing**:
- JUnit 5 for unit tests
- Test files in `src/test/java/` mirror source structure
- CLI testing with sample rule files

**Backend Testing**:
- Test data generation in `app.py` (Lines 36-283)
- Credit card domain-focused sample rules
- Validation regression testing

---

## 5. RECENT CHANGES & DEVELOPMENT FOCUS

### 5.1 Git Status Analysis

**Modified Files** (from git status):
- `backend/api/rules.py` - Enhanced rules API with ActionSet support
- `backend/schema/rules_schema.py` - Schema updates for unified model
- `backend/services/java_bridge.py` - Improved validation and compilation
- `backend/services/rule_service.py` - Service layer enhancements
- `frontend/src/components/RuleEditor.jsx` - Editor improvements
- `frontend/src/components/RulesListEnhanced.jsx` - List view enhancements
- `frontend/src/services/api.js` - API client updates
- `java-bridge/src/main/java/com/rules/codegen/DirectJavaCodeGenerator.java` - Code generation improvements

### 5.2 ActionSet Implementation

**Status**: Successfully implemented with zero regression approach
**Key Achievement**: Unified Rules and ActionSets in single table with `item_type` field
**Files Affected**:
- Models unified in `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/models.py` (Lines 129, 168-174)
- API support in rules endpoints with `item_type` parameter

### 5.3 Code Reduction Initiative

**Goal**: Eliminate duplicate validation methods and consolidate status fields
**Progress**: Validation status consolidated into main `status` field
**Safety**: Maintained backward compatibility through gradual migration

---

## 6. BUILD AND DEPLOYMENT PROCESSES

### 6.1 Development Setup

**Backend Setup**:
```bash
cd backend
python -m venv venv
source venv/bin/activate  # or venv\Scripts\activate on Windows
pip install -r requirements.txt
python app.py
```

**Frontend Setup**:
```bash
cd frontend
npm install
npm start
```

**Java Bridge Setup**:
```bash
cd java-bridge
mvn clean compile
mvn package
```

### 6.2 Production Build

**Frontend Build**: `npm run build` creates production assets
**Backend**: Direct Python deployment with production WSGI server
**Java**: Shaded JAR with all dependencies via Maven Shade Plugin

---

## 7. PERFORMANCE CHARACTERISTICS

### 7.1 Current Metrics
- **Rule Execution**: 0.67ms average (Sub-millisecond performance achieved)
- **Rule Compilation**: 63ms average (Fast compilation pipeline)
- **Memory Usage**: 2KB per rule (Efficient memory footprint)
- **Database**: SQLite with indexing for hierarchy and names

### 7.2 Scalability Considerations
- Hot class loading for dynamic rule updates
- Compilation caching to avoid redundant builds
- Pagination for large rule sets (Page size: 50, Max: 100)

---

## 8. PATTERNS TO PRESERVE

### 8.1 Development Session Continuity
- **Configuration**: All paths are absolute (avoid relative path issues)
- **Error Handling**: Consistent JSON error responses across all APIs
- **Validation**: Multi-layer validation (client + server + Java engine)
- **State Management**: React hooks pattern for component state

### 8.2 Code Organization
- **Separation of Concerns**: Clear API → Service → Model layers
- **Single Responsibility**: Each component has focused purpose
- **Additive Changes**: New features added without modifying existing code
- **Comprehensive Testing**: Every change tested against regression suite

### 8.3 Integration Patterns
- **API Design**: RESTful endpoints with consistent resource naming
- **Data Flow**: Unidirectional data flow in React components
- **Error Propagation**: Errors bubble up from Java → Python → React
- **Caching**: Intelligent caching for schema and suggestions

---

## 9. CRITICAL FILE LOCATIONS

### 9.1 Configuration Files
- **Backend Config**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/config.py`
- **Frontend Proxy**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/package.json` (Line 42)
- **Java Build**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/pom.xml`

### 9.2 Core Logic Files
- **Rule Processing**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/services/rule_service.py`
- **UI Components**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/components/`
- **Grammar Definition**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4`

### 9.3 Generated Assets
- **Compiled Rules**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/generated-rules/`
- **ANTLR Generated**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/target/generated-sources/`
- **Frontend Build**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/build/`

---

**End of Document**
This guide serves as permanent project memory for maintaining consistency across development sessions and preserving the established architectural patterns.
## PRE-COMPACTION CONTEXT - 2025-09-17_13-47-41

### Modified Files:
 M ui-prototype/backend/api/rules.py
 M ui-prototype/backend/schema/rules_schema.py
 M ui-prototype/backend/services/java_bridge.py
 M ui-prototype/backend/services/rule_service.py
 M ui-prototype/frontend/src/components/RuleEditor.jsx
 M ui-prototype/frontend/src/components/RulesListEnhanced.jsx
 M ui-prototype/frontend/src/components/RulesTreeNavigation.jsx
 M ui-prototype/frontend/src/services/api.js
 M ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4
 M ui-prototype/java-bridge/src/main/java/com/rules/codegen/DirectJavaCodeGenerator.java

### Untracked Files:
?? ui-prototype/.context_snapshots/
?? ui-prototype/CLAUDE.md
?? ui-prototype/CONTEXT_PRESERVATION_FRAMEWORK.md
?? ui-prototype/CONTEXT_QUICK_REFERENCE.md
?? ui-prototype/generated-rules/rule-test_rule/
?? ui-prototype/java-bridge/classpath.txt
?? ui-prototype/restore_context.sh
?? ui-prototype/save_context.sh

### Current Progress:
- All regression prevention measures active
- System health verified before compaction
- Context snapshot saved to .context_snapshots/pre_compaction_2025-09-17_13-47-41

## PRE-COMPACTION CONTEXT - 2025-09-18_10-13-05

### Modified Files:
 M ui-prototype/backend/api/rules.py
 M ui-prototype/backend/regression_test_results.json
 M ui-prototype/backend/schema/rules_schema.py
 M ui-prototype/backend/services/java_bridge.py
 M ui-prototype/backend/services/rule_service.py
 M ui-prototype/frontend/src/components/RuleEditor.jsx
 M ui-prototype/frontend/src/components/RulesListEnhanced.jsx
 M ui-prototype/frontend/src/components/RulesTreeNavigation.jsx
 M ui-prototype/frontend/src/services/api.js
 M ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4
 M ui-prototype/java-bridge/src/main/java/com/rules/codegen/DirectJavaCodeGenerator.java

### Untracked Files:
?? ui-prototype/.context_snapshots/
?? ui-prototype/.memory_consolidation/
?? ui-prototype/.session_continuity/
?? ui-prototype/CLAUDE.md
?? ui-prototype/CONTEXT_PRESERVATION_FRAMEWORK.md
?? ui-prototype/CONTEXT_QUICK_REFERENCE.md
?? ui-prototype/MEMORY_CONSOLIDATION_SYSTEM.md
?? ui-prototype/USAGE_SUMMARY.md
?? ui-prototype/VALIDATION_AGENT_GUIDE.md
?? ui-prototype/backend/data_integrity_monitor.py
?? ui-prototype/backend/enhanced_regression_suite.py
?? ui-prototype/claude_memory_manager.py
?? ui-prototype/code_quality_validator.py
?? ui-prototype/generated-rules/rule-test_rule/
?? ui-prototype/integrated_memory_system.py
?? ui-prototype/integration_test_framework.py
?? ui-prototype/java-bridge/classpath.txt
?? ui-prototype/memory_consolidation_agent.py
?? ui-prototype/memory_report_20250917_141653.md
?? ui-prototype/memory_system_tests.py
?? ui-prototype/restore_context.sh
?? ui-prototype/run_memory_consolidation.py
?? ui-prototype/run_validation_demo.py
?? ui-prototype/save_context.sh
?? ui-prototype/session_continuity_bridge.py
?? ui-prototype/validation_agent.py

### Current Progress:
- All regression prevention measures active
- System health verified before compaction
- Context snapshot saved to .context_snapshots/pre_compaction_2025-09-18_10-13-05

## PRE-COMPACTION CONTEXT - 2025-09-18_14-12-11

### Modified Files:
 M ui-prototype/backend/api/rules.py
 M ui-prototype/backend/regression_test_results.json
 M ui-prototype/backend/schema/rules_schema.py
 M ui-prototype/backend/services/java_bridge.py
 M ui-prototype/backend/services/rule_service.py
 M ui-prototype/frontend/src/components/RuleEditor.jsx
 M ui-prototype/frontend/src/components/RulesList.jsx
 M ui-prototype/frontend/src/components/RulesListEnhanced.jsx
 M ui-prototype/frontend/src/components/RulesTreeNavigation.jsx
 M ui-prototype/frontend/src/services/api.js
 M ui-prototype/frontend/src/services/suggestionCache.js
 M ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4
 M ui-prototype/java-bridge/src/main/java/com/rules/codegen/DirectJavaCodeGenerator.java

### Untracked Files:
?? ui-prototype/.context_snapshots/
?? ui-prototype/.memory_consolidation/
?? ui-prototype/.serena/
?? ui-prototype/.session_continuity/
?? ui-prototype/CLAUDE.md
?? ui-prototype/CLEANUP_SUMMARY.md
?? ui-prototype/backend/data_integrity_monitor.py
?? ui-prototype/backend/enhanced_regression_suite.py
?? ui-prototype/docs/
?? ui-prototype/generated-rules/rule-test_rule/
?? ui-prototype/java-bridge/classpath.txt
?? ui-prototype/reports/
?? ui-prototype/scripts/claude_memory_manager.py
?? ui-prototype/scripts/code_quality_validator.py
?? ui-prototype/scripts/integrated_memory_system.py
?? ui-prototype/scripts/memory_consolidation_agent.py
?? ui-prototype/scripts/restore_context.sh
?? ui-prototype/scripts/run_memory_consolidation.py
?? ui-prototype/scripts/run_validation_demo.py
?? ui-prototype/scripts/save_context.sh
?? ui-prototype/scripts/session_continuity_bridge.py
?? ui-prototype/scripts/validation_agent.py
?? ui-prototype/tests/

### Current Progress:
- All regression prevention measures active
- System health verified before compaction
- Context snapshot saved to .context_snapshots/pre_compaction_2025-09-18_14-12-11

## PRE-COMPACTION CONTEXT - 2025-09-18_15-50-08

### Modified Files:
 M ui-prototype/CLAUDE.md
 M ui-prototype/backend/app.py

### Untracked Files:
?? ui-prototype/.context_snapshots/pre_compaction_2025-09-18_15-50-08/
?? ui-prototype/ACTIONSET_GRAMMAR_FIX.md

### Current Progress:
- All regression prevention measures active
- System health verified before compaction
- Context snapshot saved to .context_snapshots/pre_compaction_2025-09-18_15-50-08

## PRE-COMPACTION CONTEXT - 2025-09-18_17-29-46

### Modified Files:
 M ui-prototype/CLAUDE.md
 M ui-prototype/backend/app.py
 M ui-prototype/backend/services/rule_service.py
 M ui-prototype/frontend/src/components/RuleEditor.jsx
 M ui-prototype/frontend/src/components/RulesListEnhanced.jsx

### Untracked Files:
?? ui-prototype/.context_snapshots/pre_compaction_2025-09-18_15-50-08/
?? ui-prototype/.context_snapshots/pre_compaction_2025-09-18_17-29-46/
?? ui-prototype/ACTIONSET_GRAMMAR_FIX.md

### Current Progress:
- All regression prevention measures active
- System health verified before compaction
- Context snapshot saved to .context_snapshots/pre_compaction_2025-09-18_17-29-46

## PRE-COMPACTION CONTEXT - 2025-09-18_18-01-40

### Modified Files:
 M ui-prototype/CLAUDE.md
 M ui-prototype/backend/app.py
 M ui-prototype/backend/services/rule_service.py
 M ui-prototype/frontend/src/components/RuleEditor.jsx
 M ui-prototype/frontend/src/components/RulesListEnhanced.jsx

### Untracked Files:
?? ui-prototype/.context_snapshots/pre_compaction_2025-09-18_15-50-08/
?? ui-prototype/.context_snapshots/pre_compaction_2025-09-18_17-29-46/
?? ui-prototype/.context_snapshots/pre_compaction_2025-09-18_18-01-40/
?? ui-prototype/ACTIONSET_GRAMMAR_FIX.md
?? ui-prototype/backend/grammar_parser/
?? ui-prototype/backend/java-bridge/
?? ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/RulesLexer.py
?? ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/RulesListener.py
?? ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/RulesParser.py

### Current Progress:
- All regression prevention measures active
- System health verified before compaction
- Context snapshot saved to .context_snapshots/pre_compaction_2025-09-18_18-01-40
