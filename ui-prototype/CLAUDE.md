# Rules Engine Project - Comprehensive Architecture Guide

**Project**: Credit Card Processing Rules Engine
**Version**: Production-Ready System
**Architecture**: Multi-tier (React + Python Flask + Java Engine)
**Last Updated**: November 8, 2025

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

- **Backend**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/app.py` - Flask application factory
- **Frontend**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/App.jsx` - React main component
- **Java CLI**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/java/com/rules/cli/RulesEngineCLI.java` - Rules engine CLI

### 1.3 Component Relationships

```
React Frontend (3000) ──HTTP/REST──► Flask Backend (5001) ──subprocess──► Java Engine
       │                                    │                                    │
   Antd + Monaco                  JSON File Storage + jsonschema        ANTLR Parser + JVM
```

---

## 2. TECHNOLOGY STACK

### 2.1 Backend Technologies

**Framework**: Flask 2.3.3
**Storage**: JSON file-based storage (SQLite retired as of October 2025)
**Dependencies** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/requirements.txt`):
- Flask-CORS 4.0.0 - Cross-origin resource sharing
- python-dotenv 1.0.0 - Environment configuration
- Requests 2.31.0 - HTTP client for Java bridge
- antlr4-python3-runtime 4.13.2 - ANTLR parser runtime
- jsonschema (implied) - JSON schema validation

**Key Backend Files**:
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/app.py` - Application factory and initialization
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/services/rule_file_service.py` - File-based CRUD service
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/api/rules_file.py` - Rules REST endpoints
- `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/services/context_file_service.py` - Context management

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

**File-Based Storage** (`/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/rules/`):
- **Storage Format**: JSON files organized in hierarchical directory structure
- **Path Pattern**: `{client_code}/{process_group_code}/{process_area_code}/rule-{id}.json`
- **Schema Validation**: `rules/schema.json` validates all rule files via jsonschema
- **Hierarchical Organization**: Client → ProcessGroup → ProcessArea → Rules (arbitrary depth supported)
- **Item Types**: Both rules and ActionSets stored with `item_type` field discrimination
- **Git-Native**: Human-readable JSON enables version control, diffs, and collaboration

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
- Credit card domain-focused sample rules
- Validation regression testing
- File-based storage integration tests

---

## 5. CURRENT DEVELOPMENT FOCUS

### 5.1 ActionSet Implementation

**Status**: Successfully implemented
**Key Achievement**: Unified Rules and ActionSets in single storage with `item_type` field
**Implementation**:
- File-based storage with `item_type` discrimination
- API support in rules endpoints with `item_type` parameter
- Zero regression approach maintained throughout implementation

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
- **File I/O**: < 10ms read, < 100ms write with validation
- **Storage**: Direct filesystem access (no database overhead)

### 7.2 Scalability Considerations
- Hot class loading for dynamic rule updates
- Compilation caching to avoid redundant builds
- Pagination for large rule sets (Page size: 50, Max: 100)
- File-based storage scales horizontally with filesystem performance
- Optional DuckDB integration available for complex analytics queries

---

## 8. PATTERNS TO PRESERVE

### 8.1 Development Session Continuity
- **Configuration**: All paths are absolute (avoid relative path issues)
- **Error Handling**: Consistent JSON error responses across all APIs
- **Validation**: Multi-layer validation (client + server + Java engine)
- **State Management**: React hooks pattern for component state

### 8.2 Code Organization
- **Separation of Concerns**: Clear API → Service → Storage layers
- **Single Responsibility**: Each component has focused purpose
- **Additive Changes**: New features added without modifying existing code
- **Comprehensive Testing**: Every change tested against regression suite
- **File-Based Architecture**: JSON storage with schema validation and hierarchical organization

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
- **Rule File Service**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/services/rule_file_service.py`
- **Context Service**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/services/context_file_service.py`
- **UI Components**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/src/components/`
- **Grammar Definition**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4`

### 9.3 Storage Locations
- **Rules Storage**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/rules/`
- **Schemas Storage**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/schemas/`
- **Lists Storage**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/lists/`
- **Contexts Storage**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/contexts/`

### 9.4 Generated Assets
- **Compiled Rules**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/generated-rules/`
- **ANTLR Generated**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/java-bridge/target/generated-sources/`
- **Frontend Build**: `/Users/chandramohn/workspace/rules_engine/ui-prototype/frontend/build/`

---

## CLEANUP - 2025-11-02 - ANTLR Python File Deduplication

### Actions Taken:
**Removed duplicate Python ANTLR generated files from `java-bridge/` directory:**
- ❌ Deleted: `java-bridge/src/main/antlr4/com/rules/grammar/RulesLexer.py`
- ❌ Deleted: `java-bridge/src/main/antlr4/com/rules/grammar/RulesListener.py`
- ❌ Deleted: `java-bridge/src/main/antlr4/com/rules/grammar/RulesParser.py`

**Retained active Python files in `backend/java-bridge/` directory:**
- ✅ Active: `backend/java-bridge/src/main/antlr4/com/rules/grammar/RulesLexer.py`
- ✅ Active: `backend/java-bridge/src/main/antlr4/com/rules/grammar/RulesListener.py`
- ✅ Active: `backend/java-bridge/src/main/antlr4/com/rules/grammar/RulesParser.py`
- ✅ Active: `backend/java-bridge/src/main/antlr4/com/rules/grammar/RulesVisitor.py`

### Rationale:
- Files were byte-for-byte identical (verified via MD5 checksums)
- Python backend imports exclusively from `backend/java-bridge/` path
- No Python code references the `java-bridge/` location
- Cleanup eliminates confusion and reduces codebase by ~83KB
- All functionality preserved and validated post-cleanup

### File Location Architecture:

**Rules.g4 Grammar Source** (maintained in both locations):
1. **Java Build Source**: `java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4`
   - Used by Maven ANTLR plugin for Java code generation
   - Source of truth for grammar definition

2. **Python Backend Copy**: `backend/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4`
   - Used for Python ANTLR parser generation
   - Should be synced from Java source when grammar changes

**Generated Python Files** (single location only):
- **Location**: `backend/java-bridge/src/main/antlr4/com/rules/grammar/`
- **Files**: RulesLexer.py, RulesParser.py, RulesListener.py, RulesVisitor.py
- **Used By**: Backend validation and parsing (`backend/grammar_parser/`)

**Import Pattern**:
```python
# In backend/grammar_parser/rules_parser.py and rule_validator.py
antlr_path = Path(__file__).parent.parent / "java-bridge" / "src" / "main" / "antlr4"
sys.path.insert(0, str(antlr_path))
from com.rules.grammar.RulesParser import RulesParser
```

---

## STORAGE ARCHITECTURE - CURRENT STATE (November 2025)

### ✅ Confirmed: SQLite Fully Retired
- **Status**: 100% complete as of October 2025
- **No SQLite Dependencies**: Zero SQLAlchemy/Flask-SQLAlchemy in `requirements.txt`
- **No Database Code**: No `models.py`, no `db.init_app()`, no ORM code
- **Migration Success**: 33/33 rules migrated to JSON files with zero data loss

### ✅ Active: JSON File-Based Storage
- **Primary Storage**: Hierarchical JSON files in `backend/rules/`, `backend/schemas/`, `backend/lists/`, `backend/contexts/`
- **CRUD Service**: `rule_file_service.py` handles all file I/O operations
- **Validation**: `jsonschema` library validates against `rules/schema.json`
- **Performance**: < 10ms reads, < 100ms writes with validation
- **Version Control**: Git-native with human-readable diffs

### ⚠️ DuckDB Status: NOT Currently Implemented
- **Mentioned In**: Project memories reference DuckDB for SQL analytics
- **Reality**: No active DuckDB code in current implementation
- **Codebase Search**: Zero DuckDB imports or usage found
- **Current Queries**: Direct file I/O via `rule_file_service.py` only
- **Future Option**: DuckDB integration available if complex analytics needed

### Storage Service Layer
```python
# Current Active Services (File-Based)
rule_file_service.py       # CRUD operations on rule JSON files
context_file_service.py    # Context management via JSON files
schema_file_service.py     # Schema storage and validation
list_cache.py              # Named value lists from JSON files

# Future Optional (If Analytics Needed)
rule_query_service.py      # DuckDB SQL analytics over JSON files (not currently implemented)
```

---

**Historical Context**: Pre-compaction contexts archived in `/docs/HISTORY.md`

**End of Document**
This guide serves as permanent project memory for maintaining consistency across development sessions and preserving the established architectural patterns.

**Last Architecture Verification**: November 8, 2025
