# Storage Architecture - Current State (November 8, 2025)

## ✅ SQLite Retirement: COMPLETE

**Status**: 100% migrated to file-based storage
**Completion Date**: October 2025
**Migration Success**: 33/33 rules (100% success rate)

### What Was Removed
- ❌ SQLite database (`backend/instance/rules.db`)
- ❌ SQLAlchemy ORM (`Flask-SQLAlchemy`, `Flask-Migrate`)
- ❌ Database models (`backend/models.py` removed)
- ❌ Database initialization (`db.init_app()`, `db.create_all()`)
- ❌ Marshmallow-SQLAlchemy serialization

### Current Dependencies (requirements.txt)
```
Flask==2.3.3
Flask-CORS==4.0.0
python-dotenv==1.0.0
requests==2.31.0
antlr4-python3-runtime==4.13.2
```

**Note**: No SQLAlchemy, no database libraries

## ✅ File-Based Storage: ACTIVE

### Architecture
**Storage Method**: JSON files in hierarchical directory structure
**Validation**: jsonschema library (implied dependency)
**Performance**: < 10ms reads, < 100ms writes

### Directory Structure
```
backend/
├── rules/                    # Rule definitions
│   ├── {client_code}/
│   │   └── {process_group_code}/
│   │       └── {process_area_code}/
│   │           └── rule-{id}.json
│   └── schema.json          # Validation schema
├── schemas/                  # Entity schemas
│   ├── entities/
│   │   ├── applicant.json
│   │   └── transaction.json
│   └── versions/
├── lists/                    # Named value lists
│   └── schema.json
└── contexts/                 # Execution contexts
```

### Active Services
1. **rule_file_service.py** - CRUD operations on rule JSON files
   - `get_rule()` - Read single rule
   - `save_rule()` - Create/update with validation
   - `delete_rule()` - Remove rule file
   - `list_rules()` - List with filtering

2. **context_file_service.py** - Context management
3. **schema_file_service.py** - Schema storage
4. **list_cache.py** - Named value lists

### API Endpoints
- `api/rules_file.py` - Rules CRUD endpoints (file-based)
- `api/contexts_file.py` - Contexts endpoints (file-based)
- `api/schema.py` - Schema endpoints
- `api/lists.py` - Lists endpoints

## ⚠️ DuckDB: NOT IMPLEMENTED

**Status**: Mentioned in memories but NOT active in codebase

### Evidence
- ❌ No DuckDB imports in any Python files
- ❌ No `rule_query_service.py` with DuckDB implementation
- ❌ No DuckDB in requirements.txt
- ❌ Grep search for "duckdb" returns zero active code results

### What This Means
- **Current Queries**: Direct file I/O only via `rule_file_service.py`
- **No SQL Analytics**: No DuckDB queries over JSON files
- **Future Option**: DuckDB can be added if complex analytics needed

### Memory Discrepancy
The `file_storage_implementation` memory mentions:
- "Query Service: services/rule_query_service.py - DuckDB SQL analytics"
- "Dependencies: duckdb>=0.9.2"

**Reality**: This was planned but never implemented in active codebase.

## Application Entry Point

**File**: `backend/app.py`
```python
from flask import Flask
from flask_cors import CORS
from api.rules_file import rules_file_bp as rules_bp
from api.schema import schema_bp
from api.lists import lists_bp
from api.hierarchy import hierarchy_bp
from api.contexts_file import contexts_file_bp as contexts_bp

# No database imports
# No models.py
# No db.init_app()

def create_app():
    app = Flask(__name__)
    CORS(app, origins=Config.CORS_ORIGINS)
    
    # Register file-based blueprints
    app.register_blueprint(rules_bp, url_prefix='/api')
    app.register_blueprint(schema_bp, url_prefix='/api')
    app.register_blueprint(lists_bp, url_prefix='/api')
    
    # Initialize storage directories
    with app.app_context():
        for dir_path in [Config.RULES_DIR, Config.LISTS_DIR, 
                        Config.SCHEMAS_DIR, Config.CONTEXTS_DIR]:
            os.makedirs(dir_path, exist_ok=True)
```

## Benefits Achieved

1. **✅ Git-Native Version Control**
   - Human-readable JSON diffs
   - Full commit history per rule
   - Easy rollback and collaboration

2. **✅ Simplified Deployment**
   - No database initialization
   - No migrations to manage
   - No connection pooling

3. **✅ Performance**
   - Direct file access: O(1) by path
   - No ORM overhead
   - < 10ms reads, < 100ms writes

4. **✅ Arbitrary Hierarchy Depth**
   - No longer limited to 3 levels (Client → ProcessGroup → ProcessArea)
   - Recursive directory scanning supports unlimited depth

5. **✅ Schema Validation**
   - jsonschema validates all rule files
   - Ensures data integrity without database constraints

## Development Commands

**Start Backend**:
```bash
cd backend
python app.py
# Server runs on http://localhost:5001
```

**Test Storage**:
```bash
curl http://localhost:5001/api/health
curl http://localhost:5001/api/rules
curl http://localhost:5001/api/hierarchy/tree
```

## Future Considerations

**If Analytics Needed**:
- Can add DuckDB for SQL queries over JSON files
- Would create `rule_query_service.py` with DuckDB integration
- Would add `duckdb>=0.9.2` to requirements.txt
- Current file-based storage compatible with DuckDB

**Current Approach**: Direct file I/O sufficient for CRUD operations
