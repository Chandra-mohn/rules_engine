# SQLite Retirement - Final Execution Plan
## Parallel Migration with Minimal Code Changes

**Status**: 📋 Ready for Execution
**Strategy**: Parallel migration, surgical code changes, preserve all interfaces
**Timeline**: 1-2 days (all phases in parallel)
**Risk Level**: 🟢 Low (minimal changes, preserved interfaces)

---

## 🎯 **Final Architecture**

### **Directory Structure**
```
backend/
├── rules/                    # ✅ Renamed from rules_data
│   └── {client}/{process_group}/{process_area}/rule-{id}.json
├── lists/                    # ✅ New - named value lists
│   ├── countries.json
│   ├── card_types.json
│   └── schema.json
├── schemas/                  # ✅ New - entity/attribute definitions
│   ├── entities/
│   │   ├── applicant.json
│   │   └── transaction.json
│   ├── versions/
│   │   └── v2.json
│   └── schema.json
├── services/
│   ├── rule_file_service.py       # ✅ Existing - update paths
│   ├── rule_query_service.py      # ✅ Existing - DuckDB analytics
│   ├── list_service.py            # 🔄 Modify in-place (remove db.session)
│   ├── context_file_service.py    # ✅ Existing
│   └── schema_service.py          # 🔄 Modify schema/rules_schema.py
└── api/
    ├── rules_file.py              # 🔄 Update path references
    ├── lists.py                   # ✅ No changes needed
    ├── schema.py                  # ✅ No changes needed
    ├── hierarchy.py               # 🔄 Enhance for arbitrary depth
    └── contexts_file.py           # 🔄 Update path references
```

---

## 🔧 **Technology Decisions**

| Component | Technology | Usage |
|-----------|-----------|-------|
| **Rules CRUD** | File I/O (json module) | Read/write individual rules |
| **Rules Analytics** | DuckDB | Dashboard stats, complex queries, aggregations |
| **Lists CRUD** | File I/O (json module) | Read/write named lists |
| **Schema CRUD** | File I/O (json module) | Read entity/attribute definitions |
| **Hierarchy** | Folder scanning (pathlib) | Arbitrary depth, no hardcoded levels |
| **Validation** | jsonschema | Schema validation for all JSON files |

**DuckDB Scope** (Confirmed):
- ✅ Rules analytics (aggregations, filtering, sorting)
- ✅ Dashboard statistics (counts, trends, performance)
- ❌ Lists (simple CRUD, no analytics needed)
- ❌ Schemas (simple reads, no complex queries)

---

## 📋 **Parallel Migration Tasks**

All tasks can be executed in parallel since they modify different files and have minimal dependencies.

---

## **TASK 1: Directory Rename** 🔄
**Files**: All references to `rules_data`
**Effort**: 30 minutes
**Risk**: Low (find/replace operation)

### 1.1 Rename Directory
```bash
cd backend
mv rules_data rules

# Update .gitignore if needed
sed -i '' 's/rules_data/rules/g' .gitignore
```

### 1.2 Update All Path References
```bash
# Find all references (for review)
grep -r "rules_data" backend/ --exclude-dir=venv

# Files to update:
# - services/rule_file_service.py
# - services/rule_query_service.py
# - api/rules_file.py
# - api/hierarchy.py
# - config.py (if RULES_DATA_PATH exists)
```

### 1.3 Surgical Changes
```python
# Example: services/rule_file_service.py
# Find: RULES_DATA_PATH = Path(__file__).parent.parent / 'rules_data'
# Replace: RULES_DATA_PATH = Path(__file__).parent.parent / 'rules'

# Use find/replace in each file - exact line matches only
```

**Validation**:
```bash
# Verify all references updated
grep -r "rules_data" backend/ --exclude-dir=venv
# Should return: 0 matches

# Test path exists
python3 -c "from pathlib import Path; print(Path('backend/rules').exists())"
# Should return: True
```

---

## **TASK 2: Lists Module Migration** 🟡
**Files**: `services/list_cache.py`, minimal `api/lists.py` updates
**Effort**: 2 hours
**Risk**: Low (surgical changes, preserved interface)

### 2.1 Create Directory Structure
```bash
mkdir -p backend/lists
touch backend/lists/schema.json
```

### 2.2 Define JSON Schema
```json
# File: backend/lists/schema.json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["list_name", "display_name", "values"],
  "properties": {
    "list_name": {"type": "string"},
    "display_name": {"type": "string"},
    "description": {"type": "string"},
    "schema_version": {"type": "string", "enum": ["v1", "v2", "both"]},
    "values": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["code", "name"],
        "properties": {
          "code": {"type": "string"},
          "name": {"type": "string"},
          "active": {"type": "boolean", "default": true}
        }
      }
    },
    "metadata": {
      "type": "object",
      "properties": {
        "created_at": {"type": "string", "format": "date-time"},
        "updated_at": {"type": "string", "format": "date-time"},
        "version": {"type": "integer"}
      }
    }
  }
}
```

### 2.3 Modify list_cache.py (Surgical Changes)
```python
# File: services/list_cache.py

# ADD at top (after existing imports):
from pathlib import Path
import json

# REMOVE these lines:
# from models import db, RuleList

# ADD constant:
LISTS_PATH = Path(__file__).parent.parent / 'lists'

# MODIFY __init__ method:
class ListService:
    def __init__(self):
        # REMOVE: self.cache = {} or any db references
        # ADD:
        self.base_path = LISTS_PATH
        self.base_path.mkdir(exist_ok=True)
        self._cache = {}  # Optional: in-memory cache

    # MODIFY each method - keep exact signatures, change implementation:

    def get_all_lists(self, schema_version='both'):
        """Get all lists - MODIFIED to use files"""
        # REMOVE: db.session.query(RuleList).all()
        # ADD:
        lists = []
        for file_path in self.base_path.glob('*.json'):
            if file_path.name == 'schema.json':
                continue
            with open(file_path, 'r') as f:
                list_data = json.load(f)
                if schema_version == 'both' or list_data.get('schema_version') == schema_version:
                    lists.append(list_data)
        return lists

    def get_list(self, list_name):
        """Get specific list - MODIFIED to use files"""
        # REMOVE: db.session.query(RuleList).filter_by(name=list_name).first()
        # ADD:
        file_path = self.base_path / f'{list_name}.json'
        if not file_path.exists():
            return None
        with open(file_path, 'r') as f:
            return json.load(f)

    def save_list(self, list_name, list_data):
        """Save list - MODIFIED to write file"""
        # REMOVE: db.session.add(rule_list); db.session.commit()
        # ADD:
        from datetime import datetime
        list_data['metadata'] = list_data.get('metadata', {})
        list_data['metadata']['updated_at'] = datetime.utcnow().isoformat()

        file_path = self.base_path / f'{list_name}.json'
        with open(file_path, 'w') as f:
            json.dump(list_data, f, indent=2)

        self._cache.pop(list_name, None)  # Invalidate cache
        return list_data

    def delete_list(self, list_name):
        """Delete list - MODIFIED to remove file"""
        # REMOVE: db.session.delete(rule_list); db.session.commit()
        # ADD:
        file_path = self.base_path / f'{list_name}.json'
        if file_path.exists():
            file_path.unlink()
            self._cache.pop(list_name, None)
            return True
        return False

    # Keep all other methods with similar modifications
```

### 2.4 Create Migration Script
```python
# File: backend/scripts/migrate_lists.py

import sqlite3
import json
from pathlib import Path
from datetime import datetime

def migrate_lists():
    """Migrate lists from SQLite to JSON files"""
    db_path = Path(__file__).parent.parent / 'database' / 'rules.db'
    lists_path = Path(__file__).parent.parent / 'lists'
    lists_path.mkdir(exist_ok=True)

    # Connect to SQLite
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()

    # Check if table exists
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='rule_lists'")
    if not cursor.fetchone():
        print("No rule_lists table found - nothing to migrate")
        return

    # Fetch all lists
    cursor.execute("SELECT * FROM rule_lists")
    lists = cursor.fetchall()

    migrated = 0
    for row in lists:
        list_data = {
            'list_name': row['name'],
            'display_name': row['display_name'] or row['name'],
            'description': row['description'] or '',
            'schema_version': row['schema_version'] or 'v2',
            'values': json.loads(row['values']) if row['values'] else [],
            'metadata': {
                'created_at': row['created_at'] or datetime.utcnow().isoformat(),
                'updated_at': row['updated_at'] or datetime.utcnow().isoformat(),
                'version': 1
            }
        }

        # Write to file
        file_path = lists_path / f"{row['name']}.json"
        with open(file_path, 'w') as f:
            json.dump(list_data, f, indent=2)

        migrated += 1
        print(f"✅ Migrated list: {row['name']}")

    conn.close()
    print(f"\n✅ Migration complete: {migrated} lists migrated")

if __name__ == '__main__':
    migrate_lists()
```

### 2.5 Validation
```bash
# Run migration
python backend/scripts/migrate_lists.py

# Verify files created
ls -la backend/lists/

# Test API
curl http://localhost:5001/api/lists
curl http://localhost:5001/api/lists/countries
```

---

## **TASK 3: Schema Module Migration** 🔴
**Files**: `schema/rules_schema.py`, `services/schema_service.py` (optional)
**Effort**: 3 hours
**Risk**: Medium (complex queries, DuckDB integration for rules only)

### 3.1 Create Directory Structure
```bash
mkdir -p backend/schemas/entities
mkdir -p backend/schemas/versions
touch backend/schemas/schema.json
```

### 3.2 Define JSON Schemas
```json
# File: backend/schemas/schema.json
{
  "entity_schema": {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "required": ["entity_name", "display_name", "attributes"],
    "properties": {
      "entity_name": {"type": "string"},
      "display_name": {"type": "string"},
      "description": {"type": "string"},
      "schema_version": {"type": "string"},
      "attributes": {
        "type": "array",
        "items": {
          "type": "object",
          "required": ["name", "type"],
          "properties": {
            "name": {"type": "string"},
            "type": {"type": "string"},
            "description": {"type": "string"},
            "required": {"type": "boolean"},
            "default": {}
          }
        }
      }
    }
  },
  "version_schema": {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "required": ["version_id", "display_name", "is_active"],
    "properties": {
      "version_id": {"type": "string"},
      "display_name": {"type": "string"},
      "description": {"type": "string"},
      "is_active": {"type": "boolean"},
      "entities": {"type": "array", "items": {"type": "string"}},
      "actions": {"type": "array", "items": {"type": "string"}}
    }
  }
}
```

### 3.3 Modify rules_schema.py (Surgical Changes)
```python
# File: schema/rules_schema.py

# ADD at top (after existing imports):
from pathlib import Path
import json

# REMOVE:
# import sqlite3

# ADD constants:
SCHEMAS_PATH = Path(__file__).parent.parent / 'schemas'

# KEEP all existing constant definitions unchanged:
ATTRIBUTES = { ... }  # ✅ No changes
ACTIONS = { ... }     # ✅ No changes
FUNCTIONS = { ... }   # ✅ No changes
KEYWORDS = { ... }    # ✅ No changes
OPERATORS = { ... }   # ✅ No changes

# MODIFY functions - keep exact signatures:

def get_all_attributes():
    """Get all attributes - KEEP AS-IS (uses constants)"""
    # ✅ No changes needed - already uses constants
    return list(ATTRIBUTES.keys())

def get_all_actions():
    """Get all actions - KEEP AS-IS (uses constants)"""
    # ✅ No changes needed - already uses constants
    return list(ACTIONS.keys())

def get_schema_attributes(version='v2'):
    """Get attributes for version - MODIFIED to use files"""
    # REMOVE: sqlite3.connect() and queries
    # ADD:
    entities_path = SCHEMAS_PATH / 'entities'
    all_attributes = []

    for entity_file in entities_path.glob('*.json'):
        with open(entity_file, 'r') as f:
            entity_data = json.load(f)
            if entity_data.get('schema_version') == version or version == 'both':
                all_attributes.extend(entity_data.get('attributes', []))

    return all_attributes

def get_attributes_by_entity(entity_name):
    """Get attributes for entity - MODIFIED to use files"""
    # REMOVE: sqlite3 queries
    # ADD:
    entity_file = SCHEMAS_PATH / 'entities' / f'{entity_name}.json'
    if not entity_file.exists():
        return []

    with open(entity_file, 'r') as f:
        entity_data = json.load(f)
        return entity_data.get('attributes', [])

def get_schema_versions():
    """Get all versions - MODIFIED to use files"""
    # REMOVE: sqlite3 queries
    # ADD:
    versions_path = SCHEMAS_PATH / 'versions'
    versions = []

    for version_file in versions_path.glob('*.json'):
        with open(version_file, 'r') as f:
            version_data = json.load(f)
            versions.append(version_data)

    return versions

def get_schema_for_version(version_id):
    """Get full schema for version - MODIFIED to use files"""
    # REMOVE: sqlite3 queries
    # ADD:
    version_file = SCHEMAS_PATH / 'versions' / f'{version_id}.json'
    if not version_file.exists():
        return None

    with open(version_file, 'r') as f:
        return json.load(f)

def detect_rule_schema_version(rule_content):
    """Detect schema version from rule - KEEP LOGIC, use file-based check"""
    # ✅ Keep existing detection logic
    # Just ensure it doesn't use SQLite for lookups

    # Existing heuristic-based detection should work as-is
    # Example: if 'applicant.creditScore' in rule_content: return 'v2'
    pass  # Keep existing implementation

# Keep all other functions with similar modifications
```

### 3.4 Create Migration Script
```python
# File: backend/scripts/migrate_schemas.py

import sqlite3
import json
from pathlib import Path
from datetime import datetime

def migrate_schemas():
    """Migrate schemas from SQLite to JSON files"""
    db_path = Path(__file__).parent.parent / 'database' / 'rules.db'
    schemas_path = Path(__file__).parent.parent / 'schemas'
    entities_path = schemas_path / 'entities'
    versions_path = schemas_path / 'versions'

    entities_path.mkdir(parents=True, exist_ok=True)
    versions_path.mkdir(parents=True, exist_ok=True)

    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()

    # Check if tables exist
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='schema_entities'")
    if not cursor.fetchone():
        print("No schema_entities table - nothing to migrate")
        return

    # Migrate entities
    cursor.execute("SELECT * FROM schema_entities")
    entities = cursor.fetchall()

    for entity_row in entities:
        # Get attributes for this entity
        cursor.execute("""
            SELECT * FROM schema_attributes
            WHERE entity_id = ?
        """, (entity_row['id'],))
        attributes = cursor.fetchall()

        entity_data = {
            'entity_name': entity_row['name'],
            'display_name': entity_row['display_name'] or entity_row['name'],
            'description': entity_row['description'] or '',
            'schema_version': entity_row['schema_version'] or 'v2',
            'attributes': [
                {
                    'name': attr['name'],
                    'type': attr['type'],
                    'description': attr['description'] or '',
                    'required': bool(attr['required']),
                    'default': attr['default_value']
                }
                for attr in attributes
            ],
            'metadata': {
                'created_at': entity_row['created_at'] or datetime.utcnow().isoformat(),
                'updated_at': entity_row['updated_at'] or datetime.utcnow().isoformat()
            }
        }

        file_path = entities_path / f"{entity_row['name']}.json"
        with open(file_path, 'w') as f:
            json.dump(entity_data, f, indent=2)

        print(f"✅ Migrated entity: {entity_row['name']}")

    # Migrate versions
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='schema_versions'")
    if cursor.fetchone():
        cursor.execute("SELECT * FROM schema_versions")
        versions = cursor.fetchall()

        for ver_row in versions:
            version_data = {
                'version_id': ver_row['version_id'],
                'display_name': ver_row['display_name'] or ver_row['version_id'],
                'description': ver_row['description'] or '',
                'is_active': bool(ver_row['is_active']),
                'entities': json.loads(ver_row['entities']) if ver_row['entities'] else [],
                'actions': json.loads(ver_row['actions']) if ver_row['actions'] else [],
                'created_at': ver_row['created_at'] or datetime.utcnow().isoformat()
            }

            file_path = versions_path / f"{ver_row['version_id']}.json"
            with open(file_path, 'w') as f:
                json.dump(version_data, f, indent=2)

            print(f"✅ Migrated version: {ver_row['version_id']}")

    conn.close()
    print("\n✅ Schema migration complete")

if __name__ == '__main__':
    migrate_schemas()
```

### 3.5 Validation
```bash
# Run migration
python backend/scripts/migrate_schemas.py

# Verify structure
tree backend/schemas/

# Test API
curl http://localhost:5001/api/schema
curl http://localhost:5001/api/schema/attributes
curl http://localhost:5001/api/schema/entities/applicant
```

---

## **TASK 4: Hierarchy Arbitrary Depth** 🎨
**File**: `api/hierarchy.py`
**Effort**: 1 hour
**Risk**: Low (enhancement, not breaking change)

### 4.1 Current Implementation Analysis
```python
# Current: api/hierarchy.py scans folders but assumes 3 levels
# Client → ProcessGroup → ProcessArea

# Goal: Support arbitrary depth without hardcoding levels
```

### 4.2 Enhanced Hierarchy Implementation
```python
# File: api/hierarchy.py (MODIFY existing function)

from flask import Blueprint, request, jsonify
from pathlib import Path
import json

hierarchy_bp = Blueprint('hierarchy', __name__)

# UPDATE path constant:
RULES_PATH = Path(__file__).parent.parent / 'rules'  # Changed from rules_data

@hierarchy_bp.route('/hierarchy/tree', methods=['GET'])
def get_hierarchy_tree():
    """Get complete hierarchy tree - ENHANCED for arbitrary depth"""
    try:
        # REPLACE entire function with recursive approach:

        def scan_directory(path, depth=0, max_depth=10):
            """Recursively scan directory structure"""
            if depth > max_depth or not path.is_dir():
                return None

            node = {
                'name': path.name,
                'path': str(path.relative_to(RULES_PATH)),
                'type': 'folder',
                'children': [],
                'rule_count': 0
            }

            # Scan children
            for child_path in sorted(path.iterdir()):
                if child_path.name.startswith('.'):
                    continue

                if child_path.is_dir():
                    # Recursive subfolder
                    child_node = scan_directory(child_path, depth + 1, max_depth)
                    if child_node:
                        node['children'].append(child_node)
                        node['rule_count'] += child_node['rule_count']

                elif child_path.suffix == '.json' and child_path.name.startswith('rule-'):
                    # Rule file
                    node['rule_count'] += 1

            return node

        if not RULES_PATH.exists():
            return jsonify({'tree': []})

        # Build tree from all top-level folders
        tree = []
        for top_level in sorted(RULES_PATH.iterdir()):
            if top_level.is_dir() and not top_level.name.startswith('.'):
                node = scan_directory(top_level)
                if node:
                    tree.append(node)

        return jsonify({
            'tree': tree,
            'total_nodes': len(tree),
            'supports_arbitrary_depth': True
        })

    except Exception as e:
        return jsonify({'error': str(e)}), 500

# Keep other existing endpoints unchanged
```

### 4.3 Validation
```bash
# Test arbitrary depth support
mkdir -p backend/rules/client1/group1/area1/subarea1/subarea2
echo '{"id": "test"}' > backend/rules/client1/group1/area1/subarea1/subarea2/rule-test.json

# Test API
curl http://localhost:5001/api/hierarchy/tree | jq
# Should show nested structure to subarea2 level
```

---

## **TASK 5: Remove Context Legacy API** ❌
**File**: `api/context.py`
**Effort**: 30 minutes
**Risk**: Low (removal, not modification)

### 5.1 Verify No Frontend Usage
```bash
# Check frontend usage
cd frontend
grep -r "/api/context[^s]" src/
# Note: /api/contexts (with 's') is the new one - keep that
# Remove: /api/context (without 's') if unused
```

### 5.2 Remove API File
```bash
# Remove legacy context API
git rm backend/api/context.py

# Update app.py
# REMOVE lines:
# from api.context import bp as context_bp
# app.register_blueprint(context_bp)
```

### 5.3 Update app.py
```python
# File: backend/app.py

# REMOVE these lines:
# from api.context import bp as context_bp
# app.register_blueprint(context_bp)

# Keep these (new file-based):
from api.contexts_file import contexts_file_bp as contexts_bp
app.register_blueprint(contexts_bp, url_prefix='/api')
```

---

## **TASK 6: Remove SQLite Models & Dependencies** 🗑️
**Files**: `models.py`, `app.py`, `config.py`, `requirements.txt`
**Effort**: 1 hour
**Risk**: Low (clean removal)

### 6.1 Update models.py
```python
# File: backend/models.py

# DELETE entire file content and replace with minimal version:

# This file is retained for backward compatibility but no longer uses SQLAlchemy
# All models have been migrated to file-based storage

# If you need model definitions for reference, see:
# - Rules: backend/rules/{client}/{group}/{area}/rule-{id}.json
# - Lists: backend/lists/{list_name}.json
# - Schemas: backend/schemas/entities/{entity_name}.json

# Placeholder for potential future use
class _Deprecated:
    """All models have been migrated to file-based storage"""
    pass
```

**OR completely remove and update imports**:
```bash
# Option B: Remove file completely
git rm backend/models.py

# Then update all imports:
# Find: from models import db, Client, ProcessGroup, etc.
# Remove those import lines
```

### 6.2 Update app.py
```python
# File: backend/app.py

# REMOVE:
# from models import db

# REMOVE from create_app():
# db.init_app(app)
# with app.app_context():
#     db.create_all()

# FINAL app.py should look like:
from flask import Flask
from flask_cors import CORS
from api.rules_file import rules_file_bp as rules_bp
from api.schema import schema_bp
from api.lists import lists_bp
from api.hierarchy import hierarchy_bp
from api.contexts_file import contexts_file_bp as contexts_bp
from api.java_files import java_files_bp
from config import Config
from cli_commands_file import register_commands

def create_app():
    """Application factory pattern."""
    app = Flask(__name__)
    app.config.from_object(Config)

    # Initialize extensions
    CORS(app, origins=Config.CORS_ORIGINS)

    # Register blueprints
    app.register_blueprint(rules_bp, url_prefix='/api')
    app.register_blueprint(schema_bp, url_prefix='/api')
    app.register_blueprint(lists_bp, url_prefix='/api')
    app.register_blueprint(hierarchy_bp, url_prefix='/api')
    app.register_blueprint(contexts_bp, url_prefix='/api')
    app.register_blueprint(java_files_bp)

    # Register CLI commands
    register_commands(app)

    print("✅ Application initialized - File-based storage active")

    @app.route('/api/health')
    def health_check():
        """Health check endpoint."""
        return {
            'status': 'healthy',
            'message': 'Rules authoring API running',
            'storage': 'file-based',
            'database': 'none'
        }

    return app

if __name__ == '__main__':
    app = create_app()
    app.run(debug=True, host='0.0.0.0', port=5001)
```

### 6.3 Update config.py
```python
# File: backend/config.py

import os
from pathlib import Path

class Config:
    # Base directory
    BASE_DIR = Path(__file__).parent

    # File storage paths (REMOVE DATABASE_PATH)
    RULES_PATH = BASE_DIR / 'rules'
    LISTS_PATH = BASE_DIR / 'lists'
    SCHEMAS_PATH = BASE_DIR / 'schemas'

    # REMOVE these lines:
    # DATABASE_PATH = BASE_DIR / 'database' / 'rules.db'
    # SQLALCHEMY_DATABASE_URI = f'sqlite:///{DATABASE_PATH}'
    # SQLALCHEMY_TRACK_MODIFICATIONS = False

    # Python ANTLR integration (keep if used)
    ANTLR_GRAMMAR_PATH = BASE_DIR.parent / 'java-bridge' / 'src' / 'main' / 'antlr4'

    # API configuration
    SECRET_KEY = os.environ.get('SECRET_KEY') or 'dev-secret-key-change-in-production'

    # CORS configuration
    CORS_ORIGINS = ['http://localhost:3000']

    # Pagination
    DEFAULT_PAGE_SIZE = 10
    MAX_PAGE_SIZE = 100

    # DuckDB configuration (for analytics)
    DUCKDB_MEMORY_LIMIT = '1GB'
    DUCKDB_THREADS = 4
```

### 6.4 Update requirements.txt
```bash
# File: backend/requirements.txt

Flask==2.3.3
Flask-CORS==4.0.0
python-dotenv==1.0.0
marshmallow==3.20.1
requests==2.31.0
antlr4-python3-runtime==4.13.2
jsonschema>=4.0.0
duckdb>=0.9.2

# REMOVED (SQLite dependencies):
# Flask-SQLAlchemy==3.0.5
# Flask-Migrate==4.0.5
# marshmallow-sqlalchemy==0.29.0
```

### 6.5 Update Virtual Environment
```bash
cd backend

# Backup current requirements
cp requirements.txt requirements.txt.backup

# Recreate venv
deactivate
rm -rf venv
python3 -m venv venv
source venv/bin/activate

# Install new dependencies
pip install -r requirements.txt

# Verify no SQLAlchemy
pip list | grep -i sql
# Should return: 0 matches
```

### 6.6 Archive SQLite Files
```bash
mkdir -p backend/database/archive
mv backend/database/*.db backend/database/archive/
mv backend/database/*.db.backup* backend/database/archive/ 2>/dev/null || true

# Update .gitignore
echo "database/archive/*" >> backend/.gitignore
echo "database/*.db" >> backend/.gitignore
```

---

## **TASK 7: Comprehensive Testing** ✅
**Effort**: 2 hours
**Risk**: Critical (validation of all changes)

### 7.1 Pre-Testing Checklist
```bash
# Ensure all migrations complete
python backend/scripts/migrate_lists.py
python backend/scripts/migrate_schemas.py

# Verify directory structure
tree -L 3 backend/ -I 'venv|__pycache__|*.pyc'

# Start backend
cd backend
python app.py
# Should start without SQLite errors
```

### 7.2 Unit Tests
```bash
# Run existing tests - should mostly pass
cd backend
pytest tests/ -v

# Tests that should still pass (unchanged interfaces):
# - test_rules_api.py (if exists)
# - test_lists_api.py (if exists)
# - test_schema_api.py (if exists)

# New tests needed:
# - test_list_service.py (file I/O)
# - test_schema_service.py (file I/O)
```

### 7.3 Integration Tests
```bash
# Test all API endpoints
curl http://localhost:5001/api/health

# Rules
curl http://localhost:5001/api/rules
curl http://localhost:5001/api/rules?client=AMEX

# Lists
curl http://localhost:5001/api/lists
curl http://localhost:5001/api/lists/countries

# Schema
curl http://localhost:5001/api/schema
curl http://localhost:5001/api/schema/attributes
curl http://localhost:5001/api/schema/entities/applicant

# Hierarchy (arbitrary depth)
curl http://localhost:5001/api/hierarchy/tree | jq

# Contexts
curl http://localhost:5001/api/contexts
```

### 7.4 Frontend Integration
```bash
cd frontend
npm install
npm start

# Manual testing:
# 1. Open http://localhost:3000
# 2. Create new rule - verify autocomplete works
# 3. Edit rule - verify syntax highlighting
# 4. Delete rule - verify file removed
# 5. Check lists dropdown - verify lists load
# 6. Test schema suggestions - verify attributes appear
```

### 7.5 Performance Testing
```python
# Create: backend/tests/test_performance.py

import time
from services.rule_file_service import RuleFileService
from services.list_service import ListService
from services.rule_query_service import RuleQueryService
import duckdb

def test_rule_read_performance():
    """Test: <10ms average"""
    service = RuleFileService()

    times = []
    for _ in range(100):
        start = time.time()
        service.get_rule('test_rule_id')
        times.append((time.time() - start) * 1000)

    avg_time = sum(times) / len(times)
    assert avg_time < 10, f"Average read time {avg_time}ms exceeds 10ms"
    print(f"✅ Rule read: {avg_time:.2f}ms average")

def test_duckdb_analytics_performance():
    """Test: <500ms for complex queries"""
    query_service = RuleQueryService()

    start = time.time()
    stats = query_service.get_dashboard_stats()
    duration = (time.time() - start) * 1000

    assert duration < 500, f"Analytics query {duration}ms exceeds 500ms"
    print(f"✅ DuckDB analytics: {duration:.2f}ms")

def test_list_read_performance():
    """Test: <5ms average"""
    service = ListService()

    times = []
    for _ in range(100):
        start = time.time()
        service.get_list('countries')
        times.append((time.time() - start) * 1000)

    avg_time = sum(times) / len(times)
    assert avg_time < 5, f"Average list read {avg_time}ms exceeds 5ms"
    print(f"✅ List read: {avg_time:.2f}ms average")

if __name__ == '__main__':
    test_rule_read_performance()
    test_duckdb_analytics_performance()
    test_list_read_performance()
    print("\n✅ All performance tests passed")
```

### 7.6 Regression Testing
```bash
# If you have existing regression suite:
python backend/test_regression_suite.py

# Verify:
# - All previously passing tests still pass
# - No data loss
# - No performance degradation
# - All features work as before
```

---

## 📦 **Execution Sequence** (Parallel)

Since all tasks modify different files and have minimal dependencies, execute in parallel:

### **Developer 1** (or Session 1):
- ✅ TASK 1: Directory rename
- ✅ TASK 2: Lists migration
- ✅ TASK 7.1-7.3: Backend testing

### **Developer 2** (or Session 2):
- ✅ TASK 3: Schema migration
- ✅ TASK 4: Hierarchy enhancement
- ✅ TASK 7.4-7.5: Frontend & performance testing

### **Developer 3** (or Session 3):
- ✅ TASK 5: Remove context legacy
- ✅ TASK 6: Remove SQLite dependencies
- ✅ TASK 7.6: Regression testing

**Merge Strategy**:
```bash
# Create feature branch
git checkout -b feature/sqlite-retirement

# All developers work on same branch
# Or: use separate branches and merge incrementally

# Test after each task completion
# Commit after successful testing
```

---

## 🚨 **Rollback Plan**

### Quick Rollback (Per Task)
```bash
# Task 1 rollback (directory rename)
mv backend/rules backend/rules_data
git checkout services/rule_file_service.py api/rules_file.py

# Task 2 rollback (lists)
rm -rf backend/lists
git checkout services/list_cache.py

# Task 3 rollback (schema)
rm -rf backend/schemas
git checkout schema/rules_schema.py

# Task 6 rollback (dependencies)
git checkout models.py app.py config.py requirements.txt
cp database/archive/rules.db database/rules.db
pip install -r requirements.txt.backup
```

### Complete Rollback
```bash
# Nuclear option: restore everything
git checkout HEAD backend/
cp database/archive/rules.db database/rules.db
pip install Flask-SQLAlchemy Flask-Migrate marshmallow-sqlalchemy
python app.py  # Verify works
```

---

## ✅ **Success Criteria**

### Technical
- [ ] Zero `db.session` or `db.Model` references in production code
- [ ] Zero SQLite dependencies in `requirements.txt`
- [ ] All API endpoints return valid responses
- [ ] Frontend works without changes
- [ ] Existing tests pass (>95%)
- [ ] Performance maintained (<10ms read, <100ms write)
- [ ] DuckDB analytics <500ms

### Operational
- [ ] Backend starts without SQLite errors
- [ ] All 35 existing rules still accessible
- [ ] Git diff shows clean file changes
- [ ] Documentation updated
- [ ] Team can clone and run immediately

---

## 📝 **Post-Migration Tasks**

### Immediate
- [ ] Update README.md architecture section
- [ ] Update CLAUDE.md memory with new structure
- [ ] Document DuckDB analytics usage
- [ ] Create team migration guide
- [ ] Tag release: `v2.0.0-file-storage`

### Short-term (Week 1)
- [ ] Monitor system performance
- [ ] Gather user feedback
- [ ] Optimize slow queries (if any)
- [ ] Add missing test coverage
- [ ] Create migration retrospective

---

## 🚀 **Ready to Execute**

**Final Confirmation**:
- [x] Directory naming: `rules/`, `lists/`, `schemas/`
- [x] DuckDB: Only for rules analytics + dashboard
- [x] Hierarchy: Arbitrary folder depth support
- [x] Execution: All tasks in parallel
- [x] Testing: Preserve interfaces, test file I/O layer
- [x] Code changes: Surgical, minimal modifications

**Begin with**:
```bash
cd /Users/chandramohn/workspace/rules_engine/ui-prototype
git checkout -b feature/sqlite-retirement
git tag pre-sqlite-retirement-v2

# Start parallel execution:
# Terminal 1: Tasks 1-2
# Terminal 2: Tasks 3-4
# Terminal 3: Tasks 5-6
```

**Estimated Completion**: 6-8 hours (all tasks parallel)

---

**Ready to proceed? Execute all tasks in parallel for fastest migration!**
