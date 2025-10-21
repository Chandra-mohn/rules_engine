# Test Data Migration - Quick Reference

## ✅ What's Complete

All test/demo data migrated from SQLite to file-based JSON storage:
- ✅ Schemas (2 files)
- ✅ Demo rules (31 files)
- ✅ Test contexts (3 files)
- ✅ Context API (file-based)
- ✅ CLI commands (file-based)

## 📂 Directory Structure

```
backend/
├── rules_data/              # Production (gitignored)
└── test_data/               # Test/demo (committed)
    ├── schemas/            # 2 schema entities
    ├── contexts/           # 3 test contexts
    └── demo_rules/         # 31 demo rule templates
```

## 🔧 CLI Commands

```bash
# Seed demo data (copy from test_data to rules_data)
flask seed-demo

# Clear working data (delete rules_data contents)
flask clear-data

# Show statistics (scan file system)
flask db-info
```

## 📦 Services

```python
# Schema service
from services.schema_file_service import SchemaFileService
schema_service = SchemaFileService()
schema = schema_service.get_schema('applicant')

# Context service
from services.context_file_service import ContextFileService
context_service = ContextFileService()
context = context_service.get_context('Standard Approval Context')

# Rule service (already exists)
from services.rule_file_service import RuleFileService
rule_service = RuleFileService()
rules = rule_service.list_rules()
```

## 🌐 API Endpoints

```bash
# Context API (file-based, same interface as SQLite version)
GET    /api/contexts                       # List contexts
GET    /api/contexts/<name>                # Get context
POST   /api/contexts                       # Create context
PUT    /api/contexts/<name>                # Update context
DELETE /api/contexts/<name>                # Delete context
POST   /api/contexts/<name>/clone         # Clone context
GET    /api/contexts/schema-templates     # List templates
```

## ⚡ Quick Actions

### Reset to Demo Data
```bash
flask clear-data && flask seed-demo
```

### Create New Context
```bash
curl -X POST http://localhost:5001/api/contexts \
  -H "Content-Type: application/json" \
  -d '{
    "name": "My Test Context",
    "context_data": {"applicant": {"creditScore": 750}}
  }'
```

### Export Current Demo Data
```bash
python scripts/export_demo_data.py
```

## 🎯 Next Step: Activation

To activate in `app.py`, replace:

```python
# OLD (SQLite)
from cli_commands import register_commands
from api.contexts import contexts_bp

# NEW (File-based)
from cli_commands_file import register_commands
from api.contexts_file import contexts_file_bp as contexts_bp
```

## 📝 Key Files

- `services/schema_file_service.py` - Schema CRUD
- `services/context_file_service.py` - Context CRUD
- `api/contexts_file.py` - Context API endpoints
- `cli_commands_file.py` - CLI commands
- `scripts/export_demo_data.py` - Export script
- `test_data/README.md` - Full documentation

## ✅ Status: Ready for Production
All functionality tested and working. Safe to activate whenever ready.
