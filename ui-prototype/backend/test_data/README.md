# Test Data Directory

This directory contains all test/demo data for the rules engine, stored as JSON files.

## Directory Structure

```
test_data/
├── schemas/                    # Schema entity definitions
│   ├── applicant.json         # Applicant schema with attributes
│   └── transaction.json       # Transaction schema with attributes
│
├── contexts/                   # Test contexts for rule execution
│   ├── *.json                 # Individual test context files
│   └── schema-templates/      # Schema template contexts
│       └── *.json
│
└── demo_rules/                 # Demo rule templates
    ├── DEMO/                  # Mirror of rules_data hierarchy
    │   ├── CC_STD/
    │   │   ├── APPROVAL/
    │   │   └── FRAUD/
    │   └── CC_PREM/
    └── PREMIUM/
```

## Purpose

### `schemas/`
- Replaces `SchemaEntity` and `SchemaAttribute` database tables
- Defines data structures for rule validation
- Used by rule editor for autocomplete and validation

### `contexts/`
- Replaces `RuleContext` database table
- Test data for rule execution and testing
- Schema templates for creating new contexts

### `demo_rules/`
- Template rules for `flask seed-demo` command
- Copied to `rules_data/` when seeding demo data
- Version controlled in git for consistent demo experience

## Usage

### Seeding Demo Data
```bash
flask seed-demo
```
Copies demo rules from `test_data/demo_rules/` to `rules_data/`

### Clearing Data
```bash
flask clear-data
```
Deletes all files in `rules_data/` (production rules)

### Getting Info
```bash
flask db-info
```
Scans file system and reports rule counts

## Services

- **SchemaFileService** (`services/schema_file_service.py`) - Schema CRUD operations
- **ContextFileService** (`services/context_file_service.py`) - Context CRUD operations
- **RuleFileService** (`services/rule_file_service.py`) - Rule CRUD operations

## Git Strategy

- **Committed**: All files in `test_data/` (templates and test data)
- **Gitignored**: All files in `rules_data/` (working rules)

This allows developers to always reset to a clean demo state.

## Migration Notes

**Date**: 2025-10-13
**Status**: Migrated from SQLite to file-based storage

Previously stored in SQLite tables:
- `schema_entities` → `schemas/*.json`
- `schema_attributes` → embedded in schema files
- `rule_contexts` → `contexts/*.json`
- Demo data → `demo_rules/` (from `fixtures/demo_data.py`)

