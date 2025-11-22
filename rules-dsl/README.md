# Rules DSL - VS Code Extension

**Status**: Week 1 Implementation Complete ✅
**Version**: 1.0.0-alpha
**Extension Name**: `rules-dsl`

---

## 📋 Project Overview

This project provides VS Code extension support for the Rules Engine DSL with a hybrid LSP + ANTLR architecture for optimal performance and production-grade validation.

### Architecture

```
rules-dsl/
├── rules/                  # Rules data (top-level)
│   ├── mon/                # Monetary .rules files
│   ├── non-mon/            # Non-monetary .rules files
│   ├── actionsets/         # Action set .rules files
│   ├── schemas/            # Entity schemas (applicant, transaction)
│   ├── contexts/           # Test contexts
│   └── lists/              # Value lists
├── backend/                # Flask backend (.rules only)
├── extension/              # VS Code extension (TypeScript)
├── java-bridge/            # ANTLR grammar & code generation
└── rules.config.yaml       # Workspace configuration
```

---

## 🚀 Quick Start

### 1. Backend Setup

```bash
cd backend
python3 -m venv venv
source venv/bin/activate  # Fish: source venv/bin/activate.fish
pip install -r requirements.txt
python app.py  # Runs on port 5002
```

### 2. Extension Development (Week 2+)

```bash
cd extension
npm install
npm run compile
# Press F5 in VS Code to launch extension development host
```

---

## 📄 File Format: `.rules`

### Basic Rule (No Frontmatter)
```
# Basic credit score validation for standard cards

rule creditScoreCheck:
    if applicant.creditScore >= 700 then approveApplication
    if applicant.creditScore < 600 then rejectApplication
```

### Rule with Test Context
```yaml
---
context: premium_approval_context
---
# Platinum card eligibility requirements

rule platinumEligibility:
    if applicant.creditScore >= 750 and applicant.income >= 120000 then
        approveApplication with "Platinum Approved"
```

### Rule with Effective/Expiry Dates
```yaml
---
context: premium_approval_context
effective: 2025-12-01
expires: 2026-01-31
---
# Holiday season platinum card promotion

rule holidayPromotion:
    if applicant.creditScore >= 750 and applicant.income >= 100000 then
        approveApplication with "Platinum - Holiday Promo"
```

---

## ✅ Week 1 Progress (Complete)

- [x] Created `rules-dsl` project structure
- [x] Copied backend from `ui-prototype` with clean separation
- [x] Converted all 34 JSON rules → `.rules` format (big bang migration)
- [x] Built clean `.rules`-only file service (no JSON support)
- [x] Added PyYAML for frontmatter parsing
- [x] Created workspace configuration (`rules.config.yaml`)
- [x] Updated backend config to point to `../rules/`

### Migration Statistics

- **Total Rules Converted**: 34
- **Monetary Rules**: 23
- **Non-Monetary Rules**: 4
- **Action Sets**: 7
- **Schemas**: 2 (applicant, transaction)
- **Contexts**: 2 (standard, premium)

---

## 🎯 Next Steps: Week 2

### Tasks
1. Initialize VS Code extension scaffold (`yo code`)
2. Define `.rules` language in `package.json`
3. Implement TextMate grammar for syntax highlighting
4. Load workspace config on activation
5. Auto-discover entity schemas
6. Create Flask API client
7. Implement basic commands (validate, generate)

### Deliverable
VS Code extension with syntax highlighting + schema loading + ANTLR validation

---

## 🔧 Development

### Backend Port
- **Port**: 5002 (vs ui-prototype on 5001)
- **Reason**: Allows both systems to run simultaneously during development

### Workspace Config
Edit `rules.config.yaml` to configure paths and backend URL.

### Testing
```bash
# Test rules file service
cd backend
python -c "
from services.rules_file_service import RulesFileService
from pathlib import Path

service = RulesFileService()
rules = service.list_rules('mon')
print(f'Found {len(rules)} monetary rules')
for rule in rules[:3]:
    print(f'  - {rule[\"name\"]}')
"
```

---

## 📚 Resources

- **Implementation Spec**: `docs/VSCODE_EXTENSION_IMPLEMENTATION_SPEC.md`
- **Conversion Script**: `scripts/convert_json_to_rules.py`
- **Original Project**: `../ui-prototype/`

---

## 🎨 Design Decisions

### Why `.rules` Only?
- Clean codebase optimized for VS Code plugin
- No dual format complexity
- Single source of truth
- Easier to maintain and evolve

### Why Top-Level `rules/` Directory?
- Clear separation between code (`backend/`, `extension/`) and data (`rules/`)
- VS Code workspace friendly
- Git-native with clean diffs
- Intuitive project structure

### Why No `entities/` Subfolder in `schemas/`?
- Simpler structure: `rules/schemas/` instead of `rules/schemas/entities/`
- All schema files are entities anyway
- Reduces nesting depth

---

**Last Updated**: November 22, 2025
**Status**: Week 1 Complete, Week 2 Ready to Start
