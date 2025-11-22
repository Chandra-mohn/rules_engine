# VS Code Extension Implementation Specification

**Document Version**: 2.0
**Date**: November 22, 2025
**Status**: Ready for Implementation
**Decision**: Approved hybrid LSP + ANTLR architecture for optimal performance

---

## 📋 Executive Summary

This specification defines the complete architecture for migrating the Rules Engine from React + Flask web application to VS Code extension + Flask backend (hybrid approach).

**Key Principles:**
- **Git-native**: Git is the single source of truth for versioning, authorship, and timestamps
- **Convention over configuration**: Minimize explicit configuration through filesystem conventions
- **Minimal metadata**: Only business-critical metadata retained (context, effective/expiry dates)
- **Zero grammar changes**: No modifications to ANTLR DSL grammar
- **Backward compatible**: Support existing JSON rules during transition
- **Hybrid parsing**: LSP for fast editing, ANTLR for production validation/codegen

---

## 🎯 Architecture Overview

### Hybrid LSP + ANTLR Architecture

**Design Philosophy:** Separate editing experience (LSP) from production validation/codegen (ANTLR)

```
┌─────────────────────────────────────────────────────┐
│         VS Code Extension (TypeScript)               │
│                                                      │
│  ┌────────────────────────────────────────────────┐ │
│  │  Language Server Protocol (LSP)                │ │
│  │                                                │ │
│  │  LOCAL FEATURES (Fast, No Network):            │ │
│  │  ✅ Syntax Highlighting (TextMate grammar)     │ │
│  │  ✅ IntelliSense (schema-driven autocomplete)  │ │
│  │  ✅ Hover Documentation (schema-driven)        │ │
│  │  ✅ Basic Schema Validation (entity/fields)    │ │
│  │  ✅ CodeLens (context preview, dates)          │ │
│  │  ✅ Frontmatter Parsing (YAML)                 │ │
│  │                                                │ │
│  │  Performance: < 100ms response time            │ │
│  │  Works Offline: ✅ Yes (except full validation)│ │
│  └────────────────────────────────────────────────┘ │
└──────────────┬──────────────────────────────────────┘
               │
               │ HTTP/REST (localhost:5001)
               │ Only for: Validation & Code Generation
               ↓
┌─────────────────────────────────────────┐
│      Flask Backend (Python)              │
│                                          │
│  ┌────────────────────────────────────┐ │
│  │  ANTLR Parser (Source of Truth)    │ │
│  │                                    │ │
│  │  REMOTE FEATURES (Production):     │ │
│  │  ✅ Full Syntax Validation         │ │
│  │  ✅ Java Code Generation           │ │
│  │  ✅ Bytecode Compilation           │ │
│  │  ✅ Rule Testing & Execution       │ │
│  │  ✅ Batch Processing               │ │
│  │                                    │ │
│  │  Triggered: On save, manual cmd    │ │
│  └────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

**Key Design Decisions:**

1. **LSP handles editing experience** - Fast, local, offline-capable
2. **ANTLR handles production tasks** - Accurate, complete, authoritative
3. **No duplicate parser maintenance** - LSP uses schema validation, ANTLR is grammar source of truth
4. **Best of both worlds** - Instant autocomplete + production-grade validation

---

## 📄 File Format Specification

### 1. File Extension

**Extension:** `.rules`

**Purpose:** Single file extension for all rule types (monetary, non-monetary, action sets)

**Rationale:** Folder structure indicates rule type, not file extension

### 2. File Structure

```yaml
---
# Optional frontmatter (ALL fields optional)
context: <context_name>     # Test context reference
effective: YYYY-MM-DD       # Effective date
expires: YYYY-MM-DD         # Expiry date
---
# Comments and human-readable documentation
# Additional comments about rule logic, business requirements, etc.

rule ruleName:
    if condition then action
    if condition then action
```

### 3. Frontmatter Specification

**Format:** YAML (between `---` delimiters)

**Fields (ALL OPTIONAL):**

| Field | Type | Format | Required | Default | Description |
|-------|------|--------|----------|---------|-------------|
| `context` | string | Context name | No | None | Test context for rule execution |
| `effective` | string | YYYY-MM-DD | No | Immediate | Date when rule becomes active |
| `expires` | string | YYYY-MM-DD | No | Never | Date when rule becomes inactive |

**Validation Rules:**
1. Frontmatter section is entirely optional (can omit `---` delimiters)
2. If present, ONLY these 3 fields are permitted
3. All fields within frontmatter are optional
4. Date format must be ISO 8601 (YYYY-MM-DD)
5. If both dates present: `expires` must be after `effective`
6. Unknown fields in frontmatter → VS Code warning

**Business Logic:**
- **No frontmatter** → Rule effective immediately, never expires, no test context
- **`effective` missing** → Effective immediately (from git commit date)
- **`expires` missing** → Never expires (open-ended rule)
- **`context` missing** → No default test context assigned

### 4. Example Files

#### Example 1: Basic Rule (No Frontmatter)

**File:** `backend/rules/mon/DEMO/CC_STD/APPROVAL/creditScoreCheck.rules`

```
# Credit score validation for standard cards
# Approves applicants with FICO >= 700
# Rejects applicants with FICO < 600

rule creditScoreCheck:
    if applicant.creditScore >= 700 then approveApplication
    if applicant.creditScore < 600 then rejectApplication
```

**Behavior:** Effective immediately, never expires, no test context

---

#### Example 2: Rule with Test Context

**File:** `backend/rules/mon/PREMIUM/PLATINUM/ELIGIBILITY/platinumEligibility.rules`

```yaml
---
context: premium_approval_context
---
# Platinum card eligibility requirements
# High credit score and income threshold

rule platinumEligibility:
    if applicant.creditScore >= 750 and applicant.income >= 120000 then
        approveApplication with "Platinum Approved"
    else
        rejectApplication with "Does not meet platinum criteria"
```

**Behavior:** Uses premium_approval_context for testing, effective immediately, never expires

---

#### Example 3: Seasonal Rule with Dates

**File:** `backend/rules/mon/PREMIUM/PLATINUM/PROMOTIONS/holidayPromotion.rules`

```yaml
---
context: premium_approval_context
effective: 2025-12-01
expires: 2026-01-31
---
# Holiday season platinum card promotion
# Temporary reduced income threshold for Q4 2025
# AUTO-EXPIRES: January 31, 2026

rule holidayPromotion:
    if applicant.creditScore >= 750 and applicant.income >= 100000 then
        approveApplication with "Platinum - Holiday Promo"
```

**Behavior:** Active Dec 1, 2025 → Jan 31, 2026, uses premium context, auto-expires

---

#### Example 4: Action Set (No Dates)

**File:** `backend/rules/actionsets/DEMO/CC_STD/ACTIONS/approvalActions.rules`

```yaml
---
context: standard_approval_context
---
# Standard approval action set
# Sends email, creates account, issues card

actionset standardApprovalActions:
    sendEmail to applicant.email with "Application Approved"
    createAccount with applicant.details
    issueCard with cardType="STANDARD"
```

**Behavior:** Uses standard context for testing, effective immediately, never expires

---

## 📁 Directory Structure Specification

### 1. Folder-Based Rule Type Convention

**Principle:** Folder structure defines rule taxonomy, not metadata

```
backend/rules/
├── mon/                              # Monetary rules
│   ├── DEMO/
│   │   ├── CC_STD/
│   │   │   ├── APPROVAL/
│   │   │   │   ├── creditScoreCheck.rules
│   │   │   │   └── incomeVerification.rules
│   │   │   └── FRAUD/
│   │   │       └── velocityCheck.rules
│   │   └── CC_PREM/
│   │       └── CREDIT_LIMITS/
│   │           └── premiumCreditLimit.rules
│   └── PREMIUM/
│       └── PLATINUM/
│           └── ELIGIBILITY/
│               └── platinumEligibility.rules
│
├── non-mon/                          # Non-monetary rules
│   └── DEMO/CC_STD/
│       └── FRAUD/
│           ├── deviceFingerprint.rules
│           └── geoLocationCheck.rules
│
└── actionsets/                       # Action sets
    └── DEMO/CC_STD/
        └── ACTIONS/
            ├── approvalActions.rules
            └── rejectionActions.rules
```

**Hierarchy Preserved:**
- Level 1: Rule type (`mon/`, `non-mon/`, `actionsets/`)
- Level 2: Client code (`DEMO/`, `PREMIUM/`)
- Level 3: Process group (`CC_STD/`, `CC_PREM/`)
- Level 4: Process area (`APPROVAL/`, `FRAUD/`, `ELIGIBILITY/`)
- Level 5: Rule files (`*.rules`)

**Auto-Discovery:**
- VS Code extension scans `backend/rules/` for subdirectories
- Each subdirectory becomes a rule type
- No configuration needed

---

### 2. Supporting Directories

```
backend/
├── rules.config.yaml              # Minimal workspace configuration
├── rules/                         # Rule files (see above)
├── schemas/
│   └── entities/
│       ├── applicant.json         # Auto-discovered entity schema
│       └── transaction.json       # Auto-discovered entity schema
└── contexts/
    ├── standard_approval_context.json    # Auto-discovered test context
    └── premium_approval_context.json     # Auto-discovered test context
```

---

## ⚙️ Workspace Configuration

### Configuration File

**File:** `rules.config.yaml` (project root)

**Content:**

```yaml
rulesEngine:
  # Schema version for DSL grammar
  schemaVersion: v2

  # Convention-based discovery paths
  paths:
    entities: ./backend/schemas/entities/
    contexts: ./backend/contexts/
    rules: ./backend/rules/
```

**That's it!** Only 3 paths needed.

### Auto-Discovery Behavior

**Entities:**
- Extension scans `./backend/schemas/entities/`
- All `.json` files become available entities
- `applicant.json` → `applicant` entity
- `transaction.json` → `transaction` entity

**Contexts:**
- Extension scans `./backend/contexts/`
- All `.json` files become available contexts
- `standard_approval_context.json` → `standard_approval_context`

**Rule Types:**
- Extension scans `./backend/rules/` subdirectories
- Each subdirectory becomes a rule type
- `rules/mon/` → "Monetary Rules" type
- `rules/non-mon/` → "Non-Monetary Rules" type

**No explicit lists needed!** Filesystem is the source of truth.

---

## 🔧 LSP vs ANTLR Responsibilities

### LSP Features (Local, Fast, Offline-Capable)

**Handled by Language Server in VS Code Extension:**

| Feature | Implementation | Speed | Network Required |
|---------|---------------|-------|------------------|
| **Syntax Highlighting** | TextMate grammar | Instant | ❌ No |
| **Entity Autocomplete** | Schema-driven | < 100ms | ❌ No |
| **Field Autocomplete** | Schema-driven | < 50ms | ❌ No |
| **Hover Documentation** | Schema-driven | < 50ms | ❌ No |
| **Basic Schema Validation** | Regex + schema checks | < 100ms | ❌ No |
| **Frontmatter Parsing** | YAML parser | < 10ms | ❌ No |
| **CodeLens (Context/Dates)** | File reading | < 50ms | ❌ No |

**Code Example - LSP Autocomplete:**
```typescript
// VS Code Extension - No ANTLR needed!
class RulesCompletionProvider implements CompletionItemProvider {
  private entitySchemas: Map<string, EntitySchema>;

  provideCompletionItems(document: TextDocument, position: Position) {
    const line = document.lineAt(position.line).text;
    const prefix = line.substring(0, position.character);

    // Entity field autocomplete: "applicant."
    const match = prefix.match(/(\w+)\.$/);
    if (match) {
      const entityName = match[1];
      const schema = this.entitySchemas.get(entityName);

      if (schema) {
        // Return fields instantly from loaded schema
        return schema.attributes.map(attr => ({
          label: attr.name,
          kind: CompletionItemKind.Property,
          detail: `(${attr.type})`,
          documentation: attr.description
        }));
      }
    }

    return [];
  }
}
```

---

### ANTLR Features (Remote, Production-Grade)

**Handled by Flask Backend Python ANTLR Parser:**

| Feature | Implementation | Speed | When Triggered |
|---------|---------------|-------|----------------|
| **Full Syntax Validation** | ANTLR grammar parsing | < 500ms | On save, manual validate |
| **Java Code Generation** | ANTLR AST → Java code | < 1000ms | Manual command |
| **Bytecode Compilation** | Java compiler | < 2000ms | Manual command |
| **Rule Testing** | Python rules engine | Variable | Manual command |
| **Batch Execution** | Multi-rule orchestration | Variable | Manual command |

**Code Example - ANTLR Validation:**
```typescript
// VS Code Extension - Calls Flask ANTLR endpoint
class RulesAntlrValidator {
  async validateWithAntlr(document: TextDocument): Promise<Diagnostic[]> {
    try {
      // Send to Flask for FULL grammar validation
      const response = await axios.post('http://localhost:5001/api/validate', {
        content: document.getText()
      });

      // Convert ANTLR errors to VS Code diagnostics
      return response.data.errors.map(err => ({
        range: new Range(err.line - 1, err.column, err.line - 1, err.endColumn),
        message: err.message,
        severity: DiagnosticSeverity.Error,
        source: 'rules-antlr'
      }));

    } catch (error) {
      // Flask not running - graceful degradation
      vscode.window.showWarningMessage(
        'Flask backend not running. Full validation unavailable. Basic schema validation active.'
      );
      return [];
    }
  }
}
```

---

### Performance Comparison

| Operation | LSP Only | ANTLR Only | Hybrid (This Design) |
|-----------|----------|------------|----------------------|
| **Autocomplete** | < 100ms ✅ | ~500ms ⚠️ | < 100ms ✅ |
| **Hover Docs** | < 50ms ✅ | ~500ms ⚠️ | < 50ms ✅ |
| **Basic Validation** | < 100ms ✅ | ~500ms ⚠️ | < 100ms ✅ |
| **Full Grammar Check** | ❌ Limited | ✅ Complete | ✅ Complete (on-demand) |
| **Code Generation** | ❌ No | ✅ Yes | ✅ Yes |
| **Offline Editing** | ✅ Full | ❌ No | ✅ Autocomplete + basic validation |
| **Production Ready** | ❌ No | ✅ Yes | ✅ Yes |

**Result:** Hybrid approach provides **best of both worlds** - fast editing + production validation!

---

## 🎨 VS Code Extension Features

### 1. Syntax Highlighting (LSP - Local)

**Language Definition:**
- File extension: `.rules`
- TextMate grammar for DSL keywords
- Highlight: keywords (rule, if, then, else), operators (>=, <=, and, or), entities (applicant, transaction)

**Example:**
```
rule creditScoreCheck:           # Purple (keyword)
    if applicant.creditScore     # Blue (entity.field)
       >= 700                    # Orange (operator, number)
       then approveApplication   # Green (action)
```

**Implementation:** TextMate grammar JSON (no network calls)

---

### 2. IntelliSense (LSP - Local, Schema-Driven)

**Entity Autocomplete:**
```
rule myRule:
    if applicant.█
                 ↓
    ┌─────────────────────────────┐
    │ creditScore (number)        │ ← From applicant.json schema
    │ income (number)             │
    │ age (number)                │
    │ employmentStatus (string)   │
    └─────────────────────────────┘
```

**How it works:**
1. Extension loads all entity schemas from `entities/` folder on startup
2. User types `applicant.`
3. LSP immediately returns fields from cached schema
4. **No ANTLR needed, no network call**

**Action Autocomplete:**
- Autocomplete available actions: `approveApplication`, `rejectApplication`, etc.
- Source: Workspace config or inferred from existing rules

---

### 3. Diagnostics (Hybrid - LSP + ANTLR)

**Two-Tier Validation:**

**Tier 1: Basic Schema Validation (LSP - Instant)**
```
rule myRule:
    if applicant.unknownField >= 700 then approveApplication
                 ~~~~~~~~~~~~~
                 ⚠️ Warning: Property 'unknownField' not found in entity 'applicant'
                    Source: rules-lsp (schema validation)
```

**Tier 2: Full Grammar Validation (ANTLR - On Save)**
```
rule myRule:
    if applicant.creditScore >= 700 then unknownAction
                                          ~~~~~~~~~~~~~
                 ❌ Error: Action 'unknownAction' not recognized
                    Source: rules-antlr (grammar validation)
```

**Validation Strategy:**
1. **On keystroke**: LSP runs basic schema checks (< 100ms)
2. **On save**: ANTLR runs full grammar validation (< 500ms)
3. **On demand**: User can trigger manual validation (Cmd+Shift+V)

**Graceful Degradation:**
- If Flask offline → LSP provides basic validation
- User gets warning: "Full validation unavailable (Flask not running)"
- Can still edit rules with autocomplete and basic schema checks

---

### 4. Hover Documentation

**Entity Field Documentation:**

```
rule myRule:
    if applicant.creditScore >= 700 then approveApplication
                 ^^^^^^^^^^^
                 ℹ️ FICO credit score (300-850)
                    Type: number
                    Required: false
                    Default: null
```

**Implementation:**
- Load entity schema descriptions
- Show on hover over entity fields

---

### 5. CodeLens (Context Preview)

**Context Data Preview:**

```yaml
---
context: premium_approval_context
effective: 2025-12-01
expires: 2026-01-31
---

▼ Context: premium_approval_context (creditScore=800, income=150000) [Edit] [View JSON]
📅 Active: Dec 1, 2025 → Jan 31, 2026 (59 days remaining)

rule platinumEligibility:
    if applicant.creditScore >= 750 then approveApplication
```

**Features:**
- **Context Preview**: Shows key test data values inline
- **Date Status**: Shows effective/expiry dates with countdown
- **Click Actions**:
  - "Edit" → Opens context editor
  - "View JSON" → Opens context file in split pane

**Implementation:**
```typescript
class RuleContextCodeLens implements CodeLensProvider {
  provideCodeLenses(document: TextDocument) {
    const metadata = parseFrontmatter(document);
    const lenses = [];

    // Context CodeLens
    if (metadata.context) {
      lenses.push(new CodeLens(
        new Range(0, 0, 0, 0),
        {
          title: `▼ Context: ${metadata.context} [Edit] [View JSON]`,
          command: 'rules.showContext'
        }
      ));
    }

    // Date CodeLens
    if (metadata.effective || metadata.expires) {
      const dateText = formatDateRange(metadata.effective, metadata.expires);
      lenses.push(new CodeLens(
        new Range(1, 0, 1, 0),
        {
          title: `📅 ${dateText}`,
          command: 'rules.showDateInfo'
        }
      ));
    }

    return lenses;
  }
}
```

---

### 6. Commands

**Command Palette Actions:**

| Command | Action | Shortcut |
|---------|--------|----------|
| `Rules: Validate Rule` | Validate current rule via Flask | `Cmd+Shift+V` |
| `Rules: Generate Java Code` | Generate Java from rule | `Cmd+Shift+G` |
| `Rules: Attach Context` | Select test context | `Cmd+Shift+C` |
| `Rules: Set Effective Date` | Set effective date | - |
| `Rules: Set Expiry Date` | Set expiry date | - |
| `Rules: Remove Dates` | Clear effective/expiry dates | - |
| `Rules: Test Rule` | Execute rule with context | `Cmd+Shift+T` |

---

### 7. Tree View (Explorer)

**Rules Explorer Panel:**

```
RULES EXPLORER
├─ 📁 Monetary Rules
│  ├─ 📁 DEMO
│  │  ├─ 📁 CC_STD
│  │  │  ├─ 📁 APPROVAL
│  │  │  │  ├─ ✅ creditScoreCheck.rules
│  │  │  │  └─ ✅ incomeVerification.rules
│  │  │  └─ 📁 FRAUD
│  │  │     └─ ✅ velocityCheck.rules
│  │  └─ 📁 CC_PREM
│  │     └─ 📁 CREDIT_LIMITS
│  │        └─ ✅ premiumCreditLimit.rules
│  └─ 📁 PREMIUM
│     └─ 📁 PLATINUM
│        └─ 📁 ELIGIBILITY
│           └─ ⏰ platinumEligibility.rules (Expires in 59 days)
├─ 📁 Non-Monetary Rules
│  └─ 📁 DEMO/CC_STD/FRAUD
│     ├─ ✅ deviceFingerprint.rules
│     └─ ✅ geoLocationCheck.rules
└─ 📁 Action Sets
   └─ 📁 DEMO/CC_STD/ACTIONS
      ├─ ✅ approvalActions.rules
      └─ ✅ rejectionActions.rules
```

**Icons:**
- ✅ Valid rule
- ❌ Invalid rule (syntax errors)
- ⏰ Rule with expiry date (shows countdown)
- 🚫 Expired rule

---

## 🔄 Migration Strategy

### Phase 1: Backend Support for .rules Files

**Goal:** Flask backend reads both `.json` and `.rules` files

**Implementation:**

```python
# backend/services/rule_file_service.py

def load_rule(file_path: str) -> dict:
    """Load rule from either .json or .rules file"""

    if file_path.endswith('.json'):
        # Existing JSON loader
        return load_json_rule(file_path)

    elif file_path.endswith('.rules'):
        # New .rules loader
        return load_rules_file(file_path)

    else:
        raise ValueError(f"Unknown file format: {file_path}")


def load_rules_file(file_path: str) -> dict:
    """Parse .rules file with optional frontmatter"""

    with open(file_path, 'r') as f:
        content = f.read()

    # Check for frontmatter
    if content.startswith('---'):
        # Split frontmatter and DSL
        parts = content.split('---', 2)
        if len(parts) >= 3:
            frontmatter_yaml = parts[1]
            dsl_content = parts[2].strip()

            # Parse frontmatter
            metadata = yaml.safe_load(frontmatter_yaml) or {}
        else:
            metadata = {}
            dsl_content = content
    else:
        # No frontmatter, pure DSL
        metadata = {}
        dsl_content = content

    # Extract rule name from filename
    rule_name = Path(file_path).stem

    # Build rule object
    rule = {
        'name': rule_name,
        'content': dsl_content,
        'context_id': metadata.get('context'),
        'effective_date': metadata.get('effective'),
        'expiry_date': metadata.get('expires'),
        # Git provides: version, author, timestamps
        'source_format': 'rules'  # Track original format
    }

    return rule
```

---

### Phase 2: Conversion Script (JSON → .rules)

**Goal:** Migrate existing 33 JSON rules to .rules format

**Script:** `scripts/migrate_json_to_rules.py`

```python
#!/usr/bin/env python3
"""
Migrate JSON rules to .rules format
Preserves git history by using original commit timestamps
"""

import json
import yaml
from pathlib import Path
from datetime import datetime

def convert_json_to_rules(json_file: Path) -> tuple[str, dict]:
    """Convert JSON rule to .rules format"""

    with open(json_file, 'r') as f:
        data = json.load(f)

    # Build minimal frontmatter (only if fields present)
    frontmatter = {}
    if data.get('context_id'):
        frontmatter['context'] = data['context_id']
    if data.get('effective_date'):
        frontmatter['effective'] = data['effective_date']
    if data.get('expiry_date'):
        frontmatter['expires'] = data['expiry_date']

    # Build .rules file content
    lines = []

    # Add frontmatter if any fields present
    if frontmatter:
        lines.append('---')
        lines.append(yaml.dump(frontmatter, default_flow_style=False).strip())
        lines.append('---')

    # Add description as comment if present
    if data.get('description'):
        lines.append(f"# {data['description']}")

    # Add DSL content
    lines.append(data['content'])

    rules_content = '\n'.join(lines)

    # Determine new file path
    # JSON: backend/rules/DEMO/CC_STD/APPROVAL/rule-1.json
    # .rules: backend/rules/mon/DEMO/CC_STD/APPROVAL/creditScoreCheck.rules

    item_type = data.get('item_type', 'rule')
    type_folder = {
        'rule': 'mon',  # Default to monetary
        'mon_rule': 'mon',
        'non_mon_rule': 'non-mon',
        'actionset': 'actionsets'
    }.get(item_type, 'mon')

    rule_name = data.get('name', f"rule-{data['id']}")

    # Extract hierarchy from current path
    relative_path = json_file.relative_to(Path('backend/rules'))
    hierarchy_parts = relative_path.parent.parts  # ('DEMO', 'CC_STD', 'APPROVAL')

    new_path = Path('backend/rules') / type_folder / '/'.join(hierarchy_parts) / f"{rule_name}.rules"

    return rules_content, new_path, data


def migrate_all_rules():
    """Migrate all JSON rules to .rules format"""

    json_files = Path('backend/rules').glob('**/*.json')

    for json_file in json_files:
        if json_file.name == 'schema.json':
            continue  # Skip schema file

        print(f"Converting {json_file}...")

        rules_content, new_path, original_data = convert_json_to_rules(json_file)

        # Create directory if needed
        new_path.parent.mkdir(parents=True, exist_ok=True)

        # Write .rules file
        with open(new_path, 'w') as f:
            f.write(rules_content)

        print(f"  → {new_path}")

        # Optional: Git commit with original timestamp
        created_at = original_data.get('created_at')
        if created_at:
            commit_date = datetime.fromisoformat(created_at.replace('Z', '+00:00'))
            # Git commands to preserve timestamp:
            # git add {new_path}
            # git commit --date="{commit_date}" -m "Migrate {rule_name} to .rules format"


if __name__ == '__main__':
    migrate_all_rules()
    print("\nMigration complete!")
```

**Usage:**
```bash
cd /Users/chandramohn/workspace/rules_engine/ui-prototype
python scripts/migrate_json_to_rules.py
```

---

### Phase 3: Dual Format Support Period

**Timeline:** 2-4 weeks

**Behavior:**
- Backend supports both `.json` and `.rules` files
- Web UI continues to work with JSON files
- VS Code extension works with `.rules` files
- No forced migration

**Testing:**
- Verify all 33 rules load correctly in both formats
- Run full test suite against both formats
- Performance testing (no regression)

---

### Phase 4: Deprecation (Optional Long-term)

**After all users migrated to VS Code:**
- Mark JSON format as deprecated
- Update documentation to recommend .rules format
- Optional cleanup: Remove .json files (with backups)

---

## 🚀 Implementation Roadmap

### Week 1: Flask Backend Enhancement

**Goal:** Enable dual format support (.json + .rules) with ANTLR endpoints

**Tasks:**
- [ ] Add YAML dependency: `pip install pyyaml`
- [ ] Implement `.rules` file loader in `rule_file_service.py`
- [ ] Add frontmatter parser with validation (YAML parsing)
- [ ] Support both `.json` and `.rules` in all endpoints
- [ ] Add date validation (expires > effective)
- [ ] Update API to return source format metadata
- [ ] **Optimize ANTLR endpoints for LSP**: Fast validation endpoint
- [ ] Write unit tests for dual format support

**Deliverable:** Flask backend reads both .json and .rules files, optimized ANTLR validation endpoint

---

### Week 2: VS Code Extension MVP + LSP Foundation

**Goal:** Basic extension with TextMate grammar and LSP foundation

**Tasks:**
- [ ] Create extension scaffold: `yo code`
- [ ] Define `.rules` language in `package.json`
- [ ] **Implement TextMate grammar** for syntax highlighting (LSP local)
- [ ] **Load workspace config** (`rules.config.yaml`) on activation
- [ ] **Auto-discover entities** from `schemas/entities/` folder
- [ ] **Cache entity schemas** in memory for fast autocomplete
- [ ] Create Flask API client (HTTP wrapper for ANTLR calls)
- [ ] Implement basic commands (validate → ANTLR, generate → ANTLR)
- [ ] Add status bar integration
- [ ] Package and test extension locally

**Deliverable:** Extension with syntax highlighting + schema loading + ANTLR validation

---

### Week 3: LSP Features (Local, Schema-Driven)

**Goal:** Fast, offline-capable editing features using LSP

**Tasks:**
- [ ] **Implement completion provider** (LSP - schema-driven autocomplete)
  - Entity field autocomplete (`applicant.` → fields)
  - Action autocomplete (`then ` → actions)
- [ ] **Implement hover provider** (LSP - schema-driven documentation)
  - Show field descriptions on hover
  - Show field types and requirements
- [ ] **Implement basic diagnostics** (LSP - schema validation)
  - Unknown entity detection
  - Unknown field detection
  - Basic syntax checks (regex-based)
- [ ] **Implement frontmatter parser** (LSP - YAML parsing)
  - Parse context, effective, expires fields
  - Validate frontmatter structure
- [ ] **Graceful degradation** when Flask offline
  - Show warning if ANTLR unavailable
  - Continue providing LSP features

**Deliverable:** Instant autocomplete, hover, basic validation (all offline-capable)

---

### Week 4: ANTLR Integration + Advanced Features

**Goal:** Production-grade validation + code generation via ANTLR

**Tasks:**
- [ ] **Implement ANTLR validation integration**
  - On-save trigger: Send to Flask `/validate` endpoint
  - Manual trigger: `Cmd+Shift+V` command
  - Merge ANTLR errors with LSP warnings in diagnostics
- [ ] **Implement code generation** (ANTLR via Flask)
  - `Cmd+Shift+G`: Generate Java code
  - Open generated file in split pane
- [ ] **Implement CodeLens providers**
  - Context preview (load context JSON, show inline)
  - Date status (show effective/expiry with countdown)
- [ ] **Add tree view** for rule hierarchy
  - Show folder structure
  - Status icons (valid, invalid, expired)
- [ ] **Implement context/date commands**
  - Attach context (dropdown selection)
  - Set effective/expiry dates (date picker)
- [ ] **Add code snippets** for common patterns
- [ ] Create extension README and documentation

**Deliverable:** Production-ready extension with LSP + ANTLR integration

---

### Week 5: Migration & Testing

**Tasks:**
- [ ] Build migration script (JSON → .rules)
- [ ] Test migration with all 33 rules
- [ ] Verify git history preservation
- [ ] Performance testing (both formats)
- [ ] User acceptance testing
- [ ] Create migration guide documentation

**Deliverable:** Migration complete, dual format support verified

---

## 📚 Technical Reference

### Frontmatter Parsing Library

**Python (Backend):**
```python
import yaml

def parse_frontmatter(content: str) -> tuple[dict, str]:
    """Parse YAML frontmatter from .rules file"""

    if not content.startswith('---'):
        return {}, content

    parts = content.split('---', 2)
    if len(parts) < 3:
        return {}, content

    frontmatter_yaml = parts[1]
    dsl_content = parts[2].strip()

    metadata = yaml.safe_load(frontmatter_yaml) or {}

    # Validate allowed fields
    allowed_fields = {'context', 'effective', 'expires'}
    unknown_fields = set(metadata.keys()) - allowed_fields
    if unknown_fields:
        raise ValueError(f"Unknown frontmatter fields: {unknown_fields}")

    return metadata, dsl_content
```

**TypeScript (VS Code Extension):**
```typescript
import * as yaml from 'js-yaml';

interface RuleFrontmatter {
  context?: string;
  effective?: string;  // YYYY-MM-DD
  expires?: string;    // YYYY-MM-DD
}

function parseFrontmatter(content: string): [RuleFrontmatter, string] {
  if (!content.startsWith('---')) {
    return [{}, content];
  }

  const parts = content.split('---', 3);
  if (parts.length < 3) {
    return [{}, content];
  }

  const frontmatterYaml = parts[1];
  const dslContent = parts[2].trim();

  const metadata = yaml.load(frontmatterYaml) as RuleFrontmatter || {};

  // Validate allowed fields
  const allowedFields = new Set(['context', 'effective', 'expires']);
  const unknownFields = Object.keys(metadata).filter(k => !allowedFields.has(k));
  if (unknownFields.length > 0) {
    console.warn(`Unknown frontmatter fields: ${unknownFields.join(', ')}`);
  }

  return [metadata, dslContent];
}
```

---

### Date Validation

**Business Rules:**
1. Dates must be ISO 8601 format: `YYYY-MM-DD`
2. If both dates present: `expires` must be after `effective`
3. Dates in the past are valid (for historical rules)
4. Future dates are valid (for scheduled rules)

**Python Validation:**
```python
from datetime import datetime

def validate_dates(effective: str | None, expires: str | None) -> list[str]:
    """Validate effective and expiry dates, return error messages"""

    errors = []

    # Parse dates
    effective_date = None
    expires_date = None

    if effective:
        try:
            effective_date = datetime.strptime(effective, '%Y-%m-%d').date()
        except ValueError:
            errors.append(f"Invalid effective date format: {effective} (expected YYYY-MM-DD)")

    if expires:
        try:
            expires_date = datetime.strptime(expires, '%Y-%m-%d').date()
        except ValueError:
            errors.append(f"Invalid expiry date format: {expires} (expected YYYY-MM-DD)")

    # Validate date order
    if effective_date and expires_date and expires_date <= effective_date:
        errors.append(f"Expiry date ({expires}) must be after effective date ({effective})")

    return errors
```

---

### VS Code Extension Entry Point

**extension.ts:**
```typescript
import * as vscode from 'vscode';
import { FlaskApiClient } from './flaskClient';
import { RulesCompletionProvider } from './completion';
import { RulesHoverProvider } from './hover';
import { RulesDiagnostics } from './diagnostics';
import { RulesCodeLensProvider } from './codeLens';

export function activate(context: vscode.ExtensionContext) {
  console.log('Rules Engine extension activated');

  // Initialize Flask API client
  const flaskClient = new FlaskApiClient('http://localhost:5001');

  // Load workspace config
  const workspaceConfig = loadWorkspaceConfig();

  // Register language features
  const rulesSelector = { language: 'rules', scheme: 'file' };

  // Completion (IntelliSense)
  context.subscriptions.push(
    vscode.languages.registerCompletionItemProvider(
      rulesSelector,
      new RulesCompletionProvider(workspaceConfig),
      '.'  // Trigger on dot (applicant.)
    )
  );

  // Hover documentation
  context.subscriptions.push(
    vscode.languages.registerHoverProvider(
      rulesSelector,
      new RulesHoverProvider(workspaceConfig)
    )
  );

  // CodeLens (context preview)
  context.subscriptions.push(
    vscode.languages.registerCodeLensProvider(
      rulesSelector,
      new RulesCodeLensProvider(workspaceConfig)
    )
  );

  // Diagnostics (validation)
  const diagnostics = new RulesDiagnostics(flaskClient);
  context.subscriptions.push(diagnostics);

  // Commands
  context.subscriptions.push(
    vscode.commands.registerCommand('rules.validate', () => validateCurrentRule(flaskClient))
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('rules.generate', () => generateJavaCode(flaskClient))
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('rules.attachContext', () => attachContext(workspaceConfig))
  );
}

export function deactivate() {
  console.log('Rules Engine extension deactivated');
}
```

---

## 🔍 Testing Strategy

### Unit Tests

**Backend Tests:**
```python
# tests/test_rules_file_loader.py

def test_load_pure_dsl():
    """Test loading .rules file with no frontmatter"""
    content = """
# Simple rule
rule test:
    if applicant.age >= 18 then approveApplication
"""
    rule = parse_rules_content(content)
    assert rule['context_id'] is None
    assert rule['effective_date'] is None
    assert rule['content'].strip().startswith('rule test:')


def test_load_with_frontmatter():
    """Test loading .rules file with frontmatter"""
    content = """---
context: test_context
effective: 2025-01-01
expires: 2025-12-31
---
rule test:
    if applicant.age >= 18 then approveApplication
"""
    rule = parse_rules_content(content)
    assert rule['context_id'] == 'test_context'
    assert rule['effective_date'] == '2025-01-01'
    assert rule['expiry_date'] == '2025-12-31'


def test_date_validation_error():
    """Test validation error when expires <= effective"""
    content = """---
effective: 2025-12-31
expires: 2025-01-01
---
rule test:
    if applicant.age >= 18 then approveApplication
"""
    with pytest.raises(ValueError, match="Expiry date.*must be after effective date"):
        parse_rules_content(content)
```

**Extension Tests:**
```typescript
// src/test/frontmatter.test.ts

import { parseFrontmatter } from '../frontmatter';

test('parse pure DSL (no frontmatter)', () => {
  const content = 'rule test:\n    if applicant.age >= 18 then approveApplication';
  const [metadata, dsl] = parseFrontmatter(content);

  expect(metadata).toEqual({});
  expect(dsl).toContain('rule test:');
});

test('parse with frontmatter', () => {
  const content = `---
context: test_context
effective: 2025-01-01
---
rule test:
    if applicant.age >= 18 then approveApplication`;

  const [metadata, dsl] = parseFrontmatter(content);

  expect(metadata.context).toBe('test_context');
  expect(metadata.effective).toBe('2025-01-01');
  expect(dsl).toContain('rule test:');
});
```

---

### Integration Tests

**Dual Format Support:**
```python
def test_load_json_and_rules_equivalence():
    """Verify .json and .rules files produce equivalent rule objects"""

    # Load same rule from both formats
    json_rule = load_rule('backend/rules/DEMO/CC_STD/APPROVAL/rule-1.json')
    rules_rule = load_rule('backend/rules/mon/DEMO/CC_STD/APPROVAL/creditScoreCheck.rules')

    # Compare essential fields
    assert json_rule['name'] == rules_rule['name']
    assert json_rule['content'] == rules_rule['content']
    assert json_rule['context_id'] == rules_rule['context_id']
```

---

## 📖 User Documentation

### Creating a New Rule

**Step 1: Create .rules file**

Right-click in VS Code Explorer → New File → `myRule.rules`

**Step 2: Add frontmatter (optional)**

```yaml
---
context: standard_approval_context
---
```

**Step 3: Write rule logic**

```
# My new rule description

rule myRule:
    if applicant.creditScore >= 700 then approveApplication
```

**Step 4: Validate**

- Red squiggles show errors automatically
- Or: `Cmd+Shift+V` to validate manually

---

### Attaching a Test Context

**Option 1: Manual frontmatter edit**

```yaml
---
context: premium_approval_context
---
```

**Option 2: Command Palette**

1. Open Command Palette: `Cmd+Shift+P`
2. Type: "Rules: Attach Context"
3. Select context from dropdown
4. Frontmatter auto-inserted

---

### Setting Effective/Expiry Dates

**Manual frontmatter edit:**

```yaml
---
effective: 2025-12-01
expires: 2026-01-31
---
```

**Validation:**
- Invalid date format → Red squiggle
- Expiry before effective → Error message
- VS Code shows date countdown in CodeLens

---

## ⚠️ Important Implementation Notes

### 1. Frontmatter is Optional

**Critical:** A valid `.rules` file can have ZERO frontmatter.

```
# This is valid
rule myRule:
    if applicant.age >= 18 then approveApplication
```

### 2. Only 3 Fields Permitted

**Validation:** Extension MUST warn on unknown frontmatter fields.

```yaml
---
context: test_context
effective: 2025-01-01
unknown_field: invalid  # ← WARNING
---
```

### 3. Date Format is ISO 8601

**Only accepted format:** `YYYY-MM-DD`

❌ Invalid: `12/01/2025`, `2025-12-1`, `Dec 1 2025`
✅ Valid: `2025-12-01`

### 4. Git as Source of Truth

**Do NOT store in frontmatter:**
- Version numbers (use git history)
- Author names (use git commit author)
- Created/updated timestamps (use git commit dates)
- Status flags (use VS Code diagnostics)

---

## 🎯 Success Criteria

### Functional Requirements

- ✅ VS Code extension reads `.rules` files with syntax highlighting
- ✅ IntelliSense provides entity/attribute autocomplete
- ✅ Real-time validation via Flask backend
- ✅ CodeLens shows context preview and date status
- ✅ Commands for validation, generation, context attachment
- ✅ Tree view shows rule hierarchy with status icons
- ✅ Frontmatter parser supports optional context and dates
- ✅ Date validation enforces business rules

### Non-Functional Requirements

- ✅ Extension activates in < 1 second
- ✅ Validation response < 500ms
- ✅ IntelliSense suggestions appear < 100ms
- ✅ Zero regression in Flask backend performance
- ✅ All 33 existing rules migrate without data loss

---

## 🎯 Architecture Decision Summary

### Why Hybrid LSP + ANTLR is Superior

**The Problem We Solved:**
- **Option 1 (ANTLR only)**: Accurate but slow editing (500ms network calls for autocomplete)
- **Option 2 (LSP only)**: Fast editing but limited validation (no grammar checking)
- **Option 3 (Hybrid)**: ✅ **Fast editing + production validation**

**Key Benefits:**

1. **⚡ Performance**
   - Autocomplete: < 100ms (LSP, local, no network)
   - Hover docs: < 50ms (LSP, local, no network)
   - Full validation: < 500ms (ANTLR, on-save only)
   - Code generation: < 1000ms (ANTLR, on-demand)

2. **🔌 Offline Capability**
   - LSP features work without Flask running
   - Users can edit rules on planes, trains, coffee shops
   - Basic schema validation always available
   - Full ANTLR validation when back online

3. **🎨 Developer Experience**
   - Instant autocomplete as you type
   - Instant hover documentation
   - Immediate schema feedback
   - Production-grade validation on save

4. **🏗️ Maintainability**
   - Single source of truth: ANTLR grammar
   - LSP uses simple schema validation (no duplicate grammar)
   - Changes to grammar → only update ANTLR
   - LSP remains stable (schema-driven)

5. **🚀 Production Ready**
   - ANTLR handles all production tasks
   - Code generation proven and stable
   - No compromise on validation quality
   - LSP enhances UX without replacing ANTLR

**Architecture Diagram:**
```
User Types            LSP                 ANTLR
─────────────────────────────────────────────────
applicant.     →    Instant           (not called)
                    autocomplete
                    < 100ms

Save file      →    (LSP validates  → Full grammar
                     schema first)     validation
                                      < 500ms

Generate code  →    (not involved)  → Java codegen
                                      < 1000ms
```

---

## 📞 Next Steps

**Ready to implement:**

1. **Week 1**: Enhance Flask backend with .rules support + optimize ANTLR endpoints
2. **Week 2**: Build VS Code extension MVP + LSP foundation (schema loading)
3. **Week 3**: Implement LSP features (autocomplete, hover, basic validation)
4. **Week 4**: Integrate ANTLR (full validation, codegen) + advanced features
5. **Week 5**: Migration and testing

**Questions before starting?**
- Frontend stack confirmation (TypeScript + VS Code Extension API + LSP)
- Backend changes approval (dual format support, ANTLR optimization)
- Migration timeline preferences

---

**End of Specification**
**Version**: 2.0
**Status**: Ready for Implementation
**Architecture**: Hybrid LSP + ANTLR (Optimal Performance)
**Last Updated**: November 22, 2025
