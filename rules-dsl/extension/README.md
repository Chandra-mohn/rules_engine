# Rules DSL - VS Code Extension

**Version**: 0.1.0
**Status**: Week 2 Complete - Ready for Testing
**Architecture**: Hybrid LSP (local) + ANTLR (remote)

---

## 🎯 Features

### LSP Features (Local, Offline-Capable)
- ✅ **Syntax Highlighting** - TextMate grammar for `.rules` files
- ✅ **IntelliSense** - Entity field autocomplete (`applicant.` → fields)
- ✅ **Hover Documentation** - Field descriptions and types
- ✅ **Keyword Autocomplete** - `rule`, `if`, `then`, `else`, etc.
- ✅ **Action Autocomplete** - `approveApplication`, `rejectApplication`, etc.
- ✅ **Live Diagnostics** - Real-time validation for structural errors
  - Missing `endif` for `if` statements
  - Unmatched `endif` without corresponding `if`
  - Missing `rule` or `actionset` definition

### ANTLR Features (Remote, Production-Grade)
- ✅ **Full Validation** - Grammar validation via Flask backend
- ✅ **Code Generation** - Generate Java code from rules
- ✅ **Context Attachment** - Attach test contexts to rules

---

## 🚀 Quick Start

### 1. Install Dependencies

```bash
cd /Users/chandramohn/workspace/rules_engine/rules-dsl/extension
npm install
```

### 2. Compile TypeScript

```bash
npm run compile
```

### 3. Launch Extension

1. Open `/Users/chandramohn/workspace/rules_engine/rules-dsl/` in VS Code
2. Press `F5` to launch Extension Development Host
3. Open a `.rules` file from `rules/mon/`, `rules/non-mon/`, or `rules/actionsets/`

---

## 📝 Usage

### Open a Rule File

Navigate to any `.rules` file, for example:
```
rules/mon/DEMO/CC_STD/APPROVAL/creditScoreCheck.rules
```

### Features in Action

**Autocomplete**:
- Type `applicant.` → See all applicant fields
- Type `then ` → See available actions

**Hover**:
- Hover over `applicant` → See entity info
- Hover over `creditScore` → See field type and description
- Hover over keywords → See documentation

**Commands** (Cmd+Shift+P):
- `Rules: Validate Current Rule` - Validate with ANTLR (Cmd+Shift+V)
- `Rules: Generate Java Code` - Generate Java (Cmd+Shift+G)
- `Rules: Attach Test Context` - Add context to frontmatter (Cmd+Shift+C)

---

## 🔧 Configuration

### Extension Settings

```json
{
  "rules.backend.url": "http://localhost:5002",
  "rules.workspace.configPath": "./rules.config.yaml"
}
```

### Workspace Config

Edit `rules.config.yaml` in project root:

```yaml
rulesEngine:
  schemaVersion: v2
  paths:
    schemas: ./rules/schemas/
    contexts: ./rules/contexts/
    rules: ./rules/
  backend:
    url: http://localhost:5002
    validateEndpoint: /api/validate
    generateEndpoint: /api/generate
```

---

## 🏗️ Architecture

### Hybrid LSP + ANTLR

```
┌─────────────────────────────────────┐
│  VS Code Extension (TypeScript)      │
│                                      │
│  LOCAL FEATURES (Fast, Offline):    │
│  • Syntax highlighting               │
│  • Entity autocomplete               │
│  • Hover documentation               │
│  • Basic schema validation           │
│                                      │
│  Performance: < 100ms                │
│  Works Offline: ✅ Yes               │
└──────────────┬──────────────────────┘
               │
               │ HTTP/REST (localhost:5002)
               │ Only for: Validation & Code Generation
               ↓
┌─────────────────────────────────────┐
│      Flask Backend (Python)          │
│                                      │
│  REMOTE FEATURES (Production):      │
│  • Full ANTLR grammar validation     │
│  • Java code generation              │
│  • Rule testing & execution          │
│                                      │
│  Triggered: On save, manual command  │
└─────────────────────────────────────┘
```

---

## 📁 Extension Structure

```
extension/
├── src/
│   ├── extension.ts              # Entry point
│   ├── flask-client.ts           # HTTP client for ANTLR
│   ├── workspace-config.ts       # Schema and context loader
│   └── providers/
│       ├── completion.ts         # IntelliSense provider
│       └── hover.ts              # Hover documentation provider
├── syntaxes/
│   └── rules.tmLanguage.json    # TextMate grammar
├── out/                          # Compiled JavaScript
├── package.json                  # Extension manifest
└── tsconfig.json                # TypeScript config
```

---

## 🧪 Testing

### Manual Testing

1. **Launch Extension**: Press `F5` in VS Code
2. **Open Workspace**: `/Users/chandramohn/workspace/rules_engine/rules-dsl/`
3. **Open Rule File**: `rules/mon/DEMO/CC_STD/APPROVAL/creditScoreCheck.rules`
4. **Test Autocomplete**:
   - Type `applicant.` → Should see `creditScore`, `income`, `age`, etc.
5. **Test Hover**:
   - Hover over `applicant` → Should see entity documentation
6. **Test Validation**:
   - Cmd+Shift+P → "Rules: Validate Current Rule"
   - Should validate via Flask (requires backend running)

### Backend Setup (for Validation)

```bash
cd /Users/chandramohn/workspace/rules_engine/rules-dsl/backend
source venv/bin/activate.fish  # Fish shell
python app.py  # Runs on port 5002
```

---

## 🎨 Syntax Highlighting

The extension highlights:
- **Keywords**: `rule`, `actionset`, `if`, `then`, `else`, `and`, `or`, `not`
- **Entities**: `applicant`, `transaction`, `card`, `account`
- **Operators**: `>=`, `<=`, `>`, `<`, `==`, `!=`
- **Actions**: `approveApplication`, `rejectApplication`, etc.
- **Numbers**: `700`, `18.5`
- **Strings**: `"Hello World"`
- **Comments**: `# This is a comment`
- **Frontmatter**: YAML metadata (context, effective, expires)

---

## 🚀 Next Steps (Week 3)

### Planned Features
- [ ] Enhanced diagnostics (LSP-based schema validation)
- [ ] Code snippets for common patterns
- [ ] Frontmatter validation
- [ ] CodeLens for context preview
- [ ] Tree view for rule hierarchy

---

## 📚 Resources

- **Implementation Spec**: `../docs/VSCODE_EXTENSION_IMPLEMENTATION_SPEC.md`
- **Week 1 Summary**: `../docs/WEEK1_SUMMARY.md`
- **Project README**: `../README.md`

---

**Status**: ✅ Week 2 Complete
**Ready For**: Manual testing and Week 3 development
