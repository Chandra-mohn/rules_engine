# COBOL-to-Rules DSL Migration Workbench

Interactive VS Code tool for converting legacy COBOL code to modern Rules DSL with AST-based transpilation.

## Project Overview

**Purpose**: Accelerate legacy COBOL modernization by providing incremental, interactive conversion to Rules DSL
**Architecture**: Dual-panel VS Code extension with Python transpiler backend
**Approach**: AST-based transpilation using ANTLR4 COBOL grammar

## Key Features

✅ **Dual-Panel Interface**: Side-by-side COBOL and DSL editors
✅ **Incremental Conversion**: Select chunks, convert progressively
✅ **Attribute Mapping**: Database-driven COBOL→target system mapping
✅ **Pattern Detection**: Automatic ActionSet consolidation suggestions
✅ **Error Recovery**: Graceful handling of unparseable code
✅ **Git-Friendly**: CSV-based mapping storage for version control

## Technical Stack

### Backend (Python 3.9+)
- **Parser**: ANTLR4 with COBOL grammar
- **Database**: DuckDB (in-memory, reading CSV files)
- **Transpiler**: AST transformation pipeline

### Frontend (TypeScript)
- **Framework**: VS Code Extension API
- **UI**: Webview-based dual-panel editor

## Project Structure

```
cobol2dsl/
├── backend/              # Python transpiler engine
│   ├── cobol_parser/     # ANTLR4 COBOL parser and AST builder
│   ├── transpiler/       # AST transformation and DSL generation
│   ├── services/         # Mapping, validation, pattern detection
│   └── mappings/         # User-provided CSV mapping files
├── extension/            # VS Code extension (TypeScript)
│   └── src/              # Extension source code
├── tests/                # Test suite
├── docs/                 # Architecture and decision documents
│   ├── COBOL_MIGRATION_ARCHITECTURE.md
│   └── COBOL_MIGRATION_DECISIONS.md
└── README.md
```

## Quick Start

### Prerequisites
- Python 3.9+
- Node.js 16+
- VS Code

### Backend Setup
```bash
cd backend
pip install -r requirements.txt
```

### Extension Setup
```bash
cd extension
npm install
npm run compile
```

### CSV Mapping File
Create a mapping CSV file with your COBOL→target attribute mappings:

```csv
cobol_name,target_name,mapping_type,data_type,confidence
CUSTOMER-TYPE,customer.type,direct,string,1.0
BALANCE,customer.balance,direct,decimal,1.0
CREDIT-SCORE,customer.creditScore,direct,integer,1.0
APPROVE-PREMIUM,approvePremium,action,function,0.7
```

## Usage Workflow

1. **Open Migration Workbench**: Command Palette → "COBOL to Rules: Open Migration Workbench"
2. **Load COBOL**: Paste or open COBOL file in left panel
3. **Select & Convert**: Select COBOL chunk → Right-click → "Convert to Rules DSL"
4. **Review Output**: Generated DSL appears in right panel with mapping metadata
5. **Repeat**: Continue converting chunks incrementally
6. **Export**: Save accumulated DSL to .rules file

## Technical Decisions

All technical decisions documented in `docs/COBOL_MIGRATION_DECISIONS.md`:

- ✅ **Database**: DuckDB with CSV files (git-friendly)
- ✅ **88-Level Conditions**: Boolean expressions only (no enum syntax)
- ✅ **Pattern Threshold**: > 1 occurrence triggers ActionSet suggestion
- ✅ **Error Recovery**: Unparseable code pasted as comments
- ✅ **Temp Variables**: camelCase conversion, inline single-use
- ⏸️ **NLP Enhancement**: Deferred (no LLM access currently)

## Development Roadmap

### Phase 1: Core Transpiler (Weeks 1-4) 🚧 Current
- [ ] ANTLR4 COBOL grammar setup
- [ ] DuckDB CSV mapping service
- [ ] Basic AST parser and transformer
- [ ] DSL code generator
- [ ] Error recovery implementation

### Phase 2: VS Code Extension (Weeks 5-7)
- [ ] Dual-panel webview interface
- [ ] Context menu integration
- [ ] Incremental conversion workflow
- [ ] Metadata visualization

### Phase 3: Advanced Features (Weeks 8-10)
- [ ] Pattern detection algorithm
- [ ] ActionSet consolidation
- [ ] 88-level condition handling
- [ ] On-demand schema generation

## Example Conversion

**Input COBOL:**
```cobol
IF CUSTOMER-TYPE = 'PREMIUM' AND BALANCE > 10000
   PERFORM APPROVE-PREMIUM
ELSE
   PERFORM REJECT-APPLICATION
END-IF
```

**Generated DSL:**
```javascript
# MAPPED: CUSTOMER-TYPE → customer.type (direct, confidence: 1.0)
# MAPPED: BALANCE → customer.balance (direct, confidence: 1.0)
# DERIVED: APPROVE-PREMIUM → approvePremium (action, confidence: 0.7)

rule "Customer Approval Logic":
    if customer.type == "PREMIUM" and customer.balance > 10000 then
        approvePremium()
    else
        rejectApplication()
    endif
```

## Documentation

- **Architecture**: `docs/COBOL_MIGRATION_ARCHITECTURE.md` - System design and component architecture
- **Decisions**: `docs/COBOL_MIGRATION_DECISIONS.md` - Finalized technical decisions
- **Roadmap**: See implementation phases above

## Status

**Current Phase**: Phase 1 - Core Transpiler (Setup)
**Last Updated**: 2025-11-23
**Project Root**: `/Users/chandramohn/workspace/rules_engine/cobol2dsl`

---

**Related Projects**:
- [Rules DSL](../rules-dsl) - Target DSL language and validation
- [UI Prototype](../ui-prototype) - Original Rules DSL UI (reference architecture)
