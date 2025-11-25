# COBOL-to-DSL Migration: Technical Decisions

**Date**: 2025-11-23
**Status**: Decisions Finalized - Ready for Implementation

---

## ✅ Finalized Decisions

### 1. Database Technology
**Decision**: DuckDB with CSV file storage (not SQLite)
**Rationale**:
- **Git-friendly**: CSV files are human-readable text (no binary commits)
- **Embedded database**: No separate server needed, like SQLite
- **Excellent CSV integration**: DuckDB reads CSVs directly as tables
- **Analytical capabilities**: Better for complex mapping queries
- **SQLite was retired**: Previous Rules DSL project deprecated SQLite
- **Version control**: Mapping changes tracked in git diffs

**Implementation**:
```python
# backend/services/cobol_mapping_service.py
import duckdb

class MappingService:
    def __init__(self, csv_path: str = "mappings/attribute_mappings.csv"):
        self.conn = duckdb.connect(':memory:')  # In-memory DuckDB
        self.csv_path = csv_path
        self.cache = {}

        # Load CSV as view - DuckDB reads it directly
        self.conn.execute(f"""
            CREATE VIEW v_attribute_mappings AS
            SELECT * FROM read_csv_auto('{csv_path}')
        """)

    def get_mapping(self, cobol_name: str):
        """Query CSV-backed view for mappings"""
        query = """
            SELECT cobol_name, target_name, mapping_type, data_type
            FROM v_attribute_mappings
            WHERE cobol_name = ?
        """
        result = self.conn.execute(query, [cobol_name]).fetchone()
        return result if result else None
```

**CSV File Format** (`mappings/attribute_mappings.csv`):
```csv
cobol_name,target_name,mapping_type,data_type,confidence
CUSTOMER-TYPE,customer.type,direct,string,1.0
BALANCE,customer.balance,direct,decimal,1.0
CUST-BAL,customer.balance,fuzzy,decimal,0.85
APPROVE-PREMIUM,approvePremium,action,function,0.7
```

### 2. Attribute Mapping Storage
**Decision**: User provides CSV file with mapping data (read-only)
**Rationale**:
- User already has or can create mapping CSV
- No database table creation needed
- DuckDB reads CSV directly as view
- Simple text file editing for mapping updates
- Git-friendly version control for mapping changes
- Session-only in-memory cache for performance

**User-Provided CSV Format**:
```csv
cobol_name,target_name,mapping_type,data_type,confidence
CUSTOMER-TYPE,customer.type,direct,string,1.0
BALANCE,customer.balance,direct,decimal,1.0
CREDIT-SCORE,customer.creditScore,direct,integer,1.0
APPROVE-PREMIUM,approvePremium,action,function,0.7
```

**Usage**:
```python
# User points to their mapping CSV file
mapper = MappingService(csv_path="path/to/user/mappings.csv")
mapping = mapper.get_mapping("CUSTOMER-TYPE")
# Returns: ("CUSTOMER-TYPE", "customer.type", "direct", "string", 1.0)
```

### 3. Semantic Intent Extraction Strategy

**Phase 1: Basic Literal Conversion** ✅ **START HERE**
- Direct AST transformation
- Pattern-based mapping (IF → if, PERFORM → action)
- Attribute lookup from database view
- Simple comment annotations

**Phase 2: Pattern Matching + Manual Annotation** (Future)
- Detect common COBOL patterns (validation, calculations, etc.)
- Allow user to annotate business intent
- Build pattern library over time

**Phase 3: Advanced NLP-Based Intent Extraction** ⏸️ **DEFERRED**
**Status**: Not currently feasible - no LLM API access available
**Future Consideration**: May be implemented when LLM access becomes available

**Proposal** (for future reference):
```
COBOL Code → LLM (GPT-4/Claude API) → Business Intent → Validation → DSL
```

**Use Cases**:
- Extract business rules from complex nested logic
- Generate meaningful rule names from context
- Identify domain concepts (e.g., "credit approval", "fraud detection")
- Suggest ActionSet consolidations based on semantic similarity

**Why Deferred**:
- ❌ No current LLM API access
- ❌ Would require internet connectivity
- ❌ API costs not currently budgeted
- ✅ Phase 1 & 2 provide sufficient value without NLP
- ✅ Can be added later as optional enhancement

### 4. Temporary Variables Handling

**Decision**:
1. Convert COBOL names to JSON-case (camelCase)
2. Inline single-use variables

**Rules**:
```python
def handle_temp_variable(cobol_name: str, usage_count: int) -> str:
    """
    WORK-TOTAL-AMT → workTotalAmt (multi-use)
    TEMP-CALC → inlined if used once
    """
    if usage_count == 1:
        return None  # Inline the value

    # Convert to camelCase
    parts = cobol_name.lower().replace('_', '-').split('-')
    return parts[0] + ''.join(p.capitalize() for p in parts[1:])
```

**Examples**:
```cobol
WORKING-STORAGE SECTION.
01 WORK-TOTAL-AMT PIC 9(10)V99.
01 TEMP-CALC PIC 9(5).

PROCEDURE DIVISION.
    COMPUTE WORK-TOTAL-AMT = BALANCE + INTEREST.
    COMPUTE TEMP-CALC = WORK-TOTAL-AMT * 0.1.
    IF TEMP-CALC > 100
       PERFORM APPROVE-LOAN.
```

**Generated DSL**:
```javascript
rule "Loan Approval Check":
    // Multi-use temp variable preserved
    let workTotalAmt = customer.balance + customer.interest

    // Single-use TEMP-CALC inlined
    if (workTotalAmt * 0.1) > 100 then
        approveLoan()
    endif
```

### 5. 88-Level Conditions

**Decision**: Convert to boolean expressions ONLY (Rules DSL doesn't support enum/constants syntax)

**Rationale**:
- Rules DSL does not support Option A (enum/constants) syntax
- Must use inline boolean comparisons only
- Add comments to preserve original 88-level condition names
- Maintain semantic meaning through naming in comments

**Strategy**:
```cobol
01 STATUS-CODE PIC 9.
   88 STATUS-APPROVED VALUE 1.
   88 STATUS-REJECTED VALUE 2.
   88 STATUS-PENDING VALUE 3.
```

**Conversion (Boolean Expressions Only)**:
```cobol
IF STATUS-APPROVED
   PERFORM SEND-APPROVAL-EMAIL
END-IF
```

**Generates**:
```javascript
rule "Approval Email":
    # 88-LEVEL: STATUS-APPROVED (value: 1)
    if application.statusCode == 1 then
        sendApprovalEmail()
    endif
```

**Implementation**:
```python
def convert_88_level(condition_name: str, value: int, parent_field: str) -> str:
    """
    Converts 88-level condition to boolean expression
    Always generates inline comparison with comment
    """
    # Map parent COBOL field to target attribute
    mapped_field = mapper.get_mapping(parent_field).target_name

    # Generate comment preserving 88-level semantics
    comment = f"# 88-LEVEL: {condition_name} (value: {value})"

    # Generate boolean expression
    expression = f"{mapped_field} == {value}"

    return comment, expression
```

**Multiple 88-Level Handling**:
```cobol
IF STATUS-APPROVED
   PERFORM PROCESS-APPROVED
ELSE
   IF STATUS-REJECTED
      PERFORM PROCESS-REJECTED
   ELSE
      PERFORM PROCESS-PENDING
   END-IF
END-IF
```

**Generates**:
```javascript
rule "Status Processing":
    # 88-LEVEL: STATUS-APPROVED (value: 1)
    if application.statusCode == 1 then
        processApproved()
    else
        # 88-LEVEL: STATUS-REJECTED (value: 2)
        if application.statusCode == 2 then
            processRejected()
        else
            # 88-LEVEL: STATUS-PENDING (value: 3)
            processPending()
        endif
    endif
```

### 6. Copybook Handling

**Decision**: On-demand schema generation (user-initiated only)

**Workflow**:
```
User selects COPYBOOK section → Right-click "Generate Schema from Copybook"
→ Parses COPYBOOK → Generates JSON schema → Saves to schemas/
```

**Out of Scope** (for now):
- Automatic copybook detection
- Copybook import/merge
- Cross-copybook dependency resolution

**Example**:
```cobol
01 CUSTOMER-RECORD.
   05 CUST-ID           PIC 9(8).
   05 CUST-NAME         PIC X(50).
   05 CREDIT-SCORE      PIC 999.
   05 BALANCE           PIC S9(10)V99.
```

**Generated Schema** (only when user explicitly requests):
```json
{
  "entity": "customer",
  "attributes": {
    "id": {"type": "number", "cobol_pic": "9(8)"},
    "name": {"type": "string", "cobol_pic": "X(50)", "maxLength": 50},
    "creditScore": {"type": "number", "cobol_pic": "999", "range": [0, 999]},
    "balance": {"type": "decimal", "cobol_pic": "S9(10)V99", "precision": 12, "scale": 2}
  }
}
```

### 7. Pattern Detection Threshold

**Decision**: Anything repeated MORE THAN ONCE (>1) is a consolidation candidate

**Rationale**:
- Low threshold ensures we catch all potential ActionSets
- Better to suggest consolidation and let user reject than miss opportunities
- Repeated code is technical debt - even 2 occurrences worth consolidating

**Implementation**:
```python
class PatternDetector:
    def __init__(self):
        self.patterns = {}
        self.threshold = 2  # Minimum occurrences (anything > 1)

    def detect_repeated_patterns(self, cobol_chunks: list) -> dict:
        """
        Analyzes COBOL chunks to find patterns occurring > 1 time
        Returns: Dictionary of patterns exceeding threshold
        """
        for chunk in cobol_chunks:
            ast = parse_cobol(chunk)
            normalized = self._normalize_ast(ast)
            signature = self._compute_signature(normalized)

            if signature not in self.patterns:
                self.patterns[signature] = {
                    'count': 0,
                    'instances': [],
                    'template': normalized
                }

            self.patterns[signature]['count'] += 1
            self.patterns[signature]['instances'].append(chunk)

        # Return patterns that occur MORE THAN ONCE
        return {
            sig: data for sig, data in self.patterns.items()
            if data['count'] > 1  # User feedback: "> 1" is threshold
        }
```

**Examples**:
- 2 occurrences → Suggest ActionSet ✅
- 3 occurrences → Suggest ActionSet ✅
- 1 occurrence → No suggestion ❌

### 8. Error Recovery Strategy

**Decision**: Paste unparseable COBOL as comments with failure annotation

**Rationale**:
- Preserves original COBOL logic for manual review
- Doesn't block overall conversion process
- User can manually convert problematic sections later
- Clear indication of what failed and why

**Implementation**:
```python
def convert_cobol_chunk(cobol_text: str) -> str:
    """
    Attempts to parse and convert COBOL to DSL
    Falls back to commenting on parse failure
    """
    try:
        # Parse COBOL
        ast = parse_cobol(cobol_text)

        # Transform AST
        dsl_ast = transform_ast(ast)

        # Generate DSL
        return generate_dsl(dsl_ast)

    except ANTLRException as e:
        # Parsing failed - preserve as comment
        commented_cobol = '\n'.join(f'# {line}' for line in cobol_text.split('\n'))

        return f"""
# ⚠️ PARSING FAILED - Manual conversion required
# Error: {str(e)}
# Original COBOL code preserved below:
{commented_cobol}
"""
```

**Example Failure Handling**:
```cobol
* Unparseable COBOL with syntax errors
IF INVALID SYNTAX HERE
   PERFORM SOMETHING
```

**Generates**:
```javascript
# ⚠️ PARSING FAILED - Manual conversion required
# Error: line 1:3 mismatched input 'INVALID' expecting {IF, EVALUATE, PERFORM}
# Original COBOL code preserved below:
# * Unparseable COBOL with syntax errors
# IF INVALID SYNTAX HERE
#    PERFORM SOMETHING
```

**User Workflow**:
1. Conversion generates DSL with commented failures
2. User reviews generated output
3. User manually converts commented sections
4. User validates final DSL with CLI

---

## 📋 Implementation Roadmap

### Phase 1: Core Transpiler (Weeks 1-4)

**Week 1: Setup & Foundation**
- [ ] Set up ANTLR4 COBOL grammar in project
- [ ] Create DuckDB CSV mapping service
- [ ] Implement basic COBOL AST parser
- [ ] Test parsing simple IF statements

**Week 2: Mapping & Transformation**
- [ ] Implement attribute mapper (query CSV via DuckDB)
- [ ] Build AST transformer (COBOL → DSL)
- [ ] Handle temporary variables (camelCase + inlining)
- [ ] Implement error recovery (comment unparseable code)
- [ ] Test with COBOL snippets

**Week 3: Code Generation**
- [ ] Implement DSL code generator
- [ ] Add mapping metadata comments
- [ ] Handle 88-level conditions (boolean expressions only)
- [ ] Implement pattern detection (threshold > 1)
- [ ] Generate validation-ready DSL output

**Week 4: Testing & Refinement**
- [ ] Test with real COBOL code samples
- [ ] Fix edge cases
- [ ] Performance optimization
- [ ] Documentation

### Phase 2: VS Code Extension (Weeks 5-7)

**Week 5: Dual-Panel Interface**
- [ ] Create webview-based dual-panel UI
- [ ] COBOL editor (left panel)
- [ ] DSL editor (right panel)
- [ ] Selection synchronization

**Week 6: Integration**
- [ ] Context menu "Convert to DSL"
- [ ] Connect to transpiler backend
- [ ] Accumulation logic (append to right panel)
- [ ] Metadata hover/tooltips

**Week 7: UX Polish**
- [ ] Progress indicators
- [ ] Error handling UI
- [ ] Mapping confidence visualization
- [ ] Export functionality

### Phase 3: Advanced Features (Weeks 8-10)

**Week 8: Pattern Detection**
- [ ] Implement pattern detector
- [ ] ActionSet consolidation algorithm
- [ ] User approval workflow

**Week 9: 88-Level & Schema Generation**
- [ ] 88-level enum generation
- [ ] Copybook schema generator (on-demand)
- [ ] Constants file generation

**Week 10: NLP Integration (Optional)**
- [ ] Research LLM API integration
- [ ] Prototype business intent extraction
- [ ] User review workflow
- [ ] Cost/benefit analysis

---

## 🎯 Success Criteria

### Phase 1 MVP (Core Transpiler)
- ✅ Convert IF/ELSE statements accurately
- ✅ Handle PERFORM statements → actions
- ✅ Map attributes via database view
- ✅ Generate valid DSL code
- ✅ Include mapping metadata comments

### Phase 2 MVP (VS Code Extension)
- ✅ Dual-panel interface working
- ✅ Incremental conversion workflow
- ✅ Accumulated DSL output
- ✅ Basic error handling

### Phase 3 (Advanced)
- ✅ Pattern detection functional
- ✅ ActionSet consolidation working
- ✅ 88-level handling
- ✅ On-demand schema generation

---

## 🔧 Technical Stack (Finalized)

### Backend
- **Language**: Python 3.9+
- **Parser**: ANTLR4 with COBOL grammar
- **Database**: DuckDB (in-memory, reading CSV files)
- **Libraries**:
  - `antlr4-python3-runtime` - Parser runtime
  - `duckdb` - Embedded analytical database with CSV support
  - (Future) `openai` or `anthropic` - NLP intent extraction (deferred)

### Frontend
- **Framework**: VS Code Extension API (TypeScript)
- **UI**: Webview with dual editors
- **Communication**: JSON-RPC between extension and backend

### Integration
- **CLI**: Extend existing `generate_code_cli.py`
- **Validation**: Use existing Rules DSL validation
- **Schemas**: Integrate with existing schema system

---

## 📝 Next Immediate Steps

1. **Set up ANTLR4 COBOL grammar** in backend
2. **Create DuckDB CSV mapping service** with sample CSV
3. **Build minimal IF statement converter** (proof of concept)
4. **Test with sample COBOL code**
5. **Design dual-panel VS Code webview**

---

## ✅ All Decisions Finalized

All technical decisions have been made based on user feedback:
- ✅ **Database**: DuckDB with CSV files (git-friendly, no binary commits)
- ✅ **Mapping**: User-provided CSV file (read-only access)
- ✅ **88-Level**: Boolean expressions only (no enum syntax)
- ✅ **Pattern Threshold**: > 1 occurrence triggers suggestion
- ✅ **Error Recovery**: Paste unparseable code as comments with failure note
- ✅ **NLP**: Deferred (no LLM access currently)
- ✅ **Temp Variables**: camelCase conversion, inline single-use
- ✅ **Project Root**: `/Users/chandramohn/workspace/rules_engine/cobol2dsl`

---

**Status**: ✅ 100% Decisions Finalized - Ready for Implementation
**Ready for**: Phase 1 Core Transpiler Development
**Next Step**: Set up project structure and ANTLR4 COBOL grammar

