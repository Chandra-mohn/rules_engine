# COBOL-to-Rules DSL Migration Workbench

**Project Vision**: Interactive COBOL modernization tool with dual-panel VS Code interface for incremental migration to Rules DSL.

---

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    VS Code Extension                             │
│                                                                   │
│  ┌──────────────────────┐    ┌────────────────────────────┐    │
│  │   COBOL Editor       │    │   Rules DSL Editor          │    │
│  │   (Left Panel)       │    │   (Right Panel)             │    │
│  │                      │    │                             │    │
│  │  Select COBOL →      │    │  ← Accumulated Rules        │    │
│  │  Right-click         │    │     Auto-generated          │    │
│  │  "Convert to DSL"    │    │     Editable                │    │
│  └──────────┬───────────┘    └────────────┬────────────────┘    │
│             │                              │                     │
└─────────────┼──────────────────────────────┼─────────────────────┘
              │                              │
              │ Selected COBOL text          │ Generated DSL
              ↓                              ↑
┌─────────────────────────────────────────────────────────────────┐
│                  COBOL-to-DSL Transpiler Engine                  │
│                                                                   │
│  ┌──────────────┐   ┌──────────────┐   ┌───────────────────┐  │
│  │ ANTLR4       │→→→│ AST          │→→→│ Attribute         │  │
│  │ COBOL Parser │   │ Transformer  │   │ Mapper (DB)       │  │
│  └──────────────┘   └──────────────┘   └───────────────────┘  │
│                                                                   │
│  ┌──────────────┐   ┌──────────────┐   ┌───────────────────┐  │
│  │ Pattern      │→→→│ ActionSet    │→→→│ Rules DSL         │  │
│  │ Detector     │   │ Consolidator │   │ Generator         │  │
│  └──────────────┘   └──────────────┘   └───────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────────────────────────┐
│                    Supporting Services                            │
│                                                                   │
│  • Attribute Mapping Database (COBOL ↔ Target System)           │
│  • Schema Generator (Copybook → JSON Schema)                     │
│  • Semantic Analyzer (Business Intent Extraction)                │
│  • Validation Engine (Generated DSL Verification)                │
└─────────────────────────────────────────────────────────────────┘
```

---

## 2. Component Architecture

### 2.1 VS Code Extension Layer

**Dual-Panel Webview Controller**
```typescript
// Extension command: "COBOL to Rules: Open Migration Workbench"
class MigrationWorkbenchProvider {
  private cobolPanel: TextDocument;
  private dslPanel: TextDocument;
  private accumulatedRules: string[] = [];

  async convertSelection(cobolText: string): Promise<void> {
    // 1. Send to transpiler engine
    const result = await transpiler.convert(cobolText);

    // 2. Append to DSL panel
    this.accumulatedRules.push(result.dsl);
    this.dslPanel.edit(edit => {
      edit.insert(end, result.dsl);
    });

    // 3. Show mapping metadata
    this.showConversionMetadata(result.metadata);
  }
}
```

**Context Menu Integration**
```json
{
  "contributes": {
    "commands": [
      {
        "command": "cobol-migration.convertToDSL",
        "title": "Convert to Rules DSL"
      },
      {
        "command": "cobol-migration.openWorkbench",
        "title": "COBOL to Rules: Open Migration Workbench"
      }
    ],
    "menus": {
      "editor/context": [
        {
          "command": "cobol-migration.convertToDSL",
          "when": "editorLangId == cobol && editorHasSelection",
          "group": "navigation"
        }
      ]
    }
  }
}
```

---

### 2.2 COBOL Parser Layer

**ANTLR4 COBOL Grammar Integration**

```python
# backend/cobol_parser/cobol_ast_parser.py

from antlr4 import *
from cobol_grammar.CobolLexer import CobolLexer
from cobol_grammar.CobolParser import CobolParser
from cobol_grammar.CobolVisitor import CobolVisitor

class CobolASTBuilder(CobolVisitor):
    """
    Walks COBOL AST and builds intermediate representation
    """

    def visitIfStatement(self, ctx):
        """
        COBOL: IF condition THEN statements END-IF
        IR: IfNode(condition, thenBranch, elseBranch)
        """
        return {
            'type': 'if',
            'condition': self.visit(ctx.condition()),
            'then': self.visit(ctx.thenBranch()),
            'else': self.visit(ctx.elseBranch()) if ctx.elseBranch() else None
        }

    def visitEvaluateStatement(self, ctx):
        """
        COBOL: EVALUATE ... WHEN ... END-EVALUATE
        IR: EvaluateNode(subject, whenClauses)
        """
        return {
            'type': 'evaluate',
            'subject': self.visit(ctx.subject()),
            'cases': [self.visit(w) for w in ctx.whenClause()]
        }

    def visitPerformStatement(self, ctx):
        """
        COBOL: PERFORM paragraph-name
        IR: CallNode(paragraphName, params)
        """
        return {
            'type': 'call',
            'target': ctx.paragraphName().getText(),
            'params': []
        }
```

**Example COBOL → AST**
```cobol
IF CUSTOMER-TYPE = 'PREMIUM' AND BALANCE > 10000
   PERFORM APPROVE-PREMIUM
ELSE
   PERFORM REJECT-APPLICATION
END-IF
```

**Intermediate AST**:
```json
{
  "type": "if",
  "condition": {
    "type": "and",
    "left": {
      "type": "comparison",
      "operator": "=",
      "left": {"type": "variable", "name": "CUSTOMER-TYPE"},
      "right": {"type": "literal", "value": "PREMIUM"}
    },
    "right": {
      "type": "comparison",
      "operator": ">",
      "left": {"type": "variable", "name": "BALANCE"},
      "right": {"type": "literal", "value": 10000}
    }
  },
  "then": {
    "type": "call",
    "target": "APPROVE-PREMIUM"
  },
  "else": {
    "type": "call",
    "target": "REJECT-APPLICATION"
  }
}
```

---

### 2.3 Attribute Mapping Layer

**Database Schema**
```sql
CREATE TABLE attribute_mappings (
    id SERIAL PRIMARY KEY,
    cobol_name VARCHAR(255) NOT NULL,
    target_name VARCHAR(255),
    mapping_type VARCHAR(50), -- 'direct', 'derived', 'temporary'
    data_type VARCHAR(50),
    transformation_rule TEXT,
    confidence_score FLOAT,
    is_validated BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE temporary_variables (
    id SERIAL PRIMARY KEY,
    cobol_name VARCHAR(255) NOT NULL,
    suggested_name VARCHAR(255),
    scope VARCHAR(100), -- 'local', 'paragraph', 'program'
    usage_context TEXT
);

CREATE INDEX idx_cobol_name ON attribute_mappings(cobol_name);
```

**Mapping Service**
```python
# backend/services/attribute_mapper.py

class AttributeMapper:
    def __init__(self, db_connection):
        self.db = db_connection
        self.cache = {}

    def map_attribute(self, cobol_name: str) -> AttributeMapping:
        """
        Maps COBOL attribute to target system attribute
        Returns: AttributeMapping with metadata
        """
        # Check cache first
        if cobol_name in self.cache:
            return self.cache[cobol_name]

        # Query database
        result = self.db.query(
            "SELECT * FROM attribute_mappings WHERE cobol_name = %s",
            (cobol_name,)
        )

        if result:
            mapping = AttributeMapping(
                cobol_name=cobol_name,
                target_name=result['target_name'],
                mapping_type='direct',
                confidence=1.0
            )
        else:
            # Generate derived name
            mapping = self._derive_attribute_name(cobol_name)

        self.cache[cobol_name] = mapping
        return mapping

    def _derive_attribute_name(self, cobol_name: str) -> AttributeMapping:
        """
        Generates target attribute name from COBOL name
        CUSTOMER-TYPE → customer.type
        CUST-BAL → customer.balance (with fuzzy matching)
        """
        # Convert COBOL naming to modern naming
        parts = cobol_name.lower().split('-')

        # Fuzzy match to known entities
        entity = self._fuzzy_match_entity(parts[0])
        attribute = '_'.join(parts[1:]) if len(parts) > 1 else parts[0]

        target_name = f"{entity}.{attribute}" if entity else attribute

        return AttributeMapping(
            cobol_name=cobol_name,
            target_name=target_name,
            mapping_type='derived',
            confidence=0.7,
            comment=f"// DERIVED: {cobol_name} → {target_name}"
        )

    def handle_temporary_variable(self, cobol_name: str, context: str) -> str:
        """
        Handles WORKING-STORAGE temporary variables
        Returns: Suggested local variable name
        """
        # Check if it's a temp var pattern (WS-, TEMP-, WORK-, etc.)
        if any(cobol_name.startswith(prefix) for prefix in ['WS-', 'TEMP-', 'WORK-']):
            # Generate meaningful local variable name
            base_name = cobol_name.split('-', 1)[1] if '-' in cobol_name else cobol_name
            return f"temp_{base_name.lower()}"

        return cobol_name.lower().replace('-', '_')
```

**Mapping Metadata in Generated DSL**
```
# Example output with mapping comments

rule "Customer Approval Logic":
    # MAPPED: CUSTOMER-TYPE → customer.type
    # MAPPED: BALANCE → customer.balance
    # DERIVED: APPROVE-PREMIUM → approvePremium() (assumed from PERFORM)

    if customer.type == "PREMIUM" and customer.balance > 10000 then
        approvePremium()
    else
        rejectApplication()
    endif
```

---

### 2.4 AST Transformation Layer

**COBOL AST → Rules DSL AST Transformer**

```python
# backend/transpiler/ast_transformer.py

class CobolToDSLTransformer:
    def __init__(self, attribute_mapper: AttributeMapper):
        self.mapper = attribute_mapper
        self.metadata = []

    def transform(self, cobol_ast: dict) -> dict:
        """
        Transforms COBOL AST to Rules DSL AST
        """
        node_type = cobol_ast['type']

        if node_type == 'if':
            return self._transform_if(cobol_ast)
        elif node_type == 'evaluate':
            return self._transform_evaluate(cobol_ast)
        elif node_type == 'call':
            return self._transform_perform(cobol_ast)
        else:
            return cobol_ast

    def _transform_if(self, node: dict) -> dict:
        """
        IF CUSTOMER-TYPE = 'PREMIUM' ... END-IF
        →
        if customer.type == "PREMIUM" ... endif
        """
        return {
            'type': 'if',
            'condition': self._transform_condition(node['condition']),
            'then': self._transform_block(node['then']),
            'else': self._transform_block(node['else']) if node['else'] else None
        }

    def _transform_condition(self, node: dict) -> dict:
        """
        Transforms COBOL condition to DSL condition
        Handles attribute mapping
        """
        if node['type'] == 'comparison':
            left_var = node['left']['name']
            mapped = self.mapper.map_attribute(left_var)

            # Track mapping metadata
            self.metadata.append({
                'type': 'mapping',
                'cobol': left_var,
                'target': mapped.target_name,
                'mapping_type': mapped.mapping_type,
                'confidence': mapped.confidence
            })

            return {
                'type': 'comparison',
                'operator': self._map_operator(node['operator']),
                'left': {'type': 'variable', 'name': mapped.target_name},
                'right': node['right']
            }

        elif node['type'] == 'and':
            return {
                'type': 'and',
                'left': self._transform_condition(node['left']),
                'right': self._transform_condition(node['right'])
            }

        return node

    def _transform_perform(self, node: dict) -> dict:
        """
        PERFORM APPROVE-PREMIUM
        →
        approvePremium() or actionset reference
        """
        paragraph_name = node['target']

        # Check if this is a known action or needs derivation
        mapped = self.mapper.map_attribute(paragraph_name)

        # Convert to camelCase action name
        action_name = self._to_camel_case(mapped.target_name or paragraph_name)

        self.metadata.append({
            'type': 'action',
            'cobol': paragraph_name,
            'target': action_name,
            'mapping_type': mapped.mapping_type
        })

        return {
            'type': 'action',
            'name': action_name
        }

    def _to_camel_case(self, name: str) -> str:
        """APPROVE-PREMIUM → approvePremium"""
        parts = name.split('-')
        return parts[0].lower() + ''.join(p.capitalize() for p in parts[1:])
```

---

### 2.5 Pattern Detection & ActionSet Consolidation

**Repeated Logic Detector**

```python
# backend/transpiler/pattern_detector.py

class PatternDetector:
    def __init__(self):
        self.patterns = {}
        self.threshold = 2  # Min occurrences to consolidate

    def detect_repeated_patterns(self, cobol_chunks: list) -> dict:
        """
        Analyzes multiple COBOL chunks to find repeated patterns
        Returns: Dictionary of patterns and their occurrences
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

        # Return patterns that exceed threshold
        return {
            sig: data for sig, data in self.patterns.items()
            if data['count'] >= self.threshold
        }

    def _normalize_ast(self, ast: dict) -> dict:
        """
        Normalizes AST by replacing specific values with placeholders
        Example: IF BALANCE > 10000 → IF BALANCE > {PARAM}
        """
        if ast['type'] == 'comparison':
            return {
                'type': 'comparison',
                'operator': ast['operator'],
                'left': ast['left'],
                'right': {'type': 'param', 'placeholder': True}
            }
        # ... more normalization logic
        return ast

    def generate_actionset(self, pattern: dict) -> str:
        """
        Generates ActionSet from repeated pattern
        """
        instances = pattern['instances']

        # Extract parameters that vary across instances
        params = self._extract_parameters(instances)

        # Generate ActionSet DSL
        actionset_name = self._generate_actionset_name(pattern)

        return f"""
actionset "{actionset_name}":
    params: {', '.join(params)}

    {self._generate_actionset_body(pattern['template'], params)}
"""

class ActionSetConsolidator:
    def __init__(self, pattern_detector: PatternDetector):
        self.detector = pattern_detector

    def consolidate(self, accumulated_rules: list) -> dict:
        """
        Analyzes accumulated rules and suggests ActionSet consolidations
        Returns: {
            'original_rules': [...],
            'suggested_actionsets': [...],
            'refactored_rules': [...]
        }
        """
        patterns = self.detector.detect_repeated_patterns(accumulated_rules)

        actionsets = []
        for pattern_id, pattern_data in patterns.items():
            actionset = self.detector.generate_actionset(pattern_data)
            actionsets.append(actionset)

        return {
            'actionsets': actionsets,
            'consolidation_report': self._generate_report(patterns)
        }
```

---

### 2.6 Rules DSL Generator

```python
# backend/transpiler/dsl_generator.py

class RulesDSLGenerator:
    def __init__(self):
        self.indentation = "    "

    def generate(self, dsl_ast: dict, metadata: list) -> str:
        """
        Generates Rules DSL code from transformed AST
        Includes mapping metadata as comments
        """
        # Generate metadata comments
        comments = self._generate_metadata_comments(metadata)

        # Generate rule structure
        rule_name = self._extract_rule_name(dsl_ast)
        rule_body = self._generate_body(dsl_ast)

        return f"""
{comments}
rule "{rule_name}":
{rule_body}
"""

    def _generate_metadata_comments(self, metadata: list) -> str:
        """
        # MAPPED: CUSTOMER-TYPE → customer.type (direct, confidence: 1.0)
        # DERIVED: WORK-TOTAL → temp_total (derived, confidence: 0.7)
        """
        comments = []
        for meta in metadata:
            if meta['mapping_type'] == 'direct':
                comments.append(
                    f"# MAPPED: {meta['cobol']} → {meta['target']}"
                )
            elif meta['mapping_type'] == 'derived':
                comments.append(
                    f"# DERIVED: {meta['cobol']} → {meta['target']} (assumed, confidence: {meta['confidence']})"
                )
        return '\n'.join(comments)

    def _generate_body(self, ast: dict, indent_level: int = 1) -> str:
        """Recursively generates DSL code from AST"""
        indent = self.indentation * indent_level

        if ast['type'] == 'if':
            condition = self._generate_expression(ast['condition'])
            then_block = self._generate_body(ast['then'], indent_level + 1)

            code = f"{indent}if {condition} then\n{then_block}"

            if ast['else']:
                else_block = self._generate_body(ast['else'], indent_level + 1)
                code += f"\n{indent}else\n{else_block}"

            code += f"\n{indent}endif"
            return code

        elif ast['type'] == 'action':
            return f"{indent}{ast['name']}()"

        return ""

    def _generate_expression(self, expr: dict) -> str:
        """Generates DSL expression from AST"""
        if expr['type'] == 'comparison':
            left = expr['left']['name']
            op = expr['operator']
            right = self._format_literal(expr['right'])
            return f"{left} {op} {right}"

        elif expr['type'] == 'and':
            left = self._generate_expression(expr['left'])
            right = self._generate_expression(expr['right'])
            return f"{left} and {right}"

        return ""

    def _format_literal(self, literal: dict) -> str:
        """Formats literal values"""
        if literal['type'] == 'literal':
            value = literal['value']
            if isinstance(value, str):
                return f'"{value}"'
            return str(value)
        return ""
```

---

## 3. User Workflow

### Phase 1: Open Migration Workbench
```
Command Palette → "COBOL to Rules: Open Migration Workbench"
→ Opens dual-panel view
→ Left: Load COBOL file or paste code
→ Right: Empty Rules DSL editor
```

### Phase 2: Incremental Conversion
```
1. User selects COBOL chunk (e.g., IF statement block)
2. Right-click → "Convert to Rules DSL"
3. Transpiler processes:
   - Parse COBOL → AST
   - Map attributes (DB lookup)
   - Transform AST → DSL AST
   - Generate DSL code with metadata
4. DSL code appended to right panel
5. Metadata shown in status bar/hover
```

### Phase 3: Review & Refine
```
- User reviews generated DSL
- Checks mapping metadata
- Manually adjusts derived mappings
- Validates generated code (CLI validation)
```

### Phase 4: ActionSet Consolidation
```
Command: "Analyze for ActionSets"
→ Detects repeated patterns
→ Suggests consolidations
→ User approves/rejects
→ Refactors rules to use ActionSets
```

### Phase 5: Export
```
Command: "Export Rules"
→ Saves accumulated rules to .rules file
→ Optionally saves mapping metadata
→ Generates schema if copybook was converted
```

---

## 4. Example Conversion Flow

### Input COBOL
```cobol
PROCEDURE DIVISION.
    IF CUSTOMER-TYPE = 'PREMIUM'
       AND BALANCE > 10000
       PERFORM APPROVE-PREMIUM
    ELSE
       IF BALANCE > 5000
          PERFORM APPROVE-STANDARD
       ELSE
          PERFORM REJECT-APPLICATION
       END-IF
    END-IF.

APPROVE-PREMIUM.
    MOVE 'APPROVED' TO STATUS-CODE.
    MOVE 'PREMIUM' TO APPROVAL-LEVEL.
    PERFORM UPDATE-CUSTOMER-RECORD.

APPROVE-STANDARD.
    MOVE 'APPROVED' TO STATUS-CODE.
    MOVE 'STANDARD' TO APPROVAL-LEVEL.
    PERFORM UPDATE-CUSTOMER-RECORD.
```

### Intermediate AST (after parsing)
```json
{
  "type": "if",
  "condition": {
    "type": "and",
    "left": {"type": "eq", "left": "CUSTOMER-TYPE", "right": "PREMIUM"},
    "right": {"type": "gt", "left": "BALANCE", "right": 10000}
  },
  "then": {"type": "perform", "target": "APPROVE-PREMIUM"},
  "else": {
    "type": "if",
    "condition": {"type": "gt", "left": "BALANCE", "right": 5000},
    "then": {"type": "perform", "target": "APPROVE-STANDARD"},
    "else": {"type": "perform", "target": "REJECT-APPLICATION"}
  }
}
```

### Generated Rules DSL (with metadata)
```
# MAPPED: CUSTOMER-TYPE → customer.type (direct, confidence: 1.0)
# MAPPED: BALANCE → customer.balance (direct, confidence: 1.0)
# DERIVED: APPROVE-PREMIUM → approvePremium (assumed action)
# DERIVED: APPROVE-STANDARD → approveStandard (assumed action)
# DERIVED: REJECT-APPLICATION → rejectApplication (assumed action)

rule "Customer Approval Logic":
    if customer.type == "PREMIUM" and customer.balance > 10000 then
        approvePremium()
    else
        if customer.balance > 5000 then
            approveStandard()
        else
            rejectApplication()
        endif
    endif

# Pattern detected: Repeated approval logic
# Suggested ActionSet consolidation below:

actionset "ApprovalProcess":
    params: level

    setStatus("APPROVED")
    setApprovalLevel(level)
    updateCustomerRecord()
```

---

## 5. Implementation Phases

### Phase 1: Foundation (Week 1-2)
- [ ] Set up ANTLR4 COBOL grammar
- [ ] Build COBOL AST parser
- [ ] Create attribute mapping database schema
- [ ] Implement basic AST transformer

### Phase 2: Core Transpiler (Week 3-4)
- [ ] Implement IF/ELSE transformation
- [ ] Implement EVALUATE transformation
- [ ] Implement PERFORM transformation
- [ ] Build attribute mapper with DB integration
- [ ] Create DSL code generator

### Phase 3: VS Code Extension (Week 5-6)
- [ ] Build dual-panel webview
- [ ] Implement context menu integration
- [ ] Add incremental conversion workflow
- [ ] Create metadata visualization

### Phase 4: Advanced Features (Week 7-8)
- [ ] Pattern detection algorithm
- [ ] ActionSet consolidation engine
- [ ] Copybook → Schema generator
- [ ] Validation integration

### Phase 5: Polish & Testing (Week 9-10)
- [ ] User testing with real COBOL code
- [ ] Performance optimization
- [ ] Documentation
- [ ] Migration guides

---

## 6. Technical Decisions

### Language Choices
- **Backend Transpiler**: Python (existing codebase, ANTLR integration)
- **VS Code Extension**: TypeScript (standard for VS Code)
- **Database**: PostgreSQL (attribute mapping storage)

### Libraries
- **ANTLR4**: Parser generation (COBOL + Rules DSL)
- **python-antlr4**: Python runtime for ANTLR
- **SQLAlchemy**: Database ORM
- **TypeScript**: VS Code extension development

### Integration Points
- **Existing CLI**: Leverage `generate_code_cli.py` for validation
- **Schema System**: Extend existing schema file service
- **Context System**: Use existing context file service

---

## 7. Open Questions & Decisions Needed

1. **Semantic Intent Extraction**: How deep should we go?
   - Basic: Literal code conversion
   - Advanced: NLP-based business intent extraction
   - Hybrid: Pattern matching + manual annotation

2. **Temporary Variables**:
   - Should we preserve COBOL temp vars as local variables?
   - Or inline them if they're single-use?

3. **88-Level Conditions**:
   ```cobol
   01 STATUS-CODE PIC 9.
      88 STATUS-APPROVED VALUE 1.
      88 STATUS-REJECTED VALUE 2.
   ```
   - Convert to constants/enums?
   - Convert to boolean expressions?

4. **Copybook Handling**:
   - Full schema auto-generation priority?
   - Or manual schema creation with assist?

---

## 8. Success Metrics

- **Conversion Accuracy**: 90%+ correct mappings for direct attributes
- **Pattern Recognition**: Detect 80%+ of repeated logic patterns
- **User Productivity**: 5x faster than manual conversion
- **Code Quality**: Generated DSL passes validation without manual fixes

---

## Next Steps

1. Review this architecture document
2. Discuss open questions and make decisions
3. Set up ANTLR4 COBOL grammar in project
4. Build minimal viable prototype:
   - Simple IF statement conversion
   - Basic attribute mapping
   - Dual-panel proof of concept
5. Iterate based on feedback

---

**Status**: Architecture Proposal - Ready for Review
**Author**: Claude Code Assistant
**Date**: 2025-11-23
