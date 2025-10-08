# Rules Engine Code Generation Architecture

**Last Updated**: 2025-10-07
**Current Implementation**: Template-Based Generation with Python ANTLR

---

## Overview

The Rules Engine implements a template-based code generation system that transforms Domain-Specific Language (DSL) rules into optimized Java code for high-performance execution. The system uses Python ANTLR for parsing and Python f-string templates for code generation - **no external template engines required**.

---

## System Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│                    RULES ENGINE ARCHITECTURE                     │
├──────────────────────────────────────────────────────────────────┤
│  Frontend (React)    │  Backend (Flask)     │  Engine (Java)     │
│  ┌─────────────────┐ │  ┌─────────────────┐ │  ┌───────────────┐ │
│  │ Rule Editor     │ │  │ Python Rules    │ │  │ Generated     │ │
│  │ (Monaco)        │ │  │ Engine          │ │  │ Java Classes  │ │
│  │                 │ │  │                 │ │  │               │ │
│  │ - Syntax HL     │ │  │ - ANTLR Parser  │ │  │ - Executors   │ │
│  │ - Validation    │ │  │ - Templates     │ │  │ - Compiled    │ │
│  │ - Auto-complete │ │  │ - Validation    │ │  │ - Optimized   │ │
│  └─────────────────┘ │  └─────────────────┘ │  └───────────────┘ │
└──────────────────────────────────────────────────────────────────┘

                    HTTP/REST API ──────────┐
                                            ▼
┌──────────────────────────────────────────────────────────────────┐
│                  TEMPLATE-BASED GENERATION PIPELINE              │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  DSL Input → ANTLR Parse → Extract Data → Apply Templates → Java │
│                   │            │              │                  │
│                   ▼            ▼              ▼                  │
│              RulesEngineParser              TemplateCodeGenerator│
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

---

## Code Generation Flow

### Phase 1: Input Processing & Parsing

```
┌─────────────────────────────────────────────────────────────────┐
│                     PHASE 1: PARSING                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Rule DSL                ANTLR Parser               AST         │
│  ┌─────────┐             ┌─────────────┐           ┌─────────┐  │
│  │ rule    │             │ Python      │           │ Parse   │  │
│  │ "Credit │    ──────►  │ ANTLR       │  ──────►  │ Tree    │  │
│  │ Check": │             │ Parser      │           │         │  │
│  │ if ...  │             │             │           │ - Rules │  │
│  │ then... │             │ - Tokenize  │           │ - Conds │  │
│  └─────────┘             │ - Grammar   │           │ - Acts  │  │
│                          │ - Validate  │           │         │  │
│                          └─────────────┘           └─────────┘  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Components Involved:**

1. **RulesEngineParser** (`grammar_parser/rules_parser.py`)
   - Python ANTLR4-based parser
   - Uses Python-generated lexer/parser from Rules.g4
   - Lexical and syntactic analysis
   - Error collection and reporting

2. **Grammar Definition** (`java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4`)
   - DSL syntax specification
   - Token definitions
   - Parse tree structure
   - Generates both Java and Python parsers

3. **Generated Python ANTLR Files** (`backend/java-bridge/src/main/antlr4/com/rules/grammar/`)
   - `RulesLexer.py` - Token recognition
   - `RulesParser.py` - Grammar parsing
   - `RulesListener.py` - Parse tree walking

**Example DSL Input:**
```
rule "Credit Score Check":
    if applicant.creditScore >= 750 then
        approveApplication
    endif
```

---

### Phase 2: Data Extraction

```
┌─────────────────────────────────────────────────────────────────┐
│                   PHASE 2: DATA EXTRACTION                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Parse Tree            RuleDataExtractor        Structured Data │
│  ┌─────────┐           ┌─────────────┐          ┌────────────┐  │
│  │ AST     │           │ ANTLR       │          │ Python     │  │
│  │ Nodes   │  ──────►  │ Listener    │  ──────► │ Dict/List  │  │
│  │         │           │             │          │            │  │
│  │ - Rules │           │ - Extract   │          │ - Name     │  │
│  │ - Conds │           │ - Convert   │          │ - Steps    │  │
│  │ - Acts  │           │ - Analyze   │          │ - Entities │  │
│  └─────────┘           └─────────────┘          └────────────┘  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Components Involved:**

1. **RuleDataExtractor** (`grammar_parser/template_code_generator.py:20-250`)
   - ANTLR Listener implementation
   - Walks parse tree extracting structured data
   - Converts conditions, actions, attributes to Java code strings
   - Tracks entities, complexity, performance category

2. **Data Structure** (Python dicts/lists):
```python
{
    'rule_name': 'Credit Score Check',
    'entities': {'applicant'},
    'rule_steps': ['if (condition) { actions.add("..."); }'],
    'complexity_score': 2,
    'performance_category': 'hot',
    'estimated_steps': 3
}
```

---

### Phase 3: Template-Based Code Generation

```
┌─────────────────────────────────────────────────────────────────┐
│                 PHASE 3: TEMPLATE GENERATION                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Structured Data     Python Templates          Java Code        │
│  ┌────────────┐      ┌─────────────┐          ┌─────────────┐   │
│  │ Rule Data  │      │ f-string    │          │ Complete    │   │
│  │ Dict/List  │  ──► │ Templates   │  ──────► │ Java Class  │   │
│  │            │      │             │          │             │   │
│  │ - Name     │      │ - Skeleton  │          │ - Package   │   │
│  │ - Steps    │      │ - Methods   │          │ - Imports   │   │
│  │ - Category │      │ - Helpers   │          │ - Logic     │   │
│  └────────────┘      └─────────────┘          └─────────────┘   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Components Involved:**

1. **TemplateCodeGenerator** (`grammar_parser/template_code_generator.py:260-340`)
   ```python
   class TemplateCodeGenerator:
       def generate(self, rule_content: str, item_type: str = 'rule') -> str:
           # Parse with ANTLR
           tree, error_listener = self.parser.parse(rule_content)

           # Extract structured data
           extractor = RuleDataExtractor()
           walker.walk(extractor, tree)

           # Apply appropriate template
           if item_type == 'action':
               return generate_action(extractor)
           elif item_type == 'actionset':
               return generate_actionset(extractor)
           else:
               return generate_standard_rule(extractor)
   ```

2. **Template Functions** (`templates/java/standard_rule_template.py`)
   - `generate_standard_rule()` - For regular rules
   - `generate_actionset()` - For ActionSet composition
   - `generate_action()` - For single actions

   Uses Python f-strings:
   ```python
   def generate_standard_rule(extractor: RuleDataExtractor) -> str:
       class_name = sanitize_name(extractor.rule_name) + "Rule"

       java_code = f"""
   package com.rules.generated;

   import java.util.*;

   /**
    * Generated rule: {extractor.rule_name}
    * Performance Category: {extractor.performance_category}
    * Complexity Score: {extractor.complexity_score}/10
    */
   public class {class_name} {{

       public static RuleResult evaluate(Map<String, Object> context) {{
           // ... generated logic ...
       }}
   }}
   """
       return java_code
   ```

---

### Phase 4: Compilation & Deployment

```
┌─────────────────────────────────────────────────────────────────┐
│                 PHASE 4: COMPILATION & DEPLOY                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Java Source            Compiler                Runtime         │
│  ┌─────────────┐        ┌─────────────┐        ┌─────────────┐  │
│  │ Generated   │        │ Maven       │        │ Class       │  │
│  │ .java Files │ ────►  │ Build       │ ────►  │ Loading     │  │
│  │             │        │             │        │             │  │
│  │ - Classes   │        │ - Compile   │        │ - Hot       │  │
│  │ - Methods   │        │ - Package   │        │ - Dynamic   │  │
│  │ - Logic     │        │ - Test      │        │ - Cached    │  │
│  └─────────────┘        └─────────────┘        └─────────────┘  │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Components Involved:**

1. **Maven Build System** (`java-bridge/pom.xml`)
   - Automated compilation
   - Dependency resolution
   - JAR packaging

2. **PythonRulesEngine** (`services/python_rules_engine.py`)
   ```python
   class PythonRulesEngine:
       def __init__(self):
           self.parser = RulesEngineParser()
           self.code_generator = TemplateCodeGenerator()  # Current implementation
           self.validator = RuleValidator()
           self.compiled_rules_cache = {}
   ```

---

## Detailed Component Analysis

### 1. Current Generator Architecture

```
PythonRulesEngine
       │
       ├── RulesEngineParser (Python ANTLR)
       │        │
       │        └── Python generated files from Rules.g4
       │
       ├── TemplateCodeGenerator
       │        │
       │        ├── RuleDataExtractor (ANTLR Listener)
       │        └── Template Functions (f-strings)
       │
       └── RuleValidator (Semantic validation)
```

**Architecture Benefits:**
- **No External Dependencies**: Pure Python f-strings, no Jinja2 or other template engines
- **ANTLR Native**: Full DSL parsing with Python-generated ANTLR classes
- **Simple & Maintainable**: Direct f-string templates easy to understand and modify
- **Type Safe**: Python type hints throughout code generation pipeline

---

### 2. Template System

The template system uses Python f-strings for maximum simplicity:

**Template Structure** (`templates/java/standard_rule_template.py`):
```python
def generate_standard_rule(extractor: RuleDataExtractor) -> str:
    """
    Generate Java code for a standard rule using f-string templates.
    No external template engine required.
    """
    # Extract data
    class_name = sanitize_name(extractor.rule_name) + "Rule"
    entities = ', '.join(f'"{e}"' for e in extractor.entities)
    rule_steps_java = '\n        '.join(extractor.rule_steps)

    # Generate using f-string
    return f"""
package com.rules.generated;

import java.util.*;
import java.time.LocalDate;

public class {class_name} {{

    public static class RuleResult {{
        private final boolean matched;
        private final List<String> actions;

        // ... constructor and getters ...
    }}

    public static RuleResult evaluate(Map<String, Object> context) {{
        List<String> actions = new ArrayList<>();
        boolean matched = false;

        // Extract entities
        {extractor._generate_entity_extraction()}

        // Execute rule logic
        {rule_steps_java}

        return new RuleResult(matched, actions, null);
    }}

    // Helper methods for null-safe operations
    private static Object _getFieldValue(Map<String, Object> entity, String fieldName) {{
        return entity != null ? entity.get(fieldName) : null;
    }}

    // ... more helper methods ...
}}
"""
```

---

### 3. ANTLR Integration

**Grammar File** (`Rules.g4`):
- Defines DSL syntax
- Generates both Java and Python parsers
- Python version used by backend

**Generation Command**:
```bash
# Generate Python parser classes
antlr4 -Dlanguage=Python3 -o backend/java-bridge/src/main/antlr4/com/rules/grammar Rules.g4
```

**Generated Python Files**:
```
backend/java-bridge/src/main/antlr4/com/rules/grammar/
├── RulesLexer.py         # Tokenization
├── RulesParser.py        # Grammar parsing
├── RulesListener.py      # Tree walking
└── Rules.tokens          # Token definitions
```

---

### 4. Performance Categories

Rules are categorized based on complexity:

```
┌────────────────────────────────────────────────────────────┐
│                   PERFORMANCE TIERS                        │
├────────────────────────────────────────────────────────────┤
│                                                            │
│   HOT PATH (Score 1-3)                                     │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ • Simple conditions (1-2 operators)                 │   │
│  │ • Direct attribute access                           │   │
│  │ • Single action execution                           │   │
│  │ • Estimated: <5 execution steps                     │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                            │
│   WARM PATH (Score 4-7)                                    │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ • Moderate complexity (3-5 operators)               │   │
│  │ • Some nested conditions                            │   │
│  │ • Multiple actions                                  │   │
│  │ • Estimated: 5-15 execution steps                   │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                            │
│   COLD PATH (Score 8-10)                                   │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ • Complex nested logic                              │   │
│  │ • Heavy computational requirements                  │   │
│  │ • Multiple decision points                          │   │
│  │ • Estimated: >15 execution steps                    │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

**Complexity Scoring** (in `RuleDataExtractor`):
- Base score starts at 0
- +1 for each condition
- +2 for each AND/OR operator
- +1 for each nested condition
- +1 for every 3 actions

---

## Example: Complete Generation Process

### Input DSL Rule
```
rule "Premium Card Approval":
    if applicant.creditScore >= 800 and applicant.income > 100000 then
        approveApplication,
        assignPremiumBenefits
    endif
```

### Step 1: ANTLR Parsing
```python
# RulesEngineParser creates parse tree
tree, error_listener = parser.parse(rule_content)
# tree is ANTLR ParseTree with structured nodes
```

### Step 2: Data Extraction
```python
# RuleDataExtractor walks tree
extractor = RuleDataExtractor()
walker = ParseTreeWalker()
walker.walk(extractor, tree)

# Extracted data:
{
    'rule_name': 'Premium Card Approval',
    'entities': {'applicant'},
    'rule_steps': [
        'if (condition_java) {\n    matched = true;\n    actions.add("approveApplication");\n    actions.add("assignPremiumBenefits");\n}'
    ],
    'complexity_score': 4,
    'performance_category': 'warm',
    'estimated_steps': 8
}
```

### Step 3: Template Application
```python
# generate_standard_rule() applies f-string template
java_code = generate_standard_rule(extractor)
```

### Step 4: Generated Java Output
```java
package com.rules.generated;

import java.util.*;

/**
 * Generated rule: Premium Card Approval
 * Performance Category: warm
 * Complexity Score: 4/10
 */
public class PremiumCardApprovalRule {

    public static class RuleResult {
        private final boolean matched;
        private final List<String> actions;
        private final String finalAction;

        public RuleResult(boolean matched, List<String> actions, String finalAction) {
            this.matched = matched;
            this.actions = actions;
            this.finalAction = finalAction;
        }

        public boolean isMatched() { return matched; }
        public List<String> getActions() { return actions; }
        public String getFinalAction() { return finalAction; }
    }

    public static RuleResult evaluate(Map<String, Object> context) {
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = false;

        // Extract entities
        Map<String, Object> applicant = (Map<String, Object>) context.get("applicant");

        // Execute rule logic
        if (_compareTo(_getFieldValue(applicant, "creditScore"), 800) >= 0
            && _compareTo(_getFieldValue(applicant, "income"), 100000) > 0) {
            matched = true;
            actions.add("approveApplication");
            actions.add("assignPremiumBenefits");
        }

        return new RuleResult(matched, actions, finalAction);
    }

    // Helper methods
    private static Object _getFieldValue(Map<String, Object> entity, String fieldName) {
        return entity != null ? entity.get(fieldName) : null;
    }

    private static int _compareTo(Object a, Object b) {
        if (a == null || b == null) return 0;
        if (a instanceof Number && b instanceof Number) {
            return Double.compare(((Number)a).doubleValue(), ((Number)b).doubleValue());
        }
        return a.toString().compareTo(b.toString());
    }
}
```

---

## Performance Characteristics

```
┌─────────────────────────────────────────────────────────────┐
│                    PERFORMANCE METRICS                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Generation Performance                                    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │ • Rule Compilation: 63ms average                    │    │
│  │ • Rule Execution: 0.67ms average                    │    │
│  │ • Memory Usage: 2KB per rule                        │    │
│  │ • Target Throughput: 80K+ TPS                       │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                             │
│   Optimization Strategies                                   │
│  ┌─────────────────────────────────────────────────────┐    │
│  │ • Hot Path: Direct field access, inline conditions  │    │
│  │ • Warm Path: Balanced code/performance trade-offs   │    │
│  │ • Cold Path: Maintainable, readable generated code  │    │
│  │ • Caching: Compiled rule bytecode caching           │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## API Integration

**Current Endpoints**:
- `POST /api/rules/validate` - Validates DSL syntax and semantics using Python ANTLR
- `POST /api/rules/compile` - Compiles rule to Java via TemplateCodeGenerator
- `POST /api/rules/test` - Tests generated Java rule with sample data
- `GET /api/rules` - Retrieves rules and metadata

**Code Flow**:
```
API Request → RuleService → PythonRulesEngine → TemplateCodeGenerator → Java Output
```

---

## File Locations

### Key Files
- **Grammar**: `java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4`
- **Python Parser**: `backend/grammar_parser/rules_parser.py`
- **Generator**: `backend/grammar_parser/template_code_generator.py`
- **Templates**: `backend/templates/java/standard_rule_template.py`
- **Engine**: `backend/services/python_rules_engine.py`
- **Validator**: `backend/grammar_parser/rule_validator.py`

### Generated Files (Python ANTLR)
```
backend/java-bridge/src/main/antlr4/com/rules/grammar/
├── RulesLexer.py
├── RulesParser.py
├── RulesListener.py
└── Rules.tokens
```

---

## Current Architecture Benefits

✅ **Simple**: No external template engines (Jinja2, Mako, etc.)
✅ **Fast**: Direct Python f-string generation
✅ **Maintainable**: Easy to read and modify templates
✅ **Type Safe**: Python type hints throughout
✅ **ANTLR Native**: Full DSL parsing capabilities
✅ **Flexible**: Easy to add new rule types or templates

---

## Enhancement Opportunities

1. **Grammar Extensions**: Add new DSL constructs (loops, functions, etc.)
2. **Template Variants**: Hot-path specific templates for ultra-fast rules
3. **Multi-Language**: Generate Python, C#, or Rust code from same DSL
4. **Optimization**: Machine learning-based performance tuning
5. **Distributed**: Horizontal scaling for enterprise rule sets

---

