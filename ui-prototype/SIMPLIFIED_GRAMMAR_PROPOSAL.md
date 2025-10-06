# Simplified Grammar Proposal

**Current**: 90 lines with redundant elements
**Proposed**: 76 lines, cleaner and more maintainable

---

## Proposed Simplified Rules.g4

```antlr
grammar Rules;

// Parser rules
ruleSet: rule+ EOF;

rule: RULE ruleName COLON ruleStep+;
ruleName: STRING | IDENTIFIER;

ruleStep:
    IF condition THEN actionList (ELSE actionList)?
    | actionList;

actionList: action (',' action)*;

condition: orExpression;

orExpression: andExpression (OR andExpression)*;
andExpression: notExpression (AND notExpression)*;
notExpression: NOT? primaryExpression;

primaryExpression:
    comparison
    | '(' orExpression ')';

comparison: operand operator operand;
operand: attribute | value | functionCall;

attribute: attributeIdentifier ('.' attributeIdentifier)*;
attributeIdentifier: STRING | IDENTIFIER;

functionCall: IDENTIFIER '(' functionArgs? ')';
functionArgs: operand (',' operand)*;

operator: (IN | NOT_IN | IS_NULL | IS_NOT_NULL | CONTAINS |
          STARTS_WITH | ENDS_WITH | MATCHES | EQ | NE | LT | LE | GT | GE);

value: STRING | NUMBER | BOOLEAN | NULL | list;
list: '[' (value (',' value)*)? ']';

action:
    IDENTIFIER ('(' parameterList? ')')?
    | STRING ('(' parameterList? ')')?;

parameterList: parameter (',' parameter)*;
parameter: value | attribute;

// Lexer rules
RULE: 'rule';
IF: 'if';
THEN: 'then';
ELSE: 'else';
AND: 'and';
OR: 'or';
NOT: 'not';
IN: 'in';
NOT_IN: 'not_in';
IS_NULL: 'is_null';
IS_NOT_NULL: 'is_not_null';
CONTAINS: 'contains';
STARTS_WITH: 'starts_with';
ENDS_WITH: 'ends_with';
MATCHES: 'matches';

NULL: 'null';

// SIMPLIFIED: One syntax per operator
EQ: '==';
NE: '!=';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';

LPAREN: '(';
RPAREN: ')';
LBRACKET: '[';
RBRACKET: ']';
DOT: '.';
COMMA: ',';
COLON: ':';

BOOLEAN: 'true' | 'false';
NUMBER: [0-9]+ ('.' [0-9]+)?;
STRING: '"' (~["\r\n])* '"' | '\'' (~['\r\n])* '\'';
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;

WS: [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
```

---

## Changes Summary

| Change | Current | Proposed | Benefit |
|--------|---------|----------|---------|
| **Rule Name** | `unifiedRule` | `rule` | Clearer, more intuitive |
| **Wrapper** | `definition` layer | Removed | Simpler parse tree |
| **Comments** | 3 styles (`//`, `/**/`, `#`) | 1 style (`//`) | Consistent, minimal |
| **Operators** | Multiple aliases | One per operator | Easier to learn |
| **Lines** | 90 | 76 | 15% reduction |

---

## Detailed Changes

### 1. Parser Rules Simplification

**Before**:
```antlr
ruleSet: definition+ EOF;
definition: unifiedRule;
unifiedRule: RULE ruleName COLON ruleStep+;
```

**After**:
```antlr
ruleSet: rule+ EOF;
rule: RULE ruleName COLON ruleStep+;
```

**Impact**: One less layer in parse tree, easier tree walking

---

### 2. Comment Simplification

**Before**:
```antlr
WS: [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
HASH_COMMENT: '#' ~[\r\n]* -> skip;
```

**After**:
```antlr
WS: [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
```

**Impact**: DSL authors can only use `//` for comments (consistent with Java/C++)

---

### 3. Operator Simplification

**Before**:
```antlr
NULL: 'null';
EQ: '==' | '=' | 'equals';
NE: '!=' | '<>' | 'not_equals';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';
```

**After**:
```antlr
NULL: 'null';
EQ: '==';
NE: '!=';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';
```

**Impact**:
- ⚠️ Rules using `=` must change to `==`
- ⚠️ Rules using `equals` must change to `==`
- ⚠️ Rules using `<>` must change to `!=`
- ⚠️ Rules using `not_equals` must change to `!=`

**Migration**: Search existing rules for these patterns before deploying

---

## Code Impact Assessment

### Python Code Changes Required

**File**: `backend/grammar_parser/advanced_java_generator.py`

```python
# BEFORE
def enterUnifiedRule(self, ctx):
    if ctx.ruleName().IDENTIFIER():
        self.rule_name = ctx.ruleName().IDENTIFIER().getText()

# AFTER
def enterRule(self, ctx):
    if ctx.ruleName().IDENTIFIER():
        self.rule_name = ctx.ruleName().IDENTIFIER().getText()
```

**Search Pattern**: `enterUnifiedRule`, `exitUnifiedRule`, `unifiedRule`

---

### Rule Migration Examples

**Rules using `=` (single equals)**:
```dsl
// BEFORE
if status = "ACTIVE" then approve

// AFTER
if status == "ACTIVE" then approve
```

**Rules using `equals` keyword**:
```dsl
// BEFORE
if status equals "ACTIVE" then approve

// AFTER
if status == "ACTIVE" then approve
```

---

## Benefits

### Developer Experience
- ✅ **Easier to learn**: One syntax per concept
- ✅ **Clearer naming**: `rule` vs `unifiedRule`
- ✅ **Simpler docs**: Fewer variations to document
- ✅ **Better errors**: Clearer error messages

### Maintenance
- ✅ **Fewer lines**: 15% reduction (90 → 76)
- ✅ **Flatter tree**: One less layer to walk
- ✅ **Consistent style**: One comment syntax
- ✅ **Less cognitive load**: Fewer things to remember

### Performance
- ✅ **Faster parsing**: Simpler grammar = faster parser
- ✅ **Smaller generated code**: Fewer alternatives = less code
- ⚠️ **Minimal impact**: Microseconds difference

---

## Migration Checklist

Before deploying simplified grammar:

### 1. Search Existing Rules
```bash
# Find rules using single equals
grep -r "if .* = " backend/fixtures/demo_data.py

# Find rules using 'equals' keyword
grep -r "equals" backend/fixtures/demo_data.py

# Find rules using '<>'
grep -r "<>" backend/fixtures/demo_data.py

# Find rules using 'not_equals'
grep -r "not_equals" backend/fixtures/demo_data.py
```

### 2. Update Found Rules
- Replace `= ` with `== `
- Replace `equals` with `==`
- Replace `<>` with `!=`
- Replace `not_equals` with `!=`

### 3. Update Python Code
```bash
# Find all references to unifiedRule
grep -r "unifiedRule" backend/

# Update to 'rule'
```

### 4. Regenerate Parser
```bash
cd java-bridge/src/main/antlr4
antlr4 -Dlanguage=Python3 com/rules/grammar/Rules.g4
# Copy generated files to backend
```

### 5. Test
```python
# Test parsing with simplified grammar
test_rules = [
    'rule test: if x == 5 then approve',  # Uses ==
    'rule test2: if y != 10 then reject',  # Uses !=
]

for rule in test_rules:
    parse_and_verify(rule)
```

---

## Recommendation

**Phase 0 (Pre-Enhancement)**:
1. Apply these simplifications FIRST
2. Update existing rules and code
3. Regenerate parser
4. Verify all tests pass

**Then proceed to Phase 1** (arithmetic expressions) with clean foundation.

**Benefits of This Approach**:
- ✅ Start with clean grammar
- ✅ All naming consistent
- ✅ Easier to add arithmetic expressions
- ✅ Better foundation for template system

---

## Approval Required

Do you approve these simplifications?

- ✅ Rename `unifiedRule` → `rule`
- ✅ Remove `definition` wrapper
- ✅ Keep only `//` comments
- ✅ Simplify operators (`==` only, `!=` only)

If yes, we'll apply these changes before Day 2 (arithmetic enhancement).
