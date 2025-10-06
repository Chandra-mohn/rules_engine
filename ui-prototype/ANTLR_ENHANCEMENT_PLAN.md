# ANTLR Grammar Enhancement & Template System Implementation Plan

**Goal**: Master ANTLR grammar design and create template-based code generation
**Timeline**: 1-2 weeks for complete implementation
**Focus**: Learning-driven development with production-ready outcomes

---

## 🎓 Phase 1: ANTLR Fundamentals (Days 1-2)

### **1.1 Understanding Your Current Grammar**

**Current Capabilities** ✅:
```antlr
attribute: attributeIdentifier ('.' attributeIdentifier)*;  // Line 29
```
- ✅ Nested attributes ALREADY SUPPORTED: `transaction.location.country`
- The grammar accepts this syntax!
- **Issue**: Code generator doesn't handle it properly

**What Works**:
```dsl
applicant.creditScore >= 700              ✅ Simple attribute
applicant.employmentStatus == "employed"  ✅ String comparison
functionCall(param1, param2)              ✅ Function calls (line 33-34)
action(param1, param2)                    ✅ Parameterized actions (line 43-44)
```

**What Needs Work**:
```dsl
transaction.location.country != "US"      ⚠️ Grammar OK, generator broken
approveTransaction(amount * 0.025)        ❌ Arithmetic not in grammar
applyFee(amount * 0.025)                  ❌ Arithmetic not in grammar
```

### **1.2 ANTLR Grammar Analysis Exercise**

**Let's analyze your grammar structure**:

```
Grammar Hierarchy:
└── ruleSet (root)
    └── definition
        └── unifiedRule
            ├── ruleName (STRING | IDENTIFIER)
            └── ruleStep+ (one or more steps)
                ├── IF condition THEN actionList (ELSE actionList)?
                └── actionList (direct actions)

Condition Hierarchy:
condition → orExpression → andExpression → notExpression → primaryExpression → comparison

Operand Support:
operand: attribute | value | functionCall
         ↑          ↑       ↑
         Nested     Literal  Function
```

**Key Insight**: Your grammar is well-structured! The issues are in:
1. **Code generator** not handling nested attributes
2. **Missing arithmetic expressions** in grammar
3. **Parameter validation** not handling complex expressions

---

## 🔧 Phase 2: Grammar Enhancements (Days 3-4)

### **2.1 Add Arithmetic Expression Support**

**Current Problem**:
```antlr
operand: attribute | value | functionCall;  // Line 27
parameter: value | attribute;                // Line 47
```

Can't do: `amount * 0.025` or `price + tax`

**Solution - Add Expression Rules**:

```antlr
// NEW: Arithmetic expressions with proper precedence
operand: expression;

expression:
    term ((PLUS | MINUS) term)*;

term:
    factor ((MULTIPLY | DIVIDE | MODULO) factor)*;

factor:
    attribute
    | value
    | functionCall
    | '(' expression ')'
    | MINUS factor;  // Unary minus

// NEW: Lexer tokens
PLUS: '+';
MINUS: '-';
MULTIPLY: '*';
DIVIDE: '/';
MODULO: '%';
```

**Why This Works**:
- Proper operator precedence: `2 + 3 * 4 = 14` (not 20)
- Supports parentheses: `(2 + 3) * 4 = 20`
- Allows nested expressions: `(price * quantity) * (1 + taxRate)`

### **2.2 Enhanced Parameter Support**

**Current Problem**:
```antlr
parameter: value | attribute;  // Can't pass expressions
```

**Solution**:
```antlr
parameter: expression | attribute | value;
```

Now supports:
```dsl
approveTransaction(amount * 0.025)           ✅
calculateFee(baseAmount * rate + overhead)   ✅
updateBalance(amount + fee)                  ✅
```

### **2.3 Complete Enhanced Grammar**

Here's the full grammar with all enhancements:

```antlr
grammar Rules;

// Parser rules
ruleSet: definition+ EOF;
definition: unifiedRule;

unifiedRule: RULE ruleName COLON ruleStep+;
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

// ENHANCED: Now uses expression instead of operand
comparison: expression operator expression;

// NEW: Arithmetic expression support with precedence
expression:
    term ((PLUS | MINUS) term)*;

term:
    factor ((MULTIPLY | DIVIDE | MODULO) factor)*;

factor:
    attribute
    | value
    | functionCall
    | LPAREN expression RPAREN
    | MINUS factor;  // Unary minus

attribute: attributeIdentifier (DOT attributeIdentifier)*;
attributeIdentifier: STRING | IDENTIFIER;

functionCall: IDENTIFIER LPAREN functionArgs? RPAREN;
functionArgs: expression (COMMA expression)*;

operator: (IN | NOT_IN | IS_NULL | IS_NOT_NULL | CONTAINS |
          STARTS_WITH | ENDS_WITH | MATCHES | EQ | NE | LT | LE | GT | GE);

value: STRING | NUMBER | BOOLEAN | NULL | list;
list: LBRACKET (value (COMMA value)*)? RBRACKET;

action:
    IDENTIFIER (LPAREN parameterList? RPAREN)?
    | STRING (LPAREN parameterList? RPAREN)?;

// ENHANCED: Parameters can now be expressions
parameterList: parameter (COMMA parameter)*;
parameter: expression;

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
EQ: '==' | '=' | 'equals';
NE: '!=' | '<>' | 'not_equals';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';

// NEW: Arithmetic operators
PLUS: '+';
MINUS: '-';
MULTIPLY: '*';
DIVIDE: '/';
MODULO: '%';

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
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
HASH_COMMENT: '#' ~[\r\n]* -> skip;
```

**Key Changes Summary**:
1. ✅ `operand` → `expression` (supports arithmetic)
2. ✅ Added `expression`, `term`, `factor` hierarchy (precedence)
3. ✅ Added arithmetic operators: `+`, `-`, `*`, `/`, `%`
4. ✅ `parameter` now accepts `expression` (complex parameters)
5. ✅ Unary minus support: `-balance`
6. ✅ Parentheses in expressions: `(a + b) * c`

---

## 🎨 Phase 3: Template System Design (Days 5-6)

### **3.1 Template Architecture**

**Directory Structure**:
```
backend/
  templates/
    java/
      base_rule.java.template          # Common structure
      standard_rule.java.template      # Simple if-then rules
      actionset.java.template          # Multi-branch decisions
      monetary_rule.java.template      # Financial calculations
      non_monetary_rule.java.template  # Business logic

    fragments/
      condition_handler.java           # Reusable condition blocks
      arithmetic_expression.java       # Arithmetic conversion
      nested_attribute.java            # Multi-level field access
      action_executor.java             # Action invocation patterns
```

### **3.2 Template Variables**

**Standard Variables** (all templates):
```python
{
    'class_name': 'RewardsPointsRule',
    'package_name': 'com.rules.generated',
    'rule_name': 'rewardsPointsRule',
    'imports': ['java.util.*', 'java.math.BigDecimal'],
    'entity_declarations': 'Map<String, Object> applicant = ...',
    'rule_logic': '// Generated condition/action code',
    'helper_methods': '// Utility methods',
}
```

**Rule-Specific Variables**:
```python
# Standard Rule
{
    'conditions': [
        {
            'java_code': 'if (creditScore >= 700 && annualIncome >= 60000)',
            'then_actions': ['approveApplication'],
            'else_actions': []
        }
    ]
}

# Monetary Rule
{
    'arithmetic_expressions': [
        {
            'original': 'amount * 0.025',
            'java_code': '_multiplyDecimal(amount, new BigDecimal("0.025"))'
        }
    ],
    'precision_mode': 'BigDecimal',  # vs 'double'
}

# ActionSet
{
    'decision_tree': {
        'if': 'creditScore >= 720',
        'then': {
            'if': 'annualIncome >= 75000',
            'then': ['premiumRewards', '"5% cashback"'],
            'else': ['standardRewards', '"2% cashback"']
        }
    }
}
```

### **3.3 Base Template Structure**

```java
// base_rule.java.template
package {{package_name}};

{{#each imports}}
import {{this}};
{{/each}}

/**
 * Generated rule: {{rule_name}}
 * Type: {{rule_type}}
 * Generated: {{timestamp}}
 */
public class {{class_name}} {

    public static class RuleResult {
        private boolean matched;
        private List<String> actions;
        private Map<String, Object> updatedContext;

        // Constructor and getters
    }

    public static RuleResult evaluate(Map<String, Object> context) {
        List<String> actions = new ArrayList<>();
        boolean matched = false;

        {{entity_declarations}}

        {{rule_logic}}

        return new RuleResult(matched, actions, context);
    }

    {{helper_methods}}
}
```

---

## 🔨 Phase 4: Code Generator Implementation (Days 7-9)

### **4.1 Template-Based Generator**

```python
# backend/services/template_code_generator.py

from pathlib import Path
from typing import Dict, Any, List
from jinja2 import Environment, FileSystemLoader
from antlr4 import *
from com.rules.grammar.RulesLexer import RulesLexer
from com.rules.grammar.RulesParser import RulesParser

class TemplateCodeGenerator:
    """
    Template-based code generator leveraging ANTLR parse tree.
    Uses Jinja2 templates for maintainable, readable generation.
    """

    def __init__(self, templates_dir: str = 'templates/java'):
        self.env = Environment(loader=FileSystemLoader(templates_dir))
        self.env.filters['to_java_type'] = self._to_java_type
        self.env.filters['escape_string'] = self._escape_string

    def generate(self, rule_content: str, rule_name: str,
                 rule_type: str = 'standard') -> str:
        """
        Main generation method.

        Args:
            rule_content: DSL rule text
            rule_name: Rule identifier
            rule_type: 'standard', 'actionset', 'monetary', 'non_monetary'

        Returns:
            Generated Java code
        """
        # Parse with ANTLR
        parse_tree = self._parse_rule(rule_content)

        # Extract structured data from parse tree
        rule_data = self._extract_rule_data(parse_tree, rule_type)

        # Generate from template
        template_name = f'{rule_type}_rule.java.template'
        template = self.env.get_template(template_name)

        return template.render(**rule_data)

    def _parse_rule(self, rule_content: str):
        """Parse DSL using ANTLR."""
        input_stream = InputStream(rule_content)
        lexer = RulesLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = RulesParser(token_stream)
        return parser.unifiedRule()

    def _extract_rule_data(self, parse_tree, rule_type: str) -> Dict[str, Any]:
        """
        Extract structured data from ANTLR parse tree.
        This is where you leverage your ANTLR knowledge!
        """
        extractor = RuleDataExtractor(rule_type)
        walker = ParseTreeWalker()
        walker.walk(extractor, parse_tree)

        return {
            'class_name': self._to_class_name(extractor.rule_name),
            'package_name': 'com.rules.generated',
            'rule_name': extractor.rule_name,
            'imports': extractor.required_imports,
            'entity_declarations': self._generate_entity_declarations(
                extractor.entities_used
            ),
            'rule_logic': self._generate_rule_logic(
                extractor.conditions,
                extractor.actions,
                rule_type
            ),
            'helper_methods': self._select_helper_methods(
                extractor.required_helpers
            ),
            'timestamp': datetime.now().isoformat(),
            'rule_type': rule_type
        }

    def _generate_rule_logic(self, conditions: List[Dict],
                            actions: List[Dict],
                            rule_type: str) -> str:
        """
        Generate the core rule logic based on type.
        This method routes to type-specific generators.
        """
        if rule_type == 'standard':
            return self._generate_standard_logic(conditions, actions)
        elif rule_type == 'actionset':
            return self._generate_actionset_logic(conditions, actions)
        elif rule_type == 'monetary':
            return self._generate_monetary_logic(conditions, actions)
        elif rule_type == 'non_monetary':
            return self._generate_non_monetary_logic(conditions, actions)

    def _generate_standard_logic(self, conditions: List[Dict],
                                 actions: List[Dict]) -> str:
        """Generate simple if-then-else chains."""
        logic_lines = []

        for condition in conditions:
            # Convert parsed condition to Java
            java_condition = self._condition_to_java(condition['expression'])

            logic_lines.append(f"if ({java_condition}) {{")
            logic_lines.append("    matched = true;")

            # Generate then actions
            for action in condition['then_actions']:
                java_action = self._action_to_java(action)
                logic_lines.append(f"    {java_action}")

            # Generate else actions if present
            if condition.get('else_actions'):
                logic_lines.append("} else {")
                for action in condition['else_actions']:
                    java_action = self._action_to_java(action)
                    logic_lines.append(f"    {java_action}")

            logic_lines.append("}")

        return '\n'.join(logic_lines)

    def _condition_to_java(self, condition_tree) -> str:
        """
        Convert ANTLR condition parse tree to Java expression.

        This is where you handle:
        - Nested attributes: transaction.location.country
        - Arithmetic: amount * 0.025
        - Complex operators: contains, starts_with, etc.
        """
        visitor = ConditionToJavaVisitor()
        return visitor.visit(condition_tree)

    def _action_to_java(self, action_tree) -> str:
        """
        Convert ANTLR action parse tree to Java statement.

        Handles:
        - Simple actions: approveApplication
        - Parameterized: approveTransaction(amount)
        - With arithmetic: applyFee(amount * 0.025)
        """
        visitor = ActionToJavaVisitor()
        return visitor.visit(action_tree)


class RuleDataExtractor(RulesListener):
    """
    ANTLR listener that extracts structured data from parse tree.
    This is your learning playground for ANTLR tree walking!
    """

    def __init__(self, rule_type: str):
        self.rule_type = rule_type
        self.rule_name = None
        self.conditions = []
        self.actions = []
        self.entities_used = set()
        self.required_imports = [
            'java.util.*',
            'java.math.BigDecimal',
            'java.time.LocalDateTime'
        ]
        self.required_helpers = set()

    def enterUnifiedRule(self, ctx):
        """Extract rule name (called when parser enters rule definition)."""
        if ctx.ruleName().IDENTIFIER():
            self.rule_name = ctx.ruleName().IDENTIFIER().getText()
        elif ctx.ruleName().STRING():
            self.rule_name = ctx.ruleName().STRING().getText().strip('"\'')

    def enterAttribute(self, ctx):
        """
        Extract attribute references (nested or simple).
        Example: transaction.location.country
        """
        attribute_parts = []
        for identifier in ctx.attributeIdentifier():
            if identifier.IDENTIFIER():
                attribute_parts.append(identifier.IDENTIFIER().getText())
            elif identifier.STRING():
                attribute_parts.append(identifier.STRING().getText().strip('"\''))

        if attribute_parts:
            # Extract entity (first part)
            self.entities_used.add(attribute_parts[0])

            # If nested, mark that we need nested access helper
            if len(attribute_parts) > 2:
                self.required_helpers.add('nested_attribute_access')

    def enterComparison(self, ctx):
        """
        Extract comparison expressions.
        Now handles arithmetic due to grammar enhancement!
        """
        condition_info = {
            'left': self._extract_expression(ctx.expression(0)),
            'operator': ctx.operator().getText(),
            'right': self._extract_expression(ctx.expression(1)),
        }

        # Check if expressions use arithmetic
        if self._has_arithmetic(ctx.expression(0)) or \
           self._has_arithmetic(ctx.expression(1)):
            self.required_helpers.add('arithmetic_evaluation')
            if self.rule_type == 'monetary':
                self.required_imports.append('java.math.BigDecimal')

        self.conditions.append(condition_info)

    def enterAction(self, ctx):
        """Extract action calls with parameters."""
        action_info = {
            'name': None,
            'parameters': []
        }

        # Get action name
        if ctx.IDENTIFIER():
            action_info['name'] = ctx.IDENTIFIER().getText()
        elif ctx.STRING():
            action_info['name'] = ctx.STRING().getText().strip('"\'')

        # Extract parameters if present
        if ctx.parameterList():
            for param in ctx.parameterList().parameter():
                param_expr = self._extract_expression(param.expression())
                action_info['parameters'].append(param_expr)

                # Check for arithmetic in parameters
                if self._has_arithmetic(param.expression()):
                    self.required_helpers.add('arithmetic_evaluation')

        self.actions.append(action_info)

    def _extract_expression(self, expr_ctx) -> Dict[str, Any]:
        """
        Extract expression structure (handles arithmetic).
        Returns structured representation for code generation.
        """
        if expr_ctx.term():  # This is an expression with potential arithmetic
            return {
                'type': 'arithmetic',
                'tree': expr_ctx,
                'text': expr_ctx.getText()
            }
        elif expr_ctx.factor():
            return self._extract_factor(expr_ctx.factor())
        else:
            return {'type': 'simple', 'text': expr_ctx.getText()}

    def _extract_factor(self, factor_ctx) -> Dict[str, Any]:
        """Extract factor (attribute, value, function call, etc.)"""
        if factor_ctx.attribute():
            return {
                'type': 'attribute',
                'path': self._get_attribute_path(factor_ctx.attribute())
            }
        elif factor_ctx.value():
            return {
                'type': 'literal',
                'value': factor_ctx.value().getText()
            }
        elif factor_ctx.functionCall():
            return {
                'type': 'function',
                'name': factor_ctx.functionCall().IDENTIFIER().getText(),
                'args': []  # Extract function args
            }
        else:
            return {'type': 'expression', 'text': factor_ctx.getText()}

    def _get_attribute_path(self, attr_ctx) -> List[str]:
        """Get full attribute path for nested attributes."""
        parts = []
        for identifier in attr_ctx.attributeIdentifier():
            if identifier.IDENTIFIER():
                parts.append(identifier.IDENTIFIER().getText())
            elif identifier.STRING():
                parts.append(identifier.STRING().getText().strip('"\''))
        return parts

    def _has_arithmetic(self, expr_ctx) -> bool:
        """Check if expression contains arithmetic operators."""
        text = expr_ctx.getText()
        return any(op in text for op in ['+', '-', '*', '/', '%'])
```

### **4.2 Java Code Visitors**

```python
# Visitor for converting conditions to Java
class ConditionToJavaVisitor:
    """
    Visits ANTLR parse tree nodes and converts to Java expressions.
    This is core ANTLR pattern you'll learn deeply.
    """

    def visit(self, ctx):
        """Dispatch to appropriate visit method."""
        if ctx.orExpression():
            return self.visit_or_expression(ctx.orExpression())
        elif ctx.andExpression():
            return self.visit_and_expression(ctx.andExpression())
        elif ctx.comparison():
            return self.visit_comparison(ctx.comparison())
        # ... more node types

    def visit_comparison(self, ctx):
        """
        Convert comparison to Java.
        Example: transaction.location.country != "US"
        → _getNestedField(transaction, "location", "country") != "US"
        """
        left = self.visit_expression(ctx.expression(0))
        operator = self._convert_operator(ctx.operator())
        right = self.visit_expression(ctx.expression(1))

        return f"{left} {operator} {right}"

    def visit_expression(self, ctx):
        """
        Handle arithmetic expressions with precedence.
        Example: amount * 0.025 + fee
        """
        if self._is_arithmetic(ctx):
            return self._visit_arithmetic(ctx)
        else:
            return self._visit_simple_expression(ctx)

    def _visit_arithmetic(self, ctx):
        """
        Convert arithmetic parse tree to Java.
        Respects precedence from grammar.
        """
        # Handle term + term - term pattern
        terms = []
        operators = []

        for i, term in enumerate(ctx.term()):
            terms.append(self.visit_term(term))

            # Get operator between terms
            if i < len(ctx.term()) - 1:
                if ctx.PLUS(i):
                    operators.append('+')
                elif ctx.MINUS(i):
                    operators.append('-')

        # Build Java expression
        java_expr = terms[0]
        for i, op in enumerate(operators):
            java_expr = f"({java_expr} {op} {terms[i+1]})"

        return java_expr

    def visit_term(self, ctx):
        """Handle multiplication/division (higher precedence)."""
        factors = []
        operators = []

        for i, factor in enumerate(ctx.factor()):
            factors.append(self.visit_factor(factor))

            if i < len(ctx.factor()) - 1:
                if ctx.MULTIPLY(i):
                    operators.append('*')
                elif ctx.DIVIDE(i):
                    operators.append('/')
                elif ctx.MODULO(i):
                    operators.append('%')

        java_expr = factors[0]
        for i, op in enumerate(operators):
            java_expr = f"({java_expr} {op} {factors[i+1]})"

        return java_expr

    def visit_factor(self, ctx):
        """Visit lowest level: attribute, value, function, or parenthesized expr."""
        if ctx.attribute():
            return self._visit_attribute(ctx.attribute())
        elif ctx.value():
            return self._visit_value(ctx.value())
        elif ctx.functionCall():
            return self._visit_function_call(ctx.functionCall())
        elif ctx.expression():
            return f"({self.visit_expression(ctx.expression())})"
        elif ctx.MINUS():
            # Unary minus
            return f"(-{self.visit_factor(ctx.factor())})"

    def _visit_attribute(self, ctx):
        """
        Convert attribute to Java field access.

        Simple: applicant.creditScore
        → _getFieldValue(applicant, "creditScore")

        Nested: transaction.location.country
        → _getNestedField(transaction, "location", "country")
        """
        parts = []
        for identifier in ctx.attributeIdentifier():
            if identifier.IDENTIFIER():
                parts.append(identifier.IDENTIFIER().getText())
            elif identifier.STRING():
                parts.append(identifier.STRING().getText().strip('"\''))

        if len(parts) == 2:
            # Simple: entity.field
            return f'_getFieldValue({parts[0]}, "{parts[1]}")'
        else:
            # Nested: entity.field1.field2...
            entity = parts[0]
            fields = '", "'.join(parts[1:])
            return f'_getNestedField({entity}, "{fields}")'

    def _visit_value(self, ctx):
        """Convert value literals."""
        if ctx.STRING():
            return ctx.STRING().getText()  # Keep quotes
        elif ctx.NUMBER():
            return ctx.NUMBER().getText()
        elif ctx.BOOLEAN():
            return ctx.BOOLEAN().getText()
        elif ctx.NULL():
            return 'null'
        elif ctx.list():
            values = [self._visit_value(v) for v in ctx.list().value()]
            return f"Arrays.asList({', '.join(values)})"

    def _visit_function_call(self, ctx):
        """Convert function calls to Java method calls."""
        func_name = ctx.IDENTIFIER().getText()

        if ctx.functionArgs():
            args = []
            for arg in ctx.functionArgs().expression():
                args.append(self.visit_expression(arg))
            args_str = ', '.join(args)
            return f'{func_name}({args_str})'
        else:
            return f'{func_name}()'
```

---

## 📖 Phase 5: Learning Resources & Practice (Days 10-12)

### **5.1 ANTLR Learning Path**

**Day 10: ANTLR Basics**
- Read: "The Definitive ANTLR 4 Reference" - Chapters 1-4
- Practice: Write simple grammar for calculator
- Exercise: Parse and evaluate `2 + 3 * 4`

**Day 11: Tree Walking**
- Learn: Listener vs Visitor patterns
- Practice: Walk parse tree and extract data
- Exercise: Extract all variables from arithmetic expression

**Day 12: Advanced Patterns**
- Learn: Left recursion, precedence, error recovery
- Practice: Add error messages to your grammar
- Exercise: Handle malformed rules gracefully

### **5.2 Hands-On Exercises**

**Exercise 1: Parse Tree Visualization**
```bash
# Generate parse tree image for your rule
java -jar antlr-4.13.1-complete.jar Rules.g4
javac Rules*.java
java org.antlr.v4.gui.TestRig Rules unifiedRule -gui < test_rule.txt
```

**Exercise 2: Custom Visitor**
Write a visitor that:
1. Counts all attribute references
2. Lists all entities used
3. Reports maximum nesting depth

**Exercise 3: Grammar Testing**
```python
# Test your enhanced grammar
test_rules = [
    'if amount * 0.025 > 10 then applyFee(amount * 0.025)',
    'if transaction.location.country != "US" then applyForeignFee',
    'if (price + tax) * quantity > 1000 then approveWithReview',
]

for rule in test_rules:
    parse_tree = parse(rule)
    print(f"Parsed: {rule}")
    print(f"Tree: {parse_tree.toStringTree()}")
```

---

## 🎯 Phase 6: Integration & Testing (Days 13-14)

### **6.1 Integration Plan**

```python
# backend/services/rule_service.py

from .template_code_generator import TemplateCodeGenerator

class RuleService:
    def __init__(self):
        # Keep existing generator for backward compatibility
        self.legacy_generator = AdvancedJavaCodeGenerator()

        # New template-based generator
        self.template_generator = TemplateCodeGenerator()

    def generate_rule_code(self, rule_content: str, rule_name: str,
                          rule_type: str, use_templates: bool = True):
        """
        Generate Java code for a rule.

        Args:
            use_templates: If True, use new template system.
                          If False, use legacy generator.
        """
        if use_templates:
            return self.template_generator.generate(
                rule_content, rule_name, rule_type
            )
        else:
            return self.legacy_generator.generate(rule_content, rule_name)
```

### **6.2 Testing Strategy**

**Test Suite**:
```python
# tests/test_template_generator.py

class TestTemplateGenerator:

    def test_standard_rule_generation(self):
        """Test simple if-then rules."""
        rule_content = '''
        rule rewardsPoints:
            if applicant.creditScore >= 700 then approveApplication
            if applicant.creditScore < 680 then rejectApplication
        '''

        java_code = generator.generate(rule_content, 'rewardsPoints', 'standard')

        # Verify generated code
        assert 'class RewardsPointsRule' in java_code
        assert 'creditScore >= 700' in java_code
        assert 'creditScore < 680' in java_code

    def test_nested_attributes(self):
        """Test nested attribute access."""
        rule_content = '''
        rule internationalCheck:
            if transaction.location.country != "US" then applyForeignFee
        '''

        java_code = generator.generate(rule_content, 'internationalCheck', 'standard')

        # Should use nested field access helper
        assert '_getNestedField(transaction, "location", "country")' in java_code

    def test_arithmetic_expressions(self):
        """Test arithmetic in conditions and actions."""
        rule_content = '''
        rule feeCalculation:
            if amount * 0.025 > 10 then applyFee(amount * 0.025)
        '''

        java_code = generator.generate(rule_content, 'feeCalculation', 'monetary')

        # Should generate arithmetic correctly
        assert '(amount * 0.025)' in java_code or \
               '_multiply(amount, 0.025)' in java_code  # Either direct or helper

    def test_complex_actionset(self):
        """Test ActionSet with nested if-then-else."""
        rule_content = '''
        rule rewardsSelection:
            if applicant.creditScore >= 720 then
                if applicant.annualIncome >= 75000 then premiumRewards
                else standardRewards
        '''

        java_code = generator.generate(rule_content, 'rewardsSelection', 'actionset')

        # Should preserve nested structure
        assert 'if' in java_code
        assert 'premiumRewards' in java_code
        assert 'standardRewards' in java_code

    def test_compilation(self):
        """Verify generated code compiles."""
        for rule_type in ['standard', 'actionset', 'monetary', 'non_monetary']:
            java_code = generator.generate(test_rules[rule_type], f'test_{rule_type}', rule_type)

            # Write to file
            java_file = Path(f'generated-rules/test_{rule_type}/TestRule.java')
            java_file.parent.mkdir(parents=True, exist_ok=True)
            java_file.write_text(java_code)

            # Compile
            result = subprocess.run(['javac', str(java_file)], capture_output=True)
            assert result.returncode == 0, f"Compilation failed: {result.stderr}"
```

---

## 📊 Success Metrics

### **Phase Completion Checklist**

**Phase 1: ANTLR Fundamentals** ✅
- [ ] Understand grammar structure
- [ ] Can read and modify `.g4` files
- [ ] Understand parse tree hierarchy

**Phase 2: Grammar Enhancements** ✅
- [ ] Added arithmetic expression support
- [ ] Tested with complex rules
- [ ] Parse tree visualizations work

**Phase 3: Template System** ✅
- [ ] Created all 4 rule type templates
- [ ] Template variables documented
- [ ] Reusable fragments identified

**Phase 4: Code Generator** ✅
- [ ] Template generator implemented
- [ ] ANTLR tree walking works
- [ ] All rule types generate code

**Phase 5: Learning** ✅
- [ ] Comfortable reading grammar files
- [ ] Can modify grammar independently
- [ ] Understand listener vs visitor

**Phase 6: Integration** ✅
- [ ] Integrated with existing system
- [ ] All tests pass
- [ ] Generated code compiles and runs

### **Quality Targets**

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| Compilation Success | 100% | 75% | 🟡 Improve |
| Logic Correctness | 95% | 34% | 🔴 Fix |
| Code Coverage | 80% | 0% | 🔴 Add tests |
| Rule Types Supported | 4/4 | 1/4 | 🔴 Enhance |

---

## 🚀 Quick Start Roadmap

### **Week 1: Grammar & Learning**
- **Day 1-2**: Study current grammar, understand structure
- **Day 3-4**: Implement arithmetic expressions in grammar
- **Day 5**: Test enhanced grammar with complex rules
- **Day 6-7**: Build confidence with ANTLR tree walking

### **Week 2: Templates & Integration**
- **Day 8-9**: Create template system and fragments
- **Day 10-11**: Implement template-based generator
- **Day 12**: Integration and backward compatibility
- **Day 13-14**: Testing, validation, documentation

---

## 📝 Next Steps

1. **Review this plan** - Does the approach make sense?
2. **Prioritize phases** - Do we start with grammar or templates?
3. **Set timeline** - How much time per day can you dedicate?
4. **Prepare environment** - Need any tools/libraries installed?

**I recommend starting with Phase 2 (Grammar Enhancements)** because:
- Your grammar is 80% there already
- Adding arithmetic is straightforward
- Enables all advanced features
- Deep learning about ANTLR immediately

Ready to start? Let me know which phase you want to dive into first!
