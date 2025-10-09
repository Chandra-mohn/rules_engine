# Generated Rule Code Structure Documentation

**Version**: 1.0
**Last Updated**: 2025-10-08
**Purpose**: Comprehensive documentation of the Java code structure generated from DSL rules

---

## Table of Contents

1. [Overview](#overview)
2. [Universal Code Template](#universal-code-template)
3. [Structural Components](#structural-components)
4. [Rule Type Variations](#rule-type-variations)
5. [Helper Methods](#helper-methods)
6. [Code Generation Patterns](#code-generation-patterns)
7. [Examples by Type](#examples-by-type)
8. [Design Principles](#design-principles)

---

## Overview

### What is Generated?

The Rules Engine DSL compiler transforms business rules written in a domain-specific language into **production-ready Java classes**. Each generated class:

- ✅ Is **self-contained** with no external dependencies (except Java standard library)
- ✅ Is **stateless** and **thread-safe** (all methods are static)
- ✅ Follows a **consistent structure** across all rule types
- ✅ Includes **defensive programming** (null-safe, type-safe operations)
- ✅ Provides **uniform API** via `RuleResult` return type

### Generation Process

```
DSL Rule Text
      ↓
ANTLR Parser (Parse Tree)
      ↓
Template Code Generator
      ↓
Java Source Code
      ↓
Java Compiler
      ↓
Executable Bytecode
```

---

## Universal Code Template

Every generated rule follows this exact structure:

```java
package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * {Rule Display Name}
 * Generated from DSL rule definition
 * Performance Category: {hot|warm|cold}
 * Complexity Score: {0-10}/10
 */
public class {RuleName}Rule {

    // ═══════════════════════════════════════════════════════════
    // SECTION 1: RESULT CONTAINER (Identical in all rules)
    // ═══════════════════════════════════════════════════════════

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

    // ═══════════════════════════════════════════════════════════
    // SECTION 2: MAIN EVALUATION METHOD
    // ═══════════════════════════════════════════════════════════

    public static RuleResult evaluate(Map<String, Object> context) {
        // A. INITIALIZATION
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = false;

        // B. ENTITY EXTRACTION
        Map<String, Object> entity1 = (Map<String, Object>) context.get("entity1");
        Map<String, Object> entity2 = (Map<String, Object>) context.get("entity2");
        // ... more entities as referenced in DSL

        // C. RULE LOGIC (varies by rule type - see sections below)
        {RULE_TYPE_SPECIFIC_LOGIC}

        // D. RETURN RESULT
        return new RuleResult(matched, actions, finalAction);
    }

    // ═══════════════════════════════════════════════════════════
    // SECTION 3: HELPER METHODS (Identical in all rules)
    // ═══════════════════════════════════════════════════════════

    private static Object _getFieldValue(Map<String, Object> entity, String fieldName) {
        return entity != null ? entity.get(fieldName) : null;
    }

    private static boolean _equals(Object a, Object b) {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        return a.toString().equals(b.toString());
    }

    private static int _compareTo(Object a, Object b) {
        if (a == null || b == null) return 0;
        try {
            if (a instanceof Number && b instanceof Number) {
                return Double.compare(((Number)a).doubleValue(), ((Number)b).doubleValue());
            }
            return a.toString().compareTo(b.toString());
        } catch (Exception e) {
            return 0;
        }
    }

    private static double _toNumber(Object obj) {
        if (obj == null) return 0.0;
        if (obj instanceof Number) return ((Number)obj).doubleValue();
        try {
            return Double.parseDouble(obj.toString());
        } catch (NumberFormatException e) {
            return 0.0;
        }
    }
}
```

---

## Structural Components

### 1. Package and Imports

```java
package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
```

**Purpose**: Standard Java setup for rule classes

| Element | Value | Notes |
|---------|-------|-------|
| Package | `com.rules` | Fixed for all generated rules |
| Imports | Standard Java libraries | Same set for all rules |
| Dependencies | None (self-contained) | No external frameworks required |

---

### 2. Class Declaration and Metadata

```java
/**
 * {Rule Display Name}
 * Generated from DSL rule definition
 * Performance Category: {hot|warm|cold}
 * Complexity Score: {0-10}/10
 */
public class {RuleName}Rule {
```

**Generated Metadata:**

| Field | Source | Purpose |
|-------|--------|---------|
| Display Name | DSL rule name | Human-readable identifier |
| Performance Category | Calculated from complexity | `hot` (simple), `warm` (moderate), `cold` (complex) |
| Complexity Score | Analyzed from parse tree | 0-10 scale based on conditions, nesting, operators |

**Naming Convention:**
- DSL: `"Monthly Fee Application"`
- Class: `MonthlyFeeApplicationRule`
- Pattern: PascalCase + "Rule" suffix

---

### 3. RuleResult Inner Class

```java
public static class RuleResult {
    private final boolean matched;
    private final List<String> actions;
    private final String finalAction;

    // Constructor and getters...
}
```

**Purpose**: Immutable container for rule evaluation results

| Field | Type | Meaning |
|-------|------|---------|
| `matched` | boolean | Did the rule fire? |
| `actions` | List<String> | Actions to execute (in order) |
| `finalAction` | String | Single decisive action (optional, often null) |

**Why Immutable?**
- Thread-safe by design
- Prevents accidental modification
- Clear contract: results don't change after creation

**Usage Pattern:**
```java
RuleResult result = SomeRule.evaluate(context);

if (result.isMatched()) {
    for (String action : result.getActions()) {
        // Execute each action
        executeAction(action);
    }
}
```

---

### 4. Main Evaluation Method

#### Method Signature (Universal)

```java
public static RuleResult evaluate(Map<String, Object> context)
```

**Why Static?**
- No instance state needed
- Simple invocation: `RuleName.evaluate(context)`
- Thread-safe by default
- Lightweight execution

**Input: Context Map**

```java
Map<String, Object> context = new HashMap<>();
context.put("transaction", transactionData);
context.put("account", accountData);
context.put("customer", customerData);
```

The context is a **dynamic map** containing all entities needed for rule evaluation.

**Structure:**
```
{
    "entityName1": {
        "field1": value1,
        "field2": value2,
        "nested": {
            "field3": value3
        }
    },
    "entityName2": { ... }
}
```

#### Evaluation Flow

```java
public static RuleResult evaluate(Map<String, Object> context) {
    // ──────────────────────────────────────────────────────
    // PHASE 1: INITIALIZATION
    // ──────────────────────────────────────────────────────
    List<String> actions = new ArrayList<>();
    String finalAction = null;
    boolean matched = false;

    // ──────────────────────────────────────────────────────
    // PHASE 2: ENTITY EXTRACTION
    // ──────────────────────────────────────────────────────
    // Extract entities referenced in the DSL rule
    Map<String, Object> account = (Map<String, Object>) context.get("account");
    Map<String, Object> transaction = (Map<String, Object>) context.get("transaction");

    // ──────────────────────────────────────────────────────
    // PHASE 3: RULE LOGIC (varies by type)
    // ──────────────────────────────────────────────────────
    // See "Rule Type Variations" section

    // ──────────────────────────────────────────────────────
    // PHASE 4: RETURN RESULT
    // ──────────────────────────────────────────────────────
    return new RuleResult(matched, actions, finalAction);
}
```

---

## Rule Type Variations

The **ONLY** difference between generated rules is the logic structure in Phase 3.

### Type 1: Standard Rule (IF/ELSE IF/ELSE)

**DSL Pattern:**
```
rule "Rule Name":
    if {condition1} then
        {actions}
    elseif {condition2} then
        {actions}
    else
        {actions}
    endif
```

**Generated Logic:**
```java
// Standard rule - first matching branch wins
if ({CONDITION_1}) {
    matched = true;
    actions.add("action1(param1)");
    actions.add("action2(param2)");
} else if ({CONDITION_2}) {
    matched = true;
    actions.add("action3(param3)");
} else {
    matched = true;
    actions.add("defaultAction()");
}
```

**Characteristics:**
- ✅ **Mutually Exclusive**: Only ONE branch executes
- ✅ **Short-Circuit**: Stops after first match
- ✅ **Matched Flag**: Set in executed branch only
- ✅ **Ordered Evaluation**: Top to bottom

**Example:**
```java
if ((_equals(_getFieldValue(account, "tier"), "PREMIUM")) &&
    (_compareTo(_getFieldValue(account, "monthlySpend"), 1000) < 0)) {
    matched = true;
    actions.add("applyMonthlyFee(25.00)");
    actions.add("notifyCustomer(\"Premium account low spending\")");
} else if ((_equals(_getFieldValue(account, "tier"), "STANDARD")) &&
           (_compareTo(_getFieldValue(account, "balance"), 500) < 0)) {
    matched = true;
    actions.add("applyMonthlyFee(10.00)");
} else {
    matched = true;
    actions.add("waiveFee(\"Threshold met\")");
}
```

---

### Type 2: ActionSet (Accumulative Evaluation)

**DSL Pattern:**
```
actionset "ActionSet Name":
    if {condition1} then {action1}
    if {condition2} then {action2}
    if {condition3} then {action3}
end
```

**Generated Logic:**
```java
// ActionSet - ALL conditions evaluated independently
if ({CONDITION_1}) {
    actions.add("action1(param1)");
}
if ({CONDITION_2}) {
    actions.add("action2(param2)");
}
if ({CONDITION_3}) {
    actions.add("action3(param3)");
}

// Set matched if any action was added
matched = !actions.isEmpty();
```

**Characteristics:**
- ✅ **Independent Evaluation**: All conditions checked
- ✅ **Accumulative**: Multiple actions can execute
- ✅ **No Short-Circuit**: All IFs evaluated
- ✅ **Matched Logic**: True if ANY action added

**Example:**
```java
// Check high-risk indicators
if (_compareTo(_getFieldValue(transaction, "amount"), 10000) >= 0) {
    actions.add("flagHighValue()");
}
if (_equals(_getFieldValue(transaction, "country"), "SANCTIONED")) {
    actions.add("flagSanctionedCountry()");
}
if (_compareTo(_getFieldValue(customer, "riskScore"), 80) >= 0) {
    actions.add("escalateToManager()");
}

matched = !actions.isEmpty();  // Matched if any flag triggered
```

---

### Type 3: Direct Action (Unconditional)

**DSL Pattern:**
```
action "Action Name":
    {action1},
    {action2}
end
```

**Generated Logic:**
```java
// Direct action - always executes
matched = true;
actions.add("action1(param1)");
actions.add("action2(param2)");
```

**Characteristics:**
- ✅ **No Conditions**: Always executes
- ✅ **Always Matches**: `matched = true`
- ✅ **Simple Execution**: Just action registration
- ✅ **Use Cases**: Logging, auditing, default behaviors

**Example:**
```java
// Unconditional audit logging
matched = true;
actions.add("logTransaction(transaction.id)");
actions.add("updateMetrics(\"transaction_count\")");
actions.add("notifyMonitoring(\"transaction_processed\")");
```

---

## Helper Methods

All generated rules include these four helper methods for safe operations.

### 1. `_getFieldValue()` - Null-Safe Field Access

```java
private static Object _getFieldValue(Map<String, Object> entity, String fieldName) {
    return entity != null ? entity.get(fieldName) : null;
}
```

**Purpose**: Safely retrieve field values without NullPointerException

**Usage in Generated Code:**
```java
// DSL: account.tier
// Java: _getFieldValue(account, "tier")

// Handles:
// - Null entity → returns null
// - Missing field → returns null
// - Valid field → returns value
```

**Why Not Direct Access?**
```java
// UNSAFE (crashes if account is null):
account.get("tier")

// SAFE (returns null if account is null):
_getFieldValue(account, "tier")
```

---

### 2. `_equals()` - Null-Safe Equality Check

```java
private static boolean _equals(Object a, Object b) {
    if (a == null && b == null) return true;
    if (a == null || b == null) return false;
    return a.toString().equals(b.toString());
}
```

**Purpose**: Compare values safely with null handling

**Truth Table:**

| a | b | Result | Reason |
|---|---|--------|--------|
| null | null | true | Both null = equal |
| null | "X" | false | One null = not equal |
| "X" | null | false | One null = not equal |
| "X" | "X" | true | String equality |
| "X" | "Y" | false | Different strings |

**Usage in Generated Code:**
```java
// DSL: account.tier == "PREMIUM"
// Java: _equals(_getFieldValue(account, "tier"), "PREMIUM")

// DSL: status != "ACTIVE"
// Java: !_equals(_getFieldValue(entity, "status"), "ACTIVE")
```

**Why String Comparison?**
- Works with any object type (flexible)
- Database values often come as strings
- Consistent behavior across types

---

### 3. `_compareTo()` - Type-Safe Numeric Comparison

```java
private static int _compareTo(Object a, Object b) {
    if (a == null || b == null) return 0;
    try {
        if (a instanceof Number && b instanceof Number) {
            return Double.compare(((Number)a).doubleValue(), ((Number)b).doubleValue());
        }
        return a.toString().compareTo(b.toString());
    } catch (Exception e) {
        return 0;
    }
}
```

**Purpose**: Compare values numerically when possible, safely handle types

**Return Values:**
- Negative (< 0): a is less than b
- Zero (= 0): a equals b or error/null
- Positive (> 0): a is greater than b

**Usage in Generated Code:**
```java
// DSL: amount >= 1000
// Java: _compareTo(_getFieldValue(entity, "amount"), 1000) >= 0

// DSL: balance < 500
// Java: _compareTo(_getFieldValue(entity, "balance"), 500) < 0
```

**Type Handling:**
```java
// Both numbers → numeric comparison
_compareTo(750.0, 1000)    // Returns negative (750 < 1000)
_compareTo(1500, 1000)     // Returns positive (1500 > 1000)

// Not numbers → string comparison
_compareTo("apple", "banana")  // Alphabetical

// Null values → returns 0 (safe default)
_compareTo(null, 1000)     // Returns 0 (no crash)
```

---

### 4. `_toNumber()` - Null-Safe Numeric Conversion

```java
private static double _toNumber(Object obj) {
    if (obj == null) return 0.0;
    if (obj instanceof Number) return ((Number)obj).doubleValue();
    try {
        return Double.parseDouble(obj.toString());
    } catch (NumberFormatException e) {
        return 0.0;
    }
}
```

**Purpose**: Convert values to numbers for arithmetic operations

**Usage:** Currently **not used** in generated code but available for future enhancements (arithmetic expressions in DSL).

**Conversion Examples:**
```java
_toNumber(null)      // → 0.0
_toNumber(42)        // → 42.0
_toNumber(3.14)      // → 3.14
_toNumber("123.45")  // → 123.45
_toNumber("invalid") // → 0.0 (safe fallback)
```

---

## Code Generation Patterns

### Pattern 1: Entity Extraction

**Rule:** ONE line of extraction per entity referenced in DSL

**DSL:**
```
rule "Transaction Check":
    if transaction.amount > 1000 and account.status == "ACTIVE" then
        ...
```

**Generated:**
```java
Map<String, Object> transaction = (Map<String, Object>) context.get("transaction");
Map<String, Object> account = (Map<String, Object>) context.get("account");
```

**Naming:** Entity names in generated code match DSL exactly

---

### Pattern 2: Condition Translation

**DSL Operator → Java Code Mapping:**

| DSL Operator | Generated Java Code | Helper Used |
|--------------|---------------------|-------------|
| `==` | `_equals(a, b)` | `_equals()` |
| `!=` | `!_equals(a, b)` | `_equals()` |
| `<` | `_compareTo(a, b) < 0` | `_compareTo()` |
| `<=` | `_compareTo(a, b) <= 0` | `_compareTo()` |
| `>` | `_compareTo(a, b) > 0` | `_compareTo()` |
| `>=` | `_compareTo(a, b) >= 0` | `_compareTo()` |
| `and` | `&&` | - |
| `or` | `\|\|` | - |

**Examples:**

```
DSL:  account.tier == "PREMIUM"
Java: _equals(_getFieldValue(account, "tier"), "PREMIUM")

DSL:  transaction.amount >= 1000
Java: _compareTo(_getFieldValue(transaction, "amount"), 1000) >= 0

DSL:  status != "INACTIVE"
Java: !_equals(_getFieldValue(entity, "status"), "INACTIVE")

DSL:  score > 75 and score <= 100
Java: (_compareTo(_getFieldValue(entity, "score"), 75) > 0) &&
      (_compareTo(_getFieldValue(entity, "score"), 100) <= 0)
```

---

### Pattern 3: Action Translation

**DSL Action → Java Code Mapping:**

```
DSL:  actionName(param1, param2)
Java: actions.add("actionName(" + param1 + ", " + param2 + ")");
```

**String Parameters:** Escaped and quoted
```
DSL:  notifyCustomer("Welcome message")
Java: actions.add("notifyCustomer(" + "\"Welcome message\"" + ")");
```

**Numeric Parameters:** Converted to strings
```
DSL:  applyFee(25.00)
Java: actions.add("applyFee(" + "25.00" + ")");
```

**Field References:** Evaluated and embedded
```
DSL:  sendEmail(customer.email)
Java: actions.add("sendEmail(" + _getFieldValue(customer, "email") + ")");
```

---

## Examples by Type

### Example 1: Standard Rule - Credit Card Approval

**DSL:**
```
rule "Credit Card Approval":
    if applicant.creditScore >= 750 and applicant.income >= 50000 then
        approve("PLATINUM"),
        setCreditLimit(15000)
    elseif applicant.creditScore >= 650 and applicant.income >= 30000 then
        approve("GOLD"),
        setCreditLimit(7500)
    else
        reject("Insufficient qualifications")
    endif
```

**Generated Java:**
```java
public class CreditCardApprovalRule {

    public static class RuleResult { /* ... standard ... */ }

    public static RuleResult evaluate(Map<String, Object> context) {
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = false;

        Map<String, Object> applicant = (Map<String, Object>) context.get("applicant");

        if ((_compareTo(_getFieldValue(applicant, "creditScore"), 750) >= 0) &&
            (_compareTo(_getFieldValue(applicant, "income"), 50000) >= 0)) {
            matched = true;
            actions.add("approve(\"PLATINUM\")");
            actions.add("setCreditLimit(15000)");
        } else if ((_compareTo(_getFieldValue(applicant, "creditScore"), 650) >= 0) &&
                   (_compareTo(_getFieldValue(applicant, "income"), 30000) >= 0)) {
            matched = true;
            actions.add("approve(\"GOLD\")");
            actions.add("setCreditLimit(7500)");
        } else {
            matched = true;
            actions.add("reject(\"Insufficient qualifications\")");
        }

        return new RuleResult(matched, actions, finalAction);
    }

    // ... helper methods ...
}
```

---

### Example 2: ActionSet - Fraud Detection

**DSL:**
```
actionset "Fraud Indicators":
    if transaction.amount > 10000 then flagHighValue()
    if transaction.country == "RESTRICTED" then flagRestrictedCountry()
    if customer.accountAge < 30 then flagNewAccount()
end
```

**Generated Java:**
```java
public class FraudIndicatorsActionSet {

    public static class RuleResult { /* ... standard ... */ }

    public static RuleResult evaluate(Map<String, Object> context) {
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = false;

        Map<String, Object> transaction = (Map<String, Object>) context.get("transaction");
        Map<String, Object> customer = (Map<String, Object>) context.get("customer");

        if (_compareTo(_getFieldValue(transaction, "amount"), 10000) > 0) {
            actions.add("flagHighValue()");
        }
        if (_equals(_getFieldValue(transaction, "country"), "RESTRICTED")) {
            actions.add("flagRestrictedCountry()");
        }
        if (_compareTo(_getFieldValue(customer, "accountAge"), 30) < 0) {
            actions.add("flagNewAccount()");
        }

        matched = !actions.isEmpty();

        return new RuleResult(matched, actions, finalAction);
    }

    // ... helper methods ...
}
```

---

### Example 3: Direct Action - Audit Logging

**DSL:**
```
action "Transaction Audit":
    logTransaction(transaction.id),
    updateMetrics("processed"),
    notifyMonitoring()
end
```

**Generated Java:**
```java
public class TransactionAuditAction {

    public static class RuleResult { /* ... standard ... */ }

    public static RuleResult evaluate(Map<String, Object> context) {
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = false;

        Map<String, Object> transaction = (Map<String, Object>) context.get("transaction");

        matched = true;
        actions.add("logTransaction(" + _getFieldValue(transaction, "id") + ")");
        actions.add("updateMetrics(\"processed\")");
        actions.add("notifyMonitoring()");

        return new RuleResult(matched, actions, finalAction);
    }

    // ... helper methods ...
}
```

---

## Design Principles

### 1. Consistency

**Every generated rule follows the exact same structure:**
- ✅ Same package and imports
- ✅ Same `RuleResult` inner class
- ✅ Same `evaluate()` method signature
- ✅ Same helper methods

**Benefits:**
- Predictable code structure
- Easy to understand and debug
- Simple to test systematically
- Uniform integration points

---

### 2. Safety First (Defensive Programming)

**All operations are protected:**
- ✅ **Null-safe**: No NullPointerException thrown
- ✅ **Type-safe**: Type mismatches handled gracefully
- ✅ **Exception-safe**: Try-catch blocks prevent crashes

**Safety Mechanisms:**
```java
// Field access: Checks entity != null
_getFieldValue(entity, "field")

// Equality: Handles null on both sides
_equals(a, b)

// Comparison: Returns 0 on null/error
_compareTo(a, b)

// Conversion: Returns 0.0 on invalid input
_toNumber(obj)
```

---

### 3. Immutability

**RuleResult is immutable:**
```java
public static class RuleResult {
    private final boolean matched;        // final = can't change
    private final List<String> actions;   // final = can't change
    private final String finalAction;     // final = can't change

    // No setters!
}
```

**Benefits:**
- Thread-safe without synchronization
- Prevents accidental modification
- Clear contract with callers
- Easier to reason about behavior

---

### 4. Statelessness

**No instance state:**
```java
// All methods are static
public static RuleResult evaluate(Map<String, Object> context)

// No instance variables
// No constructors needed
```

**Benefits:**
- Thread-safe by default
- No object lifecycle management
- Simple invocation pattern
- Efficient memory usage

---

### 5. Performance Optimization

**Design for speed:**
- ✅ **No reflection**: Direct field access via helpers
- ✅ **Minimal objects**: Only ArrayList and RuleResult created
- ✅ **Short-circuit**: Standard rules stop at first match
- ✅ **JIT-friendly**: Simple branching, easy to optimize

**Performance Characteristics:**
- **Execution time**: Sub-millisecond (< 1ms)
- **Memory footprint**: ~2KB per evaluation
- **Thread safety**: Completely safe
- **Scalability**: Handles thousands of evaluations/second

---

### 6. Testability

**Generated code is highly testable:**

```java
// Simple to test - just create context and call evaluate()
@Test
void testPremiumAccountLowSpending() {
    // Arrange
    Map<String, Object> context = new HashMap<>();
    Map<String, Object> account = new HashMap<>();
    account.put("tier", "PREMIUM");
    account.put("monthlySpend", 750.0);
    context.put("account", account);

    // Act
    RuleResult result = MonthlyFeeApplicationRule.evaluate(context);

    // Assert
    assertTrue(result.isMatched());
    assertEquals(2, result.getActions().size());
    assertEquals("applyMonthlyFee(25.00)", result.getActions().get(0));
}
```

**Testing Benefits:**
- Pure functions (same input → same output)
- No mocking needed
- Fast test execution
- Easy to create test data

---

## Summary

### Code Structure at a Glance

```
┌─────────────────────────────────────────────────────────┐
│ STRUCTURE (Every Generated Rule)                        │
├─────────────────────────────────────────────────────────┤
│                                                         │
│ 1. Package & Imports (Fixed)                            │
│    - package com.rules;                                 │
│    - Standard Java imports                              │
│                                                         │
│ 2. Class & Metadata (Dynamic)                           │
│    - JavaDoc with complexity/performance                │
│    - Class name from rule name                          │
│                                                         │
│ 3. RuleResult Inner Class (Fixed)                       │
│    - boolean matched                                    │
│    - List<String> actions                               │
│    - String finalAction                                 │
│                                                         │
│ 4. evaluate() Method (Structured)                       │
│    - Initialize variables                               │
│    - Extract entities (dynamic)                         │
│    - Rule logic (varies by type)                        │
│    - Return RuleResult                                  │
│                                                         │
│ 5. Helper Methods (Fixed)                               │
│    - _getFieldValue()                                   │
│    - _equals()                                          │
│    - _compareTo()                                       │
│    - _toNumber()                                        │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### Key Takeaways

1. **Consistent Structure**: All rules follow identical template
2. **Type-Specific Logic**: Only rule logic varies (IF/ELSE vs ActionSet vs Direct)
3. **Safety First**: Null-safe, type-safe, exception-safe operations
4. **Stateless Design**: Thread-safe, no instance state
5. **Immutable Results**: Thread-safe, predictable behavior
6. **High Performance**: Sub-millisecond execution, minimal overhead
7. **Easy Testing**: Pure functions, simple test setup

---

**Document Version**: 1.0
**Generated**: 2025-10-08
**Maintained By**: Rules Engine Development Team
