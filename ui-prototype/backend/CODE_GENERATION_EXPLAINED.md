# How Action Calls Connect to Generated Code

## The Core Issue You're Seeing

Your DSL rule contains action calls like:
```
approveTransaction(transaction.amount)
applyForeignExchangeFee(transaction.amount * 0.025)
updateAccountBalance(...)
declineTransaction("...")
```

But the generated Java code **DOES NOT** have actual implementations for these actions. Let me explain the architecture.

---

## Current Architecture: Action Registry Pattern

### What Actually Happens

The code generator (`template_code_generator.py`) **stores action calls as strings** in the `actions` list, not as executable code:

```java
// Generated code from template_code_generator.py:277-283
if (condition) {
    matched = true;
    actions.add("approveTransaction(500.00)");  // ← String representation!
    actions.add("applyForeignExchangeFee(12.50)");
    actions.add("updateAccountBalance(512.50)");
}
```

### Where Actions Are Stored

Looking at `standard_rule_template.py:106-117`:

```java
public static RuleResult evaluate(Map<String, Object> context) {
    List<String> actions = new ArrayList<>();  // ← Action strings stored here
    String finalAction = null;
    boolean matched = false;

    // ... rule logic adds strings to actions list ...

    return new RuleResult(matched, actions, finalAction);
    //                              ^^^^^^^
    // Returns list of action call STRINGS, not executed code
}
```

### What You Get Back

The `RuleResult` object contains:
- `boolean matched` - Did any condition match?
- `List<String> actions` - **Action call strings** like `["approveTransaction(500.00)", "applyForeignExchangeFee(12.50)"]`
- `String finalAction` - Final action to perform

---

## The Missing Piece: Action Execution Layer

### What's NOT in Your Codebase

You need an **Action Executor** that maps action strings to actual implementations:

```java
// THIS DOESN'T EXIST YET - You need to implement this
public class ActionExecutor {

    public static void execute(String actionCall, Map<String, Object> context) {
        // Parse action string: "approveTransaction(500.00)"
        String actionName = extractActionName(actionCall);  // "approveTransaction"
        Object[] params = extractParams(actionCall);         // [500.00]

        // Route to actual implementation
        switch (actionName) {
            case "approveTransaction":
                TransactionActions.approve((BigDecimal) params[0], context);
                break;
            case "applyForeignExchangeFee":
                TransactionActions.applyFee((BigDecimal) params[0], context);
                break;
            case "updateAccountBalance":
                AccountActions.updateBalance((BigDecimal) params[0], context);
                break;
            case "declineTransaction":
                TransactionActions.decline((String) params[0], context);
                break;
            default:
                throw new UnknownActionException(actionName);
        }
    }
}

// Actual business logic implementations
public class TransactionActions {
    public static void approve(BigDecimal amount, Map<String, Object> ctx) {
        // Real implementation: Update database, send notifications, etc.
        Transaction txn = (Transaction) ctx.get("transaction");
        txn.setStatus("APPROVED");
        txn.setApprovedAmount(amount);
        // ... database operations ...
    }

    public static void applyFee(BigDecimal feeAmount, Map<String, Object> ctx) {
        Transaction txn = (Transaction) ctx.get("transaction");
        txn.setFeeAmount(feeAmount);
        // ... database operations ...
    }
}
```

---

## Why The Generated Code Looks Broken

### Problem 1: Invalid Nested Class Syntax

The generated code you showed has:

```java
public static RuleResult evaluate(Map<String, Object> input, String currency) {
    try {
        // Generated rule logic with performance monitoring
        import java.math.BigDecimal;  // ← SYNTAX ERROR: can't import inside method

        public class GeneratedRule implements Rule {  // ← NESTED CLASS in wrong place
            // ...
        }
```

**This is a bug in code generation** - the template is inserting code in the wrong location.

### Problem 2: No Actual Rule Logic

The generated `execute()` method just returns:

```java
@Override
public RuleResult execute(RuleContext ctx) {
    return RuleResult.noMatch();  // ← NO LOGIC!
}
```

This means **the if-then-else conditions from your DSL are missing** from the generated code.

### Problem 3: Duplicate Methods

You have 7 identical `getTransactionAmount()` methods - clear code generation bug.

---

## What SHOULD Be Generated

### Expected Output Structure

```java
public class InternationalTransactionProcessingRule {

    public static class RuleResult {
        private final boolean matched;
        private final List<String> actions;  // Action strings
        // ...
    }

    public static RuleResult evaluate(Map<String, Object> context) {
        List<String> actions = new ArrayList<>();
        boolean matched = false;

        // Extract entities
        Map<String, Object> transaction = (Map<String, Object>) context.get("transaction");
        Map<String, Object> account = (Map<String, Object>) context.get("account");

        // *** THIS IS THE CRITICAL PART - YOUR DSL LOGIC ***
        String country = _getFieldValue(transaction, "location.country");
        double amount = _toNumber(_getFieldValue(transaction, "amount"));
        double intlLimit = _toNumber(_getFieldValue(account, "internationalLimit"));

        if (!_equals(country, "US") && amount <= intlLimit) {
            matched = true;
            // Store action CALLS as strings (not execute them!)
            actions.add("approveTransaction(" + amount + ")");
            actions.add("applyForeignExchangeFee(" + (amount * 0.025) + ")");
            actions.add("updateAccountBalance(" + (amount + amount * 0.025) + ")");

        } else if (!_equals(country, "US") && amount > intlLimit) {
            matched = true;
            actions.add("declineTransaction(\"International transaction limit exceeded\")");

        } else {
            matched = true;
            actions.add("approveTransaction(" + amount + ")");
        }

        return new RuleResult(matched, actions, null);
    }

    // Helper methods for safe field access and comparisons
    private static Object _getFieldValue(Map<String, Object> entity, String fieldName) { ... }
    private static boolean _equals(Object a, Object b) { ... }
    private static double _toNumber(Object obj) { ... }
}
```

---

## How It All Connects: Full Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ 1. DSL Rule Definition                                          │
│    rule "International Transaction Processing":                 │
│      if condition then approveTransaction(amount) endif         │
└────────────────────┬────────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────────┐
│ 2. ANTLR Parser (Python)                                        │
│    grammar_parser/template_code_generator.py                    │
│    - Parses DSL into parse tree                                 │
│    - RuleDataExtractor walks tree                               │
│    - Converts conditions to Java: if (!_equals(country, "US")) │
│    - Converts actions to strings: actions.add("approve(...)");  │
└────────────────────┬────────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────────┐
│ 3. Template Generation (Python f-strings)                       │
│    templates/java/standard_rule_template.py                     │
│    - generate_standard_rule() builds Java class                 │
│    - Embeds converted rule logic                                │
│    - Returns complete Java source code                          │
└────────────────────┬────────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────────┐
│ 4. Generated Java Class                                         │
│    InternationalTransactionProcessingRule.java                  │
│    - evaluate(context) → returns RuleResult                     │
│    - RuleResult contains List<String> actions                   │
│    - Actions are STRING representations, not executed           │
└────────────────────┬────────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────────┐
│ 5. Rule Execution (Your Application)                            │
│    RuleResult result = InternationalTransactionProcessingRule   │
│                        .evaluate(transactionContext);           │
│                                                                  │
│    if (result.isMatched()) {                                    │
│        for (String actionCall : result.getActions()) {          │
│            // *** YOU NEED TO IMPLEMENT THIS PART ***           │
│            ActionExecutor.execute(actionCall, context);         │
│        }                                                         │
│    }                                                             │
└────────────────────┬────────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────────┐
│ 6. Action Execution Layer (MISSING - You Must Implement)        │
│    ActionExecutor.execute("approveTransaction(500.00)", ctx)    │
│      ↓                                                           │
│    TransactionActions.approve(500.00, ctx)                      │
│      ↓                                                           │
│    // Actual business logic:                                    │
│    // - Update database                                         │
│    // - Send notifications                                      │
│    // - Log audit trail                                         │
│    // - Call external APIs                                      │
└─────────────────────────────────────────────────────────────────┘
```

---

## Why This Design?

### Separation of Concerns

1. **Rule Logic** (generated): Determines WHAT actions to perform
2. **Action Implementation** (hand-written): Determines HOW to perform them

### Flexibility

- Actions can have different implementations in different environments (dev/staging/prod)
- Same rule can trigger different backends (database, message queue, API calls)
- Action implementations can be versioned independently from rules

### Safety

- Rules cannot directly manipulate database or call external systems
- All actions go through controlled execution layer with:
  - Authorization checks
  - Transaction management
  - Error handling
  - Audit logging

---

## What You Need To Do

### Option 1: Implement Action Executor (Recommended)

Create a Java action execution framework:

```java
// 1. Define action interfaces
public interface Action {
    void execute(Map<String, Object> context, Object[] params);
}

// 2. Register action implementations
public class ActionRegistry {
    private static final Map<String, Action> actions = new HashMap<>();

    static {
        register("approveTransaction", new ApproveTransactionAction());
        register("applyForeignExchangeFee", new ApplyFeeAction());
        register("updateAccountBalance", new UpdateBalanceAction());
        register("declineTransaction", new DeclineTransactionAction());
    }

    public static void register(String name, Action action) {
        actions.put(name, action);
    }

    public static Action get(String name) {
        return actions.get(name);
    }
}

// 3. Parse and execute action strings
public class ActionExecutor {
    public static void execute(String actionCall, Map<String, Object> context) {
        String name = parseActionName(actionCall);
        Object[] params = parseParams(actionCall);

        Action action = ActionRegistry.get(name);
        if (action == null) {
            throw new UnknownActionException("Action not found: " + name);
        }

        action.execute(context, params);
    }
}
```

### Option 2: Generate Action Execution Code

Modify `template_code_generator.py` to generate actual method calls instead of strings:

```python
# In _convert_action_list() method (line 264):
def _convert_action_list(self, ctx):
    """Convert action list to actual Java method calls."""
    java_code = []
    for action_ctx in ctx.action():
        action_name = action_ctx.IDENTIFIER().getText()

        if action_ctx.parameterList():
            params = [self._convert_expression(p.expression())
                     for p in action_ctx.parameterList().parameter()]
            # Generate actual method call
            java_code.append(f"ActionExecutor.{action_name}({', '.join(params)}, context);")
        else:
            java_code.append(f"ActionExecutor.{action_name}(context);")

    return java_code
```

---

## Current Bug Analysis

The generated code you showed has **multiple serious issues**:

1. ✗ **Syntax errors**: Imports and classes in wrong locations
2. ✗ **Missing rule logic**: No if-then-else conditions translated
3. ✗ **Duplicate code**: 7 identical methods generated
4. ✗ **Empty execution**: Returns `noMatch()` with no processing

**Root cause**: Looks like code generation template integration is broken. The monetary rule template is being injected incorrectly into the standard template.

### Investigation Needed

Check `services/rule_service.py` line ~471 where it calls the code generator - it might be mixing templates incorrectly or using a different generator than `template_code_generator.py`.

---

## Summary

**Question**: "How are actions connected to generated code?"

**Answer**: They're **NOT connected yet**. The generated code only returns action call **strings** in a list. You must implement an **Action Execution Layer** that:
1. Parses action strings (`"approveTransaction(500.00)"`)
2. Routes to real implementations (`TransactionActions.approve()`)
3. Executes business logic (database updates, API calls, etc.)

The current generated code has **bugs preventing even the string generation from working correctly**, so that needs to be fixed first before implementing action execution.
