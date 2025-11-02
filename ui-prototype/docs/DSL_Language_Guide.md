# Rules Engine DSL Language Guide

**Version**: 1.0
**Last Updated**: 2025-10-23
**Audience**: Business Analysts, Rules Authors, Developers

---

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Language Syntax](#language-syntax)
4. [Data Types](#data-types)
5. [Operators](#operators)
6. [Expressions](#expressions)
7. [Functions](#functions)
8. [Actions](#actions)
9. [Complete Examples](#complete-examples)
10. [Best Practices](#best-practices)
11. [Common Patterns](#common-patterns)
12. [Troubleshooting](#troubleshooting)

---

## Introduction

The Rules Engine DSL (Domain-Specific Language) is designed for authoring credit card processing rules in a natural, readable syntax. Rules define conditional logic that drives business decisions such as application approvals, fraud detection, and limit adjustments.

### Key Features

- **Natural Language Syntax**: Easy to read and write for business users
- **Type Safety**: Strong typing with validation at authoring time
- **Composable**: Build complex rules from simple conditions
- **High Performance**: Compiled to Java bytecode (sub-millisecond execution)
- **Integration Ready**: Works with existing Java actions and systems

### Use Cases

- Credit card application decisioning
- Fraud detection and prevention
- Credit limit adjustments
- Transaction authorization
- Customer segmentation
- Promotional eligibility

---

## Getting Started

### Your First Rule

```
rule welcomeRule:
    if applicant.creditScore >= 700 then approveApplication endif
```

This simple rule:
- Is named `welcomeRule`
- Checks if credit score is 700 or higher
- Calls the `approveApplication` action if true
- **Note**: `endif` is required when using `if-then` constructs

### Rule Structure

Every rule follows this pattern:

```
rule <name>:
    <rule steps>
```

**Components**:
- `rule` keyword (required)
- Rule name (required, quoted or unquoted)
- `:` colon separator (required)
- One or more rule steps (required)

---

## Language Syntax

### Rule Definition

```antlr
rule ruleName:
    <conditional or action steps>
```

**Rule Names**:
- Unquoted: `rule myRule:` (letters, numbers, underscores)
- Double-quoted: `rule "My Complex Rule Name":` (any characters)
- Single-quoted: `rule 'Another Rule':` (any characters)

### Rule Steps

Rules can have two types of steps:

**1. Conditional Step** (if-then-else):
```
if <condition> then <actions>
elseif <condition> then <actions>  // optional, multiple allowed
else <actions>                      // optional
endif                               // REQUIRED when using if-then
```

**Important**: The `endif` keyword is **mandatory** when using conditional logic (`if-then-else`).

**2. Direct Action Step** (no conditionals):
```
<action>
```

Direct actions don't need `if-then-endif` - just the action itself.

**Examples showing the difference**:

```
// Conditional - REQUIRES endif
rule conditionalRule:
    if applicant.creditScore >= 700 then approve endif

// Direct action - NO endif needed
rule directActionRule:
    approve
```

### Multiple Actions

Actions can be chained with commas:

```
rule multiAction:
    if condition then action1, action2, action3 endif
```

### Comments

```
// This is a line comment
rule myRule:  // Comments can appear after code
    if condition then action endif  // Inline comments supported
```

---

## Data Types

### Primitive Types

| Type | Examples | Description |
|------|----------|-------------|
| **Number** | `100`, `3.14`, `-25.5` | Integers and decimals |
| **String** | `'approved'`, `'John Doe'` | Text values (single quotes) |
| **Boolean** | `true`, `false` | Logical values |
| **Null** | `null` | Absence of value |

### Complex Types

| Type | Example | Description |
|------|---------|-------------|
| **List** | `[1, 2, 3]`, `['a', 'b', 'c']` | Ordered collections |
| **Date/Time** | `2023-09-17`, `14:30:00` | Temporal values |

### Type Rules

- String literals use **single quotes**: `'value'`
- Attribute names use **double quotes** for special characters: `"attribute-name"`
- Numbers don't need quotes: `100`, `3.14`
- Booleans are lowercase: `true`, `false`

---

## Operators

### Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal to | `applicant.age == 21` |
| `!=` | Not equal to | `status != 'PENDING'` |
| `<` | Less than | `creditScore < 600` |
| `<=` | Less than or equal | `income <= 50000` |
| `>` | Greater than | `balance > 1000` |
| `>=` | Greater than or equal | `age >= 18` |

### Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `and` | Logical AND | `age >= 18 and income > 30000` |
| `or` | Logical OR | `vip == true or balance > 100000` |
| `not` | Logical NOT | `not suspended` |

**Precedence** (highest to lowest):
1. `not`
2. `and`
3. `or`

Use parentheses for clarity: `(a or b) and (c or d)`

### Special Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `in` | Value in list | `status in ['PENDING', 'REVIEW']` |
| `not_in` | Value not in list | `country not_in ['US', 'CA']` |
| `is_null` | Check if null | `middleName is_null` |
| `is_not_null` | Check if not null | `email is_not_null` |
| `contains` | String/list contains | `name contains 'Smith'` |
| `starts_with` | String starts with | `email starts_with 'admin'` |
| `ends_with` | String ends with | `domain ends_with '.com'` |
| `matches` | Regex pattern match | `phone matches '[0-9]{3}-[0-9]{4}'` |

### Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `income + bonus` |
| `-` | Subtraction | `limit - used` |
| `*` | Multiplication | `rate * amount` |
| `/` | Division | `total / count` |
| `%` | Modulo | `balance % 100` |

**Precedence**:
1. `*`, `/`, `%` (left to right)
2. `+`, `-` (left to right)

---

## Expressions

### Attribute Access

Access nested object properties with dot notation:

```
applicant.creditScore          // Simple attribute
applicant.address.zipCode      // Nested attribute
transaction.amount.usd         // Multiple levels
```

**Special Characters in Attribute Names**:

If attribute names contain special characters, use double quotes:

```
"annual-income"                // Hyphenated attribute
"credit_score"                 // Underscore attribute (can be unquoted too)
applicant."home-address"       // Nested with special chars
```

### Arithmetic Expressions

```
applicant.income + applicant.bonus                    // Addition
transaction.amount * 1.02                             // Multiplication
(applicant.income - applicant.expenses) / 12          // Complex calculation
```

### Parentheses for Grouping

```
(applicant.creditScore >= 700 and applicant.income > 50000) or applicant.vip == true
```

---

## Functions

### Built-in Functions

Functions provide advanced operations beyond simple operators.

#### Date/Time Functions

```
day_of_week(timestamp)          // Returns 1-7 (1=Monday, 7=Sunday)
hour_of_day(timestamp)          // Returns 0-23
month_of_year(date)             // Returns 1-12
year(date)                      // Returns year (e.g., 2023)
date_diff(date1, date2)         // Days between dates
add_days(date, days)            // Add days to date
```

**Example**:
```
rule weekendRule:
    if day_of_week(transaction.timestamp) >= 6 then applyWeekendRules endif
```

#### String Functions

```
upper(text)                     // Convert to uppercase
lower(text)                     // Convert to lowercase
trim(text)                      // Remove whitespace
length(text)                    // String length
substring(text, start, end)     // Extract substring
```

**Example**:
```
rule emailDomainCheck:
    if lower(applicant.email) ends_with '@company.com' then approveInternal endif
```

#### Math Functions

```
abs(number)                     // Absolute value
round(number)                   // Round to nearest integer
ceil(number)                    // Round up
floor(number)                   // Round down
min(num1, num2)                 // Minimum of two values
max(num1, num2)                 // Maximum of two values
```

**Example**:
```
rule limitCalculation:
    if round(applicant.income / 3) > 10000 then setHighLimit endif
```

#### Collection Functions

```
size(list)                      // Number of elements
contains(list, value)           // Check if list contains value
first(list)                     // First element
last(list)                      // Last element
```

**Example**:
```
rule multipleTransactions:
    if size(applicant.recentTransactions) > 5 then flagForReview endif
```

### Function Call Syntax

```
functionName(arg1, arg2, ...)
```

Arguments can be:
- Attributes: `day_of_week(transaction.timestamp)`
- Literals: `add_days(today, 30)`
- Expressions: `max(income * 0.3, 5000)`

---

## Actions

### What are Actions?

Actions are operations executed when rule conditions are met. They can be:
- Java methods (compiled classes)
- ActionSets (groups of actions defined as rules)
- External system calls

### Action Syntax

**Simple Action**:
```
approveApplication
```

**Action with Parameters**:
```
setCreditLimit(5000)
sendNotification('Application approved')
adjustLimit(applicant.income * 0.3, 10000)
```

**Multiple Actions**:
```
approveApplication, setCreditLimit(5000), sendNotification('approved')
```

### ActionSets

ActionSets are reusable groups of actions defined as rules:

**Define ActionSet**:
```
rule standardApproval:
    setCreditLimit(5000),
    sendNotification('Your application has been approved'),
    scheduleFollowup(30)
```

**Call ActionSet**:
```
rule highScoreApplicant:
    if applicant.creditScore >= 750 then standardApproval endif
```

### Action Parameters

Actions can accept:
- Literals: `setCreditLimit(5000)`
- Attributes: `setCreditLimit(applicant.requestedLimit)`
- Expressions: `setCreditLimit(applicant.income * 0.3)`
- Function results: `scheduleFollowup(add_days(today, 30))`

### Special Characters in Action Names

For actions with special characters, use double quotes:

```
"approve-application"()
"send_email_notification"('approved')
```

---

## Complete Examples

### Example 1: Simple Approval Rule

```
rule basicApproval:
    if applicant.creditScore >= 700 and applicant.income > 50000
    then approveApplication
    else rejectApplication
    endif
```

**What it does**:
- Checks credit score is 700 or higher
- Checks income is over $50,000
- Approves if both conditions are true
- Rejects otherwise
- **Note**: This example is correct - `endif` is present

### Example 2: Tiered Credit Limits

```
rule tieredCreditLimit:
    if applicant.creditScore >= 800 then setCreditLimit(15000)
    elseif applicant.creditScore >= 750 then setCreditLimit(10000)
    elseif applicant.creditScore >= 700 then setCreditLimit(7500)
    else setCreditLimit(5000)
    endif
```

**What it does**:
- Sets credit limit based on credit score tiers
- Higher scores get higher limits
- Falls through tiers until match found

### Example 3: Fraud Detection

```
rule fraudDetection:
    if transaction.amount > 5000
       and hour_of_day(transaction.timestamp) >= 22
       and transaction.location not_in applicant.normalLocations
    then flagForFraudReview, sendAlertToCustomer, temporaryHold
    endif
```

**What it does**:
- Detects suspicious transactions
- Checks for high amount late at night
- Verifies location is unusual
- Triggers multiple fraud prevention actions

### Example 4: Weekend Processing

```
rule weekendProcessing:
    if day_of_week(application.submittedDate) >= 6 then
        scheduleForMonday,
        sendDelayNotification('Your application will be processed on Monday')
    else
        processImmediately
    endif
```

**What it does**:
- Checks if application submitted on weekend
- Delays processing until Monday if needed
- Notifies customer of delay
- Processes immediately on weekdays

### Example 5: VIP Customer Treatment

```
rule vipCustomerApproval:
    if applicant.vipStatus == true
       and applicant.accountAge > 365
       and applicant.paymentHistory == 'EXCELLENT'
    then
        approveApplication,
        setCreditLimit(applicant.requestedLimit),
        waiveAnnualFee,
        assignPersonalBanker
    endif
```

**What it does**:
- Identifies VIP customers
- Checks account age and payment history
- Auto-approves with requested limit
- Provides VIP benefits

### Example 6: Complex Income Calculation

```
rule incomeBasedLimit:
    if (applicant.annualIncome + applicant.spouseIncome) / 12 > 5000
       and applicant.debtToIncome < 0.4
    then setCreditLimit(
        min(
            (applicant.annualIncome + applicant.spouseIncome) * 0.3,
            50000
        )
    )
    endif
```

**What it does**:
- Calculates combined household monthly income
- Checks debt-to-income ratio
- Sets limit to 30% of annual income
- Caps limit at $50,000

---

## Best Practices

### 1. Rule Naming

**Good**:
```
rule highValueCustomerApproval:        // Descriptive
rule fraudDetectionOvernight:          // Clear purpose
rule "Weekend Processing Logic":       // Quoted for spaces
```

**Avoid**:
```
rule rule1:                            // Not descriptive
rule a:                                // Too short
rule thisRuleChecksIfTheApplicantHasSufficientIncomeAndCreditScore:  // Too long
```

### 2. Condition Clarity

**Good**:
```
if applicant.creditScore >= 700
   and applicant.income > 50000
   and applicant.employmentStatus == 'EMPLOYED'
```

**Avoid**:
```
if applicant.creditScore >= 700 and applicant.income > 50000 and applicant.employmentStatus == 'EMPLOYED' and applicant.age >= 21 and applicant.residencyStatus == 'PERMANENT' and applicant.bankruptcyHistory == false
```

Use parentheses and line breaks for readability.

### 3. Magic Numbers

**Good**:
```
rule minimumCreditScoreCheck:
    if applicant.creditScore >= 700 then approve endif  // 700 is standard minimum
```

**Better with Documentation**:
```
// Minimum credit score threshold per policy P-2023-CR-01
rule minimumCreditScoreCheck:
    if applicant.creditScore >= 700 then approve endif
```

### 4. Consistent Formatting

```
rule standardFormat:
    if condition then
        action1,
        action2,
        action3
    elseif condition2 then
        action4
    else
        action5
    endif
```

### 5. Reuse ActionSets

Instead of duplicating actions:

```
rule approvalRule1:
    if condition1 then setCreditLimit(5000), sendEmail(), scheduleFollowup() endif

rule approvalRule2:
    if condition2 then setCreditLimit(5000), sendEmail(), scheduleFollowup() endif
```

Create an ActionSet:

```
rule standardApprovalActions:
    setCreditLimit(5000),
    sendEmail(),
    scheduleFollowup()

rule approvalRule1:
    if condition1 then standardApprovalActions endif

rule approvalRule2:
    if condition2 then standardApprovalActions endif
```

---

## Common Patterns

### Pattern 1: Range Checks

```
rule ageRangeCheck:
    if applicant.age >= 21 and applicant.age <= 65 then processApplication endif
```

### Pattern 2: List Membership

```
rule validStatus:
    if applicant.status in ['PENDING', 'REVIEW', 'APPROVED'] then continueProcessing endif
```

### Pattern 3: String Matching

```
rule emailValidation:
    if applicant.email matches '[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}'
    then acceptEmail
    else rejectInvalidEmail
    endif
```

### Pattern 4: Null Safety

```
rule nullSafeCheck:
    if applicant.middleName is_not_null
       and applicant.middleName != ''
    then useMiddleName
    endif
```

### Pattern 5: Date-Based Logic

```
rule businessHoursOnly:
    if hour_of_day(transaction.timestamp) >= 9
       and hour_of_day(transaction.timestamp) <= 17
       and day_of_week(transaction.timestamp) <= 5
    then processImmediately
    else queueForNextBusinessDay
    endif
```

### Pattern 6: Threshold Escalation

```
rule escalationThresholds:
    if transaction.amount > 100000 then seniorManagerApproval
    elseif transaction.amount > 50000 then managerApproval
    elseif transaction.amount > 10000 then supervisorApproval
    else autoApprove
    endif
```

---

## Troubleshooting

### Common Errors

#### 1. Missing Colon After Rule Name

**Error**: `Syntax error at 'if'`

**Wrong**:
```
rule myRule
    if condition then action endif
```

**Correct**:
```
rule myRule:
    if condition then action endif
```

#### 2. Wrong Quote Type for Strings

**Error**: `Unexpected token ""`

**Wrong**:
```
if status == "PENDING" then action endif  // Double quotes for literals
```

**Correct**:
```
if status == 'PENDING' then action endif  // Single quotes for literals
```

#### 3. Missing `endif`

**Error**: `Expected 'endif' at end of conditional`

**Wrong**:
```
rule myRule:
    if condition then action
    else otherAction
```

**Correct**:
```
rule myRule:
    if condition then action
    else otherAction
    endif
```

#### 4. Arithmetic Precedence

**Error**: Logic produces unexpected results

**Wrong (ambiguous)**:
```
if income + bonus * 1.2 > 50000 then approve endif  // Multiplication happens first
```

**Correct (explicit)**:
```
if (income + bonus) * 1.2 > 50000 then approve endif
```

#### 5. Action Not Found

**Error**: `Unknown action: approveApp`

**Cause**: Action name misspelled or action not defined

**Solution**: Verify action exists in Actions or ActionSets, check spelling

#### 6. Attribute Not Found

**Error**: `Unknown attribute: applicant.crediScore`

**Cause**: Typo in attribute name (should be `creditScore`)

**Solution**: Check attribute spelling in schema reference

---

## Next Steps

- **Backend Integration**: See [Backend Integration Guide](./Backend_Integration_Guide.md)
- **UI Usage**: See [UI User Guide](./UI_User_Guide.md)
- **Schema Reference**: Use Schema Viewer in UI to see available attributes and actions
- **Examples**: Browse existing rules in the system for real-world patterns

---

## Quick Reference Card

### Basic Syntax
```
rule name: <steps>
if condition then actions [elseif...] [else...] endif
action1, action2, action3
```

### Operators
- **Compare**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Logic**: `and`, `or`, `not`
- **Special**: `in`, `not_in`, `is_null`, `is_not_null`, `contains`, `starts_with`, `ends_with`, `matches`
- **Math**: `+`, `-`, `*`, `/`, `%`

### Data Types
- **String**: `'text'` (single quotes)
- **Number**: `42`, `3.14`
- **Boolean**: `true`, `false`
- **Null**: `null`
- **List**: `[1, 2, 3]`

### Functions
- **Date**: `day_of_week()`, `hour_of_day()`, `add_days()`
- **String**: `upper()`, `lower()`, `trim()`, `length()`
- **Math**: `abs()`, `round()`, `min()`, `max()`
- **Collection**: `size()`, `contains()`, `first()`, `last()`

---

**End of DSL Language Guide**
