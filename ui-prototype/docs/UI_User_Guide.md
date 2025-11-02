# Rules Engine UI User Guide

**Version**: 1.0
**Last Updated**: 2025-10-23
**Audience**: Business Analysts, Rules Authors, QA Testers

---

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [User Interface Overview](#user-interface-overview)
4. [Creating Rules](#creating-rules)
5. [Editing Rules](#editing-rules)
6. [Rule Validation](#rule-validation)
7. [Testing Rules](#testing-rules)
8. [Context Management](#context-management)
9. [Schema Reference](#schema-reference)
10. [Actions and ActionSets](#actions-and-actionsets)
11. [Production Deployment](#production-deployment)
12. [Tips and Best Practices](#tips-and-best-practices)
13. [Troubleshooting](#troubleshooting)

---

## Introduction

The Rules Engine UI is a web-based application for authoring, testing, and managing business rules for credit card processing. It provides an intuitive interface with:

- **Monaco Editor**: Professional code editor with syntax highlighting and autocomplete
- **Real-time Validation**: Instant feedback on rule syntax and logic
- **Context Testing**: Test rules against sample data
- **Schema Browser**: Explore available attributes and actions
- **Code Generation**: Generate production-ready Java code

### Who Should Use This Guide?

- **Business Analysts**: Define and document business logic
- **Rules Authors**: Write and test credit decisioning rules
- **QA Testers**: Validate rule behavior with test contexts
- **Product Owners**: Review and approve rule changes

---

## Getting Started

### Accessing the Application

1. **URL**: Open your web browser and navigate to `http://localhost:3000`
2. **Login**: (If authentication is enabled, enter credentials)
3. **Dashboard**: You'll see the Rules Dashboard with navigation on the left

### System Requirements

- **Browser**: Chrome 90+, Firefox 88+, Safari 14+, Edge 90+
- **Screen Resolution**: 1920x1080 or higher recommended
- **Internet Connection**: Required for initial load only (offline mode supported)

### First-Time Setup

1. **Check Backend Connection**:
   - Look for green status indicator in top-right corner
   - If red, verify backend is running at `http://localhost:5001`

2. **Explore Sample Rules**:
   - Click "Rules" in left navigation
   - Browse existing rules to understand patterns

3. **Review Schema**:
   - Open any rule
   - Click "Schema Reference" button
   - Familiarize yourself with available attributes

---

## User Interface Overview

### Main Navigation

```
┌──────────────────────────────────────────────────────────────┐
│  Rules Engine                             🟢 Connected        │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  📋 Rules                                                     │
│  🗂️  Process Areas                                           │
│  📊 Contexts                                                  │
│  📖 Schema                                                    │
│  ⚙️  Settings                                                │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

### Rules List View

The main Rules List displays all rules in a hierarchical tree structure:

**Columns**:
- **Name**: Rule identifier
- **Type**: Rule, Action, or ActionSet
- **Status**: DRAFT, VALID, PEND, SCHD, PROD
- **Process Area**: Organizational grouping
- **Last Updated**: Modification timestamp

**Actions**:
- **Edit** (pencil icon): Open rule editor
- **Delete** (trash icon): Remove rule
- **Clone** (copy icon): Duplicate rule

**Filters**:
- Process Area dropdown
- Status dropdown
- Search by name

### Rule Editor Layout

```
┌─────────────────────────────────────────────────────────────────┐
│  ← Back to Rules  |  Edit Rule: creditApprovalRule              │
│  [Save] [Validate] [Generate Code] [Test Code] [Schema]         │
├──────────────┬────────────────────────────┬─────────────────────┤
│              │                            │                     │
│  Rule Info   │    Monaco Editor           │  Context Panel      │
│              │                            │  (Toggle On/Off)    │
│  - Name      │  rule creditApprovalRule:  │                     │
│  - Desc      │      if applicant.credit   │  Context Data:      │
│  - Process   │         Score >= 700       │  - creditScore: 750 │
│  - Context   │      then approve          │  - income: 75000    │
│  - Status    │                            │                     │
│              │                            │                     │
│  Validation  │                            │                     │
│  ✅ Valid    │                            │                     │
│              │                            │                     │
└──────────────┴────────────────────────────┴─────────────────────┘
```

### Context Panel (Right Side)

When a context is selected, the Context Panel shows:
- **Context Name**: Selected test context
- **Context Data**: JSON view of test data
- **Attribute Values**: Quick reference while authoring
- **Toggle Button**: Show/hide panel for more editor space

---

## Creating Rules

### Step 1: Navigate to Rules

1. Click "Rules" in left navigation
2. Click "+ New Rule" button in top-right corner

### Step 2: Fill Rule Information

**Rule Information Panel** (left side):

1. **Rule Name**: Auto-extracted from rule content (shows as "(not parsed)" until rule is written)
2. **Description**: Brief explanation of rule purpose
3. **Process Area**: Select from dropdown (e.g., "Credit Decisioning")
4. **Test Context**: (Optional) Select test data for validation
5. **Status**: Leave as "DRAFT" for new rules

### Step 3: Write Rule Content

In the **Monaco Editor** (center):

1. Start typing your rule:
   ```
   rule myFirstRule:
       if applicant.creditScore >= 700 then approveApplication
   ```

2. **Use Autocomplete**:
   - Press `Ctrl+Space` (Windows/Linux) or `Cmd+Space` (Mac)
   - Select from suggested attributes and actions

3. **Syntax Highlighting**:
   - Keywords appear in **blue** (`rule`, `if`, `then`)
   - Attributes in **purple** (`applicant.creditScore`)
   - Actions in **green** (`approveApplication`)
   - Strings in **orange** (`'approved'`)

### Step 4: Validate

1. Click **[Validate]** button in header
2. Review validation results in left panel
3. Fix any errors highlighted in red in the editor

### Step 5: Save

1. Click **[Save Rule]** button
2. Wait for success message: "Rule created successfully"
3. Note the rule ID assigned

### Example: Creating a Simple Approval Rule

```
rule basicCreditApproval:
    if applicant.creditScore >= 700
       and applicant.income > 50000
    then approveApplication
    else rejectApplication
    endif
```

**Steps**:
1. Click "+ New Rule"
2. Select Process Area: "Credit Decisioning"
3. Enter Description: "Approve applications with good credit and income"
4. Type the rule in the editor (use autocomplete for attributes)
5. Click [Validate] - should show ✅ Valid
6. Click [Save Rule]
7. Success! Rule is created with status DRAFT

---

## Editing Rules

### Opening a Rule for Editing

**Method 1: From Rules List**
1. Click "Rules" in left navigation
2. Find your rule in the list
3. Click the pencil (edit) icon

**Method 2: From Tree Navigation**
1. Expand the organizational hierarchy in left panel
2. Click on a rule name
3. Rule opens in editor

### Making Changes

1. **Edit Rule Content**:
   - Click in the Monaco Editor
   - Make your changes
   - Syntax highlighting updates in real-time

2. **Change Status**:
   - Use Status dropdown in Rule Information panel
   - Status progression: DRAFT → VALID → PEND → SCHD → PROD

3. **Update Description**:
   - Edit in Rule Information panel
   - Useful for documentation

4. **Change Process Area**:
   - Select different area from dropdown
   - Affects organizational hierarchy

### Saving Changes

1. Click **[Update Rule]** button (replaces "Save Rule" for existing rules)
2. Confirm the success message
3. Changes are immediately reflected

### Version History

(Future Feature) The system will track:
- Who made changes
- When changes were made
- What was changed
- Previous versions for rollback

---

## Rule Validation

### Automatic Validation

The editor validates your rule automatically:
- **On Type**: Validation runs 1 second after you stop typing
- **On Save**: Validation runs before saving
- **On Demand**: Click [Validate] button anytime

### Validation Results

**Success (Green)**:
```
✅ Valid
Rule syntax is valid and ready for deployment
```

**Error (Red)**:
```
❌ Validation Error
Syntax error at line 2, column 10: Expected 'then' after condition

Grammar Errors:
- Missing 'endif' at line 5
```

**Warning (Yellow)**:
```
⚠️ Warnings
- Unknown action: unknownAction
- Unknown attribute: applicant.unknownField
```

### Understanding Validation Messages

#### Syntax Errors (Must Fix)

Red underlines in editor indicate syntax errors:

**Missing `endif`**:
```
rule myRule:
    if condition then action
    else otherAction
    // Missing endif here!
```

**Wrong Quote Type**:
```
rule myRule:
    if status == "PENDING" then action  // Should use single quotes
```

**Fix**: Change to `status == 'PENDING'`

#### Semantic Warnings (Should Review)

Yellow warnings indicate missing references:

**Unknown Attribute**:
```
⚠️ Unknown attribute: applicant.creditScor
```
**Fix**: Check spelling - should be `creditScore`

**Unknown Action**:
```
⚠️ Unknown action: approveApp
```
**Fix**: Check action name - should be `approveApplication`

### Validation Panel Features

**Missing Attributes**:
- Count of undefined attributes
- List of attribute names
- Suggestion: Check Schema Reference

**Missing Actions**:
- Count of undefined actions
- List of action names
- Suggestion: Create action or check spelling

### Inline Error Markers

**Red Squiggly Lines**: Syntax errors
- Hover over the line to see error details
- Click on the line to see fix suggestions

**Example**:
```
if applicant.creditScore >= 700 then approve
// Red underline under "approve" if action doesn't exist
```

---

## Testing Rules

### Overview

Testing validates that your rule produces correct results with sample data.

### Prerequisites

1. **Valid Rule**: Rule must pass syntax validation
2. **Test Context**: Select a context with sample data
3. **Status**: Rule status must be VALID or higher

### Creating a Test Context

**Method 1: From Rule Editor**
1. Click "Manage Contexts" link in Rule Information panel
2. Click "+ New Context" in modal
3. Fill in:
   - **Name**: Descriptive name (e.g., "high_credit_applicant")
   - **Description**: What this context represents
   - **Schema**: Select applicable schema
   - **Context Data**: JSON with attribute values

**Example Context**:
```json
{
  "applicant": {
    "creditScore": 750,
    "income": 75000,
    "age": 35,
    "employmentStatus": "EMPLOYED"
  },
  "transaction": {
    "amount": 2500,
    "timestamp": "2023-09-17T14:30:00"
  }
}
```

4. Click [Create Context]

**Method 2: From Contexts Page**
1. Click "Contexts" in left navigation
2. Click "+ New Context" button
3. Fill in details as above

### Running Tests

#### Visual Testing (Context Panel)

1. **Select Context**:
   - In Rule Information panel, select context from dropdown
   - Context Panel opens on right side automatically

2. **Review Context Data**:
   - See attribute values used for testing
   - Verify values match your test scenario

3. **Test Logic Mentally**:
   - Read through your rule
   - Trace execution with context values
   - Predict expected outcome

4. **Example**:
   ```
   Rule: if applicant.creditScore >= 700 then approve
   Context: creditScore = 750
   Expected: approve action should execute
   ```

#### Code Testing (Advanced)

For rules with status VALID or higher:

1. **Generate Code**:
   - Click [Generate Code] button
   - Wait for "Code Generated Successfully!" message
   - Note the output directory

2. **Test Code**:
   - Click [Test Code] button
   - System builds and runs Maven tests
   - View test results in modal

**Test Results Modal**:
```
✅ All Tests Passed!

Build: ✅ Success (1243ms)
Tests: ✅ All Passed (456ms)

Summary: 5 tests, 0 failures, 0 errors

📂 Directory: /path/to/generated-rules/rule-creditApprovalRule
```

### Test Scenarios

#### Scenario 1: Boundary Testing

Test values at the edges of conditions:

**Rule**:
```
if applicant.creditScore >= 700 then approve
```

**Test Contexts**:
- `creditScore = 699` → Expected: Reject
- `creditScore = 700` → Expected: Approve (boundary)
- `creditScore = 701` → Expected: Approve

#### Scenario 2: Multi-Condition Testing

Test all logical paths:

**Rule**:
```
if applicant.creditScore >= 750 then highLimit
elseif applicant.creditScore >= 700 then mediumLimit
else lowLimit
endif
```

**Test Contexts**:
- `creditScore = 800` → Expected: highLimit
- `creditScore = 725` → Expected: mediumLimit
- `creditScore = 650` → Expected: lowLimit

#### Scenario 3: Null Handling

Test with missing or null values:

**Rule**:
```
if applicant.middleName is_not_null then includeMiddleName
```

**Test Contexts**:
- `middleName = "James"` → Expected: includeMiddleName
- `middleName = null` → Expected: Skip action

---

## Context Management

### What are Contexts?

Contexts provide test data for rule validation. Each context represents a specific scenario with sample attribute values.

### Context Types

1. **Test Contexts**: Real test data for rule validation
2. **Schema Templates**: Define data structures without values

### Viewing Contexts

1. Click "Contexts" in left navigation
2. Browse available contexts
3. Click context name to view details

### Context Details

**Fields**:
- **Name**: Unique identifier
- **Description**: What scenario this represents
- **Schema**: Which schema definition it follows
- **Created**: Creation timestamp
- **Context Data**: JSON view of attributes

### Creating Contexts

See [Testing Rules → Creating a Test Context](#creating-a-test-context)

### Editing Contexts

1. Click on context in Contexts list
2. Edit JSON data in the editor
3. Click [Update Context]

### Deleting Contexts

1. Click on context in Contexts list
2. Click [Delete] button
3. Confirm deletion

**Warning**: Deleting a context affects rules using that context for testing.

### Context Best Practices

1. **Descriptive Names**: Use names that explain the scenario
   - Good: `high_income_excellent_credit`
   - Bad: `context1`

2. **Complete Data**: Include all attributes used in rules
   ```json
   {
     "applicant": {
       "creditScore": 750,
       "income": 75000,
       "age": 35,
       "employmentStatus": "EMPLOYED"
     }
   }
   ```

3. **Realistic Values**: Use values representative of real data
   - Credit scores: 300-850
   - Income: Realistic annual amounts
   - Dates: Proper ISO format

4. **Multiple Scenarios**: Create contexts for:
   - Happy path (everything valid)
   - Edge cases (boundary values)
   - Error cases (invalid/missing data)

---

## Schema Reference

### Accessing Schema Reference

1. Open any rule in the editor
2. Click **[Schema Reference]** button in header
3. Schema Viewer modal opens

### Schema Viewer Features

**Tabs**:
1. **Attributes**: Browse available data attributes
2. **Actions**: Browse available actions and ActionSets

**Search**:
- Type in search box to filter attributes/actions
- Search by name or description

**Entity Tree**:
- Attributes grouped by entity (e.g., "applicant", "transaction")
- Click entity to expand and see attributes

### Attribute Details

Click on an attribute to see:

**Basic Info**:
- **Name**: Full qualified name (e.g., `applicant.creditScore`)
- **Type**: Data type (integer, string, decimal, etc.)
- **Description**: What this attribute represents

**Validation Rules**:
- **Required**: Whether attribute must be present
- **Range**: Min/max values (for numeric types)
- **Pattern**: Regex pattern (for strings)
- **Default**: Default value if not provided

**Example**:
```
Name: applicant.creditScore
Type: integer
Description: Credit score from credit bureau (FICO)
Required: Yes
Range: 300 - 850
```

### Action Details

Click on an action to see:

**Basic Info**:
- **Name**: Action identifier
- **Type**: Action or ActionSet
- **Description**: What this action does

**Parameters** (if any):
- **Name**: Parameter name
- **Type**: Expected data type
- **Required**: Whether parameter is required

**Java Source** (for Actions):
- View the Java implementation
- See parameter handling
- Understand action behavior

**Rule Content** (for ActionSets):
- View the rule that defines the ActionSet
- See what actions are grouped together

### Using Schema Reference While Authoring

**Workflow**:
1. Open Schema Reference
2. Find the attribute you need
3. Copy the full name (e.g., `applicant.creditScore`)
4. Close Schema Reference modal
5. Paste attribute name in your rule
6. Autocomplete will recognize it

**Tip**: Keep Schema Reference open in a separate browser window for quick reference.

---

## Actions and ActionSets

### What are Actions?

Actions are operations executed when rule conditions are met. They represent business operations like:
- Approve or reject applications
- Set credit limits
- Send notifications
- Log audit trails

### Action Types

#### 1. Java Actions

Implemented as Java classes with business logic.

**Example**: `approveApplication`
- Java file: `ApprovalAction.java`
- Sets application status to APPROVED
- Calculates credit limit
- Sends approval notification

#### 2. ActionSets

Groups of actions defined as rules.

**Example**: `standardApprovalProcess`
```
rule standardApprovalProcess:
    approveApplication,
    setCreditLimit(5000),
    sendNotification('Application approved')
```

### Using Actions in Rules

**Simple Action**:
```
rule simpleRule:
    if condition then approveApplication
```

**Action with Parameters**:
```
rule parameterizedAction:
    if condition then setCreditLimit(10000)
```

**Multiple Actions**:
```
rule multipleActions:
    if condition then
        approveApplication,
        sendNotification('approved'),
        scheduleFollowup
```

**Calling ActionSets**:
```
rule callActionSet:
    if condition then standardApprovalProcess
```

### Creating Actions

#### Creating an ActionSet (UI)

1. Click "+ New Rule" button
2. **Important**: In URL or create dialog, select type "ActionSet"
3. Enter name in Name field (not parsed from content for ActionSets)
4. Write the action list:
   ```
   rule myActionSet:
       action1,
       action2,
       action3
   ```
5. Click [Save Rule]

#### Creating a Java Action (Advanced)

1. Create Java class in `backend/sample_java_actions/`
2. Implement action logic
3. Add action definition to schema
4. Restart backend
5. Action appears in autocomplete

**Example Java Action**:
```java
package com.creditcard.actions;

public class CustomApprovalAction {
    public void execute(Application application) {
        // Custom approval logic
        application.setStatus("APPROVED");
        // ... additional logic
    }
}
```

### Action Best Practices

1. **Reusable Actions**: Create ActionSets for repeated action patterns
2. **Descriptive Names**: Action names should describe what they do
3. **Parameter Clarity**: Document expected parameters
4. **Error Handling**: Actions should gracefully handle errors

---

## Production Deployment

### Deployment Workflow

```
DRAFT → Validate → VALID → Review → PEND → Schedule → SCHD → Deploy → PROD
```

### Status Progression

#### 1. DRAFT Status

**Purpose**: Initial authoring and testing

**Actions Available**:
- ✅ Edit rule content
- ✅ Validate syntax
- ✅ Save changes
- ❌ Generate code (disabled)
- ❌ Deploy to production (disabled)

**Next Step**: Validate and change status to VALID

#### 2. VALID Status

**Purpose**: Rule is syntactically correct and ready for review

**Actions Available**:
- ✅ Edit rule content
- ✅ Validate syntax
- ✅ Generate production code
- ✅ Test code with Maven
- ❌ Auto-deploy (disabled)

**Next Step**: Submit for approval (PEND)

#### 3. PEND Status (Pending Approval)

**Purpose**: Rule is awaiting review and approval

**Actions Available**:
- ⚠️ Edit rule content (with caution)
- ✅ Generate code
- ✅ Test code
- ❌ Auto-deploy (disabled)

**Next Step**: Approve and schedule (SCHD)

#### 4. SCHD Status (Scheduled)

**Purpose**: Rule approved and scheduled for deployment

**Actions Available**:
- ❌ Edit disabled (rule locked for deployment)
- ✅ Generate code
- ✅ Test code
- ⏳ Awaiting deployment window

**Next Step**: Deploy to production (PROD)

#### 5. PROD Status (Production)

**Purpose**: Rule is live in production

**Actions Available**:
- ❌ Edit disabled (create new version instead)
- ✅ View code
- ✅ Monitor execution
- ⚠️ Emergency deactivation only

**Next Step**: Create new version for changes

### Generating Production Code

#### Step 1: Prepare Rule

1. Ensure rule status is VALID or higher
2. Run [Validate] to confirm no errors
3. Test with multiple contexts

#### Step 2: Generate Code

1. Click **[Generate Code]** button
2. Wait for generation to complete (typically 1-2 seconds)
3. Review success modal:
   ```
   ✅ Code Generated Successfully!

   Rule ID: rule-12345
   Package: com.rules.creditapproval
   Files Generated: 3

   📂 Location: /generated-rules/rule-creditApprovalRule
   ```
4. Click [Done]

#### Step 3: Build and Test

1. Click **[Test Code]** button
2. System runs Maven build and tests
3. Review test results:
   - Build status (success/failure)
   - Test results (passed/failed)
   - Build and test logs

#### Step 4: Review Generated Files

Generated files location: `generated-rules/rule-{ruleName}/`

**Files Created**:
- `{RuleName}.java`: Java class with rule logic
- `pom.xml`: Maven build configuration
- `README.md`: Rule documentation

**Java Class Structure**:
```java
package com.rules.creditapproval;

public class CreditApprovalRule {
    public Map<String, Object> execute(Map<String, Object> context) {
        // Generated rule logic
    }
}
```

#### Step 5: Version Control

1. Navigate to generated rule directory
2. Review generated code
3. Commit to version control:
   ```bash
   git add generated-rules/rule-creditApprovalRule/
   git commit -m "Generated production code for creditApprovalRule"
   git push
   ```

### Deployment Checklist

Before deploying a rule to production:

- [ ] Rule syntax is valid (no errors)
- [ ] Rule tested with multiple contexts
- [ ] Code generated successfully
- [ ] Maven build passes
- [ ] Maven tests pass
- [ ] Code reviewed by peer
- [ ] Documentation updated
- [ ] Approval obtained
- [ ] Deployment window scheduled
- [ ] Rollback plan prepared

### Emergency Procedures

#### Deactivating a Production Rule

If a rule causes issues in production:

1. **Immediate Action**:
   - Change rule status from PROD to DRAFT
   - This prevents rule execution

2. **Notification**:
   - Alert relevant stakeholders
   - Document the issue

3. **Investigation**:
   - Review rule logic
   - Check recent changes
   - Analyze error logs

4. **Resolution**:
   - Fix the rule
   - Test thoroughly
   - Redeploy through normal workflow

---

## Tips and Best Practices

### Authoring Tips

#### 1. Use Autocomplete

**Trigger Autocomplete**:
- Press `Ctrl+Space` (Windows/Linux)
- Press `Cmd+Space` (Mac)
- Start typing and wait 1 second

**What You'll See**:
- Available attributes
- Available actions
- Keywords (if, then, else)
- Functions

**Example**:
Type `app` → Autocomplete suggests:
- `applicant.creditScore`
- `applicant.income`
- `applicant.age`
- `approveApplication` (action)

#### 2. Format for Readability

**Good Formatting**:
```
rule wellFormattedRule:
    if applicant.creditScore >= 750
       and applicant.income > 50000
       and applicant.age >= 21
    then
        approveApplication,
        setCreditLimit(15000),
        sendNotification('Approved with high limit')
    elseif applicant.creditScore >= 700
    then
        approveApplication,
        setCreditLimit(10000)
    else
        rejectApplication,
        sendNotification('Application declined')
    endif
```

**Poor Formatting**:
```
rule poorlyFormattedRule:
if applicant.creditScore>=750 and applicant.income>50000 and applicant.age>=21 then approveApplication,setCreditLimit(15000),sendNotification('Approved with high limit') elseif applicant.creditScore>=700 then approveApplication,setCreditLimit(10000) else rejectApplication,sendNotification('Application declined') endif
```

#### 3. Use Comments

```
// Credit score threshold per policy P-2023-CR-01
rule policyCompliantRule:
    if applicant.creditScore >= 700 then approve
```

#### 4. Test Incrementally

Don't write the entire rule at once:

**Step 1**: Write basic structure
```
rule myRule:
    if condition then action
```

**Step 2**: Add first condition
```
rule myRule:
    if applicant.creditScore >= 700 then action
```

**Step 3**: Add more conditions
```
rule myRule:
    if applicant.creditScore >= 700
       and applicant.income > 50000
    then action
```

**Step 4**: Add actions
```
rule myRule:
    if applicant.creditScore >= 700
       and applicant.income > 50000
    then approveApplication, setCreditLimit(10000)
```

Validate at each step!

### Context Panel Usage

#### When to Show Context Panel

**Show panel when**:
- Testing rule with specific data
- Need to reference attribute values while authoring
- Debugging unexpected rule behavior

**Hide panel when**:
- Authoring new rules (need more editor space)
- Reviewing rule logic without test data
- Working on complex multi-line rules

#### Toggle Shortcut

Panel visibility persists between sessions:
- Open once → Stays open in future sessions
- Close once → Stays closed in future sessions

### Schema Reference Strategies

#### Strategy 1: Keep Schema Reference Open

Open Schema Reference in a separate browser window:
1. Right-click [Schema Reference] button
2. Select "Open in new window"
3. Position window next to editor
4. Reference while authoring

#### Strategy 2: Use Autocomplete + Schema Reference

1. Use autocomplete for quick attribute names
2. Open Schema Reference for:
   - Attribute descriptions
   - Data types and ranges
   - Validation rules

#### Strategy 3: Print Quick Reference

1. Open Schema Reference
2. Print to PDF
3. Keep PDF open while authoring

### Keyboard Shortcuts

#### Monaco Editor

| Shortcut | Action |
|----------|--------|
| `Ctrl+Space` / `Cmd+Space` | Trigger autocomplete |
| `Ctrl+S` / `Cmd+S` | Save rule |
| `Ctrl+F` / `Cmd+F` | Find in editor |
| `Ctrl+H` / `Cmd+H` | Find and replace |
| `Ctrl+/` / `Cmd+/` | Toggle comment |
| `Alt+Up` / `Option+Up` | Move line up |
| `Alt+Down` / `Option+Down` | Move line down |
| `Ctrl+D` / `Cmd+D` | Add selection to next match |
| `F1` | Command palette |

#### Application

| Shortcut | Action |
|----------|--------|
| `Ctrl+K Ctrl+V` | Toggle validation panel |
| `Ctrl+K Ctrl+C` | Toggle context panel |
| `Esc` | Close modal |

---

## Troubleshooting

### Common Issues

#### Issue 1: Autocomplete Not Working

**Symptoms**:
- Press `Ctrl+Space`, nothing happens
- No suggestions appear while typing

**Solutions**:
1. **Check Backend Connection**: Look for green status indicator in top-right
2. **Refresh Page**: Press `F5` or `Ctrl+R`
3. **Clear Browser Cache**: `Ctrl+Shift+Delete` → Clear cache
4. **Verify Schema Loaded**: Open Schema Reference to confirm data loaded

#### Issue 2: Validation Errors Persist After Fix

**Symptoms**:
- Fixed syntax error
- Red underline still appears
- Validation panel shows old errors

**Solutions**:
1. **Re-run Validation**: Click [Validate] button manually
2. **Save and Reload**: Save rule, then reload page
3. **Check for Multiple Errors**: One visible error might hide others

#### Issue 3: Context Panel Not Showing Data

**Symptoms**:
- Selected context from dropdown
- Context Panel shows "Loading..." indefinitely

**Solutions**:
1. **Check Context Exists**: Go to Contexts page, verify context exists
2. **Verify Context Has Data**: Open context, check `context_data` field is not empty
3. **Backend Connection**: Ensure backend is running
4. **Try Different Context**: Select another context to test

#### Issue 4: Cannot Change Rule Status

**Symptoms**:
- Status dropdown disabled
- Cannot change from DRAFT to VALID

**Solutions**:
1. **Validate First**: Rule must pass validation before status change
2. **Check Permissions**: (If auth enabled) Verify you have edit permissions
3. **Rule Must Be Saved**: Save rule first, then change status

#### Issue 5: Generate Code Button Disabled

**Symptoms**:
- [Generate Code] button is grayed out
- Tooltip shows "VALID+ Only"

**Solutions**:
1. **Check Status**: Rule status must be VALID, PEND, SCHD, or PROD
2. **Save Rule First**: Must save rule before code generation
3. **Validate Rule**: Ensure rule has no syntax errors

### Error Messages

#### "Rule with this name already exists"

**Cause**: Rule name conflicts with existing rule

**Solution**:
- Choose a different rule name
- Check existing rules list
- Delete or rename conflicting rule

#### "Cyclic dependency detected"

**Cause**: Rules call each other in a loop (A calls B, B calls A)

**Solution**:
- Review rule dependencies
- Break the cycle by removing one call
- Restructure rule logic

**Example Error Modal**:
```
❌ Cyclic Rule Call Detected

Rule dependencies form a cycle, which is not allowed.

Cycle path: ruleA → ruleB → ruleC → ruleA

Direct dependencies: ruleB, ruleC

Solution: Remove one of the circular references to break the cycle.
```

#### "Failed to load full rule details"

**Cause**: Rule ID is invalid or rule was deleted

**Solution**:
- Return to Rules List
- Verify rule still exists
- Refresh page

#### "Context not found"

**Cause**: Selected context was deleted or renamed

**Solution**:
- Clear context selection
- Select a different context
- Create new context if needed

### Performance Issues

#### Slow Editor Loading

**Symptoms**:
- Editor takes >5 seconds to load
- Page feels sluggish

**Solutions**:
1. **Check Network**: Ensure stable internet connection
2. **Close Other Tabs**: Free up browser memory
3. **Update Browser**: Use latest version
4. **Disable Extensions**: Some extensions interfere with Monaco

#### Slow Validation

**Symptoms**:
- Validation takes >10 seconds
- "Validating..." spinner shows for long time

**Solutions**:
1. **Check Backend**: Backend might be under load
2. **Simplify Rule**: Very complex rules take longer to validate
3. **Check Network**: Slow connection to backend

### Browser Compatibility

#### Chrome/Edge (Recommended)

✅ Fully supported
- All features work
- Best performance
- Monaco editor runs smoothly

#### Firefox

✅ Supported
- All features work
- Slightly slower Monaco performance

#### Safari

⚠️ Mostly supported
- Some Monaco features limited
- Autocomplete works
- Update to latest version for best experience

#### Internet Explorer

❌ Not supported
- Use Chrome, Edge, or Firefox instead

---

## Additional Resources

### Quick Reference Card

**Rule Structure**:
```
rule name:
    if condition then actions
    [elseif condition then actions]
    [else actions]
    endif
```

**Common Operators**:
- Compare: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Logic: `and`, `or`, `not`
- Special: `in`, `contains`, `starts_with`, `ends_with`

**Example Rules**:
```
// Simple approval
rule approve:
    if applicant.creditScore >= 700 then approveApplication

// Tiered limits
rule tieredLimits:
    if applicant.creditScore >= 800 then setCreditLimit(15000)
    elseif applicant.creditScore >= 750 then setCreditLimit(10000)
    else setCreditLimit(5000)
    endif
```

### Getting Help

1. **Hover Tooltips**: Hover over validation errors for quick help
2. **Schema Reference**: Click [Schema Reference] for attribute/action details
3. **Example Rules**: Browse existing rules for patterns
4. **Documentation**: See [DSL Language Guide](./DSL_Language_Guide.md) for syntax details

### Related Guides

- **Language Guide**: [DSL Language Guide](./DSL_Language_Guide.md)
- **Backend Guide**: [Backend Integration Guide](./Backend_Integration_Guide.md)
- **API Reference**: See Backend Guide for API details

---

**End of UI User Guide**
