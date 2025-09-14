# ActionSets Enhancement Specification

## Document Overview
**Version**: 1.0
**Date**: September 13, 2025
**Status**: Planning Phase
**Author**: Rules Engine Team

This document specifies the major enhancement to the Rules Engine DSL to support ActionSets alongside Rules, providing a unified but user-segmented approach to **credit card domain** business logic definition.

---

## 1. Current State Analysis

### 1.1 Current Grammar Structure
```antlr
rule ruleName:
    if attribute operator value then action
    [logical operations: and/or/not]
    [else clause support]
```

### 1.2 Current Limitations
- **Single Action Per Condition**: Each `if-then` can only execute one action
- **No Action Reusability**: Complex action sequences must be repeated
- **No Nested Logic**: Limited conditional depth
- **Actions as Identifiers**: Actions are simple identifiers with no internal definition

### 1.3 Current Strengths to Preserve
- ✅ Business readable and declarative
- ✅ Domain-focused (credit card domain)
- ✅ Simple for business users
- ✅ Clear separation between conditions and actions

---

## 2. Enhancement Vision

### 2.1 Core Philosophy
- **Maintain DSL Nature**: Not a programming language, but enhanced business logic expression
- **User Segmentation**: Different capabilities for different user types
- **Unified Grammar**: Single underlying grammar supporting both Rules and ActionSets
- **Composition Over Complexity**: Reusable components rather than complex constructs

### 2.2 Key Enhancement Areas
1. **Multiple Actions Per Condition**: `if condition then action1, action2, action3`
2. **Nested Conditionals**: Support for `if-then-else` within actions
3. **ActionSet Definition**: Reusable action sequences with conditional logic
4. **ActionSet Invocation**: Rules and ActionSets can call other ActionSets

---

## 3. Unified Grammar Architecture

### 3.1 Grammar Structure
```antlr
dslFile:
    (actionSetDefinition | ruleDefinition)+

actionSetDefinition:
    ACTIONSET actionSetName COLON step+

ruleDefinition:
    RULE ruleName COLON step+

step:
    conditionalStep | unconditionalStep

conditionalStep:
    IF condition THEN stepAction (ELSE stepAction)?

unconditionalStep:
    stepAction

stepAction:
    actionCall | actionSetCall | multipleActions

multipleActions:
    stepAction (COMMA stepAction)*

condition:
    orExpression

actionCall:
    IDENTIFIER

actionSetCall:
    IDENTIFIER  // ActionSets called same as actions
```

### 3.2 Design Principles

#### 3.2.1 Rules as Subset of ActionSets
- Rules constrain ActionSets by typically starting with conditions
- ActionSets can start with unconditional actions or conditions
- Both compile to the same internal structure

#### 3.2.2 Unified Data Context (React-like State Management)
- **Single Global Context**: One data structure passed through execution
- **Mutable State**: Actions can modify the shared execution context
- **No Parameter Passing**: All data access through shared context
- **State Transparency**: All modifications visible to subsequent steps

```javascript
// Credit Card Domain Execution Context
const executionContext = {
    applicant: {
        creditScore: 720,
        age: 28,
        annualIncome: 85000,
        employmentStatus: "employed",
        employmentYears: 3,
        requestedCreditLimit: 15000,
        existingDebt: 25000,
        bankruptcyHistory: false
    },
    transaction: {
        amount: 150.00,
        merchantCategory: "5411", // Grocery stores
        location: "US-CA-San Francisco",
        timestamp: "2025-09-13T10:30:00Z",
        type: "purchase",
        isOnline: false
    },
    account: {
        currentBalance: 1250.00,
        creditLimit: 5000,
        availableCredit: 3750.00,
        paymentHistory: "excellent",
        accountAge: 24 // months
    },
    decisions: [],        // Credit decisions populate this
    flags: [],           // Risk flags, fraud alerts, etc.
    metadata: {          // Credit card processing metadata
        business_date: "2025-09-13",
        processing_center: "west_coast",
        risk_score: null  // Calculated during execution
    }
    // Actions read from and write to this structure
}
```

#### 3.2.3 Call Tree Management
- **Maximum Depth**: 32 levels of ActionSet calls
- **Cycle Prevention**: Static analysis to detect circular references
- **Call Stack Tracking**: Runtime depth monitoring

---

## 4. User Segmentation & Experience

### 4.1 User Types

#### 4.1.1 Business Users
- **Primary Role**: Define simple business rules
- **Capabilities**:
  - Create and edit Rules
  - Use existing ActionSets (but not edit them)
  - View ActionSet results but not internal logic
- **UI Experience**: Simplified, guided editor

#### 4.1.2 SuperUsers
- **Primary Role**: Define complex, reusable logic
- **Capabilities**:
  - Create and edit ActionSets
  - Create and edit Rules
  - Manage ActionSet library
  - Debug execution flows
- **UI Experience**: Full-featured editor

### 4.2 UI Architecture

#### 4.2.1 Navigation Hierarchy
```
📁 Client: ACME Bank
   📁 Group: Retail Banking
      📁 Area: Credit Cards
         📁 Rules
            📄 ageVerification
            📄 creditScoreCheck
            📄 incomeVerification
         📁 ActionSets
            📄 standardRejection
            📄 approvalWorkflow
            📄 manualReviewProcess
      📁 Area: Personal Loans
         📁 Rules
         📁 ActionSets
```

#### 4.2.2 Editor Differentiation
- **Mode Selection**: User selects Rule or ActionSet at creation
- **Context-Aware UI**: Different editor experiences based on selection
- **Permission-Based Access**: User role determines available actions

#### 4.2.3 No Smart Conversion
- **Decision**: No automatic Rule → ActionSet conversion
- **Rationale**: Keeps UI simple, maintains clear user responsibilities
- **Alternative**: Manual migration assistance if needed

---

## 5. Grammar Examples

### 5.1 Enhanced Rules
```
rule creditCardApplicationRule:
    if applicant.age < 18 then rejectApplication, logRejection, notifyApplicant
    if applicant.creditScore < 600 then standardCreditRejection
    if applicant.creditScore > 750 and applicant.annualIncome > 75000 then
        approveApplication, assignCreditLimit, sendWelcomePackage
    else requireCreditReview
```

### 5.2 ActionSet Definitions
```
ActionSet standardCreditRejection:
    logCreditDecision("rejected", "insufficient_creditworthiness")
    updateApplicantCreditRecord
    sendCreditRejectionLetter
    reportToCreditBureaus

ActionSet creditCardApprovalWorkflow:
    if applicant.requestedCreditLimit > 25000 then requireManagerApproval
    approveCreditCardApplication
    calculateInitialCreditLimit
    sendCreditCardWelcomePackage
    activateCreditCard
    if applicant.creditScore > 800 then offerPremiumCardBenefits

ActionSet creditReviewProcess:
    setCreditApplicationStatus("pending_review")
    assignToCreditAnalyst
    if applicant.hasExistingBankRelationship then expeditedCreditReview
    scheduleCreditFollowUp
    requestAdditionalIncomeDocuments
```

### 5.3 ActionSet Composition
```
ActionSet comprehensiveCreditCardLogic:
    validateCreditApplicationInfo
    performCreditScoreCheck
    if applicant.age < 18 then standardCreditRejection
    if applicant.creditScore > 750 and applicant.debtToIncomeRatio < 0.3 then creditCardApprovalWorkflow
    else creditReviewProcess
```

---

## 6. Implementation Strategy

### 6.1 Grammar Evolution
1. **Extend Current Grammar**: Add ActionSet constructs to existing Rules.g4
2. **Maintain Backward Compatibility**: Existing rules continue to work
3. **Unified Parsing**: Single parser handles both Rules and ActionSets
4. **Compilation Target**: Both compile to same internal representation

### 6.2 Backend Changes
- **New API Endpoints**: `/api/actionsets/` alongside `/api/rules/`
- **Unified Execution Engine**: Single engine processes both types
- **Call Stack Management**: Track execution depth and prevent cycles
- **State Management**: Implement React-like mutable context passing

### 6.3 Frontend Changes
- **Navigation Enhancement**: Add Rules/ActionSets nodes to hierarchy tree
- **Dual Editors**: Rule editor and ActionSet editor experiences
- **Permission Integration**: Role-based access to features
- **ActionSet Library**: Browse and select ActionSets for use in Rules

### 6.4 Database Schema
- **Unified Storage**: Store both Rules and ActionSets in same table with type field
- **Hierarchy Support**: Client → Group → Area → Rules/ActionSets
- **Dependency Tracking**: Track which Rules use which ActionSets

---

## 7. Execution Model

### 7.1 Execution Context
```javascript
class CreditCardExecutionContext {
    constructor(creditCardData) {
        this.applicant = creditCardData.applicant;
        this.transaction = creditCardData.transaction;
        this.account = creditCardData.account;
        this.creditDecisions = [];        // Credit-specific decisions
        this.riskFlags = [];             // Credit risk and fraud flags
        this.creditMetadata = {
            executionId: generateId(),
            startTime: new Date(),
            callStack: [],
            processingCenter: "credit_ops",
            regulatoryCompliance: true
        };
    }

    // Credit card domain-specific methods
    addCreditDecision(decision) { this.creditDecisions.push(decision); }
    setRiskFlag(flag) { this.riskFlags.push(flag); }
    updateCreditProfile(updates) { Object.assign(this.applicant, updates); }
    updateAccountLimits(limits) { Object.assign(this.account, limits); }
}
```

### 7.2 Call Stack Management
- **Maximum Depth**: 32 levels (configurable)
- **Cycle Detection**: Build call graph during parse phase
- **Runtime Tracking**: Track current call depth
- **Error Handling**: Clear error messages for depth/cycle violations

### 7.3 Action Resolution
- **Built-in Credit Actions**: `rejectApplication`, `approveApplication`, `assignCreditLimit`, `activateCreditCard`, etc.
- **ActionSet Calls**: Resolved dynamically during execution
- **Credit Domain Actions**: `checkCreditScore`, `calculateDebtToIncomeRatio`, `reportToCreditBureaus`, etc.

---

## 8. Migration Strategy

### 8.1 Backward Compatibility
- **Existing Rules**: Continue to work without modification
- **Gradual Adoption**: New features optional, not required
- **Migration Path**: Clear upgrade path for complex rules

### 8.2 Training & Adoption
- **SuperUser Training**: ActionSet creation and management
- **Business User Training**: Using ActionSets in Rules
- **Documentation**: Comprehensive examples and best practices

---

## 9. Success Criteria

### 9.1 Technical Goals
- ✅ Unified grammar supporting both Rules and ActionSets
- ✅ Cycle detection and depth limiting working
- ✅ Backward compatibility with existing rules
- ✅ Performance: No significant execution overhead

### 9.2 User Experience Goals
- ✅ Business users can easily use ActionSets without understanding internals
- ✅ SuperUsers can create reusable, complex logic
- ✅ Clear separation of concerns between user types
- ✅ Intuitive navigation and organization

### 9.3 Business Goals
- ✅ Reduced duplication of business logic
- ✅ Faster rule development through reusable components
- ✅ Better maintenance through centralized ActionSets
- ✅ Scalable organization for enterprise use

---

## 10. Future Considerations

### 10.1 Potential Enhancements (Not in Scope)
- **ActionSet Parameters**: Parameterized ActionSets
- **Versioning**: ActionSet version management
- **Testing Framework**: Built-in testing for ActionSets
- **Performance Optimization**: Compilation optimizations

### 10.2 Integration Points
- **External Systems**: ActionSets calling external APIs (future)
- **Monitoring**: ActionSet execution metrics and monitoring
- **Governance**: Approval workflows for ActionSet changes

---

## 11. Appendix

### 11.1 Current Grammar Reference
[Reference to existing Rules.g4 file]

### 11.2 Example Transformations
**Before (Current)**:
```
rule creditCardEligibilityCheck:
    if applicant.creditScore < 600 then rejectApplication
```

**After (Enhanced)**:
```
ActionSet creditRejectionProcess:
    rejectApplication
    logCreditDecision
    sendRejectionLetter
    updateCreditBureauRecord

rule creditCardEligibilityCheck:
    if applicant.creditScore < 600 then creditRejectionProcess
```

### 11.3 Glossary
- **ActionSet**: Reusable sequence of actions and conditional logic
- **Rule**: Business logic definition (subset of ActionSet functionality)
- **SuperUser**: Advanced user who can create ActionSets
- **Business User**: Standard user who creates Rules and uses ActionSets
- **Execution Context**: Shared data structure passed through rule/actionset execution
- **Call Stack**: Track of nested ActionSet calls during execution

---

**Document Status**: ✅ Planning Complete - Ready for Implementation Planning Phase