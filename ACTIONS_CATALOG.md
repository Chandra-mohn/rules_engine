# Actions Catalog - Credit Card Rules Engine

## Overview
This document catalogs all actions identified from the sample rules, organized by functional category. Each action receives the complete `RuleContext` containing all available data.

## Action Categories

### Application Processing Actions

#### `rejectApplication`
**Purpose**: Reject a credit card application
**Usage**: Used when applicant doesn't meet basic eligibility criteria
**Context Data Needed**: 
- `applicant.*` - for logging rejection reasons
- `system.timestamp` - for audit trail

#### `approveApplication` 
**Purpose**: Approve a credit card application with standard terms
**Usage**: Used for qualified applicants meeting standard criteria
**Context Data Needed**:
- `applicant.*` - for account setup
- `card.*` - for credit limit assignment

#### `instantApproval`
**Purpose**: Immediately approve application for high-quality applicants
**Usage**: Used for excellent credit score applicants with strong financials
**Context Data Needed**:
- `applicant.*` - for premium account setup
- `card.*` - for higher credit limits

#### `conditionalApproval`
**Purpose**: Approve application with conditions or restrictions
**Usage**: Used for medium-risk applicants requiring additional terms
**Context Data Needed**:
- `applicant.*` - for conditional terms setup
- `card.*` - for restricted credit limits

#### `manualReview`
**Purpose**: Route application for human review
**Usage**: Used for complex cases requiring manual assessment
**Context Data Needed**:
- `applicant.*` - for reviewer context
- `system.*` - for review queue assignment

#### `requireManualReview`
**Purpose**: Flag application for manual review (alias for manualReview)
**Usage**: Used when automated decision cannot be made
**Context Data Needed**: Same as `manualReview`

### Transaction Authorization Actions

#### `approveTransaction`
**Purpose**: Authorize the transaction to proceed
**Usage**: Used when transaction passes all validation checks
**Context Data Needed**:
- `transaction.*` - for authorization logging
- `card.*` - for balance updates
- `merchant.*` - for settlement processing

#### `blockTransaction`
**Purpose**: Block/decline the transaction immediately
**Usage**: Used for high-risk or invalid transactions
**Context Data Needed**:
- `transaction.*` - for decline reason logging
- `customer.*` - for notification preferences
- `card.*` - for fraud tracking

#### `decline`
**Purpose**: Decline the transaction (alias for blockTransaction)
**Usage**: Used when transaction cannot be processed
**Context Data Needed**: Same as `blockTransaction`

#### `temporaryBlock`
**Purpose**: Temporarily block card due to suspicious activity
**Usage**: Used when velocity or pattern anomalies detected
**Context Data Needed**:
- `card.*` - for block duration and settings
- `customer.*` - for notification and unblock procedures
- `transaction.*` - for block reason documentation

### Risk Management Actions

#### `flagForReview`
**Purpose**: Mark transaction for post-processing review
**Usage**: Used when transaction is suspicious but not immediately blocked
**Context Data Needed**:
- `transaction.*` - for review queue details
- `customer.*` - for risk profile assessment
- `merchant.*` - for merchant risk analysis

#### `requireVerification`
**Purpose**: Require additional customer verification
**Usage**: Used when merchant or transaction risk exceeds tolerance
**Context Data Needed**:
- `customer.*` - for verification method selection
- `transaction.*` - for verification context
- `merchant.*` - for risk justification

#### `requirePhoneVerification`
**Purpose**: Require phone-based customer verification
**Usage**: Used for high-value or unusual location transactions
**Context Data Needed**:
- `customer.*` - for phone number and preferences
- `transaction.*` - for verification details
- `card.*` - for account verification

#### `requireStepUpAuth`
**Purpose**: Require enhanced authentication (2FA, biometric, etc.)
**Usage**: Used for high-risk transactions requiring stronger auth
**Context Data Needed**:
- `customer.*` - for available auth methods
- `transaction.*` - for auth context
- `card.*` - for security settings

#### `requireAdditionalAuth`
**Purpose**: Require additional authentication (generic)
**Usage**: Used when standard auth is insufficient
**Context Data Needed**: Same as `requireStepUpAuth`

### Communication Actions

#### `sendAlert`
**Purpose**: Send alert notification to customer
**Usage**: Used for unusual but authorized transactions
**Context Data Needed**:
- `customer.*` - for notification preferences and contact info
- `transaction.*` - for alert content
- `merchant.*` - for transaction context

#### `sendSMSVerification`
**Purpose**: Send SMS verification code to customer
**Usage**: Used for international or high-risk transactions
**Context Data Needed**:
- `customer.*` - for phone number and SMS preferences
- `transaction.*` - for verification context
- `system.*` - for verification code generation

#### `sendRealTimeAlert`
**Purpose**: Send immediate real-time alert to customer
**Usage**: Used for behavioral anomalies or off-hours transactions
**Context Data Needed**:
- `customer.*` - for real-time notification channels
- `transaction.*` - for alert urgency and content
- `merchant.*` - for transaction details

### Account Management Actions

#### `increaseCreditLimit`
**Purpose**: Increase customer's credit limit
**Usage**: Used when customer qualifies for higher credit
**Context Data Needed**:
- `customer.*` - for new limit calculation
- `card.*` - for current limit and utilization
- `applicant.*` - for income verification

#### `decreaseCreditLimit`
**Purpose**: Decrease customer's credit limit
**Usage**: Used when customer's risk profile deteriorates
**Context Data Needed**:
- `customer.*` - for risk assessment
- `card.*` - for current utilization and new limit
- `transaction.*` - for recent activity patterns

## Action Implementation Pattern

### Interface Definition
```java
public interface Action {
    void execute(RuleContext context);
}
```

### Example Implementation
```java
public class ApproveTransactionAction implements Action {
    @Override
    public void execute(RuleContext context) {
        // Access any needed data from context
        String transactionId = (String) context.getValue("transaction.id");
        Double amount = (Double) context.getValue("transaction.amount");
        String customerId = (String) context.getValue("customer.id");
        
        // Execute business logic
        transactionService.approve(transactionId, amount, customerId);
        
        // Log action execution
        auditService.logAction("approveTransaction", context.getData());
    }
}
```

## Action Registry

### Registry Structure
```java
public class ActionRegistry {
    private Map<String, Action> actions = new HashMap<>();
    
    public void registerAction(String name, Action action) {
        actions.put(name, action);
    }
    
    public void executeAction(String actionName, RuleContext context) {
        Action action = actions.get(actionName);
        if (action != null) {
            action.execute(context);
        } else {
            throw new ActionNotFoundException("Action not found: " + actionName);
        }
    }
}
```

## Action Summary by Category

| Category | Actions | Count |
|----------|---------|-------|
| **Application Processing** | rejectApplication, approveApplication, instantApproval, conditionalApproval, manualReview, requireManualReview | 6 |
| **Transaction Authorization** | approveTransaction, blockTransaction, decline, temporaryBlock | 4 |
| **Risk Management** | flagForReview, requireVerification, requirePhoneVerification, requireStepUpAuth, requireAdditionalAuth | 5 |
| **Communication** | sendAlert, sendSMSVerification, sendRealTimeAlert | 3 |
| **Account Management** | increaseCreditLimit, decreaseCreditLimit | 2 |
| **Total** | | **20** |

## Implementation Notes

### Context Access Patterns
- All actions receive complete `RuleContext` with full JSON data
- Actions can access any attribute using `context.getValue("path.to.attribute")`
- No parameter passing required - actions determine what data they need
- Type casting required for specific data types

### Error Handling
- Actions should handle missing data gracefully
- Use default values for optional attributes
- Log errors for audit and debugging
- Fail fast for critical missing data

### Performance Considerations
- Actions should be stateless for thread safety
- Cache frequently used context values within action execution
- Minimize external service calls where possible
- Use async processing for non-critical operations (logging, notifications)