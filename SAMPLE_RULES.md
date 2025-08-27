# Sample Rules - Credit Card Domain

This document contains sample rules of varying complexity for a credit card processing system.

## Simple Rules (Basic if-then logic)

### Rule 1: Basic Age Verification
```
rule ageVerification:
    if applicant.age < 18 then rejectApplication
```

### Rule 2: Income vs Expenses Check
```
rule incomeExpenseCheck:
    if applicant.annualIncome < applicant.monthlyExpenses * 12 then rejectApplication
```

### Rule 3: Credit Limit Check
```
rule creditLimitCheck:
    if transaction.amount > card.availableBalance then decline
```

## Medium Rules (Multiple conditions with AND/OR)

### Rule 4: Debt to Income Ratio
```
rule debtToIncomeCheck:
    if applicant.totalDebt > applicant.annualIncome * 0.4 then requireManualReview
    if applicant.creditScore >= 700 and applicant.annualIncome > applicant.requestedCreditLimit * 3 then approveApplication
```

### Rule 5: Behavioral Pattern Analysis
```
rule behavioralPatterns:
    if transaction.amount > customer.averageTransactionAmount * 5 then flagForReview
    if transaction.location != customer.homeLocation and transaction.amount > customer.travelThreshold then sendAlert
```

### Rule 6: Risk Tolerance Matching
```
rule riskToleranceCheck:
    if merchant.riskScore > customer.riskTolerance then requireVerification
    if transaction.timestamp > card.lastUsedTimestamp + 86400 and transaction.amount > customer.dormancyThreshold then requireStepUpAuth
```

## High Complexity Rules (Complex boolean logic with grouping)

### Rule 7: Advanced Fraud Detection
```
rule advancedFraudDetection:
    if (transaction.amount > customer.monthlySpendingLimit and transaction.merchantCategory != customer.frequentCategory) 
       or (merchant.riskScore > customer.acceptableRiskLevel and transaction.amount > customer.riskThreshold) then flagForReview
    if transaction.amount > customer.averageTransaction * 10 
       and transaction.location != customer.usualLocation then requirePhoneVerification
```

### Rule 8: Dynamic Credit Limit Management
```
rule dynamicCreditLimitManagement:
    if (customer.currentBalance < customer.creditLimit * 0.2 and customer.currentIncome > customer.originalIncome * 1.2) 
       or (customer.paymentScore > customer.baselinePaymentScore * 1.5 and customer.tenure > 24) then increaseCreditLimit
    if customer.currentBalance > customer.creditLimit * 0.95 
       and (customer.missedPayments > customer.allowedMissedPayments or customer.currentIncome < customer.minimumIncomeThreshold) then decreaseCreditLimit
```

## Very High Complexity Rules (Multiple steps with complex nested conditions)

### Rule 9: Comprehensive Application Processing
```
rule comprehensiveApplicationProcessing:
    # Initial eligibility check
    if applicant.age < 18 or applicant.annualIncome < 15000 then rejectApplication
    
    # Credit score based processing
    if applicant.creditScore >= 750 
       and (applicant.annualIncome > applicant.requestedCreditLimit * 4 or applicant.employmentType = "government") 
       and not applicant.bankruptcyHistory then instantApproval
    
    # Medium risk assessment
    if (applicant.creditScore between 650 and 749) 
       and (applicant.totalDebt < applicant.annualIncome * 0.4) 
       and (applicant.employmentTenure >= 24 or applicant.homeOwnership) then conditionalApproval
    
    # High risk but salvageable
    if (applicant.creditScore between 580 and 649) 
       and (applicant.annualIncome > applicant.requestedCreditLimit * 2) 
       and (applicant.existingRelationship or applicant.collateralValue > applicant.requestedCreditLimit) 
       and not (applicant.recentInquiries > applicant.maxAllowedInquiries or applicant.bankruptcyHistory) then manualReview
    
    # Default rejection for remaining cases
    if applicant.creditScore < 580 then rejectApplication
```

### Rule 10: Real-time Transaction Authorization
```
rule realtimeTransactionAuth:
    # Immediate blocks
    if card.status = "blocked" or card.status = "stolen" or card.status = "expired" then blockTransaction
    
    # Amount based checks
    if transaction.amount > card.dailyLimit then blockTransaction
    if transaction.amount > customer.comfortableSpendingLimit 
       and not (merchant.category = "hospital" or merchant.category = "emergency") then blockTransaction
    
    # Velocity and pattern analysis
    if (transaction.count1h > customer.maxHourlyTransactions and transaction.amount1h > customer.hourlySpendingLimit) 
       or (transaction.consecutiveDeclines >= customer.maxConsecutiveDeclines and transaction.timeSinceLastDecline < customer.cooldownPeriod) then temporaryBlock
    
    # Geographic and merchant risk
    if (transaction.country != customer.homeCountry 
        and not customer.travelNotification 
        and transaction.amount > customer.internationalTransactionThreshold) 
       or (merchant.fraudScore > customer.merchantRiskTolerance 
           and customer.riskProfile != "low" 
           and transaction.amount > customer.averageTransaction * customer.riskMultiplier) then requireStepUpAuth
    
    # Behavioral analysis
    if (transaction.time not between customer.usualTransactionStartHour and customer.usualTransactionEndHour 
        and transaction.amount > customer.offHoursSpendingLimit) 
       or (merchant.category not in customer.frequentCategories 
           and transaction.amount > customer.newMerchantThreshold 
           and customer.newMerchantRiskTolerance = "low") then sendRealTimeAlert
    
    # Final approval for normal transactions
    if transaction.amount <= card.availableCredit 
       and merchant.trustScore >= customer.minimumMerchantTrustScore 
       and transaction.riskScore <= customer.acceptableTransactionRisk then approveTransaction
```

## Rule Complexity Analysis

| Complexity | Characteristics | Example Features |
|------------|----------------|------------------|
| **Simple** | Single condition, basic operators | Age checks, simple thresholds |
| **Medium** | Multiple conditions, AND/OR logic | Credit approval, velocity checks |
| **High** | Complex boolean expressions, grouping | Fraud detection, risk assessment |
| **Very High** | Multiple steps, nested conditions, comprehensive logic | Full application processing, real-time authorization |

## Domain Attributes Used

### Applicant/Customer Attributes
- `applicant.age`, `applicant.annualIncome`, `applicant.creditScore`
- `customer.creditUtilization`, `customer.paymentHistory`, `customer.vipStatus`
- `customer.transactionHistory`, `customer.averageTransaction`

### Card Attributes  
- `card.expirationDate`, `card.homeCountry`, `card.status`
- `card.dailyLimit`, `card.singleTransactionLimit`, `card.availableCredit`

### Transaction Attributes
- `transaction.amount`, `transaction.country`, `transaction.time`
- `transaction.count24h`, `transaction.amount24h`, `transaction.count1h`

### Merchant Attributes
- `merchant.category`, `merchant.riskScore`, `merchant.fraudScore`
- `merchant.status`

## Actions Used

### Application Actions
- `rejectApplication`, `approveApplication`, `instantApproval`
- `conditionalApproval`, `manualReview`

### Transaction Actions  
- `blockTransaction`, `approveTransaction`, `flagForReview`
- `requireAdditionalAuth`, `requirePhoneVerification`, `sendSMSVerification`

### Account Management Actions
- `increaseCreditLimit`, `decreaseCreditLimit`, `temporaryBlock`
- `requireStepUpAuth`, `sendRealTimeAlert`