# ActionSets Complex Examples - Credit Card Domain

## Document Overview
**Purpose**: Demonstrate complex Rules and ActionSets syntax and interaction patterns
**Domain**: Credit Card Processing and Risk Management
**Complexity Level**: Production-ready business scenarios

---

## 🎯 COMPLEX ACTIONSET EXAMPLES

### **Example 1: Comprehensive Credit Application Processing**
```
ActionSet comprehensiveCreditApplicationProcessing:
    validateApplicantBasicInfo
    performCreditBureauLookup
    calculateDebtToIncomeRatio

    if applicant.age < 18 then standardMinorRejection
    if applicant.creditScore < 500 then highRiskRejection
    if applicant.bankruptcyHistory == true and applicant.bankruptcyAge < 24 then recentBankruptcyRejection

    if applicant.creditScore >= 800 and applicant.annualIncome >= 100000 then
        premiumCardApprovalWorkflow
    else if applicant.creditScore >= 650 and applicant.debtToIncomeRatio <= 0.4 then
        standardCardApprovalWorkflow
    else if applicant.creditScore >= 550 and applicant.hasExistingBankRelationship == true then
        conditionalApprovalWorkflow
    else
        manualUnderwritingWorkflow

    updateApplicantCreditProfile
    generateApplicationDecisionLetter
    logCreditApplicationDecision
```

### **Example 2: Advanced Fraud Detection and Risk Assessment**
```
ActionSet advancedFraudDetectionWorkflow:
    calculateTransactionRiskScore
    checkVelocityPatterns
    validateGeographicConsistency

    if transaction.amount > account.creditLimit then overlimitProcessing
    if transaction.riskScore > 0.8 then highRiskTransactionHandling

    if transaction.merchantCategory == "6051" and transaction.amount > 5000 then
        cashAdvanceLargeAmountReview
    else if transaction.isInternational == true and account.internationalTravelFlag == false then
        internationalTransactionVerification
    else if transaction.location.distance(account.homeLocation) > 500 then
        geographicAnomalyProcessing

    if account.suspiciousActivityFlags.length > 0 then enhancedMonitoringProtocol

    updateAccountRiskProfile
    if shouldBlockTransaction then temporaryCardBlock, immediateCustomerNotification
    if shouldFlagForReview then flagAccountForManualReview, scheduleAnalystReview

    recordFraudAssessmentDecision
```

### **Example 3: Dynamic Credit Limit Management**
```
ActionSet dynamicCreditLimitManagement:
    analyzeAccountPerformanceMetrics
    reviewPaymentHistoryTrend
    assessCurrentDebtUtilization

    if account.paymentHistory == "excellent" and account.utilization < 0.3 then
        evaluateForCreditIncrease
    else if account.paymentHistory == "poor" or account.utilization > 0.9 then
        evaluateForCreditDecrease

    if account.accountAge >= 12 and account.creditScore.improvement > 50 then
        automaticCreditIncreaseConsideration
    else if account.delinquencyCount > 2 then
        creditLimitReductionProtocol

    if proposedCreditLimit > currentCreditLimit then
        if proposedCreditLimit > 50000 then requireManagerApproval, scheduleSeniorReview
        else if proposedCreditLimit > 25000 then requireSupervisorApproval
        else processAutomaticCreditIncrease

    updateCreditLimitDecision
    generateCreditLimitChangeNotification
    reportCreditLimitChangeToBureaus
```

### **Example 4: Complex Collections and Recovery Process**
```
ActionSet collectionsAndRecoveryWorkflow:
    assessAccountDelinquencyStatus
    calculateTotalOutstandingBalance
    reviewCustomerContactHistory

    if account.daysPastDue >= 30 and account.daysPastDue < 60 then
        earlyStageCollectionsProcess
    else if account.daysPastDue >= 60 and account.daysPastDue < 90 then
        intensifiedCollectionsProcess
    else if account.daysPastDue >= 90 then
        seriousDelinquencyProtocol

    if account.outstandingBalance > 10000 and account.daysPastDue > 60 then
        legalActionConsideration
    else if account.customerResponseRate == "good" then
        negotiatedPaymentPlanOffering
    else
        standardCollectionsEscalation

    if customer.financialHardshipFlag == true then
        hardshipAssistanceProgramEvaluation
        if hardshipProgram.eligible then temporaryPaymentReliefOptions

    updateCollectionsStatus
    scheduleNextCollectionsAction
    documentCollectionsActivity
```

---

## 🔄 COMPLEX RULE EXAMPLES USING ACTIONSETS

### **Example 1: Master Credit Card Application Rule**
```
rule masterCreditCardApplicationDecisionRule:
    if applicant.applicationChannel == "online" then validateOnlineApplicationSecurity
    if applicant.existingCustomer == false then newCustomerOnboardingChecks

    if applicant.requestedCardType == "premium" and applicant.annualIncome < 75000 then
        downgradeTostandardCard, comprehensiveCreditApplicationProcessing
    else if applicant.requestedCardType == "business" then
        businessCardSpecificValidation, businessCreditApplicationProcessing
    else
        comprehensiveCreditApplicationProcessing

    if creditDecision == "approved" then
        cardProductionAndFulfillment, welcomePackageGeneration
    else if creditDecision == "conditionally_approved" then
        conditionalApprovalRequirementsNotification
    else
        applicationRejectionProcessing
```

### **Example 2: Transaction Authorization Master Rule**
```
rule transactionAuthorizationMasterRule:
    if transaction.type == "cash_advance" then cashAdvanceSpecificValidation
    if transaction.amount == 0 then zeroAmountTransactionHandling

    if account.status == "blocked" or account.status == "frozen" then
        blockedAccountTransactionHandling
    else if account.status == "restricted" then
        restrictedAccountTransactionProcessing
    else
        advancedFraudDetectionWorkflow

    if fraudAssessment.decision == "approve" then
        if transaction.amount > account.dailyLimit then dailyLimitExceededProcessing
        else if account.availableCredit < transaction.amount then insufficientCreditProcessing
        else standardTransactionApproval, updateAccountBalances
    else if fraudAssessment.decision == "review" then
        manualAuthorizationRequired, customerVerificationRequest
    else
        transactionDeclined, securityAlertGeneration

    recordTransactionDecision
```

### **Example 3: Account Maintenance and Risk Management Rule**
```
rule monthlyAccountMaintenanceAndRiskReview:
    if account.daysSinceLastReview >= 30 then performMonthlyAccountReview

    if account.creditUtilization > 0.8 then
        highUtilizationAlert, dynamicCreditLimitManagement
    else if account.creditUtilization < 0.1 and account.accountAge > 6 then
        lowUtilizationRetentionOffer

    if account.paymentHistory.contains("late_payment") then
        riskProfileReassessment
        if riskProfile.level == "high" then enhancedMonitoringActivation
        else if riskProfile.level == "medium" then standardMonitoringContinuation

    if account.inactivityDays > 90 then
        dormantAccountProcessing
    else if account.monthlySpend.trend == "increasing" and account.creditScore.improvement > 0 then
        customerLoyaltyRewardConsideration, proactiveOfferEvaluation

    generateMonthlyAccountStatement
    updateCustomerRiskProfile
```

### **Example 4: Delinquency and Collections Management Rule**
```
rule delinquencyAndCollectionsManagementRule:
    if account.daysPastDue == 1 then earlyDelinquencyNotification
    if account.daysPastDue == 7 then firstReminderNotification
    if account.daysPastDue == 15 then secondReminderNotification

    if account.daysPastDue >= 30 then
        collectionsAndRecoveryWorkflow
        if collectionsDecision == "continue_standard" then standardCollectionsContinue
        else if collectionsDecision == "escalate_legal" then legalActionInitiation
        else if collectionsDecision == "hardship_program" then hardshipAssistanceActivation
        else if collectionsDecision == "charge_off" then accountChargeOffProcessing

    if account.paymentReceived == true then
        if payment.amount >= account.minimumPaymentDue then
            standardPaymentProcessing, delinquencyStatusUpdate
        else if payment.amount > 0 then
            partialPaymentProcessing, remainingBalanceNotification

    if account.customerContact.successful == true then updateContactHistory
    else if account.customerContact.attempts >= 5 then alternateContactMethodInitiation

    generateCollectionsActivityReport
    updateDelinquencyStatus
```

---

## 🔗 ACTIONSET COMPOSITION EXAMPLES

### **Example 1: Nested ActionSet Calls**
```
ActionSet standardCreditRejection:
    logCreditDecision("rejected", "standard_criteria_failed")
    updateApplicantCreditRecord
    generateRejectionLetterContent
    sendRejectionCommunication
    updateCreditBureauReporting

ActionSet highRiskRejection:
    logCreditDecision("rejected", "high_risk_profile")
    standardCreditRejection
    flagApplicantHighRisk
    enhancedRejectionDocumentation
    regulatoryComplianceReporting

ActionSet recentBankruptcyRejection:
    logCreditDecision("rejected", "recent_bankruptcy_history")
    highRiskRejection
    bankruptcySpecificDocumentation
    legalComplianceValidation
```

### **Example 2: Complex Conditional ActionSet Flow**
```
ActionSet premiumCardApprovalWorkflow:
    validatePremiumCardEligibility
    calculatePremiumCreditLimit

    if calculatedCreditLimit >= 50000 then
        executiveApprovalRequired
        if executiveApproval.approved == true then
            ultrahighNetWorthCustomerProcessing
        else
            standardPremiumProcessing
    else
        standardPremiumProcessing

    if applicant.hasExistingPremiumRelationships == true then
        existingPremiumCustomerBenefits
    else
        newPremiumCustomerOnboarding

    premiumCardProductionProcessing
    conciergeServiceActivation
    premiumRewardsEnrollment

ActionSet standardPremiumProcessing:
    assignPremiumAccountManager
    activatePremiumBenefits
    generatePremiumWelcomePackage
    schedulePremiumCustomerCall

ActionSet ultrahighNetWorthCustomerProcessing:
    assignPrivateBankingRelationshipManager
    activateUltraPremiumBenefits
    privateWealthManagementReferral
    generateUltraPremiumWelcomePackage
```

### **Example 3: Multi-Level ActionSet Hierarchy**
```
ActionSet customerNotificationOrchestration:
    determineCustomerPreferredChannel

    if customer.preferredChannel == "email" then emailNotificationProcessing
    else if customer.preferredChannel == "sms" then smsNotificationProcessing
    else if customer.preferredChannel == "phone" then phoneNotificationProcessing
    else multiChannelNotificationProcessing

    logNotificationAttempt
    scheduleFollowUpIfNoResponse

ActionSet emailNotificationProcessing:
    validateEmailAddress
    generatePersonalizedEmailContent
    applyBrandingAndTemplating
    sendEmailNotification
    trackEmailDeliveryStatus

ActionSet smsNotificationProcessing:
    validateMobileNumber
    generateSMSContent
    applySMSRegulationsCompliance
    sendSMSNotification
    trackSMSDeliveryStatus

ActionSet phoneNotificationProcessing:
    validatePhoneNumber
    generateCallScript
    scheduleCallbackAttempt
    if customer.availableNow == true then immediatePhoneCall
    else scheduledPhoneCall

ActionSet multiChannelNotificationProcessing:
    emailNotificationProcessing
    if email.deliveryFailed == true then smsNotificationProcessing
    if sms.deliveryFailed == true then phoneNotificationProcessing
```

---

## 🧪 EXECUTION FLOW EXAMPLES

### **Example: Complex Rule Execution with Multiple ActionSet Calls**

**Input Data:**
```javascript
{
  applicant: {
    age: 45,
    creditScore: 720,
    annualIncome: 95000,
    requestedCardType: "premium",
    existingCustomer: true,
    bankruptcyHistory: false,
    debtToIncomeRatio: 0.35
  },
  application: {
    channel: "online",
    requestedCreditLimit: 30000
  }
}
```

**Rule Execution:**
```
rule masterCreditCardApplicationDecisionRule:
    ✓ applicant.applicationChannel == "online" → validateOnlineApplicationSecurity
    ✗ applicant.existingCustomer == false (skip newCustomerOnboardingChecks)
    ✗ applicant.requestedCardType == "premium" and applicant.annualIncome < 75000 (skip downgrade)
    ✗ applicant.requestedCardType == "business" (skip business validation)
    ✓ else → comprehensiveCreditApplicationProcessing
```

**ActionSet Execution Flow:**
```
comprehensiveCreditApplicationProcessing:
    ✓ validateApplicantBasicInfo
    ✓ performCreditBureauLookup
    ✓ calculateDebtToIncomeRatio
    ✗ applicant.age < 18 (skip minor rejection)
    ✗ applicant.creditScore < 500 (skip high risk rejection)
    ✗ applicant.bankruptcyHistory == true (skip bankruptcy rejection)
    ✓ applicant.creditScore >= 650 and applicant.debtToIncomeRatio <= 0.4 → standardCardApprovalWorkflow
    ✓ updateApplicantCreditProfile
    ✓ generateApplicationDecisionLetter
    ✓ logCreditApplicationDecision

standardCardApprovalWorkflow:
    ✓ assignStandardAccountManager
    ✓ activateStandardBenefits
    ✓ generateStandardWelcomePackage
    ✓ scheduleStandardCustomerCall
```

**Final Execution Context:**
```javascript
{
  // Original data preserved
  applicant: { age: 45, creditScore: 720, ... },

  // Decisions added by ActionSets
  creditDecisions: [
    { decision: "approved", type: "standard_card", timestamp: "..." },
    { creditLimit: 25000, approvalMethod: "automated" }
  ],

  // Flags set during execution
  riskFlags: [],

  // Actions executed
  executedActions: [
    "validateApplicantBasicInfo",
    "performCreditBureauLookup",
    "calculateDebtToIncomeRatio",
    "standardCardApprovalWorkflow",
    "updateApplicantCreditProfile",
    "generateApplicationDecisionLetter",
    "logCreditApplicationDecision"
  ],

  // Metadata
  creditMetadata: {
    executionId: "cc_app_20250914_001",
    callStack: [
      "masterCreditCardApplicationDecisionRule",
      "comprehensiveCreditApplicationProcessing",
      "standardCardApprovalWorkflow"
    ],
    maxCallDepth: 3,
    processingTimeMs: 1247
  }
}
```

---

## 📊 COMPLEXITY METRICS

### **ActionSet Complexity Analysis**
- **comprehensiveCreditApplicationProcessing**:
  - Lines: 17
  - Conditional branches: 6
  - ActionSet calls: 4
  - Call depth: Up to 3 levels

- **advancedFraudDetectionWorkflow**:
  - Lines: 19
  - Conditional branches: 8
  - ActionSet calls: 5
  - Real-time decision complexity: High

### **Rule Complexity Analysis**
- **masterCreditCardApplicationDecisionRule**:
  - Conditions: 6
  - ActionSet calls: 8
  - Decision paths: 12
  - Integration complexity: High

- **transactionAuthorizationMasterRule**:
  - Conditions: 9
  - ActionSet calls: 6
  - Real-time requirements: Critical
  - Performance requirements: <100ms execution

---

**These examples demonstrate production-level complexity while maintaining the declarative, business-readable nature of the DSL. The ActionSets provide powerful reusability and composition capabilities without becoming a programming language.**