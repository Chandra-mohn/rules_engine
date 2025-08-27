package com.rules.actions.application;

import com.rules.actions.Action;
import com.rules.context.RuleContext;
import java.time.LocalDateTime;

/**
 * Mock implementation for instant approval of high-quality credit card applications.
 */
public class InstantApprovalAction implements Action {
    
    @Override
    public void execute(RuleContext context) {
        // Extract relevant data from context
        String applicantId = context.getString("applicant.id");
        Integer creditScore = context.getInteger("applicant.creditScore");
        Double annualIncome = context.getDouble("applicant.annualIncome");
        String employmentType = context.getString("applicant.employmentType");
        
        // Mock business logic
        System.out.println("=== INSTANT APPROVAL ===");
        System.out.println("Timestamp: " + LocalDateTime.now());
        System.out.println("Applicant ID: " + applicantId);
        System.out.println("Credit Score: " + creditScore + " (Excellent)");
        System.out.println("Annual Income: $" + annualIncome);
        System.out.println("Employment: " + employmentType);
        
        // Calculate premium credit limit
        Double premiumLimit = calculatePremiumCreditLimit(context);
        System.out.println("Instant Approved Limit: $" + premiumLimit);
        
        // Mock external service calls
        mockCreatePremiumAccount(applicantId, premiumLimit);
        mockIssuePremiumCard(applicantId);
        mockActivateInstantCredit(applicantId, premiumLimit);
        mockNotifyInstantApproval(applicantId, premiumLimit);
        mockLogToAuditSystem(applicantId, "INSTANT_APPROVAL", premiumLimit);
        mockUpdateCRM(applicantId, "INSTANT_APPROVED");
        
        System.out.println("Instant approval processing completed.\n");
    }
    
    @Override
    public String getName() {
        return "instantApproval";
    }
    
    private Double calculatePremiumCreditLimit(RuleContext context) {
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Double requestedLimit = context.getDouble("applicant.requestedCreditLimit");
        Integer creditScore = context.getInteger("applicant.creditScore");
        String employmentType = context.getString("applicant.employmentType");
        
        if (annualIncome == null) return 5000.0; // Premium minimum
        
        // Premium calculation: 30% of annual income
        Double premiumLimit = annualIncome * 0.3;
        
        // Government employees get additional boost
        if ("government".equals(employmentType)) {
            premiumLimit *= 1.3;
        }
        
        // Excellent credit score bonus
        if (creditScore != null && creditScore >= 750) {
            premiumLimit *= 1.4;
        }
        
        // Honor requested amount if higher and reasonable
        if (requestedLimit != null && requestedLimit > premiumLimit && requestedLimit <= annualIncome * 0.5) {
            premiumLimit = requestedLimit;
        }
        
        // Premium maximum limit
        return Math.min(premiumLimit, 100000.0);
    }
    
    private void mockCreatePremiumAccount(String applicantId, Double creditLimit) {
        System.out.println("  → Creating PREMIUM credit card account for " + applicantId);
        System.out.println("  → Account type: Premium Rewards Card");
        System.out.println("  → Credit limit: $" + creditLimit);
        System.out.println("  → Premium benefits: 2x rewards, no annual fee first year");
    }
    
    private void mockIssuePremiumCard(String applicantId) {
        System.out.println("  → Issuing premium card to " + applicantId);
        System.out.println("  → Card type: Visa Signature");
        System.out.println("  → Express delivery: 2-3 business days");
        System.out.println("  → Digital wallet ready immediately");
    }
    
    private void mockActivateInstantCredit(String applicantId, Double creditLimit) {
        System.out.println("  → Activating instant credit access for " + applicantId);
        System.out.println("  → Available immediately: $" + Math.min(creditLimit, 2000.0));
        System.out.println("  → Full limit available upon card activation");
    }
    
    private void mockNotifyInstantApproval(String applicantId, Double creditLimit) {
        System.out.println("  → Sending instant approval notification to " + applicantId);
        System.out.println("  → Premium credit limit: $" + creditLimit);
        System.out.println("  → Welcome to premium banking experience");
    }
    
    private void mockLogToAuditSystem(String applicantId, String action, Double creditLimit) {
        System.out.println("  → Logging to audit system: " + action + " for " + applicantId);
        System.out.println("  → Premium limit: $" + creditLimit);
        System.out.println("  → Processing time: < 30 seconds");
    }
    
    private void mockUpdateCRM(String applicantId, String status) {
        System.out.println("  → Updating CRM system: " + applicantId + " status = " + status);
        System.out.println("  → Customer segment: Premium");
    }
}