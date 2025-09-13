package com.rules.actions.application;

import com.rules.actions.Action;
import com.rules.context.RuleContext;
import java.time.LocalDateTime;

/**
 * Mock implementation for approving credit card applications.
 */
public class ApproveApplicationAction implements Action {
    
    @Override
    public void execute(RuleContext context) {
        // Extract relevant data from context
        String applicantId = context.getString("applicant.id");
        Integer creditScore = context.getInteger("applicant.creditScore");
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Double requestedLimit = context.getDouble("applicant.requestedCreditLimit");
        
        // Mock business logic
        System.out.println("=== APPLICATION APPROVED ===");
        System.out.println("Timestamp: " + LocalDateTime.now());
        System.out.println("Applicant ID: " + applicantId);
        System.out.println("Credit Score: " + creditScore);
        System.out.println("Annual Income: $" + annualIncome);
        
        // Calculate approved credit limit
        Double approvedLimit = calculateCreditLimit(context);
        System.out.println("Requested Limit: $" + requestedLimit);
        System.out.println("Approved Limit: $" + approvedLimit);
        
        // Mock external service calls
        mockCreateAccount(applicantId, approvedLimit);
        mockIssueCard(applicantId, approvedLimit);
        mockNotifyApplicant(applicantId, approvedLimit);
        mockLogToAuditSystem(applicantId, "APPLICATION_APPROVED", approvedLimit);
        mockUpdateCRM(applicantId, "APPROVED");
        
        System.out.println("Application approval processing completed.\n");
    }
    
    @Override
    public String getName() {
        return "approveApplication";
    }
    
    private Double calculateCreditLimit(RuleContext context) {
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Double requestedLimit = context.getDouble("applicant.requestedCreditLimit");
        Integer creditScore = context.getInteger("applicant.creditScore");
        
        if (annualIncome == null) return 1000.0; // Default minimum
        
        // Base calculation: 20% of annual income
        Double baseLimit = annualIncome * 0.2;
        
        // Adjust based on credit score
        if (creditScore != null) {
            if (creditScore >= 750) {
                baseLimit *= 1.5; // 50% increase for excellent credit
            } else if (creditScore >= 700) {
                baseLimit *= 1.2; // 20% increase for good credit
            }
        }
        
        // Cap at requested amount if reasonable
        if (requestedLimit != null && requestedLimit < baseLimit) {
            return requestedLimit;
        }
        
        // Maximum limit cap
        return Math.min(baseLimit, 50000.0);
    }
    
    private void mockCreateAccount(String applicantId, Double creditLimit) {
        System.out.println("  → Creating credit card account for " + applicantId);
        System.out.println("  → Account type: Standard Credit Card");
        System.out.println("  → Credit limit: $" + creditLimit);
    }
    
    private void mockIssueCard(String applicantId, Double creditLimit) {
        System.out.println("  → Issuing physical card to " + applicantId);
        System.out.println("  → Card type: Visa Classic");
        System.out.println("  → Estimated delivery: 7-10 business days");
    }
    
    private void mockNotifyApplicant(String applicantId, Double creditLimit) {
        System.out.println("  → Sending approval notification to " + applicantId);
        System.out.println("  → Approved credit limit: $" + creditLimit);
        System.out.println("  → Welcome package being prepared");
    }
    
    private void mockLogToAuditSystem(String applicantId, String action, Double creditLimit) {
        System.out.println("  → Logging to audit system: " + action + " for " + applicantId);
        System.out.println("  → Approved limit: $" + creditLimit);
    }
    
    private void mockUpdateCRM(String applicantId, String status) {
        System.out.println("  → Updating CRM system: " + applicantId + " status = " + status);
    }
}