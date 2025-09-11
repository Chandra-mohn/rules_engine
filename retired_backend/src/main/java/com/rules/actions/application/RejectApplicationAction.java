package com.rules.actions.application;

import com.rules.actions.Action;
import com.rules.context.RuleContext;
import java.time.LocalDateTime;

/**
 * Mock implementation for rejecting credit card applications.
 */
public class RejectApplicationAction implements Action {
    
    @Override
    public void execute(RuleContext context) {
        // Extract relevant data from context
        Integer applicantAge = context.getInteger("applicant.age");
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Integer creditScore = context.getInteger("applicant.creditScore");
        String applicantId = context.getString("applicant.id");
        
        // Mock business logic
        System.out.println("=== APPLICATION REJECTED ===");
        System.out.println("Timestamp: " + LocalDateTime.now());
        System.out.println("Applicant ID: " + applicantId);
        System.out.println("Age: " + applicantAge);
        System.out.println("Annual Income: $" + annualIncome);
        System.out.println("Credit Score: " + creditScore);
        
        // Determine rejection reason based on available data
        String rejectionReason = determineRejectionReason(context);
        System.out.println("Rejection Reason: " + rejectionReason);
        
        // Mock external service calls
        mockNotifyApplicant(applicantId, rejectionReason);
        mockLogToAuditSystem(applicantId, "APPLICATION_REJECTED", rejectionReason);
        mockUpdateCRM(applicantId, "REJECTED");
        
        System.out.println("Application rejection processing completed.\n");
    }
    
    @Override
    public String getName() {
        return "rejectApplication";
    }
    
    private String determineRejectionReason(RuleContext context) {
        Integer age = context.getInteger("applicant.age");
        Double income = context.getDouble("applicant.annualIncome");
        Integer creditScore = context.getInteger("applicant.creditScore");
        Double monthlyExpenses = context.getDouble("applicant.monthlyExpenses");
        
        if (age != null && age < 18) {
            return "Applicant under minimum age requirement (18)";
        }
        
        if (income != null && income < 15000) {
            return "Annual income below minimum threshold ($15,000)";
        }
        
        if (creditScore != null && creditScore < 580) {
            return "Credit score below minimum requirement (580)";
        }
        
        if (income != null && monthlyExpenses != null && income < (monthlyExpenses * 12)) {
            return "Annual income insufficient to cover monthly expenses";
        }
        
        return "Does not meet standard eligibility criteria";
    }
    
    private void mockNotifyApplicant(String applicantId, String reason) {
        System.out.println("  → Sending rejection notification to applicant " + applicantId);
        System.out.println("  → Notification method: Email + SMS");
    }
    
    private void mockLogToAuditSystem(String applicantId, String action, String reason) {
        System.out.println("  → Logging to audit system: " + action + " for " + applicantId);
        System.out.println("  → Audit reason: " + reason);
    }
    
    private void mockUpdateCRM(String applicantId, String status) {
        System.out.println("  → Updating CRM system: " + applicantId + " status = " + status);
    }
}