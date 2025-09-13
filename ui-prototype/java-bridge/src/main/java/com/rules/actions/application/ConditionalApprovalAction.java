package com.rules.actions.application;

import com.rules.actions.Action;
import com.rules.context.RuleContext;
import java.time.LocalDateTime;

/**
 * Mock implementation for conditional approval of credit card applications.
 */
public class ConditionalApprovalAction implements Action {
    
    @Override
    public void execute(RuleContext context) {
        // Extract relevant data from context
        String applicantId = context.getString("applicant.id");
        Integer creditScore = context.getInteger("applicant.creditScore");
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Double totalDebt = context.getDouble("applicant.totalDebt");
        
        // Mock business logic
        System.out.println("=== CONDITIONAL APPROVAL ===");
        System.out.println("Timestamp: " + LocalDateTime.now());
        System.out.println("Applicant ID: " + applicantId);
        System.out.println("Credit Score: " + creditScore + " (Good)");
        System.out.println("Annual Income: $" + annualIncome);
        System.out.println("Total Debt: $" + totalDebt);
        
        // Calculate conditional credit limit and terms
        Double conditionalLimit = calculateConditionalLimit(context);
        String[] conditions = determineConditions(context);
        
        System.out.println("Conditional Limit: $" + conditionalLimit);
        System.out.println("Conditions Applied:");
        for (String condition : conditions) {
            System.out.println("  - " + condition);
        }
        
        // Mock external service calls
        mockCreateConditionalAccount(applicantId, conditionalLimit, conditions);
        mockSetupMonitoring(applicantId, conditions);
        mockNotifyConditionalApproval(applicantId, conditionalLimit, conditions);
        mockLogToAuditSystem(applicantId, "CONDITIONAL_APPROVAL", conditionalLimit);
        mockUpdateCRM(applicantId, "CONDITIONALLY_APPROVED");
        
        System.out.println("Conditional approval processing completed.\n");
    }
    
    @Override
    public String getName() {
        return "conditionalApproval";
    }
    
    private Double calculateConditionalLimit(RuleContext context) {
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Double totalDebt = context.getDouble("applicant.totalDebt");
        Integer creditScore = context.getInteger("applicant.creditScore");
        
        if (annualIncome == null) return 1500.0; // Conservative minimum
        
        // Conservative calculation: 15% of annual income
        Double baseLimit = annualIncome * 0.15;
        
        // Reduce if high debt-to-income ratio
        if (totalDebt != null && annualIncome > 0) {
            double debtRatio = totalDebt / annualIncome;
            if (debtRatio > 0.3) {
                baseLimit *= 0.8; // 20% reduction for high debt
            }
        }
        
        // Adjust based on credit score
        if (creditScore != null) {
            if (creditScore >= 700) {
                baseLimit *= 1.1; // 10% increase for good credit
            } else if (creditScore < 650) {
                baseLimit *= 0.9; // 10% reduction for fair credit
            }
        }
        
        // Conservative maximum limit
        return Math.min(baseLimit, 10000.0);
    }
    
    private String[] determineConditions(RuleContext context) {
        Integer creditScore = context.getInteger("applicant.creditScore");
        Double totalDebt = context.getDouble("applicant.totalDebt");
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Integer employmentTenure = context.getInteger("applicant.employmentTenure");
        
        java.util.List<String> conditions = new java.util.ArrayList<>();
        
        // Standard conditions for conditional approval
        conditions.add("6-month probationary period with account review");
        conditions.add("Monthly payment history monitoring");
        
        // Credit score based conditions
        if (creditScore != null && creditScore < 680) {
            conditions.add("Credit score improvement required within 12 months");
            conditions.add("Financial counseling session recommended");
        }
        
        // Debt-to-income based conditions
        if (totalDebt != null && annualIncome != null && (totalDebt / annualIncome) > 0.35) {
            conditions.add("Debt reduction plan required");
            conditions.add("No additional credit inquiries for 6 months");
        }
        
        // Employment stability conditions
        if (employmentTenure != null && employmentTenure < 24) {
            conditions.add("Employment verification required every 6 months");
        }
        
        return conditions.toArray(new String[0]);
    }
    
    private void mockCreateConditionalAccount(String applicantId, Double creditLimit, String[] conditions) {
        System.out.println("  → Creating conditional credit card account for " + applicantId);
        System.out.println("  → Account type: Conditional Credit Card");
        System.out.println("  → Initial credit limit: $" + creditLimit);
        System.out.println("  → Conditions count: " + conditions.length);
    }
    
    private void mockSetupMonitoring(String applicantId, String[] conditions) {
        System.out.println("  → Setting up enhanced monitoring for " + applicantId);
        System.out.println("  → Monthly account reviews scheduled");
        System.out.println("  → Automated condition tracking enabled");
    }
    
    private void mockNotifyConditionalApproval(String applicantId, Double creditLimit, String[] conditions) {
        System.out.println("  → Sending conditional approval notification to " + applicantId);
        System.out.println("  → Conditional credit limit: $" + creditLimit);
        System.out.println("  → Terms and conditions document attached");
    }
    
    private void mockLogToAuditSystem(String applicantId, String action, Double creditLimit) {
        System.out.println("  → Logging to audit system: " + action + " for " + applicantId);
        System.out.println("  → Conditional limit: $" + creditLimit);
        System.out.println("  → Review schedule: 6 months");
    }
    
    private void mockUpdateCRM(String applicantId, String status) {
        System.out.println("  → Updating CRM system: " + applicantId + " status = " + status);
        System.out.println("  → Customer segment: Conditional");
    }
}