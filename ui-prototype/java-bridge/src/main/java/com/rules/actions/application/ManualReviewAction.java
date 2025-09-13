package com.rules.actions.application;

import com.rules.actions.Action;
import com.rules.context.RuleContext;
import java.time.LocalDateTime;

/**
 * Mock implementation for routing applications to manual review.
 */
public class ManualReviewAction implements Action {
    
    @Override
    public void execute(RuleContext context) {
        // Extract relevant data from context
        String applicantId = context.getString("applicant.id");
        Integer creditScore = context.getInteger("applicant.creditScore");
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Double requestedLimit = context.getDouble("applicant.requestedCreditLimit");
        
        // Mock business logic
        System.out.println("=== MANUAL REVIEW REQUIRED ===");
        System.out.println("Timestamp: " + LocalDateTime.now());
        System.out.println("Applicant ID: " + applicantId);
        System.out.println("Credit Score: " + creditScore);
        System.out.println("Annual Income: $" + annualIncome);
        System.out.println("Requested Limit: $" + requestedLimit);
        
        // Determine review priority and reasons
        String priority = determineReviewPriority(context);
        String[] reviewReasons = determineReviewReasons(context);
        String assignedReviewer = assignReviewer(priority);
        
        System.out.println("Review Priority: " + priority);
        System.out.println("Assigned Reviewer: " + assignedReviewer);
        System.out.println("Review Reasons:");
        for (String reason : reviewReasons) {
            System.out.println("  - " + reason);
        }
        
        // Mock external service calls
        mockCreateReviewCase(applicantId, priority, reviewReasons, assignedReviewer);
        mockNotifyReviewer(assignedReviewer, applicantId, priority);
        mockNotifyApplicant(applicantId);
        mockLogToAuditSystem(applicantId, "MANUAL_REVIEW_ASSIGNED", priority);
        mockUpdateCRM(applicantId, "UNDER_REVIEW");
        
        System.out.println("Manual review assignment completed.\n");
    }
    
    @Override
    public String getName() {
        return "manualReview";
    }
    
    private String determineReviewPriority(RuleContext context) {
        Integer creditScore = context.getInteger("applicant.creditScore");
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Boolean existingRelationship = context.getBoolean("applicant.existingRelationship");
        Double collateralValue = context.getDouble("applicant.collateralValue");
        
        // High priority for existing customers or high collateral
        if (Boolean.TRUE.equals(existingRelationship) || 
            (collateralValue != null && collateralValue > 50000)) {
            return "HIGH";
        }
        
        // Medium priority for borderline cases with decent income
        if (creditScore != null && creditScore >= 600 && 
            annualIncome != null && annualIncome >= 40000) {
            return "MEDIUM";
        }
        
        // Low priority for other cases
        return "LOW";
    }
    
    private String[] determineReviewReasons(RuleContext context) {
        Integer creditScore = context.getInteger("applicant.creditScore");
        Double annualIncome = context.getDouble("applicant.annualIncome");
        Double requestedLimit = context.getDouble("applicant.requestedCreditLimit");
        Integer recentInquiries = context.getInteger("applicant.recentInquiries");
        Boolean existingRelationship = context.getBoolean("applicant.existingRelationship");
        Double collateralValue = context.getDouble("applicant.collateralValue");
        
        java.util.List<String> reasons = new java.util.ArrayList<>();
        
        // Credit score reasons
        if (creditScore != null && creditScore < 650) {
            reasons.add("Credit score below standard threshold (" + creditScore + ")");
        }
        
        // Income vs requested limit
        if (annualIncome != null && requestedLimit != null && 
            requestedLimit > (annualIncome * 0.5)) {
            reasons.add("Requested limit high relative to income");
        }
        
        // Recent credit inquiries
        if (recentInquiries != null && recentInquiries > 3) {
            reasons.add("Multiple recent credit inquiries (" + recentInquiries + ")");
        }
        
        // Positive factors
        if (Boolean.TRUE.equals(existingRelationship)) {
            reasons.add("Existing customer relationship - potential for approval");
        }
        
        if (collateralValue != null && collateralValue > 0) {
            reasons.add("Collateral offered ($" + collateralValue + ") - secured option available");
        }
        
        if (reasons.isEmpty()) {
            reasons.add("Complex case requiring human judgment");
        }
        
        return reasons.toArray(new String[0]);
    }
    
    private String assignReviewer(String priority) {
        // Mock reviewer assignment logic
        switch (priority) {
            case "HIGH":
                return "Senior Underwriter - Sarah Johnson";
            case "MEDIUM":
                return "Credit Analyst - Mike Chen";
            case "LOW":
            default:
                return "Junior Analyst - Alex Rodriguez";
        }
    }
    
    private void mockCreateReviewCase(String applicantId, String priority, String[] reasons, String reviewer) {
        System.out.println("  → Creating review case for " + applicantId);
        System.out.println("  → Case ID: REV-" + System.currentTimeMillis());
        System.out.println("  → Priority: " + priority);
        System.out.println("  → Assigned to: " + reviewer);
        System.out.println("  → SLA: " + getSLA(priority));
    }
    
    private void mockNotifyReviewer(String reviewer, String applicantId, String priority) {
        System.out.println("  → Notifying reviewer: " + reviewer);
        System.out.println("  → New " + priority + " priority case: " + applicantId);
        System.out.println("  → Review dashboard updated");
    }
    
    private void mockNotifyApplicant(String applicantId) {
        System.out.println("  → Notifying applicant " + applicantId);
        System.out.println("  → Message: Application under review, decision within 3-5 business days");
    }
    
    private void mockLogToAuditSystem(String applicantId, String action, String priority) {
        System.out.println("  → Logging to audit system: " + action + " for " + applicantId);
        System.out.println("  → Review priority: " + priority);
    }
    
    private void mockUpdateCRM(String applicantId, String status) {
        System.out.println("  → Updating CRM system: " + applicantId + " status = " + status);
    }
    
    private String getSLA(String priority) {
        switch (priority) {
            case "HIGH": return "24 hours";
            case "MEDIUM": return "48 hours";
            case "LOW": 
            default: return "72 hours";
        }
    }
}