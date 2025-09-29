package com.creditcard.actions;

import com.creditcard.model.Application;
import com.creditcard.service.NotificationService;
import com.creditcard.service.CreditLimitService;
import java.util.logging.Logger;

/**
 * Standard approval action for credit card applications.
 * Approves the application and triggers downstream processes.
 */
public class ApprovalAction {

    private static final Logger LOGGER = Logger.getLogger(ApprovalAction.class.getName());
    private final NotificationService notificationService;
    private final CreditLimitService creditLimitService;

    public ApprovalAction(NotificationService notificationService,
                         CreditLimitService creditLimitService) {
        this.notificationService = notificationService;
        this.creditLimitService = creditLimitService;
    }

    /**
     * Executes the approval action for a credit card application.
     * @param application The credit card application to approve
     */
    public void execute(Application application) {
        try {
            LOGGER.info("Processing approval for application: " + application.getId());

            // Update application status
            application.setStatus("APPROVED");
            application.setApprovalDate(java.time.LocalDateTime.now());

            // Calculate and set credit limit
            double creditLimit = creditLimitService.calculateLimit(application);
            application.setCreditLimit(creditLimit);

            // Send approval notification
            notificationService.sendApprovalNotification(application);

            // Log the approval
            LOGGER.info("Application " + application.getId() +
                       " approved with credit limit: $" + creditLimit);

        } catch (Exception e) {
            LOGGER.severe("Failed to process approval for application " +
                         application.getId() + ": " + e.getMessage());
            throw new RuntimeException("Approval processing failed", e);
        }
    }

    /**
     * Validates if the application can be approved.
     * @param application The application to validate
     * @return true if application can be approved
     */
    public boolean canApprove(Application application) {
        return application.getCreditScore() >= 600
            && application.getAnnualIncome() > 0
            && application.getEmploymentStatus().equals("EMPLOYED");
    }
}