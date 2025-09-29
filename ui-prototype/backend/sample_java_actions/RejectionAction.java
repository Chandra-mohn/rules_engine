package com.creditcard.actions;

import com.creditcard.model.Application;
import com.creditcard.service.NotificationService;
import java.util.logging.Logger;

/**
 * Standard rejection action for credit card applications.
 * Rejects the application and notifies the applicant.
 */
public class RejectionAction {

    private static final Logger LOGGER = Logger.getLogger(RejectionAction.class.getName());
    private final NotificationService notificationService;

    public RejectionAction(NotificationService notificationService) {
        this.notificationService = notificationService;
    }

    /**
     * Executes the rejection action for a credit card application.
     * @param application The credit card application to reject
     */
    public void execute(Application application) {
        try {
            LOGGER.info("Processing rejection for application: " + application.getId());

            // Update application status
            application.setStatus("REJECTED");
            application.setRejectionDate(java.time.LocalDateTime.now());

            // Determine rejection reason
            String rejectionReason = determineRejectionReason(application);
            application.setRejectionReason(rejectionReason);

            // Send rejection notification with reason
            notificationService.sendRejectionNotification(application, rejectionReason);

            // Log the rejection
            LOGGER.info("Application " + application.getId() +
                       " rejected. Reason: " + rejectionReason);

        } catch (Exception e) {
            LOGGER.severe("Failed to process rejection for application " +
                         application.getId() + ": " + e.getMessage());
            throw new RuntimeException("Rejection processing failed", e);
        }
    }

    /**
     * Determines the primary reason for rejection.
     * @param application The application being rejected
     * @return A human-readable rejection reason
     */
    private String determineRejectionReason(Application application) {
        if (application.getCreditScore() < 500) {
            return "Credit score below minimum threshold (500)";
        }
        if (application.getAnnualIncome() < 15000) {
            return "Annual income below minimum requirement ($15,000)";
        }
        if (application.getEmploymentStatus().equals("UNEMPLOYED")) {
            return "Current employment required";
        }
        if (application.hasBankruptcyHistory()) {
            return "Recent bankruptcy history";
        }
        return "Application does not meet credit policy requirements";
    }
}