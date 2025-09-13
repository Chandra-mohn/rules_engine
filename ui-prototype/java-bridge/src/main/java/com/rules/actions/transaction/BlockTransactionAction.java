package com.rules.actions.transaction;

import com.rules.actions.Action;
import com.rules.context.RuleContext;
import java.time.LocalDateTime;

/**
 * Mock implementation for blocking/declining credit card transactions.
 */
public class BlockTransactionAction implements Action {
    
    @Override
    public void execute(RuleContext context) {
        // Extract relevant data from context
        String transactionId = context.getString("transaction.id");
        Double amount = context.getDouble("transaction.amount");
        String currency = context.getString("transaction.currency");
        String merchantName = context.getString("merchant.name");
        String customerId = context.getString("customer.id");
        String cardStatus = context.getString("card.status");
        
        // Mock business logic
        System.out.println("=== TRANSACTION BLOCKED ===");
        System.out.println("Timestamp: " + LocalDateTime.now());
        System.out.println("Transaction ID: " + transactionId);
        System.out.println("Customer ID: " + customerId);
        System.out.println("Amount: " + currency + " " + amount);
        System.out.println("Merchant: " + merchantName);
        System.out.println("Card Status: " + cardStatus);
        
        // Determine block reason
        String blockReason = determineBlockReason(context);
        System.out.println("Block Reason: " + blockReason);
        
        // Determine if customer notification is needed
        boolean requiresNotification = requiresCustomerNotification(blockReason);
        
        // Mock external service calls
        mockDeclineTransaction(transactionId, blockReason);
        mockNotifyMerchant(transactionId, "DECLINED", blockReason);
        mockLogDeclinedTransaction(transactionId, blockReason, amount);
        mockUpdateFraudScoring(customerId, transactionId, "DECLINED", blockReason);
        
        if (requiresNotification) {
            mockNotifyCustomer(customerId, transactionId, blockReason);
        }
        
        // Check if card should be temporarily blocked
        if (shouldTemporarilyBlockCard(blockReason)) {
            mockTemporaryCardBlock(customerId, blockReason);
        }
        
        System.out.println("Transaction blocking processing completed.\n");
    }
    
    @Override
    public String getName() {
        return "blockTransaction";
    }
    
    private String determineBlockReason(RuleContext context) {
        String cardStatus = context.getString("card.status");
        Double amount = context.getDouble("transaction.amount");
        Double dailyLimit = context.getDouble("card.dailyLimit");
        Double availableCredit = context.getDouble("card.availableCredit");
        String country = context.getString("transaction.country");
        String homeCountry = context.getString("customer.homeCountry");
        
        // Check card status first
        if ("blocked".equals(cardStatus)) {
            return "Card is currently blocked";
        }
        if ("stolen".equals(cardStatus)) {
            return "Card reported as stolen";
        }
        if ("expired".equals(cardStatus)) {
            return "Card has expired";
        }
        
        // Check limits
        if (amount != null && dailyLimit != null && amount > dailyLimit) {
            return "Transaction exceeds daily limit ($" + dailyLimit + ")";
        }
        
        if (amount != null && availableCredit != null && amount > availableCredit) {
            return "Insufficient available credit ($" + availableCredit + " available)";
        }
        
        // Check geographic restrictions
        if (country != null && homeCountry != null && !country.equals(homeCountry)) {
            return "International transaction from restricted location";
        }
        
        return "Transaction blocked by security rules";
    }
    
    private boolean requiresCustomerNotification(String blockReason) {
        // Notify customer for security-related blocks, not for limit/balance issues
        return blockReason.contains("stolen") || 
               blockReason.contains("security") || 
               blockReason.contains("blocked") ||
               blockReason.contains("restricted location");
    }
    
    private boolean shouldTemporarilyBlockCard(String blockReason) {
        // Temporarily block card for fraud-related reasons
        return blockReason.contains("stolen") || 
               blockReason.contains("security") ||
               blockReason.contains("restricted location");
    }
    
    private void mockDeclineTransaction(String transactionId, String reason) {
        System.out.println("  → Declining transaction " + transactionId);
        System.out.println("  → Decline reason: " + reason);
        System.out.println("  → Response code: 05 (Do Not Honor)");
    }
    
    private void mockNotifyMerchant(String transactionId, String status, String reason) {
        System.out.println("  → Notifying merchant of transaction " + transactionId);
        System.out.println("  → Status: " + status);
        System.out.println("  → Reason: " + reason);
        System.out.println("  → Merchant advised to request alternative payment");
    }
    
    private void mockLogDeclinedTransaction(String transactionId, String reason, Double amount) {
        System.out.println("  → Logging declined transaction to audit system");
        System.out.println("  → Transaction ID: " + transactionId);
        System.out.println("  → Decline reason: " + reason);
        System.out.println("  → Amount: $" + amount);
    }
    
    private void mockUpdateFraudScoring(String customerId, String transactionId, String outcome, String reason) {
        System.out.println("  → Updating fraud scoring models");
        System.out.println("  → Customer: " + customerId);
        System.out.println("  → Transaction outcome: " + outcome);
        System.out.println("  → Decline reason: " + reason);
    }
    
    private void mockNotifyCustomer(String customerId, String transactionId, String reason) {
        System.out.println("  → Sending security alert to customer " + customerId);
        System.out.println("  → Transaction blocked: " + transactionId);
        System.out.println("  → Reason: " + reason);
        System.out.println("  → Customer advised to contact support if legitimate");
    }
    
    private void mockTemporaryCardBlock(String customerId, String reason) {
        System.out.println("  → Initiating temporary card block for customer " + customerId);
        System.out.println("  → Block reason: " + reason);
        System.out.println("  → Customer must call to unblock");
        System.out.println("  → Block duration: Until customer verification");
    }
}