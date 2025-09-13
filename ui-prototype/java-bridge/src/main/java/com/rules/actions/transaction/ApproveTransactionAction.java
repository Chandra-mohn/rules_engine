package com.rules.actions.transaction;

import com.rules.actions.Action;
import com.rules.context.RuleContext;
import java.time.LocalDateTime;

/**
 * Mock implementation for approving credit card transactions.
 */
public class ApproveTransactionAction implements Action {
    
    @Override
    public void execute(RuleContext context) {
        // Extract relevant data from context
        String transactionId = context.getString("transaction.id");
        Double amount = context.getDouble("transaction.amount");
        String currency = context.getString("transaction.currency");
        String merchantName = context.getString("merchant.name");
        String customerId = context.getString("customer.id");
        Double availableCredit = context.getDouble("card.availableCredit");
        
        // Mock business logic
        System.out.println("=== TRANSACTION APPROVED ===");
        System.out.println("Timestamp: " + LocalDateTime.now());
        System.out.println("Transaction ID: " + transactionId);
        System.out.println("Customer ID: " + customerId);
        System.out.println("Amount: " + currency + " " + amount);
        System.out.println("Merchant: " + merchantName);
        System.out.println("Available Credit Before: " + currency + " " + availableCredit);
        
        // Calculate new available credit
        Double newAvailableCredit = availableCredit - amount;
        System.out.println("Available Credit After: " + currency + " " + newAvailableCredit);
        
        // Generate authorization code
        String authCode = generateAuthorizationCode();
        System.out.println("Authorization Code: " + authCode);
        
        // Mock external service calls
        mockProcessPayment(transactionId, amount, authCode);
        mockUpdateCardBalance(customerId, amount, newAvailableCredit);
        mockNotifyMerchant(transactionId, authCode, "APPROVED");
        mockLogTransaction(transactionId, "APPROVED", amount);
        mockUpdateFraudScoring(customerId, transactionId, "APPROVED");
        
        System.out.println("Transaction approval processing completed.\n");
    }
    
    @Override
    public String getName() {
        return "approveTransaction";
    }
    
    private String generateAuthorizationCode() {
        // Mock authorization code generation
        return "AUTH" + System.currentTimeMillis() % 1000000;
    }
    
    private void mockProcessPayment(String transactionId, Double amount, String authCode) {
        System.out.println("  → Processing payment for transaction " + transactionId);
        System.out.println("  → Amount authorized: $" + amount);
        System.out.println("  → Authorization code: " + authCode);
        System.out.println("  → Payment network: Visa/MasterCard");
    }
    
    private void mockUpdateCardBalance(String customerId, Double amount, Double newBalance) {
        System.out.println("  → Updating card balance for customer " + customerId);
        System.out.println("  → Transaction amount: $" + amount);
        System.out.println("  → New available credit: $" + newBalance);
    }
    
    private void mockNotifyMerchant(String transactionId, String authCode, String status) {
        System.out.println("  → Notifying merchant of transaction " + transactionId);
        System.out.println("  → Status: " + status);
        System.out.println("  → Authorization code: " + authCode);
        System.out.println("  → Settlement will occur within 1-2 business days");
    }
    
    private void mockLogTransaction(String transactionId, String status, Double amount) {
        System.out.println("  → Logging transaction to audit system");
        System.out.println("  → Transaction ID: " + transactionId);
        System.out.println("  → Status: " + status);
        System.out.println("  → Amount: $" + amount);
    }
    
    private void mockUpdateFraudScoring(String customerId, String transactionId, String outcome) {
        System.out.println("  → Updating fraud scoring models");
        System.out.println("  → Customer: " + customerId);
        System.out.println("  → Transaction outcome: " + outcome);
        System.out.println("  → Risk profile updated");
    }
}