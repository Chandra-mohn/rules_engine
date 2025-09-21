package com.rules.orchestration.core;

/**
 * Centralized error handling for rule execution
 */
public final class ErrorHandler {

    /**
     * Handle exceptions during rule execution
     */
    public RuleResult handleException(String clientId, String transactionCode, Exception e) {
        // Log error (would integrate with actual logging framework)
        String errorMessage = String.format("Rule execution failed for client=%s, txn=%s: %s",
                                           clientId, transactionCode, e.getMessage());

        // Return error result
        return RuleResult.error(errorMessage);
    }

    /**
     * Handle timeout exceptions
     */
    public RuleResult handleTimeout(String clientId, String transactionCode, long timeoutMs) {
        String errorMessage = String.format("Rule execution timeout for client=%s, txn=%s after %dms",
                                           clientId, transactionCode, timeoutMs);
        return RuleResult.error(errorMessage);
    }
}