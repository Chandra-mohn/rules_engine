package com.rules.orchestration.core;

import java.util.List;
import java.util.Collections;

/**
 * Rule execution result - optimized for minimal allocation
 */
public final class RuleResult {

    public enum Status {
        SUCCESS, REJECTED, ERROR, UNKNOWN_TRANSACTION, UNKNOWN_CLIENT
    }

    private final Status status;
    private final String message;
    private final TransactionContext finalContext;
    private final List<String> executedActions;

    private RuleResult(Status status, String message, TransactionContext finalContext, List<String> executedActions) {
        this.status = status;
        this.message = message;
        this.finalContext = finalContext;
        this.executedActions = executedActions != null ? executedActions : Collections.emptyList();
    }

    // Factory methods for common results
    public static RuleResult success(TransactionContext context) {
        return new RuleResult(Status.SUCCESS, "Rule executed successfully", context, null);
    }

    public static RuleResult rejected(TransactionContext context, String reason) {
        return new RuleResult(Status.REJECTED, reason, context, null);
    }

    public static RuleResult error(String message) {
        return new RuleResult(Status.ERROR, message, null, null);
    }

    public static RuleResult unknownTransaction(String transactionCode) {
        return new RuleResult(Status.UNKNOWN_TRANSACTION, "Unknown transaction: " + transactionCode, null, null);
    }

    public static RuleResult unknownClient(String clientId) {
        return new RuleResult(Status.UNKNOWN_CLIENT, "Unknown client: " + clientId, null, null);
    }

    // Getters
    public Status getStatus() { return status; }
    public String getMessage() { return message; }
    public TransactionContext getFinalContext() { return finalContext; }
    public List<String> getExecutedActions() { return executedActions; }

    public boolean isSuccess() { return status == Status.SUCCESS; }
    public boolean isRejected() { return status == Status.REJECTED; }
    public boolean isError() { return status == Status.ERROR; }
}