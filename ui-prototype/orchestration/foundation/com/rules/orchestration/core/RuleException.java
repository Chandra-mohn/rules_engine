package com.rules.orchestration.core;

/**
 * Custom exception for rule execution errors
 */
public class RuleException extends RuntimeException {

    private final String clientId;
    private final String transactionCode;
    private final long executionTimeNanos;

    public RuleException(String message, String clientId, String transactionCode, long executionTimeNanos) {
        super(message);
        this.clientId = clientId;
        this.transactionCode = transactionCode;
        this.executionTimeNanos = executionTimeNanos;
    }

    public RuleException(String message, Throwable cause, String clientId, String transactionCode, long executionTimeNanos) {
        super(message, cause);
        this.clientId = clientId;
        this.transactionCode = transactionCode;
        this.executionTimeNanos = executionTimeNanos;
    }

    public String getClientId() { return clientId; }
    public String getTransactionCode() { return transactionCode; }
    public long getExecutionTimeNanos() { return executionTimeNanos; }
}