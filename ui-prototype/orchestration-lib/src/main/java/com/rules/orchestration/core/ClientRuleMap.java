package com.rules.orchestration.core;

/**
 * Interface for client-specific rule mapping and execution
 *
 * Generated client router classes implement this interface.
 * The interface is kept minimal for maximum performance.
 */
public interface ClientRuleMap {

    /**
     * Execute rule for given transaction code
     *
     * Implementing classes contain generated static maps for O(1) lookup:
     * - Hot path executors (80% of traffic, <=5 steps)
     * - Cold path executors (20% of traffic, complex rules)
     *
     * @param transactionCode Transaction code to execute
     * @param context Transaction context (immutable)
     * @return Rule execution result
     */
    RuleResult execute(String transactionCode, TransactionContext context);

    /**
     * Get client identifier
     */
    default String getClientId() {
        return this.getClass().getSimpleName().replaceAll("RuleMap$", "");
    }

    /**
     * Get supported transaction codes for this client
     */
    default java.util.Set<String> getSupportedTransactionCodes() {
        // Default implementation - can be overridden by generated classes
        return java.util.Collections.emptySet();
    }

    /**
     * Warm up this client's rule executors
     */
    default void warmUp() {
        // Default no-op - can be overridden for client-specific warmup
    }
}