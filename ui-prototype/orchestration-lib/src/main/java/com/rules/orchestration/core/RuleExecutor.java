package com.rules.orchestration.core;

/**
 * High-performance rule executor interface
 *
 * Generated rule executor classes implement this interface.
 * Kept minimal for maximum JIT optimization.
 */
@FunctionalInterface
public interface RuleExecutor {

    /**
     * Execute rule with given context
     *
     * Implementations are generated from DSL and contain:
     * - Fully inlined logic for hot path rules (<=5 steps)
     * - Method-based execution for cold path rules (>5 steps)
     *
     * @param context Immutable transaction context
     * @return Rule execution result
     */
    RuleResult execute(TransactionContext context);
}