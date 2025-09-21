package com.rules.orchestration.core;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Universal Transaction Router - Hard-coded for maximum performance
 * Optimized for 80K+ TPS with sub-millisecond latency
 *
 * This class NEVER changes - all customization happens in generated ClientRuleMap implementations
 */
public final class TransactionRouter {

    // Registry of client-specific rule maps (populated at startup)
    private static final Map<String, ClientRuleMap> CLIENT_ROUTERS = new ConcurrentHashMap<>(256);

    // Global performance metrics
    private static final PerformanceMetrics globalMetrics = new PerformanceMetrics("global");

    // Throughput monitoring for 80K+ TPS validation
    private static final ThroughputMonitor throughputMonitor = new ThroughputMonitor();

    // Error handling
    private static final ErrorHandler errorHandler = new ErrorHandler();

    /**
     * Register a client rule map (called during application startup)
     */
    public static void registerClient(String clientId, ClientRuleMap ruleMap) {
        CLIENT_ROUTERS.put(clientId, ruleMap);
        globalMetrics.incrementClientRegistrations();
    }

    /**
     * MAIN ENTRY POINT - Route transaction to appropriate client rule executor
     *
     * This method is hand-optimized for maximum throughput:
     * - No reflection or dynamic lookups
     * - Minimal object allocation
     * - JIT-friendly code patterns
     * - Cache-line optimized data access
     */
    public static RuleResult route(String clientId, String transactionCode, TransactionContext context) {
        // Start performance tracking
        long startNanos = System.nanoTime();
        throughputMonitor.recordRequest();

        try {
            // Fast path: O(1) client lookup
            ClientRuleMap clientRules = CLIENT_ROUTERS.get(clientId);
            if (clientRules == null) {
                globalMetrics.incrementUnknownClients();
                return RuleResult.unknownClient(clientId);
            }

            // Delegate to client-specific router (generated code)
            globalMetrics.incrementTotalTransactions();
            RuleResult result = clientRules.execute(transactionCode, context);

            // Record successful execution
            long executionTime = System.nanoTime() - startNanos;
            globalMetrics.recordExecutionTime(executionTime);
            throughputMonitor.recordSuccess(executionTime);

            return result;

        } catch (Exception e) {
            // Fast error handling
            long executionTime = System.nanoTime() - startNanos;
            globalMetrics.incrementErrors();
            throughputMonitor.recordError(executionTime);

            return errorHandler.handleException(clientId, transactionCode, e);
        }
    }

    /**
     * Batch processing for high-throughput scenarios
     */
    public static List<RuleResult> routeBatch(String clientId,
                                             List<String> transactionCodes,
                                             List<TransactionContext> contexts) {
        if (transactionCodes.size() != contexts.size()) {
            throw new IllegalArgumentException("Transaction codes and contexts must have same size");
        }

        ClientRuleMap clientRules = CLIENT_ROUTERS.get(clientId);
        if (clientRules == null) {
            // Return batch of unknown client results
            return transactionCodes.stream()
                .map(RuleResult::unknownClient)
                .collect(java.util.stream.Collectors.toList());
        }

        // Process batch efficiently
        List<RuleResult> results = new ArrayList<>(transactionCodes.size());
        for (int i = 0; i < transactionCodes.size(); i++) {
            RuleResult result = route(clientId, transactionCodes.get(i), contexts.get(i));
            results.add(result);
        }

        return results;
    }

    /**
     * Get performance metrics for monitoring
     */
    public static PerformanceMetrics getGlobalMetrics() {
        return globalMetrics;
    }

    /**
     * Get throughput statistics
     */
    public static ThroughputMonitor getThroughputMonitor() {
        return throughputMonitor;
    }

    /**
     * Health check for deployment validation
     */
    public static boolean isHealthy() {
        return throughputMonitor.isHealthy() &&
               !CLIENT_ROUTERS.isEmpty() &&
               globalMetrics.getErrorRate() < 0.01; // Less than 1% error rate
    }

    /**
     * Get registered client count
     */
    public static int getRegisteredClientCount() {
        return CLIENT_ROUTERS.size();
    }

    /**
     * Warm up the router for optimal JIT compilation
     */
    public static void warmUp(String clientId, String sampleTransactionCode) {
        // Trigger JIT compilation with sample transactions
        TransactionContext sampleContext = ContextPool.acquireContext("warmup-txn");

        for (int i = 0; i < 10000; i++) {
            route(clientId, sampleTransactionCode, sampleContext);
        }

        ContextPool.releaseContext(sampleContext);
    }
}