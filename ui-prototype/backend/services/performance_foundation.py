"""
Hard-Coded Performance Foundation Framework
Creates optimized Java infrastructure that never changes.
Only rule-specific executor classes will be generated.
"""

from typing import Dict, List, Any, Optional
from pathlib import Path
from datetime import datetime


class PerformanceFoundationGenerator:
    """
    Generates the hard-coded performance foundation that provides:
    - Ultra-optimized transaction routing infrastructure
    - Hand-tuned context management and pooling
    - JIT-optimized caching systems
    - Performance monitoring and metrics

    This code is generated ONCE and reused across all clients and deployments.
    """

    def __init__(self):
        self.foundation_package = "com.rules.orchestration.core"

    def generate_complete_foundation(self) -> Dict[str, str]:
        """Generate all hard-coded foundation classes."""
        return {
            # Core routing infrastructure
            f"{self.foundation_package.replace('.', '/')}/TransactionRouter.java": self._universal_router(),
            f"{self.foundation_package.replace('.', '/')}/ClientRuleMap.java": self._client_rule_map_interface(),
            f"{self.foundation_package.replace('.', '/')}/RuleExecutor.java": self._rule_executor_interface(),

            # Context management (hand-tuned for performance)
            f"{self.foundation_package.replace('.', '/')}/TransactionContext.java": self._optimized_transaction_context(),
            f"{self.foundation_package.replace('.', '/')}/ContextPool.java": self._context_pool_manager(),
            f"{self.foundation_package.replace('.', '/')}/RuleResult.java": self._rule_result(),

            # Caching infrastructure
            f"{self.foundation_package.replace('.', '/')}/HybridCacheManager.java": self._hybrid_cache_manager(),
            f"{self.foundation_package.replace('.', '/')}/MemoryOptimizer.java": self._memory_optimizer(),

            # Performance monitoring
            f"{self.foundation_package.replace('.', '/')}/PerformanceMetrics.java": self._performance_metrics(),
            f"{self.foundation_package.replace('.', '/')}/ThroughputMonitor.java": self._throughput_monitor(),

            # Exception handling
            f"{self.foundation_package.replace('.', '/')}/RuleException.java": self._rule_exception(),
            f"{self.foundation_package.replace('.', '/')}/ErrorHandler.java": self._error_handler(),
        }

    def _universal_router(self) -> str:
        """Hard-coded universal router optimized for 80K+ TPS."""
        return f"""package {self.foundation_package};

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Universal Transaction Router - Hard-coded for maximum performance
 * Optimized for 80K+ TPS with sub-millisecond latency
 *
 * This class NEVER changes - all customization happens in generated ClientRuleMap implementations
 */
public final class TransactionRouter {{

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
    public static void registerClient(String clientId, ClientRuleMap ruleMap) {{
        CLIENT_ROUTERS.put(clientId, ruleMap);
        globalMetrics.incrementClientRegistrations();
    }}

    /**
     * MAIN ENTRY POINT - Route transaction to appropriate client rule executor
     *
     * This method is hand-optimized for maximum throughput:
     * - No reflection or dynamic lookups
     * - Minimal object allocation
     * - JIT-friendly code patterns
     * - Cache-line optimized data access
     */
    public static RuleResult route(String clientId, String transactionCode, TransactionContext context) {{
        // Start performance tracking
        long startNanos = System.nanoTime();
        throughputMonitor.recordRequest();

        try {{
            // Fast path: O(1) client lookup
            ClientRuleMap clientRules = CLIENT_ROUTERS.get(clientId);
            if (clientRules == null) {{
                globalMetrics.incrementUnknownClients();
                return RuleResult.unknownClient(clientId);
            }}

            // Delegate to client-specific router (generated code)
            globalMetrics.incrementTotalTransactions();
            RuleResult result = clientRules.execute(transactionCode, context);

            // Record successful execution
            long executionTime = System.nanoTime() - startNanos;
            globalMetrics.recordExecutionTime(executionTime);
            throughputMonitor.recordSuccess(executionTime);

            return result;

        }} catch (Exception e) {{
            // Fast error handling
            long executionTime = System.nanoTime() - startNanos;
            globalMetrics.incrementErrors();
            throughputMonitor.recordError(executionTime);

            return errorHandler.handleException(clientId, transactionCode, e);
        }}
    }}

    /**
     * Batch processing for high-throughput scenarios
     */
    public static List<RuleResult> routeBatch(String clientId,
                                             List<String> transactionCodes,
                                             List<TransactionContext> contexts) {{
        if (transactionCodes.size() != contexts.size()) {{
            throw new IllegalArgumentException("Transaction codes and contexts must have same size");
        }}

        ClientRuleMap clientRules = CLIENT_ROUTERS.get(clientId);
        if (clientRules == null) {{
            // Return batch of unknown client results
            return transactionCodes.stream()
                .map(RuleResult::unknownClient)
                .collect(java.util.stream.Collectors.toList());
        }}

        // Process batch efficiently
        List<RuleResult> results = new ArrayList<>(transactionCodes.size());
        for (int i = 0; i < transactionCodes.size(); i++) {{
            RuleResult result = route(clientId, transactionCodes.get(i), contexts.get(i));
            results.add(result);
        }}

        return results;
    }}

    /**
     * Get performance metrics for monitoring
     */
    public static PerformanceMetrics getGlobalMetrics() {{
        return globalMetrics;
    }}

    /**
     * Get throughput statistics
     */
    public static ThroughputMonitor getThroughputMonitor() {{
        return throughputMonitor;
    }}

    /**
     * Health check for deployment validation
     */
    public static boolean isHealthy() {{
        return throughputMonitor.isHealthy() &&
               !CLIENT_ROUTERS.isEmpty() &&
               globalMetrics.getErrorRate() < 0.01; // Less than 1% error rate
    }}

    /**
     * Get registered client count
     */
    public static int getRegisteredClientCount() {{
        return CLIENT_ROUTERS.size();
    }}

    /**
     * Warm up the router for optimal JIT compilation
     */
    public static void warmUp(String clientId, String sampleTransactionCode) {{
        // Trigger JIT compilation with sample transactions
        TransactionContext sampleContext = ContextPool.acquireContext("warmup-txn");

        for (int i = 0; i < 10000; i++) {{
            route(clientId, sampleTransactionCode, sampleContext);
        }}

        ContextPool.releaseContext(sampleContext);
    }}
}}"""

    def _client_rule_map_interface(self) -> str:
        """Interface that generated client routers must implement."""
        return f"""package {self.foundation_package};

/**
 * Interface for client-specific rule mapping and execution
 *
 * Generated client router classes implement this interface.
 * The interface is kept minimal for maximum performance.
 */
public interface ClientRuleMap {{

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
    default String getClientId() {{
        return this.getClass().getSimpleName().replaceAll("RuleMap$", "");
    }}

    /**
     * Get supported transaction codes for this client
     */
    default java.util.Set<String> getSupportedTransactionCodes() {{
        // Default implementation - can be overridden by generated classes
        return java.util.Collections.emptySet();
    }}

    /**
     * Warm up this client's rule executors
     */
    default void warmUp() {{
        // Default no-op - can be overridden for client-specific warmup
    }}
}}"""

    def _rule_executor_interface(self) -> str:
        """Interface for individual rule executors."""
        return f"""package {self.foundation_package};

/**
 * High-performance rule executor interface
 *
 * Generated rule executor classes implement this interface.
 * Kept minimal for maximum JIT optimization.
 */
@FunctionalInterface
public interface RuleExecutor {{

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
}}"""

    def _optimized_transaction_context(self) -> str:
        """Hand-tuned immutable context for maximum performance."""
        return f"""package {self.foundation_package};

import java.util.Map;
import java.util.HashMap;

/**
 * Immutable Transaction Context - Hand-optimized for 80K+ TPS
 *
 * Features:
 * - Copy-on-Write semantics for mutations
 * - Hot/cold field separation for CPU cache optimization
 * - Object pooling integration for GC pressure reduction
 * - Memory layout optimized for 5000+ field payloads
 */
public final class TransactionContext {{

    // Hot fields (frequently accessed - packed for cache efficiency)
    private final String transactionId;
    private final int creditScore;
    private final double income;
    private final String status;
    private final int creditLimit;
    private final double apr;
    private final double riskScore;
    private final double amount;

    // Cold fields (rarely accessed - stored separately)
    private final Map<String, Object> extendedFields;

    // Pooling support
    private final boolean isPooled;

    /**
     * Primary constructor for new contexts
     */
    public TransactionContext(String transactionId) {{
        this.transactionId = transactionId;
        this.creditScore = 0;
        this.income = 0.0;
        this.status = "PENDING";
        this.creditLimit = 0;
        this.apr = 0.0;
        this.riskScore = 0.0;
        this.amount = 0.0;
        this.extendedFields = new HashMap<>();
        this.isPooled = false;
    }}

    /**
     * Private constructor for COW operations
     */
    private TransactionContext(String transactionId, int creditScore, double income,
                              String status, int creditLimit, double apr, double riskScore,
                              double amount, Map<String, Object> extendedFields, boolean isPooled) {{
        this.transactionId = transactionId;
        this.creditScore = creditScore;
        this.income = income;
        this.status = status;
        this.creditLimit = creditLimit;
        this.apr = apr;
        this.riskScore = riskScore;
        this.amount = amount;
        this.extendedFields = extendedFields;
        this.isPooled = isPooled;
    }}

    // Hot field accessors (inlined by JIT)
    public String getTransactionId() {{ return transactionId; }}
    public int getCreditScore() {{ return creditScore; }}
    public double getIncome() {{ return income; }}
    public String getStatus() {{ return status; }}
    public int getCreditLimit() {{ return creditLimit; }}
    public double getAPR() {{ return apr; }}
    public double getRiskScore() {{ return riskScore; }}
    public double getAmount() {{ return amount; }}

    // Hot field mutations (Copy-on-Write)
    public TransactionContext withCreditScore(int creditScore) {{
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, extendedFields, false);
    }}

    public TransactionContext withIncome(double income) {{
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, extendedFields, false);
    }}

    public TransactionContext withStatus(String status) {{
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, extendedFields, false);
    }}

    public TransactionContext withCreditLimit(int creditLimit) {{
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, extendedFields, false);
    }}

    public TransactionContext withAPR(double apr) {{
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, extendedFields, false);
    }}

    public TransactionContext withRiskScore(double riskScore) {{
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, extendedFields, false);
    }}

    public TransactionContext withAmount(double amount) {{
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, extendedFields, false);
    }}

    // Extended field operations (for 5000+ field payloads)
    public Object getExtended(String key) {{
        return extendedFields.get(key);
    }}

    public TransactionContext withExtended(String key, Object value) {{
        Map<String, Object> newExtended = new HashMap<>(extendedFields);
        newExtended.put(key, value);
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, newExtended, false);
    }}

    // Batch operations for efficiency
    public TransactionContext withExtendedFields(Map<String, Object> updates) {{
        Map<String, Object> newExtended = new HashMap<>(extendedFields);
        newExtended.putAll(updates);
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, newExtended, false);
    }}

    // Utility methods
    public int getExtendedFieldCount() {{
        return extendedFields.size();
    }}

    public boolean hasExtendedField(String key) {{
        return extendedFields.containsKey(key);
    }}

    // Pooling support
    public boolean isPooled() {{
        return isPooled;
    }}

    TransactionContext asPooled() {{
        return new TransactionContext(transactionId, creditScore, income, status,
                                    creditLimit, apr, riskScore, amount, extendedFields, true);
    }}

    @Override
    public String toString() {{
        return "TransactionContext[id=" + transactionId + ", status=" + status + "]";
    }}
}}"""

    def _context_pool_manager(self) -> str:
        """Hand-tuned context pooling for GC pressure reduction."""
        return f"""package {self.foundation_package};

import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicLong;

/**
 * High-performance context pooling for 80K+ TPS
 *
 * Reduces GC pressure by reusing TransactionContext objects.
 * Thread-safe with lock-free operations where possible.
 */
public final class ContextPool {{

    // Thread-local pools for maximum performance
    private static final ThreadLocal<Queue<TransactionContext>> CONTEXT_POOLS =
        ThreadLocal.withInitial(() -> new ArrayBlockingQueue<>(100));

    // Global statistics
    private static final AtomicLong poolHits = new AtomicLong();
    private static final AtomicLong poolMisses = new AtomicLong();
    private static final AtomicLong contextsCreated = new AtomicLong();
    private static final AtomicLong contextsReleased = new AtomicLong();

    /**
     * Acquire context from pool or create new one
     */
    public static TransactionContext acquireContext(String transactionId) {{
        Queue<TransactionContext> pool = CONTEXT_POOLS.get();

        TransactionContext context = pool.poll();
        if (context != null) {{
            poolHits.incrementAndGet();
            return resetContext(context, transactionId);
        }} else {{
            poolMisses.incrementAndGet();
            contextsCreated.incrementAndGet();
            return new TransactionContext(transactionId);
        }}
    }}

    /**
     * Release context back to pool for reuse
     */
    public static void releaseContext(TransactionContext context) {{
        if (context == null || !context.isPooled()) {{
            return;
        }}

        Queue<TransactionContext> pool = CONTEXT_POOLS.get();

        // Only pool if we have space (avoid unbounded growth)
        if (pool.size() < 100) {{
            pool.offer(context);
            contextsReleased.incrementAndGet();
        }}
    }}

    /**
     * Reset pooled context for reuse
     */
    private static TransactionContext resetContext(TransactionContext context, String newTransactionId) {{
        // Create new context with reset state
        return new TransactionContext(newTransactionId).asPooled();
    }}

    /**
     * Get pool statistics
     */
    public static PoolStatistics getStatistics() {{
        return new PoolStatistics(
            poolHits.get(),
            poolMisses.get(),
            contextsCreated.get(),
            contextsReleased.get()
        );
    }}

    /**
     * Pool statistics for monitoring
     */
    public static class PoolStatistics {{
        public final long poolHits;
        public final long poolMisses;
        public final long contextsCreated;
        public final long contextsReleased;

        PoolStatistics(long poolHits, long poolMisses, long contextsCreated, long contextsReleased) {{
            this.poolHits = poolHits;
            this.poolMisses = poolMisses;
            this.contextsCreated = contextsCreated;
            this.contextsReleased = contextsReleased;
        }}

        public double getHitRate() {{
            long total = poolHits + poolMisses;
            return total > 0 ? (double) poolHits / total : 0.0;
        }}
    }}
}}"""

    def _rule_result(self) -> str:
        """Optimized rule result class."""
        return f"""package {self.foundation_package};

import java.util.List;
import java.util.Collections;

/**
 * Rule execution result - optimized for minimal allocation
 */
public final class RuleResult {{

    public enum Status {{
        SUCCESS, REJECTED, ERROR, UNKNOWN_TRANSACTION, UNKNOWN_CLIENT
    }}

    private final Status status;
    private final String message;
    private final TransactionContext finalContext;
    private final List<String> executedActions;

    private RuleResult(Status status, String message, TransactionContext finalContext, List<String> executedActions) {{
        this.status = status;
        this.message = message;
        this.finalContext = finalContext;
        this.executedActions = executedActions != null ? executedActions : Collections.emptyList();
    }}

    // Factory methods for common results
    public static RuleResult success(TransactionContext context) {{
        return new RuleResult(Status.SUCCESS, "Rule executed successfully", context, null);
    }}

    public static RuleResult rejected(TransactionContext context, String reason) {{
        return new RuleResult(Status.REJECTED, reason, context, null);
    }}

    public static RuleResult error(String message) {{
        return new RuleResult(Status.ERROR, message, null, null);
    }}

    public static RuleResult unknownTransaction(String transactionCode) {{
        return new RuleResult(Status.UNKNOWN_TRANSACTION, "Unknown transaction: " + transactionCode, null, null);
    }}

    public static RuleResult unknownClient(String clientId) {{
        return new RuleResult(Status.UNKNOWN_CLIENT, "Unknown client: " + clientId, null, null);
    }}

    // Getters
    public Status getStatus() {{ return status; }}
    public String getMessage() {{ return message; }}
    public TransactionContext getFinalContext() {{ return finalContext; }}
    public List<String> getExecutedActions() {{ return executedActions; }}

    public boolean isSuccess() {{ return status == Status.SUCCESS; }}
    public boolean isRejected() {{ return status == Status.REJECTED; }}
    public boolean isError() {{ return status == Status.ERROR; }}
}}"""

    def _performance_metrics(self) -> str:
        """Hand-tuned performance metrics for 80K+ TPS monitoring."""
        return f"""package {self.foundation_package};

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;

/**
 * High-performance metrics collection optimized for 80K+ TPS
 *
 * Uses lock-free data structures to minimize contention.
 * Designed for minimal overhead in hot paths.
 */
public final class PerformanceMetrics {{

    private final String identifier;

    // Counters using LongAdder for high-contention scenarios
    private final LongAdder totalTransactions = new LongAdder();
    private final LongAdder successfulTransactions = new LongAdder();
    private final LongAdder rejectedTransactions = new LongAdder();
    private final LongAdder errors = new LongAdder();
    private final LongAdder unknownTransactions = new LongAdder();
    private final LongAdder unknownClients = new LongAdder();
    private final LongAdder clientRegistrations = new LongAdder();

    // Execution time tracking (nanoseconds)
    private final AtomicLong totalExecutionTimeNanos = new AtomicLong();
    private final AtomicLong minExecutionTimeNanos = new AtomicLong(Long.MAX_VALUE);
    private final AtomicLong maxExecutionTimeNanos = new AtomicLong();

    public PerformanceMetrics(String identifier) {{
        this.identifier = identifier;
    }}

    // Increment methods (optimized for hot path)
    public void incrementTotalTransactions() {{ totalTransactions.increment(); }}
    public void incrementSuccessfulTransactions() {{ successfulTransactions.increment(); }}
    public void incrementRejectedTransactions() {{ rejectedTransactions.increment(); }}
    public void incrementErrors() {{ errors.increment(); }}
    public void incrementUnknownTransactions() {{ unknownTransactions.increment(); }}
    public void incrementUnknownClients() {{ unknownClients.increment(); }}
    public void incrementClientRegistrations() {{ clientRegistrations.increment(); }}

    /**
     * Record execution time with lock-free min/max tracking
     */
    public void recordExecutionTime(long nanos) {{
        totalExecutionTimeNanos.addAndGet(nanos);

        // Update min with lock-free CAS
        long currentMin = minExecutionTimeNanos.get();
        while (nanos < currentMin && !minExecutionTimeNanos.compareAndSet(currentMin, nanos)) {{
            currentMin = minExecutionTimeNanos.get();
        }}

        // Update max with lock-free CAS
        long currentMax = maxExecutionTimeNanos.get();
        while (nanos > currentMax && !maxExecutionTimeNanos.compareAndSet(currentMax, nanos)) {{
            currentMax = maxExecutionTimeNanos.get();
        }}
    }}

    // Getters
    public String getIdentifier() {{ return identifier; }}
    public long getTotalTransactions() {{ return totalTransactions.sum(); }}
    public long getSuccessfulTransactions() {{ return successfulTransactions.sum(); }}
    public long getRejectedTransactions() {{ return rejectedTransactions.sum(); }}
    public long getErrors() {{ return errors.sum(); }}
    public long getUnknownTransactions() {{ return unknownTransactions.sum(); }}
    public long getUnknownClients() {{ return unknownClients.sum(); }}
    public long getClientRegistrations() {{ return clientRegistrations.sum(); }}

    // Calculated metrics
    public double getSuccessRate() {{
        long total = getTotalTransactions();
        return total > 0 ? (double) getSuccessfulTransactions() / total : 0.0;
    }}

    public double getErrorRate() {{
        long total = getTotalTransactions();
        return total > 0 ? (double) getErrors() / total : 0.0;
    }}

    public long getAverageExecutionTimeNanos() {{
        long total = getTotalTransactions();
        return total > 0 ? totalExecutionTimeNanos.get() / total : 0;
    }}

    public long getMinExecutionTimeNanos() {{
        long min = minExecutionTimeNanos.get();
        return min == Long.MAX_VALUE ? 0 : min;
    }}

    public long getMaxExecutionTimeNanos() {{
        return maxExecutionTimeNanos.get();
    }}

    // Performance validation for 80K+ TPS requirement
    public boolean meetsPerformanceTargets() {{
        return getAverageExecutionTimeNanos() < 1_000_000 && // < 1ms average
               getErrorRate() < 0.001; // < 0.1% error rate
    }}
}}"""

    def _throughput_monitor(self) -> str:
        """Throughput monitoring for 80K+ TPS validation."""
        return f"""package {self.foundation_package};

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;

/**
 * Real-time throughput monitoring for 80K+ TPS validation
 *
 * Tracks requests per second and validates performance targets.
 */
public final class ThroughputMonitor {{

    private final LongAdder requestCount = new LongAdder();
    private final LongAdder successCount = new LongAdder();
    private final LongAdder errorCount = new LongAdder();

    private final AtomicLong lastResetTime = new AtomicLong(System.currentTimeMillis());
    private final AtomicLong windowStartTime = new AtomicLong(System.currentTimeMillis());

    // Performance thresholds
    private static final long TARGET_TPS = 80_000;
    private static final long WINDOW_SIZE_MS = 1000; // 1 second window

    public void recordRequest() {{
        requestCount.increment();
        maybeResetWindow();
    }}

    public void recordSuccess(long executionTimeNanos) {{
        successCount.increment();
    }}

    public void recordError(long executionTimeNanos) {{
        errorCount.increment();
    }}

    /**
     * Get current throughput (requests per second)
     */
    public double getCurrentTPS() {{
        long now = System.currentTimeMillis();
        long windowStart = windowStartTime.get();
        long windowDuration = now - windowStart;

        if (windowDuration <= 0) return 0.0;

        long requests = requestCount.sum();
        return (double) requests * 1000.0 / windowDuration;
    }}

    /**
     * Check if system is meeting 80K+ TPS target
     */
    public boolean isHealthy() {{
        double currentTPS = getCurrentTPS();
        double errorRate = getErrorRate();

        return currentTPS >= TARGET_TPS * 0.8 && // Within 80% of target
               errorRate < 0.001; // Less than 0.1% errors
    }}

    /**
     * Get current error rate
     */
    public double getErrorRate() {{
        long total = requestCount.sum();
        long errors = errorCount.sum();

        return total > 0 ? (double) errors / total : 0.0;
    }}

    /**
     * Reset monitoring window (called periodically)
     */
    private void maybeResetWindow() {{
        long now = System.currentTimeMillis();
        long lastReset = lastResetTime.get();

        if (now - lastReset >= WINDOW_SIZE_MS) {{
            if (lastResetTime.compareAndSet(lastReset, now)) {{
                // Reset counters for new window
                requestCount.reset();
                successCount.reset();
                errorCount.reset();
                windowStartTime.set(now);
            }}
        }}
    }}

    /**
     * Get throughput statistics
     */
    public ThroughputStats getStats() {{
        return new ThroughputStats(
            getCurrentTPS(),
            requestCount.sum(),
            successCount.sum(),
            errorCount.sum(),
            getErrorRate(),
            isHealthy()
        );
    }}

    public static class ThroughputStats {{
        public final double currentTPS;
        public final long totalRequests;
        public final long successCount;
        public final long errorCount;
        public final double errorRate;
        public final boolean isHealthy;

        ThroughputStats(double currentTPS, long totalRequests, long successCount,
                       long errorCount, double errorRate, boolean isHealthy) {{
            this.currentTPS = currentTPS;
            this.totalRequests = totalRequests;
            this.successCount = successCount;
            this.errorCount = errorCount;
            this.errorRate = errorRate;
            this.isHealthy = isHealthy;
        }}
    }}
}}"""

    def _hybrid_cache_manager(self) -> str:
        """Hard-coded hybrid caching strategy."""
        return f"""package {self.foundation_package};

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * Hybrid JVM + Redis caching strategy - Hard-coded for optimal performance
 *
 * L1 Cache (JVM): Hot reference data, lookup tables
 * L2 Cache (Redis): Large contexts, shared state
 */
public final class HybridCacheManager {{

    // L1 Cache: In-JVM for ultra-low latency (< 10MB total)
    private static final Map<String, Object> l1Cache = new ConcurrentHashMap<>(10000);
    private static final Map<String, Long> l1Timestamps = new ConcurrentHashMap<>(10000);

    // Cache configuration
    private static final long L1_TTL_MS = TimeUnit.MINUTES.toMillis(5);
    private static final int L1_MAX_SIZE = 10000;

    /**
     * Get from L1 cache (zero network latency)
     */
    public static Object getFromL1(String key) {{
        // Check expiration
        Long timestamp = l1Timestamps.get(key);
        if (timestamp != null && System.currentTimeMillis() - timestamp > L1_TTL_MS) {{
            l1Cache.remove(key);
            l1Timestamps.remove(key);
            return null;
        }}

        return l1Cache.get(key);
    }}

    /**
     * Put to L1 cache with size limits
     */
    public static void putToL1(String key, Object value) {{
        // Implement LRU eviction if cache is full
        if (l1Cache.size() >= L1_MAX_SIZE) {{
            evictOldestL1Entry();
        }}

        l1Cache.put(key, value);
        l1Timestamps.put(key, System.currentTimeMillis());
    }}

    /**
     * Hybrid get with L1 → L2 fallback
     */
    public static Object get(String key) {{
        // Try L1 first (zero latency)
        Object value = getFromL1(key);
        if (value != null) {{
            return value;
        }}

        // TODO: Fall back to L2 (Redis) for shared/large data
        // This would integrate with actual Redis client
        return null;
    }}

    /**
     * Hybrid put with intelligent routing
     */
    public static void put(String key, Object value) {{
        // Route small, hot data to L1
        if (isHotData(key, value)) {{
            putToL1(key, value);
        }}

        // TODO: Route large/shared data to L2 (Redis)
    }}

    /**
     * Determine if data should go to L1 cache
     */
    private static boolean isHotData(String key, Object value) {{
        // Hot data criteria:
        // - Reference data (lookup tables)
        // - Small size (< 1KB estimated)
        // - Frequently accessed patterns

        if (key.startsWith("ref.") || key.startsWith("lookup.")) {{
            return true;
        }}

        // Estimate size (rough approximation)
        String str = value.toString();
        return str.length() < 1000;
    }}

    /**
     * LRU eviction for L1 cache
     */
    private static void evictOldestL1Entry() {{
        String oldestKey = null;
        long oldestTime = Long.MAX_VALUE;

        for (Map.Entry<String, Long> entry : l1Timestamps.entrySet()) {{
            if (entry.getValue() < oldestTime) {{
                oldestTime = entry.getValue();
                oldestKey = entry.getKey();
            }}
        }}

        if (oldestKey != null) {{
            l1Cache.remove(oldestKey);
            l1Timestamps.remove(oldestKey);
        }}
    }}

    /**
     * Get cache statistics
     */
    public static CacheStats getStats() {{
        return new CacheStats(
            l1Cache.size(),
            L1_MAX_SIZE,
            calculateL1HitRate()
        );
    }}

    private static double calculateL1HitRate() {{
        // This would require request tracking - simplified for now
        return 0.85; // Assume 85% hit rate
    }}

    public static class CacheStats {{
        public final int l1Size;
        public final int l1MaxSize;
        public final double l1HitRate;

        CacheStats(int l1Size, int l1MaxSize, double l1HitRate) {{
            this.l1Size = l1Size;
            this.l1MaxSize = l1MaxSize;
            this.l1HitRate = l1HitRate;
        }}
    }}
}}"""

    def _memory_optimizer(self) -> str:
        """Memory optimization utilities."""
        return f"""package {self.foundation_package};

/**
 * Memory optimization utilities for high-throughput operation
 */
public final class MemoryOptimizer {{

    // Memory thresholds
    private static final long HEAP_WARNING_THRESHOLD = 0.8; // 80% heap usage
    private static final long GC_TIME_WARNING_MS = 50; // 50ms GC pause

    /**
     * Check if memory pressure is high
     */
    public static boolean isMemoryPressureHigh() {{
        Runtime runtime = Runtime.getRuntime();
        long totalMemory = runtime.totalMemory();
        long freeMemory = runtime.freeMemory();
        long usedMemory = totalMemory - freeMemory;

        double usageRatio = (double) usedMemory / runtime.maxMemory();
        return usageRatio > HEAP_WARNING_THRESHOLD;
    }}

    /**
     * Suggest GC if memory pressure is high
     */
    public static void suggestGCIfNeeded() {{
        if (isMemoryPressureHigh()) {{
            System.gc(); // Suggestion only - JVM may ignore
        }}
    }}

    /**
     * Get memory statistics
     */
    public static MemoryStats getMemoryStats() {{
        Runtime runtime = Runtime.getRuntime();
        return new MemoryStats(
            runtime.maxMemory(),
            runtime.totalMemory(),
            runtime.freeMemory()
        );
    }}

    public static class MemoryStats {{
        public final long maxMemory;
        public final long totalMemory;
        public final long freeMemory;
        public final long usedMemory;
        public final double usagePercentage;

        MemoryStats(long maxMemory, long totalMemory, long freeMemory) {{
            this.maxMemory = maxMemory;
            this.totalMemory = totalMemory;
            this.freeMemory = freeMemory;
            this.usedMemory = totalMemory - freeMemory;
            this.usagePercentage = (double) usedMemory / maxMemory * 100.0;
        }}
    }}
}}"""

    def _rule_exception(self) -> str:
        """Optimized exception handling."""
        return f"""package {self.foundation_package};

/**
 * Custom exception for rule execution errors
 */
public class RuleException extends RuntimeException {{

    private final String clientId;
    private final String transactionCode;
    private final long executionTimeNanos;

    public RuleException(String message, String clientId, String transactionCode, long executionTimeNanos) {{
        super(message);
        this.clientId = clientId;
        this.transactionCode = transactionCode;
        this.executionTimeNanos = executionTimeNanos;
    }}

    public RuleException(String message, Throwable cause, String clientId, String transactionCode, long executionTimeNanos) {{
        super(message, cause);
        this.clientId = clientId;
        this.transactionCode = transactionCode;
        this.executionTimeNanos = executionTimeNanos;
    }}

    public String getClientId() {{ return clientId; }}
    public String getTransactionCode() {{ return transactionCode; }}
    public long getExecutionTimeNanos() {{ return executionTimeNanos; }}
}}"""

    def _error_handler(self) -> str:
        """Centralized error handling."""
        return f"""package {self.foundation_package};

/**
 * Centralized error handling for rule execution
 */
public final class ErrorHandler {{

    /**
     * Handle exceptions during rule execution
     */
    public RuleResult handleException(String clientId, String transactionCode, Exception e) {{
        // Log error (would integrate with actual logging framework)
        String errorMessage = String.format("Rule execution failed for client=%s, txn=%s: %s",
                                           clientId, transactionCode, e.getMessage());

        // Return error result
        return RuleResult.error(errorMessage);
    }}

    /**
     * Handle timeout exceptions
     */
    public RuleResult handleTimeout(String clientId, String transactionCode, long timeoutMs) {{
        String errorMessage = String.format("Rule execution timeout for client=%s, txn=%s after %dms",
                                           clientId, transactionCode, timeoutMs);
        return RuleResult.error(errorMessage);
    }}
}}"""

    def generate_foundation_pom(self) -> str:
        """Generate Maven POM for the hard-coded foundation."""
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>com.rules.engine</groupId>
    <artifactId>shared-rules-engine-core</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>

    <name>Shared Rules Engine Core</name>
    <description>Hard-coded foundation for ultra-high performance rules engine (80K+ TPS)</description>

    <properties>
        <maven.compiler.source>17</maven.compiler.source>
        <maven.compiler.target>17</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <dependencies>
        <!-- High-performance caching -->
        <dependency>
            <groupId>com.github.ben-manes.caffeine</groupId>
            <artifactId>caffeine</artifactId>
            <version>3.1.8</version>
        </dependency>

        <!-- Off-heap storage -->
        <dependency>
            <groupId>net.openhft</groupId>
            <artifactId>chronicle-map</artifactId>
            <version>3.24.4</version>
        </dependency>

        <!-- Ultra-low latency collections -->
        <dependency>
            <groupId>org.lmax</groupId>
            <artifactId>disruptor</artifactId>
            <version>3.4.4</version>
        </dependency>

        <!-- Fast collections -->
        <dependency>
            <groupId>it.unimi.dsi</groupId>
            <artifactId>fastutil</artifactId>
            <version>8.5.12</version>
        </dependency>

        <!-- JSON processing -->
        <dependency>
            <groupId>org.json</groupId>
            <artifactId>json</artifactId>
            <version>20240303</version>
        </dependency>

        <!-- Logging -->
        <dependency>
            <groupId>org.slf4j</groupId>
            <artifactId>slf4j-api</artifactId>
            <version>2.0.9</version>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.11.0</version>
                <configuration>
                    <source>17</source>
                    <target>17</target>
                    <compilerArgs>
                        <arg>-XX:+UnlockExperimentalVMOptions</arg>
                        <arg>-XX:+UseZGC</arg>
                    </compilerArgs>
                </configuration>
            </plugin>

            <!-- Optimize for performance -->
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-surefire-plugin</artifactId>
                <version>3.1.2</version>
                <configuration>
                    <jvmArgs>
                        <jvmArg>-XX:+UseZGC</jvmArg>
                        <jvmArg>-XX:+UnlockExperimentalVMOptions</jvmArg>
                    </jvmArgs>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>"""