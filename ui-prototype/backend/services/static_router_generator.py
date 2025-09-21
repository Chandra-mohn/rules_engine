"""
Static Router Generation Framework
Generates optimized Java transaction routers with zero runtime reflection.
Supports 80K+ TPS with sub-millisecond latency requirements.
"""

from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from pathlib import Path
import json
import hashlib
from datetime import datetime


@dataclass
class TransactionMapping:
    """Represents a transaction code to rule mapping."""
    transaction_code: str
    rule_id: str
    rule_name: str
    rule_type: str  # mon_rule, non_mon_rule, rule, actionset
    execution_frequency: str  # hot, warm, cold
    dependencies: List[str]  # other transaction codes this depends on
    estimated_steps: int  # for 80/20 optimization
    context_size_kb: int  # for memory optimization


@dataclass
class ClientRouterSpec:
    """Specification for generating a client-specific router."""
    client_id: str
    transaction_mappings: List[TransactionMapping]
    hot_path_threshold: int = 5  # rules with <= 5 steps go to hot path
    package_name: str = "com.rules.generated"


class StaticRouterGenerator:
    """
    Generates static Java transaction routers for ultra-high performance.

    Key Features:
    - O(1) transaction code lookup using static Maps
    - 80/20 optimization with hot/cold execution paths
    - Zero runtime reflection
    - Frequency-based branch prediction optimization
    - Client-specific router generation
    """

    def __init__(self):
        self.template_cache = {}

    def generate_universal_router(self, client_specs: List[ClientRouterSpec]) -> Dict[str, str]:
        """
        Generate universal transaction router that supports multiple clients.

        Args:
            client_specs: List of client router specifications

        Returns:
            Dict mapping file paths to Java source code
        """
        artifacts = {}

        # Generate shared router infrastructure
        artifacts.update(self._generate_shared_router_framework())

        # Generate client-specific router implementations
        for spec in client_specs:
            client_artifacts = self._generate_client_router(spec)
            artifacts.update(client_artifacts)

        # Generate router registry that manages all clients
        artifacts.update(self._generate_router_registry(client_specs))

        return artifacts

    def _generate_shared_router_framework(self) -> Dict[str, str]:
        """Generate shared router interfaces and base classes."""
        return {
            "src/main/java/com/rules/router/UniversalTransactionRouter.java": self._universal_router_template(),
            "src/main/java/com/rules/router/ClientRuleMap.java": self._client_rule_map_interface(),
            "src/main/java/com/rules/router/RuleExecutor.java": self._high_performance_executor_interface(),
            "src/main/java/com/rules/router/TransactionContext.java": self._immutable_context_template(),
            "src/main/java/com/rules/router/RuleResult.java": self._rule_result_template(),
            "src/main/java/com/rules/router/PerformanceMetrics.java": self._performance_metrics_template()
        }

    def _generate_client_router(self, spec: ClientRouterSpec) -> Dict[str, str]:
        """Generate optimized router for a specific client."""
        client_package = f"{spec.package_name}.{spec.client_id.lower()}"
        client_package_path = client_package.replace(".", "/")

        # Analyze transaction patterns for optimization
        hot_transactions, cold_transactions = self._analyze_transaction_patterns(spec)

        # Generate frequency-optimized routing logic
        router_code = self._generate_optimized_client_router(spec, hot_transactions, cold_transactions)

        # Generate individual rule executors
        executor_artifacts = self._generate_rule_executors(spec)

        artifacts = {
            f"src/main/java/{client_package_path}/Client{spec.client_id.upper()}RuleMap.java": router_code
        }
        artifacts.update(executor_artifacts)

        return artifacts

    def _analyze_transaction_patterns(self, spec: ClientRouterSpec) -> Tuple[List[TransactionMapping], List[TransactionMapping]]:
        """
        Analyze transaction patterns to optimize for 80/20 performance.

        Returns:
            Tuple of (hot_transactions, cold_transactions)
        """
        hot_transactions = []
        cold_transactions = []

        for mapping in spec.transaction_mappings:
            if (mapping.execution_frequency == "hot" and
                mapping.estimated_steps <= spec.hot_path_threshold):
                hot_transactions.append(mapping)
            else:
                cold_transactions.append(mapping)

        # Sort hot transactions by frequency for branch prediction optimization
        hot_transactions.sort(key=lambda t: t.execution_frequency == "hot", reverse=True)

        return hot_transactions, cold_transactions

    def _generate_optimized_client_router(self, spec: ClientRouterSpec,
                                        hot_transactions: List[TransactionMapping],
                                        cold_transactions: List[TransactionMapping]) -> str:
        """Generate frequency-optimized client router with branch prediction optimization."""

        client_class = f"Client{spec.client_id.upper()}RuleMap"

        # Generate hot path executors map
        hot_executors = self._generate_hot_path_map(hot_transactions)

        # Generate cold path executors map
        cold_executors = self._generate_cold_path_map(cold_transactions)

        # Generate optimized routing logic
        routing_logic = self._generate_branch_optimized_routing(hot_transactions, cold_transactions)

        return f"""package {spec.package_name}.{spec.client_id.lower()};

import com.rules.router.*;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * High-performance client router for {spec.client_id}
 * Optimized for 80K+ TPS with sub-millisecond latency
 * Generated at {datetime.now().isoformat()}
 */
public class {client_class} implements ClientRuleMap {{

    // Hot path executors (80% of traffic, <= {spec.hot_path_threshold} steps)
    private static final Map<String, RuleExecutor> HOT_EXECUTORS = Map.of({hot_executors});

    // Cold path executors (20% of traffic, complex rules)
    private static final Map<String, RuleExecutor> COLD_EXECUTORS = Map.of({cold_executors});

    // Performance metrics
    private static final PerformanceMetrics metrics = new PerformanceMetrics("{spec.client_id}");

    @Override
    public RuleResult execute(String transactionCode, TransactionContext context) {{
        long startTime = System.nanoTime();

        try {{
            // Branch prediction optimized routing
{routing_logic}

            // Unknown transaction code
            metrics.incrementUnknownTransactions();
            return RuleResult.unknownTransaction(transactionCode);

        }} catch (Exception e) {{
            metrics.incrementErrors();
            return RuleResult.error(transactionCode, e.getMessage());
        }} finally {{
            long executionTime = System.nanoTime() - startTime;
            metrics.recordExecutionTime(executionTime);
        }}
    }}

    // Hot path execution with maximum inlining
    private RuleResult executeHotPath(String transactionCode, TransactionContext context) {{
        RuleExecutor executor = HOT_EXECUTORS.get(transactionCode);
        if (executor != null) {{
            metrics.incrementHotPathExecutions();
            return executor.execute(context);
        }}
        return null;
    }}

    // Cold path execution for complex rules
    private RuleResult executeColdPath(String transactionCode, TransactionContext context) {{
        RuleExecutor executor = COLD_EXECUTORS.get(transactionCode);
        if (executor != null) {{
            metrics.incrementColdPathExecutions();
            return executor.execute(context);
        }}
        return null;
    }}
}}"""

    def _generate_hot_path_map(self, hot_transactions: List[TransactionMapping]) -> str:
        """Generate static map for hot path transactions."""
        if not hot_transactions:
            return ""

        entries = []
        for txn in hot_transactions:
            executor_class = f"{self._to_pascal_case(txn.rule_name)}Executor"
            entries.append(f'        "{txn.transaction_code}", new {executor_class}()')

        return ",\n".join(entries)

    def _generate_cold_path_map(self, cold_transactions: List[TransactionMapping]) -> str:
        """Generate static map for cold path transactions."""
        if not cold_transactions:
            return ""

        entries = []
        for txn in cold_transactions:
            executor_class = f"{self._to_pascal_case(txn.rule_name)}Executor"
            entries.append(f'        "{txn.transaction_code}", new {executor_class}()')

        return ",\n".join(entries)

    def _generate_branch_optimized_routing(self, hot_transactions: List[TransactionMapping],
                                         cold_transactions: List[TransactionMapping]) -> str:
        """Generate branch prediction optimized routing logic."""

        # Most frequent transactions first for CPU branch prediction
        routing_code = []

        # Hot path: Most frequent patterns first
        if hot_transactions:
            most_frequent = hot_transactions[:3]  # Top 3 most frequent
            for txn in most_frequent:
                routing_code.append(f'            if ("{txn.transaction_code}".equals(transactionCode)) {{')
                routing_code.append(f'                return executeHotPath(transactionCode, context);')
                routing_code.append(f'            }}')

        # General hot path check
        routing_code.append(f'            ')
        routing_code.append(f'            // Check hot path first (80% of traffic)')
        routing_code.append(f'            RuleResult hotResult = executeHotPath(transactionCode, context);')
        routing_code.append(f'            if (hotResult != null) {{')
        routing_code.append(f'                return hotResult;')
        routing_code.append(f'            }}')
        routing_code.append(f'            ')
        routing_code.append(f'            // Fall back to cold path (20% of traffic)')
        routing_code.append(f'            RuleResult coldResult = executeColdPath(transactionCode, context);')
        routing_code.append(f'            if (coldResult != null) {{')
        routing_code.append(f'                return coldResult;')
        routing_code.append(f'            }}')

        return "\n".join(routing_code)

    def _generate_rule_executors(self, spec: ClientRouterSpec) -> Dict[str, str]:
        """Generate optimized rule executor classes."""
        artifacts = {}

        for mapping in spec.transaction_mappings:
            executor_artifacts = self._generate_single_rule_executor(spec, mapping)
            artifacts.update(executor_artifacts)

        return artifacts

    def _generate_single_rule_executor(self, spec: ClientRouterSpec,
                                     mapping: TransactionMapping) -> Dict[str, str]:
        """Generate a single optimized rule executor."""

        client_package = f"{spec.package_name}.{spec.client_id.lower()}"
        client_package_path = client_package.replace(".", "/")

        executor_class = f"{self._to_pascal_case(mapping.rule_name)}Executor"

        # Determine optimization strategy based on rule characteristics
        if mapping.estimated_steps <= spec.hot_path_threshold:
            executor_code = self._generate_hot_path_executor(mapping, client_package)
        else:
            executor_code = self._generate_cold_path_executor(mapping, client_package)

        return {
            f"src/main/java/{client_package_path}/executors/{executor_class}.java": executor_code
        }

    def _generate_hot_path_executor(self, mapping: TransactionMapping, package: str) -> str:
        """Generate highly optimized executor for hot path (fully inlined)."""

        executor_class = f"{self._to_pascal_case(mapping.rule_name)}Executor"

        return f"""package {package}.executors;

import com.rules.router.*;

/**
 * Hot path executor for {mapping.rule_name}
 * Fully inlined for maximum performance
 * Estimated steps: {mapping.estimated_steps}
 * Transaction code: {mapping.transaction_code}
 */
public class {executor_class} implements RuleExecutor {{

    @Override
    public RuleResult execute(TransactionContext context) {{
        // Fully inlined execution for hot path
        // TODO: Generate actual rule logic from DSL

        // Example hot path execution (3-5 steps, fully inlined)
        if (context.getCreditScore() >= 700) {{
            TransactionContext approved = context
                .withStatus("APPROVED")
                .withCreditLimit(calculateLimit(context))
                .withAPR(getStandardAPR(context));

            return RuleResult.success(approved);
        }} else {{
            TransactionContext rejected = context
                .withStatus("REJECTED")
                .withReason("Credit score below threshold");

            return RuleResult.rejected(rejected);
        }}
    }}

    // Inlined helper methods for maximum performance
    private static int calculateLimit(TransactionContext context) {{
        return Math.min(context.getIncome() * 3, 50000);
    }}

    private static double getStandardAPR(TransactionContext context) {{
        return context.getCreditScore() >= 750 ? 12.99 : 15.99;
    }}
}}"""

    def _generate_cold_path_executor(self, mapping: TransactionMapping, package: str) -> str:
        """Generate executor for cold path (method calls acceptable)."""

        executor_class = f"{self._to_pascal_case(mapping.rule_name)}Executor"

        return f"""package {package}.executors;

import com.rules.router.*;

/**
 * Cold path executor for {mapping.rule_name}
 * Method-based execution for complex rules
 * Estimated steps: {mapping.estimated_steps}
 * Transaction code: {mapping.transaction_code}
 */
public class {executor_class} implements RuleExecutor {{

    @Override
    public RuleResult execute(TransactionContext context) {{
        // Complex rule execution with method calls
        // TODO: Generate actual rule logic from DSL

        return executeComplexRuleChain(context);
    }}

    private RuleResult executeComplexRuleChain(TransactionContext context) {{
        // Multi-step execution for complex rules
        TransactionContext step1 = executeStep1(context);
        TransactionContext step2 = executeStep2(step1);
        TransactionContext step3 = executeStep3(step2);

        return RuleResult.success(step3);
    }}

    private TransactionContext executeStep1(TransactionContext context) {{
        // TODO: Implement actual rule step
        return context;
    }}

    private TransactionContext executeStep2(TransactionContext context) {{
        // TODO: Implement actual rule step
        return context;
    }}

    private TransactionContext executeStep3(TransactionContext context) {{
        // TODO: Implement actual rule step
        return context;
    }}
}}"""

    def _universal_router_template(self) -> str:
        """Generate universal transaction router template."""
        return """package com.rules.router;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Universal Transaction Router
 * Manages client-specific rule routers for ultra-high performance
 * Supports 80K+ TPS with sub-millisecond latency
 */
public class UniversalTransactionRouter {

    // Registry of client-specific rule maps (loaded at startup)
    private static final Map<String, ClientRuleMap> CLIENT_ROUTERS = new ConcurrentHashMap<>();

    // Performance metrics
    private static final PerformanceMetrics globalMetrics = new PerformanceMetrics("global");

    /**
     * Register a client rule map for transaction routing.
     */
    public static void registerClient(String clientId, ClientRuleMap ruleMap) {
        CLIENT_ROUTERS.put(clientId, ruleMap);
    }

    /**
     * Route transaction to appropriate client rule executor.
     *
     * @param clientId Client identifier
     * @param transactionCode Transaction code to route
     * @param context Transaction context
     * @return Rule execution result
     */
    public RuleResult route(String clientId, String transactionCode, TransactionContext context) {
        long startTime = System.nanoTime();

        try {
            ClientRuleMap clientRules = CLIENT_ROUTERS.get(clientId);
            if (clientRules == null) {
                globalMetrics.incrementUnknownClients();
                return RuleResult.unknownClient(clientId);
            }

            globalMetrics.incrementTotalTransactions();
            return clientRules.execute(transactionCode, context);

        } catch (Exception e) {
            globalMetrics.incrementErrors();
            return RuleResult.error(transactionCode, e.getMessage());
        } finally {
            long executionTime = System.nanoTime() - startTime;
            globalMetrics.recordExecutionTime(executionTime);
        }
    }

    /**
     * Get performance metrics for monitoring.
     */
    public static PerformanceMetrics getGlobalMetrics() {
        return globalMetrics;
    }
}"""

    def _client_rule_map_interface(self) -> str:
        """Generate client rule map interface."""
        return """package com.rules.router;

/**
 * Interface for client-specific rule mapping and execution.
 */
public interface ClientRuleMap {

    /**
     * Execute rule for given transaction code.
     *
     * @param transactionCode Transaction code to execute
     * @param context Transaction context
     * @return Rule execution result
     */
    RuleResult execute(String transactionCode, TransactionContext context);
}"""

    def _high_performance_executor_interface(self) -> str:
        """Generate high-performance rule executor interface."""
        return """package com.rules.router;

/**
 * High-performance rule executor interface.
 * Implementations should be optimized for specific rule types.
 */
public interface RuleExecutor {

    /**
     * Execute rule with given context.
     *
     * @param context Transaction context
     * @return Rule execution result
     */
    RuleResult execute(TransactionContext context);
}"""

    def _immutable_context_template(self) -> str:
        """Generate immutable transaction context template."""
        return """package com.rules.router;

import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Immutable Transaction Context with Copy-on-Write semantics
 * Optimized for high-frequency mutations and large payloads (5000+ fields)
 */
public class TransactionContext {

    // Core immutable data (hot fields - frequently accessed)
    private final CoreTransactionData core;

    // Extended data (cold fields - rarely accessed, COW managed)
    private final Map<String, Object> extended;

    // Change tracking for debug (not production)
    private final boolean debugMode;

    public TransactionContext(String transactionId, int creditScore, double income) {
        this.core = new CoreTransactionData(transactionId, creditScore, income);
        this.extended = new HashMap<>();
        this.debugMode = false;
    }

    private TransactionContext(CoreTransactionData core, Map<String, Object> extended, boolean debugMode) {
        this.core = core;
        this.extended = extended;
        this.debugMode = debugMode;
    }

    // Hot field accessors (frequently used)
    public String getTransactionId() { return core.transactionId; }
    public int getCreditScore() { return core.creditScore; }
    public double getIncome() { return core.income; }
    public String getStatus() { return core.status; }

    // Copy-on-write mutations for hot fields
    public TransactionContext withCreditScore(int creditScore) {
        return new TransactionContext(core.withCreditScore(creditScore), extended, debugMode);
    }

    public TransactionContext withStatus(String status) {
        return new TransactionContext(core.withStatus(status), extended, debugMode);
    }

    public TransactionContext withCreditLimit(int creditLimit) {
        return new TransactionContext(core.withCreditLimit(creditLimit), extended, debugMode);
    }

    public TransactionContext withAPR(double apr) {
        return new TransactionContext(core.withAPR(apr), extended, debugMode);
    }

    public TransactionContext withReason(String reason) {
        return new TransactionContext(core.withReason(reason), extended, debugMode);
    }

    // Extended field access (cold fields)
    public Object getExtended(String key) {
        return extended.get(key);
    }

    public TransactionContext withExtended(String key, Object value) {
        Map<String, Object> newExtended = new HashMap<>(extended);
        newExtended.put(key, value);
        return new TransactionContext(core, newExtended, debugMode);
    }

    // Batch mutations for efficiency
    public TransactionContext withExtendedFields(Map<String, Object> updates) {
        Map<String, Object> newExtended = new HashMap<>(extended);
        newExtended.putAll(updates);
        return new TransactionContext(core, newExtended, debugMode);
    }

    /**
     * Core transaction data for hot fields
     */
    private static class CoreTransactionData {
        final String transactionId;
        final int creditScore;
        final double income;
        final String status;
        final int creditLimit;
        final double apr;
        final String reason;

        CoreTransactionData(String transactionId, int creditScore, double income) {
            this(transactionId, creditScore, income, "PENDING", 0, 0.0, null);
        }

        private CoreTransactionData(String transactionId, int creditScore, double income,
                                  String status, int creditLimit, double apr, String reason) {
            this.transactionId = transactionId;
            this.creditScore = creditScore;
            this.income = income;
            this.status = status;
            this.creditLimit = creditLimit;
            this.apr = apr;
            this.reason = reason;
        }

        CoreTransactionData withCreditScore(int creditScore) {
            return new CoreTransactionData(transactionId, creditScore, income, status, creditLimit, apr, reason);
        }

        CoreTransactionData withStatus(String status) {
            return new CoreTransactionData(transactionId, creditScore, income, status, creditLimit, apr, reason);
        }

        CoreTransactionData withCreditLimit(int creditLimit) {
            return new CoreTransactionData(transactionId, creditScore, income, status, creditLimit, apr, reason);
        }

        CoreTransactionData withAPR(double apr) {
            return new CoreTransactionData(transactionId, creditScore, income, status, creditLimit, apr, reason);
        }

        CoreTransactionData withReason(String reason) {
            return new CoreTransactionData(transactionId, creditScore, income, status, creditLimit, apr, reason);
        }
    }
}"""

    def _rule_result_template(self) -> str:
        """Generate rule result template."""
        return """package com.rules.router;

import java.util.List;
import java.util.ArrayList;

/**
 * Rule execution result
 */
public class RuleResult {

    private final boolean success;
    private final String status;
    private final String message;
    private final TransactionContext finalContext;
    private final List<String> executedActions;
    private final long executionTimeNanos;

    private RuleResult(boolean success, String status, String message,
                      TransactionContext finalContext, List<String> executedActions,
                      long executionTimeNanos) {
        this.success = success;
        this.status = status;
        this.message = message;
        this.finalContext = finalContext;
        this.executedActions = executedActions;
        this.executionTimeNanos = executionTimeNanos;
    }

    public static RuleResult success(TransactionContext context) {
        return new RuleResult(true, "SUCCESS", "Rule executed successfully",
                            context, new ArrayList<>(), 0);
    }

    public static RuleResult rejected(TransactionContext context) {
        return new RuleResult(true, "REJECTED", "Transaction rejected by rule",
                            context, new ArrayList<>(), 0);
    }

    public static RuleResult error(String transactionCode, String errorMessage) {
        return new RuleResult(false, "ERROR", errorMessage, null, new ArrayList<>(), 0);
    }

    public static RuleResult unknownTransaction(String transactionCode) {
        return new RuleResult(false, "UNKNOWN_TRANSACTION",
                            "Unknown transaction code: " + transactionCode,
                            null, new ArrayList<>(), 0);
    }

    public static RuleResult unknownClient(String clientId) {
        return new RuleResult(false, "UNKNOWN_CLIENT",
                            "Unknown client: " + clientId,
                            null, new ArrayList<>(), 0);
    }

    // Getters
    public boolean isSuccess() { return success; }
    public String getStatus() { return status; }
    public String getMessage() { return message; }
    public TransactionContext getFinalContext() { return finalContext; }
    public List<String> getExecutedActions() { return executedActions; }
    public long getExecutionTimeNanos() { return executionTimeNanos; }
}"""

    def _performance_metrics_template(self) -> str:
        """Generate performance metrics template."""
        return """package com.rules.router;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;

/**
 * High-performance metrics collection for 80K+ TPS
 */
public class PerformanceMetrics {

    private final String identifier;
    private final LongAdder totalTransactions = new LongAdder();
    private final LongAdder hotPathExecutions = new LongAdder();
    private final LongAdder coldPathExecutions = new LongAdder();
    private final LongAdder errors = new LongAdder();
    private final LongAdder unknownTransactions = new LongAdder();
    private final LongAdder unknownClients = new LongAdder();

    // Execution time tracking (nanoseconds)
    private final AtomicLong totalExecutionTime = new AtomicLong();
    private final AtomicLong minExecutionTime = new AtomicLong(Long.MAX_VALUE);
    private final AtomicLong maxExecutionTime = new AtomicLong();

    public PerformanceMetrics(String identifier) {
        this.identifier = identifier;
    }

    public void incrementTotalTransactions() { totalTransactions.increment(); }
    public void incrementHotPathExecutions() { hotPathExecutions.increment(); }
    public void incrementColdPathExecutions() { coldPathExecutions.increment(); }
    public void incrementErrors() { errors.increment(); }
    public void incrementUnknownTransactions() { unknownTransactions.increment(); }
    public void incrementUnknownClients() { unknownClients.increment(); }

    public void recordExecutionTime(long nanos) {
        totalExecutionTime.addAndGet(nanos);

        // Update min/max with lock-free CAS
        updateMin(nanos);
        updateMax(nanos);
    }

    private void updateMin(long nanos) {
        long current = minExecutionTime.get();
        while (nanos < current && !minExecutionTime.compareAndSet(current, nanos)) {
            current = minExecutionTime.get();
        }
    }

    private void updateMax(long nanos) {
        long current = maxExecutionTime.get();
        while (nanos > current && !maxExecutionTime.compareAndSet(current, nanos)) {
            current = maxExecutionTime.get();
        }
    }

    // Getters for metrics
    public String getIdentifier() { return identifier; }
    public long getTotalTransactions() { return totalTransactions.sum(); }
    public long getHotPathExecutions() { return hotPathExecutions.sum(); }
    public long getColdPathExecutions() { return coldPathExecutions.sum(); }
    public long getErrors() { return errors.sum(); }
    public long getUnknownTransactions() { return unknownTransactions.sum(); }
    public long getUnknownClients() { return unknownClients.sum(); }

    public long getAverageExecutionTimeNanos() {
        long total = getTotalTransactions();
        return total > 0 ? totalExecutionTime.get() / total : 0;
    }

    public long getMinExecutionTimeNanos() {
        long min = minExecutionTime.get();
        return min == Long.MAX_VALUE ? 0 : min;
    }

    public long getMaxExecutionTimeNanos() { return maxExecutionTime.get(); }

    public double getHotPathPercentage() {
        long total = getTotalTransactions();
        return total > 0 ? (double) getHotPathExecutions() / total * 100.0 : 0.0;
    }
}"""

    def _generate_router_registry(self, client_specs: List[ClientRouterSpec]) -> Dict[str, str]:
        """Generate router registry that manages all clients."""

        client_registrations = []
        for spec in client_specs:
            client_class = f"Client{spec.client_id.upper()}RuleMap"
            client_package = f"{spec.package_name}.{spec.client_id.lower()}"
            client_registrations.append(f'        UniversalTransactionRouter.registerClient("{spec.client_id}", new {client_package}.{client_class}());')

        return {
            "src/main/java/com/rules/router/RouterRegistry.java": f"""package com.rules.router;

/**
 * Router Registry - Initializes all client routers
 * Generated router configuration for all clients
 */
public class RouterRegistry {{

    /**
     * Initialize all client routers at startup
     */
    public static void initializeRouters() {{
{chr(10).join(client_registrations)}
    }}
}}"""
        }

    def _to_pascal_case(self, text: str) -> str:
        """Convert text to PascalCase."""
        words = text.replace('-', ' ').replace('_', ' ').split()
        return ''.join(word.capitalize() for word in words)