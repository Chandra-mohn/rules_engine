package com.rules.client.chase_retail.chase_retail;

import com.rules.engine.core.*;
import com.rules.client.chase_retail.chase_retail.executors.*;
import java.util.Map;
import java.util.Set;

/**
 * Client-specific rule router for chase_retail
 * Optimized for 80K+ TPS with hot/cold path separation
 *
 * Hot path rules: 2 (≤5 steps)
 * Cold path rules: 1 (>5 steps)
 * Generated: 2025-09-20T22:05:52.366486
 */
public final class ClientCHASE_RETAILRuleMap implements ClientRuleMap {

    // Hot path executors (80% of traffic, inlined execution)
    private static final Map<String, RuleExecutor> HOT_EXECUTORS = Map.of(""withdrawal"", new Chase001Executor(),\n            ""withdrawal"", new Chase002Executor());

    // Cold path executors (20% of traffic, method-based execution)
    private static final Map<String, RuleExecutor> COLD_EXECUTORS = Map.of(""withdrawal"", new Chase003Executor());

    // All supported transaction codes for validation
    private static final Set<String> SUPPORTED_CODES = Set.of("withdrawal", "withdrawal", "withdrawal");

    @Override
    public RuleResult execute(String transactionCode, TransactionContext context) {
        // Branch prediction optimized routing\n        if ("withdrawal".equals(transactionCode)) {\n            return HOT_EXECUTORS.get(transactionCode).execute(context);\n        }\n        if ("withdrawal".equals(transactionCode)) {\n            return HOT_EXECUTORS.get(transactionCode).execute(context);\n        }\n\n        // Check hot path executors first (80% of traffic)\n        RuleExecutor hotExecutor = HOT_EXECUTORS.get(transactionCode);\n        if (hotExecutor != null) {\n            return hotExecutor.execute(context);\n        }\n\n        // Fall back to cold path executors (20% of traffic)\n        RuleExecutor coldExecutor = COLD_EXECUTORS.get(transactionCode);\n        if (coldExecutor != null) {\n            return coldExecutor.execute(context);\n        }\n\n        // Unknown transaction code\n        return RuleResult.unknownTransaction(transactionCode);
    }

    @Override
    public String getClientId() {
        return "chase_retail";
    }

    @Override
    public Set<String> getSupportedTransactionCodes() {
        return SUPPORTED_CODES;
    }

    @Override
    public void warmUp() {
        // Warm up hot path executors for JIT optimization
        TransactionContext warmupContext = ContextPool.acquireContext("warmup");

        // Execute each hot path rule once to trigger JIT compilation
        HOT_EXECUTORS.get("withdrawal").execute(warmupContext);\n        HOT_EXECUTORS.get("withdrawal").execute(warmupContext);

        ContextPool.releaseContext(warmupContext);
    }
}