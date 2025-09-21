package com.rules.client.chase_retail.chase_retail;

import com.rules.engine.core.UniversalTransactionRouter;

/**
 * Router registry for chase_retail
 * Initializes client-specific router with the universal system
 */
public final class RouterRegistry {

    /**
     * Initialize chase_retail router
     * Called during application startup
     */
    public static void initialize() {
        ClientCHASE_RETAILRuleMap clientRouter = new ClientCHASE_RETAILRuleMap();

        // Register with universal router
        UniversalTransactionRouter.registerClient("chase_retail", clientRouter);

        // Perform JIT warmup
        clientRouter.warmUp();
    }
}