package com.rules.client.wells_fargo_business.wells_fargo_business;

import com.rules.engine.core.UniversalTransactionRouter;

/**
 * Router registry for wells_fargo_business
 * Initializes client-specific router with the universal system
 */
public final class RouterRegistry {

    /**
     * Initialize wells_fargo_business router
     * Called during application startup
     */
    public static void initialize() {
        ClientWELLS_FARGO_BUSINESSRuleMap clientRouter = new ClientWELLS_FARGO_BUSINESSRuleMap();

        // Register with universal router
        UniversalTransactionRouter.registerClient("wells_fargo_business", clientRouter);

        // Perform JIT warmup
        clientRouter.warmUp();
    }
}