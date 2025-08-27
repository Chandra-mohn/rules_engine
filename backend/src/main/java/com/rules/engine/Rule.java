package com.rules.engine;

import com.rules.context.RuleContext;

/**
 * Interface for compiled rule implementations.
 * Each generated rule class implements this interface.
 */
public interface Rule {
    
    /**
     * Execute this rule with the provided context.
     * 
     * @param context Rule execution context containing all data
     * @return Rule execution result
     */
    RuleResult execute(RuleContext context);
    
    /**
     * Get the name of this rule.
     * 
     * @return Rule name
     */
    String getRuleName();
    
    /**
     * Get the version of this rule (for hot reloading).
     * 
     * @return Rule version
     */
    default String getVersion() {
        return "1.0";
    }
    
    /**
     * Check if this rule is enabled.
     * 
     * @return true if rule is enabled
     */
    default boolean isEnabled() {
        return true;
    }
}