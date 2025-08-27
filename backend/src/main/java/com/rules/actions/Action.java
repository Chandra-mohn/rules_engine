package com.rules.actions;

import com.rules.context.RuleContext;

/**
 * Base interface for all rule actions.
 * Actions receive complete context and determine their own data needs.
 */
public interface Action {
    /**
     * Execute the action with the provided context.
     * 
     * @param context Complete rule context containing all available data
     */
    void execute(RuleContext context);
    
    /**
     * Get the action name for registry purposes.
     * 
     * @return Action name as used in rules
     */
    String getName();
}