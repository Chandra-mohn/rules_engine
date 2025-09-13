package com.rules.actions;

import com.rules.context.RuleContext;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Registry for managing and executing rule actions.
 * Provides centralized action management and execution.
 */
public class ActionRegistry {
    private final Map<String, Action> actions = new HashMap<>();
    
    /**
     * Register an action with the registry.
     * 
     * @param action The action to register
     */
    public void registerAction(Action action) {
        actions.put(action.getName(), action);
    }
    
    /**
     * Register an action with a custom name.
     * 
     * @param name Custom name for the action
     * @param action The action to register
     */
    public void registerAction(String name, Action action) {
        actions.put(name, action);
    }
    
    /**
     * Execute an action by name.
     * 
     * @param actionName Name of the action to execute
     * @param context Rule context containing all data
     * @throws ActionNotFoundException if action is not found
     */
    public void executeAction(String actionName, RuleContext context) {
        Action action = actions.get(actionName);
        if (action == null) {
            throw new ActionNotFoundException("Action not found: " + actionName);
        }
        
        try {
            action.execute(context);
        } catch (Exception e) {
            throw new ActionExecutionException("Failed to execute action: " + actionName, e);
        }
    }
    
    /**
     * Check if an action is registered.
     * 
     * @param actionName Name of the action to check
     * @return true if action is registered, false otherwise
     */
    public boolean hasAction(String actionName) {
        return actions.containsKey(actionName);
    }
    
    /**
     * Get all registered action names.
     * 
     * @return Set of registered action names
     */
    public Set<String> getActionNames() {
        return actions.keySet();
    }
    
    /**
     * Get the number of registered actions.
     * 
     * @return Number of registered actions
     */
    public int getActionCount() {
        return actions.size();
    }
    
    /**
     * Clear all registered actions.
     */
    public void clear() {
        actions.clear();
    }
    
    /**
     * Exception thrown when an action is not found in the registry.
     */
    public static class ActionNotFoundException extends RuntimeException {
        public ActionNotFoundException(String message) {
            super(message);
        }
    }
    
    /**
     * Exception thrown when an action execution fails.
     */
    public static class ActionExecutionException extends RuntimeException {
        public ActionExecutionException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}