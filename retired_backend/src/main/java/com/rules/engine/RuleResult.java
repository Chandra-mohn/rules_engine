package com.rules.engine;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Result of rule execution containing actions to be performed.
 */
public class RuleResult {
    private final String ruleName;
    private final List<String> actions;
    private final boolean executed;
    private final long executionTimeNanos;
    private final Exception error;
    
    private RuleResult(String ruleName, List<String> actions, boolean executed, 
                      long executionTimeNanos, Exception error) {
        this.ruleName = ruleName;
        this.actions = new ArrayList<>(actions);
        this.executed = executed;
        this.executionTimeNanos = executionTimeNanos;
        this.error = error;
    }
    
    /**
     * Create a result with a single action.
     * 
     * @param ruleName Name of the rule
     * @param action Action to execute
     * @return Rule result
     */
    public static RuleResult action(String ruleName, String action) {
        List<String> actions = new ArrayList<>();
        actions.add(action);
        return new RuleResult(ruleName, actions, true, 0, null);
    }
    
    /**
     * Create a result with multiple actions.
     * 
     * @param ruleName Name of the rule
     * @param actions Actions to execute
     * @return Rule result
     */
    public static RuleResult actions(String ruleName, List<String> actions) {
        return new RuleResult(ruleName, actions, true, 0, null);
    }
    
    /**
     * Create a result with no actions (rule didn't trigger).
     * 
     * @param ruleName Name of the rule
     * @return Rule result
     */
    public static RuleResult noAction(String ruleName) {
        return new RuleResult(ruleName, new ArrayList<>(), false, 0, null);
    }
    
    /**
     * Create a result with execution error.
     * 
     * @param ruleName Name of the rule
     * @param error Execution error
     * @return Rule result
     */
    public static RuleResult error(String ruleName, Exception error) {
        return new RuleResult(ruleName, new ArrayList<>(), false, 0, error);
    }
    
    /**
     * Create a result with execution timing.
     * 
     * @param ruleName Name of the rule
     * @param actions Actions to execute
     * @param executed Whether rule was executed
     * @param executionTimeNanos Execution time in nanoseconds
     * @return Rule result
     */
    public static RuleResult withTiming(String ruleName, List<String> actions, 
                                       boolean executed, long executionTimeNanos) {
        return new RuleResult(ruleName, actions, executed, executionTimeNanos, null);
    }
    
    /**
     * Get the rule name.
     * 
     * @return Rule name
     */
    public String getRuleName() {
        return ruleName;
    }
    
    /**
     * Get the actions to execute.
     * 
     * @return List of action names
     */
    public List<String> getActions() {
        return new ArrayList<>(actions);
    }
    
    /**
     * Check if the rule was executed (conditions were met).
     * 
     * @return true if rule was executed
     */
    public boolean wasExecuted() {
        return executed;
    }
    
    /**
     * Check if there are actions to execute.
     * 
     * @return true if there are actions
     */
    public boolean hasActions() {
        return !actions.isEmpty();
    }
    
    /**
     * Get the execution time in nanoseconds.
     * 
     * @return Execution time in nanoseconds
     */
    public long getExecutionTimeNanos() {
        return executionTimeNanos;
    }
    
    /**
     * Get the execution time in milliseconds.
     * 
     * @return Execution time in milliseconds
     */
    public double getExecutionTimeMillis() {
        return executionTimeNanos / 1_000_000.0;
    }
    
    /**
     * Check if there was an execution error.
     * 
     * @return true if there was an error
     */
    public boolean hasError() {
        return error != null;
    }
    
    /**
     * Get the execution error.
     * 
     * @return Execution error, or null if none
     */
    public Exception getError() {
        return error;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("RuleResult[");
        sb.append("rule='").append(ruleName).append("'");
        sb.append(", executed=").append(executed);
        if (hasActions()) {
            sb.append(", actions=").append(actions);
        }
        if (executionTimeNanos > 0) {
            sb.append(", time=").append(String.format("%.3fms", getExecutionTimeMillis()));
        }
        if (hasError()) {
            sb.append(", error=").append(error.getMessage());
        }
        sb.append("]");
        return sb.toString();
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        RuleResult that = (RuleResult) obj;
        return executed == that.executed &&
               executionTimeNanos == that.executionTimeNanos &&
               Objects.equals(ruleName, that.ruleName) &&
               Objects.equals(actions, that.actions) &&
               Objects.equals(error, that.error);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(ruleName, actions, executed, executionTimeNanos, error);
    }
}