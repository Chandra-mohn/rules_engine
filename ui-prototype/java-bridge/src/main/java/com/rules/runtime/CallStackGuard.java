package com.rules.runtime;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

/**
 * Runtime guard against cyclic rule calls.
 *
 * Provides thread-local call stack tracking to prevent infinite recursion
 * when rules call other rules or actionsets.
 *
 * Usage:
 *   CallStackGuard.enter("myRule");
 *   try {
 *       // Execute rule logic
 *   } finally {
 *       CallStackGuard.exit("myRule");
 *   }
 */
public class CallStackGuard {

    /**
     * Maximum call depth allowed before throwing exception.
     * This prevents stack overflow even if cycles aren't detected immediately.
     */
    public static final int MAX_DEPTH = 32;

    /**
     * Thread-local call stack for tracking rule execution chain.
     * Each thread maintains its own independent call stack.
     */
    private static final ThreadLocal<Stack<String>> callStack =
        ThreadLocal.withInitial(Stack::new);

    /**
     * Thread-local set for O(1) cycle detection.
     * Tracks all rules currently in the execution chain for fast lookup.
     */
    private static final ThreadLocal<Set<String>> callSet =
        ThreadLocal.withInitial(HashSet::new);

    /**
     * Enter a rule execution context.
     *
     * @param ruleName Name of the rule being entered
     * @throws CyclicCallException if the rule is already in the call stack (cycle detected)
     * @throws MaxDepthExceededException if MAX_DEPTH is exceeded
     */
    public static void enter(String ruleName) {
        Stack<String> stack = callStack.get();
        Set<String> set = callSet.get();

        // Check for cycle
        if (set.contains(ruleName)) {
            String cyclePath = buildCyclePath(stack, ruleName);
            throw new CyclicCallException(
                "Cyclic rule call detected: " + cyclePath + " → " + ruleName
            );
        }

        // Check max depth
        if (stack.size() >= MAX_DEPTH) {
            throw new MaxDepthExceededException(
                "Maximum call depth of " + MAX_DEPTH + " exceeded. " +
                "Current stack: " + stack
            );
        }

        // Add to stack and set
        stack.push(ruleName);
        set.add(ruleName);
    }

    /**
     * Exit a rule execution context.
     *
     * @param ruleName Name of the rule being exited
     * @throws IllegalStateException if the rule name doesn't match the top of stack
     */
    public static void exit(String ruleName) {
        Stack<String> stack = callStack.get();
        Set<String> set = callSet.get();

        if (stack.isEmpty()) {
            throw new IllegalStateException(
                "Cannot exit rule '" + ruleName + "': call stack is empty"
            );
        }

        String top = stack.pop();
        if (!top.equals(ruleName)) {
            // Restore stack state
            stack.push(top);
            throw new IllegalStateException(
                "Cannot exit rule '" + ruleName + "': expected '" + top + "' at top of stack"
            );
        }

        set.remove(ruleName);
    }

    /**
     * Get current call depth.
     *
     * @return Number of rules in the current call stack
     */
    public static int getDepth() {
        return callStack.get().size();
    }

    /**
     * Get current call stack as array.
     *
     * @return Array of rule names in call order (oldest to newest)
     */
    public static String[] getCallStack() {
        Stack<String> stack = callStack.get();
        return stack.toArray(new String[0]);
    }

    /**
     * Clear the call stack for the current thread.
     * Use with caution - typically only needed for cleanup after errors.
     */
    public static void clear() {
        callStack.get().clear();
        callSet.get().clear();
    }

    /**
     * Build human-readable cycle path string.
     */
    private static String buildCyclePath(Stack<String> stack, String cycleTo) {
        StringBuilder path = new StringBuilder();
        boolean foundStart = false;

        for (String rule : stack) {
            if (rule.equals(cycleTo)) {
                foundStart = true;
            }
            if (foundStart) {
                if (path.length() > 0) {
                    path.append(" → ");
                }
                path.append(rule);
            }
        }

        return path.toString();
    }

    /**
     * Exception thrown when a cyclic rule call is detected.
     */
    public static class CyclicCallException extends RuntimeException {
        public CyclicCallException(String message) {
            super(message);
        }
    }

    /**
     * Exception thrown when maximum call depth is exceeded.
     */
    public static class MaxDepthExceededException extends RuntimeException {
        public MaxDepthExceededException(String message) {
            super(message);
        }
    }
}
