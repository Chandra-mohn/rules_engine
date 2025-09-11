package com.rules.engine;

import com.rules.actions.ActionRegistry;
import com.rules.ast.RuleSetNode;
import com.rules.codegen.RuleCompiler;
import com.rules.context.RuleContext;
import com.rules.parser.RulesParser;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * Main rules engine that orchestrates rule compilation and execution.
 * Provides high-level interface for rule processing.
 */
public class RulesEngine {
    
    private final RuleCompiler compiler;
    private final ActionRegistry actionRegistry;
    private final Map<String, Rule> loadedRules = new ConcurrentHashMap<>();
    private final ExecutorService executorService;
    private final boolean parallelExecution;
    
    public RulesEngine(ActionRegistry actionRegistry) throws Exception {
        this(actionRegistry, false);
    }
    
    public RulesEngine(ActionRegistry actionRegistry, boolean parallelExecution) throws Exception {
        this.compiler = new RuleCompiler();
        this.actionRegistry = actionRegistry;
        this.parallelExecution = parallelExecution;
        this.executorService = parallelExecution ? 
            Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors()) : null;
    }
    
    /**
     * Load and compile rules from DSL string.
     * 
     * @param rulesDSL Rules DSL content
     * @throws Exception if parsing or compilation fails
     */
    public void loadRules(String rulesDSL) throws Exception {
        // Parse rules
        RuleSetNode ruleSet = RulesParser.parse(rulesDSL);
        
        // Compile rules
        Map<String, Class<? extends Rule>> compiledRules = compiler.compileRules(ruleSet);
        
        // Instantiate and load rules
        for (Map.Entry<String, Class<? extends Rule>> entry : compiledRules.entrySet()) {
            Rule rule = entry.getValue().getDeclaredConstructor().newInstance();
            loadedRules.put(rule.getRuleName(), rule);
        }
        
        System.out.println("Loaded " + loadedRules.size() + " rules successfully");
    }
    
    /**
     * Load and compile rules from file.
     * 
     * @param filename Path to rules file
     * @throws Exception if parsing or compilation fails
     */
    public void loadRulesFromFile(String filename) throws Exception {
        // Parse rules from file
        RuleSetNode ruleSet = RulesParser.parseFile(filename);
        
        // Compile rules
        Map<String, Class<? extends Rule>> compiledRules = compiler.compileRules(ruleSet);
        
        // Instantiate and load rules
        for (Map.Entry<String, Class<? extends Rule>> entry : compiledRules.entrySet()) {
            Rule rule = entry.getValue().getDeclaredConstructor().newInstance();
            loadedRules.put(rule.getRuleName(), rule);
        }
        
        System.out.println("Loaded " + loadedRules.size() + " rules from file: " + filename);
    }
    
    /**
     * Execute all loaded rules with the provided context.
     * 
     * @param context Rule execution context
     * @return List of rule results
     */
    public List<RuleResult> executeAllRules(RuleContext context) {
        if (parallelExecution && executorService != null) {
            return executeRulesParallel(context, loadedRules.values());
        } else {
            return executeRulesSequential(context, loadedRules.values());
        }
    }
    
    /**
     * Execute a specific rule by name.
     * 
     * @param ruleName Name of the rule to execute
     * @param context Rule execution context
     * @return Rule result, or null if rule not found
     */
    public RuleResult executeRule(String ruleName, RuleContext context) {
        Rule rule = loadedRules.get(ruleName);
        if (rule == null) {
            return RuleResult.error(ruleName, new IllegalArgumentException("Rule not found: " + ruleName));
        }
        
        return executeRuleSafely(rule, context);
    }
    
    /**
     * Execute rules and perform actions.
     * 
     * @param context Rule execution context
     * @return Execution summary
     */
    public ExecutionSummary executeRulesWithActions(RuleContext context) {
        long startTime = System.nanoTime();
        
        // Execute all rules
        List<RuleResult> results = executeAllRules(context);
        
        // Execute actions
        int actionsExecuted = 0;
        List<String> errors = new ArrayList<>();
        
        for (RuleResult result : results) {
            if (result.hasActions()) {
                for (String actionName : result.getActions()) {
                    try {
                        actionRegistry.executeAction(actionName, context);
                        actionsExecuted++;
                    } catch (Exception e) {
                        errors.add("Failed to execute action '" + actionName + "': " + e.getMessage());
                    }
                }
            }
        }
        
        long totalTime = System.nanoTime() - startTime;
        
        return new ExecutionSummary(results, actionsExecuted, errors, totalTime);
    }
    
    /**
     * Get all loaded rule names.
     * 
     * @return Set of rule names
     */
    public Set<String> getLoadedRuleNames() {
        return new HashSet<>(loadedRules.keySet());
    }
    
    /**
     * Get the number of loaded rules.
     * 
     * @return Number of loaded rules
     */
    public int getLoadedRuleCount() {
        return loadedRules.size();
    }
    
    /**
     * Check if a rule is loaded.
     * 
     * @param ruleName Name of the rule
     * @return true if rule is loaded
     */
    public boolean isRuleLoaded(String ruleName) {
        return loadedRules.containsKey(ruleName);
    }
    
    /**
     * Clear all loaded rules.
     */
    public void clearRules() {
        loadedRules.clear();
        compiler.clearCache();
    }
    
    private List<RuleResult> executeRulesSequential(RuleContext context, Collection<Rule> rules) {
        List<RuleResult> results = new ArrayList<>();
        
        for (Rule rule : rules) {
            if (rule.isEnabled()) {
                RuleResult result = executeRuleSafely(rule, context);
                results.add(result);
            }
        }
        
        return results;
    }
    
    private List<RuleResult> executeRulesParallel(RuleContext context, Collection<Rule> rules) {
        List<Future<RuleResult>> futures = new ArrayList<>();
        
        // Submit all rules for execution
        for (Rule rule : rules) {
            if (rule.isEnabled()) {
                futures.add(executorService.submit(() -> executeRuleSafely(rule, context)));
            }
        }
        
        // Collect results
        List<RuleResult> results = new ArrayList<>();
        for (Future<RuleResult> future : futures) {
            try {
                results.add(future.get());
            } catch (Exception e) {
                results.add(RuleResult.error("unknown", e));
            }
        }
        
        return results;
    }
    
    private RuleResult executeRuleSafely(Rule rule, RuleContext context) {
        try {
            return rule.execute(context);
        } catch (Exception e) {
            return RuleResult.error(rule.getRuleName(), e);
        }
    }
    
    /**
     * Shutdown the rules engine and cleanup resources.
     */
    public void shutdown() {
        if (executorService != null) {
            executorService.shutdown();
        }
        compiler.cleanup();
    }
    
    /**
     * Summary of rule execution including performance metrics.
     */
    public static class ExecutionSummary {
        private final List<RuleResult> ruleResults;
        private final int actionsExecuted;
        private final List<String> errors;
        private final long totalExecutionTimeNanos;
        
        public ExecutionSummary(List<RuleResult> ruleResults, int actionsExecuted, 
                               List<String> errors, long totalExecutionTimeNanos) {
            this.ruleResults = new ArrayList<>(ruleResults);
            this.actionsExecuted = actionsExecuted;
            this.errors = new ArrayList<>(errors);
            this.totalExecutionTimeNanos = totalExecutionTimeNanos;
        }
        
        public List<RuleResult> getRuleResults() {
            return new ArrayList<>(ruleResults);
        }
        
        public int getRulesExecuted() {
            return (int) ruleResults.stream().filter(RuleResult::wasExecuted).count();
        }
        
        public int getTotalRules() {
            return ruleResults.size();
        }
        
        public int getActionsExecuted() {
            return actionsExecuted;
        }
        
        public List<String> getErrors() {
            return new ArrayList<>(errors);
        }
        
        public boolean hasErrors() {
            return !errors.isEmpty();
        }
        
        public long getTotalExecutionTimeNanos() {
            return totalExecutionTimeNanos;
        }
        
        public double getTotalExecutionTimeMillis() {
            return totalExecutionTimeNanos / 1_000_000.0;
        }
        
        @Override
        public String toString() {
            return String.format(
                "ExecutionSummary[rules=%d/%d executed, actions=%d, errors=%d, time=%.3fms]",
                getRulesExecuted(), getTotalRules(), actionsExecuted, errors.size(),
                getTotalExecutionTimeMillis()
            );
        }
    }
}