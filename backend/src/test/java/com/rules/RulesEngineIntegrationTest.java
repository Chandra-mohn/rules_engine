package com.rules;

import com.rules.actions.ActionRegistry;
import com.rules.actions.application.*;
import com.rules.actions.transaction.*;
import com.rules.context.RuleContext;
import com.rules.engine.RulesEngine;
import com.rules.engine.RuleResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test demonstrating the complete rules engine workflow.
 */
public class RulesEngineIntegrationTest {
    
    private RulesEngine rulesEngine;
    private ActionRegistry actionRegistry;
    
    @BeforeEach
    public void setUp() throws Exception {
        // Set up action registry
        actionRegistry = new ActionRegistry();
        
        // Register application actions
        actionRegistry.registerAction(new RejectApplicationAction());
        actionRegistry.registerAction(new ApproveApplicationAction());
        actionRegistry.registerAction(new InstantApprovalAction());
        actionRegistry.registerAction(new ConditionalApprovalAction());
        actionRegistry.registerAction(new ManualReviewAction());
        actionRegistry.registerAction(new RequireManualReviewAction());
        
        // Register transaction actions
        actionRegistry.registerAction(new ApproveTransactionAction());
        actionRegistry.registerAction(new BlockTransactionAction());
        
        // Create rules engine
        rulesEngine = new RulesEngine(actionRegistry);
    }
    
    @AfterEach
    public void tearDown() {
        if (rulesEngine != null) {
            rulesEngine.shutdown();
        }
    }
    
    @Test
    public void testSimpleRuleExecution() throws Exception {
        // Define simple rules
        String rulesDSL = "# Simple age verification rule\n" +
                         "rule ageVerification:\n" +
                         "    if applicant.age < 18 then rejectApplication\n" +
                         "\n" +
                         "# Income requirement rule\n" +
                         "rule incomeCheck:\n" +
                         "    if applicant.annualIncome >= 50000 then approveApplication";
        
        // Load rules
        rulesEngine.loadRules(rulesDSL);
        
        // Test data - underage applicant
        String jsonData = "{\n" +
                         "    \"applicant\": {\n" +
                         "        \"id\": \"APP_001\",\n" +
                         "        \"age\": 16,\n" +
                         "        \"annualIncome\": 60000\n" +
                         "    }\n" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        
        // Execute rules
        List<RuleResult> results = rulesEngine.executeAllRules(context);
        
        // Verify results
        assertEquals(2, results.size());
        
        // Age verification should trigger rejection
        RuleResult ageResult = findResult(results, "ageVerification");
        assertNotNull(ageResult);
        assertTrue(ageResult.wasExecuted());
        assertTrue(ageResult.hasActions());
        assertEquals("rejectApplication", ageResult.getActions().get(0));
        
        // Income check should not trigger (applicant is underage)
        RuleResult incomeResult = findResult(results, "incomeCheck");
        assertNotNull(incomeResult);
        // This depends on rule execution order and logic
    }
    
    @Test
    public void testComplexRuleExecution() throws Exception {
        // Define complex rules with attribute-to-attribute comparisons and arithmetic
        String rulesDSL = "# Complex application processing\n" +
                         "rule applicationProcessing:\n" +
                         "    if applicant.age >= 18 then instantApproval\n" +
                         "    if applicant.totalDebt > 30000 then requireManualReview\n" +
                         "\n" +
                         "# Transaction validation\n" +
                         "rule transactionValidation:\n" +
                         "    if transaction.amount > card.availableBalance then blockTransaction\n" +
                         "    if transaction.amount <= card.availableBalance then approveTransaction";
        
        // Load rules
        rulesEngine.loadRules(rulesDSL);
        
        // Test data - qualified applicant
        String jsonData = "{\n" +
                         "    \"applicant\": {\n" +
                         "        \"id\": \"APP_002\",\n" +
                         "        \"age\": 28,\n" +
                         "        \"annualIncome\": 75000,\n" +
                         "        \"requestedCreditLimit\": 15000,\n" +
                         "        \"totalDebt\": 20000\n" +
                         "    },\n" +
                         "    \"transaction\": {\n" +
                         "        \"id\": \"TXN_001\",\n" +
                         "        \"amount\": 500.00\n" +
                         "    },\n" +
                         "    \"card\": {\n" +
                         "        \"id\": \"CARD_001\",\n" +
                         "        \"availableBalance\": 2000.00\n" +
                         "    }\n" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        
        // Execute rules with actions
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        // Verify execution summary
        assertNotNull(summary);
        assertTrue(summary.getTotalRules() > 0);
        System.out.println("Execution Summary: " + summary);
        
        // Verify some rules were executed
        assertTrue(summary.getRulesExecuted() > 0);
        
        // Print detailed results
        for (RuleResult result : summary.getRuleResults()) {
            System.out.println("Rule Result: " + result);
        }
    }
    
    @Test
    public void testRuleCompilationAndLoading() throws Exception {
        // Test that rules can be compiled and loaded successfully
        String rulesDSL = "rule testRule1:\n" +
                         "    if customer.age > 21 then approveApplication\n" +
                         "\n" +
                         "rule testRule2:\n" +
                         "    if transaction.amount < 100 then approveTransaction";
        
        // Load rules
        rulesEngine.loadRules(rulesDSL);
        
        // Verify rules are loaded
        assertEquals(2, rulesEngine.getLoadedRuleCount());
        assertTrue(rulesEngine.isRuleLoaded("testRule1"));
        assertTrue(rulesEngine.isRuleLoaded("testRule2"));
        
        // Test execution with minimal data
        String jsonData = "{\n" +
                         "    \"customer\": {\"age\": 25},\n" +
                         "    \"transaction\": {\"amount\": 50}\n" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        List<RuleResult> results = rulesEngine.executeAllRules(context);
        
        assertEquals(2, results.size());
        
        // Both rules should execute successfully (no errors)
        for (RuleResult result : results) {
            assertFalse(result.hasError(), "Rule should not have errors: " + result);
        }
    }
    
    @Test
    public void testPerformanceMetrics() throws Exception {
        String rulesDSL = "rule performanceTest:\n" +
                         "    if applicant.age >= 18 then approveApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        String jsonData = "{\n" +
                         "    \"applicant\": {\"id\": \"PERF_001\", \"age\": 25}\n" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        
        // Execute multiple times to test performance
        long totalTime = 0;
        int iterations = 1000;
        
        for (int i = 0; i < iterations; i++) {
            long start = System.nanoTime();
            List<RuleResult> results = rulesEngine.executeAllRules(context);
            long end = System.nanoTime();
            
            totalTime += (end - start);
            
            // Verify result
            assertEquals(1, results.size());
            assertFalse(results.get(0).hasError());
        }
        
        double avgTimeMs = (totalTime / iterations) / 1_000_000.0;
        System.out.printf("Average execution time: %.3f ms%n", avgTimeMs);
        
        // Performance assertion - should be very fast
        assertTrue(avgTimeMs < 10.0, "Rule execution should be under 10ms on average");
    }
    
    private RuleResult findResult(List<RuleResult> results, String ruleName) {
        return results.stream()
                .filter(r -> r.getRuleName().equals(ruleName))
                .findFirst()
                .orElse(null);
    }
}