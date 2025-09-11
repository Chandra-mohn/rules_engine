package com.rules;

import com.rules.actions.ActionRegistry;
import com.rules.actions.application.*;
import com.rules.context.RuleContext;
import com.rules.engine.RulesEngine;
import com.rules.engine.RuleResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test arithmetic expressions in rules.
 */
public class ArithmeticExpressionTest {
    
    private RulesEngine rulesEngine;
    private ActionRegistry actionRegistry;
    
    @BeforeEach
    public void setUp() throws Exception {
        // Set up action registry
        actionRegistry = new ActionRegistry();
        actionRegistry.registerAction(new ApproveApplicationAction());
        actionRegistry.registerAction(new RejectApplicationAction());
        actionRegistry.registerAction(new ManualReviewAction());
        
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
    public void testBasicArithmeticExpressions() throws Exception {
        // Rules with arithmetic expressions
        String rulesDSL = "# Test arithmetic expressions\n" +
                         "rule arithmeticTest:\n" +
                         "    if applicant.annualIncome > applicant.monthlyExpenses * 12 then approveApplication\n" +
                         "    if applicant.totalDebt > applicant.annualIncome * 0.5 then rejectApplication";
        
        // Load rules
        rulesEngine.loadRules(rulesDSL);
        
        // Test data - income vs expenses
        String jsonData = "{\n" +
                         "    \"applicant\": {\n" +
                         "        \"id\": \"APP_ARITH_001\",\n" +
                         "        \"annualIncome\": 60000,\n" +
                         "        \"monthlyExpenses\": 4000,\n" +
                         "        \"totalDebt\": 20000\n" +
                         "    }\n" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        
        // Execute rules
        List<RuleResult> results = rulesEngine.executeAllRules(context);
        
        // Verify results
        assertEquals(1, results.size());
        
        RuleResult result = results.get(0);
        assertEquals("arithmeticTest", result.getRuleName());
        assertTrue(result.wasExecuted());
        assertTrue(result.hasActions());
        
        // Should approve because 60000 > 4000 * 12 (48000)
        // Should not reject because 20000 < 60000 * 0.5 (30000)
        assertEquals("approveApplication", result.getActions().get(0));
        
        System.out.println("Arithmetic test result: " + result);
    }
    
    @Test
    public void testComplexArithmeticExpressions() throws Exception {
        // Rules with complex arithmetic
        String rulesDSL = "# Complex arithmetic test\n" +
                         "rule complexArithmetic:\n" +
                         "    if applicant.score + applicant.bonus > 750 then approveApplication\n" +
                         "    if applicant.debt / applicant.income > 0.4 then manualReview";
        
        // Load rules
        rulesEngine.loadRules(rulesDSL);
        
        // Test data
        String jsonData = "{\n" +
                         "    \"applicant\": {\n" +
                         "        \"id\": \"APP_COMPLEX_001\",\n" +
                         "        \"score\": 700,\n" +
                         "        \"bonus\": 100,\n" +
                         "        \"debt\": 25000,\n" +
                         "        \"income\": 50000\n" +
                         "    }\n" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        
        // Execute rules
        List<RuleResult> results = rulesEngine.executeAllRules(context);
        
        // Verify results
        assertEquals(1, results.size());
        
        RuleResult result = results.get(0);
        assertTrue(result.wasExecuted());
        assertTrue(result.hasActions());
        
        // Should approve because 700 + 100 = 800 > 750
        // Should also trigger manual review because 25000 / 50000 = 0.5 > 0.4
        assertTrue(result.getActions().contains("approveApplication") || 
                  result.getActions().contains("manualReview"));
        
        System.out.println("Complex arithmetic result: " + result);
    }
    
    @Test
    public void testArithmeticWithParentheses() throws Exception {
        // Rules with parentheses for precedence
        String rulesDSL = "# Parentheses test\n" +
                         "rule parenthesesTest:\n" +
                         "    if applicant.base * (applicant.multiplier + 1) > 1000 then approveApplication";
        
        // Load rules
        rulesEngine.loadRules(rulesDSL);
        
        // Test data
        String jsonData = "{\n" +
                         "    \"applicant\": {\n" +
                         "        \"id\": \"APP_PAREN_001\",\n" +
                         "        \"base\": 400,\n" +
                         "        \"multiplier\": 2\n" +
                         "    }\n" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        
        // Execute rules
        List<RuleResult> results = rulesEngine.executeAllRules(context);
        
        // Verify results
        assertEquals(1, results.size());
        
        RuleResult result = results.get(0);
        assertTrue(result.wasExecuted());
        assertTrue(result.hasActions());
        
        // Should approve because 400 * (2 + 1) = 400 * 3 = 1200 > 1000
        assertEquals("approveApplication", result.getActions().get(0));
        
        System.out.println("Parentheses test result: " + result);
    }
    
    @Test
    public void testPerformanceWithArithmetic() throws Exception {
        String rulesDSL = "rule performanceArithmetic:\n" +
                         "    if applicant.income * 0.3 > applicant.expenses + 500 then approveApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        String jsonData = "{\n" +
                         "    \"applicant\": {\n" +
                         "        \"income\": 80000,\n" +
                         "        \"expenses\": 20000\n" +
                         "    }\n" +
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
            assertTrue(results.get(0).wasExecuted());
            // 80000 * 0.3 = 24000 > 20000 + 500 = 20500, so should approve
            assertEquals("approveApplication", results.get(0).getActions().get(0));
        }
        
        double avgTimeMs = (totalTime / iterations) / 1_000_000.0;
        System.out.printf("Average arithmetic execution time: %.3f ms%n", avgTimeMs);
        
        // Performance assertion - should still be very fast
        assertTrue(avgTimeMs < 10.0, "Arithmetic rule execution should be under 10ms on average");
    }
}