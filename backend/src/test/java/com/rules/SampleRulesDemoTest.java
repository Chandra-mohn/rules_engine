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
 * Demonstration of sample rules behavior from SAMPLE_RULES.md
 */
public class SampleRulesDemoTest {
    
    private RulesEngine rulesEngine;
    private ActionRegistry actionRegistry;
    
    @BeforeEach
    public void setUp() throws Exception {
        actionRegistry = new ActionRegistry();
        
        // Register real actions
        actionRegistry.registerAction(new RejectApplicationAction());
        actionRegistry.registerAction(new ApproveApplicationAction());
        actionRegistry.registerAction(new ManualReviewAction());
        actionRegistry.registerAction(new RequireManualReviewAction());
        actionRegistry.registerAction(new BlockTransactionAction());
        
        // Register mock actions
        actionRegistry.registerAction("flagForReview", new MockAction("flagForReview"));
        actionRegistry.registerAction("sendAlert", new MockAction("sendAlert"));
        
        rulesEngine = new RulesEngine(actionRegistry);
    }
    
    @AfterEach
    public void tearDown() {
        if (rulesEngine != null) {
            rulesEngine.shutdown();
        }
    }
    
    @Test
    public void testSampleRulesFromFile() throws Exception {
        System.out.println("\\n=== TESTING SAMPLE RULES FROM FILE ===");
        
        // Load rules from file
        String rulesFile = "/Users/chandramohn/workspace/rules/sample_rules_demo.rules";
        rulesEngine.loadRulesFromFile(rulesFile);
        
        System.out.println("Loaded " + rulesEngine.getLoadedRuleCount() + " rules from file");
        System.out.println("Rules: " + rulesEngine.getLoadedRuleNames());
        
        // Test Case 1: Underage applicant with high debt
        System.out.println("\\n--- Test Case 1: Underage Applicant with High Debt ---");
        String jsonData1 = "{" +
                          "\"applicant\": {" +
                          "\"id\": \"APP_001\"," +
                          "\"age\": 16," +
                          "\"annualIncome\": 50000," +
                          "\"monthlyExpenses\": 3000," +
                          "\"totalDebt\": 25000," +
                          "\"creditScore\": 650," +
                          "\"requestedCreditLimit\": 10000" +
                          "}," +
                          "\"transaction\": {" +
                          "\"id\": \"TXN_001\"," +
                          "\"amount\": 800," +
                          "\"location\": \"New York\"" +
                          "}," +
                          "\"card\": {" +
                          "\"availableBalance\": 1000" +
                          "}," +
                          "\"customer\": {" +
                          "\"id\": \"CUST_001\"," +
                          "\"averageTransactionAmount\": 200," +
                          "\"homeLocation\": \"New York\"," +
                          "\"travelThreshold\": 500" +
                          "}" +
                          "}";
        
        RuleContext context1 = new RuleContext(jsonData1);
        RulesEngine.ExecutionSummary summary1 = rulesEngine.executeRulesWithActions(context1);
        
        System.out.println("Execution Summary: " + summary1);
        for (RuleResult result : summary1.getRuleResults()) {
            System.out.println("  " + result);
        }
        
        // Test Case 2: Good applicant with suspicious transaction
        System.out.println("\\n--- Test Case 2: Good Applicant with Suspicious Transaction ---");
        String jsonData2 = "{" +
                          "\"applicant\": {" +
                          "\"id\": \"APP_002\"," +
                          "\"age\": 28," +
                          "\"annualIncome\": 80000," +
                          "\"monthlyExpenses\": 4000," +
                          "\"totalDebt\": 20000," +
                          "\"creditScore\": 750," +
                          "\"requestedCreditLimit\": 15000" +
                          "}," +
                          "\"transaction\": {" +
                          "\"id\": \"TXN_002\"," +
                          "\"amount\": 2500," +
                          "\"location\": \"Las Vegas\"" +
                          "}," +
                          "\"card\": {" +
                          "\"availableBalance\": 3000" +
                          "}," +
                          "\"customer\": {" +
                          "\"id\": \"CUST_002\"," +
                          "\"averageTransactionAmount\": 300," +
                          "\"homeLocation\": \"Chicago\"," +
                          "\"travelThreshold\": 1000" +
                          "}" +
                          "}";
        
        RuleContext context2 = new RuleContext(jsonData2);
        RulesEngine.ExecutionSummary summary2 = rulesEngine.executeRulesWithActions(context2);
        
        System.out.println("Execution Summary: " + summary2);
        for (RuleResult result : summary2.getRuleResults()) {
            System.out.println("  " + result);
        }
        
        // Verify that rules executed as expected
        assertTrue(summary1.getRulesExecuted() > 0);
        assertTrue(summary2.getRulesExecuted() > 0);
        assertTrue(summary1.getActionsExecuted() > 0);
        assertTrue(summary2.getActionsExecuted() > 0);
    }
    
    @Test
    public void testArithmeticExpressionsInSampleRules() throws Exception {
        System.out.println("\\n=== TESTING ARITHMETIC EXPRESSIONS IN SAMPLE RULES ===");
        
        // Test the arithmetic expressions from sample rules
        String rulesDSL = "rule arithmeticDemo:" +
                         "    if applicant.annualIncome < applicant.monthlyExpenses * 12 then rejectApplication" +
                         "    if applicant.totalDebt > applicant.annualIncome * 0.4 then requireManualReview" +
                         "    if transaction.amount > customer.averageTransactionAmount * 5 then flagForReview";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test data that should trigger all arithmetic conditions
        String jsonData = "{" +
                         "\"applicant\": {" +
                         "\"id\": \"APP_ARITH\"," +
                         "\"annualIncome\": 40000," +
                         "\"monthlyExpenses\": 4000," +
                         "\"totalDebt\": 20000" +
                         "}," +
                         "\"transaction\": {" +
                         "\"id\": \"TXN_ARITH\"," +
                         "\"amount\": 2000" +
                         "}," +
                         "\"customer\": {" +
                         "\"id\": \"CUST_ARITH\"," +
                         "\"averageTransactionAmount\": 300" +
                         "}" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Arithmetic Demo Summary: " + summary);
        for (RuleResult result : summary.getRuleResults()) {
            System.out.println("  " + result);
        }
        
        // Verify arithmetic expressions worked:
        // - 40000 < 4000 * 12 (48000) = true -> rejectApplication
        // - 20000 > 40000 * 0.4 (16000) = true -> requireManualReview  
        // - 2000 > 300 * 5 (1500) = true -> flagForReview
        assertEquals(1, summary.getRulesExecuted());
        assertEquals(3, summary.getActionsExecuted()); // All three conditions should trigger
    }
    
    /**
     * Mock action for testing
     */
    private static class MockAction implements com.rules.actions.Action {
        private final String actionName;
        
        public MockAction(String actionName) {
            this.actionName = actionName;
        }
        
        @Override
        public void execute(RuleContext context) {
            System.out.println("MOCK " + actionName.toUpperCase() + ": Executed for " + 
                             context.getString("transaction.id"));
        }
        
        @Override
        public String getName() {
            return actionName;
        }
    }
}