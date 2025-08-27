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
 * Test sample rules from SAMPLE_RULES.md to verify their behavior.
 */
public class SampleRulesExecutionTest {
    
    private RulesEngine rulesEngine;
    private ActionRegistry actionRegistry;
    
    @BeforeEach
    public void setUp() throws Exception {
        actionRegistry = new ActionRegistry();
        
        // Register existing actions
        actionRegistry.registerAction(new RejectApplicationAction());
        actionRegistry.registerAction(new ApproveApplicationAction());
        actionRegistry.registerAction(new ManualReviewAction());
        actionRegistry.registerAction(new RequireManualReviewAction());
        actionRegistry.registerAction(new ApproveTransactionAction());
        actionRegistry.registerAction(new BlockTransactionAction());
        
        // Register mock actions for missing ones
        actionRegistry.registerAction("decline", new MockAction("decline"));
        actionRegistry.registerAction("flagForReview", new MockAction("flagForReview"));
        actionRegistry.registerAction("sendAlert", new MockAction("sendAlert"));
        actionRegistry.registerAction("requireVerification", new MockAction("requireVerification"));
        actionRegistry.registerAction("requireStepUpAuth", new MockAction("requireStepUpAuth"));
        
        rulesEngine = new RulesEngine(actionRegistry);
    }
    
    @AfterEach
    public void tearDown() {
        if (rulesEngine != null) {
            rulesEngine.shutdown();
        }
    }
    
    @Test
    public void testRule1_AgeVerification() throws Exception {
        System.out.println("\n=== Testing Rule 1: Age Verification ===");
        
        String rulesDSL = "rule ageVerification:\n" +
                         "    if applicant.age < 18 then rejectApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test underage applicant
        String jsonData = "{" +
                         "\"applicant\": {" +
                         "\"id\": \"APP_001\"," +
                         "\"age\": 16" +
                         "}" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertEquals(1, summary.getActionsExecuted());
        assertFalse(summary.hasErrors());
    }
    
    @Test
    public void testRule2_IncomeExpenseCheck() throws Exception {
        System.out.println("\n=== Testing Rule 2: Income vs Expenses ===");
        
        String rulesDSL = "rule incomeExpenseCheck:\n" +
                         "    if applicant.annualIncome < applicant.monthlyExpenses * 12 then rejectApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test insufficient income (30000 < 3000 * 12 = 36000)
        String jsonData = "{" +
                         "\"applicant\": {" +
                         "\"id\": \"APP_002\"," +
                         "\"annualIncome\": 30000," +
                         "\"monthlyExpenses\": 3000" +
                         "}" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertEquals(1, summary.getActionsExecuted());
    }
    
    @Test
    public void testRule3_CreditLimitCheck() throws Exception {
        System.out.println("\n=== Testing Rule 3: Credit Limit Check ===");
        
        String rulesDSL = "rule creditLimitCheck:\n" +
                         "    if transaction.amount > card.availableBalance then decline";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test transaction exceeding limit
        String jsonData = "{" +
                         "\"transaction\": {" +
                         "\"id\": \"TXN_001\"," +
                         "\"amount\": 1500" +
                         "}," +
                         "\"card\": {" +
                         "\"availableBalance\": 1000" +
                         "}" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertEquals(1, summary.getActionsExecuted());
    }
    
    @Test
    public void testRule4_DebtToIncomeCheck() throws Exception {
        System.out.println("\n=== Testing Rule 4: Debt to Income Check ===");
        
        String rulesDSL = "rule debtToIncomeCheck:\n" +
                         "    if applicant.totalDebt > applicant.annualIncome * 0.4 then requireManualReview\n" +
                         "    if applicant.creditScore >= 700 and applicant.annualIncome > applicant.requestedCreditLimit * 3 then approveApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test high debt ratio AND good credit with sufficient income
        String jsonData = "{" +
                         "\"applicant\": {" +
                         "\"id\": \"APP_003\"," +
                         "\"totalDebt\": 35000," +
                         "\"annualIncome\": 80000," +
                         "\"creditScore\": 750," +
                         "\"requestedCreditLimit\": 20000" +
                         "}" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        // Should trigger both actions: manual review (35000 > 80000*0.4=32000) 
        // AND approval (750>=700 AND 80000 > 20000*3=60000)
        assertEquals(2, summary.getActionsExecuted());
    }
    
    @Test
    public void testRule5_BehavioralPatterns() throws Exception {
        System.out.println("\n=== Testing Rule 5: Behavioral Patterns ===");
        
        String rulesDSL = "rule behavioralPatterns:\n" +
                         "    if transaction.amount > customer.averageTransactionAmount * 5 then flagForReview\n" +
                         "    if transaction.location != customer.homeLocation and transaction.amount > customer.travelThreshold then sendAlert";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test large transaction in different location
        String jsonData = "{" +
                         "\"transaction\": {" +
                         "\"id\": \"TXN_002\"," +
                         "\"amount\": 2000," +
                         "\"location\": \"Las Vegas\"" +
                         "}," +
                         "\"customer\": {" +
                         "\"id\": \"CUST_001\"," +
                         "\"averageTransactionAmount\": 300," +
                         "\"homeLocation\": \"New York\"," +
                         "\"travelThreshold\": 500" +
                         "}" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        // Should trigger both: flagForReview (2000 > 300*5=1500) AND sendAlert (location != home AND 2000 > 500)
        assertEquals(2, summary.getActionsExecuted());
    }
    
    @Test
    public void testRule6_RiskToleranceCheck() throws Exception {
        System.out.println("\n=== Testing Rule 6: Risk Tolerance Check ===");
        
        String rulesDSL = "rule riskToleranceCheck:\n" +
                         "    if merchant.riskScore > customer.riskTolerance then requireVerification\n" +
                         "    if transaction.timestamp > card.lastUsedTimestamp + 86400 and transaction.amount > customer.dormancyThreshold then requireStepUpAuth";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test high-risk merchant and dormant card
        String jsonData = "{" +
                         "\"merchant\": {" +
                         "\"riskScore\": 8" +
                         "}," +
                         "\"customer\": {" +
                         "\"id\": \"CUST_002\"," +
                         "\"riskTolerance\": 6," +
                         "\"dormancyThreshold\": 100" +
                         "}," +
                         "\"transaction\": {" +
                         "\"timestamp\": 1705500000," +
                         "\"amount\": 200" +
                         "}," +
                         "\"card\": {" +
                         "\"lastUsedTimestamp\": 1705400000" +
                         "}" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        // Should trigger both: requireVerification (8 > 6) AND requireStepUpAuth (dormant card + amount > threshold)
        assertEquals(2, summary.getActionsExecuted());
    }
    
    @Test
    public void testMultipleRulesPerformance() throws Exception {
        System.out.println("\n=== Testing Multiple Rules Performance ===");
        
        // Load all simple and medium rules together
        String rulesDSL = "rule ageVerification:\n" +
                         "    if applicant.age < 18 then rejectApplication\n" +
                         "\n" +
                         "rule incomeExpenseCheck:\n" +
                         "    if applicant.annualIncome < applicant.monthlyExpenses * 12 then rejectApplication\n" +
                         "\n" +
                         "rule creditLimitCheck:\n" +
                         "    if transaction.amount > card.availableBalance then decline\n" +
                         "\n" +
                         "rule debtToIncomeCheck:\n" +
                         "    if applicant.totalDebt > applicant.annualIncome * 0.4 then requireManualReview";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Comprehensive test data
        String jsonData = "{" +
                         "\"applicant\": {" +
                         "\"id\": \"APP_PERF\"," +
                         "\"age\": 25," +
                         "\"annualIncome\": 60000," +
                         "\"monthlyExpenses\": 4000," +
                         "\"totalDebt\": 30000" +
                         "}," +
                         "\"transaction\": {" +
                         "\"id\": \"TXN_PERF\"," +
                         "\"amount\": 800" +
                         "}," +
                         "\"card\": {" +
                         "\"availableBalance\": 1000" +
                         "}" +
                         "}";
        
        RuleContext context = new RuleContext(jsonData);
        
        // Performance test
        long totalTime = 0;
        int iterations = 1000;
        
        for (int i = 0; i < iterations; i++) {
            long start = System.nanoTime();
            RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
            long end = System.nanoTime();
            
            totalTime += (end - start);
            
            // Verify execution
            assertEquals(4, summary.getTotalRules());
            assertTrue(summary.getRulesExecuted() > 0);
        }
        
        double avgTimeMs = (totalTime / iterations) / 1_000_000.0;
        System.out.printf("Average execution time for 4 sample rules: %.3f ms%n", avgTimeMs);
        
        // Should still be very fast
        assertTrue(avgTimeMs < 15.0, "Multiple sample rules should execute under 15ms on average");
    }
    
    /**
     * Mock action implementation for testing.
     */
    private static class MockAction implements com.rules.actions.Action {
        private final String actionName;
        
        public MockAction(String actionName) {
            this.actionName = actionName;
        }
        
        @Override
        public void execute(RuleContext context) {
            switch (actionName) {
                case "decline":
                    System.out.println("DECLINE: Transaction " + context.getString("transaction.id") + 
                                     " declined, amount: $" + context.getDouble("transaction.amount"));
                    break;
                case "flagForReview":
                    System.out.println("FLAG: Transaction " + context.getString("transaction.id") + 
                                     " flagged for review");
                    break;
                case "sendAlert":
                    System.out.println("ALERT: Sent to customer " + context.getString("customer.id"));
                    break;
                case "requireVerification":
                    System.out.println("VERIFY: Customer " + context.getString("customer.id") + 
                                     " requires verification");
                    break;
                case "requireStepUpAuth":
                    System.out.println("STEPUP: Customer " + context.getString("customer.id") + 
                                     " requires step-up authentication");
                    break;
                default:
                    System.out.println("MOCK ACTION: " + actionName);
            }
        }
        
        @Override
        public String getName() {
            return actionName;
        }
    }
}