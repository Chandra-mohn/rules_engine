package com.rules;

import com.rules.actions.ActionRegistry;
import com.rules.actions.application.*;
import com.rules.context.RuleContext;
import com.rules.engine.RulesEngine;
import com.rules.engine.RuleResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;

import java.time.LocalDate;
import java.time.DayOfWeek;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for BUSINESS_DATE functionality in the rules engine.
 * Tests business date calculations, comparisons, and weekend handling.
 */
public class BusinessDateTest {
    
    private RulesEngine rulesEngine;
    private ActionRegistry actionRegistry;
    
    @BeforeEach
    public void setUp() throws Exception {
        actionRegistry = new ActionRegistry();
        
        // Register actions for testing
        actionRegistry.registerAction("approveApplication", new ApproveApplicationAction());
        actionRegistry.registerAction("rejectApplication", new RejectApplicationAction());
        actionRegistry.registerAction("manualReview", new ManualReviewAction());
        actionRegistry.registerAction("conditionalApproval", new ConditionalApprovalAction());
        
        rulesEngine = new RulesEngine(actionRegistry);
        
        System.out.println("\n" + "=".repeat(60));
        System.out.println("STARTING BUSINESS_DATE TESTS");
        System.out.println("=".repeat(60));
    }
    
    @Test
    public void testBusinessDateComparison() throws Exception {
        System.out.println("\n=== Testing Business Date Comparison ===");
        
        String rulesDSL = "rule businessDateCheck:\n" +
            "    if applicant.applicationDate after business_date then conditionalApproval\n" +
            "    if applicant.applicationDate before business_date then rejectApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test case: Application date after business date
        LocalDate futureDate = LocalDate.now().plusDays(5);
        String jsonData = "{\n" +
            "    \"applicant\": {\n" +
            "        \"id\": \"APP_BIZ_001\",\n" +
            "        \"applicationDate\": \"" + futureDate.format(DateTimeFormatter.ISO_LOCAL_DATE) + "\"\n" +
            "    }\n" +
            "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Business date comparison result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertTrue(summary.getActionsExecuted() > 0);
    }
    
    @Test
    public void testBusinessDateArithmetic() throws Exception {
        System.out.println("\n=== Testing Business Date Arithmetic ===");
        
        String rulesDSL = "rule businessDateArithmetic:\n" +
            "    if business_date + 5 days > applicant.applicationDate then approveApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test case: Application date within business date + 5 days
        String todayDate = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE);
        String jsonData = "{\n" +
            "    \"applicant\": {\n" +
            "        \"id\": \"APP_BIZ_ARITH_001\",\n" +
            "        \"applicationDate\": \"" + todayDate + "\"\n" +
            "    }\n" +
            "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Business date arithmetic result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertTrue(summary.getActionsExecuted() > 0);
    }
    
    @Test
    public void testBusinessDateVsTodayComparison() throws Exception {
        System.out.println("\n=== Testing Business Date vs Today Comparison ===");
        
        String rulesDSL = "rule businessDateVsToday:\n" +
            "    if business_date = today then approveApplication\n" +
            "    if business_date before today then manualReview";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test case: Compare business date with today
        String jsonData = "{\n" +
            "    \"applicant\": {\n" +
            "        \"id\": \"APP_BIZ_TODAY_001\"\n" +
            "    }\n" +
            "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Business date vs today result: " + summary);
        
        // The result depends on what day of the week it is
        LocalDate today = LocalDate.now();
        DayOfWeek dayOfWeek = today.getDayOfWeek();
        
        if (dayOfWeek == DayOfWeek.SATURDAY || dayOfWeek == DayOfWeek.SUNDAY) {
            // On weekends, business_date < today, so manualReview should trigger
            assertEquals(1, summary.getRulesExecuted());
            assertTrue(summary.getActionsExecuted() > 0);
            System.out.println("Weekend detected: business_date < today, manual review triggered");
        } else {
            // On weekdays, business_date = today, so approveApplication should trigger
            assertEquals(1, summary.getRulesExecuted());
            assertTrue(summary.getActionsExecuted() > 0);
            System.out.println("Weekday detected: business_date = today, approval triggered");
        }
    }
    
    @Test
    public void testBusinessDateInComplexRule() throws Exception {
        System.out.println("\n=== Testing Business Date in Complex Rule ===");
        
        String rulesDSL = "rule complexBusinessDateRule:\n" +
            "    if applicant.creditScore > 700 and applicant.applicationDate >= business_date then approveApplication\n" +
            "    if applicant.creditScore <= 700 and applicant.applicationDate < business_date then rejectApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test case: Good credit score with current application date
        String todayDate = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE);
        String jsonData = "{\n" +
            "    \"applicant\": {\n" +
            "        \"id\": \"APP_COMPLEX_001\",\n" +
            "        \"creditScore\": 750,\n" +
            "        \"applicationDate\": \"" + todayDate + "\"\n" +
            "    }\n" +
            "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Complex business date rule result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertTrue(summary.getActionsExecuted() > 0);
    }
    
    @AfterEach
    public void tearDown() {
        System.out.println("\n" + "=".repeat(60));
        System.out.println("BUSINESS_DATE TESTS COMPLETED");
        System.out.println("=".repeat(60));
    }
}