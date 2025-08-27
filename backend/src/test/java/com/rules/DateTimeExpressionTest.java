package com.rules;

import com.rules.actions.ActionRegistry;
import com.rules.actions.application.*;
import com.rules.context.RuleContext;
import com.rules.engine.RulesEngine;
import com.rules.engine.RuleResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Comprehensive tests for date/time expressions in the rules engine.
 * Tests various date/time operations, comparisons, and functions.
 */
public class DateTimeExpressionTest {
    
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
        actionRegistry.registerAction("instantApproval", new InstantApprovalAction());
        actionRegistry.registerAction("requireManualReview", new RequireManualReviewAction());
        
        rulesEngine = new RulesEngine(actionRegistry);
        
        System.out.println("\n" + "=".repeat(60));
        System.out.println("STARTING DATE/TIME EXPRESSION TESTS");
        System.out.println("=".repeat(60));
    }
    
    @Test
    public void testBasicDateComparisons() throws Exception {
        System.out.println("\n=== Testing Basic Date Comparisons ===");
        
        String rulesDSL = "rule dateComparison:\n" +
            "    if applicant.birthDate before \"2000-01-01\" then approveApplication\n" +
            "    if applicant.applicationDate after \"2025-01-01\" then rejectApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test case: Birth date before 2000, application date after 2025
        String jsonData = "{\n" +
            "    \"applicant\": {\n" +
            "        \"id\": \"APP_DATE_001\",\n" +
            "        \"birthDate\": \"1995-06-15\",\n" +
            "        \"applicationDate\": \"2025-08-25\"\n" +
            "    }\n" +
            "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Date comparison result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertTrue(summary.getActionsExecuted() > 0);
    }
    
    @Test
    public void testAgeCalculation() throws Exception {
        System.out.println("\n=== Testing Age Calculation ===");
        
        String rulesDSL = "rule ageCheck:\n" +
            "    if year_of(now) - year_of(applicant.birthDate) >= 18 then approveApplication\n" +
            "    if year_of(now) - year_of(applicant.birthDate) < 18 then rejectApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test case: 25 years old (should approve)
        String jsonData = "{\n" +
            "    \"applicant\": {\n" +
            "        \"id\": \"APP_AGE_001\",\n" +
            "        \"birthDate\": \"1999-01-01\"\n" +
            "    }\n" +
            "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Age calculation result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertTrue(summary.getActionsExecuted() > 0);
    }
    
    @Test
    public void testWeekendDetection() throws Exception {
        System.out.println("\n=== Testing Weekend Detection ===");
        
        String rulesDSL = "rule weekendCheck:\n" +
            "    if day_of_week(transaction.timestamp) >= 6 then manualReview";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test case: Saturday transaction (should trigger manual review)
        String jsonData = "{\n" +
            "    \"transaction\": {\n" +
            "        \"id\": \"TXN_WEEKEND_001\",\n" +
            "        \"timestamp\": \"2025-08-23T14:30:00Z\",\n" +
            "        \"amount\": 1000.00\n" +
            "    }\n" +
            "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Weekend detection result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertTrue(summary.getActionsExecuted() > 0);
    }
    
    @Test
    public void testDateArithmetic() throws Exception {
        System.out.println("\n=== Testing Date Arithmetic ===");
        
        String rulesDSL = "rule dateArithmetic:\n" +
            "    if applicant.applicationDate + 30 days > today then conditionalApproval";
        
        rulesEngine.loadRules(rulesDSL);
        
        // Test case: Recent application (within 30 days)
        String todayDate = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE);
        String jsonData = "{\n" +
            "    \"applicant\": {\n" +
            "        \"id\": \"APP_ARITH_001\",\n" +
            "        \"applicationDate\": \"" + todayDate + "\"\n" +
            "    }\n" +
            "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Date arithmetic result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
        assertTrue(summary.getActionsExecuted() > 0);
    }
    
    @AfterEach
    public void tearDown() {
        System.out.println("\n" + "=".repeat(60));
        System.out.println("DATE/TIME EXPRESSION TESTS COMPLETED");
        System.out.println("=".repeat(60));
    }
}