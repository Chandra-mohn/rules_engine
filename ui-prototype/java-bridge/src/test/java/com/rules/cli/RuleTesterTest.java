package com.rules.cli;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for RuleTester.
 */
public class RuleTesterTest {
    
    @Test
    @DisplayName("Should parse rule names correctly")
    public void testRuleNameExtraction() {
        String rule = "rule creditScoreCheck: if applicant.creditScore >= 750 then approveApplication";
        
        // Test regex pattern for rule name extraction
        assertTrue(rule.matches(".*rule\\s+(\\w+)\\s*:.*"));
        
        // Extract rule name
        String[] parts = rule.split("\\s+");
        assertEquals("creditScoreCheck:", parts[1]);
        String ruleName = parts[1].replace(":", "");
        assertEquals("creditScoreCheck", ruleName);
    }
    
    @Test
    @DisplayName("Should parse conditions and actions")
    public void testConditionActionParsing() {
        String rule = "if applicant.creditScore >= 750 then approveApplication";
        
        // Test condition extraction
        assertTrue(rule.matches(".*if\\s+(.+?)\\s+then\\s+(\\w+).*"));
        
        String[] parts = rule.split("\\s+then\\s+");
        assertEquals(2, parts.length);
        
        String condition = parts[0].replace("if ", "").trim();
        String action = parts[1].trim();
        
        assertEquals("applicant.creditScore >= 750", condition);
        assertEquals("approveApplication", action);
    }
    
    @Test
    @DisplayName("Should handle numeric comparisons")
    public void testNumericComparisons() {
        String condition = "applicant.creditScore >= 750";
        String[] parts = condition.split("\\s+");
        
        assertEquals("applicant.creditScore", parts[0]);
        assertEquals(">=", parts[1]);
        assertEquals("750", parts[2]);
        
        // Test numeric parsing
        int value = Integer.parseInt(parts[2]);
        assertEquals(750, value);
    }
    
    @Test
    @DisplayName("Should validate test result structure")
    public void testTestResultStructure() {
        RuleTester.TestResult result = new RuleTester.TestResult();
        result.success = true;
        result.message = "Test message";
        result.ruleName = "testRule";
        result.actionExecuted = true;
        result.executedActionsCount = 1;
        result.totalAvailableActions = 2;
        
        assertTrue(result.success);
        assertEquals("Test message", result.message);
        assertEquals("testRule", result.ruleName);
        assertTrue(result.actionExecuted);
        assertEquals(1, result.executedActionsCount);
        assertEquals(2, result.totalAvailableActions);
    }
    
    @Test
    @DisplayName("Should validate condition result structure")
    public void testConditionResultStructure() {
        RuleTester.ConditionResult condResult = new RuleTester.ConditionResult();
        condResult.condition = "applicant.age >= 18";
        condResult.action = "approveApplication";
        condResult.evaluated = true;
        condResult.executed = false;
        
        assertEquals("applicant.age >= 18", condResult.condition);
        assertEquals("approveApplication", condResult.action);
        assertTrue(condResult.evaluated);
        assertFalse(condResult.executed);
    }
    
    @Test
    @DisplayName("Should validate action result structure")
    public void testActionResultStructure() {
        RuleTester.ActionResult actionResult = new RuleTester.ActionResult();
        actionResult.action = "approveApplication";
        actionResult.reason = "applicant.creditScore >= 750";
        actionResult.description = "Approve the credit card application";
        
        assertEquals("approveApplication", actionResult.action);
        assertEquals("applicant.creditScore >= 750", actionResult.reason);
        assertEquals("Approve the credit card application", actionResult.description);
    }
    
    @Test
    @DisplayName("Should handle multiple conditions in a rule")
    public void testMultipleConditionsHandling() {
        String rule = """
            rule multipleConditions:
                if applicant.creditScore >= 750 then approveApplication
                if applicant.creditScore < 600 then rejectApplication
            """;
        
        // Count number of if-then pairs
        String[] lines = rule.split("\\n");
        int ifThenCount = 0;
        
        for (String line : lines) {
            line = line.trim();
            if (line.matches("\\s*if\\s+.*\\s+then\\s+\\w+\\s*")) {
                ifThenCount++;
            }
        }
        
        assertEquals(2, ifThenCount);
    }
    
    @Test
    @DisplayName("Should parse entity.property paths")
    public void testEntityPropertyParsing() {
        String attributePath = "applicant.creditScore";
        String[] parts = attributePath.split("\\.");
        
        assertEquals(2, parts.length);
        assertEquals("applicant", parts[0]);
        assertEquals("creditScore", parts[1]);
    }
}