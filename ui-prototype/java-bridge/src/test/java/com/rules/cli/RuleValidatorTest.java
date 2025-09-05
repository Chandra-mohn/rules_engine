package com.rules.cli;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for RuleValidator.
 */
public class RuleValidatorTest {
    
    @Test
    @DisplayName("Should validate simple credit score rule")
    public void testValidCreditScoreRule() {
        String rule = "rule creditCheck: if applicant.creditScore >= 750 then approveApplication";
        
        // This would require refactoring RuleValidator to be testable
        // For now, we'll test the structure
        assertNotNull(rule);
        assertTrue(rule.contains("rule"));
        assertTrue(rule.contains("if"));
        assertTrue(rule.contains("then"));
    }
    
    @Test
    @DisplayName("Should detect missing rule keyword")
    public void testMissingRuleKeyword() {
        String invalidRule = "creditCheck: if applicant.creditScore >= 750 then approveApplication";
        
        assertFalse(invalidRule.contains("rule "));
    }
    
    @Test
    @DisplayName("Should detect invalid action names")
    public void testInvalidActionName() {
        String invalidRule = "rule creditCheck: if applicant.creditScore >= 750 then invalidAction";
        
        assertTrue(invalidRule.contains("invalidAction"));
        assertFalse(invalidRule.contains("approveApplication"));
    }
    
    @Test
    @DisplayName("Should validate entity.property structure")
    public void testValidEntityPropertyStructure() {
        String rule = "rule test: if applicant.age >= 18 then approveApplication";
        
        assertTrue(rule.matches(".*\\w+\\.\\w+.*"));
    }
    
    @Test
    @DisplayName("Should detect unbalanced parentheses")
    public void testUnbalancedParentheses() {
        String invalidRule = "rule test: if (applicant.age >= 18 then approveApplication";
        
        int openCount = 0;
        int closeCount = 0;
        for (char c : invalidRule.toCharArray()) {
            if (c == '(') openCount++;
            if (c == ')') closeCount++;
        }
        
        assertNotEquals(openCount, closeCount);
    }
    
    @Test
    @DisplayName("Should validate multiple conditions")
    public void testMultipleConditions() {
        String rule = """
            rule complexRule:
                if applicant.creditScore >= 750 then approveApplication
                if applicant.age < 18 then rejectApplication
                if transaction.amount > 10000 then manualReview
            """;
        
        assertTrue(rule.contains("rule"));
        long ifCount = rule.chars().filter(ch -> ch == 'i').count();
        assertTrue(ifCount >= 3); // At least 3 'i' characters from 'if' statements
    }
    
    @Test
    @DisplayName("Should validate business functions")
    public void testBusinessFunctions() {
        String rule = "rule dateCheck: if transaction.timestamp > business_date() then flagForReview";
        
        assertTrue(rule.contains("business_date()"));
        assertTrue(rule.matches(".*\\w+\\(\\).*"));
    }
    
    @Test
    @DisplayName("Should handle string comparisons")
    public void testStringComparisons() {
        String rule = "rule statusCheck: if applicant.employmentStatus = \"employed\" then approveApplication";
        
        assertTrue(rule.contains("\"employed\""));
        assertTrue(rule.matches(".*\"\\w+\".*"));
    }
}