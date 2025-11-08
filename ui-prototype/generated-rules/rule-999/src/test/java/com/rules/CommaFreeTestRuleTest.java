package com.rules;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

/**
 * Unit tests for Comma Free Test
 * Generated from DSL rule definition
 * Complexity Score: 1/10
 */
@DisplayName("Comma Free Test - Test Suite")
public class CommaFreeTestRuleTest {

    private Map<String, Object> context;

    @BeforeEach
    void setUp() {
        context = new HashMap<>();
    }

    @Test
    @DisplayName("Should match when main condition is true")
    void shouldMatchWhen_IfConditionTrue() {
        // Arrange
        // TODO: Set up test data for entities: customer
        context.put("customer", new HashMap<>());

        // Act
        CommaFreeTestRule.RuleResult result = CommaFreeTestRule.evaluate(context);

        // Assert
        assertEquals(true, result.isMatched(), "Rule should match");
        assertEquals(2, result.getActions().size(), "Expected 2 action(s)");
        assertEquals("approveTransaction", result.getActions().get(0), "Action 0 should be approveTransaction");
        assertEquals("sendNotification", result.getActions().get(1), "Action 1 should be sendNotification");
        assertNull(result.getFinalAction(), "Final action should be null");
    }

    @Test
    @DisplayName("Should not match when main condition is false")
    void shouldNotMatchWhen_IfConditionFalse() {
        // Arrange
        // TODO: Set up test data for entities: customer
        context.put("customer", new HashMap<>());

        // Act
        CommaFreeTestRule.RuleResult result = CommaFreeTestRule.evaluate(context);

        // Assert
        assertEquals(false, result.isMatched(), "Rule should not match");
    }

    // Helper method: Create entity map from key-value pairs
    private Map<String, Object> createEntity(Object... pairs) {
        Map<String, Object> entity = new HashMap<>();
        for (int i = 0; i < pairs.length; i += 2) {
            entity.put((String) pairs[i], pairs[i + 1]);
        }
        return entity;
    }

    // Helper method: Create nested entity structure
    private Map<String, Object> createNestedEntity(String path, Object value) {
        Map<String, Object> result = new HashMap<>();
        String[] parts = path.split("\\.");
        Map<String, Object> current = result;

        for (int i = 0; i < parts.length - 1; i++) {
            Map<String, Object> next = new HashMap<>();
            current.put(parts[i], next);
            current = next;
        }
        current.put(parts[parts.length - 1], value);

        return result;
    }
}
