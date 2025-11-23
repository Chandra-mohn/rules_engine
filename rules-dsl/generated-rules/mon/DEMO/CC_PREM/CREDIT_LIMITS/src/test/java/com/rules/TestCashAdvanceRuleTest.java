package com.rules;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

/**
 * Unit tests for Test Cash Advance
 * Generated from DSL rule definition
 * Complexity Score: 1/10
 */
@DisplayName("Test Cash Advance - Test Suite")
public class TestCashAdvanceRuleTest {

    private Map<String, Object> context;

    @BeforeEach
    void setUp() {
        context = new HashMap<>();
    }

    @Test
    @DisplayName("Should match when main condition is true")
    void shouldMatchWhen_IfConditionTrue() {
        // Arrange
        // TODO: Set up test data for entities: transaction
        context.put("transaction", new HashMap<>());

        // Act
        TestCashAdvanceRule.RuleResult result = TestCashAdvanceRule.evaluate(context);

        // Assert
        assertEquals(true, result.isMatched(), "Rule should match");
        assertEquals(1, result.getActions().size(), "Expected 1 action(s)");
        assertEquals("set_limit", result.getActions().get(0), "Action 0 should be set_limit");
        assertNull(result.getFinalAction(), "Final action should be null");
    }

    @Test
    @DisplayName("Should not match when main condition is false")
    void shouldNotMatchWhen_IfConditionFalse() {
        // Arrange
        // TODO: Set up test data for entities: transaction
        context.put("transaction", new HashMap<>());

        // Act
        TestCashAdvanceRule.RuleResult result = TestCashAdvanceRule.evaluate(context);

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
