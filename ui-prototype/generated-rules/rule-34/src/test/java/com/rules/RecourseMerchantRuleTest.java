package com.rules;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

/**
 * Unit tests for RecourseMerchant
 * Generated from DSL rule definition
 * Complexity Score: 4/10
 */
@DisplayName("RecourseMerchant - Test Suite")
public class RecoursemerchantRuleTest {

    private Map<String, Object> context;

    @BeforeEach
    void setUp() {
        context = new HashMap<>();
    }

    @Test
    @DisplayName("Should always execute direct actions")
    void shouldAlwaysExecute_DirectActions() {
        // Arrange
        // TODO: Set up test data for entities: RecourseMerchantIdentifier, recourseCode, trnmRecourseFlag, trnmRecourseMerchant
        context.put("RecourseMerchantIdentifier", new HashMap<>());
        context.put("recourseCode", new HashMap<>());
        context.put("trnmRecourseFlag", new HashMap<>());
        context.put("trnmRecourseMerchant", new HashMap<>());

        // Act
        RecoursemerchantRule.RuleResult result = RecoursemerchantRule.evaluate(context);

        // Assert
        assertEquals(true, result.isMatched(), "Rule should match");
        assertEquals(1, result.getActions().size(), "Expected 1 action(s)");
        assertTrue(result.getActions().get(0).startsWith("getServiceDomain("), "Action 0 should be getServiceDomain");
        assertNull(result.getFinalAction(), "Final action should be null");
    }

    @Test
    @DisplayName("Should always execute direct actions")
    void shouldAlwaysExecute_DirectActions() {
        // Arrange
        // TODO: Set up test data for entities: RecourseMerchantIdentifier, recourseCode, trnmRecourseFlag, trnmRecourseMerchant
        context.put("RecourseMerchantIdentifier", new HashMap<>());
        context.put("recourseCode", new HashMap<>());
        context.put("trnmRecourseFlag", new HashMap<>());
        context.put("trnmRecourseMerchant", new HashMap<>());

        // Act
        RecoursemerchantRule.RuleResult result = RecoursemerchantRule.evaluate(context);

        // Assert
        assertEquals(true, result.isMatched(), "Rule should match");
        assertEquals(1, result.getActions().size(), "Expected 1 action(s)");
        assertTrue(result.getActions().get(0).startsWith("reportLog("), "Action 0 should be reportLog");
        assertNull(result.getFinalAction(), "Final action should be null");
    }

    @Test
    @DisplayName("Should always execute direct actions")
    void shouldAlwaysExecute_DirectActions() {
        // Arrange
        // TODO: Set up test data for entities: RecourseMerchantIdentifier, recourseCode, trnmRecourseFlag, trnmRecourseMerchant
        context.put("RecourseMerchantIdentifier", new HashMap<>());
        context.put("recourseCode", new HashMap<>());
        context.put("trnmRecourseFlag", new HashMap<>());
        context.put("trnmRecourseMerchant", new HashMap<>());

        // Act
        RecoursemerchantRule.RuleResult result = RecoursemerchantRule.evaluate(context);

        // Assert
        assertEquals(true, result.isMatched(), "Rule should match");
        assertEquals(1, result.getActions().size(), "Expected 1 action(s)");
        assertTrue(result.getActions().get(0).startsWith("reportLog("), "Action 0 should be reportLog");
        assertNull(result.getFinalAction(), "Final action should be null");
    }

    @Test
    @DisplayName("Should match when main condition is true")
    void shouldMatchWhen_IfConditionTrue() {
        // Arrange
        // TODO: Set up test data for entities: RecourseMerchantIdentifier, recourseCode, trnmRecourseFlag, trnmRecourseMerchant
        context.put("RecourseMerchantIdentifier", new HashMap<>());
        context.put("recourseCode", new HashMap<>());
        context.put("trnmRecourseFlag", new HashMap<>());
        context.put("trnmRecourseMerchant", new HashMap<>());

        // Act
        RecoursemerchantRule.RuleResult result = RecoursemerchantRule.evaluate(context);

        // Assert
        assertEquals(true, result.isMatched(), "Rule should match");
        assertEquals(5, result.getActions().size(), "Expected 5 action(s)");
        assertTrue(result.getActions().get(0).startsWith("reportLog("), "Action 0 should be reportLog");
        assertTrue(result.getActions().get(1).startsWith("updateServiceDomain("), "Action 1 should be updateServiceDomain");
        assertTrue(result.getActions().get(2).startsWith("reportLog("), "Action 2 should be reportLog");
        assertTrue(result.getActions().get(3).startsWith("updateServiceDomain("), "Action 3 should be updateServiceDomain");
        assertTrue(result.getActions().get(4).startsWith("reportLog("), "Action 4 should be reportLog");
        assertNull(result.getFinalAction(), "Final action should be null");
    }

    @Test
    @DisplayName("Should not match when main condition is false")
    void shouldNotMatchWhen_IfConditionFalse() {
        // Arrange
        // TODO: Set up test data for entities: RecourseMerchantIdentifier, recourseCode, trnmRecourseFlag, trnmRecourseMerchant
        context.put("RecourseMerchantIdentifier", new HashMap<>());
        context.put("recourseCode", new HashMap<>());
        context.put("trnmRecourseFlag", new HashMap<>());
        context.put("trnmRecourseMerchant", new HashMap<>());

        // Act
        RecoursemerchantRule.RuleResult result = RecoursemerchantRule.evaluate(context);

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
