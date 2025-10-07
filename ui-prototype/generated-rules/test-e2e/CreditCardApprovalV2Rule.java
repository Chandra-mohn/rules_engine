package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * Credit Card Approval V2
 * Generated from DSL rule definition
 * Performance Category: hot
 * Complexity Score: 1/10
 */
public class CreditCardApprovalV2Rule {

    public static class RuleResult {
        private final boolean matched;
        private final List<String> actions;
        private final String finalAction;

        public RuleResult(boolean matched, List<String> actions, String finalAction) {
            this.matched = matched;
            this.actions = actions;
            this.finalAction = finalAction;
        }

        public boolean isMatched() { return matched; }
        public List<String> getActions() { return actions; }
        public String getFinalAction() { return finalAction; }
    }

    public static RuleResult evaluate(Map<String, Object> context) {
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = false;

        // Extract entities from context
        Map<String, Object> applicant = (Map<String, Object>) context.get("applicant");

        // Rule logic
        if ((_compareTo(_getFieldValue((Map<String, Object>)applicant, "creditScore"), 700) >= 0) && (_compareTo(_getFieldValue((Map<String, Object>)applicant, "annualIncome"), 60000) >= 0)) {
            matched = true;
            actions.add("approveApplication");
            actions.add("setLimit((_getFieldValue((Map<String, Object>)applicant, \"annualIncome\") * 0.3))");
            actions.add("applyBonus((_getFieldValue((Map<String, Object>)applicant, \"creditScore\") / 10))");
        } else {
            actions.add("applicant");
        }
        matched = true;
        actions.add("creditScore");
        

        return new RuleResult(matched, actions, finalAction);
    }

    // Helper method: Get field value with null safety
    private static Object _getFieldValue(Map<String, Object> entity, String fieldName) {
        return entity != null ? entity.get(fieldName) : null;
    }

    // Helper method: Null-safe equality comparison
    private static boolean _equals(Object a, Object b) {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        return a.toString().equals(b.toString());
    }

    // Helper method: Type-safe numeric comparison
    private static int _compareTo(Object a, Object b) {
        if (a == null || b == null) return 0;
        try {
            if (a instanceof Number && b instanceof Number) {
                return Double.compare(((Number)a).doubleValue(), ((Number)b).doubleValue());
            }
            return a.toString().compareTo(b.toString());
        } catch (Exception e) {
            return 0;
        }
    }

    // Helper method: Null-safe numeric conversion
    private static double _toNumber(Object obj) {
        if (obj == null) return 0.0;
        if (obj instanceof Number) return ((Number)obj).doubleValue();
        try {
            return Double.parseDouble(obj.toString());
        } catch (NumberFormatException e) {
            return 0.0;
        }
    }
}
