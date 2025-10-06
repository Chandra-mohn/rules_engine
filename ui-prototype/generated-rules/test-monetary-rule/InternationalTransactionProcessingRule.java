package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * International Transaction Processing
 * Generated from DSL rule definition
 * Performance Category: hot
 * Complexity Score: 1/10
 */
public class InternationalTransactionProcessingRule {

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
        Map<String, Object> account = (Map<String, Object>) context.get("account");
        Map<String, Object> transaction = (Map<String, Object>) context.get("transaction");

        // Rule logic
        if ((!_equals(_getFieldValue((Map<String, Object>)_getFieldValue((Map<String, Object>)transaction, "location"), "country"), _getFieldValue(context, "US"))) && (_compareTo(_getFieldValue((Map<String, Object>)transaction, "amount"), _getFieldValue((Map<String, Object>)account, "internationalLimit")) <= 0)) {
            matched = true;
            actions.add("approveTransaction(_getFieldValue((Map<String, Object>)transaction, \"amount\"))");
            actions.add("applyForeignExchangeFee((_getFieldValue((Map<String, Object>)transaction, \"amount\") * 0.025))");
            actions.add("updateAccountBalance((_getFieldValue((Map<String, Object>)transaction, \"amount\") + (_getFieldValue((Map<String, Object>)transaction, \"amount\") * 0.025)))");
        } else {
            actions.add("transaction");
        }

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
