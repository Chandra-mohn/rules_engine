package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

public class AdditionalCardManagementRule {

    public static class RuleResult {
        private boolean matched;
        private List<String> actions;
        private String finalAction;

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

        Map<String, Object> account = (Map<String, Object>) context.get("account");
        Map<String, Object> request = (Map<String, Object>) context.get("request");


        if (_getFieldValue(request, "type") == "ADD_CARD" && _compareTo(_getFieldValue(account, "cardCount"), 3) < 0) {
            matched = true;
        }
        else {
            actions.add("if request.type == "DISABLE_CARD" and request.reason == "LOST" then");
        }
        matched = true;
        actions.add("issueAdditionalCard(request.cardType)");
        actions.add("updateAccountStatus("ACTIVE")");
        actions.add("disableCard(request.cardNumber)");
        actions.add("alertFraudDepartment(request.cardNumber)");
        actions.add("else");
        actions.add("rejectRequest("Card limit reached")");

        return new RuleResult(matched, actions, finalAction);
    }


    private static Object _getFieldValue(Map<String, Object> entity, String fieldName) {
        return entity != null ? entity.get(fieldName) : null;
    }

    private static boolean _equals(Object a, Object b) {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        return a.toString().equals(b.toString());
    }

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
}