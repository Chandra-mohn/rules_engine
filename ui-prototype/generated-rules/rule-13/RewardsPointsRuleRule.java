package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

public class RewardsPointsRuleRule {

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

        Map<String, Object> applicant = (Map<String, Object>) context.get("applicant");


        if (_compareTo(_getFieldValue(applicant, "creditScore"), 700) >= 0 && _compareTo(_getFieldValue(applicant, "annualIncome"), 60000) >= 0) {
            matched = true;
            actions.add("approveApplication");
        }
        if (_getFieldValue(applicant, "employmentStatus") == "employed" && _compareTo(_getFieldValue(applicant, "employmentYears"), 3) >= 0) {
            matched = true;
            actions.add("approveApplication");
        }
        if (_compareTo(_getFieldValue(applicant, "creditScore"), 680) < 0) {
            matched = true;
            actions.add("rejectApplication");
        }

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