package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * {{ rule_name }}
 * Generated from DSL rule definition
 * Performance Category: {{ performance_category }}
 * Complexity Score: {{ complexity_score }}/10
 */
public class {{ class_name }} {

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
{% for entity in entities %}
        Map<String, Object> {{ entity }} = (Map<String, Object>) context.get("{{ entity }}");
{% endfor %}

        // Rule logic
{% for step in rule_steps %}
{{ step | indent(8, true) }}
{% endfor %}

        return new RuleResult(matched, actions, finalAction);
    }

{{ helper_methods }}
}
