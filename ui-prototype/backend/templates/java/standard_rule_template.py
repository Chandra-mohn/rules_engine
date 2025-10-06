"""
Standard rule template using Python f-strings.
No external template engine dependencies.
"""

def get_helper_methods():
    """Generate helper methods code."""
    return '''
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
    }'''


def generate_standard_rule(rule_name, class_name, entities, rule_steps, complexity_score, performance_category):
    """
    Generate standard rule Java code using f-strings.

    Args:
        rule_name: Rule display name
        class_name: Java class name (PascalCase)
        entities: List of entity names (e.g., ['applicant', 'transaction'])
        rule_steps: List of Java code strings for each rule step
        complexity_score: 0-10 complexity rating
        performance_category: 'hot', 'warm', or 'cold'

    Returns:
        str: Complete Java class code
    """

    # Generate entity extraction code
    entity_declarations = '\n'.join([
        f'        Map<String, Object> {entity} = (Map<String, Object>) context.get("{entity}");'
        for entity in sorted(entities)
    ])

    # Generate rule step code (indent by 8 spaces)
    rule_logic = '\n'.join([
        '\n'.join(f'        {line}' for line in step.split('\n'))
        for step in rule_steps
    ])

    # Build complete class
    code = f'''package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * {rule_name}
 * Generated from DSL rule definition
 * Performance Category: {performance_category}
 * Complexity Score: {complexity_score}/10
 */
public class {class_name} {{

    public static class RuleResult {{
        private final boolean matched;
        private final List<String> actions;
        private final String finalAction;

        public RuleResult(boolean matched, List<String> actions, String finalAction) {{
            this.matched = matched;
            this.actions = actions;
            this.finalAction = finalAction;
        }}

        public boolean isMatched() {{ return matched; }}
        public List<String> getActions() {{ return actions; }}
        public String getFinalAction() {{ return finalAction; }}
    }}

    public static RuleResult evaluate(Map<String, Object> context) {{
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = false;

        // Extract entities from context
{entity_declarations}

        // Rule logic
{rule_logic}

        return new RuleResult(matched, actions, finalAction);
    }}
{get_helper_methods()}
}}
'''

    return code


def generate_actionset(rule_name, class_name, entities, rule_steps, complexity_score, performance_category):
    """Generate ActionSet Java code (evaluates all conditions)."""

    entity_declarations = '\n'.join([
        f'        Map<String, Object> {entity} = (Map<String, Object>) context.get("{entity}");'
        for entity in sorted(entities)
    ])

    rule_logic = '\n'.join([
        '\n'.join(f'        {line}' for line in step.split('\n'))
        for step in rule_steps
    ])

    code = f'''package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * {rule_name} (ActionSet)
 * Generated from DSL actionset definition
 * Performance Category: {performance_category}
 * Complexity Score: {complexity_score}/10
 */
public class {class_name} {{

    public static class RuleResult {{
        private final boolean matched;
        private final List<String> actions;
        private final String finalAction;

        public RuleResult(boolean matched, List<String> actions, String finalAction) {{
            this.matched = matched;
            this.actions = actions;
            this.finalAction = finalAction;
        }}

        public boolean isMatched() {{ return matched; }}
        public List<String> getActions() {{ return actions; }}
        public String getFinalAction() {{ return finalAction; }}
    }}

    public static RuleResult evaluate(Map<String, Object> context) {{
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = false;

        // Extract entities from context
{entity_declarations}

        // ActionSet logic - evaluates all matching conditions
{rule_logic}

        return new RuleResult(matched, actions, finalAction);
    }}
{get_helper_methods()}
}}
'''

    return code


def generate_action(rule_name, class_name, entities, actions_list, performance_category):
    """Generate direct Action Java code (no conditions)."""

    entity_declarations = '\n'.join([
        f'        Map<String, Object> {entity} = (Map<String, Object>) context.get("{entity}");'
        for entity in sorted(entities)
    ])

    action_code = '\n'.join([
        f'        actions.add("{action}");'
        for action in actions_list
    ])

    code = f'''package com.rules;

import java.util.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * {rule_name} (Action)
 * Generated from DSL action definition
 * Performance Category: {performance_category}
 */
public class {class_name} {{

    public static class RuleResult {{
        private final boolean matched;
        private final List<String> actions;
        private final String finalAction;

        public RuleResult(boolean matched, List<String> actions, String finalAction) {{
            this.matched = matched;
            this.actions = actions;
            this.finalAction = finalAction;
        }}

        public boolean isMatched() {{ return matched; }}
        public List<String> getActions() {{ return actions; }}
        public String getFinalAction() {{ return finalAction; }}
    }}

    public static RuleResult evaluate(Map<String, Object> context) {{
        List<String> actions = new ArrayList<>();
        String finalAction = null;
        boolean matched = true;

        // Extract entities from context
{entity_declarations}

        // Direct action execution
{action_code}

        return new RuleResult(matched, actions, finalAction);
    }}
{get_helper_methods()}
}}
'''

    return code
