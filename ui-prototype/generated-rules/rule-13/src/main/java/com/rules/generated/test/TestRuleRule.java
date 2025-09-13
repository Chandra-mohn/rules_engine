package com.rules.generated.test;

import com.rules.context.RuleContext;
import com.rules.runtime.RuleResult;

/**
 * Generated rule library for: testRule
 * Rule ID: TestRuleRule
 * 
 * Original rule content:
 * rule testRule: if applicant.creditScore >= 700 then approveApplication
 * 
 * This is a library JAR - designed to be loaded into a main rules engine service.
 * Not intended as a standalone microservice.
 */
public class TestRuleRule {
    
    private static final String RULE_ID = "testrulerule";
    private static final String RULE_NAME = "testRule";
    
    /**
     * Execute this rule with the provided context.
     * 
     * @param context Rule execution context
     * @return Rule execution result
     */
    public static RuleResult execute(RuleContext context) {
        try {
            import java.util.Objects;

            /**
             * Generated rule class with type-safe attribute access
             * Auto-generated - do not modify manually
             */
            public class GeneratedRule implements Rule {

                private int getApplicantCreditscore(RuleContext ctx) {
                    Object value = ctx.getValue("applicant.creditScore");
                    if (value == null) return null;
                            if (value instanceof Number) {
                        return ((Number) value).intValue();
                    }
                    try {
                        return Integer.parseInt(value.toString());
                    } catch (NumberFormatException e) {
                        return 0;
                    }
                }

                @Override
                public RuleResult execute(RuleContext ctx) {
                    return RuleResult.noMatch();
                }

                @Override
                public String getRuleName() {
                    return "testRule";
                }
            }
        } catch (Exception e) {
            return RuleResult.error("Rule execution failed: " + e.getMessage());
        }
    }
    
    /**
     * Get the rule ID.
     */
    public static String getRuleId() {
        return RULE_ID;
    }
    
    /**
     * Get the human-readable rule name.
     */
    public static String getRuleName() {
        return RULE_NAME;
    }
}