package com.test.rules;

import com.rules.context.RuleContext;
import com.rules.runtime.RuleResult;

/**
 * Generated rule library for: Type Test Rule
 * Rule ID: TypeTestRuleRule
 * 
 * Original rule content:
 * rule typeTestRule: if applicant.creditScore > 700 and applicant.age >= 18 then approveApplication
 * 
 * This is a library JAR - designed to be loaded into a main rules engine service.
 * Not intended as a standalone microservice.
 */
public class TypeTestRuleRule {
    
    private static final String RULE_ID = "typetestrulerule";
    private static final String RULE_NAME = "Type Test Rule";
    
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

                private int getApplicantAge(RuleContext ctx) {
                    Object value = ctx.getValue("applicant.age");
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
                    return "typeTestRule";
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