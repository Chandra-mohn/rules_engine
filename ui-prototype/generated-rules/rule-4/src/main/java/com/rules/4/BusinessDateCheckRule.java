package com.rules.4;

import com.rules.context.RuleContext;
import com.rules.runtime.RuleResult;

/**
 * Generated rule library for: businessDateCheck
 * Rule ID: BusinessDateCheckRule
 * 
 * Original rule content:
 * rule businessDateCheck:
    if applicant.applicationDate after business_date then conditionalApproval
    if applicant.applicationDate before business_date then rejectApplication
 * 
 * This is a library JAR - designed to be loaded into a main rules engine service.
 * Not intended as a standalone microservice.
 */
public class BusinessDateCheckRule {
    
    private static final String RULE_ID = "businessdatecheckrule";
    private static final String RULE_NAME = "businessDateCheck";
    
    /**
     * Execute this rule with the provided context.
     * 
     * @param context Rule execution context
     * @return Rule execution result
     */
    public static RuleResult execute(RuleContext context) {
        try {
            import java.time.LocalDate;
            import java.util.Objects;

            /**
             * Generated rule class with type-safe attribute access
             * Auto-generated - do not modify manually
             */
            public class GeneratedRule implements Rule {

                private LocalDate getApplicantApplicationdate(RuleContext ctx) {
                    Object value = ctx.getValue("applicant.applicationDate");
                    if (value == null) return null;
                            if (value instanceof LocalDate) {
                        return (LocalDate) value;
                    }
                    try {
                        return LocalDate.parse(value.toString());
                    } catch (Exception e) {
                        return null;
                    }
                }

                private LocalDate getApplicantApplicationdate(RuleContext ctx) {
                    Object value = ctx.getValue("applicant.applicationDate");
                    if (value == null) return null;
                            if (value instanceof LocalDate) {
                        return (LocalDate) value;
                    }
                    try {
                        return LocalDate.parse(value.toString());
                    } catch (Exception e) {
                        return null;
                    }
                }

                @Override
                public RuleResult execute(RuleContext ctx) {
                    if (getApplicantApplicationdate(ctx) after business_date) {
                        return RuleResult.action("conditionalApproval");
                    }

                    if (getApplicantApplicationdate(ctx) before business_date) {
                        return RuleResult.action("rejectApplication");
                    }

                    return RuleResult.noMatch();
                }

                @Override
                public String getRuleName() {
                    return "businessDateCheck";
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