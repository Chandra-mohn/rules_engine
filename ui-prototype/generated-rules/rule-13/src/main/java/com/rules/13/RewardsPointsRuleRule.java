package com.rules.13;

import com.rules.context.RuleContext;
import com.rules.runtime.RuleResult;

/**
 * Generated rule library for: rewardsPointsRule
 * Rule ID: RewardsPointsRuleRule
 * 
 * Original rule content:
 * rule rewardsPointsRule:
    if applicant.creditScore >= 700 and applicant.annualIncome >= 60000 then approveApplication
    if applicant.employmentStatus == "employed" and applicant.employmentYears >= 3 then approveApplication
    if applicant.creditScore < 680 then rejectApplication
 * 
 * This is a library JAR - designed to be loaded into a main rules engine service.
 * Not intended as a standalone microservice.
 */
public class RewardsPointsRuleRule {
    
    private static final String RULE_ID = "rewardspointsrulerule";
    private static final String RULE_NAME = "rewardsPointsRule";
    
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

                private int getApplicantAnnualincome(RuleContext ctx) {
                    Object value = ctx.getValue("applicant.annualIncome");
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

                private String getApplicantEmploymentstatus(RuleContext ctx) {
                    Object value = ctx.getValue("applicant.employmentStatus");
                    if (value == null) return null;
                            return value.toString();
                }

                private int getApplicantEmploymentyears(RuleContext ctx) {
                    Object value = ctx.getValue("applicant.employmentYears");
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
                    if (getApplicantCreditscore(ctx) >= 700 and getApplicantAnnualincome(ctx) >= 60000) {
                        return RuleResult.action("approveApplication");
                    }

                    if (Objects.equals(getApplicantEmploymentstatus(ctx), "employed" and getApplicantEmploymentyears(ctx) >= 3)) {
                        return RuleResult.action("approveApplication");
                    }

                    if (getApplicantCreditscore(ctx) < 680) {
                        return RuleResult.action("rejectApplication");
                    }

                    return RuleResult.noMatch();
                }

                @Override
                public String getRuleName() {
                    return "rewardsPointsRule";
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