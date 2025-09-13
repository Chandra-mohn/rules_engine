package com.rules.generated.age;

import com.rules.context.RuleContext;
import com.rules.runtime.RuleResult;

/**
 * Generated rule library for: ageVerification
 * Rule ID: Test_Rule_2Rule
 * 
 * Original rule content:
 * rule ageVerification:
    if applicant.age >= 18 then approveApplication
    if applicant.age < 18 then rejectApplication
 * 
 * This is a library JAR - designed to be loaded into a main rules engine service.
 * Not intended as a standalone microservice.
 */
public class Test_Rule_2Rule {
    
    private static final String RULE_ID = "test_rule_2rule";
    private static final String RULE_NAME = "ageVerification";
    
    /**
     * Execute this rule with the provided context.
     * 
     * @param context Rule execution context
     * @return Rule execution result
     */
    public static RuleResult execute(RuleContext context) {
        try {
            package com.rules.generated;

            import com.rules.engine.Rule;
            import com.rules.engine.RuleResult;
            import com.rules.context.RuleContext;
            import java.util.Objects;

            /**
             * Generated rule class for: ageVerification
             * Auto-generated - do not modify manually
             */
            public class AgeVerificationRule implements Rule {

                @Override
                public RuleResult execute(RuleContext ctx) {
                    // Utility method for value comparison
                    java.util.function.BiFunction<Object, Object, Integer> compareValues = (a, b) -> {
                        if (a == null && b == null) return 0;
                        if (a == null) return -1;
                        if (b == null) return 1;
                        if (a instanceof Number && b instanceof Number) {
                            double da = ((Number) a).doubleValue();
                            double db = ((Number) b).doubleValue();
                            return Double.compare(da, db);
                        }
                        return a.toString().compareTo(b.toString());
                    };

                    if (compareValues.apply(ctx.getValue("applicant.age"), 18) >= 0) {
                        return RuleResult.action("approveApplication");
                    }

                    if (compareValues.apply(ctx.getValue("applicant.age"), 18) < 0) {
                        return RuleResult.action("rejectApplication");
                    }

                    return RuleResult.noMatch();
                }

                @Override
                public String getRuleName() {
                    return "ageVerification";
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