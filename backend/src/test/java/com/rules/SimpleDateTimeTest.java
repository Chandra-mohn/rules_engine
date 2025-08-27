package com.rules;

import com.rules.actions.ActionRegistry;
import com.rules.actions.application.*;
import com.rules.context.RuleContext;
import com.rules.engine.RulesEngine;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Simple test for basic date/time functionality that works.
 */
public class SimpleDateTimeTest {
    
    private RulesEngine rulesEngine;
    private ActionRegistry actionRegistry;
    
    @BeforeEach
    public void setUp() throws Exception {
        actionRegistry = new ActionRegistry();
        actionRegistry.registerAction("approveApplication", new ApproveApplicationAction());
        actionRegistry.registerAction("rejectApplication", new RejectApplicationAction());
        
        rulesEngine = new RulesEngine(actionRegistry);
    }
    
    @Test
    public void testBasicDateComparison() throws Exception {
        System.out.println("Testing basic date comparison...");
        
        String rulesDSL = "rule dateTest:\n" +
            "    if applicant.birthDate before \"2000-01-01\" then approveApplication";
        
        rulesEngine.loadRules(rulesDSL);
        
        String jsonData = "{\n" +
            "    \"applicant\": {\n" +
            "        \"birthDate\": \"1995-06-15\"\n" +
            "    }\n" +
            "}";
        
        RuleContext context = new RuleContext(jsonData);
        RulesEngine.ExecutionSummary summary = rulesEngine.executeRulesWithActions(context);
        
        System.out.println("Result: " + summary);
        assertEquals(1, summary.getRulesExecuted());
    }
}