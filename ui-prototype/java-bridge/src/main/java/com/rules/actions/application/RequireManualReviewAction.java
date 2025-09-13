package com.rules.actions.application;

import com.rules.actions.Action;
import com.rules.context.RuleContext;

/**
 * Alias for ManualReviewAction - routes applications to manual review.
 */
public class RequireManualReviewAction implements Action {
    
    private final ManualReviewAction manualReviewAction;
    
    public RequireManualReviewAction() {
        this.manualReviewAction = new ManualReviewAction();
    }
    
    @Override
    public void execute(RuleContext context) {
        // Delegate to ManualReviewAction
        manualReviewAction.execute(context);
    }
    
    @Override
    public String getName() {
        return "requireManualReview";
    }
}