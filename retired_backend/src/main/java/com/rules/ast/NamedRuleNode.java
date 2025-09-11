package com.rules.ast;

import java.util.List;
import java.util.ArrayList;

/**
 * AST node representing a named rule with multiple steps.
 */
public class NamedRuleNode extends ASTNode {
    private final String ruleName;
    private final List<StepNode> steps;
    
    public NamedRuleNode(String ruleName) {
        this.ruleName = ruleName;
        this.steps = new ArrayList<>();
    }
    
    public NamedRuleNode(String ruleName, List<StepNode> steps) {
        this.ruleName = ruleName;
        this.steps = new ArrayList<>(steps);
    }
    
    /**
     * Get the name of this rule.
     * 
     * @return Rule name
     */
    public String getRuleName() {
        return ruleName;
    }
    
    /**
     * Add a step to this rule.
     * 
     * @param step The step to add
     */
    public void addStep(StepNode step) {
        steps.add(step);
    }
    
    /**
     * Get all steps in this rule.
     * 
     * @return List of steps
     */
    public List<StepNode> getSteps() {
        return new ArrayList<>(steps);
    }
    
    /**
     * Get the number of steps in this rule.
     * 
     * @return Number of steps
     */
    public int getStepCount() {
        return steps.size();
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitNamedRule(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.NAMED_RULE;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("NamedRule[");
        sb.append("name='").append(ruleName).append("'");
        sb.append(", steps=").append(steps.size());
        sb.append("]");
        return sb.toString();
    }
}