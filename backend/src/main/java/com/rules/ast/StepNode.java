package com.rules.ast;

/**
 * AST node representing a rule step (if-then-else construct).
 */
public class StepNode extends ASTNode {
    private final ASTNode condition;
    private final ActionNode thenAction;
    private final ActionNode elseAction; // Optional
    
    public StepNode(ASTNode condition, ActionNode thenAction) {
        this(condition, thenAction, null);
    }
    
    public StepNode(ASTNode condition, ActionNode thenAction, ActionNode elseAction) {
        this.condition = condition;
        this.thenAction = thenAction;
        this.elseAction = elseAction;
    }
    
    /**
     * Get the condition expression for this step.
     * 
     * @return Condition AST node
     */
    public ASTNode getCondition() {
        return condition;
    }
    
    /**
     * Get the action to execute when condition is true.
     * 
     * @return Then action node
     */
    public ActionNode getThenAction() {
        return thenAction;
    }
    
    /**
     * Get the action to execute when condition is false.
     * 
     * @return Else action node, or null if not specified
     */
    public ActionNode getElseAction() {
        return elseAction;
    }
    
    /**
     * Check if this step has an else clause.
     * 
     * @return true if else action is present
     */
    public boolean hasElseAction() {
        return elseAction != null;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitStep(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.STEP;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Step[");
        sb.append("condition=").append(condition);
        sb.append(", then=").append(thenAction);
        if (elseAction != null) {
            sb.append(", else=").append(elseAction);
        }
        sb.append("]");
        return sb.toString();
    }
}