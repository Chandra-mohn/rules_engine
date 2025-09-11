package com.rules.ast;

/**
 * AST node representing an action to be executed.
 */
public class ActionNode extends ASTNode {
    private final String actionName;
    
    public ActionNode(String actionName) {
        this.actionName = actionName;
    }
    
    /**
     * Get the action name.
     * 
     * @return Action name
     */
    public String getActionName() {
        return actionName;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitAction(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.ACTION;
    }
    
    @Override
    public String toString() {
        return "Action[" + actionName + "]";
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        ActionNode that = (ActionNode) obj;
        return actionName.equals(that.actionName);
    }
    
    @Override
    public int hashCode() {
        return actionName.hashCode();
    }
}