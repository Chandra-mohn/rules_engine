package com.rules.ast;

/**
 * AST node representing a NOT expression (logical negation).
 */
public class NotExpressionNode extends ASTNode {
    private final ASTNode operand;
    private final boolean negated;
    
    public NotExpressionNode(ASTNode operand, boolean negated) {
        this.operand = operand;
        this.negated = negated;
    }
    
    /**
     * Get the operand being negated.
     * 
     * @return The operand
     */
    public ASTNode getOperand() {
        return operand;
    }
    
    /**
     * Check if this expression is negated.
     * 
     * @return true if negated (NOT applied)
     */
    public boolean isNegated() {
        return negated;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitNotExpression(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.NOT_EXPRESSION;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("NotExpression[");
        sb.append("negated=").append(negated);
        sb.append(", operand=").append(operand);
        sb.append("]");
        return sb.toString();
    }
}