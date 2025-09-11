package com.rules.ast;

import java.util.List;
import java.util.ArrayList;

/**
 * AST node representing an OR expression (logical disjunction).
 */
public class OrExpressionNode extends ASTNode {
    private final List<ASTNode> operands;
    
    public OrExpressionNode() {
        this.operands = new ArrayList<>();
    }
    
    public OrExpressionNode(List<ASTNode> operands) {
        this.operands = new ArrayList<>(operands);
    }
    
    /**
     * Add an operand to this OR expression.
     * 
     * @param operand The operand to add
     */
    public void addOperand(ASTNode operand) {
        operands.add(operand);
    }
    
    /**
     * Get all operands in this OR expression.
     * 
     * @return List of operands
     */
    public List<ASTNode> getOperands() {
        return new ArrayList<>(operands);
    }
    
    /**
     * Get the number of operands.
     * 
     * @return Number of operands
     */
    public int getOperandCount() {
        return operands.size();
    }
    
    /**
     * Check if this is a simple OR expression (exactly 2 operands).
     * 
     * @return true if simple OR expression
     */
    public boolean isSimple() {
        return operands.size() == 2;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitOrExpression(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.OR_EXPRESSION;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("OrExpression[");
        sb.append("operands=").append(operands.size());
        sb.append("]");
        return sb.toString();
    }
}