package com.rules.ast;

import java.util.List;
import java.util.ArrayList;

/**
 * AST node representing an AND expression (logical conjunction).
 */
public class AndExpressionNode extends ASTNode {
    private final List<ASTNode> operands;
    
    public AndExpressionNode() {
        this.operands = new ArrayList<>();
    }
    
    public AndExpressionNode(List<ASTNode> operands) {
        this.operands = new ArrayList<>(operands);
    }
    
    /**
     * Add an operand to this AND expression.
     * 
     * @param operand The operand to add
     */
    public void addOperand(ASTNode operand) {
        operands.add(operand);
    }
    
    /**
     * Get all operands in this AND expression.
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
     * Check if this is a simple AND expression (exactly 2 operands).
     * 
     * @return true if simple AND expression
     */
    public boolean isSimple() {
        return operands.size() == 2;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitAndExpression(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.AND_EXPRESSION;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("AndExpression[");
        sb.append("operands=").append(operands.size());
        sb.append("]");
        return sb.toString();
    }
}