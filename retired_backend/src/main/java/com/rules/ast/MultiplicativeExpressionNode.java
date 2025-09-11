package com.rules.ast;

import java.util.List;
import java.util.ArrayList;

/**
 * AST node representing a multiplicative expression (multiplication/division/modulo).
 */
public class MultiplicativeExpressionNode extends ASTNode {
    private final List<ASTNode> operands;
    private final List<ArithmeticExpressionNode.ArithmeticOperator> operators;
    
    public MultiplicativeExpressionNode() {
        this.operands = new ArrayList<>();
        this.operators = new ArrayList<>();
    }
    
    public MultiplicativeExpressionNode(ASTNode firstOperand) {
        this();
        this.operands.add(firstOperand);
    }
    
    /**
     * Add an operand with operator.
     * 
     * @param operator The arithmetic operator
     * @param operand The operand
     */
    public void addOperand(ArithmeticExpressionNode.ArithmeticOperator operator, ASTNode operand) {
        operators.add(operator);
        operands.add(operand);
    }
    
    /**
     * Get all operands.
     * 
     * @return List of operands
     */
    public List<ASTNode> getOperands() {
        return new ArrayList<>(operands);
    }
    
    /**
     * Get all operators.
     * 
     * @return List of operators
     */
    public List<ArithmeticExpressionNode.ArithmeticOperator> getOperators() {
        return new ArrayList<>(operators);
    }
    
    /**
     * Check if this is a simple expression (single operand).
     * 
     * @return true if simple expression
     */
    public boolean isSimple() {
        return operands.size() == 1;
    }
    
    /**
     * Get the single operand if this is a simple expression.
     * 
     * @return Single operand
     * @throws IllegalStateException if not a simple expression
     */
    public ASTNode getSimpleOperand() {
        if (!isSimple()) {
            throw new IllegalStateException("Not a simple expression");
        }
        return operands.get(0);
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitMultiplicativeExpression(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.MULTIPLICATIVE_EXPRESSION;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("MultiplicativeExpression[");
        sb.append("operands=").append(operands.size());
        sb.append(", operators=").append(operators.size());
        sb.append("]");
        return sb.toString();
    }
}