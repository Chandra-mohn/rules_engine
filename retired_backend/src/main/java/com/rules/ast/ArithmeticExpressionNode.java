package com.rules.ast;

import java.util.List;
import java.util.ArrayList;

/**
 * AST node representing an arithmetic expression (addition/subtraction).
 */
public class ArithmeticExpressionNode extends ASTNode {
    private final List<ASTNode> operands;
    private final List<ArithmeticOperator> operators;
    
    public ArithmeticExpressionNode() {
        this.operands = new ArrayList<>();
        this.operators = new ArrayList<>();
    }
    
    public ArithmeticExpressionNode(ASTNode firstOperand) {
        this();
        this.operands.add(firstOperand);
    }
    
    /**
     * Add an operand with operator.
     * 
     * @param operator The arithmetic operator
     * @param operand The operand
     */
    public void addOperand(ArithmeticOperator operator, ASTNode operand) {
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
    public List<ArithmeticOperator> getOperators() {
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
        return visitor.visitArithmeticExpression(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.ARITHMETIC_EXPRESSION;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("ArithmeticExpression[");
        sb.append("operands=").append(operands.size());
        sb.append(", operators=").append(operators.size());
        sb.append("]");
        return sb.toString();
    }
    
    /**
     * Enumeration of arithmetic operators.
     */
    public enum ArithmeticOperator {
        PLUS("+"),
        MINUS("-"),
        MULTIPLY("*"),
        DIVIDE("/"),
        MODULO("%");
        
        private final String symbol;
        
        ArithmeticOperator(String symbol) {
            this.symbol = symbol;
        }
        
        public String getSymbol() {
            return symbol;
        }
        
        @Override
        public String toString() {
            return symbol;
        }
        
        /**
         * Parse operator from string.
         * 
         * @param text Operator text
         * @return Arithmetic operator
         * @throws IllegalArgumentException if operator not found
         */
        public static ArithmeticOperator fromString(String text) {
            for (ArithmeticOperator op : values()) {
                if (op.symbol.equals(text)) {
                    return op;
                }
            }
            throw new IllegalArgumentException("Unknown arithmetic operator: " + text);
        }
    }
}