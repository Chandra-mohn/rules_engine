package com.rules.ast;

/**
 * AST node representing a comparison operation.
 */
public class ComparisonNode extends ASTNode {
    private final ASTNode leftExpression;
    private final ComparisonOperator operator;
    private final ASTNode rightExpression;
    
    public ComparisonNode(ASTNode leftExpression, ComparisonOperator operator, ASTNode rightExpression) {
        this.leftExpression = leftExpression;
        this.operator = operator;
        this.rightExpression = rightExpression;
    }
    
    /**
     * Get the left-hand side expression.
     * 
     * @return Left expression
     */
    public ASTNode getLeftExpression() {
        return leftExpression;
    }
    
    /**
     * Get the comparison operator.
     * 
     * @return Comparison operator
     */
    public ComparisonOperator getOperator() {
        return operator;
    }
    
    /**
     * Get the right-hand side expression.
     * 
     * @return Right expression
     */
    public ASTNode getRightExpression() {
        return rightExpression;
    }
    
    /**
     * Get the left-hand side attribute (for backward compatibility).
     * 
     * @return Attribute node if left side is an attribute
     * @deprecated Use getLeftExpression() instead
     */
    @Deprecated
    public AttributeNode getAttribute() {
        if (leftExpression instanceof AttributeNode) {
            return (AttributeNode) leftExpression;
        }
        return null;
    }
    
    /**
     * Get the right-hand side operand (for backward compatibility).
     * 
     * @return Right operand
     * @deprecated Use getRightExpression() instead
     */
    @Deprecated
    public ASTNode getOperand() {
        return rightExpression;
    }
    
    /**
     * Check if the right expression is an attribute.
     * 
     * @return true if right expression is an attribute
     */
    public boolean isAttributeComparison() {
        return rightExpression instanceof AttributeNode;
    }
    
    /**
     * Check if the right expression is a value.
     * 
     * @return true if right expression is a value
     */
    public boolean isValueComparison() {
        return rightExpression instanceof ValueNode;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitComparison(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.COMPARISON;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Comparison[");
        sb.append("left=").append(leftExpression);
        sb.append(", operator=").append(operator);
        sb.append(", right=").append(rightExpression);
        sb.append("]");
        return sb.toString();
    }
    
    /**
     * Enumeration of comparison operators.
     */
    public enum ComparisonOperator {
        EQUALS("="),
        NOT_EQUALS("!="),
        LESS_THAN("<"),
        LESS_THAN_OR_EQUAL("<="),
        GREATER_THAN(">"),
        GREATER_THAN_OR_EQUAL(">="),
        CONTAINS("contains"),
        STARTS_WITH("starts_with"),
        ENDS_WITH("ends_with"),
        IN("in"),
        NOT_IN("not_in"),
        IS_NULL("is_null"),
        IS_NOT_NULL("is_not_null"),
        MATCHES("matches"),
        // Date/Time operators
        BEFORE("before"),
        AFTER("after"),
        BETWEEN("between"),
        WITHIN("within"),
        IS_WEEKEND("is_weekend"),
        IS_WEEKDAY("is_weekday"),
        IS_HOLIDAY("is_holiday"),
        AGE_YEARS("age_years"),
        AGE_MONTHS("age_months"),
        AGE_DAYS("age_days");
        
        private final String symbol;
        
        ComparisonOperator(String symbol) {
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
         * @return Comparison operator
         * @throws IllegalArgumentException if operator not found
         */
        public static ComparisonOperator fromString(String text) {
            for (ComparisonOperator op : values()) {
                if (op.symbol.equalsIgnoreCase(text) || 
                    (op == EQUALS && "==".equals(text)) ||
                    (op == NOT_EQUALS && "<>".equals(text))) {
                    return op;
                }
            }
            throw new IllegalArgumentException("Unknown operator: " + text);
        }
    }
}