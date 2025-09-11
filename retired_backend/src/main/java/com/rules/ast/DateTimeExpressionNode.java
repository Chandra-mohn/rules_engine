package com.rules.ast;

import java.util.Objects;

/**
 * AST node representing date/time expressions with arithmetic operations.
 * Supports operations like: date + duration, date - duration, etc.
 */
public class DateTimeExpressionNode extends ASTNode {
    
    private final ASTNode dateTime;
    private final String operator; // "+", "-"
    private final ASTNode duration;
    
    public DateTimeExpressionNode(ASTNode dateTime, String operator, ASTNode duration) {
        this.dateTime = Objects.requireNonNull(dateTime, "DateTime cannot be null");
        this.operator = operator;
        this.duration = duration;
    }
    
    public DateTimeExpressionNode(ASTNode dateTime) {
        this.dateTime = Objects.requireNonNull(dateTime, "DateTime cannot be null");
        this.operator = null;
        this.duration = null;
    }
    
    public ASTNode getDateTime() {
        return dateTime;
    }
    
    public String getOperator() {
        return operator;
    }
    
    public ASTNode getDuration() {
        return duration;
    }
    
    public boolean hasOperation() {
        return operator != null && duration != null;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitDateTimeExpression(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.DATETIME_EXPRESSION;
    }
    
    @Override
    public String toString() {
        if (hasOperation()) {
            return String.format("DateTimeExpression[%s %s %s]", dateTime, operator, duration);
        } else {
            return String.format("DateTimeExpression[%s]", dateTime);
        }
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        DateTimeExpressionNode that = (DateTimeExpressionNode) obj;
        return Objects.equals(dateTime, that.dateTime) &&
               Objects.equals(operator, that.operator) &&
               Objects.equals(duration, that.duration);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(dateTime, operator, duration);
    }
}