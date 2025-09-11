package com.rules.ast;

import java.util.Objects;

/**
 * AST node representing date/time literals.
 * Supports various formats: dates, times, datetimes, and special values like NOW, TODAY.
 */
public class DateTimeLiteralNode extends ASTNode {
    
    public enum DateTimeType {
        DATE,           // 2025-08-25
        TIME,           // 14:30:00
        DATETIME,       // 2025-08-25T14:30:00Z
        NOW,            // Current timestamp
        TODAY,          // Current date at midnight
        BUSINESS_DATE   // Current business date (excludes weekends/holidays)
    }
    
    private final String value;
    private final DateTimeType type;
    
    public DateTimeLiteralNode(String value, DateTimeType type) {
        this.value = Objects.requireNonNull(value, "Value cannot be null");
        this.type = Objects.requireNonNull(type, "Type cannot be null");
    }
    
    public String getValue() {
        return value;
    }
    
    public DateTimeType getType() {
        return type;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitDateTimeLiteral(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.DATETIME_LITERAL;
    }
    
    @Override
    public String toString() {
        return String.format("DateTimeLiteral[%s:%s]", type, value);
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        DateTimeLiteralNode that = (DateTimeLiteralNode) obj;
        return Objects.equals(value, that.value) && type == that.type;
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(value, type);
    }
}