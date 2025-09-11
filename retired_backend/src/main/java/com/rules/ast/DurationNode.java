package com.rules.ast;

import java.util.Objects;

/**
 * AST node representing duration values.
 * Supports various units: years, months, days, hours, minutes, seconds, etc.
 */
public class DurationNode extends ASTNode {
    
    public enum DurationUnit {
        YEARS("years"),
        MONTHS("months"),
        WEEKS("weeks"),
        DAYS("days"),
        HOURS("hours"),
        MINUTES("minutes"),
        SECONDS("seconds"),
        MILLIS("millis");
        
        private final String unitName;
        
        DurationUnit(String unitName) {
            this.unitName = unitName;
        }
        
        public String getUnitName() {
            return unitName;
        }
        
        @Override
        public String toString() {
            return unitName;
        }
    }
    
    private final double value;
    private final DurationUnit unit;
    private final String literalValue; // For ISO 8601 duration literals like P1Y2M3D
    
    public DurationNode(double value, DurationUnit unit) {
        this.value = value;
        this.unit = Objects.requireNonNull(unit, "Unit cannot be null");
        this.literalValue = null;
    }
    
    public DurationNode(String literalValue) {
        this.literalValue = Objects.requireNonNull(literalValue, "Literal value cannot be null");
        this.value = 0;
        this.unit = null;
    }
    
    public double getValue() {
        return value;
    }
    
    public DurationUnit getUnit() {
        return unit;
    }
    
    public String getLiteralValue() {
        return literalValue;
    }
    
    public boolean isLiteral() {
        return literalValue != null;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitDuration(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.DURATION;
    }
    
    @Override
    public String toString() {
        if (isLiteral()) {
            return String.format("Duration[%s]", literalValue);
        } else {
            return String.format("Duration[%.2f %s]", value, unit);
        }
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        DurationNode that = (DurationNode) obj;
        return Double.compare(that.value, value) == 0 &&
               unit == that.unit &&
               Objects.equals(literalValue, that.literalValue);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(value, unit, literalValue);
    }
}