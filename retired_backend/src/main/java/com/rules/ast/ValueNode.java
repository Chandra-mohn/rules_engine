package com.rules.ast;

import java.util.List;
import java.util.ArrayList;

/**
 * AST node representing a literal value (string, number, boolean, null, or list).
 */
public class ValueNode extends ASTNode {
    private final Object value;
    private final ValueType type;
    
    public ValueNode(String value) {
        this.value = value;
        this.type = ValueType.STRING;
    }
    
    public ValueNode(Number value) {
        this.value = value;
        this.type = ValueType.NUMBER;
    }
    
    public ValueNode(Boolean value) {
        this.value = value;
        this.type = ValueType.BOOLEAN;
    }
    
    public ValueNode(List<ValueNode> value) {
        this.value = new ArrayList<>(value);
        this.type = ValueType.LIST;
    }
    
    private ValueNode() {
        this.value = null;
        this.type = ValueType.NULL;
    }
    
    /**
     * Create a null value node.
     * 
     * @return Null value node
     */
    public static ValueNode nullValue() {
        return new ValueNode();
    }
    
    /**
     * Get the value.
     * 
     * @return The value object
     */
    public Object getValue() {
        return value;
    }
    
    /**
     * Get the value type.
     * 
     * @return Value type
     */
    public ValueType getType() {
        return type;
    }
    
    /**
     * Get the value as a string.
     * 
     * @return String value
     * @throws ClassCastException if not a string
     */
    public String getStringValue() {
        if (type != ValueType.STRING) {
            throw new ClassCastException("Value is not a string: " + type);
        }
        return (String) value;
    }
    
    /**
     * Get the value as a number.
     * 
     * @return Number value
     * @throws ClassCastException if not a number
     */
    public Number getNumberValue() {
        if (type != ValueType.NUMBER) {
            throw new ClassCastException("Value is not a number: " + type);
        }
        return (Number) value;
    }
    
    /**
     * Get the value as a boolean.
     * 
     * @return Boolean value
     * @throws ClassCastException if not a boolean
     */
    public Boolean getBooleanValue() {
        if (type != ValueType.BOOLEAN) {
            throw new ClassCastException("Value is not a boolean: " + type);
        }
        return (Boolean) value;
    }
    
    /**
     * Get the value as a list.
     * 
     * @return List value
     * @throws ClassCastException if not a list
     */
    @SuppressWarnings("unchecked")
    public List<ValueNode> getListValue() {
        if (type != ValueType.LIST) {
            throw new ClassCastException("Value is not a list: " + type);
        }
        return new ArrayList<>((List<ValueNode>) value);
    }
    
    /**
     * Check if this is a null value.
     * 
     * @return true if null
     */
    public boolean isNull() {
        return type == ValueType.NULL;
    }
    
    /**
     * Check if this is a string value.
     * 
     * @return true if string
     */
    public boolean isString() {
        return type == ValueType.STRING;
    }
    
    /**
     * Check if this is a number value.
     * 
     * @return true if number
     */
    public boolean isNumber() {
        return type == ValueType.NUMBER;
    }
    
    /**
     * Check if this is a boolean value.
     * 
     * @return true if boolean
     */
    public boolean isBoolean() {
        return type == ValueType.BOOLEAN;
    }
    
    /**
     * Check if this is a list value.
     * 
     * @return true if list
     */
    public boolean isList() {
        return type == ValueType.LIST;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitValue(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.VALUE;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Value[");
        sb.append("type=").append(type);
        sb.append(", value=").append(value);
        sb.append("]");
        return sb.toString();
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        ValueNode that = (ValueNode) obj;
        return type == that.type && 
               (value == null ? that.value == null : value.equals(that.value));
    }
    
    @Override
    public int hashCode() {
        return type.hashCode() * 31 + (value != null ? value.hashCode() : 0);
    }
    
    /**
     * Enumeration of value types.
     */
    public enum ValueType {
        STRING,
        NUMBER,
        BOOLEAN,
        NULL,
        LIST
    }
}