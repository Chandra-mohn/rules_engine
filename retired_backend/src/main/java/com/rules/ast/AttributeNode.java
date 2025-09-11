package com.rules.ast;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * AST node representing an attribute reference (e.g., customer.age, transaction.amount).
 */
public class AttributeNode extends ASTNode {
    private final List<String> path;
    
    public AttributeNode(String... pathElements) {
        this.path = new ArrayList<>(Arrays.asList(pathElements));
    }
    
    public AttributeNode(List<String> path) {
        this.path = new ArrayList<>(path);
    }
    
    /**
     * Get the attribute path as a list of elements.
     * 
     * @return List of path elements
     */
    public List<String> getPath() {
        return new ArrayList<>(path);
    }
    
    /**
     * Get the attribute path as a dot-separated string.
     * 
     * @return Dot-separated path (e.g., "customer.age")
     */
    public String getPathString() {
        return String.join(".", path);
    }
    
    /**
     * Get the root element of the path.
     * 
     * @return Root element (e.g., "customer" from "customer.age")
     */
    public String getRoot() {
        return path.isEmpty() ? null : path.get(0);
    }
    
    /**
     * Get the leaf element of the path.
     * 
     * @return Leaf element (e.g., "age" from "customer.age")
     */
    public String getLeaf() {
        return path.isEmpty() ? null : path.get(path.size() - 1);
    }
    
    /**
     * Get the depth of the attribute path.
     * 
     * @return Path depth (number of elements)
     */
    public int getDepth() {
        return path.size();
    }
    
    /**
     * Check if this is a simple attribute (single element).
     * 
     * @return true if simple attribute
     */
    public boolean isSimple() {
        return path.size() == 1;
    }
    
    /**
     * Check if this is a nested attribute (multiple elements).
     * 
     * @return true if nested attribute
     */
    public boolean isNested() {
        return path.size() > 1;
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitAttribute(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.ATTRIBUTE;
    }
    
    @Override
    public String toString() {
        return "Attribute[" + getPathString() + "]";
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        AttributeNode that = (AttributeNode) obj;
        return path.equals(that.path);
    }
    
    @Override
    public int hashCode() {
        return path.hashCode();
    }
}