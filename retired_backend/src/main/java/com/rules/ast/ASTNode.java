package com.rules.ast;

/**
 * Base class for all AST nodes in the rules engine.
 * Provides common functionality for tree traversal and code generation.
 */
public abstract class ASTNode {
    
    /**
     * Accept a visitor for traversal and processing.
     * 
     * @param visitor The visitor to accept
     * @param <T> Return type of the visitor
     * @return Result of visitor processing
     */
    public abstract <T> T accept(ASTVisitor<T> visitor);
    
    /**
     * Get a string representation of this node for debugging.
     * 
     * @return String representation
     */
    @Override
    public abstract String toString();
    
    /**
     * Get the type of this AST node.
     * 
     * @return Node type
     */
    public abstract NodeType getNodeType();
    
    /**
     * Enumeration of AST node types.
     */
    public enum NodeType {
        RULE_SET,
        NAMED_RULE,
        STEP,
        OR_EXPRESSION,
        AND_EXPRESSION,
        NOT_EXPRESSION,
        COMPARISON,
        ARITHMETIC_EXPRESSION,
        MULTIPLICATIVE_EXPRESSION,
        ATTRIBUTE,
        VALUE,
        ACTION,
        DATETIME_EXPRESSION,
        DATETIME_LITERAL,
        DATETIME_FUNCTION,
        DURATION
    }
}