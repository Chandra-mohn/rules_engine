package com.rules.ast;

/**
 * Visitor interface for traversing and processing AST nodes.
 * Implements the Visitor pattern for type-safe AST processing.
 * 
 * @param <T> Return type of visitor methods
 */
public interface ASTVisitor<T> {
    
    /**
     * Visit a rule set node.
     * 
     * @param node The rule set node
     * @return Processing result
     */
    T visitRuleSet(RuleSetNode node);
    
    /**
     * Visit a named rule node.
     * 
     * @param node The named rule node
     * @return Processing result
     */
    T visitNamedRule(NamedRuleNode node);
    
    /**
     * Visit a step node.
     * 
     * @param node The step node
     * @return Processing result
     */
    T visitStep(StepNode node);
    
    /**
     * Visit an OR expression node.
     * 
     * @param node The OR expression node
     * @return Processing result
     */
    T visitOrExpression(OrExpressionNode node);
    
    /**
     * Visit an AND expression node.
     * 
     * @param node The AND expression node
     * @return Processing result
     */
    T visitAndExpression(AndExpressionNode node);
    
    /**
     * Visit a NOT expression node.
     * 
     * @param node The NOT expression node
     * @return Processing result
     */
    T visitNotExpression(NotExpressionNode node);
    
    /**
     * Visit a comparison node.
     * 
     * @param node The comparison node
     * @return Processing result
     */
    T visitComparison(ComparisonNode node);
    
    /**
     * Visit an attribute node.
     * 
     * @param node The attribute node
     * @return Processing result
     */
    T visitAttribute(AttributeNode node);
    
    /**
     * Visit a value node.
     * 
     * @param node The value node
     * @return Processing result
     */
    T visitValue(ValueNode node);
    
    /**
     * Visit an action node.
     * 
     * @param node The action node
     * @return Processing result
     */
    T visitAction(ActionNode node);
    
    /**
     * Visit an arithmetic expression node.
     * 
     * @param node The arithmetic expression node
     * @return Processing result
     */
    T visitArithmeticExpression(ArithmeticExpressionNode node);
    
    /**
     * Visit a multiplicative expression node.
     * 
     * @param node The multiplicative expression node
     * @return Processing result
     */
    T visitMultiplicativeExpression(MultiplicativeExpressionNode node);
    
    /**
     * Visit a date/time expression node.
     * 
     * @param node The date/time expression node
     * @return Processing result
     */
    T visitDateTimeExpression(DateTimeExpressionNode node);
    
    /**
     * Visit a date/time literal node.
     * 
     * @param node The date/time literal node
     * @return Processing result
     */
    T visitDateTimeLiteral(DateTimeLiteralNode node);
    
    /**
     * Visit a date/time function node.
     * 
     * @param node The date/time function node
     * @return Processing result
     */
    T visitDateTimeFunction(DateTimeFunctionNode node);
    
    /**
     * Visit a duration node.
     * 
     * @param node The duration node
     * @return Processing result
     */
    T visitDuration(DurationNode node);
}