// Generated from /Users/chandramohn/workspace/rules/backend/src/main/antlr4/com/rules/grammar/Rules.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link RulesParser}.
 */
public interface RulesListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link RulesParser#ruleSet}.
	 * @param ctx the parse tree
	 */
	void enterRuleSet(RulesParser.RuleSetContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#ruleSet}.
	 * @param ctx the parse tree
	 */
	void exitRuleSet(RulesParser.RuleSetContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#namedRule}.
	 * @param ctx the parse tree
	 */
	void enterNamedRule(RulesParser.NamedRuleContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#namedRule}.
	 * @param ctx the parse tree
	 */
	void exitNamedRule(RulesParser.NamedRuleContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#ruleName}.
	 * @param ctx the parse tree
	 */
	void enterRuleName(RulesParser.RuleNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#ruleName}.
	 * @param ctx the parse tree
	 */
	void exitRuleName(RulesParser.RuleNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#step}.
	 * @param ctx the parse tree
	 */
	void enterStep(RulesParser.StepContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#step}.
	 * @param ctx the parse tree
	 */
	void exitStep(RulesParser.StepContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#condition}.
	 * @param ctx the parse tree
	 */
	void enterCondition(RulesParser.ConditionContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#condition}.
	 * @param ctx the parse tree
	 */
	void exitCondition(RulesParser.ConditionContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#orExpression}.
	 * @param ctx the parse tree
	 */
	void enterOrExpression(RulesParser.OrExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#orExpression}.
	 * @param ctx the parse tree
	 */
	void exitOrExpression(RulesParser.OrExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#andExpression}.
	 * @param ctx the parse tree
	 */
	void enterAndExpression(RulesParser.AndExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#andExpression}.
	 * @param ctx the parse tree
	 */
	void exitAndExpression(RulesParser.AndExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#notExpression}.
	 * @param ctx the parse tree
	 */
	void enterNotExpression(RulesParser.NotExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#notExpression}.
	 * @param ctx the parse tree
	 */
	void exitNotExpression(RulesParser.NotExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#primaryExpression}.
	 * @param ctx the parse tree
	 */
	void enterPrimaryExpression(RulesParser.PrimaryExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#primaryExpression}.
	 * @param ctx the parse tree
	 */
	void exitPrimaryExpression(RulesParser.PrimaryExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#comparison}.
	 * @param ctx the parse tree
	 */
	void enterComparison(RulesParser.ComparisonContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#comparison}.
	 * @param ctx the parse tree
	 */
	void exitComparison(RulesParser.ComparisonContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterExpression(RulesParser.ExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitExpression(RulesParser.ExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#multiplicativeExpression}.
	 * @param ctx the parse tree
	 */
	void enterMultiplicativeExpression(RulesParser.MultiplicativeExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#multiplicativeExpression}.
	 * @param ctx the parse tree
	 */
	void exitMultiplicativeExpression(RulesParser.MultiplicativeExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#atomicExpression}.
	 * @param ctx the parse tree
	 */
	void enterAtomicExpression(RulesParser.AtomicExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#atomicExpression}.
	 * @param ctx the parse tree
	 */
	void exitAtomicExpression(RulesParser.AtomicExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#attribute}.
	 * @param ctx the parse tree
	 */
	void enterAttribute(RulesParser.AttributeContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#attribute}.
	 * @param ctx the parse tree
	 */
	void exitAttribute(RulesParser.AttributeContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#operator}.
	 * @param ctx the parse tree
	 */
	void enterOperator(RulesParser.OperatorContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#operator}.
	 * @param ctx the parse tree
	 */
	void exitOperator(RulesParser.OperatorContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#value}.
	 * @param ctx the parse tree
	 */
	void enterValue(RulesParser.ValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#value}.
	 * @param ctx the parse tree
	 */
	void exitValue(RulesParser.ValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#list}.
	 * @param ctx the parse tree
	 */
	void enterList(RulesParser.ListContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#list}.
	 * @param ctx the parse tree
	 */
	void exitList(RulesParser.ListContext ctx);
	/**
	 * Enter a parse tree produced by {@link RulesParser#action}.
	 * @param ctx the parse tree
	 */
	void enterAction(RulesParser.ActionContext ctx);
	/**
	 * Exit a parse tree produced by {@link RulesParser#action}.
	 * @param ctx the parse tree
	 */
	void exitAction(RulesParser.ActionContext ctx);
}