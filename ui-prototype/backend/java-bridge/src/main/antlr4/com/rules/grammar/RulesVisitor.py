# Generated from java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .RulesParser import RulesParser
else:
    from RulesParser import RulesParser

# This class defines a complete generic visitor for a parse tree produced by RulesParser.

class RulesVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by RulesParser#ruleSet.
    def visitRuleSet(self, ctx:RulesParser.RuleSetContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#rule.
    def visitRule(self, ctx:RulesParser.RuleContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#ruleName.
    def visitRuleName(self, ctx:RulesParser.RuleNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#ruleStep.
    def visitRuleStep(self, ctx:RulesParser.RuleStepContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#actionList.
    def visitActionList(self, ctx:RulesParser.ActionListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#condition.
    def visitCondition(self, ctx:RulesParser.ConditionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#orExpression.
    def visitOrExpression(self, ctx:RulesParser.OrExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#andExpression.
    def visitAndExpression(self, ctx:RulesParser.AndExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#notExpression.
    def visitNotExpression(self, ctx:RulesParser.NotExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#primaryExpression.
    def visitPrimaryExpression(self, ctx:RulesParser.PrimaryExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#comparison.
    def visitComparison(self, ctx:RulesParser.ComparisonContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#expression.
    def visitExpression(self, ctx:RulesParser.ExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#term.
    def visitTerm(self, ctx:RulesParser.TermContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#factor.
    def visitFactor(self, ctx:RulesParser.FactorContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#atom.
    def visitAtom(self, ctx:RulesParser.AtomContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#attribute.
    def visitAttribute(self, ctx:RulesParser.AttributeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#attributeIdentifier.
    def visitAttributeIdentifier(self, ctx:RulesParser.AttributeIdentifierContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#functionCall.
    def visitFunctionCall(self, ctx:RulesParser.FunctionCallContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#functionArgs.
    def visitFunctionArgs(self, ctx:RulesParser.FunctionArgsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#operator.
    def visitOperator(self, ctx:RulesParser.OperatorContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#value.
    def visitValue(self, ctx:RulesParser.ValueContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#list.
    def visitList(self, ctx:RulesParser.ListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#action.
    def visitAction(self, ctx:RulesParser.ActionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#parameterList.
    def visitParameterList(self, ctx:RulesParser.ParameterListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by RulesParser#parameter.
    def visitParameter(self, ctx:RulesParser.ParameterContext):
        return self.visitChildren(ctx)



del RulesParser