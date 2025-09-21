# Generated from ../java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .RulesParser import RulesParser
else:
    from RulesParser import RulesParser

# This class defines a complete listener for a parse tree produced by RulesParser.
class RulesListener(ParseTreeListener):

    # Enter a parse tree produced by RulesParser#ruleSet.
    def enterRuleSet(self, ctx:RulesParser.RuleSetContext):
        pass

    # Exit a parse tree produced by RulesParser#ruleSet.
    def exitRuleSet(self, ctx:RulesParser.RuleSetContext):
        pass


    # Enter a parse tree produced by RulesParser#definition.
    def enterDefinition(self, ctx:RulesParser.DefinitionContext):
        pass

    # Exit a parse tree produced by RulesParser#definition.
    def exitDefinition(self, ctx:RulesParser.DefinitionContext):
        pass


    # Enter a parse tree produced by RulesParser#unifiedRule.
    def enterUnifiedRule(self, ctx:RulesParser.UnifiedRuleContext):
        pass

    # Exit a parse tree produced by RulesParser#unifiedRule.
    def exitUnifiedRule(self, ctx:RulesParser.UnifiedRuleContext):
        pass


    # Enter a parse tree produced by RulesParser#ruleName.
    def enterRuleName(self, ctx:RulesParser.RuleNameContext):
        pass

    # Exit a parse tree produced by RulesParser#ruleName.
    def exitRuleName(self, ctx:RulesParser.RuleNameContext):
        pass


    # Enter a parse tree produced by RulesParser#ruleStep.
    def enterRuleStep(self, ctx:RulesParser.RuleStepContext):
        pass

    # Exit a parse tree produced by RulesParser#ruleStep.
    def exitRuleStep(self, ctx:RulesParser.RuleStepContext):
        pass


    # Enter a parse tree produced by RulesParser#actionList.
    def enterActionList(self, ctx:RulesParser.ActionListContext):
        pass

    # Exit a parse tree produced by RulesParser#actionList.
    def exitActionList(self, ctx:RulesParser.ActionListContext):
        pass


    # Enter a parse tree produced by RulesParser#condition.
    def enterCondition(self, ctx:RulesParser.ConditionContext):
        pass

    # Exit a parse tree produced by RulesParser#condition.
    def exitCondition(self, ctx:RulesParser.ConditionContext):
        pass


    # Enter a parse tree produced by RulesParser#orExpression.
    def enterOrExpression(self, ctx:RulesParser.OrExpressionContext):
        pass

    # Exit a parse tree produced by RulesParser#orExpression.
    def exitOrExpression(self, ctx:RulesParser.OrExpressionContext):
        pass


    # Enter a parse tree produced by RulesParser#andExpression.
    def enterAndExpression(self, ctx:RulesParser.AndExpressionContext):
        pass

    # Exit a parse tree produced by RulesParser#andExpression.
    def exitAndExpression(self, ctx:RulesParser.AndExpressionContext):
        pass


    # Enter a parse tree produced by RulesParser#notExpression.
    def enterNotExpression(self, ctx:RulesParser.NotExpressionContext):
        pass

    # Exit a parse tree produced by RulesParser#notExpression.
    def exitNotExpression(self, ctx:RulesParser.NotExpressionContext):
        pass


    # Enter a parse tree produced by RulesParser#primaryExpression.
    def enterPrimaryExpression(self, ctx:RulesParser.PrimaryExpressionContext):
        pass

    # Exit a parse tree produced by RulesParser#primaryExpression.
    def exitPrimaryExpression(self, ctx:RulesParser.PrimaryExpressionContext):
        pass


    # Enter a parse tree produced by RulesParser#comparison.
    def enterComparison(self, ctx:RulesParser.ComparisonContext):
        pass

    # Exit a parse tree produced by RulesParser#comparison.
    def exitComparison(self, ctx:RulesParser.ComparisonContext):
        pass


    # Enter a parse tree produced by RulesParser#operand.
    def enterOperand(self, ctx:RulesParser.OperandContext):
        pass

    # Exit a parse tree produced by RulesParser#operand.
    def exitOperand(self, ctx:RulesParser.OperandContext):
        pass


    # Enter a parse tree produced by RulesParser#attribute.
    def enterAttribute(self, ctx:RulesParser.AttributeContext):
        pass

    # Exit a parse tree produced by RulesParser#attribute.
    def exitAttribute(self, ctx:RulesParser.AttributeContext):
        pass


    # Enter a parse tree produced by RulesParser#attributeIdentifier.
    def enterAttributeIdentifier(self, ctx:RulesParser.AttributeIdentifierContext):
        pass

    # Exit a parse tree produced by RulesParser#attributeIdentifier.
    def exitAttributeIdentifier(self, ctx:RulesParser.AttributeIdentifierContext):
        pass


    # Enter a parse tree produced by RulesParser#operator.
    def enterOperator(self, ctx:RulesParser.OperatorContext):
        pass

    # Exit a parse tree produced by RulesParser#operator.
    def exitOperator(self, ctx:RulesParser.OperatorContext):
        pass


    # Enter a parse tree produced by RulesParser#value.
    def enterValue(self, ctx:RulesParser.ValueContext):
        pass

    # Exit a parse tree produced by RulesParser#value.
    def exitValue(self, ctx:RulesParser.ValueContext):
        pass


    # Enter a parse tree produced by RulesParser#list.
    def enterList(self, ctx:RulesParser.ListContext):
        pass

    # Exit a parse tree produced by RulesParser#list.
    def exitList(self, ctx:RulesParser.ListContext):
        pass


    # Enter a parse tree produced by RulesParser#action.
    def enterAction(self, ctx:RulesParser.ActionContext):
        pass

    # Exit a parse tree produced by RulesParser#action.
    def exitAction(self, ctx:RulesParser.ActionContext):
        pass


    # Enter a parse tree produced by RulesParser#parameterList.
    def enterParameterList(self, ctx:RulesParser.ParameterListContext):
        pass

    # Exit a parse tree produced by RulesParser#parameterList.
    def exitParameterList(self, ctx:RulesParser.ParameterListContext):
        pass


    # Enter a parse tree produced by RulesParser#parameter.
    def enterParameter(self, ctx:RulesParser.ParameterContext):
        pass

    # Exit a parse tree produced by RulesParser#parameter.
    def exitParameter(self, ctx:RulesParser.ParameterContext):
        pass



del RulesParser