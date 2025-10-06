# Generated from Rules.g4 by ANTLR 4.13.2
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO

def serializedATN():
    return [
        4,1,40,226,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
        6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,13,
        2,14,7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,2,19,7,19,2,20,
        7,20,2,21,7,21,2,22,7,22,2,23,7,23,2,24,7,24,1,0,4,0,52,8,0,11,0,
        12,0,53,1,0,1,0,1,1,1,1,1,1,1,1,4,1,62,8,1,11,1,12,1,63,1,2,1,2,
        1,3,1,3,1,3,1,3,1,3,1,3,3,3,74,8,3,1,3,3,3,77,8,3,1,4,1,4,1,4,5,
        4,82,8,4,10,4,12,4,85,9,4,1,5,1,5,1,6,1,6,1,6,5,6,92,8,6,10,6,12,
        6,95,9,6,1,7,1,7,1,7,5,7,100,8,7,10,7,12,7,103,9,7,1,8,3,8,106,8,
        8,1,8,1,8,1,9,1,9,1,9,1,9,1,9,3,9,115,8,9,1,10,1,10,1,10,1,10,1,
        11,1,11,1,11,5,11,124,8,11,10,11,12,11,127,9,11,1,12,1,12,1,12,5,
        12,132,8,12,10,12,12,12,135,9,12,1,13,3,13,138,8,13,1,13,1,13,1,
        14,1,14,1,14,1,14,1,14,1,14,1,14,3,14,149,8,14,1,15,1,15,1,15,5,
        15,154,8,15,10,15,12,15,157,9,15,1,16,1,16,1,17,1,17,1,17,3,17,164,
        8,17,1,17,1,17,1,18,1,18,1,18,5,18,171,8,18,10,18,12,18,174,9,18,
        1,19,1,19,1,20,1,20,1,20,1,20,1,20,3,20,183,8,20,1,21,1,21,1,21,
        1,21,5,21,189,8,21,10,21,12,21,192,9,21,3,21,194,8,21,1,21,1,21,
        1,22,1,22,1,22,3,22,201,8,22,1,22,3,22,204,8,22,1,22,1,22,1,22,3,
        22,209,8,22,1,22,3,22,212,8,22,3,22,214,8,22,1,23,1,23,1,23,5,23,
        219,8,23,10,23,12,23,222,9,23,1,24,1,24,1,24,0,0,25,0,2,4,6,8,10,
        12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,0,4,1,0,
        37,38,1,0,30,31,1,0,32,34,2,0,8,15,17,22,230,0,51,1,0,0,0,2,57,1,
        0,0,0,4,65,1,0,0,0,6,76,1,0,0,0,8,78,1,0,0,0,10,86,1,0,0,0,12,88,
        1,0,0,0,14,96,1,0,0,0,16,105,1,0,0,0,18,114,1,0,0,0,20,116,1,0,0,
        0,22,120,1,0,0,0,24,128,1,0,0,0,26,137,1,0,0,0,28,148,1,0,0,0,30,
        150,1,0,0,0,32,158,1,0,0,0,34,160,1,0,0,0,36,167,1,0,0,0,38,175,
        1,0,0,0,40,182,1,0,0,0,42,184,1,0,0,0,44,213,1,0,0,0,46,215,1,0,
        0,0,48,223,1,0,0,0,50,52,3,2,1,0,51,50,1,0,0,0,52,53,1,0,0,0,53,
        51,1,0,0,0,53,54,1,0,0,0,54,55,1,0,0,0,55,56,5,0,0,1,56,1,1,0,0,
        0,57,58,5,1,0,0,58,59,3,4,2,0,59,61,5,29,0,0,60,62,3,6,3,0,61,60,
        1,0,0,0,62,63,1,0,0,0,63,61,1,0,0,0,63,64,1,0,0,0,64,3,1,0,0,0,65,
        66,7,0,0,0,66,5,1,0,0,0,67,68,5,2,0,0,68,69,3,10,5,0,69,70,5,3,0,
        0,70,73,3,8,4,0,71,72,5,4,0,0,72,74,3,8,4,0,73,71,1,0,0,0,73,74,
        1,0,0,0,74,77,1,0,0,0,75,77,3,8,4,0,76,67,1,0,0,0,76,75,1,0,0,0,
        77,7,1,0,0,0,78,83,3,44,22,0,79,80,5,28,0,0,80,82,3,44,22,0,81,79,
        1,0,0,0,82,85,1,0,0,0,83,81,1,0,0,0,83,84,1,0,0,0,84,9,1,0,0,0,85,
        83,1,0,0,0,86,87,3,12,6,0,87,11,1,0,0,0,88,93,3,14,7,0,89,90,5,6,
        0,0,90,92,3,14,7,0,91,89,1,0,0,0,92,95,1,0,0,0,93,91,1,0,0,0,93,
        94,1,0,0,0,94,13,1,0,0,0,95,93,1,0,0,0,96,101,3,16,8,0,97,98,5,5,
        0,0,98,100,3,16,8,0,99,97,1,0,0,0,100,103,1,0,0,0,101,99,1,0,0,0,
        101,102,1,0,0,0,102,15,1,0,0,0,103,101,1,0,0,0,104,106,5,7,0,0,105,
        104,1,0,0,0,105,106,1,0,0,0,106,107,1,0,0,0,107,108,3,18,9,0,108,
        17,1,0,0,0,109,115,3,20,10,0,110,111,5,23,0,0,111,112,3,12,6,0,112,
        113,5,24,0,0,113,115,1,0,0,0,114,109,1,0,0,0,114,110,1,0,0,0,115,
        19,1,0,0,0,116,117,3,22,11,0,117,118,3,38,19,0,118,119,3,22,11,0,
        119,21,1,0,0,0,120,125,3,24,12,0,121,122,7,1,0,0,122,124,3,24,12,
        0,123,121,1,0,0,0,124,127,1,0,0,0,125,123,1,0,0,0,125,126,1,0,0,
        0,126,23,1,0,0,0,127,125,1,0,0,0,128,133,3,26,13,0,129,130,7,2,0,
        0,130,132,3,26,13,0,131,129,1,0,0,0,132,135,1,0,0,0,133,131,1,0,
        0,0,133,134,1,0,0,0,134,25,1,0,0,0,135,133,1,0,0,0,136,138,5,31,
        0,0,137,136,1,0,0,0,137,138,1,0,0,0,138,139,1,0,0,0,139,140,3,28,
        14,0,140,27,1,0,0,0,141,149,3,30,15,0,142,149,3,40,20,0,143,149,
        3,34,17,0,144,145,5,23,0,0,145,146,3,22,11,0,146,147,5,24,0,0,147,
        149,1,0,0,0,148,141,1,0,0,0,148,142,1,0,0,0,148,143,1,0,0,0,148,
        144,1,0,0,0,149,29,1,0,0,0,150,155,3,32,16,0,151,152,5,27,0,0,152,
        154,3,32,16,0,153,151,1,0,0,0,154,157,1,0,0,0,155,153,1,0,0,0,155,
        156,1,0,0,0,156,31,1,0,0,0,157,155,1,0,0,0,158,159,7,0,0,0,159,33,
        1,0,0,0,160,161,5,38,0,0,161,163,5,23,0,0,162,164,3,36,18,0,163,
        162,1,0,0,0,163,164,1,0,0,0,164,165,1,0,0,0,165,166,5,24,0,0,166,
        35,1,0,0,0,167,172,3,22,11,0,168,169,5,28,0,0,169,171,3,22,11,0,
        170,168,1,0,0,0,171,174,1,0,0,0,172,170,1,0,0,0,172,173,1,0,0,0,
        173,37,1,0,0,0,174,172,1,0,0,0,175,176,7,3,0,0,176,39,1,0,0,0,177,
        183,5,37,0,0,178,183,5,36,0,0,179,183,5,35,0,0,180,183,5,16,0,0,
        181,183,3,42,21,0,182,177,1,0,0,0,182,178,1,0,0,0,182,179,1,0,0,
        0,182,180,1,0,0,0,182,181,1,0,0,0,183,41,1,0,0,0,184,193,5,25,0,
        0,185,190,3,40,20,0,186,187,5,28,0,0,187,189,3,40,20,0,188,186,1,
        0,0,0,189,192,1,0,0,0,190,188,1,0,0,0,190,191,1,0,0,0,191,194,1,
        0,0,0,192,190,1,0,0,0,193,185,1,0,0,0,193,194,1,0,0,0,194,195,1,
        0,0,0,195,196,5,26,0,0,196,43,1,0,0,0,197,203,5,38,0,0,198,200,5,
        23,0,0,199,201,3,46,23,0,200,199,1,0,0,0,200,201,1,0,0,0,201,202,
        1,0,0,0,202,204,5,24,0,0,203,198,1,0,0,0,203,204,1,0,0,0,204,214,
        1,0,0,0,205,211,5,37,0,0,206,208,5,23,0,0,207,209,3,46,23,0,208,
        207,1,0,0,0,208,209,1,0,0,0,209,210,1,0,0,0,210,212,5,24,0,0,211,
        206,1,0,0,0,211,212,1,0,0,0,212,214,1,0,0,0,213,197,1,0,0,0,213,
        205,1,0,0,0,214,45,1,0,0,0,215,220,3,48,24,0,216,217,5,28,0,0,217,
        219,3,48,24,0,218,216,1,0,0,0,219,222,1,0,0,0,220,218,1,0,0,0,220,
        221,1,0,0,0,221,47,1,0,0,0,222,220,1,0,0,0,223,224,3,22,11,0,224,
        49,1,0,0,0,25,53,63,73,76,83,93,101,105,114,125,133,137,148,155,
        163,172,182,190,193,200,203,208,211,213,220
    ]

class RulesParser ( Parser ):

    grammarFileName = "Rules.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'rule'", "'if'", "'then'", "'else'", 
                     "'and'", "'or'", "'not'", "'in'", "'not_in'", "'is_null'", 
                     "'is_not_null'", "'contains'", "'starts_with'", "'ends_with'", 
                     "'matches'", "'null'", "'=='", "'!='", "'<'", "'<='", 
                     "'>'", "'>='", "'('", "')'", "'['", "']'", "'.'", "','", 
                     "':'", "'+'", "'-'", "'*'", "'/'", "'%'" ]

    symbolicNames = [ "<INVALID>", "RULE", "IF", "THEN", "ELSE", "AND", 
                      "OR", "NOT", "IN", "NOT_IN", "IS_NULL", "IS_NOT_NULL", 
                      "CONTAINS", "STARTS_WITH", "ENDS_WITH", "MATCHES", 
                      "NULL", "EQ", "NE", "LT", "LE", "GT", "GE", "LPAREN", 
                      "RPAREN", "LBRACKET", "RBRACKET", "DOT", "COMMA", 
                      "COLON", "PLUS", "MINUS", "MULT", "DIV", "MOD", "BOOLEAN", 
                      "NUMBER", "STRING", "IDENTIFIER", "WS", "LINE_COMMENT" ]

    RULE_ruleSet = 0
    RULE_rule = 1
    RULE_ruleName = 2
    RULE_ruleStep = 3
    RULE_actionList = 4
    RULE_condition = 5
    RULE_orExpression = 6
    RULE_andExpression = 7
    RULE_notExpression = 8
    RULE_primaryExpression = 9
    RULE_comparison = 10
    RULE_expression = 11
    RULE_term = 12
    RULE_factor = 13
    RULE_atom = 14
    RULE_attribute = 15
    RULE_attributeIdentifier = 16
    RULE_functionCall = 17
    RULE_functionArgs = 18
    RULE_operator = 19
    RULE_value = 20
    RULE_list = 21
    RULE_action = 22
    RULE_parameterList = 23
    RULE_parameter = 24

    ruleNames =  [ "ruleSet", "rule", "ruleName", "ruleStep", "actionList", 
                   "condition", "orExpression", "andExpression", "notExpression", 
                   "primaryExpression", "comparison", "expression", "term", 
                   "factor", "atom", "attribute", "attributeIdentifier", 
                   "functionCall", "functionArgs", "operator", "value", 
                   "list", "action", "parameterList", "parameter" ]

    EOF = Token.EOF
    RULE=1
    IF=2
    THEN=3
    ELSE=4
    AND=5
    OR=6
    NOT=7
    IN=8
    NOT_IN=9
    IS_NULL=10
    IS_NOT_NULL=11
    CONTAINS=12
    STARTS_WITH=13
    ENDS_WITH=14
    MATCHES=15
    NULL=16
    EQ=17
    NE=18
    LT=19
    LE=20
    GT=21
    GE=22
    LPAREN=23
    RPAREN=24
    LBRACKET=25
    RBRACKET=26
    DOT=27
    COMMA=28
    COLON=29
    PLUS=30
    MINUS=31
    MULT=32
    DIV=33
    MOD=34
    BOOLEAN=35
    NUMBER=36
    STRING=37
    IDENTIFIER=38
    WS=39
    LINE_COMMENT=40

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.13.2")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class RuleSetContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def EOF(self):
            return self.getToken(RulesParser.EOF, 0)

        def rule_(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.RuleContext)
            else:
                return self.getTypedRuleContext(RulesParser.RuleContext,i)


        def getRuleIndex(self):
            return RulesParser.RULE_ruleSet

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRuleSet" ):
                listener.enterRuleSet(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRuleSet" ):
                listener.exitRuleSet(self)




    def ruleSet(self):

        localctx = RulesParser.RuleSetContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_ruleSet)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 51 
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while True:
                self.state = 50
                self.rule_()
                self.state = 53 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if not (_la==1):
                    break

            self.state = 55
            self.match(RulesParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RuleContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def RULE(self):
            return self.getToken(RulesParser.RULE, 0)

        def ruleName(self):
            return self.getTypedRuleContext(RulesParser.RuleNameContext,0)


        def COLON(self):
            return self.getToken(RulesParser.COLON, 0)

        def ruleStep(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.RuleStepContext)
            else:
                return self.getTypedRuleContext(RulesParser.RuleStepContext,i)


        def getRuleIndex(self):
            return RulesParser.RULE_rule

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRule" ):
                listener.enterRule(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRule" ):
                listener.exitRule(self)




    def rule_(self):

        localctx = RulesParser.RuleContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_rule)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 57
            self.match(RulesParser.RULE)
            self.state = 58
            self.ruleName()
            self.state = 59
            self.match(RulesParser.COLON)
            self.state = 61 
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while True:
                self.state = 60
                self.ruleStep()
                self.state = 63 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if not ((((_la) & ~0x3f) == 0 and ((1 << _la) & 412316860420) != 0)):
                    break

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RuleNameContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def STRING(self):
            return self.getToken(RulesParser.STRING, 0)

        def IDENTIFIER(self):
            return self.getToken(RulesParser.IDENTIFIER, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_ruleName

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRuleName" ):
                listener.enterRuleName(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRuleName" ):
                listener.exitRuleName(self)




    def ruleName(self):

        localctx = RulesParser.RuleNameContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_ruleName)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 65
            _la = self._input.LA(1)
            if not(_la==37 or _la==38):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RuleStepContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IF(self):
            return self.getToken(RulesParser.IF, 0)

        def condition(self):
            return self.getTypedRuleContext(RulesParser.ConditionContext,0)


        def THEN(self):
            return self.getToken(RulesParser.THEN, 0)

        def actionList(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.ActionListContext)
            else:
                return self.getTypedRuleContext(RulesParser.ActionListContext,i)


        def ELSE(self):
            return self.getToken(RulesParser.ELSE, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_ruleStep

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRuleStep" ):
                listener.enterRuleStep(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRuleStep" ):
                listener.exitRuleStep(self)




    def ruleStep(self):

        localctx = RulesParser.RuleStepContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_ruleStep)
        self._la = 0 # Token type
        try:
            self.state = 76
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [2]:
                self.enterOuterAlt(localctx, 1)
                self.state = 67
                self.match(RulesParser.IF)
                self.state = 68
                self.condition()
                self.state = 69
                self.match(RulesParser.THEN)
                self.state = 70
                self.actionList()
                self.state = 73
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==4:
                    self.state = 71
                    self.match(RulesParser.ELSE)
                    self.state = 72
                    self.actionList()


                pass
            elif token in [37, 38]:
                self.enterOuterAlt(localctx, 2)
                self.state = 75
                self.actionList()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ActionListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def action(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.ActionContext)
            else:
                return self.getTypedRuleContext(RulesParser.ActionContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.COMMA)
            else:
                return self.getToken(RulesParser.COMMA, i)

        def getRuleIndex(self):
            return RulesParser.RULE_actionList

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterActionList" ):
                listener.enterActionList(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitActionList" ):
                listener.exitActionList(self)




    def actionList(self):

        localctx = RulesParser.ActionListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_actionList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 78
            self.action()
            self.state = 83
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==28:
                self.state = 79
                self.match(RulesParser.COMMA)
                self.state = 80
                self.action()
                self.state = 85
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ConditionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def orExpression(self):
            return self.getTypedRuleContext(RulesParser.OrExpressionContext,0)


        def getRuleIndex(self):
            return RulesParser.RULE_condition

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterCondition" ):
                listener.enterCondition(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitCondition" ):
                listener.exitCondition(self)




    def condition(self):

        localctx = RulesParser.ConditionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_condition)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 86
            self.orExpression()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class OrExpressionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def andExpression(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.AndExpressionContext)
            else:
                return self.getTypedRuleContext(RulesParser.AndExpressionContext,i)


        def OR(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.OR)
            else:
                return self.getToken(RulesParser.OR, i)

        def getRuleIndex(self):
            return RulesParser.RULE_orExpression

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterOrExpression" ):
                listener.enterOrExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitOrExpression" ):
                listener.exitOrExpression(self)




    def orExpression(self):

        localctx = RulesParser.OrExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_orExpression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 88
            self.andExpression()
            self.state = 93
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==6:
                self.state = 89
                self.match(RulesParser.OR)
                self.state = 90
                self.andExpression()
                self.state = 95
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AndExpressionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def notExpression(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.NotExpressionContext)
            else:
                return self.getTypedRuleContext(RulesParser.NotExpressionContext,i)


        def AND(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.AND)
            else:
                return self.getToken(RulesParser.AND, i)

        def getRuleIndex(self):
            return RulesParser.RULE_andExpression

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAndExpression" ):
                listener.enterAndExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAndExpression" ):
                listener.exitAndExpression(self)




    def andExpression(self):

        localctx = RulesParser.AndExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 14, self.RULE_andExpression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 96
            self.notExpression()
            self.state = 101
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==5:
                self.state = 97
                self.match(RulesParser.AND)
                self.state = 98
                self.notExpression()
                self.state = 103
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class NotExpressionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def primaryExpression(self):
            return self.getTypedRuleContext(RulesParser.PrimaryExpressionContext,0)


        def NOT(self):
            return self.getToken(RulesParser.NOT, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_notExpression

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterNotExpression" ):
                listener.enterNotExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitNotExpression" ):
                listener.exitNotExpression(self)




    def notExpression(self):

        localctx = RulesParser.NotExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 16, self.RULE_notExpression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 105
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==7:
                self.state = 104
                self.match(RulesParser.NOT)


            self.state = 107
            self.primaryExpression()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class PrimaryExpressionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def comparison(self):
            return self.getTypedRuleContext(RulesParser.ComparisonContext,0)


        def LPAREN(self):
            return self.getToken(RulesParser.LPAREN, 0)

        def orExpression(self):
            return self.getTypedRuleContext(RulesParser.OrExpressionContext,0)


        def RPAREN(self):
            return self.getToken(RulesParser.RPAREN, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_primaryExpression

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterPrimaryExpression" ):
                listener.enterPrimaryExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitPrimaryExpression" ):
                listener.exitPrimaryExpression(self)




    def primaryExpression(self):

        localctx = RulesParser.PrimaryExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 18, self.RULE_primaryExpression)
        try:
            self.state = 114
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,8,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 109
                self.comparison()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 110
                self.match(RulesParser.LPAREN)
                self.state = 111
                self.orExpression()
                self.state = 112
                self.match(RulesParser.RPAREN)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ComparisonContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expression(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.ExpressionContext)
            else:
                return self.getTypedRuleContext(RulesParser.ExpressionContext,i)


        def operator(self):
            return self.getTypedRuleContext(RulesParser.OperatorContext,0)


        def getRuleIndex(self):
            return RulesParser.RULE_comparison

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterComparison" ):
                listener.enterComparison(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitComparison" ):
                listener.exitComparison(self)




    def comparison(self):

        localctx = RulesParser.ComparisonContext(self, self._ctx, self.state)
        self.enterRule(localctx, 20, self.RULE_comparison)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 116
            self.expression()
            self.state = 117
            self.operator()
            self.state = 118
            self.expression()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExpressionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def term(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.TermContext)
            else:
                return self.getTypedRuleContext(RulesParser.TermContext,i)


        def PLUS(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.PLUS)
            else:
                return self.getToken(RulesParser.PLUS, i)

        def MINUS(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.MINUS)
            else:
                return self.getToken(RulesParser.MINUS, i)

        def getRuleIndex(self):
            return RulesParser.RULE_expression

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExpression" ):
                listener.enterExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExpression" ):
                listener.exitExpression(self)




    def expression(self):

        localctx = RulesParser.ExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 22, self.RULE_expression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 120
            self.term()
            self.state = 125
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==30 or _la==31:
                self.state = 121
                _la = self._input.LA(1)
                if not(_la==30 or _la==31):
                    self._errHandler.recoverInline(self)
                else:
                    self._errHandler.reportMatch(self)
                    self.consume()
                self.state = 122
                self.term()
                self.state = 127
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TermContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def factor(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.FactorContext)
            else:
                return self.getTypedRuleContext(RulesParser.FactorContext,i)


        def MULT(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.MULT)
            else:
                return self.getToken(RulesParser.MULT, i)

        def DIV(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.DIV)
            else:
                return self.getToken(RulesParser.DIV, i)

        def MOD(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.MOD)
            else:
                return self.getToken(RulesParser.MOD, i)

        def getRuleIndex(self):
            return RulesParser.RULE_term

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterTerm" ):
                listener.enterTerm(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitTerm" ):
                listener.exitTerm(self)




    def term(self):

        localctx = RulesParser.TermContext(self, self._ctx, self.state)
        self.enterRule(localctx, 24, self.RULE_term)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 128
            self.factor()
            self.state = 133
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 30064771072) != 0):
                self.state = 129
                _la = self._input.LA(1)
                if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 30064771072) != 0)):
                    self._errHandler.recoverInline(self)
                else:
                    self._errHandler.reportMatch(self)
                    self.consume()
                self.state = 130
                self.factor()
                self.state = 135
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class FactorContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def atom(self):
            return self.getTypedRuleContext(RulesParser.AtomContext,0)


        def MINUS(self):
            return self.getToken(RulesParser.MINUS, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_factor

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterFactor" ):
                listener.enterFactor(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitFactor" ):
                listener.exitFactor(self)




    def factor(self):

        localctx = RulesParser.FactorContext(self, self._ctx, self.state)
        self.enterRule(localctx, 26, self.RULE_factor)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 137
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==31:
                self.state = 136
                self.match(RulesParser.MINUS)


            self.state = 139
            self.atom()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AtomContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def attribute(self):
            return self.getTypedRuleContext(RulesParser.AttributeContext,0)


        def value(self):
            return self.getTypedRuleContext(RulesParser.ValueContext,0)


        def functionCall(self):
            return self.getTypedRuleContext(RulesParser.FunctionCallContext,0)


        def LPAREN(self):
            return self.getToken(RulesParser.LPAREN, 0)

        def expression(self):
            return self.getTypedRuleContext(RulesParser.ExpressionContext,0)


        def RPAREN(self):
            return self.getToken(RulesParser.RPAREN, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_atom

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAtom" ):
                listener.enterAtom(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAtom" ):
                listener.exitAtom(self)




    def atom(self):

        localctx = RulesParser.AtomContext(self, self._ctx, self.state)
        self.enterRule(localctx, 28, self.RULE_atom)
        try:
            self.state = 148
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,12,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 141
                self.attribute()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 142
                self.value()
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)
                self.state = 143
                self.functionCall()
                pass

            elif la_ == 4:
                self.enterOuterAlt(localctx, 4)
                self.state = 144
                self.match(RulesParser.LPAREN)
                self.state = 145
                self.expression()
                self.state = 146
                self.match(RulesParser.RPAREN)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AttributeContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def attributeIdentifier(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.AttributeIdentifierContext)
            else:
                return self.getTypedRuleContext(RulesParser.AttributeIdentifierContext,i)


        def DOT(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.DOT)
            else:
                return self.getToken(RulesParser.DOT, i)

        def getRuleIndex(self):
            return RulesParser.RULE_attribute

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAttribute" ):
                listener.enterAttribute(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAttribute" ):
                listener.exitAttribute(self)




    def attribute(self):

        localctx = RulesParser.AttributeContext(self, self._ctx, self.state)
        self.enterRule(localctx, 30, self.RULE_attribute)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 150
            self.attributeIdentifier()
            self.state = 155
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==27:
                self.state = 151
                self.match(RulesParser.DOT)
                self.state = 152
                self.attributeIdentifier()
                self.state = 157
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AttributeIdentifierContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def STRING(self):
            return self.getToken(RulesParser.STRING, 0)

        def IDENTIFIER(self):
            return self.getToken(RulesParser.IDENTIFIER, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_attributeIdentifier

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAttributeIdentifier" ):
                listener.enterAttributeIdentifier(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAttributeIdentifier" ):
                listener.exitAttributeIdentifier(self)




    def attributeIdentifier(self):

        localctx = RulesParser.AttributeIdentifierContext(self, self._ctx, self.state)
        self.enterRule(localctx, 32, self.RULE_attributeIdentifier)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 158
            _la = self._input.LA(1)
            if not(_la==37 or _la==38):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class FunctionCallContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IDENTIFIER(self):
            return self.getToken(RulesParser.IDENTIFIER, 0)

        def LPAREN(self):
            return self.getToken(RulesParser.LPAREN, 0)

        def RPAREN(self):
            return self.getToken(RulesParser.RPAREN, 0)

        def functionArgs(self):
            return self.getTypedRuleContext(RulesParser.FunctionArgsContext,0)


        def getRuleIndex(self):
            return RulesParser.RULE_functionCall

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterFunctionCall" ):
                listener.enterFunctionCall(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitFunctionCall" ):
                listener.exitFunctionCall(self)




    def functionCall(self):

        localctx = RulesParser.FunctionCallContext(self, self._ctx, self.state)
        self.enterRule(localctx, 34, self.RULE_functionCall)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 160
            self.match(RulesParser.IDENTIFIER)
            self.state = 161
            self.match(RulesParser.LPAREN)
            self.state = 163
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 517585567744) != 0):
                self.state = 162
                self.functionArgs()


            self.state = 165
            self.match(RulesParser.RPAREN)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class FunctionArgsContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expression(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.ExpressionContext)
            else:
                return self.getTypedRuleContext(RulesParser.ExpressionContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.COMMA)
            else:
                return self.getToken(RulesParser.COMMA, i)

        def getRuleIndex(self):
            return RulesParser.RULE_functionArgs

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterFunctionArgs" ):
                listener.enterFunctionArgs(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitFunctionArgs" ):
                listener.exitFunctionArgs(self)




    def functionArgs(self):

        localctx = RulesParser.FunctionArgsContext(self, self._ctx, self.state)
        self.enterRule(localctx, 36, self.RULE_functionArgs)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 167
            self.expression()
            self.state = 172
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==28:
                self.state = 168
                self.match(RulesParser.COMMA)
                self.state = 169
                self.expression()
                self.state = 174
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class OperatorContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IN(self):
            return self.getToken(RulesParser.IN, 0)

        def NOT_IN(self):
            return self.getToken(RulesParser.NOT_IN, 0)

        def IS_NULL(self):
            return self.getToken(RulesParser.IS_NULL, 0)

        def IS_NOT_NULL(self):
            return self.getToken(RulesParser.IS_NOT_NULL, 0)

        def CONTAINS(self):
            return self.getToken(RulesParser.CONTAINS, 0)

        def STARTS_WITH(self):
            return self.getToken(RulesParser.STARTS_WITH, 0)

        def ENDS_WITH(self):
            return self.getToken(RulesParser.ENDS_WITH, 0)

        def MATCHES(self):
            return self.getToken(RulesParser.MATCHES, 0)

        def EQ(self):
            return self.getToken(RulesParser.EQ, 0)

        def NE(self):
            return self.getToken(RulesParser.NE, 0)

        def LT(self):
            return self.getToken(RulesParser.LT, 0)

        def LE(self):
            return self.getToken(RulesParser.LE, 0)

        def GT(self):
            return self.getToken(RulesParser.GT, 0)

        def GE(self):
            return self.getToken(RulesParser.GE, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_operator

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterOperator" ):
                listener.enterOperator(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitOperator" ):
                listener.exitOperator(self)




    def operator(self):

        localctx = RulesParser.OperatorContext(self, self._ctx, self.state)
        self.enterRule(localctx, 38, self.RULE_operator)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 175
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 8322816) != 0)):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ValueContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def STRING(self):
            return self.getToken(RulesParser.STRING, 0)

        def NUMBER(self):
            return self.getToken(RulesParser.NUMBER, 0)

        def BOOLEAN(self):
            return self.getToken(RulesParser.BOOLEAN, 0)

        def NULL(self):
            return self.getToken(RulesParser.NULL, 0)

        def list_(self):
            return self.getTypedRuleContext(RulesParser.ListContext,0)


        def getRuleIndex(self):
            return RulesParser.RULE_value

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterValue" ):
                listener.enterValue(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitValue" ):
                listener.exitValue(self)




    def value(self):

        localctx = RulesParser.ValueContext(self, self._ctx, self.state)
        self.enterRule(localctx, 40, self.RULE_value)
        try:
            self.state = 182
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [37]:
                self.enterOuterAlt(localctx, 1)
                self.state = 177
                self.match(RulesParser.STRING)
                pass
            elif token in [36]:
                self.enterOuterAlt(localctx, 2)
                self.state = 178
                self.match(RulesParser.NUMBER)
                pass
            elif token in [35]:
                self.enterOuterAlt(localctx, 3)
                self.state = 179
                self.match(RulesParser.BOOLEAN)
                pass
            elif token in [16]:
                self.enterOuterAlt(localctx, 4)
                self.state = 180
                self.match(RulesParser.NULL)
                pass
            elif token in [25]:
                self.enterOuterAlt(localctx, 5)
                self.state = 181
                self.list_()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LBRACKET(self):
            return self.getToken(RulesParser.LBRACKET, 0)

        def RBRACKET(self):
            return self.getToken(RulesParser.RBRACKET, 0)

        def value(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.ValueContext)
            else:
                return self.getTypedRuleContext(RulesParser.ValueContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.COMMA)
            else:
                return self.getToken(RulesParser.COMMA, i)

        def getRuleIndex(self):
            return RulesParser.RULE_list

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterList" ):
                listener.enterList(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitList" ):
                listener.exitList(self)




    def list_(self):

        localctx = RulesParser.ListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 42, self.RULE_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 184
            self.match(RulesParser.LBRACKET)
            self.state = 193
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 240551788544) != 0):
                self.state = 185
                self.value()
                self.state = 190
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while _la==28:
                    self.state = 186
                    self.match(RulesParser.COMMA)
                    self.state = 187
                    self.value()
                    self.state = 192
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)



            self.state = 195
            self.match(RulesParser.RBRACKET)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ActionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def IDENTIFIER(self):
            return self.getToken(RulesParser.IDENTIFIER, 0)

        def LPAREN(self):
            return self.getToken(RulesParser.LPAREN, 0)

        def RPAREN(self):
            return self.getToken(RulesParser.RPAREN, 0)

        def parameterList(self):
            return self.getTypedRuleContext(RulesParser.ParameterListContext,0)


        def STRING(self):
            return self.getToken(RulesParser.STRING, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_action

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAction" ):
                listener.enterAction(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAction" ):
                listener.exitAction(self)




    def action(self):

        localctx = RulesParser.ActionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 44, self.RULE_action)
        self._la = 0 # Token type
        try:
            self.state = 213
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [38]:
                self.enterOuterAlt(localctx, 1)
                self.state = 197
                self.match(RulesParser.IDENTIFIER)
                self.state = 203
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==23:
                    self.state = 198
                    self.match(RulesParser.LPAREN)
                    self.state = 200
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if (((_la) & ~0x3f) == 0 and ((1 << _la) & 517585567744) != 0):
                        self.state = 199
                        self.parameterList()


                    self.state = 202
                    self.match(RulesParser.RPAREN)


                pass
            elif token in [37]:
                self.enterOuterAlt(localctx, 2)
                self.state = 205
                self.match(RulesParser.STRING)
                self.state = 211
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==23:
                    self.state = 206
                    self.match(RulesParser.LPAREN)
                    self.state = 208
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if (((_la) & ~0x3f) == 0 and ((1 << _la) & 517585567744) != 0):
                        self.state = 207
                        self.parameterList()


                    self.state = 210
                    self.match(RulesParser.RPAREN)


                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ParameterListContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def parameter(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.ParameterContext)
            else:
                return self.getTypedRuleContext(RulesParser.ParameterContext,i)


        def COMMA(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.COMMA)
            else:
                return self.getToken(RulesParser.COMMA, i)

        def getRuleIndex(self):
            return RulesParser.RULE_parameterList

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterParameterList" ):
                listener.enterParameterList(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitParameterList" ):
                listener.exitParameterList(self)




    def parameterList(self):

        localctx = RulesParser.ParameterListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 46, self.RULE_parameterList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 215
            self.parameter()
            self.state = 220
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==28:
                self.state = 216
                self.match(RulesParser.COMMA)
                self.state = 217
                self.parameter()
                self.state = 222
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ParameterContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expression(self):
            return self.getTypedRuleContext(RulesParser.ExpressionContext,0)


        def getRuleIndex(self):
            return RulesParser.RULE_parameter

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterParameter" ):
                listener.enterParameter(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitParameter" ):
                listener.exitParameter(self)




    def parameter(self):

        localctx = RulesParser.ParameterContext(self, self._ctx, self.state)
        self.enterRule(localctx, 48, self.RULE_parameter)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 223
            self.expression()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





