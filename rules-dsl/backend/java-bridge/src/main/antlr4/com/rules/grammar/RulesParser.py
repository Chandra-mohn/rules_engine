# Generated from /Users/chandramohn/workspace/rules_engine/ui-prototype/backend/java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4 by ANTLR 4.13.2
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
        4,1,45,253,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
        6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,13,
        2,14,7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,2,19,7,19,2,20,
        7,20,2,21,7,21,2,22,7,22,2,23,7,23,2,24,7,24,2,25,7,25,2,26,7,26,
        2,27,7,27,1,0,4,0,58,8,0,11,0,12,0,59,1,0,1,0,1,1,1,1,1,1,1,1,4,
        1,68,8,1,11,1,12,1,69,1,2,1,2,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,
        3,5,3,83,8,3,10,3,12,3,86,9,3,1,3,1,3,3,3,90,8,3,1,3,1,3,1,4,4,4,
        95,8,4,11,4,12,4,96,1,5,1,5,1,5,3,5,102,8,5,1,6,1,6,1,6,5,6,107,
        8,6,10,6,12,6,110,9,6,1,7,1,7,1,8,1,8,1,9,1,9,1,9,5,9,119,8,9,10,
        9,12,9,122,9,9,1,10,1,10,1,10,5,10,127,8,10,10,10,12,10,130,9,10,
        1,11,3,11,133,8,11,1,11,1,11,1,12,1,12,1,12,1,12,1,12,3,12,142,8,
        12,1,13,1,13,1,13,1,13,1,14,1,14,1,14,5,14,151,8,14,10,14,12,14,
        154,9,14,1,15,1,15,1,15,5,15,159,8,15,10,15,12,15,162,9,15,1,16,
        3,16,165,8,16,1,16,1,16,1,17,1,17,1,17,1,17,1,17,1,17,1,17,3,17,
        176,8,17,1,18,1,18,1,18,5,18,181,8,18,10,18,12,18,184,9,18,1,19,
        1,19,1,20,1,20,1,20,3,20,191,8,20,1,20,1,20,1,21,1,21,1,21,5,21,
        198,8,21,10,21,12,21,201,9,21,1,22,1,22,1,23,1,23,1,23,1,23,1,23,
        3,23,210,8,23,1,24,1,24,1,24,1,24,5,24,216,8,24,10,24,12,24,219,
        9,24,3,24,221,8,24,1,24,1,24,1,25,1,25,1,25,3,25,228,8,25,1,25,3,
        25,231,8,25,1,25,1,25,1,25,3,25,236,8,25,1,25,3,25,239,8,25,3,25,
        241,8,25,1,26,1,26,1,26,5,26,246,8,26,10,26,12,26,249,9,26,1,27,
        1,27,1,27,0,0,28,0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,
        36,38,40,42,44,46,48,50,52,54,0,5,2,0,40,41,43,43,1,0,33,34,1,0,
        35,37,2,0,40,40,43,43,2,0,11,18,20,25,257,0,57,1,0,0,0,2,63,1,0,
        0,0,4,71,1,0,0,0,6,73,1,0,0,0,8,94,1,0,0,0,10,101,1,0,0,0,12,103,
        1,0,0,0,14,111,1,0,0,0,16,113,1,0,0,0,18,115,1,0,0,0,20,123,1,0,
        0,0,22,132,1,0,0,0,24,141,1,0,0,0,26,143,1,0,0,0,28,147,1,0,0,0,
        30,155,1,0,0,0,32,164,1,0,0,0,34,175,1,0,0,0,36,177,1,0,0,0,38,185,
        1,0,0,0,40,187,1,0,0,0,42,194,1,0,0,0,44,202,1,0,0,0,46,209,1,0,
        0,0,48,211,1,0,0,0,50,240,1,0,0,0,52,242,1,0,0,0,54,250,1,0,0,0,
        56,58,3,2,1,0,57,56,1,0,0,0,58,59,1,0,0,0,59,57,1,0,0,0,59,60,1,
        0,0,0,60,61,1,0,0,0,61,62,5,0,0,1,62,1,1,0,0,0,63,64,5,1,0,0,64,
        65,3,4,2,0,65,67,5,32,0,0,66,68,3,10,5,0,67,66,1,0,0,0,68,69,1,0,
        0,0,69,67,1,0,0,0,69,70,1,0,0,0,70,3,1,0,0,0,71,72,7,0,0,0,72,5,
        1,0,0,0,73,74,5,2,0,0,74,75,3,16,8,0,75,76,5,3,0,0,76,84,3,8,4,0,
        77,78,5,4,0,0,78,79,3,16,8,0,79,80,5,3,0,0,80,81,3,8,4,0,81,83,1,
        0,0,0,82,77,1,0,0,0,83,86,1,0,0,0,84,82,1,0,0,0,84,85,1,0,0,0,85,
        89,1,0,0,0,86,84,1,0,0,0,87,88,5,5,0,0,88,90,3,8,4,0,89,87,1,0,0,
        0,89,90,1,0,0,0,90,91,1,0,0,0,91,92,5,6,0,0,92,7,1,0,0,0,93,95,3,
        10,5,0,94,93,1,0,0,0,95,96,1,0,0,0,96,94,1,0,0,0,96,97,1,0,0,0,97,
        9,1,0,0,0,98,102,3,6,3,0,99,102,3,12,6,0,100,102,3,14,7,0,101,98,
        1,0,0,0,101,99,1,0,0,0,101,100,1,0,0,0,102,11,1,0,0,0,103,108,3,
        50,25,0,104,105,5,31,0,0,105,107,3,50,25,0,106,104,1,0,0,0,107,110,
        1,0,0,0,108,106,1,0,0,0,108,109,1,0,0,0,109,13,1,0,0,0,110,108,1,
        0,0,0,111,112,5,7,0,0,112,15,1,0,0,0,113,114,3,18,9,0,114,17,1,0,
        0,0,115,120,3,20,10,0,116,117,5,9,0,0,117,119,3,20,10,0,118,116,
        1,0,0,0,119,122,1,0,0,0,120,118,1,0,0,0,120,121,1,0,0,0,121,19,1,
        0,0,0,122,120,1,0,0,0,123,128,3,22,11,0,124,125,5,8,0,0,125,127,
        3,22,11,0,126,124,1,0,0,0,127,130,1,0,0,0,128,126,1,0,0,0,128,129,
        1,0,0,0,129,21,1,0,0,0,130,128,1,0,0,0,131,133,5,10,0,0,132,131,
        1,0,0,0,132,133,1,0,0,0,133,134,1,0,0,0,134,135,3,24,12,0,135,23,
        1,0,0,0,136,142,3,26,13,0,137,138,5,26,0,0,138,139,3,18,9,0,139,
        140,5,27,0,0,140,142,1,0,0,0,141,136,1,0,0,0,141,137,1,0,0,0,142,
        25,1,0,0,0,143,144,3,28,14,0,144,145,3,44,22,0,145,146,3,28,14,0,
        146,27,1,0,0,0,147,152,3,30,15,0,148,149,7,1,0,0,149,151,3,30,15,
        0,150,148,1,0,0,0,151,154,1,0,0,0,152,150,1,0,0,0,152,153,1,0,0,
        0,153,29,1,0,0,0,154,152,1,0,0,0,155,160,3,32,16,0,156,157,7,2,0,
        0,157,159,3,32,16,0,158,156,1,0,0,0,159,162,1,0,0,0,160,158,1,0,
        0,0,160,161,1,0,0,0,161,31,1,0,0,0,162,160,1,0,0,0,163,165,5,34,
        0,0,164,163,1,0,0,0,164,165,1,0,0,0,165,166,1,0,0,0,166,167,3,34,
        17,0,167,33,1,0,0,0,168,176,3,36,18,0,169,176,3,46,23,0,170,176,
        3,40,20,0,171,172,5,26,0,0,172,173,3,28,14,0,173,174,5,27,0,0,174,
        176,1,0,0,0,175,168,1,0,0,0,175,169,1,0,0,0,175,170,1,0,0,0,175,
        171,1,0,0,0,176,35,1,0,0,0,177,182,3,38,19,0,178,179,5,30,0,0,179,
        181,3,38,19,0,180,178,1,0,0,0,181,184,1,0,0,0,182,180,1,0,0,0,182,
        183,1,0,0,0,183,37,1,0,0,0,184,182,1,0,0,0,185,186,7,3,0,0,186,39,
        1,0,0,0,187,188,5,43,0,0,188,190,5,26,0,0,189,191,3,42,21,0,190,
        189,1,0,0,0,190,191,1,0,0,0,191,192,1,0,0,0,192,193,5,27,0,0,193,
        41,1,0,0,0,194,199,3,28,14,0,195,196,5,31,0,0,196,198,3,28,14,0,
        197,195,1,0,0,0,198,201,1,0,0,0,199,197,1,0,0,0,199,200,1,0,0,0,
        200,43,1,0,0,0,201,199,1,0,0,0,202,203,7,4,0,0,203,45,1,0,0,0,204,
        210,5,41,0,0,205,210,5,39,0,0,206,210,5,38,0,0,207,210,5,19,0,0,
        208,210,3,48,24,0,209,204,1,0,0,0,209,205,1,0,0,0,209,206,1,0,0,
        0,209,207,1,0,0,0,209,208,1,0,0,0,210,47,1,0,0,0,211,220,5,28,0,
        0,212,217,3,46,23,0,213,214,5,31,0,0,214,216,3,46,23,0,215,213,1,
        0,0,0,216,219,1,0,0,0,217,215,1,0,0,0,217,218,1,0,0,0,218,221,1,
        0,0,0,219,217,1,0,0,0,220,212,1,0,0,0,220,221,1,0,0,0,221,222,1,
        0,0,0,222,223,5,29,0,0,223,49,1,0,0,0,224,230,5,43,0,0,225,227,5,
        26,0,0,226,228,3,52,26,0,227,226,1,0,0,0,227,228,1,0,0,0,228,229,
        1,0,0,0,229,231,5,27,0,0,230,225,1,0,0,0,230,231,1,0,0,0,231,241,
        1,0,0,0,232,238,5,40,0,0,233,235,5,26,0,0,234,236,3,52,26,0,235,
        234,1,0,0,0,235,236,1,0,0,0,236,237,1,0,0,0,237,239,5,27,0,0,238,
        233,1,0,0,0,238,239,1,0,0,0,239,241,1,0,0,0,240,224,1,0,0,0,240,
        232,1,0,0,0,241,51,1,0,0,0,242,247,3,54,27,0,243,244,5,31,0,0,244,
        246,3,54,27,0,245,243,1,0,0,0,246,249,1,0,0,0,247,245,1,0,0,0,247,
        248,1,0,0,0,248,53,1,0,0,0,249,247,1,0,0,0,250,251,3,28,14,0,251,
        55,1,0,0,0,27,59,69,84,89,96,101,108,120,128,132,141,152,160,164,
        175,182,190,199,209,217,220,227,230,235,238,240,247
    ]

class RulesParser ( Parser ):

    grammarFileName = "Rules.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'rule'", "'if'", "'then'", "'elseif'", 
                     "'else'", "'endif'", "'return'", "'and'", "'or'", "'not'", 
                     "'in'", "'not_in'", "'is_null'", "'is_not_null'", "'contains'", 
                     "'starts_with'", "'ends_with'", "'matches'", "'null'", 
                     "'=='", "'!='", "'<'", "'<='", "'>'", "'>='", "'('", 
                     "')'", "'['", "']'", "'.'", "','", "':'", "'+'", "'-'", 
                     "'*'", "'/'", "'%'" ]

    symbolicNames = [ "<INVALID>", "RULE", "IF", "THEN", "ELSEIF", "ELSE", 
                      "ENDIF", "RETURN", "AND", "OR", "NOT", "IN", "NOT_IN", 
                      "IS_NULL", "IS_NOT_NULL", "CONTAINS", "STARTS_WITH", 
                      "ENDS_WITH", "MATCHES", "NULL", "EQ", "NE", "LT", 
                      "LE", "GT", "GE", "LPAREN", "RPAREN", "LBRACKET", 
                      "RBRACKET", "DOT", "COMMA", "COLON", "PLUS", "MINUS", 
                      "MULT", "DIV", "MOD", "BOOLEAN", "NUMBER", "DQUOTED_STRING", 
                      "SQUOTED_STRING", "STRING", "IDENTIFIER", "WS", "LINE_COMMENT" ]

    RULE_ruleSet = 0
    RULE_rule = 1
    RULE_ruleName = 2
    RULE_ruleStep = 3
    RULE_block = 4
    RULE_blockItem = 5
    RULE_actionSequence = 6
    RULE_returnStatement = 7
    RULE_condition = 8
    RULE_orExpression = 9
    RULE_andExpression = 10
    RULE_notExpression = 11
    RULE_primaryExpression = 12
    RULE_comparison = 13
    RULE_expression = 14
    RULE_term = 15
    RULE_factor = 16
    RULE_atom = 17
    RULE_attribute = 18
    RULE_attributeIdentifier = 19
    RULE_functionCall = 20
    RULE_functionArgs = 21
    RULE_operator = 22
    RULE_value = 23
    RULE_list = 24
    RULE_action = 25
    RULE_parameterList = 26
    RULE_parameter = 27

    ruleNames =  [ "ruleSet", "rule", "ruleName", "ruleStep", "block", "blockItem", 
                   "actionSequence", "returnStatement", "condition", "orExpression", 
                   "andExpression", "notExpression", "primaryExpression", 
                   "comparison", "expression", "term", "factor", "atom", 
                   "attribute", "attributeIdentifier", "functionCall", "functionArgs", 
                   "operator", "value", "list", "action", "parameterList", 
                   "parameter" ]

    EOF = Token.EOF
    RULE=1
    IF=2
    THEN=3
    ELSEIF=4
    ELSE=5
    ENDIF=6
    RETURN=7
    AND=8
    OR=9
    NOT=10
    IN=11
    NOT_IN=12
    IS_NULL=13
    IS_NOT_NULL=14
    CONTAINS=15
    STARTS_WITH=16
    ENDS_WITH=17
    MATCHES=18
    NULL=19
    EQ=20
    NE=21
    LT=22
    LE=23
    GT=24
    GE=25
    LPAREN=26
    RPAREN=27
    LBRACKET=28
    RBRACKET=29
    DOT=30
    COMMA=31
    COLON=32
    PLUS=33
    MINUS=34
    MULT=35
    DIV=36
    MOD=37
    BOOLEAN=38
    NUMBER=39
    DQUOTED_STRING=40
    SQUOTED_STRING=41
    STRING=42
    IDENTIFIER=43
    WS=44
    LINE_COMMENT=45

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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRuleSet" ):
                return visitor.visitRuleSet(self)
            else:
                return visitor.visitChildren(self)




    def ruleSet(self):

        localctx = RulesParser.RuleSetContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_ruleSet)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 57 
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while True:
                self.state = 56
                self.rule_()
                self.state = 59 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if not (_la==1):
                    break

            self.state = 61
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

        def blockItem(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.BlockItemContext)
            else:
                return self.getTypedRuleContext(RulesParser.BlockItemContext,i)


        def getRuleIndex(self):
            return RulesParser.RULE_rule

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRule" ):
                listener.enterRule(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRule" ):
                listener.exitRule(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRule" ):
                return visitor.visitRule(self)
            else:
                return visitor.visitChildren(self)




    def rule_(self):

        localctx = RulesParser.RuleContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_rule)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 63
            self.match(RulesParser.RULE)
            self.state = 64
            self.ruleName()
            self.state = 65
            self.match(RulesParser.COLON)
            self.state = 67 
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while True:
                self.state = 66
                self.blockItem()
                self.state = 69 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if not ((((_la) & ~0x3f) == 0 and ((1 << _la) & 9895604650116) != 0)):
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

        def DQUOTED_STRING(self):
            return self.getToken(RulesParser.DQUOTED_STRING, 0)

        def SQUOTED_STRING(self):
            return self.getToken(RulesParser.SQUOTED_STRING, 0)

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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRuleName" ):
                return visitor.visitRuleName(self)
            else:
                return visitor.visitChildren(self)




    def ruleName(self):

        localctx = RulesParser.RuleNameContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_ruleName)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 71
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 12094627905536) != 0)):
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

        def condition(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.ConditionContext)
            else:
                return self.getTypedRuleContext(RulesParser.ConditionContext,i)


        def THEN(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.THEN)
            else:
                return self.getToken(RulesParser.THEN, i)

        def block(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.BlockContext)
            else:
                return self.getTypedRuleContext(RulesParser.BlockContext,i)


        def ENDIF(self):
            return self.getToken(RulesParser.ENDIF, 0)

        def ELSEIF(self, i:int=None):
            if i is None:
                return self.getTokens(RulesParser.ELSEIF)
            else:
                return self.getToken(RulesParser.ELSEIF, i)

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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRuleStep" ):
                return visitor.visitRuleStep(self)
            else:
                return visitor.visitChildren(self)




    def ruleStep(self):

        localctx = RulesParser.RuleStepContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_ruleStep)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 73
            self.match(RulesParser.IF)
            self.state = 74
            self.condition()
            self.state = 75
            self.match(RulesParser.THEN)
            self.state = 76
            self.block()
            self.state = 84
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==4:
                self.state = 77
                self.match(RulesParser.ELSEIF)
                self.state = 78
                self.condition()
                self.state = 79
                self.match(RulesParser.THEN)
                self.state = 80
                self.block()
                self.state = 86
                self._errHandler.sync(self)
                _la = self._input.LA(1)

            self.state = 89
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==5:
                self.state = 87
                self.match(RulesParser.ELSE)
                self.state = 88
                self.block()


            self.state = 91
            self.match(RulesParser.ENDIF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class BlockContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def blockItem(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(RulesParser.BlockItemContext)
            else:
                return self.getTypedRuleContext(RulesParser.BlockItemContext,i)


        def getRuleIndex(self):
            return RulesParser.RULE_block

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterBlock" ):
                listener.enterBlock(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitBlock" ):
                listener.exitBlock(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitBlock" ):
                return visitor.visitBlock(self)
            else:
                return visitor.visitChildren(self)




    def block(self):

        localctx = RulesParser.BlockContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_block)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 94 
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while True:
                self.state = 93
                self.blockItem()
                self.state = 96 
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if not ((((_la) & ~0x3f) == 0 and ((1 << _la) & 9895604650116) != 0)):
                    break

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class BlockItemContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def ruleStep(self):
            return self.getTypedRuleContext(RulesParser.RuleStepContext,0)


        def actionSequence(self):
            return self.getTypedRuleContext(RulesParser.ActionSequenceContext,0)


        def returnStatement(self):
            return self.getTypedRuleContext(RulesParser.ReturnStatementContext,0)


        def getRuleIndex(self):
            return RulesParser.RULE_blockItem

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterBlockItem" ):
                listener.enterBlockItem(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitBlockItem" ):
                listener.exitBlockItem(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitBlockItem" ):
                return visitor.visitBlockItem(self)
            else:
                return visitor.visitChildren(self)




    def blockItem(self):

        localctx = RulesParser.BlockItemContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_blockItem)
        try:
            self.state = 101
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [2]:
                self.enterOuterAlt(localctx, 1)
                self.state = 98
                self.ruleStep()
                pass
            elif token in [40, 43]:
                self.enterOuterAlt(localctx, 2)
                self.state = 99
                self.actionSequence()
                pass
            elif token in [7]:
                self.enterOuterAlt(localctx, 3)
                self.state = 100
                self.returnStatement()
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


    class ActionSequenceContext(ParserRuleContext):
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
            return RulesParser.RULE_actionSequence

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterActionSequence" ):
                listener.enterActionSequence(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitActionSequence" ):
                listener.exitActionSequence(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitActionSequence" ):
                return visitor.visitActionSequence(self)
            else:
                return visitor.visitChildren(self)




    def actionSequence(self):

        localctx = RulesParser.ActionSequenceContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_actionSequence)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 103
            self.action()
            self.state = 108
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==31:
                self.state = 104
                self.match(RulesParser.COMMA)
                self.state = 105
                self.action()
                self.state = 110
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ReturnStatementContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def RETURN(self):
            return self.getToken(RulesParser.RETURN, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_returnStatement

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterReturnStatement" ):
                listener.enterReturnStatement(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitReturnStatement" ):
                listener.exitReturnStatement(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitReturnStatement" ):
                return visitor.visitReturnStatement(self)
            else:
                return visitor.visitChildren(self)




    def returnStatement(self):

        localctx = RulesParser.ReturnStatementContext(self, self._ctx, self.state)
        self.enterRule(localctx, 14, self.RULE_returnStatement)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 111
            self.match(RulesParser.RETURN)
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitCondition" ):
                return visitor.visitCondition(self)
            else:
                return visitor.visitChildren(self)




    def condition(self):

        localctx = RulesParser.ConditionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 16, self.RULE_condition)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 113
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitOrExpression" ):
                return visitor.visitOrExpression(self)
            else:
                return visitor.visitChildren(self)




    def orExpression(self):

        localctx = RulesParser.OrExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 18, self.RULE_orExpression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 115
            self.andExpression()
            self.state = 120
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==9:
                self.state = 116
                self.match(RulesParser.OR)
                self.state = 117
                self.andExpression()
                self.state = 122
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAndExpression" ):
                return visitor.visitAndExpression(self)
            else:
                return visitor.visitChildren(self)




    def andExpression(self):

        localctx = RulesParser.AndExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 20, self.RULE_andExpression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 123
            self.notExpression()
            self.state = 128
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==8:
                self.state = 124
                self.match(RulesParser.AND)
                self.state = 125
                self.notExpression()
                self.state = 130
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitNotExpression" ):
                return visitor.visitNotExpression(self)
            else:
                return visitor.visitChildren(self)




    def notExpression(self):

        localctx = RulesParser.NotExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 22, self.RULE_notExpression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 132
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==10:
                self.state = 131
                self.match(RulesParser.NOT)


            self.state = 134
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitPrimaryExpression" ):
                return visitor.visitPrimaryExpression(self)
            else:
                return visitor.visitChildren(self)




    def primaryExpression(self):

        localctx = RulesParser.PrimaryExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 24, self.RULE_primaryExpression)
        try:
            self.state = 141
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,10,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 136
                self.comparison()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 137
                self.match(RulesParser.LPAREN)
                self.state = 138
                self.orExpression()
                self.state = 139
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitComparison" ):
                return visitor.visitComparison(self)
            else:
                return visitor.visitChildren(self)




    def comparison(self):

        localctx = RulesParser.ComparisonContext(self, self._ctx, self.state)
        self.enterRule(localctx, 26, self.RULE_comparison)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 143
            self.expression()
            self.state = 144
            self.operator()
            self.state = 145
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExpression" ):
                return visitor.visitExpression(self)
            else:
                return visitor.visitChildren(self)




    def expression(self):

        localctx = RulesParser.ExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 28, self.RULE_expression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 147
            self.term()
            self.state = 152
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==33 or _la==34:
                self.state = 148
                _la = self._input.LA(1)
                if not(_la==33 or _la==34):
                    self._errHandler.recoverInline(self)
                else:
                    self._errHandler.reportMatch(self)
                    self.consume()
                self.state = 149
                self.term()
                self.state = 154
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTerm" ):
                return visitor.visitTerm(self)
            else:
                return visitor.visitChildren(self)




    def term(self):

        localctx = RulesParser.TermContext(self, self._ctx, self.state)
        self.enterRule(localctx, 30, self.RULE_term)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 155
            self.factor()
            self.state = 160
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while (((_la) & ~0x3f) == 0 and ((1 << _la) & 240518168576) != 0):
                self.state = 156
                _la = self._input.LA(1)
                if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 240518168576) != 0)):
                    self._errHandler.recoverInline(self)
                else:
                    self._errHandler.reportMatch(self)
                    self.consume()
                self.state = 157
                self.factor()
                self.state = 162
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitFactor" ):
                return visitor.visitFactor(self)
            else:
                return visitor.visitChildren(self)




    def factor(self):

        localctx = RulesParser.FactorContext(self, self._ctx, self.state)
        self.enterRule(localctx, 32, self.RULE_factor)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 164
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==34:
                self.state = 163
                self.match(RulesParser.MINUS)


            self.state = 166
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAtom" ):
                return visitor.visitAtom(self)
            else:
                return visitor.visitChildren(self)




    def atom(self):

        localctx = RulesParser.AtomContext(self, self._ctx, self.state)
        self.enterRule(localctx, 34, self.RULE_atom)
        try:
            self.state = 175
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,14,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 168
                self.attribute()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 169
                self.value()
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)
                self.state = 170
                self.functionCall()
                pass

            elif la_ == 4:
                self.enterOuterAlt(localctx, 4)
                self.state = 171
                self.match(RulesParser.LPAREN)
                self.state = 172
                self.expression()
                self.state = 173
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAttribute" ):
                return visitor.visitAttribute(self)
            else:
                return visitor.visitChildren(self)




    def attribute(self):

        localctx = RulesParser.AttributeContext(self, self._ctx, self.state)
        self.enterRule(localctx, 36, self.RULE_attribute)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 177
            self.attributeIdentifier()
            self.state = 182
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==30:
                self.state = 178
                self.match(RulesParser.DOT)
                self.state = 179
                self.attributeIdentifier()
                self.state = 184
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

        def DQUOTED_STRING(self):
            return self.getToken(RulesParser.DQUOTED_STRING, 0)

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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAttributeIdentifier" ):
                return visitor.visitAttributeIdentifier(self)
            else:
                return visitor.visitChildren(self)




    def attributeIdentifier(self):

        localctx = RulesParser.AttributeIdentifierContext(self, self._ctx, self.state)
        self.enterRule(localctx, 38, self.RULE_attributeIdentifier)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 185
            _la = self._input.LA(1)
            if not(_la==40 or _la==43):
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitFunctionCall" ):
                return visitor.visitFunctionCall(self)
            else:
                return visitor.visitChildren(self)




    def functionCall(self):

        localctx = RulesParser.FunctionCallContext(self, self._ctx, self.state)
        self.enterRule(localctx, 40, self.RULE_functionCall)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 187
            self.match(RulesParser.IDENTIFIER)
            self.state = 188
            self.match(RulesParser.LPAREN)
            self.state = 190
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 12936777564160) != 0):
                self.state = 189
                self.functionArgs()


            self.state = 192
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitFunctionArgs" ):
                return visitor.visitFunctionArgs(self)
            else:
                return visitor.visitChildren(self)




    def functionArgs(self):

        localctx = RulesParser.FunctionArgsContext(self, self._ctx, self.state)
        self.enterRule(localctx, 42, self.RULE_functionArgs)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 194
            self.expression()
            self.state = 199
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==31:
                self.state = 195
                self.match(RulesParser.COMMA)
                self.state = 196
                self.expression()
                self.state = 201
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitOperator" ):
                return visitor.visitOperator(self)
            else:
                return visitor.visitChildren(self)




    def operator(self):

        localctx = RulesParser.OperatorContext(self, self._ctx, self.state)
        self.enterRule(localctx, 44, self.RULE_operator)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 202
            _la = self._input.LA(1)
            if not((((_la) & ~0x3f) == 0 and ((1 << _la) & 66582528) != 0)):
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

        def SQUOTED_STRING(self):
            return self.getToken(RulesParser.SQUOTED_STRING, 0)

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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitValue" ):
                return visitor.visitValue(self)
            else:
                return visitor.visitChildren(self)




    def value(self):

        localctx = RulesParser.ValueContext(self, self._ctx, self.state)
        self.enterRule(localctx, 46, self.RULE_value)
        try:
            self.state = 209
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [41]:
                self.enterOuterAlt(localctx, 1)
                self.state = 204
                self.match(RulesParser.SQUOTED_STRING)
                pass
            elif token in [39]:
                self.enterOuterAlt(localctx, 2)
                self.state = 205
                self.match(RulesParser.NUMBER)
                pass
            elif token in [38]:
                self.enterOuterAlt(localctx, 3)
                self.state = 206
                self.match(RulesParser.BOOLEAN)
                pass
            elif token in [19]:
                self.enterOuterAlt(localctx, 4)
                self.state = 207
                self.match(RulesParser.NULL)
                pass
            elif token in [28]:
                self.enterOuterAlt(localctx, 5)
                self.state = 208
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitList" ):
                return visitor.visitList(self)
            else:
                return visitor.visitChildren(self)




    def list_(self):

        localctx = RulesParser.ListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 48, self.RULE_list)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 211
            self.match(RulesParser.LBRACKET)
            self.state = 220
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if (((_la) & ~0x3f) == 0 and ((1 << _la) & 3023925936128) != 0):
                self.state = 212
                self.value()
                self.state = 217
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                while _la==31:
                    self.state = 213
                    self.match(RulesParser.COMMA)
                    self.state = 214
                    self.value()
                    self.state = 219
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)



            self.state = 222
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


        def DQUOTED_STRING(self):
            return self.getToken(RulesParser.DQUOTED_STRING, 0)

        def getRuleIndex(self):
            return RulesParser.RULE_action

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAction" ):
                listener.enterAction(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAction" ):
                listener.exitAction(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAction" ):
                return visitor.visitAction(self)
            else:
                return visitor.visitChildren(self)




    def action(self):

        localctx = RulesParser.ActionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 50, self.RULE_action)
        self._la = 0 # Token type
        try:
            self.state = 240
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [43]:
                self.enterOuterAlt(localctx, 1)
                self.state = 224
                self.match(RulesParser.IDENTIFIER)
                self.state = 230
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==26:
                    self.state = 225
                    self.match(RulesParser.LPAREN)
                    self.state = 227
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if (((_la) & ~0x3f) == 0 and ((1 << _la) & 12936777564160) != 0):
                        self.state = 226
                        self.parameterList()


                    self.state = 229
                    self.match(RulesParser.RPAREN)


                pass
            elif token in [40]:
                self.enterOuterAlt(localctx, 2)
                self.state = 232
                self.match(RulesParser.DQUOTED_STRING)
                self.state = 238
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==26:
                    self.state = 233
                    self.match(RulesParser.LPAREN)
                    self.state = 235
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if (((_la) & ~0x3f) == 0 and ((1 << _la) & 12936777564160) != 0):
                        self.state = 234
                        self.parameterList()


                    self.state = 237
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitParameterList" ):
                return visitor.visitParameterList(self)
            else:
                return visitor.visitChildren(self)




    def parameterList(self):

        localctx = RulesParser.ParameterListContext(self, self._ctx, self.state)
        self.enterRule(localctx, 52, self.RULE_parameterList)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 242
            self.parameter()
            self.state = 247
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==31:
                self.state = 243
                self.match(RulesParser.COMMA)
                self.state = 244
                self.parameter()
                self.state = 249
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

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitParameter" ):
                return visitor.visitParameter(self)
            else:
                return visitor.visitChildren(self)




    def parameter(self):

        localctx = RulesParser.ParameterContext(self, self._ctx, self.state)
        self.enterRule(localctx, 54, self.RULE_parameter)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 250
            self.expression()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





