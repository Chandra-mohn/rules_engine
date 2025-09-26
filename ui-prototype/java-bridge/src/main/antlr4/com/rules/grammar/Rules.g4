grammar Rules;

// Parser rules
ruleSet: definition+ EOF;
definition: unifiedRule;

unifiedRule: RULE ruleName COLON ruleStep+;
ruleName: STRING | IDENTIFIER;

ruleStep:
    IF condition THEN actionList (ELSE actionList)?
    | actionList;

actionList: action (',' action)*;

condition: orExpression;

orExpression: andExpression (OR andExpression)*;
andExpression: notExpression (AND notExpression)*;
notExpression: NOT? primaryExpression;

primaryExpression:
    comparison
    | '(' orExpression ')';

comparison: operand operator operand;
operand: attribute | value | functionCall;

attribute: attributeIdentifier ('.' attributeIdentifier)*;
attributeIdentifier: STRING | IDENTIFIER;

// Function call support (NEW)
functionCall: IDENTIFIER '(' functionArgs? ')';
functionArgs: operand (',' operand)*;

operator: (IN | NOT_IN | IS_NULL | IS_NOT_NULL | CONTAINS |
          STARTS_WITH | ENDS_WITH | MATCHES | EQ | NE | LT | LE | GT | GE);

value: STRING | NUMBER | BOOLEAN | NULL | list;
list: '[' (value (',' value)*)? ']';

action:
    IDENTIFIER ('(' parameterList? ')')?
    | STRING ('(' parameterList? ')')?;

parameterList: parameter (',' parameter)*;
parameter: value | attribute;

// Lexer rules
RULE: 'rule';
IF: 'if';
THEN: 'then';
ELSE: 'else';
AND: 'and';
OR: 'or';
NOT: 'not';
IN: 'in';
NOT_IN: 'not_in';
IS_NULL: 'is_null';
IS_NOT_NULL: 'is_not_null';
CONTAINS: 'contains';
STARTS_WITH: 'starts_with';
ENDS_WITH: 'ends_with';
MATCHES: 'matches';

NULL: 'null';
EQ: '==' | '=' | 'equals';
NE: '!=' | '<>' | 'not_equals';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';

LPAREN: '(';
RPAREN: ')';
LBRACKET: '[';
RBRACKET: ']';
DOT: '.';
COMMA: ',';
COLON: ':';

BOOLEAN: 'true' | 'false';
NUMBER: [0-9]+ ('.' [0-9]+)?;
STRING: '"' (~["\r\n])* '"' | '\'' (~['\r\n])* '\'';
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;

WS: [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
HASH_COMMENT: '#' ~[\r\n]* -> skip;