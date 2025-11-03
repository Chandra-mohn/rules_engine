grammar Rules;

// Parser rules
ruleSet: rule+ EOF;

rule: RULE ruleName COLON ruleStep+;
ruleName: DQUOTED_STRING | SQUOTED_STRING | IDENTIFIER;  // Rule names can use either quote type

ruleStep:
    IF condition THEN block (ELSEIF condition THEN block)* (ELSE block)? ENDIF
    | actionList;

block: ruleStep+ | actionList;

actionList: action (',' action)*;

condition: orExpression;

orExpression: andExpression (OR andExpression)*;
andExpression: notExpression (AND notExpression)*;
notExpression: NOT? primaryExpression;

primaryExpression:
    comparison
    | '(' orExpression ')';

comparison: expression operator expression;

// Arithmetic expressions with proper precedence
expression: term ((PLUS | MINUS) term)*;
term: factor ((MULT | DIV | MOD) factor)*;
factor: MINUS? atom;
atom: attribute | value | functionCall | '(' expression ')';

attribute: attributeIdentifier ('.' attributeIdentifier)*;
attributeIdentifier: DQUOTED_STRING | IDENTIFIER;  // Attributes use double quotes for special chars

// Function call support
functionCall: IDENTIFIER '(' functionArgs? ')';
functionArgs: expression (',' expression)*;

operator: (IN | NOT_IN | IS_NULL | IS_NOT_NULL | CONTAINS |
          STARTS_WITH | ENDS_WITH | MATCHES | EQ | NE | LT | LE | GT | GE);

value: SQUOTED_STRING | NUMBER | BOOLEAN | NULL | list;  // String literals use single quotes only
list: '[' (value (',' value)*)? ']';

action:
    IDENTIFIER ('(' parameterList? ')')?
    | DQUOTED_STRING ('(' parameterList? ')')?;  // Actions with special chars use double quotes

parameterList: parameter (',' parameter)*;
parameter: expression;

// Lexer rules
RULE: 'rule';
IF: 'if';
THEN: 'then';
ELSEIF: 'elseif';
ELSE: 'else';
ENDIF: 'endif';
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
EQ: '==';
NE: '!=';
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

// Arithmetic operators
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';

BOOLEAN: 'true' | 'false';
NUMBER: [0-9]+ ('.' [0-9]+)?;
DQUOTED_STRING: '"' (~["\r\n])* '"';  // Double-quoted: for attribute names with special chars
SQUOTED_STRING: '\'' (~['\r\n])* '\'';  // Single-quoted: for string literals/constants
STRING: DQUOTED_STRING | SQUOTED_STRING;  // Backwards compatibility - matches both
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;

WS: [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;