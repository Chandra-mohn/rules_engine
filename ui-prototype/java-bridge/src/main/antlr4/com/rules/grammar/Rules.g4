grammar Rules;

// Parser Rules
ruleSet
    : namedRule+ EOF
    ;

namedRule
    : 'rule' ruleName ':' step+
    ;

ruleName
    : IDENTIFIER
    ;

step
    : 'if' condition 'then' action
    ;

condition
    : orExpression
    ;

orExpression
    : andExpression ('or' andExpression)*
    ;

andExpression
    : notExpression ('and' notExpression)*
    ;

notExpression
    : 'not'? atomicExpression
    ;

atomicExpression
    : comparison
    | '(' orExpression ')'
    ;

comparison
    : expression operator expression
    ;

expression
    : multiplicativeExpression (('+' | '-') multiplicativeExpression)*
    ;

multiplicativeExpression
    : primaryExpression (('*' | '/' | '%') primaryExpression)*
    ;

primaryExpression
    : attribute
    | value
    | functionCall
    ;

functionCall
    : IDENTIFIER '(' (expression (',' expression)*)? ')'
    ;

attribute
    : IDENTIFIER ('.' IDENTIFIER)*
    ;

operator
    : '=' | '!=' | '<' | '<=' | '>' | '>='
    | 'in' | 'between' | 'before' | 'after' | 'within'
    ;

action
    : IDENTIFIER ('(' (value (',' value)*)? ')')?
    ;

value
    : NUMBER
    | STRING
    | BOOLEAN
    | list
    ;

list
    : '[' (value (',' value)*)? ']'
    ;

// Lexer Rules
IDENTIFIER
    : [a-zA-Z_] [a-zA-Z0-9_]*
    ;

NUMBER
    : [0-9]+ ('.' [0-9]+)?
    ;

STRING
    : '"' (~["\r\n])* '"'
    | '\'' (~['\r\n])* '\''
    ;

BOOLEAN
    : 'true' | 'false'
    ;

// Whitespace and Comments
WS
    : [ \t\r\n]+ -> skip
    ;

LINE_COMMENT
    : '//' ~[\r\n]* -> skip
    ;

BLOCK_COMMENT
    : '/*' .*? '*/' -> skip
    ;

// Error handling for unrecognized characters
ERROR_CHAR
    : .
    ;