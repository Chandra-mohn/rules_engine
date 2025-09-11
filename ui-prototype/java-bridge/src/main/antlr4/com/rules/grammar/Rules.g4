grammar Rules;

// Parser Rules
ruleSet
    : namedRule+ EOF
    ;

namedRule
    : 'rule' ruleName ':' step+
    ;

ruleName
    : IDENTIFIER | QUOTED_IDENTIFIER
    ;

step
    : 'if' condition 'then' action
    ;

condition
    : orExpression
    ;

orExpression
    : andExpression (('or' | 'OR') andExpression)*
    ;

andExpression
    : notExpression (('and' | 'AND') notExpression)*
    ;

notExpression
    : ('not' | 'NOT')? atomicExpression
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
    : (IDENTIFIER | QUOTED_IDENTIFIER) '(' (expression (',' expression)*)? ')'
    ;

attribute
    : (IDENTIFIER | QUOTED_IDENTIFIER) ('.' (IDENTIFIER | QUOTED_IDENTIFIER))*
    ;

operator
    : '=' | '!=' | '<>' | '<' | '<=' | '>' | '>='
    | 'in' | 'between' | 'before' | 'after' | 'within'
    ;

action
    : (IDENTIFIER | QUOTED_IDENTIFIER | STRING) ('(' (value (',' value)*)? ')')?
    ;

value
    : NUMBER
    | STRING
    | BOOLEAN
    | list
    | listReference
    ;

listReference
    : IDENTIFIER | QUOTED_IDENTIFIER  // References a named list like VALID_STATUSES or "SPECIAL LIST"
    ;

list
    : '[' (value (',' value)*)? ']'
    ;

// Lexer Rules
QUOTED_IDENTIFIER
    : '"' ~["\r\n]* '"'
    ;

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