grammar Rules;

// Parser Rules
ruleSet
    : namedRule+ EOF
    ;

namedRule
    : RULE ruleName COLON step+
    ;

ruleName
    : IDENTIFIER
    ;

step
    : IF condition THEN action (ELSE action)?
    ;

condition
    : orExpression
    ;

orExpression
    : andExpression (OR andExpression)*
    ;

andExpression
    : notExpression (AND notExpression)*
    ;

notExpression
    : NOT? primaryExpression
    ;

primaryExpression
    : comparison
    | LPAREN orExpression RPAREN
    ;

comparison
    : attribute operator operand
    ;

operand
    : attribute
    | value
    ;

attribute
    : IDENTIFIER (DOT IDENTIFIER)*
    ;

operator
    : EQ | NE | LT | LE | GT | GE 
    | CONTAINS | STARTS_WITH | ENDS_WITH
    | IN | NOT_IN
    | IS_NULL | IS_NOT_NULL
    | MATCHES
    ;

value
    : STRING
    | NUMBER
    | BOOLEAN
    | NULL
    | list
    ;

list
    : LBRACKET (value (COMMA value)*)? RBRACKET
    ;

action
    : IDENTIFIER
    | STRING
    ;

// Lexer Rules

// Keywords
RULE        : 'rule' | 'RULE';
IF          : 'if' | 'IF';
THEN        : 'then' | 'THEN';
ELSE        : 'else' | 'ELSE';
AND         : 'and' | 'AND' | '&&';
OR          : 'or' | 'OR' | '||';
NOT         : 'not' | 'NOT' | '!';
IN          : 'in' | 'IN';
NOT_IN      : 'not_in' | 'NOT_IN';
IS_NULL     : 'is_null' | 'IS_NULL';
IS_NOT_NULL : 'is_not_null' | 'IS_NOT_NULL';
CONTAINS    : 'contains' | 'CONTAINS';
STARTS_WITH : 'starts_with' | 'STARTS_WITH';
ENDS_WITH   : 'ends_with' | 'ENDS_WITH';
MATCHES     : 'matches' | 'MATCHES';
NULL        : 'null' | 'NULL';

// Operators
EQ          : '=' | '==';
NE          : '!=' | '<>';
LT          : '<';
LE          : '<=';
GT          : '>';
GE          : '>=';

// Punctuation
LPAREN      : '(';
RPAREN      : ')';
LBRACKET    : '[';
RBRACKET    : ']';
DOT         : '.';
COMMA       : ',';
COLON       : ':';

// Literals
BOOLEAN     : 'true' | 'false' | 'TRUE' | 'FALSE';

NUMBER      : '-'? DIGIT+ ('.' DIGIT+)? ([eE] [+-]? DIGIT+)?;

STRING      : '"' (~["\r\n] | '""')* '"'
            | '\'' (~['\r\n] | '\'\'')* '\''
            ;

IDENTIFIER  : [a-zA-Z_] [a-zA-Z0-9_]*;

// Whitespace and Comments
WS          : [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
HASH_COMMENT: '#' ~[\r\n]* -> skip;

// Fragments
fragment DIGIT : [0-9];