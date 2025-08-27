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
    : expression operator expression
    ;

expression
    : multiplicativeExpression ((PLUS | MINUS) multiplicativeExpression)*
    | dateTimeExpression
    ;

multiplicativeExpression
    : atomicExpression ((MULTIPLY | DIVIDE | MODULO) atomicExpression)*
    ;

atomicExpression
    : attribute
    | value
    | LPAREN expression RPAREN
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
    | BEFORE | AFTER | BETWEEN | WITHIN
    ;

value
    : STRING
    | NUMBER
    | BOOLEAN
    | NULL
    | dateTime
    | duration
    | list
    ;

list
    : LBRACKET (value (COMMA value)*)? RBRACKET
    ;

action
    : IDENTIFIER
    ;

// Date/Time Expressions
dateTimeExpression
    : dateTime (PLUS | MINUS) duration
    | dateTime
    ;

dateTime
    : DATE_LITERAL
    | DATETIME_LITERAL
    | TIME_LITERAL
    | NOW
    | TODAY
    | BUSINESS_DATE
    | dateTimeFunction
    | attribute
    ;

dateTimeFunction
    : DATE_OF LPAREN expression RPAREN
    | TIME_OF LPAREN expression RPAREN
    | YEAR_OF LPAREN expression RPAREN
    | MONTH_OF LPAREN expression RPAREN
    | DAY_OF LPAREN expression RPAREN
    | HOUR_OF LPAREN expression RPAREN
    | MINUTE_OF LPAREN expression RPAREN
    | SECOND_OF LPAREN expression RPAREN
    | DAY_OF_WEEK LPAREN expression RPAREN
    | DAY_OF_YEAR LPAREN expression RPAREN
    | WEEK_OF_YEAR LPAREN expression RPAREN
    | PARSE_DATE LPAREN expression COMMA expression RPAREN
    | FORMAT_DATE LPAREN expression COMMA expression RPAREN
    ;

duration
    : NUMBER (YEARS | MONTHS | WEEKS | DAYS | HOURS | MINUTES | SECONDS | MILLIS)
    | DURATION_LITERAL
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

// Date/Time Keywords
NOW           : 'now' | 'NOW';
TODAY         : 'today' | 'TODAY';
BUSINESS_DATE : 'business_date' | 'BUSINESS_DATE';
BEFORE      : 'before' | 'BEFORE';
AFTER       : 'after' | 'AFTER';
BETWEEN     : 'between' | 'BETWEEN';
WITHIN      : 'within' | 'WITHIN';
IS_WEEKEND  : 'is_weekend' | 'IS_WEEKEND';
IS_WEEKDAY  : 'is_weekday' | 'IS_WEEKDAY';
IS_HOLIDAY  : 'is_holiday' | 'IS_HOLIDAY';
AGE_YEARS   : 'age_years' | 'AGE_YEARS';
AGE_MONTHS  : 'age_months' | 'AGE_MONTHS';
AGE_DAYS    : 'age_days' | 'AGE_DAYS';

// Duration Units
YEARS       : 'years' | 'YEARS' | 'year' | 'YEAR';
MONTHS      : 'months' | 'MONTHS' | 'month' | 'MONTH';
WEEKS       : 'weeks' | 'WEEKS' | 'week' | 'WEEK';
DAYS        : 'days' | 'DAYS' | 'day' | 'DAY';
HOURS       : 'hours' | 'HOURS' | 'hour' | 'HOUR';
MINUTES     : 'minutes' | 'MINUTES' | 'minute' | 'MINUTE';
SECONDS     : 'seconds' | 'SECONDS' | 'second' | 'SECOND';
MILLIS      : 'millis' | 'MILLIS' | 'milliseconds' | 'MILLISECONDS';

// Date/Time Functions
DATE_OF     : 'date_of' | 'DATE_OF';
TIME_OF     : 'time_of' | 'TIME_OF';
YEAR_OF     : 'year_of' | 'YEAR_OF';
MONTH_OF    : 'month_of' | 'MONTH_OF';
DAY_OF      : 'day_of' | 'DAY_OF';
HOUR_OF     : 'hour_of' | 'HOUR_OF';
MINUTE_OF   : 'minute_of' | 'MINUTE_OF';
SECOND_OF   : 'second_of' | 'SECOND_OF';
DAY_OF_WEEK : 'day_of_week' | 'DAY_OF_WEEK';
DAY_OF_YEAR : 'day_of_year' | 'DAY_OF_YEAR';
WEEK_OF_YEAR: 'week_of_year' | 'WEEK_OF_YEAR';
PARSE_DATE  : 'parse_date' | 'PARSE_DATE';
FORMAT_DATE : 'format_date' | 'FORMAT_DATE';

// Operators
EQ          : '=' | '==';
NE          : '!=' | '<>';
LT          : '<';
LE          : '<=';
GT          : '>';
GE          : '>=';

// Arithmetic Operators
PLUS        : '+';
MINUS       : '-';
MULTIPLY    : '*';
DIVIDE      : '/';
MODULO      : '%';

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

// Date/Time Literals
DATE_LITERAL     : DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT;
DATETIME_LITERAL : DATE_LITERAL 'T' TIME_LITERAL ('Z' | TIMEZONE)?;
TIME_LITERAL     : DIGIT DIGIT ':' DIGIT DIGIT (':' DIGIT DIGIT ('.' DIGIT+)?)?;
DURATION_LITERAL : 'P' (DIGIT+ 'Y')? (DIGIT+ 'M')? (DIGIT+ 'W')? (DIGIT+ 'D')? ('T' (DIGIT+ 'H')? (DIGIT+ 'M')? (DIGIT+ ('.' DIGIT+)? 'S')?)?;

IDENTIFIER  : [a-zA-Z_] [a-zA-Z0-9_]*;

// Whitespace and Comments
WS          : [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
HASH_COMMENT: '#' ~[\r\n]* -> skip;

// Fragments
fragment DIGIT : [0-9];
fragment TIMEZONE : [+-] DIGIT DIGIT ':' DIGIT DIGIT;