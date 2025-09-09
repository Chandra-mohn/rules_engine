# Quoted Identifiers Support

The Rules Engine now supports **quoted identifiers** for rule names, actions, and attributes, allowing special characters and spaces in identifiers.

## Syntax

Use double quotes to wrap identifiers that contain special characters, spaces, or other non-standard characters:

```
rule "PROMOTION $5%3 @SEARS":
    if applicant.creditScore >= 750 then "TIERED DD+1"
    if applicant.age >= 21 then "INSTANT APPROVAL!"
```

## Examples

### Rule Names
```
✅ rule "My Special Rule":
✅ rule "PROMOTION $5%3 @SEARS":  
✅ rule "Multi Word Rule Name":
✅ rule regularRuleName:
```

### Actions
```
✅ if condition then "TIERED DD+1"
✅ if condition then "INSTANT APPROVAL!"
✅ if condition then "MANUAL REVIEW REQUIRED"  
✅ if condition then approveApplication
```

### Mixed Usage
You can mix quoted and unquoted identifiers in the same rule:
```
rule "SPECIAL PROMO":
    if applicant.creditScore >= 750 then "VIP TREATMENT"
    if applicant.age >= 18 then approveApplication
```

## Validation

- **Quoted identifiers**: Accept any characters within quotes (no validation)
- **Unquoted identifiers**: Must follow standard naming rules and are validated against known schemas
- **No quotes within quotes**: The content inside quotes cannot contain quote characters

## Frontend Support

- **Syntax highlighting**: Quoted identifiers appear in black
- **Validation**: Real-time validation supports both quoted and unquoted identifiers
- **Backward compatibility**: All existing rules continue to work

## Use Cases

Perfect for business scenarios requiring descriptive names:
- `"PROMOTION $5%3 @SEARS"`
- `"TIERED DD+1"`
- `"LOW BALANCE ALERT"`
- `"INSTANT APPROVAL!"`
- `"VIP GOLD+ STATUS"`

## Technical Implementation

- **ANTLR Grammar**: Added `QUOTED_IDENTIFIER` lexer rule
- **Java Validation**: Quoted identifiers bypass strict validation  
- **Frontend**: Monaco Editor syntax highlighting with dedicated token type
- **Backward Compatibility**: All existing unquoted identifiers work unchanged