 Changes Made for Nested IF Support Implementation

  Modified Files

  1. Grammar File

  File: /java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4
  - Lines 9-13: Changed grammar to support recursive nesting
    - Changed IF condition THEN actionList to IF condition THEN block
    - Changed ELSEIF condition THEN actionList to ELSEIF condition THEN block
    - Changed ELSE actionList to ELSE block
    - Added new rule: block: ruleStep+ | actionList;

  2. Python Code Generator

  File: /backend/grammar_parser/template_code_generator.py
  - Lines 74-111: Modified _convert_rule_step() method
    - Changed from ctx.actionList(0) to ctx.block(0) for THEN blocks
    - Changed from ctx.actionList(i) to ctx.block(i) for ELSEIF blocks
    - Changed from ctx.actionList(num_conditions) to ctx.block(num_conditions) for ELSE blocks
    - Calls new _convert_block() method instead of _convert_action_list()
  - Lines 113-143: Added _convert_block() method
    - Handles recursive conversion of nested rule steps
    - Handles terminal action lists
    - Applies proper indentation for nested code
  - Lines 145-160: Added _indent() helper method
    - Adds proper indentation to generated Java code
    - Supports multiple indentation levels (4 spaces per level)

  3. Rule Validator

  File: /backend/grammar_parser/rule_validator.py
  - Lines 20-51: Added NestingDepthValidator class
    - Tracks nesting depth as parse tree is walked
    - Enforces maximum depth of 32 levels
    - Generates error when depth exceeds limit
    - enterRuleStep(): Increments depth counter for IF statements
    - exitRuleStep(): Decrements depth counter
  - Lines 175-190: Integrated nesting validator into validate_rule() method
    - Added Step 2: Nesting depth validation
    - Validator runs after syntax validation, before semantic validation
    - Returns immediately if nesting depth errors found
    - Includes max_depth_reached in error response

  4. Generated Parser Files

  Updated files (regenerated from grammar):
  - /java-bridge/src/main/antlr4/com/rules/grammar/RulesParser.py
  - /java-bridge/src/main/antlr4/com/rules/grammar/RulesLexer.py
  - /java-bridge/src/main/antlr4/com/rules/grammar/RulesListener.py

  Copied to:
  - /backend/java-bridge/src/main/antlr4/com/rules/grammar/RulesParser.py
  - /backend/java-bridge/src/main/antlr4/com/rules/grammar/RulesLexer.py
  - /backend/java-bridge/src/main/antlr4/com/rules/grammar/RulesListener.py

  5. Test File (New)

  File: /backend/test_nested_if.py
  - Comprehensive test suite with 6 test cases
  - Tests backward compatibility, basic nesting, moderate nesting, depth limits, and mixed
  structures

  Git Branch

  - Created feature branch: feature/nested-if-support

  Summary of Changes

  - 4 files modified: Grammar, code generator, validator, test file
  - 6 parser files regenerated: 3 in main java-bridge, 3 copied to backend
  - Total lines changed: ~150 lines added/modified
  - Test coverage: 6 comprehensive test cases, all passing
