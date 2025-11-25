"""
COBOL to Rules DSL Converter

Converts COBOL procedural logic to Rules DSL using ANTLR parser and visitor pattern.
"""

import sys
from pathlib import Path
from antlr4 import *
from typing import List, Dict, Optional

# Add cobol_parser to path
parser_path = Path(__file__).parent.parent / "cobol_parser"
sys.path.insert(0, str(parser_path))

from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser
from Cobol85Visitor import Cobol85Visitor

# Add services to path
services_path = Path(__file__).parent.parent / "services"
sys.path.insert(0, str(services_path))

from mapping_service import MappingService, AttributeMapping, convert_to_camel_case


class ConversionMetadata:
    """Tracks metadata during conversion"""
    def __init__(self):
        self.mappings: List[Dict] = []
        self.comments: List[str] = []
        self.warnings: List[str] = []

    def add_mapping(self, cobol_name: str, target_name: str, mapping_type: str, confidence: float):
        """Record an attribute mapping"""
        self.mappings.append({
            'cobol': cobol_name,
            'target': target_name,
            'type': mapping_type,
            'confidence': confidence
        })

    def add_comment(self, comment: str):
        """Add a metadata comment"""
        self.comments.append(comment)

    def add_warning(self, warning: str):
        """Add a conversion warning"""
        self.warnings.append(warning)

    def get_metadata_comments(self) -> List[str]:
        """Generate metadata comment lines"""
        comments = []

        # Group mappings by type
        direct_mappings = [m for m in self.mappings if m['type'] == 'direct']
        derived_mappings = [m for m in self.mappings if m['type'] in ['derived', 'fuzzy']]
        action_mappings = [m for m in self.mappings if m['type'] == 'action']

        # Direct mappings
        if direct_mappings:
            for m in direct_mappings:
                comments.append(f"# MAPPED: {m['cobol']} → {m['target']} (direct, confidence: {m['confidence']})")

        # Derived/fuzzy mappings
        if derived_mappings:
            for m in derived_mappings:
                comments.append(f"# DERIVED: {m['cobol']} → {m['target']} (assumed, confidence: {m['confidence']})")

        # Action mappings
        if action_mappings:
            for m in action_mappings:
                comments.append(f"# ACTION: {m['cobol']} → {m['target']} (confidence: {m['confidence']})")

        # Add warnings if any
        if self.warnings:
            comments.append("")
            for warning in self.warnings:
                comments.append(f"# ⚠️ WARNING: {warning}")

        return comments


class CobolToDSLVisitor(Cobol85Visitor):
    """
    ANTLR Visitor that converts COBOL AST to Rules DSL code.

    Focuses on procedure division statements (IF, PERFORM, MOVE, etc.)
    """

    def __init__(self, mapping_service: MappingService):
        self.mapper = mapping_service
        self.metadata = ConversionMetadata()
        self.indent_level = 1
        self.indent = "    "  # 4 spaces

    def get_indentation(self) -> str:
        """Get current indentation string"""
        return self.indent * self.indent_level

    def visitChildren(self, ctx):
        """
        Default visitor - recursively visit children and aggregate results.
        Returns None for unhandled statement types (like STOP, DISPLAY).
        """
        # Check for statement types to skip
        ctx_name = ctx.__class__.__name__

        # Skip non-procedural statements and data division elements
        skip_patterns = [
            'DisplayStatement',  # DISPLAY
            'StopStatement',     # STOP RUN
            'ExitStatement',     # EXIT
            'GobackStatement',   # GOBACK
            'ContinueStatement', # CONTINUE
            'ReadStatement',     # READ (file I/O - commented out)
            'WriteStatement',    # WRITE (file I/O - commented out)
            'InitializeStatement', # INITIALIZE (commented out)
            'ParagraphName',     # Paragraph definitions
            'SectionName',       # Section definitions
            'DataDivision',      # DATA DIVISION (and all children)
            'DataDescriptionEntry',  # Data declarations
            'WorkingStorageSection',  # WORKING-STORAGE SECTION
            'FileSection',       # FILE SECTION
            'LinkageSection',    # LINKAGE SECTION
        ]

        for pattern in skip_patterns:
            if pattern in ctx_name:
                return None

        results = []
        for child in ctx.getChildren():
            result = self.visit(child)
            if result is not None:
                if isinstance(result, list):
                    results.extend(result)
                else:
                    results.append(result)

        return results if results else None

    def visitIfStatement(self, ctx: Cobol85Parser.IfStatementContext):
        """
        Convert IF statement to Rules DSL if/then/else.

        COBOL: IF condition THEN statements ELSE statements END-IF
        DSL:   if condition then
                   statements
               else
                   statements
               endif
        """
        lines = []
        indent = self.get_indentation()

        # Visit condition
        condition = self.visit(ctx.condition())

        lines.append(f"{indent}if {condition} then")

        # Visit THEN branch
        self.indent_level += 1
        then_statements = self.visit(ctx.ifThen())
        if then_statements:
            lines.extend(then_statements if isinstance(then_statements, list) else [then_statements])
        self.indent_level -= 1

        # Visit ELSE branch if present
        if ctx.ifElse():
            lines.append(f"{indent}else")
            self.indent_level += 1
            else_statements = self.visit(ctx.ifElse())
            if else_statements:
                lines.extend(else_statements if isinstance(else_statements, list) else [else_statements])
            self.indent_level -= 1

        lines.append(f"{indent}endif")

        return lines

    def visitCondition(self, ctx: Cobol85Parser.ConditionContext):
        """
        Convert COBOL condition to DSL condition.
        Handles comparisons, AND, OR, NOT.

        Note: In ANTLR Python, ctx.ruleName() returns a list of all matching children
        """
        # Get all combinable conditions and AND/OR operators
        from typing import cast

        # Access all children and filter by type
        children = ctx.getChildren()
        combinable_list = []
        and_or_list = []

        for child in children:
            child_name = child.__class__.__name__
            if 'CombinableConditionContext' in child_name:
                combinable_list.append(child)
            elif 'AndOrConditionContext' in child_name:
                and_or_list.append(child)

        if not combinable_list:
            return "true"

        # If only one condition, visit it directly
        if len(combinable_list) == 1:
            result = self.visit(combinable_list[0])
            return result if result else "true"

        # Multiple conditions with AND/OR
        conditions = []

        for i, comb in enumerate(combinable_list):
            cond = self.visit(comb)
            if cond:
                conditions.append(cond)

                # Add AND/OR between conditions
                if i < len(combinable_list) - 1 and i < len(and_or_list):
                    operator = and_or_list[i].getText().upper()
                    conditions.append(f" {operator.lower()} ")

        return ''.join(conditions) if conditions else "true"

    def visitCombinableCondition(self, ctx):
        """
        Handle combinableCondition: NOT? simpleCondition
        """
        simple_cond = ctx.simpleCondition()
        if simple_cond:
            result = self.visit(simple_cond)
            # Handle NOT
            if ctx.NOT():
                return f"not ({result})"
            return result
        return "true"

    def visitSimpleCondition(self, ctx):
        """
        Handle simpleCondition: can be relationCondition, parenthesized condition, classCondition, or conditionNameReference (88-level)

        Grammar:
        simpleCondition
            : LPARENCHAR condition RPARENCHAR
            | relationCondition
            | classCondition
            | conditionNameReference  <- 88-level conditions
        """
        # Check for parenthesized condition first
        if ctx.condition():
            # LPARENCHAR condition RPARENCHAR
            return f"({self.visit(ctx.condition())})"

        # Check for relationCondition
        if ctx.relationCondition():
            return self.visit(ctx.relationCondition())

        # Check for classCondition (IS NUMERIC, etc.)
        if ctx.classCondition():
            # For now, convert to manual review comment
            return "true  # Class condition - manual review needed"

        # Check for conditionNameReference (88-level conditions)
        if ctx.conditionNameReference():
            return self.visit(ctx.conditionNameReference())

        return "true"

    def visitConditionNameReference(self, ctx):
        """
        Handle 88-level condition name references.

        Grammar:
        conditionNameReference
            : conditionName (inData* inFile? conditionNameSubscriptReference* | inMnemonic*)

        COBOL 88-level pattern:
            01  APPL-RESULT             PIC S9(9)   COMP.
                88  APPL-AOK            VALUE 0.
                88  APPL-EOF            VALUE 16.

            IF  APPL-AOK THEN ...

        Conversion strategy:
        - Map the condition name (APPL-AOK) to a dotted attribute (application.ok)
        - Convert to boolean expression in DSL

        For now, we'll treat 88-level conditions as boolean attributes.
        More sophisticated approach would track the parent variable and value.
        """
        # Get the condition name
        condition_name_ctx = ctx.conditionName()
        if condition_name_ctx:
            condition_name = condition_name_ctx.getText().upper()

            # Map the condition name to target attribute
            mapping = self.mapper.map_with_fallback(condition_name)
            self.metadata.add_mapping(
                cobol_name=condition_name,
                target_name=mapping.target_name,
                mapping_type=mapping.mapping_type,
                confidence=mapping.confidence
            )

            # Return as boolean expression
            # In DSL, 88-level conditions become simple boolean attributes
            return mapping.target_name

        # Fallback
        return "true  # 88-level condition - manual review needed"

    def visitRelationCondition(self, ctx: Cobol85Parser.RelationConditionContext):
        """
        Convert COBOL comparison to DSL comparison.

        RelationCondition has 3 alternatives:
        - relationSignCondition (IS POSITIVE/NEGATIVE/ZERO)
        - relationArithmeticComparison (expr operator expr)
        - relationCombinedComparison (complex comparisons)
        """
        # Check which type of relation condition this is
        if ctx.relationArithmeticComparison():
            return self.visitRelationArithmeticComparison(ctx.relationArithmeticComparison())
        elif ctx.relationSignCondition():
            return self.visitRelationSignCondition(ctx.relationSignCondition())
        elif ctx.relationCombinedComparison():
            return self.visitRelationCombinedComparison(ctx.relationCombinedComparison())

        # Fallback
        return "true"

    def visitArithmeticExpression(self, ctx):
        """
        Handle arithmetic expressions with proper operator precedence.

        Grammar:
        arithmeticExpression: multDivs plusMinus*
        plusMinus: (PLUSCHAR | MINUSCHAR) multDivs

        Example: (TRAN-CAT-BAL * DIS-INT-RATE) / 1200
        """
        # Start with the first multDivs
        result = self.visit(ctx.multDivs())

        # Handle any plusMinus operations
        plus_minus_list = ctx.plusMinus() if hasattr(ctx, 'plusMinus') else []
        for pm in plus_minus_list:
            op_text = pm.getChild(0).getText()  # + or -
            right = self.visit(pm.multDivs())
            result = f"{result} {op_text} {right}"

        return result

    def visitMultDivs(self, ctx):
        """
        Handle multiplication and division expressions.

        Grammar:
        multDivs: powers multDiv*
        multDiv: (ASTERISKCHAR | SLASHCHAR) powers
        """
        # Start with the first powers
        result = self.visit(ctx.powers())

        # Handle any multDiv operations
        mult_div_list = ctx.multDiv() if hasattr(ctx, 'multDiv') else []
        for md in mult_div_list:
            op_text = md.getChild(0).getText()  # * or /
            right = self.visit(md.powers())
            result = f"{result} {op_text} {right}"

        return result

    def visitPowers(self, ctx):
        """
        Handle power expressions and unary +/-.

        Grammar:
        powers: (PLUSCHAR | MINUSCHAR)? basis power*
        """
        # Check for unary + or -
        result = ""
        first_child = ctx.getChild(0)
        if first_child.getText() in ['+', '-']:
            result = first_child.getText()
            basis_result = self.visit(ctx.basis())
        else:
            basis_result = self.visit(ctx.basis())

        result += basis_result

        # Handle power operations (** operator)
        power_list = ctx.power() if hasattr(ctx, 'power') else []
        for p in power_list:
            right = self.visit(p.basis())
            result = f"{result} ** {right}"

        return result

    def visitBasis(self, ctx):
        """
        Handle basis of expressions: parentheses, identifiers, literals.

        Grammar:
        basis
            : LPARENCHAR arithmeticExpression RPARENCHAR
            | identifier
            | literal
        """
        # Check for parenthesized expression
        if ctx.arithmeticExpression():
            inner = self.visit(ctx.arithmeticExpression())
            return f"({inner})"

        # Check for identifier
        if ctx.identifier():
            return self.visit(ctx.identifier())

        # Check for literal
        if ctx.literal():
            return self.visit(ctx.literal())

        # Fallback
        return ctx.getText()

    def visitRelationArithmeticComparison(self, ctx):
        """
        Handle: arithmeticExpression relationalOperator arithmeticExpression
        COBOL: CUSTOMER-TYPE = 'PREMIUM'
        DSL:   customer.type == "PREMIUM"
        """
        # Get left operand
        left = self.visit(ctx.arithmeticExpression(0))
        if isinstance(left, list):
            left = left[0] if left else ""

        # Get operator
        rel_op = ctx.relationalOperator().getText().strip()

        # Map COBOL operators to DSL operators
        operator_map = {
            '=': '==',
            'EQUAL': '==',
            'EQUALS': '==',
            '<>': '!=',
            'NOT EQUAL': '!=',
            '>': '>',
            'GREATER': '>',
            '<': '<',
            'LESS': '<',
            '>=': '>=',
            '<=': '<=',
        }

        dsl_operator = operator_map.get(rel_op.upper(), '==')

        # Get right operand
        right = self.visit(ctx.arithmeticExpression(1))
        if isinstance(right, list):
            right = right[0] if right else ""

        return f"{left} {dsl_operator} {right}"

    def visitRelationSignCondition(self, ctx):
        """Handle IS POSITIVE/NEGATIVE/ZERO conditions"""
        expr = self.visit(ctx.arithmeticExpression())

        if ctx.POSITIVE():
            return f"{expr} > 0"
        elif ctx.NEGATIVE():
            return f"{expr} < 0"
        elif ctx.ZERO():
            return f"{expr} == 0"

        return "true"

    def visitRelationCombinedComparison(self, ctx):
        """Handle combined comparisons (less common)"""
        # For now, fallback to simple conversion
        return "true  # Complex comparison - manual review needed"

    def visitIdentifier(self, ctx: Cobol85Parser.IdentifierContext):
        """
        Convert COBOL identifier to DSL identifier with mapping.

        COBOL: CUSTOMER-TYPE
        DSL:   customer.type (with mapping metadata)
        """
        cobol_name = ctx.getText().upper()

        # Get mapping
        mapping = self.mapper.map_with_fallback(cobol_name)

        # Record mapping metadata
        self.metadata.add_mapping(
            cobol_name=cobol_name,
            target_name=mapping.target_name,
            mapping_type=mapping.mapping_type,
            confidence=mapping.confidence
        )

        return mapping.target_name

    def visitLiteral(self, ctx: Cobol85Parser.LiteralContext):
        """
        Convert COBOL literal to DSL literal.
        Handles string and numeric literals.
        """
        text = ctx.getText()

        # String literals
        if text.startswith("'") or text.startswith('"'):
            # COBOL uses single quotes, DSL uses double quotes
            cleaned = text.strip("'\"")
            return f'"{cleaned}"'

        # Numeric literals
        return text

    def visitPerformStatement(self, ctx: Cobol85Parser.PerformStatementContext):
        """
        Convert PERFORM to action call.

        performStatement: PERFORM (performInlineStatement | performProcedureStatement)
        performProcedureStatement: procedureName ((THROUGH | THRU) procedureName)? performType?

        COBOL: PERFORM APPROVE-PREMIUM
        DSL:   approvePremium()
        """
        indent = self.get_indentation()

        # Check if it's a procedure call (most common)
        if ctx.performProcedureStatement():
            proc_ctx = ctx.performProcedureStatement()

            # Get paragraph name
            paragraph_name = proc_ctx.procedureName(0).getText().upper()

            # Get mapping (likely an action)
            mapping = self.mapper.map_with_fallback(paragraph_name)

            # Record mapping
            self.metadata.add_mapping(
                cobol_name=paragraph_name,
                target_name=mapping.target_name,
                mapping_type=mapping.mapping_type,
                confidence=mapping.confidence
            )

            # Use the target name directly (already camelCase from mapping)
            action_name = mapping.target_name

            return f"{indent}{action_name}()"

        # Inline PERFORM (PERFORM ... END-PERFORM)
        elif ctx.performInlineStatement():
            # For inline performs, visit the statements inside
            inline_ctx = ctx.performInlineStatement()
            statements = []
            for stmt in inline_ctx.statement():
                result = self.visit(stmt)
                if result:
                    statements.extend(result if isinstance(result, list) else [result])
            return statements

        # Fallback
        return f"{indent}# PERFORM statement - manual review needed"

    def visitMoveStatement(self, ctx: Cobol85Parser.MoveStatementContext):
        """
        Convert MOVE to assignment.

        moveStatement: MOVE ALL? (moveToStatement | moveCorrespondingToStatement)
        moveToStatement: moveToSendingArea TO identifier+
        moveToSendingArea: identifier | literal

        COBOL: MOVE 'APPROVED' TO STATUS-CODE
        DSL:   statusCode = "APPROVED"

        COBOL: MOVE TOTAL-AMT TO WORK-AMT FINAL-AMT
        DSL:   workAmt = totalAmt
               finalAmt = totalAmt
        """
        indent = self.get_indentation()

        # Check for moveToStatement
        if ctx.moveToStatement():
            move_to = ctx.moveToStatement()

            # Get source value (moveToSendingArea)
            sending_area = move_to.moveToSendingArea()
            if sending_area:
                source = self.visit(sending_area)
                # Handle list return
                if isinstance(source, list):
                    source = source[0] if source else ""
            else:
                source = "null"

            # Get target identifiers (can be multiple)
            # Access identifiers through getChildren and filter
            targets = []
            for child in move_to.getChildren():
                if 'IdentifierContext' in child.__class__.__name__:
                    target = self.visit(child)
                    if isinstance(target, list):
                        target = target[0] if target else ""
                    if target:
                        targets.append(target)

            # Generate assignment(s)
            lines = []
            for target in targets:
                lines.append(f"{indent}{target} = {source}")

            return lines if len(lines) > 1 else (lines[0] if lines else None)

        # MOVE CORRESPONDING not supported yet
        return f"{indent}# MOVE CORRESPONDING - manual review needed"

    def visitComputeStatement(self, ctx: Cobol85Parser.ComputeStatementContext):
        """
        Convert COMPUTE statement to DSL assignment.

        Grammar:
        computeStatement
            : COMPUTE computeStore+ (EQUALCHAR | EQUAL) arithmeticExpression onSizeErrorPhrase? notOnSizeErrorPhrase? END_COMPUTE?

        computeStore
            : identifier ROUNDED?

        COBOL Example:
            COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200

        DSL Output:
            ws.monthly_int = (tran.cat_bal * dis.int_rate) / 1200

        Supports multiple target variables:
            COMPUTE A B C = X + Y  →  a = x + y; b = x + y; c = x + y
        """
        indent = self.get_indentation()

        # Get all compute stores (target variables)
        stores = ctx.computeStore()
        targets = []
        for store in stores:
            identifier = store.identifier()
            if identifier:
                target = self.visit(identifier)
                if isinstance(target, list):
                    target = target[0] if target else ""
                if target:
                    targets.append(target)

        # Get the arithmetic expression (right-hand side)
        arith_expr = ctx.arithmeticExpression()
        if arith_expr:
            expression = self.visit(arith_expr)
        else:
            expression = "0  # No expression found"

        # Generate assignment(s)
        lines = []
        for target in targets:
            lines.append(f"{indent}{target} = {expression}")

        # Return single line or list
        return lines if len(lines) > 1 else (lines[0] if lines else None)

    def visitEvaluateStatement(self, ctx: Cobol85Parser.EvaluateStatementContext):
        """
        Convert EVALUATE statement to nested if/elseif/else chains.

        Grammar:
        evaluateStatement
            : EVALUATE evaluateSelect evaluateAlsoSelect* evaluateWhenPhrase+ evaluateWhenOther? END_EVALUATE?

        evaluateWhenPhrase
            : evaluateWhen+ statement*

        evaluateWhen
            : WHEN evaluateCondition evaluateAlsoCondition*

        evaluateWhenOther
            : WHEN OTHER statement*

        COBOL Example (Value-Based):
            EVALUATE DATEPARM-STATUS
              WHEN '00'
                  MOVE 0 TO APPL-RESULT
              WHEN '10'
                  MOVE 16 TO APPL-RESULT
              WHEN OTHER
                  MOVE 12 TO APPL-RESULT
            END-EVALUATE

        DSL Output:
            if dateparm.status == "00" then
                appl.result = 0
            elseif dateparm.status == "10" then
                appl.result = 16
            else
                appl.result = 12
            endif

        COBOL Example (EVALUATE TRUE):
            EVALUATE TRUE
              WHEN BALANCE > 10000
                  PERFORM PREMIUM-RATE
              WHEN BALANCE > 5000
                  PERFORM STANDARD-RATE
              WHEN OTHER
                  PERFORM BASIC-RATE
            END-EVALUATE

        DSL Output:
            if balance > 10000 then
                premium.rate()
            elseif balance > 5000 then
                standard.rate()
            else
                basic.rate()
            endif
        """
        indent = self.get_indentation()

        # Get the evaluate select (what we're evaluating)
        select_expr = self.visit(ctx.evaluateSelect())

        # Handle case where visit returns a list
        if isinstance(select_expr, list):
            select_expr = select_expr[0] if select_expr else "true"

        # Check if this is "EVALUATE TRUE" pattern
        is_evaluate_true = select_expr.lower() == "true"

        # Process WHEN phrases
        when_phrases = ctx.evaluateWhenPhrase()
        lines = []

        for i, when_phrase in enumerate(when_phrases):
            # Get WHEN conditions
            when_list = when_phrase.evaluateWhen()
            if not when_list:
                continue

            # For now, support single WHEN condition (most common CardDemo pattern)
            when_ctx = when_list[0]
            eval_condition = when_ctx.evaluateCondition()

            # Build condition
            if is_evaluate_true:
                # EVALUATE TRUE: use condition directly
                condition = self.visit(eval_condition)
                # Handle case where condition is a list
                if isinstance(condition, list):
                    condition = condition[0] if condition else "true"
            else:
                # Value-based EVALUATE: build equality check
                # Check if condition is ANY
                if eval_condition.ANY():
                    condition = "true"
                elif eval_condition.condition():
                    # Condition-based WHEN clause
                    condition = self.visit(eval_condition.condition())
                else:
                    # Value-based: EVALUATE X WHEN 'A' → x == "A"
                    value = self.visit(eval_condition.evaluateValue()) if hasattr(eval_condition, 'evaluateValue') and eval_condition.evaluateValue() else self.visit(eval_condition)
                    # Handle case where value is a list
                    if isinstance(value, list):
                        value = value[0] if value else "null"
                    condition = f"{select_expr} == {value}"

            # Determine if/elseif/else
            if i == 0:
                lines.append(f"{indent}if {condition} then")
            else:
                lines.append(f"{indent}elseif {condition} then")

            # Process statements in this WHEN clause
            self.indent_level += 1
            statements = when_phrase.statement()
            for stmt in statements:
                result = self.visit(stmt)
                if result:
                    if isinstance(result, list):
                        lines.extend(result)
                    else:
                        lines.append(result)
            self.indent_level -= 1

        # Process WHEN OTHER (else clause)
        when_other = ctx.evaluateWhenOther()
        if when_other:
            lines.append(f"{indent}else")
            self.indent_level += 1
            statements = when_other.statement()
            for stmt in statements:
                result = self.visit(stmt)
                if result:
                    if isinstance(result, list):
                        lines.extend(result)
                    else:
                        lines.append(result)
            self.indent_level -= 1

        # Close the if statement
        lines.append(f"{indent}endif")

        return lines

    def visitAddStatement(self, ctx: Cobol85Parser.AddStatementContext):
        """
        Convert ADD statement to DSL assignments.

        Grammar:
        addStatement
            : ADD (addToStatement | addToGivingStatement | addCorrespondingStatement) ...

        addToStatement
            : addFrom+ TO addTo+

        addToGivingStatement
            : addFrom+ (TO addToGiving+)? GIVING addGiving+

        COBOL Example (TO form):
            ADD A TO B              → b = b + a

        COBOL Example (GIVING form - most common in CardDemo):
            ADD 8 TO ZERO GIVING APPL-RESULT    → appl.result = 0 + 8
            ADD A B GIVING C                     → c = a + b

        DSL Output:
            TO form:       b = b + a
            GIVING form:   appl.result = 8 (optimized when adding to zero)
                          c = a + b
        """
        indent = self.get_indentation()

        # Check which form of ADD statement
        if ctx.addToGivingStatement():
            # GIVING form: ADD A B TO C GIVING D  or  ADD A B GIVING C
            giving_stmt = ctx.addToGivingStatement()

            # Get addFrom values (what to add)
            add_from_list = giving_stmt.addFrom()
            from_values = []
            for add_from in add_from_list:
                val = self.visit(add_from)
                # Handle case where visit returns a list
                if isinstance(val, list):
                    val = val[0] if val else ""
                if val:
                    from_values.append(val)

            # Get addToGiving values (optional - values to add to the sum)
            to_giving_list = giving_stmt.addToGiving() if hasattr(giving_stmt, 'addToGiving') else []
            to_values = []
            for to_giving in to_giving_list:
                val = self.visit(to_giving)
                # Handle case where visit returns a list
                if isinstance(val, list):
                    val = val[0] if val else ""
                if val:
                    to_values.append(val)

            # Get GIVING targets (where to store result)
            giving_list = giving_stmt.addGiving()
            targets = []
            for giving in giving_list:
                target = self.visit(giving.identifier())
                if isinstance(target, list):
                    target = target[0] if target else ""
                if target:
                    targets.append(target)

            # Build expression: sum of all from_values and to_values
            all_values = from_values + to_values
            if len(all_values) == 1:
                expression = all_values[0]
            else:
                expression = " + ".join(all_values)

            # Generate assignment(s)
            lines = []
            for target in targets:
                lines.append(f"{indent}{target} = {expression}")

            return lines if len(lines) > 1 else (lines[0] if lines else None)

        elif ctx.addToStatement():
            # TO form: ADD A B TO C D  →  c = c + a + b; d = d + a + b
            to_stmt = ctx.addToStatement()

            # Get addFrom values
            add_from_list = to_stmt.addFrom()
            from_values = []
            for add_from in add_from_list:
                val = self.visit(add_from)
                # Handle case where visit returns a list
                if isinstance(val, list):
                    val = val[0] if val else ""
                if val:
                    from_values.append(val)

            # Get addTo targets
            add_to_list = to_stmt.addTo()
            targets = []
            for add_to in add_to_list:
                target = self.visit(add_to.identifier())
                if isinstance(target, list):
                    target = target[0] if target else ""
                if target:
                    targets.append(target)

            # Build expression for each target: target = target + from1 + from2 + ...
            lines = []
            for target in targets:
                all_values = [target] + from_values
                expression = " + ".join(all_values)
                lines.append(f"{indent}{target} = {expression}")

            return lines if len(lines) > 1 else (lines[0] if lines else None)

        else:
            # CORRESPONDING form - not implemented yet
            return f"{indent}# ADD CORRESPONDING not supported yet"

    def visitSubtractStatement(self, ctx: Cobol85Parser.SubtractStatementContext):
        """
        Convert SUBTRACT statement to DSL assignments.

        Grammar:
        subtractStatement
            : SUBTRACT (subtractFromStatement | subtractFromGivingStatement | subtractCorrespondingStatement) ...

        subtractFromStatement
            : subtractSubtrahend+ FROM subtractMinuend+

        subtractFromGivingStatement
            : subtractSubtrahend+ FROM subtractMinuendGiving GIVING subtractGiving+

        COBOL Example (FROM form - most common in CardDemo):
            SUBTRACT APPL-RESULT FROM APPL-RESULT    → appl.result = 0  (zeroing pattern)
            SUBTRACT A FROM B                        → b = b - a

        COBOL Example (GIVING form):
            SUBTRACT A FROM B GIVING C               → c = b - a

        DSL Output:
            FROM form:     b = b - a
            GIVING form:   c = b - a
            Zero pattern:  appl.result = 0 (optimized)
        """
        indent = self.get_indentation()

        # Check which form of SUBTRACT statement
        if ctx.subtractFromGivingStatement():
            # GIVING form: SUBTRACT A FROM B GIVING C  →  c = b - a
            giving_stmt = ctx.subtractFromGivingStatement()

            # Get subtrahends (what to subtract)
            subtrahend_list = giving_stmt.subtractSubtrahend()
            subtrahend_values = []
            for subtrahend in subtrahend_list:
                val = self.visit(subtrahend)
                # Handle case where visit returns a list
                if isinstance(val, list):
                    val = val[0] if val else ""
                if val:
                    subtrahend_values.append(val)

            # Get minuend (what to subtract from)
            minuend_giving = giving_stmt.subtractMinuendGiving()
            minuend = self.visit(minuend_giving)
            # Handle case where visit returns a list
            if isinstance(minuend, list):
                minuend = minuend[0] if minuend else "0"

            # Get GIVING targets
            giving_list = giving_stmt.subtractGiving()
            targets = []
            for giving in giving_list:
                target = self.visit(giving.identifier())
                if isinstance(target, list):
                    target = target[0] if target else ""
                if target:
                    targets.append(target)

            # Build expression: minuend - sub1 - sub2 - ...
            all_parts = [minuend] + [f"{val}" for val in subtrahend_values]
            expression = " - ".join(all_parts)

            # Generate assignment(s)
            lines = []
            for target in targets:
                lines.append(f"{indent}{target} = {expression}")

            return lines if len(lines) > 1 else (lines[0] if lines else None)

        elif ctx.subtractFromStatement():
            # FROM form: SUBTRACT A FROM B  →  b = b - a
            from_stmt = ctx.subtractFromStatement()

            # Get subtrahends
            subtrahend_list = from_stmt.subtractSubtrahend()
            subtrahend_values = []
            for subtrahend in subtrahend_list:
                val = self.visit(subtrahend)
                # Handle case where visit returns a list
                if isinstance(val, list):
                    val = val[0] if val else ""
                if val:
                    subtrahend_values.append(val)

            # Get minuends (targets)
            minuend_list = from_stmt.subtractMinuend()
            targets = []
            for minuend in minuend_list:
                target = self.visit(minuend.identifier())
                if isinstance(target, list):
                    target = target[0] if target else ""
                if target:
                    targets.append(target)

            # Generate assignments: target = target - sub1 - sub2 - ...
            lines = []
            for target in targets:
                # Check for zeroing pattern: SUBTRACT X FROM X
                if len(subtrahend_values) == 1 and subtrahend_values[0] == target:
                    # Optimize: x - x = 0
                    lines.append(f"{indent}{target} = 0")
                else:
                    # Normal: target = target - sub1 - sub2
                    all_parts = [target] + [f"{val}" for val in subtrahend_values]
                    expression = " - ".join(all_parts)
                    lines.append(f"{indent}{target} = {expression}")

            return lines if len(lines) > 1 else (lines[0] if lines else None)

        else:
            # CORRESPONDING form - not implemented yet
            return f"{indent}# SUBTRACT CORRESPONDING not supported yet"


class CobolConverter:
    """
    Main converter class for COBOL to Rules DSL conversion.
    """

    def __init__(self, mapping_csv_path: str = "mappings/sample_attribute_mappings.csv"):
        """
        Initialize converter with mapping service.

        Args:
            mapping_csv_path: Path to CSV file with attribute mappings
        """
        self.mapper = MappingService(mapping_csv_path)

    def _is_snippet(self, cobol_code: str) -> bool:
        """
        Detect if code is a snippet (not a complete program).

        Returns True if the code is missing required program divisions.
        Handles arbitrary code fragments from 2 lines to full paragraphs.
        """
        code_upper = cobol_code.strip().upper()

        # If it has IDENTIFICATION or PROGRAM-ID, it's a complete program
        if 'IDENTIFICATION DIVISION' in code_upper or 'PROGRAM-ID' in code_upper:
            return False

        # Otherwise, it's a snippet - could be:
        # - Just a few statements (MOVE X TO Y, IF X = Y THEN...)
        # - A paragraph without divisions
        # - Arbitrary procedural code fragment
        return True

    def _wrap_snippet(self, cobol_snippet: str) -> str:
        """
        Wrap a COBOL paragraph snippet in minimal program structure.

        Extracts variable references and creates synthetic DATA DIVISION.
        """
        # Create minimal COBOL program wrapper
        wrapped = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. SNIPPET.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
"""

        # Extract potential variable names from the snippet
        # Look for patterns like: MOVE X TO Y, IF X = Y, etc.
        import re

        # Find all potential COBOL variable names (uppercase words with hyphens)
        var_pattern = r'\b([A-Z][A-Z0-9\-]*)\b'
        potential_vars = set(re.findall(var_pattern, cobol_snippet.upper()))

        # Filter out COBOL keywords
        keywords = {'IF', 'THEN', 'ELSE', 'END-IF', 'PERFORM', 'MOVE', 'TO', 'READ', 'INTO',
                   'WRITE', 'FROM', 'DISPLAY', 'INITIALIZE', 'CONTINUE', 'EXIT', 'STOP', 'RUN',
                   'WHEN', 'OTHER', 'THRU', 'THROUGH', 'VARYING', 'UNTIL', 'BY'}

        # Also filter out file names (contain -FILE suffix) and paragraph names (start with digits)
        variables = {v for v in potential_vars
                    if v not in keywords
                    and not v.endswith('-FILE')
                    and not (v[0].isdigit() and '-' in v)}  # Filter paragraph names like 1000-SOMETHING

        # Add variable declarations
        for var in sorted(variables):
            # Use PIC X for most, 9 for numeric-looking ones
            if any(word in var for word in ['COUNTER', 'RESULT', 'STATUS', 'CODE']):
                wrapped += f"       01 {var:<18} PIC X(10).\n"
            else:
                wrapped += f"       01 {var:<18} PIC X(100).\n"

        # Add procedure division with the snippet
        wrapped += "\n       PROCEDURE DIVISION.\n"
        wrapped += cobol_snippet

        # Ensure it ends properly
        if not cobol_snippet.strip().endswith('.'):
            wrapped += "\n           ."
        if 'STOP RUN' not in cobol_snippet.upper():
            wrapped += "\n           STOP RUN.\n"

        return wrapped

    def convert(self, cobol_code: str, rule_name: str = "Generated Rule") -> tuple[str, ConversionMetadata]:
        """
        Convert COBOL code to Rules DSL.

        Automatically detects and wraps paragraph snippets in minimal program structure.

        Args:
            cobol_code: COBOL source code string (can be snippet or complete program)
            rule_name: Name for generated rule

        Returns:
            Tuple of (generated DSL code, conversion metadata)
        """
        try:
            # Detect and wrap snippets
            if self._is_snippet(cobol_code):
                original_snippet = cobol_code
                cobol_code = self._wrap_snippet(cobol_code)
                # Add metadata about snippet wrapping
                wrapped_note = "# ℹ️ Paragraph snippet auto-wrapped for parsing\n"
            else:
                wrapped_note = ""

            # Parse COBOL
            input_stream = InputStream(cobol_code)
            lexer = Cobol85Lexer(input_stream)
            token_stream = CommonTokenStream(lexer)
            parser = Cobol85Parser(token_stream)

            # Get parse tree
            tree = parser.startRule()

            # Visit tree to generate DSL
            visitor = CobolToDSLVisitor(self.mapper)
            dsl_statements = visitor.visit(tree)

            # Generate final DSL with metadata
            metadata_comments = visitor.metadata.get_metadata_comments()

            dsl_lines = []

            # Add snippet wrapping note if applicable
            if wrapped_note:
                dsl_lines.append(wrapped_note)

            # Add metadata comments
            if metadata_comments:
                dsl_lines.extend(metadata_comments)
                dsl_lines.append("")

            # Add rule structure
            dsl_lines.append(f'rule "{rule_name}":')

            # Add statements (filter out None values)
            if isinstance(dsl_statements, list):
                # Filter out None values and flatten nested lists
                for stmt in dsl_statements:
                    if stmt is not None:
                        if isinstance(stmt, list):
                            dsl_lines.extend([s for s in stmt if s is not None])
                        else:
                            dsl_lines.append(stmt)
            elif dsl_statements is not None:
                dsl_lines.append(dsl_statements)

            dsl_code = '\n'.join(dsl_lines)

            return dsl_code, visitor.metadata

        except Exception as e:
            # Error recovery: paste original code as comment
            error_msg = str(e)
            commented_cobol = '\n'.join(f'# {line}' for line in cobol_code.split('\n'))

            error_output = f"""# ⚠️ PARSING FAILED - Manual conversion required
# Error: {error_msg}
# Original COBOL code preserved below:
{commented_cobol}
"""

            metadata = ConversionMetadata()
            metadata.add_warning(f"Parsing failed: {error_msg}")

            return error_output, metadata

    def close(self):
        """Close mapper connection"""
        self.mapper.close()

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()


# CLI interface for VS Code extension integration
if __name__ == "__main__":
    import argparse
    import sys
    import json

    parser = argparse.ArgumentParser(description='Convert COBOL code to Rules DSL')
    parser.add_argument('--stdin', action='store_true', help='Read COBOL code from stdin')
    parser.add_argument('--rule-name', default='Converted Rule', help='Name for the generated rule')
    parser.add_argument('--file', help='Path to COBOL file (alternative to --stdin)')

    args = parser.parse_args()

    try:
        # Read COBOL code
        if args.stdin:
            cobol_code = sys.stdin.read()
        elif args.file:
            with open(args.file, 'r') as f:
                cobol_code = f.read()
        else:
            print(json.dumps({"error": "Either --stdin or --file must be specified"}))
            sys.exit(1)

        # Convert
        with CobolConverter() as converter:
            dsl_code, metadata = converter.convert(cobol_code, args.rule_name)

            # Output JSON result
            # Handle both object and dict mappings
            mappings = []
            for m in metadata.mappings:
                if isinstance(m, dict):
                    mappings.append(m)
                else:
                    mappings.append({
                        "cobol": m.cobol_var,
                        "target": m.target_var,
                        "type": m.mapping_type,
                        "confidence": m.confidence,
                        "comment": m.comment
                    })

            result = {
                "dsl": dsl_code,
                "metadata": {
                    "mappings": mappings,
                    "warnings": metadata.warnings if hasattr(metadata, 'warnings') else [],
                    "errors": []
                }
            }

            print(json.dumps(result, indent=2))

    except Exception as e:
        error_result = {
            "error": str(e),
            "dsl": f"# Error during conversion: {str(e)}",
            "metadata": {
                "mappings": [],
                "warnings": [],
                "errors": [str(e)]
            }
        }
        print(json.dumps(error_result, indent=2))
        sys.exit(1)
