"""
Python ANTLR-based Rule Validator
Validates rules using ANTLR parsing and semantic analysis.
"""

import sys
from pathlib import Path
from typing import Dict, Any, List
from antlr4 import *

# Add the ANTLR generated files to the path
antlr_path = Path(__file__).parent.parent / "java-bridge" / "src" / "main" / "antlr4"
sys.path.insert(0, str(antlr_path))

from .rules_parser import RulesEngineParser, RuleInfoExtractor
from com.rules.grammar.RulesParser import RulesParser
from com.rules.grammar.RulesListener import RulesListener


class SemanticValidator(RulesListener):
    """Semantic validator that checks rule semantics beyond syntax."""

    def __init__(self, available_actions: List[str] = None, available_attributes: List[str] = None):
        self.available_actions = set(available_actions or [])
        self.available_attributes = set(available_attributes or [])
        self.errors = []
        self.warnings = []
        self.undefined_actions = set()
        self.undefined_attributes = set()

    def enterAction(self, ctx: RulesParser.ActionContext):
        """Validate action references."""
        action_name = None
        if ctx.IDENTIFIER():
            action_name = ctx.IDENTIFIER().getText()
        elif ctx.STRING():
            action_name = ctx.STRING().getText().strip('"')

        if action_name and self.available_actions:
            if action_name not in self.available_actions:
                self.undefined_actions.add(action_name)
                self.errors.append({
                    'type': 'undefined_action',
                    'message': f"Unknown action: '{action_name}'",
                    'action': action_name,
                    'line': ctx.start.line if ctx.start else 0,
                    'column': ctx.start.column if ctx.start else 0
                })

    def enterAttribute(self, ctx: RulesParser.AttributeContext):
        """Validate attribute references."""
        attr_parts = []
        for identifier in ctx.attributeIdentifier():
            if identifier.IDENTIFIER():
                attr_parts.append(identifier.IDENTIFIER().getText())
            elif identifier.STRING():
                attr_parts.append(identifier.STRING().getText().strip('"'))

        if attr_parts and self.available_attributes:
            attr_name = '.'.join(attr_parts)
            if attr_name not in self.available_attributes:
                self.undefined_attributes.add(attr_name)
                self.warnings.append({
                    'type': 'undefined_attribute',
                    'message': f"Unknown attribute: '{attr_name}'",
                    'attribute': attr_name,
                    'line': ctx.start.line if ctx.start else 0,
                    'column': ctx.start.column if ctx.start else 0
                })

    def enterComparison(self, ctx: RulesParser.ComparisonContext):
        """Validate comparison operations."""
        # Check for potential type mismatches
        # This is simplified - a full implementation would track types
        pass

    def enterRuleStep(self, ctx: RulesParser.RuleStepContext):
        """Validate rule step structure."""
        # Check for unreachable code, missing actions, etc.
        if ctx.IF() and not ctx.actionList():
            self.warnings.append({
                'type': 'empty_condition',
                'message': "Condition without any actions",
                'line': ctx.start.line if ctx.start else 0,
                'column': ctx.start.column if ctx.start else 0
            })


class RuleValidator:
    """Complete rule validator using Python ANTLR."""

    def __init__(self):
        self.parser = RulesEngineParser()

    def validate_rule(self, rule_content: str, context: Dict[str, Any] = None) -> Dict[str, Any]:
        """
        Comprehensive rule validation.

        Args:
            rule_content: Rule content to validate
            context: Validation context with available actions, attributes, etc.

        Returns:
            dict: Validation result
        """
        context = context or {}
        available_actions = context.get('available_actions', [])
        available_attributes = context.get('available_attributes', [])

        # Step 1: Syntax validation
        syntax_result = self.parser.validate_syntax(rule_content)

        if not syntax_result['valid']:
            return {
                'valid': False,
                'errors': syntax_result['errors'],
                'warnings': syntax_result['warnings'],
                'type': 'syntax_error'
            }

        # Step 2: Semantic validation
        semantic_validator = SemanticValidator(available_actions, available_attributes)

        if syntax_result['parse_tree']:
            walker = ParseTreeWalker()
            walker.walk(semantic_validator, syntax_result['parse_tree'])

        # Step 3: Extract rule information
        rule_info = self.parser.extract_rule_info(rule_content)

        # Combine all results
        all_errors = syntax_result['errors'] + semantic_validator.errors
        all_warnings = syntax_result['warnings'] + semantic_validator.warnings

        return {
            'valid': len(all_errors) == 0,
            'errors': all_errors,
            'warnings': all_warnings,
            'rule_info': rule_info,
            'undefined_actions': list(semantic_validator.undefined_actions),
            'undefined_attributes': list(semantic_validator.undefined_attributes),
            'parse_tree': syntax_result['parse_tree']
        }

    def validate_rule_parameters(self, rule_content: str, action_schemas: Dict[str, Any] = None) -> Dict[str, Any]:
        """
        Validate rule action parameters against schemas.

        Args:
            rule_content: Rule content to validate
            action_schemas: Dictionary of action schemas

        Returns:
            dict: Parameter validation result
        """
        action_schemas = action_schemas or {}

        # Parse the rule
        syntax_result = self.parser.validate_syntax(rule_content)
        if not syntax_result['valid']:
            return {
                'valid': False,
                'errors': ['Cannot validate parameters: syntax errors present'],
                'warnings': []
            }

        # Extract actions and their parameters
        param_validator = ParameterValidator(action_schemas)
        if syntax_result['parse_tree']:
            walker = ParseTreeWalker()
            walker.walk(param_validator, syntax_result['parse_tree'])

        return {
            'valid': len(param_validator.errors) == 0,
            'errors': param_validator.errors,
            'warnings': param_validator.warnings,
            'parameter_info': param_validator.parameter_info
        }

    def get_completion_context(self, rule_content: str, cursor_position: int) -> Dict[str, Any]:
        """
        Analyze context at cursor position for autocomplete.

        Args:
            rule_content: Rule content
            cursor_position: Character position of cursor

        Returns:
            dict: Context information for autocomplete
        """
        try:
            # Parse the rule
            tree, error_listener = self.parser.parse(rule_content)

            if tree is None:
                return {
                    'context_type': 'unknown',
                    'suggestions': [],
                    'errors': error_listener.errors
                }

            # Find context at cursor position
            context_finder = CursorContextFinder(cursor_position)
            walker = ParseTreeWalker()
            walker.walk(context_finder, tree)

            return {
                'context_type': context_finder.context_type,
                'in_condition': context_finder.in_condition,
                'in_action': context_finder.in_action,
                'current_entity': context_finder.current_entity,
                'suggestions': self._get_context_suggestions(context_finder),
                'errors': []
            }

        except Exception as e:
            return {
                'context_type': 'error',
                'suggestions': [],
                'errors': [str(e)]
            }

    def _get_context_suggestions(self, context_finder) -> List[str]:
        """Generate suggestions based on context."""
        suggestions = []

        if context_finder.in_condition:
            if context_finder.current_entity:
                # Suggest attributes for the current entity
                suggestions.extend([
                    f"{context_finder.current_entity}.field1",
                    f"{context_finder.current_entity}.field2"
                ])
            else:
                # Suggest entities and operators
                suggestions.extend(['applicant', 'transaction', 'account', 'if', 'then', 'else'])

        elif context_finder.in_action:
            # Suggest available actions
            suggestions.extend([
                'approveApplication',
                'rejectApplication',
                'requestDocumentation',
                'scheduleReview'
            ])

        return suggestions


class ParameterValidator(RulesListener):
    """Validates action parameters against schemas."""

    def __init__(self, action_schemas: Dict[str, Any]):
        self.action_schemas = action_schemas
        self.errors = []
        self.warnings = []
        self.parameter_info = []

    def enterAction(self, ctx: RulesParser.ActionContext):
        """Validate action parameters."""
        action_name = None
        if ctx.IDENTIFIER():
            action_name = ctx.IDENTIFIER().getText()
        elif ctx.STRING():
            action_name = ctx.STRING().getText().strip('"')

        if action_name and ctx.parameterList():
            schema = self.action_schemas.get(action_name, {})
            expected_params = schema.get('parameters', [])

            # Count actual parameters
            actual_param_count = len(ctx.parameterList().parameter())
            expected_param_count = len([p for p in expected_params if p.get('required', False)])

            if actual_param_count < expected_param_count:
                self.errors.append(f"Action '{action_name}' requires {expected_param_count} parameters, got {actual_param_count}")

            # Record parameter info
            self.parameter_info.append({
                'action': action_name,
                'expected_params': expected_params,
                'actual_param_count': actual_param_count
            })


class CursorContextFinder(RulesListener):
    """Finds the parsing context at a specific cursor position."""

    def __init__(self, cursor_position: int):
        self.cursor_position = cursor_position
        self.context_type = 'unknown'
        self.in_condition = False
        self.in_action = False
        self.current_entity = None

    def enterRuleStep(self, ctx: RulesParser.RuleStepContext):
        """Determine if cursor is in condition or action context."""
        # print(f"Entering RuleStep, cursor at {self.cursor_position}")
        if self._contains_position(ctx):
            if ctx.IF() and ctx.condition():
                condition_start = ctx.condition().start.start
                condition_stop = ctx.condition().stop.stop
                if condition_start <= self.cursor_position <= condition_stop:
                    self.in_condition = True
                    self.context_type = 'condition'

            if ctx.actionList():
                for action_list in ctx.actionList():
                    action_start = action_list.start.start
                    action_stop = action_list.stop.stop
                    if action_start <= self.cursor_position <= action_stop:
                        self.in_action = True
                        self.context_type = 'action'

    def enterAttribute(self, ctx: RulesParser.AttributeContext):
        """Extract current entity context."""
        if self._contains_position(ctx):
            if ctx.attributeIdentifier() and len(ctx.attributeIdentifier()) > 0:
                first_identifier = ctx.attributeIdentifier()[0]
                if first_identifier.IDENTIFIER():
                    self.current_entity = first_identifier.IDENTIFIER().getText()

    def _contains_position(self, ctx) -> bool:
        """Check if context contains the cursor position."""
        if not ctx.start or not ctx.stop:
            return False

        # Use character positions (start.start and stop.stop are character indices)
        start_pos = ctx.start.start
        stop_pos = ctx.stop.stop

        # Debug logging
        # print(f"Context check: {start_pos} <= {self.cursor_position} <= {stop_pos} = {start_pos <= self.cursor_position <= stop_pos}")

        return start_pos <= self.cursor_position <= stop_pos