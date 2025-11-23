#!/usr/bin/env python3
"""
Standalone CLI for Java code generation from Rules DSL with Semantic Validation
Usage: python generate_code_cli.py <rule_file_path> [--validate-only]
"""

import sys
import json
from pathlib import Path
import re

# Add backend directory to path
sys.path.insert(0, str(Path(__file__).parent))

from grammar_parser.template_code_generator import TemplateCodeGenerator
from grammar_parser.rule_validator import RuleValidator
from services.schema_file_service import SchemaFileService
from services.context_file_service import ContextFileService


def sanitize_name(name):
    """Remove spaces and special characters from names."""
    return re.sub(r'\s+', '', name)


def load_validation_context():
    """Load schemas and contexts for validation."""
    backend_dir = Path(__file__).parent

    # Initialize services pointing to ui-prototype data (shared data)
    ui_prototype_backend = backend_dir.parent.parent / 'ui-prototype' / 'backend'

    schema_service = SchemaFileService(str(ui_prototype_backend / 'test_data' / 'schemas'))
    context_service = ContextFileService(str(ui_prototype_backend / 'test_data' / 'contexts'))

    # Load all schemas
    schemas = schema_service.list_schemas()

    # Extract available attributes from schemas
    available_attributes = set()
    for schema in schemas:
        schema_name = schema.get('name')
        for attr in schema.get('attributes', []):
            attr_name = attr.get('name')
            if schema_name and attr_name:
                available_attributes.add(f"{schema_name}.{attr_name}")

    # Extract available actions (from primitive actions list + contexts)
    available_actions = {
        'approveTransaction', 'declineTransaction', 'updateAccountBalance',
        'calculateRiskScore', 'logHighRiskApplicant', 'requireCoSigner',
        'scheduleManualReview', 'approveApplication', 'conditionalApproval',
        'rejectApplication', 'logTransaction', 'sendNotification',
        'requireManualReview', 'flagForReview', 'updateRiskScore',
        'instantApproval', 'immediateReject', 'done', 'validateApplicantInfo',
        'sendWelcomeEmail', 'sendRejectionLetter', 'requestDocumentation',
        'updateCustomerRecord', 'fastTrackApproval', 'assignPrivateBanker',
        'premiumApproval', 'schedulePersonalConsultation'
    }

    return {
        'available_actions': list(available_actions),
        'available_attributes': list(available_attributes)
    }


def validate_rule(rule_content, validation_context, strict_mode=False):
    """
    Validate rule semantically before code generation.

    Validation levels:
    - ERROR: Syntax errors (block generation)
    - WARNING: Undefined attributes (allow generation)
    - INFO: Undefined actions/actionsets (allow generation)

    Args:
        rule_content: Rule content to validate
        validation_context: Validation context with available actions/attributes
        strict_mode: If True, treat warnings/info as errors (for CI/CD)

    Returns:
        dict: Validation result with categorized issues
    """
    validator = RuleValidator()
    result = validator.validate_rule(rule_content, context=validation_context)

    # Categorize validation issues by severity
    syntax_errors = []
    semantic_warnings = []
    semantic_info = []

    # Separate syntax errors from semantic issues
    for err in result.get('errors', []):
        if err.get('type') in ['grammar', 'nesting_depth']:
            # Syntax/structural errors block generation
            syntax_errors.append({
                **err,
                'severity': 'error'
            })
        elif err.get('type') == 'undefined_action':
            # Undefined actions are INFO in normal mode
            semantic_info.append({
                **err,
                'severity': 'info',
                'message': f"Unknown action: '{err.get('action')}' (likely being developed)"
            })
        else:
            # Other semantic issues are warnings
            semantic_warnings.append({
                **err,
                'severity': 'warning'
            })

    # Attribute warnings stay as warnings
    for warn in result.get('warnings', []):
        if warn.get('type') == 'undefined_attribute':
            semantic_warnings.append({
                **warn,
                'severity': 'warning',
                'message': f"Unknown attribute: '{warn.get('attribute')}' (check schema)"
            })
        else:
            semantic_warnings.append({
                **warn,
                'severity': 'warning'
            })

    # In strict mode, all issues block generation
    if strict_mode:
        has_blocking_issues = len(syntax_errors) > 0 or len(semantic_warnings) > 0 or len(semantic_info) > 0
    else:
        # Normal mode: only syntax errors block
        has_blocking_issues = len(syntax_errors) > 0

    return {
        'valid': not has_blocking_issues,
        'syntax_valid': len(syntax_errors) == 0,
        'syntax_errors': syntax_errors,
        'warnings': semantic_warnings,
        'info': semantic_info,
        'undefined_actions': result.get('undefined_actions', []),
        'undefined_attributes': result.get('undefined_attributes', []),
        'strict_mode': strict_mode
    }


def generate_code_from_file(rule_file_path, validate_only=False, strict_mode=False):
    """
    Generate Java code from a .rules file with semantic validation.

    Args:
        rule_file_path: Path to .rules file
        validate_only: If True, only validate without generating code
        strict_mode: If True, warnings/info block generation (for CI/CD)

    Returns:
        dict: Result with success status, validation details, and generated files
    """

    rule_file = Path(rule_file_path)

    if not rule_file.exists():
        return {
            'success': False,
            'error': f'File not found: {rule_file_path}'
        }

    if not rule_file.suffix == '.rules':
        return {
            'success': False,
            'error': f'File must have .rules extension'
        }

    # Read rule content
    rule_content = rule_file.read_text()

    # Extract rule name from filename
    rule_name = rule_file.stem

    # Determine item type from content
    item_type = 'rule'  # Default
    if re.search(r'^\s*actionset\s+', rule_content, re.MULTILINE):
        item_type = 'actionset'
    elif re.search(r'^\s*action\s+', rule_content, re.MULTILINE):
        item_type = 'action'

    # Step 1: Semantic Validation
    try:
        validation_context = load_validation_context()
        validation_result = validate_rule(rule_content, validation_context, strict_mode=strict_mode)

        # If validation-only mode, return validation result
        if validate_only:
            return {
                'success': validation_result['valid'],
                'validation': validation_result,
                'mode': 'validate_only'
            }

        # Check if validation blocks generation
        if not validation_result['valid']:
            # Determine error type for helpful message
            if validation_result.get('syntax_errors'):
                error_msg = 'Syntax errors detected - fix syntax before generating code'
            elif strict_mode:
                error_msg = 'Validation failed in strict mode - resolve all warnings before generating code'
            else:
                error_msg = 'Validation failed'

            return {
                'success': False,
                'error': error_msg,
                'validation': validation_result
            }

        # Validation passed (or only has warnings/info)
        # Store validation result to include in output
        validation_summary = validation_result

    except Exception as e:
        # Validation system error - log but continue with code generation
        validation_summary = {
            'valid': True,
            'syntax_valid': True,
            'syntax_errors': [],
            'warnings': [],
            'info': [f"Validation system error: {str(e)}"],
            'system_error': str(e)
        }

    # Extract hierarchical structure from file path
    # Expected: rules/mon/{client}/{group}/{area}/{rulename}.rules
    path_parts = rule_file.parts

    # Find 'rules' directory index
    try:
        rules_idx = path_parts.index('rules')
    except ValueError:
        # Not in standard location, use simple structure
        client_code = 'default'
        process_group = 'default'
        process_area = 'default'
    else:
        # Extract hierarchy
        remaining_parts = path_parts[rules_idx + 1:]

        if len(remaining_parts) >= 5:  # mon/client/group/area/file.rules
            client_code = sanitize_name(remaining_parts[1])
            process_group = sanitize_name(remaining_parts[2])
            process_area = sanitize_name(remaining_parts[3])
        elif len(remaining_parts) >= 4:  # client/group/area/file.rules
            client_code = sanitize_name(remaining_parts[0])
            process_group = sanitize_name(remaining_parts[1])
            process_area = sanitize_name(remaining_parts[2])
        else:
            client_code = 'default'
            process_group = 'default'
            process_area = 'default'

    rule_name_sanitized = sanitize_name(rule_name)

    # Determine base directory for generated code
    backend_dir = Path(__file__).parent
    base_dir = backend_dir.parent / 'generated-rules' / client_code / process_group / process_area / rule_name_sanitized

    try:
        # Generate code
        code_generator = TemplateCodeGenerator()
        production_code, test_code = code_generator.generate_with_tests(rule_content, rule_name, item_type)

        # Create directories
        src_main_dir = base_dir / 'src' / 'main' / 'java' / 'com' / 'rules'
        src_main_dir.mkdir(parents=True, exist_ok=True)

        src_test_dir = base_dir / 'src' / 'test' / 'java' / 'com' / 'rules'
        src_test_dir.mkdir(parents=True, exist_ok=True)

        # Extract class name from generated code
        class_match = re.search(r'public\s+class\s+(\w+)', production_code)
        if class_match:
            class_name = class_match.group(1)
        else:
            # Fallback: convert rule name to class name
            class_name = ''.join(word.capitalize() for word in rule_name.replace('_', ' ').replace('-', ' ').split())
            if not class_name.endswith('Rule'):
                class_name += 'Rule'

        # Write files
        java_file = src_main_dir / f"{class_name}.java"
        java_file.write_text(production_code)

        test_file = src_test_dir / f"{class_name}Test.java"
        test_file.write_text(test_code)

        result = {
            'success': True,
            'ruleName': rule_name,
            'className': class_name,
            'outputDirectory': str(base_dir),
            'files': {
                'production': str(java_file),
                'test': str(test_file)
            },
            'artifactCount': 2
        }

        # Include validation summary
        if validation_summary:
            result['validation'] = validation_summary

            # Add developer note if there are warnings/info
            total_issues = len(validation_summary.get('warnings', [])) + len(validation_summary.get('info', []))
            if total_issues > 0:
                result['developer_note'] = (
                    f"Code generated successfully with {total_issues} validation issue(s). "
                    f"Review and implement missing elements before deployment."
                )

        return result

    except Exception as e:
        return {
            'success': False,
            'error': str(e),
            'traceback': str(e.__class__.__name__)
        }


def main():
    if len(sys.argv) < 2:
        print(json.dumps({
            'success': False,
            'error': 'Usage: python generate_code_cli.py <rule_file_path> [--validate-only] [--strict]'
        }))
        sys.exit(1)

    rule_file_path = sys.argv[1]
    validate_only = '--validate-only' in sys.argv
    strict_mode = '--strict' in sys.argv

    result = generate_code_from_file(rule_file_path, validate_only=validate_only, strict_mode=strict_mode)

    # Output JSON result
    print(json.dumps(result, indent=2))

    # Exit with appropriate code
    sys.exit(0 if result['success'] else 1)


if __name__ == '__main__':
    main()
