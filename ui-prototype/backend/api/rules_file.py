"""
Rules API Endpoints (File-Based)
CRUD operations for rules stored as JSON files in hierarchical folder structure
"""

from flask import Blueprint, request, jsonify
from services.rule_file_service import RuleFileService
from services.python_rules_engine import PythonRulesEngine
from services.cycle_detector import RuleCycleDetector
from config import Config
import math
import re
from pathlib import Path
from datetime import datetime

rules_file_bp = Blueprint('rules_file', __name__)

# Initialize file service and rules engine
rule_file_service = RuleFileService()
rules_engine = PythonRulesEngine()
cycle_detector = RuleCycleDetector(rule_file_service)


def parse_rule_name_from_content(content: str) -> str:
    """
    Parse rule name from content.
    Supports both quoted and unquoted identifiers.
    """
    if not content:
        return ''

    pattern = r'^\s*(?:rule|actionset)\s+(?:"([^"]+)"|([a-zA-Z_][a-zA-Z0-9_]*))\s*:'
    match = re.search(pattern, content, re.MULTILINE)
    if match:
        return match.group(1) or match.group(2) or ''

    return ''


def parse_item_type_from_content(content: str) -> str:
    """
    Parse item type from content (rule, actionset, action, etc.).
    """
    if not content:
        return 'rule'

    # Check for actionset keyword
    if re.search(r'^\s*actionset\s+', content, re.MULTILINE):
        return 'actionset'

    # Check for action keyword
    if re.search(r'^\s*action\s+', content, re.MULTILINE):
        return 'action'

    # Default to rule
    return 'rule'


def get_next_rule_id() -> int:
    """Generate next available rule ID by scanning all existing rules."""
    max_id = 0
    rules_path = Path(__file__).parent.parent / 'rules'

    if rules_path.exists():
        for rule_file in rules_path.rglob('rule-*.json'):
            try:
                # Extract ID from filename: rule-123.json -> 123
                rule_id = int(rule_file.stem.split('-')[1])
                max_id = max(max_id, rule_id)
            except (ValueError, IndexError):
                continue

    return max_id + 1


@rules_file_bp.route('/rules', methods=['GET'])
def get_rules():
    """Get paginated list of rules with optional filtering."""
    try:
        # Parse query parameters
        page = request.args.get('page', 1, type=int)
        limit = min(request.args.get('limit', Config.DEFAULT_PAGE_SIZE, type=int), Config.MAX_PAGE_SIZE)
        status = request.args.get('status')
        search = request.args.get('search')
        item_type = request.args.get('item_type')
        client_code = request.args.get('client_code')
        process_group_code = request.args.get('process_group_code')
        process_area_code = request.args.get('process_area_code')

        # Get all rules from file service
        all_rules = rule_file_service.list_rules(
            client_code=client_code,
            process_group_code=process_group_code,
            process_area_code=process_area_code
        )

        # Apply filters
        filtered_rules = all_rules

        if status:
            filtered_rules = [r for r in filtered_rules if r.get('status') == status]

        if item_type:
            filtered_rules = [r for r in filtered_rules if r.get('item_type') == item_type]

        if search:
            search_lower = search.lower()
            filtered_rules = [
                r for r in filtered_rules
                if search_lower in r.get('name', '').lower() or
                   search_lower in r.get('description', '').lower()
            ]

        # Sort by name
        filtered_rules.sort(key=lambda r: r.get('name', ''))

        # Paginate
        total = len(filtered_rules)
        offset = (page - 1) * limit
        paginated_rules = filtered_rules[offset:offset + limit]

        # Calculate pagination info
        pages = math.ceil(total / limit) if total > 0 else 1

        return jsonify({
            'rules': paginated_rules,
            'total': total,
            'page': page,
            'pages': pages,
            'limit': limit
        })

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/<int:rule_id>', methods=['GET'])
def get_rule(rule_id):
    """Get a specific rule by ID (searches all hierarchy)."""
    try:
        # Search for rule across all hierarchy
        rules_path = Path(__file__).parent.parent / 'rules'

        for rule_file in rules_path.rglob(f'rule-{rule_id}.json'):
            import json
            with open(rule_file, 'r') as f:
                rule_data = json.load(f)

            rule_data['hierarchy'] = rule_file_service._derive_hierarchy_from_path(rule_file)
            return jsonify(rule_data)

        return jsonify({'error': 'Rule not found'}), 404

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules', methods=['POST'])
def create_rule():
    """Create a new rule."""
    try:
        import json
        data = request.get_json()

        # Validate required fields
        if not data or 'content' not in data:
            return jsonify({'error': 'Content is required'}), 400

        # Process area can be provided as code (new way) or id (legacy)
        process_area_code = data.get('process_area_code') or data.get('process_area_id')
        if not process_area_code:
            return jsonify({'error': 'Process area is required'}), 400

        # Parse rule name from content
        rule_name = parse_rule_name_from_content(data['content'])
        if not rule_name:
            return jsonify({'error': 'Could not parse rule name from content'}), 400

        # Parse item type from content
        item_type = parse_item_type_from_content(data['content'])

        # Validate rule syntax
        validation_result = rules_engine.validate_rule(data['content'])
        if not validation_result.get('valid'):
            return jsonify({
                'error': 'Rule validation failed',
                'validation': validation_result
            }), 400

        # Check for cyclic dependencies
        cycle_result = cycle_detector.detect_cycles(rule_name, data['content'])
        if cycle_result['has_cycle']:
            return jsonify({
                'error': 'Cyclic dependency detected',
                'cycle_path': ' → '.join(cycle_result['cycle_path']),
                'message': f"Rule call cycle detected: {' → '.join(cycle_result['cycle_path'])}",
                'dependencies': cycle_result['dependencies']
            }), 400

        # Get hierarchy info for the process area by finding directory structure
        rules_path = Path(__file__).parent.parent / 'rules'
        found_hierarchy = None

        # Search for directory matching process_area_code
        for pa_dir in rules_path.rglob('*'):
            if pa_dir.is_dir() and pa_dir.name == process_area_code:
                # Extract hierarchy from path structure: rules/client/process_group/process_area
                parts = pa_dir.relative_to(rules_path).parts
                if len(parts) == 3:
                    found_hierarchy = {
                        'client_code': parts[0],
                        'process_group_code': parts[1],
                        'process_area_code': parts[2]
                    }
                    break

        if not found_hierarchy:
            return jsonify({'error': f'Process area {process_area_code} not found'}), 404

        # Generate new rule ID
        new_rule_id = get_next_rule_id()

        # Create rule data structure
        rule_data = {
            'id': new_rule_id,
            'name': rule_name,
            'description': data.get('description', ''),
            'content': data['content'],
            'status': 'VALID' if validation_result.get('valid') else 'DRAFT',
            'item_type': item_type,
            'schema_version': data.get('schema_version', 'modern'),
            'version': 1,
            'created_at': datetime.utcnow().isoformat(),
            'updated_at': datetime.utcnow().isoformat(),
            'hierarchy': found_hierarchy
        }

        # Save rule using file service
        result = rule_file_service.save_rule(rule_data)

        if result['success']:
            rule_data['validation'] = validation_result
            return jsonify(rule_data), 201
        else:
            return jsonify({'error': 'Failed to save rule'}), 500

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/<int:rule_id>', methods=['PUT'])
def update_rule(rule_id):
    """Update an existing rule."""
    try:
        import json
        data = request.get_json()
        if not data:
            return jsonify({'error': 'Request body is required'}), 400

        # Find existing rule
        rules_path = Path(__file__).parent.parent / 'rules'
        existing_rule_file = None
        existing_rule_data = None

        for rule_file in rules_path.rglob(f'rule-{rule_id}.json'):
            existing_rule_file = rule_file
            with open(rule_file, 'r') as f:
                existing_rule_data = json.load(f)

            # Derive hierarchy from file path
            existing_rule_data['hierarchy'] = rule_file_service._derive_hierarchy_from_path(rule_file)
            break

        if not existing_rule_data:
            return jsonify({'error': 'Rule not found'}), 404

        # Update fields
        if 'content' in data:
            existing_rule_data['content'] = data['content']

            # Parse new rule name
            rule_name = parse_rule_name_from_content(data['content'])
            if rule_name:
                existing_rule_data['name'] = rule_name

            # Only update item_type if explicitly provided, otherwise preserve existing special types (non_mon_rule, mon_rule, etc.)
            if 'item_type' in data:
                existing_rule_data['item_type'] = data['item_type']

            # Validate
            validation_result = rules_engine.validate_rule(data['content'])
            if validation_result.get('valid'):
                existing_rule_data['status'] = 'VALID'

            # Check for cyclic dependencies
            cycle_result = cycle_detector.detect_cycles(rule_name, data['content'])
            if cycle_result['has_cycle']:
                return jsonify({
                    'error': 'Cyclic dependency detected',
                    'cycle_path': ' → '.join(cycle_result['cycle_path']),
                    'message': f"Rule call cycle detected: {' → '.join(cycle_result['cycle_path'])}",
                    'dependencies': cycle_result['dependencies']
                }), 400

        if 'description' in data:
            existing_rule_data['description'] = data['description']

        if 'status' in data:
            existing_rule_data['status'] = data['status']

        if 'schema_version' in data:
            existing_rule_data['schema_version'] = data['schema_version']

        if 'context_id' in data:
            existing_rule_data['context_id'] = data['context_id']

        if 'process_area_id' in data:
            # Update process_area in hierarchy
            if 'hierarchy' not in existing_rule_data:
                existing_rule_data['hierarchy'] = {}
            existing_rule_data['hierarchy']['process_area_code'] = data['process_area_id']

        # Update timestamp and version
        existing_rule_data['updated_at'] = datetime.utcnow().isoformat()
        existing_rule_data['version'] = existing_rule_data.get('version', 1) + 1

        # Save updated rule
        result = rule_file_service.save_rule(existing_rule_data)

        if result['success']:
            if 'content' in data:
                existing_rule_data['validation'] = validation_result
            return jsonify(existing_rule_data)
        else:
            return jsonify({'error': 'Failed to update rule'}), 500

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/<int:rule_id>', methods=['DELETE'])
def delete_rule(rule_id):
    """Delete a rule."""
    try:
        # Find and delete rule
        rules_path = Path(__file__).parent.parent / 'rules'

        for rule_file in rules_path.rglob(f'rule-{rule_id}.json'):
            import json
            with open(rule_file, 'r') as f:
                rule_data = json.load(f)

            h = rule_data.get('hierarchy', {})
            result = rule_file_service.delete_rule(
                h['client_code'],
                h['process_group_code'],
                h['process_area_code'],
                rule_id
            )

            if result['success']:
                return jsonify({'message': 'Rule deleted successfully'})
            else:
                return jsonify({'error': 'Failed to delete rule'}), 500

        return jsonify({'error': 'Rule not found'}), 404

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/validate', methods=['POST'])
def validate_rule():
    """Validate rule syntax without saving."""
    try:
        data = request.get_json()
        if not data or 'content' not in data:
            return jsonify({'error': 'Content is required'}), 400

        validation_result = rules_engine.validate_rule(data['content'])
        return jsonify(validation_result)

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/test', methods=['POST'])
def test_rule_content():
    """Test rule execution with sample data without requiring a saved rule."""
    try:
        data = request.get_json()
        if not data or 'rule_content' not in data or 'test_data' not in data:
            return jsonify({'error': 'Rule content and test data are required'}), 400

        # Test rule with provided content and data
        test_result = rules_engine.execute(data['rule_content'], data['test_data'])
        return jsonify(test_result)

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/<int:rule_id>/test', methods=['POST'])
def test_rule(rule_id):
    """Test rule execution with sample data."""
    try:
        import json
        data = request.get_json()
        if not data or 'test_data' not in data:
            return jsonify({'error': 'Test data is required'}), 400

        # Get rule content
        rule_content = data.get('rule_content')
        if not rule_content:
            # Find rule file
            rules_path = Path(__file__).parent.parent / 'rules'
            for rule_file in rules_path.rglob(f'rule-{rule_id}.json'):
                with open(rule_file, 'r') as f:
                    rule_data = json.load(f)
                rule_content = rule_data['content']
                break

            if not rule_content:
                return jsonify({'error': 'Rule not found'}), 404

        # Test rule
        test_result = rules_engine.execute(rule_content, data['test_data'])
        return jsonify(test_result)

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/suggestions/complete', methods=['GET'])
def get_complete_suggestions():
    """Get all autocomplete suggestions for client-side caching."""
    try:
        # Import schema data
        from schema.rules_schema import (
            get_all_attributes, get_all_actions, get_all_functions,
            get_attributes_by_entity, KEYWORDS, OPERATORS, TIME_UNITS
        )

        # Get all schema data
        all_attributes = get_all_attributes()
        all_actions = get_all_actions()
        all_functions = get_all_functions()

        # Organize attributes by entity for context-aware suggestions
        attributes_by_entity = {
            'applicant': get_attributes_by_entity('applicant'),
            'transaction': get_attributes_by_entity('transaction'),
            'account': get_attributes_by_entity('account')
        }

        # Return comprehensive suggestions structure
        suggestions = {
            'attributes': {
                'all': all_attributes,
                'by_entity': attributes_by_entity
            },
            'actions': {
                'all': all_actions
            },
            'functions': {
                'all': all_functions,
                'by_category': {
                    'datetime': [f for f in all_functions if any(kw in f.get('label', '').lower()
                                 for kw in ['date', 'time', 'year', 'month', 'day'])]
                }
            },
            'keywords': KEYWORDS,
            'operators': OPERATORS,
            'time_units': TIME_UNITS,
            'metadata': {
                'version': '1.0',
                'total_count': len(all_attributes) + len(all_actions) + len(all_functions)
            }
        }

        return jsonify(suggestions)

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/<int:rule_id>/promote', methods=['POST'])
def promote_rule_status(rule_id):
    """Promote rule to next status or specific status."""
    try:
        import json
        data = request.get_json() or {}
        target_status = data.get('target_status')

        if not target_status:
            return jsonify({'error': 'target_status is required'}), 400

        # Validate target status
        valid_statuses = ['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD']
        if target_status not in valid_statuses:
            return jsonify({'error': f'Invalid status. Must be one of: {valid_statuses}'}), 400

        # Find and update rule
        rules_path = Path(__file__).parent.parent / 'rules'

        for rule_file in rules_path.rglob(f'rule-{rule_id}.json'):
            with open(rule_file, 'r') as f:
                rule_data = json.load(f)

            current_status = rule_data['status']

            # Validate transition
            valid_transitions = {
                'DRAFT': ['VALID'],
                'VALID': ['PEND', 'DRAFT'],
                'PEND': ['SCHD', 'VALID'],
                'SCHD': ['PROD', 'VALID'],
                'PROD': []
            }

            if target_status not in valid_transitions.get(current_status, []):
                return jsonify({
                    'error': f'Invalid status transition from {current_status} to {target_status}'
                }), 400

            # Update status
            rule_data['status'] = target_status
            rule_data['updated_at'] = datetime.utcnow().isoformat()

            # Save
            result = rule_file_service.save_rule(rule_data)

            if result['success']:
                return jsonify({
                    'message': f'Rule status updated from {current_status} to {target_status}',
                    'rule': rule_data
                })
            else:
                return jsonify({'error': 'Failed to update status'}), 500

        return jsonify({'error': 'Rule not found'}), 404

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/generate-production', methods=['POST'])
def generate_production_code():
    """Generate production-ready Java code for a rule."""
    try:
        import json
        import os
        data = request.get_json()

        if not data or 'ruleId' not in data:
            return jsonify({'error': 'ruleId is required'}), 400

        rule_id = data['ruleId']
        rule_content = data.get('ruleContent', '')
        rule_name = data.get('ruleName', f'rule{rule_id}')

        if not rule_content:
            return jsonify({'error': 'ruleContent is required'}), 400

        # Generate Java code using Python rules engine
        result = rules_engine.compile_rule(rule_content, str(rule_id))

        if result.get('success'):
            # Write generated files to disk using Maven standard directory structure
            base_dir = Path(__file__).parent.parent.parent / 'generated-rules' / f'rule-{rule_id}'

            # Create src/main/java directory for production code
            src_main_dir = base_dir / 'src' / 'main' / 'java' / 'com' / 'rules'
            src_main_dir.mkdir(parents=True, exist_ok=True)

            # Create src/test/java directory for test code
            src_test_dir = base_dir / 'src' / 'test' / 'java' / 'com' / 'rules'
            src_test_dir.mkdir(parents=True, exist_ok=True)

            # Write main Java file
            class_name = result['className'].split('.')[-1]
            java_file = src_main_dir / f"{class_name}.java"
            java_file.write_text(result['java_code'])

            # Write test file
            test_file = src_test_dir / f"{class_name}Test.java"
            test_file.write_text(result['test_code'])

            files = {
                str(java_file.relative_to(base_dir)): result['java_code'],
                str(test_file.relative_to(base_dir)): result['test_code']
            }

            return jsonify({
                'success': True,
                'files': files,
                'ruleId': rule_id,
                'ruleName': rule_name,
                'packageName': '.'.join(result['className'].split('.')[:-1]),
                'outputDirectory': str(base_dir),
                'artifactCount': len(files)
            })
        else:
            return jsonify({
                'success': False,
                'message': result.get('message', 'Code generation failed'),
                'errors': result.get('errors', [])
            }), 500

    except Exception as e:
        return jsonify({'error': str(e)}), 500
