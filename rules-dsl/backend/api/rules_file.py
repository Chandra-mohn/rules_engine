"""
Rules API Endpoints (File-Based)
CRUD operations for rules stored as JSON files in hierarchical folder structure
"""

from flask import Blueprint, request, jsonify
from services.rules_file_service import RulesFileService
from grammar_parser.template_code_generator import TemplateCodeGenerator
from config import Config
import math
import re
from pathlib import Path
from datetime import datetime

rules_file_bp = Blueprint('rules_file', __name__)

# Initialize file service and code generator
rule_file_service = RulesFileService()
code_generator = TemplateCodeGenerator()


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
        import re
        data = request.get_json()

        # Accept either ruleId or filePath (for VS Code extension integration)
        file_path = data.get('filePath', '')
        rule_content = data.get('ruleContent', '')
        rule_name = data.get('ruleName', '')

        if not rule_content:
            return jsonify({'error': 'ruleContent is required'}), 400

        # Helper function to remove spaces from path components
        def sanitize_name(name):
            """Remove spaces and special characters from names."""
            return re.sub(r'\s+', '', name)

        # Extract hierarchical structure from file path
        # Expected format: rules/{client}/{process_group}/{process_area}/{rulename}.rules
        # Or fallback: {rulename}.rules
        if file_path:
            path_parts = Path(file_path).parts

            # Try to extract hierarchy (skip 'rules' prefix if present)
            start_idx = 1 if len(path_parts) > 0 and path_parts[0] == 'rules' else 0

            if len(path_parts) >= start_idx + 4:
                # Full hierarchy: client/group/area/rulename.rules
                client_code = sanitize_name(path_parts[start_idx])
                process_group = sanitize_name(path_parts[start_idx + 1])
                process_area = sanitize_name(path_parts[start_idx + 2])
                rule_filename = Path(path_parts[start_idx + 3]).stem  # Remove .rules extension
                rule_name = sanitize_name(rule_filename)

                base_dir = Path(__file__).parent.parent.parent / 'generated-rules' / client_code / process_group / process_area / rule_name
            else:
                # Fallback: use just the rule name
                rule_filename = Path(file_path).stem if file_path else rule_name
                rule_name = sanitize_name(rule_filename) if rule_filename else 'DefaultRule'
                base_dir = Path(__file__).parent.parent.parent / 'generated-rules' / 'default' / rule_name
        else:
            # No file path provided, use rule name only
            rule_name = sanitize_name(rule_name) if rule_name else 'DefaultRule'
            base_dir = Path(__file__).parent.parent.parent / 'generated-rules' / 'default' / rule_name

        # Determine item type from content
        item_type = 'rule'  # Default
        if re.search(r'^\s*actionset\s+', rule_content, re.MULTILINE):
            item_type = 'actionset'
        elif re.search(r'^\s*action\s+', rule_content, re.MULTILINE):
            item_type = 'action'

        # Generate Java code using template code generator
        try:
            production_code, test_code = code_generator.generate_with_tests(rule_content, rule_name, item_type)

            # Create src/main/java directory for production code
            src_main_dir = base_dir / 'src' / 'main' / 'java' / 'com' / 'rules'
            src_main_dir.mkdir(parents=True, exist_ok=True)

            # Create src/test/java directory for test code
            src_test_dir = base_dir / 'src' / 'test' / 'java' / 'com' / 'rules'
            src_test_dir.mkdir(parents=True, exist_ok=True)

            # Extract class name from generated code (first public class declaration)
            class_match = re.search(r'public\s+class\s+(\w+)', production_code)
            if class_match:
                class_name = class_match.group(1)
            else:
                # Fallback: convert rule name to class name
                class_name = ''.join(word.capitalize() for word in rule_name.replace('_', ' ').replace('-', ' ').split())
                if not class_name.endswith('Rule'):
                    class_name += 'Rule'

            # Write main Java file
            java_file = src_main_dir / f"{class_name}.java"
            java_file.write_text(production_code)

            # Write test file
            test_file = src_test_dir / f"{class_name}Test.java"
            test_file.write_text(test_code)

            files = {
                str(java_file.relative_to(base_dir)): production_code,
                str(test_file.relative_to(base_dir)): test_code
            }

            return jsonify({
                'success': True,
                'files': files,
                'ruleName': rule_name,
                'packageName': 'com.rules',
                'outputDirectory': str(base_dir),
                'artifactCount': len(files)
            })
        except Exception as e:
            return jsonify({
                'success': False,
                'message': f'Code generation failed: {str(e)}',
                'errors': [str(e)]
            }), 500

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@rules_file_bp.route('/rules/gap-analysis', methods=['GET'])
def get_gap_analysis():
    """
    Gap analysis endpoint to identify missing actions and validate rules.
    Returns analysis of all rules with validation status and missing dependencies.
    """
    try:
        # Get pagination params
        page = int(request.args.get('page', 1))
        limit = int(request.args.get('limit', 20))
        filters = request.args.getlist('filter')  # Get multiple filter values

        # Get all rules
        all_rules = rule_file_service.list_rules()

        # Perform gap analysis on each rule
        analysis_results = []

        for rule in all_rules:
            # Extract actions and attributes from rule content
            content = rule.get('content', '')

            # Simple regex-based extraction (can be enhanced with proper parsing)
            actions_found = set(re.findall(r'\b([a-z][a-zA-Z0-9_]*)\s*\(', content))
            attributes_found = set(re.findall(r'\b(customer|applicant|transaction|account)\.[a-zA-Z0-9_]+', content))

            # For now, assume all found actions are valid (no missing actions)
            # In a real implementation, this would check against a known set of actions
            missing_actions = []

            # Determine validation status based on whether rule has been tested/compiled
            validation_status = 'valid' if rule.get('status') in ['VALID', 'PROD'] else 'error'

            analysis_results.append({
                'rule': {
                    'id': rule.get('id'),
                    'name': rule.get('name'),
                    'item_type': rule.get('item_type', 'rule'),
                    'status': rule.get('status', 'DRAFT'),
                    'content': rule.get('content', '')  # Frontend expects this for expanded row
                },
                'validation_status': validation_status,
                'validation_errors': [],  # Frontend expects this for expanded row
                'missing_actions': missing_actions,
                'extracted_attributes': list(attributes_found),  # Frontend expects this name
                'referenced_actions': list(actions_found),  # Frontend expects this name
                'referenced_actionsets': [],  # Frontend expects this field
                'analyzed_at': datetime.now().isoformat()
            })

        # Apply filters if provided
        if filters:
            filtered_results = []
            for result in analysis_results:
                # Filter by item type
                if 'rule' in filters and result['rule']['item_type'] == 'rule':
                    filtered_results.append(result)
                elif 'actionset' in filters and result['rule']['item_type'] == 'actionset':
                    filtered_results.append(result)
                elif 'action' in filters and result['rule']['item_type'] == 'action':
                    filtered_results.append(result)
                # Filter by status
                elif result['rule']['status'] in filters:
                    filtered_results.append(result)
                # Filter by validation status
                elif result['validation_status'] in filters:
                    filtered_results.append(result)
            analysis_results = filtered_results

        # Calculate pagination
        total = len(analysis_results)
        total_pages = math.ceil(total / limit)
        start_idx = (page - 1) * limit
        end_idx = start_idx + limit
        paginated_results = analysis_results[start_idx:end_idx]

        # Calculate comprehensive aggregate statistics
        type_distribution = {}
        status_distribution = {}
        all_actions = set()
        all_attributes = set()

        for result in analysis_results:
            # Count by type
            item_type = result['rule']['item_type']
            type_distribution[item_type] = type_distribution.get(item_type, 0) + 1

            # Count by status
            status = result['rule']['status']
            status_distribution[status] = status_distribution.get(status, 0) + 1

            # Collect all actions and attributes
            all_actions.update(result['referenced_actions'])
            all_attributes.update(result['extracted_attributes'])

        # Build aggregates structure matching frontend expectations
        aggregates = {
            'total_rules': len(all_rules),
            'type_distribution': {
                'rule': type_distribution.get('rule', 0),
                'actionset': type_distribution.get('actionset', 0),
                'mon_rule': type_distribution.get('mon_rule', 0),
                'non_mon_rule': type_distribution.get('non_mon_rule', 0)
            },
            'status_distribution': {
                'VALID': status_distribution.get('VALID', 0),
                'DRAFT': status_distribution.get('DRAFT', 0),
                'PROD': status_distribution.get('PROD', 0),
                'ERROR': status_distribution.get('ERROR', 0)
            },
            'action_analysis': {
                'unique_actions': len(all_actions),
                'missing_actions': sum(1 for r in analysis_results if len(r['missing_actions']) > 0),
                'most_referenced': []  # Frontend expects this field
            },
            'attribute_analysis': {
                'missing_attributes': 0,
                'most_common_attributes': []  # Frontend expects this field
            }
        }

        return jsonify({
            'success': True,
            'results': paginated_results,
            'pagination': {
                'page': page,
                'limit': limit,
                'total': total,
                'total_pages': total_pages
            },
            'aggregates': aggregates,
            'generated_at': datetime.now().isoformat()
        })

    except Exception as e:
        return jsonify({
            'success': False,
            'error': str(e)
        }), 500
