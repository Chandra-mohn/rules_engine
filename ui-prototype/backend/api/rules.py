from flask import Blueprint, request, jsonify
from marshmallow import ValidationError
from models import Rule, RuleSchema
from services.rule_service import RuleService
from config import Config
import math

rules_bp = Blueprint('rules', __name__)
rule_service = RuleService()
rule_schema = RuleSchema()
rules_schema = RuleSchema(many=True)

@rules_bp.route('/rules', methods=['GET'])
def get_rules():
    """Get paginated list of rules with optional filtering."""
    try:
        # Parse query parameters
        page = request.args.get('page', 1, type=int)
        limit = min(request.args.get('limit', Config.DEFAULT_PAGE_SIZE, type=int), Config.MAX_PAGE_SIZE)
        status = request.args.get('status')
        search = request.args.get('search')
        
        # Get rules from service
        rules, total = rule_service.get_rules(page=page, limit=limit, status=status, search=search)
        
        # Calculate pagination info
        pages = math.ceil(total / limit) if total > 0 else 1
        
        return jsonify({
            'rules': [rule.to_dict() for rule in rules],
            'total': total,
            'page': page,
            'pages': pages,
            'limit': limit
        })
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules/<int:rule_id>', methods=['GET'])
def get_rule(rule_id):
    """Get a specific rule by ID."""
    try:
        rule = rule_service.get_rule_by_id(rule_id)
        if not rule:
            return jsonify({'error': 'Rule not found'}), 404
        
        return jsonify(rule.to_dict())
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules', methods=['POST'])
def create_rule():
    """Create a new rule."""
    try:
        data = request.get_json()
        
        # Validate required fields
        if not data or 'name' not in data or 'content' not in data:
            return jsonify({'error': 'Name and content are required'}), 400
        
        # Check if rule name already exists
        existing_rule = rule_service.get_rule_by_name(data['name'])
        if existing_rule:
            return jsonify({'error': 'Rule name already exists'}), 409
        
        # Create rule
        created_by = request.headers.get('X-User-ID', 'system')
        rule, validation_result = rule_service.create_rule(data, created_by)
        
        response_data = rule.to_dict()
        response_data['validation'] = validation_result
        
        return jsonify(response_data), 201
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules/<int:rule_id>', methods=['PUT'])
def update_rule(rule_id):
    """Update an existing rule."""
    try:
        data = request.get_json()
        if not data:
            return jsonify({'error': 'Request body is required'}), 400
        
        # Check if new name conflicts with existing rule
        if 'name' in data:
            existing_rule = rule_service.get_rule_by_name(data['name'])
            if existing_rule and existing_rule.id != rule_id:
                return jsonify({'error': 'Rule name already exists'}), 409
        
        # Update rule
        updated_by = request.headers.get('X-User-ID', 'system')
        rule, validation_result = rule_service.update_rule(rule_id, data, updated_by)
        
        if not rule:
            return jsonify({'error': 'Rule not found'}), 404
        
        response_data = rule.to_dict()
        response_data['validation'] = validation_result
        
        return jsonify(response_data)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules/<int:rule_id>', methods=['DELETE'])
def delete_rule(rule_id):
    """Delete a rule."""
    try:
        deleted_by = request.headers.get('X-User-ID', 'system')
        success = rule_service.delete_rule(rule_id, deleted_by)
        
        if not success:
            return jsonify({'error': 'Rule not found'}), 404
        
        return jsonify({'message': 'Rule deleted successfully'})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules/validate', methods=['POST'])
def validate_rule():
    """Validate rule syntax without saving."""
    try:
        data = request.get_json()
        if not data or 'content' not in data:
            return jsonify({'error': 'Content is required'}), 400
        
        validation_result = rule_service.validate_rule_content(data['content'])
        return jsonify(validation_result)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules/test', methods=['POST'])
def test_rule_content():
    """Test rule execution with sample data without requiring a saved rule."""
    try:
        data = request.get_json()
        if not data or 'rule_content' not in data or 'test_data' not in data:
            return jsonify({'error': 'Rule content and test data are required'}), 400
        
        # Test rule with provided content and data
        test_result = rule_service.test_rule(data['rule_content'], data['test_data'])
        return jsonify(test_result)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules/<int:rule_id>/test', methods=['POST'])
def test_rule(rule_id):
    """Test rule execution with sample data."""
    try:
        data = request.get_json()
        if not data or 'test_data' not in data:
            return jsonify({'error': 'Test data is required'}), 400
        
        # Get rule content
        rule_content = data.get('rule_content')
        if not rule_content:
            rule = rule_service.get_rule_by_id(rule_id)
            if not rule:
                return jsonify({'error': 'Rule not found'}), 404
            rule_content = rule.content
        
        # Test rule
        test_result = rule_service.test_rule(rule_content, data['test_data'])
        return jsonify(test_result)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules/autocomplete', methods=['POST'])
def get_autocomplete():
    """Get autocomplete suggestions."""
    try:
        data = request.get_json()
        if not data or 'context' not in data:
            return jsonify({'error': 'Context is required'}), 400
        
        context = data['context']
        position = data.get('position', len(context))
        
        suggestions = rule_service.get_autocomplete_suggestions(context, position)
        return jsonify(suggestions)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules/<int:rule_id>/history', methods=['GET'])
def get_rule_history(rule_id):
    """Get rule change history."""
    try:
        history = rule_service.get_rule_history(rule_id)
        return jsonify([entry.to_dict() for entry in history])
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@rules_bp.route('/rules/<int:rule_id>/revert/<int:version>', methods=['POST'])
def revert_rule(rule_id, version):
    """Revert rule to a previous version."""
    try:
        reverted_by = request.headers.get('X-User-ID', 'system')
        rule, validation_result = rule_service.revert_rule(rule_id, version, reverted_by)
        
        if not rule:
            return jsonify({'error': 'Rule or version not found'}), 404
        
        response_data = rule.to_dict()
        response_data['validation'] = validation_result
        
        return jsonify(response_data)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500