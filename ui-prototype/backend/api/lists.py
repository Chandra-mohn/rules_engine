from flask import Blueprint, request, jsonify
from marshmallow import ValidationError
from services.list_cache import ListService

lists_bp = Blueprint('lists', __name__)
list_service = ListService()

@lists_bp.route('/lists', methods=['GET'])
def get_lists():
    """Get all named lists with optional schema filtering."""
    try:
        schema_version = request.args.get('schema_version', 'both')
        lists = list_service.get_all_lists(schema_version)
        
        return jsonify({
            'lists': lists,
            'total': len(lists)
        })
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@lists_bp.route('/lists/<list_name>', methods=['GET'])
def get_list(list_name):
    """Get a specific named list by name."""
    try:
        lists = list_service.get_all_lists()
        
        if list_name not in lists:
            return jsonify({'error': 'List not found'}), 404
        
        return jsonify({
            'list': lists[list_name]
        })
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@lists_bp.route('/lists', methods=['POST'])
def create_list():
    """Create a new named list."""
    try:
        data = request.get_json()
        if not data:
            return jsonify({'error': 'Request body is required'}), 400
        
        # Validate required fields
        required_fields = ['name', 'data_type', 'values']
        for field in required_fields:
            if field not in data:
                return jsonify({'error': f'Missing required field: {field}'}), 400
        
        # Validate data types
        valid_types = ['string', 'number', 'boolean']
        if data['data_type'] not in valid_types:
            return jsonify({'error': f'Invalid data_type. Must be one of: {valid_types}'}), 400
        
        if not isinstance(data['values'], list):
            return jsonify({'error': 'values must be a list'}), 400
        
        # Check if list name already exists
        existing_lists = list_service.get_all_lists()
        if data['name'] in existing_lists:
            return jsonify({'error': 'List name already exists'}), 409
        
        # Create the list
        created_by = request.headers.get('X-User-ID', 'system')
        result = list_service.create_list(data, created_by)

        return jsonify(result), 201
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@lists_bp.route('/lists/<int:list_id>', methods=['PUT'])
def update_list(list_id):
    """Update an existing named list."""
    try:
        data = request.get_json()
        if not data:
            return jsonify({'error': 'Request body is required'}), 400
        
        # Validate data type if provided
        if 'data_type' in data:
            valid_types = ['string', 'number', 'boolean']
            if data['data_type'] not in valid_types:
                return jsonify({'error': f'Invalid data_type. Must be one of: {valid_types}'}), 400
        
        # Validate values if provided
        if 'values' in data and not isinstance(data['values'], list):
            return jsonify({'error': 'values must be a list'}), 400
        
        # Update the list
        updated_by = request.headers.get('X-User-ID', 'system')
        result = list_service.update_list(list_id, data, updated_by)

        if not result:
            return jsonify({'error': 'List not found'}), 404

        return jsonify(result)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@lists_bp.route('/lists/<int:list_id>', methods=['DELETE'])
def delete_list(list_id):
    """Delete a named list."""
    try:
        deleted_by = request.headers.get('X-User-ID', 'system')
        success = list_service.delete_list(list_id)
        
        if not success:
            return jsonify({'error': 'List not found'}), 404
        
        return jsonify({'message': 'List deleted successfully'})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@lists_bp.route('/lists/cache/refresh', methods=['POST'])
def refresh_cache():
    """Force refresh of the list cache."""
    try:
        list_service.cache.refresh()
        stats = list_service.cache.stats()
        
        return jsonify({
            'message': 'Cache refreshed successfully',
            'stats': stats
        })
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@lists_bp.route('/lists/cache/stats', methods=['GET'])
def get_cache_stats():
    """Get cache statistics."""
    try:
        stats = list_service.cache.stats()
        return jsonify(stats)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@lists_bp.route('/lists/resolve', methods=['POST'])
def resolve_rule():
    """Resolve named lists in rule content."""
    try:
        data = request.get_json()
        if not data:
            return jsonify({'error': 'Request body is required'}), 400
        
        content = data.get('content', '')
        schema_version = data.get('schema_version', 'both')
        
        if not content:
            return jsonify({'error': 'content field is required'}), 400
        
        resolved_content = list_service.resolve_rule_lists(content, schema_version)
        
        return jsonify({
            'original': content,
            'resolved': resolved_content
        })
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500