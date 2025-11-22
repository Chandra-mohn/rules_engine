"""
Context API Endpoints (File-Based)
CRUD operations for rule contexts stored as JSON files
"""

from flask import Blueprint, request, jsonify
from services.context_file_service import ContextFileService
from config import Config
import math

contexts_file_bp = Blueprint('contexts_file', __name__)

# Initialize file service
context_service = ContextFileService()


@contexts_file_bp.route('/contexts', methods=['GET'])
def get_contexts():
    """Get paginated list of contexts with optional filtering."""
    try:
        # Parse query parameters
        page = request.args.get('page', 1, type=int)
        limit = min(request.args.get('limit', Config.DEFAULT_PAGE_SIZE, type=int), Config.MAX_PAGE_SIZE)
        is_schema_template = request.args.get('is_schema_template', type=lambda v: v.lower() == 'true')
        client_code = request.args.get('client_code')
        search = request.args.get('search')

        # Get all contexts with filters
        all_contexts = context_service.list_contexts(
            is_schema_template=is_schema_template,
            client_code=client_code,
            search=search
        )

        # Sort: templates first, then by name
        all_contexts.sort(key=lambda c: (not c.get('is_schema_template', False), c.get('name', '')))

        # Paginate
        total = len(all_contexts)
        offset = (page - 1) * limit
        contexts = all_contexts[offset:offset + limit]

        # Calculate pagination info
        pages = math.ceil(total / limit) if total > 0 else 1

        return jsonify({
            'contexts': contexts,
            'total': total,
            'page': page,
            'pages': pages,
            'limit': limit
        })

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@contexts_file_bp.route('/contexts/<string:context_name>', methods=['GET'])
def get_context(context_name):
    """Get a single context by name."""
    try:
        context = context_service.get_context(context_name)
        if not context:
            return jsonify({'error': 'Context not found'}), 404
        return jsonify(context)
    except Exception as e:
        return jsonify({'error': str(e)}), 500


@contexts_file_bp.route('/contexts', methods=['POST'])
def create_context():
    """Create a new context."""
    try:
        data = request.get_json()

        # Validate required fields
        if not data.get('name'):
            return jsonify({'error': 'name is required'}), 400
        if not data.get('context_data'):
            return jsonify({'error': 'context_data is required'}), 400

        # Check for duplicate name
        existing = context_service.get_context(data['name'])
        if existing:
            return jsonify({'error': f'Context with name "{data["name"]}" already exists'}), 400

        # Create context
        context = context_service.save_context(data)

        return jsonify(context), 201

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@contexts_file_bp.route('/contexts/<string:context_name>', methods=['PUT'])
def update_context(context_name):
    """Update an existing context."""
    try:
        existing_context = context_service.get_context(context_name)
        if not existing_context:
            return jsonify({'error': 'Context not found'}), 404

        data = request.get_json()

        # Check for duplicate name (if changing name)
        if 'name' in data and data['name'] != context_name:
            existing = context_service.get_context(data['name'])
            if existing:
                return jsonify({'error': f'Context with name "{data["name"]}" already exists'}), 400

            # Delete old context and create with new name
            context_service.delete_context(context_name)

        # Update fields
        updated_context = existing_context.copy()
        if 'name' in data:
            updated_context['name'] = data['name']
        if 'description' in data:
            updated_context['description'] = data['description']
        if 'context_data' in data:
            updated_context['context_data'] = data['context_data']
        if 'is_schema_template' in data:
            updated_context['is_schema_template'] = data['is_schema_template']
        if 'version' in data:
            updated_context['version'] = data['version']
        if 'client_code' in data:
            updated_context['client_code'] = data['client_code']

        # Save updated context
        saved_context = context_service.save_context(updated_context)

        return jsonify(saved_context)

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@contexts_file_bp.route('/contexts/<string:context_name>', methods=['DELETE'])
def delete_context(context_name):
    """Delete a context."""
    try:
        deleted = context_service.delete_context(context_name)

        if not deleted:
            return jsonify({'error': 'Context not found'}), 404

        return jsonify({'message': 'Context deleted successfully'})

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@contexts_file_bp.route('/contexts/<string:context_name>/clone', methods=['POST'])
def clone_context(context_name):
    """Clone an existing context with a new name."""
    try:
        data = request.get_json()

        if not data.get('name'):
            return jsonify({'error': 'name is required for cloned context'}), 400

        # Check for duplicate name
        existing = context_service.get_context(data['name'])
        if existing:
            return jsonify({'error': f'Context with name "{data["name"]}" already exists'}), 400

        # Clone context
        cloned_context = context_service.clone_context(
            source_name=context_name,
            new_name=data['name'],
            new_description=data.get('description')
        )

        # Update additional fields if provided
        if 'is_schema_template' in data:
            cloned_context['is_schema_template'] = data['is_schema_template']
        if 'version' in data:
            cloned_context['version'] = data['version']
        if 'client_code' in data:
            cloned_context['client_code'] = data['client_code']

        # Save with updates
        saved_context = context_service.save_context(cloned_context)

        return jsonify(saved_context), 201

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@contexts_file_bp.route('/contexts/schema-templates', methods=['GET'])
def get_schema_templates():
    """Get all schema templates (convenience endpoint)."""
    try:
        client_code = request.args.get('client_code')

        templates = context_service.list_contexts(
            is_schema_template=True,
            client_code=client_code
        )

        return jsonify({
            'templates': templates
        })

    except Exception as e:
        return jsonify({'error': str(e)}), 500
