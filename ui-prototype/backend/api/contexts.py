"""
Context API Endpoints
CRUD operations for rule contexts (test data and schema templates)
"""

from flask import Blueprint, request, jsonify
from marshmallow import ValidationError
from models import RuleContext, RuleContextSchema, db
from config import Config
import math

contexts_bp = Blueprint('contexts', __name__)
context_schema = RuleContextSchema()
contexts_schema = RuleContextSchema(many=True)


@contexts_bp.route('/contexts', methods=['GET'])
def get_contexts():
    """Get paginated list of contexts with optional filtering."""
    try:
        # Parse query parameters
        page = request.args.get('page', 1, type=int)
        limit = min(request.args.get('limit', Config.DEFAULT_PAGE_SIZE, type=int), Config.MAX_PAGE_SIZE)
        is_schema_template = request.args.get('is_schema_template', type=lambda v: v.lower() == 'true')
        client_id = request.args.get('client_id', type=int)
        search = request.args.get('search')

        # Build query
        query = RuleContext.query

        # Apply filters
        if is_schema_template is not None:
            query = query.filter(RuleContext.is_schema_template == is_schema_template)

        if client_id is not None:
            query = query.filter(RuleContext.client_id == client_id)

        if search:
            search_pattern = f'%{search}%'
            query = query.filter(
                db.or_(
                    RuleContext.name.like(search_pattern),
                    RuleContext.description.like(search_pattern)
                )
            )

        # Get total count
        total = query.count()

        # Paginate
        offset = (page - 1) * limit
        contexts = query.order_by(
            RuleContext.is_schema_template.desc(),  # Schema templates first
            RuleContext.name
        ).offset(offset).limit(limit).all()

        # Calculate pagination info
        pages = math.ceil(total / limit) if total > 0 else 1

        return jsonify({
            'contexts': [context.to_dict() for context in contexts],
            'total': total,
            'page': page,
            'pages': pages,
            'limit': limit
        })

    except Exception as e:
        return jsonify({'error': str(e)}), 500


@contexts_bp.route('/contexts/<int:context_id>', methods=['GET'])
def get_context(context_id):
    """Get a single context by ID."""
    try:
        context = RuleContext.query.get_or_404(context_id)
        return jsonify(context.to_dict())
    except Exception as e:
        return jsonify({'error': str(e)}), 404


@contexts_bp.route('/contexts', methods=['POST'])
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
        existing = RuleContext.query.filter_by(name=data['name']).first()
        if existing:
            return jsonify({'error': f'Context with name "{data["name"]}" already exists'}), 400

        # Create context
        context = RuleContext(
            name=data['name'],
            description=data.get('description'),
            context_data=data['context_data'],
            is_schema_template=data.get('is_schema_template', False),
            version=data.get('version'),
            client_id=data.get('client_id')
        )

        db.session.add(context)
        db.session.commit()

        return jsonify(context.to_dict()), 201

    except ValidationError as e:
        return jsonify({'error': e.messages}), 400
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@contexts_bp.route('/contexts/<int:context_id>', methods=['PUT'])
def update_context(context_id):
    """Update an existing context."""
    try:
        context = RuleContext.query.get_or_404(context_id)
        data = request.get_json()

        # Check for duplicate name (if changing name)
        if 'name' in data and data['name'] != context.name:
            existing = RuleContext.query.filter_by(name=data['name']).first()
            if existing:
                return jsonify({'error': f'Context with name "{data["name"]}" already exists'}), 400

        # Update fields
        if 'name' in data:
            context.name = data['name']
        if 'description' in data:
            context.description = data['description']
        if 'context_data' in data:
            context.context_data = data['context_data']
        if 'is_schema_template' in data:
            context.is_schema_template = data['is_schema_template']
        if 'version' in data:
            context.version = data['version']
        if 'client_id' in data:
            context.client_id = data['client_id']

        db.session.commit()

        return jsonify(context.to_dict())

    except ValidationError as e:
        return jsonify({'error': e.messages}), 400
    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@contexts_bp.route('/contexts/<int:context_id>', methods=['DELETE'])
def delete_context(context_id):
    """Delete a context."""
    try:
        context = RuleContext.query.get_or_404(context_id)

        # Check if context is in use
        if context.rules:
            return jsonify({
                'error': f'Cannot delete context "{context.name}" - it is used by {len(context.rules)} rule(s)'
            }), 400

        db.session.delete(context)
        db.session.commit()

        return jsonify({'message': 'Context deleted successfully'})

    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@contexts_bp.route('/contexts/<int:context_id>/clone', methods=['POST'])
def clone_context(context_id):
    """Clone an existing context with a new name."""
    try:
        source_context = RuleContext.query.get_or_404(context_id)
        data = request.get_json()

        if not data.get('name'):
            return jsonify({'error': 'name is required for cloned context'}), 400

        # Check for duplicate name
        existing = RuleContext.query.filter_by(name=data['name']).first()
        if existing:
            return jsonify({'error': f'Context with name "{data["name"]}" already exists'}), 400

        # Create cloned context
        cloned_context = RuleContext(
            name=data['name'],
            description=data.get('description', f'Cloned from {source_context.name}'),
            context_data=source_context.context_data.copy(),  # Deep copy of JSON
            is_schema_template=data.get('is_schema_template', source_context.is_schema_template),
            version=data.get('version', source_context.version),
            client_id=data.get('client_id', source_context.client_id)
        )

        db.session.add(cloned_context)
        db.session.commit()

        return jsonify(cloned_context.to_dict()), 201

    except Exception as e:
        db.session.rollback()
        return jsonify({'error': str(e)}), 500


@contexts_bp.route('/contexts/schema-templates', methods=['GET'])
def get_schema_templates():
    """Get all schema templates (convenience endpoint)."""
    try:
        client_id = request.args.get('client_id', type=int)

        query = RuleContext.query.filter_by(is_schema_template=True)

        if client_id is not None:
            query = query.filter(
                db.or_(
                    RuleContext.client_id == client_id,
                    RuleContext.client_id.is_(None)  # Include global templates
                )
            )

        templates = query.order_by(RuleContext.name).all()

        return jsonify({
            'templates': [template.to_dict() for template in templates]
        })

    except Exception as e:
        return jsonify({'error': str(e)}), 500
