"""
Context API endpoints for right-click help system.
Provides ActionSet definitions, Action Java source, and Attribute schema information.
"""

from flask import Blueprint, jsonify, request, current_app
from models import Rule, SchemaAttribute, db
import os
from pathlib import Path
import logging

bp = Blueprint('context', __name__, url_prefix='/api/context')
logger = logging.getLogger(__name__)


def sanitize_file_path(java_file_path, base_dir=None):
    """
    Sanitize file path to prevent directory traversal attacks.
    Returns absolute path if safe, None if dangerous.
    """
    if base_dir is None:
        # Use the sample_java_actions directory for testing
        base_dir = os.path.join(os.path.dirname(__file__), '..', 'sample_java_actions')
    if not java_file_path:
        return None

    try:
        # Convert to Path objects for safe manipulation
        base_path = Path(base_dir).resolve()
        requested_path = Path(java_file_path)

        # Combine and resolve full path
        full_path = (base_path / requested_path).resolve()

        # Verify the resolved path is within the base directory
        if not str(full_path).startswith(str(base_path)):
            logger.warning(f"Directory traversal attempt blocked: {java_file_path}")
            return None

        return full_path

    except Exception as e:
        logger.error(f"Path sanitization error: {e}")
        return None


@bp.route('/rule/<name>')
def get_rule_context(name):
    """
    Get context information for a rule name (ActionSet or Action).
    Returns rule content and metadata.
    """
    try:
        # Query by name to find matching rule/actionset/action
        rule = Rule.query.filter_by(name=name).first()

        if not rule:
            return jsonify({'error': f'Rule/ActionSet/Action not found: {name}'}), 404

        context_info = {
            'name': rule.name,
            'item_type': rule.item_type,
            'content': rule.content,
            'description': rule.description,
            'status': rule.status,
            'created_at': rule.created_at.isoformat() if rule.created_at else None,
            'updated_at': rule.updated_at.isoformat() if rule.updated_at else None
        }

        # Add Java file path for actions
        if rule.item_type == 'action' and rule.java_file_path:
            context_info['java_file_path'] = rule.java_file_path

        return jsonify(context_info)

    except Exception as e:
        logger.error(f"Error getting rule context for {name}: {e}")
        return jsonify({'error': 'Internal server error'}), 500


@bp.route('/action/<name>/source')
def get_action_java_source(name):
    """
    Get Java source code for an action.
    Reads from file system with security checks.
    """
    try:
        # Find the action in database
        action = Rule.query.filter_by(name=name, item_type='action').first()

        if not action:
            return jsonify({'error': f'Action not found: {name}'}), 404

        if not action.java_file_path:
            return jsonify({'error': f'No Java file path configured for action: {name}'}), 404

        # Sanitize the file path
        safe_path = sanitize_file_path(action.java_file_path)
        if not safe_path:
            return jsonify({'error': 'Invalid file path'}), 400

        # Check if file exists
        if not safe_path.exists():
            return jsonify({'error': f'Java source file not found: {action.java_file_path}'}), 404

        # Read the Java source file
        try:
            java_source = safe_path.read_text(encoding='utf-8')

            return jsonify({
                'name': action.name,
                'item_type': 'action',
                'java_file_path': action.java_file_path,
                'source_code': java_source,
                'file_size': len(java_source),
                'language': 'java'
            })

        except UnicodeDecodeError:
            return jsonify({'error': 'File encoding error - not valid UTF-8'}), 500

    except Exception as e:
        logger.error(f"Error reading Java source for action {name}: {e}")
        return jsonify({'error': 'Internal server error'}), 500


@bp.route('/attribute/<path:attribute_name>/schema')
def get_attribute_schema(attribute_name):
    """
    Get schema information for an attribute.
    Supports dot notation like 'applicant.creditScore'.
    """
    try:
        # Strategy 1: Try exact match first (handles dot-notation like "applicant.creditScore")
        schema_attr = SchemaAttribute.query.filter_by(name=attribute_name).first()

        # Strategy 2: If no exact match and it's dot-notation, try entity resolution
        if not schema_attr and '.' in attribute_name:
            entity_name, attr_name = attribute_name.split('.', 1)

            # Try to find the entity
            from models import SchemaEntity
            entity = SchemaEntity.query.filter_by(name=entity_name).first()

            if entity:
                # Look for the attribute name without the entity prefix in that entity
                schema_attr = SchemaAttribute.query.filter_by(
                    entity_id=entity.id,
                    name=attr_name
                ).first()

        # Strategy 3: If still no match, try searching all attributes for the base name
        if not schema_attr and '.' in attribute_name:
            entity_name, attr_name = attribute_name.split('.', 1)
            # Search for attribute by base name across all entities
            schema_attr = SchemaAttribute.query.filter_by(name=attr_name).first()

        if not schema_attr:
            return jsonify({'error': f'Attribute schema not found: {attribute_name}'}), 404

        schema_info = {
            'name': schema_attr.name,
            'item_type': 'attribute',
            'data_type': schema_attr.data_type,
            'description': schema_attr.description,
            'is_required': schema_attr.is_required
        }

        # Add entity information if available
        if hasattr(schema_attr, 'entity_id') and schema_attr.entity_id:
            from models import SchemaEntity
            entity = SchemaEntity.query.get(schema_attr.entity_id)
            if entity:
                schema_info['entity_name'] = entity.name
                schema_info['entity_description'] = entity.description

                # Construct full qualified name if attribute doesn't already have it
                if '.' not in schema_attr.name:
                    schema_info['qualified_name'] = f"{entity.name}.{schema_attr.name}"
                else:
                    schema_info['qualified_name'] = schema_attr.name

        # Add additional metadata if available
        if hasattr(schema_attr, 'default_value') and schema_attr.default_value is not None:
            schema_info['default_value'] = schema_attr.default_value
        if hasattr(schema_attr, 'validation_rules') and schema_attr.validation_rules is not None:
            schema_info['validation_rules'] = schema_attr.validation_rules
        if hasattr(schema_attr, 'source_table') and schema_attr.source_table is not None:
            schema_info['source_table'] = schema_attr.source_table
        if hasattr(schema_attr, 'min_value') and schema_attr.min_value is not None:
            schema_info['min_value'] = schema_attr.min_value
        if hasattr(schema_attr, 'max_value') and schema_attr.max_value is not None:
            schema_info['max_value'] = schema_attr.max_value

        return jsonify(schema_info)

    except Exception as e:
        logger.error(f"Error getting attribute schema for {attribute_name}: {e}")
        return jsonify({'error': 'Internal server error'}), 500


@bp.route('/debug/path-test')
def debug_path_test():
    """
    Debug endpoint to test path sanitization (remove in production).
    """
    test_path = request.args.get('path', '')
    safe_path = sanitize_file_path(test_path)

    return jsonify({
        'input_path': test_path,
        'safe_path': str(safe_path) if safe_path else None,
        'exists': safe_path.exists() if safe_path else False
    })


@bp.errorhandler(404)
def not_found(error):
    return jsonify({'error': 'Context resource not found'}), 404


@bp.errorhandler(500)
def internal_error(error):
    return jsonify({'error': 'Internal server error'}), 500