from flask import Blueprint, jsonify, request
from schema.rules_schema import (
    ATTRIBUTES, ACTIONS, FUNCTIONS, KEYWORDS, OPERATORS, TIME_UNITS,
    get_all_attributes, get_all_actions, get_all_functions,
    get_attributes_by_entity, get_schema_versions, get_schema_attributes,
    get_schema_actions, get_schema_for_version, detect_rule_schema_version
)

schema_bp = Blueprint('schema', __name__)

@schema_bp.route('/schema', methods=['GET'])
def get_full_schema():
    """Get the complete rules schema."""
    return jsonify({
        'attributes': ATTRIBUTES,
        'actions': ACTIONS,
        'functions': FUNCTIONS,
        'keywords': KEYWORDS,
        'operators': OPERATORS,
        'time_units': TIME_UNITS
    })

@schema_bp.route('/schema/attributes', methods=['GET'])
def get_attributes():
    """Get all attributes for autocomplete."""
    return jsonify({
        'attributes': get_all_attributes()
    })

@schema_bp.route('/schema/attributes/<entity>', methods=['GET'])
def get_entity_attributes(entity):
    """Get attributes for a specific entity."""
    return jsonify({
        'entity': entity,
        'attributes': get_attributes_by_entity(entity)
    })

@schema_bp.route('/schema/actions', methods=['GET'])
def get_actions():
    """Get all actions for autocomplete."""
    return jsonify({
        'actions': get_all_actions()
    })

@schema_bp.route('/schema/functions', methods=['GET'])
def get_functions():
    """Get all functions for autocomplete."""
    return jsonify({
        'functions': get_all_functions()
    })

@schema_bp.route('/schema/keywords', methods=['GET'])
def get_keywords():
    """Get all keywords and operators."""
    return jsonify({
        'keywords': KEYWORDS,
        'operators': OPERATORS,
        'time_units': TIME_UNITS
    })

@schema_bp.route('/schema/versions', methods=['GET'])
def get_versions():
    """Get available schema versions."""
    return jsonify({
        'versions': get_schema_versions()
    })

@schema_bp.route('/schema/<version>', methods=['GET'])
def get_schema_by_version(version):
    """Get complete schema for a specific version."""
    return jsonify(get_schema_for_version(version))

@schema_bp.route('/schema/<version>/attributes', methods=['GET'])
def get_version_attributes(version):
    """Get attributes for a specific schema version."""
    return jsonify({
        'version': version,
        'attributes': get_schema_attributes(version)
    })

@schema_bp.route('/schema/<version>/actions', methods=['GET'])
def get_version_actions(version):
    """Get actions for a specific schema version."""
    return jsonify({
        'version': version,
        'actions': get_schema_actions(version)
    })

@schema_bp.route('/schema/detect', methods=['POST'])
def detect_schema_version():
    """Detect schema version from rule content."""
    data = request.get_json()
    rule_content = data.get('content', '')
    detected_version = detect_rule_schema_version(rule_content)
    
    return jsonify({
        'detected_version': detected_version,
        'confidence': 'high' if detected_version == 'legacy' and 'CREDIT_SCORE' in rule_content else 'medium'
    })