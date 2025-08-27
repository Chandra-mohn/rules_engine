from flask import Blueprint, jsonify
from schema.rules_schema import (
    ATTRIBUTES, ACTIONS, FUNCTIONS, KEYWORDS, OPERATORS, TIME_UNITS,
    get_all_attributes, get_all_actions, get_all_functions,
    get_attributes_by_entity
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