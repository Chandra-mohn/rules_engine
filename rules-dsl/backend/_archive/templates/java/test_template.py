"""
JUnit 5 test template using Python f-strings.
Generates comprehensive unit tests for generated rule classes.
"""

def generate_standard_rule_test(rule_name, class_name, entities, test_scenarios, complexity_score):
    """
    Generate JUnit 5 test class for standard rule.

    Args:
        rule_name: Rule display name
        class_name: Java class name to test (PascalCase)
        entities: List of entity names (e.g., ['applicant', 'transaction'])
        test_scenarios: List of test scenario dictionaries with structure:
            {
                'name': 'scenario_name',
                'description': 'what this tests',
                'entity_values': {'applicant': {'creditScore': 750}, 'transaction': {...}},
                'expected_matched': True/False,
                'expected_actions': ['action1', 'action2'],
                'expected_final_action': 'actionName' or None
            }
        complexity_score: 0-10 complexity rating

    Returns:
        str: Complete JUnit 5 test class code
    """

    # Generate test methods
    test_methods = []
    for scenario in test_scenarios:
        test_method = _generate_test_method(scenario, entities, class_name)
        test_methods.append(test_method)

    test_methods_code = '\n\n'.join(test_methods)

    # Build complete test class
    code = f'''package com.rules;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

/**
 * Unit tests for {rule_name}
 * Generated from DSL rule definition
 * Complexity Score: {complexity_score}/10
 */
@DisplayName("{rule_name} - Test Suite")
public class {class_name}Test {{

    private Map<String, Object> context;

    @BeforeEach
    void setUp() {{
        context = new HashMap<>();
    }}

{test_methods_code}

    // Helper method: Create entity map from key-value pairs
    private Map<String, Object> createEntity(Object... pairs) {{
        Map<String, Object> entity = new HashMap<>();
        for (int i = 0; i < pairs.length; i += 2) {{
            entity.put((String) pairs[i], pairs[i + 1]);
        }}
        return entity;
    }}

    // Helper method: Create nested entity structure
    private Map<String, Object> createNestedEntity(String path, Object value) {{
        Map<String, Object> result = new HashMap<>();
        String[] parts = path.split("\\\\.");
        Map<String, Object> current = result;

        for (int i = 0; i < parts.length - 1; i++) {{
            Map<String, Object> next = new HashMap<>();
            current.put(parts[i], next);
            current = next;
        }}
        current.put(parts[parts.length - 1], value);

        return result;
    }}
}}
'''

    return code


def _generate_test_method(scenario, entities, class_name):
    """Generate a single JUnit test method from scenario."""

    scenario_name = scenario['name']
    description = scenario['description']
    entity_values = scenario.get('entity_values', {})
    expected_matched = scenario.get('expected_matched', False)
    expected_actions = scenario.get('expected_actions', [])
    expected_final_action = scenario.get('expected_final_action', None)

    # Generate entity setup code
    entity_setup = []
    if entity_values and any(entity_values.values()):
        # Use provided entity values
        for entity_name in sorted(entity_values.keys()):
            values = entity_values[entity_name]
            entity_setup.append(_generate_entity_setup(entity_name, values))
    else:
        # Generate default entity setup with TODO comment
        entity_setup.append('// TODO: Set up test data for entities: ' + ', '.join(sorted(entities)))
        for entity_name in sorted(entities):
            entity_setup.append(f'context.put("{entity_name}", new HashMap<>());')

    entity_setup_code = '\n        '.join(entity_setup)

    # Generate assertions
    assertions = []
    assertions.append(f'assertEquals({str(expected_matched).lower()}, result.isMatched(), "Rule should {"" if expected_matched else "not "}match");')

    if expected_actions:
        assertions.append(f'assertEquals({len(expected_actions)}, result.getActions().size(), "Expected {len(expected_actions)} action(s)");')
        for i, action in enumerate(expected_actions):
            # Handle action with parameters
            if '(' in action:
                action_name = action.split('(')[0]
                assertions.append(f'assertTrue(result.getActions().get({i}).startsWith("{action_name}("), "Action {i} should be {action_name}");')
            else:
                assertions.append(f'assertEquals("{action}", result.getActions().get({i}), "Action {i} should be {action}");')

    if expected_final_action:
        assertions.append(f'assertEquals("{expected_final_action}", result.getFinalAction(), "Final action should be {expected_final_action}");')
    elif expected_final_action is None and expected_matched:
        assertions.append('assertNull(result.getFinalAction(), "Final action should be null");')

    assertions_code = '\n        '.join(assertions)

    # Clean up display name (limit to 80 chars)
    clean_description = description[:80] if len(description) > 80 else description

    # Clean up method name (convert to camelCase, limit length)
    clean_method_name = _clean_method_name(scenario_name)

    method_code = f'''    @Test
    @DisplayName("{clean_description}")
    void {clean_method_name}() {{
        // Arrange
        {entity_setup_code}

        // Act
        {class_name}.RuleResult result = {class_name}.evaluate(context);

        // Assert
        {assertions_code}
    }}'''

    return method_code


def _generate_entity_setup(entity_name, values):
    """Generate entity setup code from values dictionary."""

    if not values:
        return f'context.put("{entity_name}", new HashMap<>());'

    # Check if we have nested structure
    has_nested = any('.' in str(k) or isinstance(v, dict) for k, v in values.items())

    if has_nested:
        # Generate nested structure setup
        setup_lines = [f'Map<String, Object> {entity_name} = new HashMap<>();']
        _add_nested_values(setup_lines, entity_name, values, '')
        setup_lines.append(f'context.put("{entity_name}", {entity_name});')
        return '\n        '.join(setup_lines)
    else:
        # Simple flat structure
        pairs = []
        for key, value in values.items():
            pairs.append(f'"{key}", {_format_value(value)}')

        return f'context.put("{entity_name}", createEntity({", ".join(pairs)}));'


def _add_nested_values(lines, entity_var, values, path):
    """Recursively add nested values to setup code."""

    for key, value in values.items():
        if isinstance(value, dict):
            # Create nested map
            nested_var = f'{entity_var}_{key}'
            lines.append(f'Map<String, Object> {nested_var} = new HashMap<>();')
            _add_nested_values(lines, nested_var, value, f'{path}{key}.')
            lines.append(f'{entity_var}.put("{key}", {nested_var});')
        else:
            # Simple value
            lines.append(f'{entity_var}.put("{key}", {_format_value(value)});')


def _clean_method_name(name):
    """Clean and shorten test method name."""
    # Limit to 50 chars to avoid overly long method names
    if len(name) > 50:
        name = name[:50]

    # Remove trailing underscores from truncation
    name = name.rstrip('_')

    return name


def _format_value(value):
    """Format Python value as Java literal."""

    if value is None:
        return 'null'
    elif isinstance(value, bool):
        return 'true' if value else 'false'
    elif isinstance(value, str):
        # Escape quotes in string
        escaped = value.replace('\\', '\\\\').replace('"', '\\"')
        return f'"{escaped}"'
    elif isinstance(value, (int, float)):
        return str(value)
    elif isinstance(value, list):
        items = ', '.join(_format_value(item) for item in value)
        return f'Arrays.asList({items})'
    else:
        return f'"{str(value)}"'


def generate_actionset_test(rule_name, class_name, entities, test_scenarios, complexity_score):
    """
    Generate JUnit 5 test class for ActionSet rule.
    ActionSets evaluate all conditions and accumulate matching actions.

    Args: Same as generate_standard_rule_test

    Returns:
        str: Complete JUnit 5 test class code
    """

    # ActionSet tests are similar to standard rule tests
    # but may have multiple actions accumulated
    return generate_standard_rule_test(rule_name, class_name, entities, test_scenarios, complexity_score)


def generate_action_test(rule_name, class_name, entities, test_scenarios, complexity_score):
    """
    Generate JUnit 5 test class for direct Action rule.
    Direct actions always execute without conditions.

    Args: Same as generate_standard_rule_test

    Returns:
        str: Complete JUnit 5 test class code
    """

    # Direct action tests always expect matched=true
    for scenario in test_scenarios:
        scenario['expected_matched'] = True

    return generate_standard_rule_test(rule_name, class_name, entities, test_scenarios, complexity_score)
