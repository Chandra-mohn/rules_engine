"""
Debug script to see what scenarios are being generated.
"""

import sys
from pathlib import Path

# Add paths for imports
sys.path.insert(0, str(Path(__file__).parent / 'grammar_parser'))

from grammar_parser import RulesEngineParser
from test_scenario_generator import TestScenarioGenerator
from antlr4.tree.Tree import ParseTreeWalker

# Simple test rule
RULE = """rule "Credit Score Check":
    if applicant.creditScore >= 750 then
        approveApplication("Excellent credit")
    endif"""

parser = RulesEngineParser()
tree, error_listener = parser.parse(RULE)

if tree and not error_listener.errors:
    print("Parse tree type:", type(tree))
    print()

    # Now extract scenarios
    scenario_gen = TestScenarioGenerator()
    walker = ParseTreeWalker()
    walker.walk(scenario_gen, tree)

    scenarios = scenario_gen.get_scenarios()

    print(f"Generated {len(scenarios)} scenarios:")
    for i, scenario in enumerate(scenarios, 1):
        print(f"\n--- Scenario {i}: {scenario['name']} ---")
        print(f"Description: {scenario['description']}")
        print(f"Entity values: {scenario['entity_values']}")
        print(f"Expected matched: {scenario['expected_matched']}")
        print(f"Expected actions: {scenario['expected_actions']}")

    # Also check what entities were found
    print(f"\nEntities found: {scenario_gen.entities}")
else:
    print("Parse errors:", error_listener.errors if error_listener else "Unknown")
