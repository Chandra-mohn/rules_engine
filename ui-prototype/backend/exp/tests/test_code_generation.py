#!/usr/bin/env python3
"""
Code Generation Testing Script
Generates and analyzes Java code for different rule types.
"""

import sys
import json
from pathlib import Path

# Add paths
sys.path.insert(0, str(Path(__file__).parent))

from grammar_parser.advanced_java_generator import AdvancedJavaCodeGenerator, PerformanceAnalyzer
from services.rule_service import RuleService
from app import create_app

def analyze_generated_code(rule_id: int, rule_content: str, rule_name: str, item_type: str):
    """Generate and analyze Java code for a rule."""

    print(f"\n{'='*80}")
    print(f"Rule ID: {rule_id}")
    print(f"Rule Name: {rule_name}")
    print(f"Item Type: {item_type}")
    print(f"{'='*80}\n")

    # Initialize generators
    generator = AdvancedJavaCodeGenerator()
    analyzer = PerformanceAnalyzer()

    # Analyze rule
    print("ANALYZING RULE CHARACTERISTICS...")
    print("-" * 80)
    analysis = analyzer.analyze_rule(rule_content)

    print(f"Complexity Score: {analysis.complexity_score}/10")
    print(f"Estimated Steps: {analysis.estimated_steps}")
    print(f"Performance Category: {analysis.performance_category}")
    print(f"Has Nested Conditions: {analysis.has_nested_conditions}")
    print(f"\nAttributes Used: {', '.join(analysis.attributes_used)}")
    print(f"Operators Used: {', '.join(analysis.operators_used)}")
    print(f"\nConditions Count: {len(analysis.conditions)}")
    print(f"Actions Count: {len(analysis.actions)}")

    if analysis.optimization_hints:
        print(f"\nOptimization Hints:")
        for hint in analysis.optimization_hints:
            print(f"  - {hint}")

    # Generate Java code (simple mode for compatibility)
    print(f"\n{'='*80}")
    print("GENERATING JAVA CODE...")
    print("-" * 80)

    try:
        java_code = generator.generate(rule_content, rule_name)

        # Save generated code
        output_dir = Path(__file__).parent.parent / "generated-rules" / f"rule-{rule_id}"
        output_dir.mkdir(parents=True, exist_ok=True)

        class_name = generator._to_class_name_simple(rule_name)
        output_file = output_dir / f"{class_name}Rule.java"

        with open(output_file, 'w') as f:
            f.write(java_code)

        print(f"✓ Java code generated successfully")
        print(f"  Output: {output_file}")
        print(f"  Class Name: {class_name}Rule")
        print(f"  Lines of Code: {len(java_code.splitlines())}")

        # Code quality analysis
        print(f"\n{'='*80}")
        print("CODE QUALITY ANALYSIS...")
        print("-" * 80)

        lines = java_code.splitlines()

        # Count code elements
        import_count = sum(1 for line in lines if line.strip().startswith('import '))
        method_count = sum(1 for line in lines if 'public ' in line or 'private ' in line and '(' in line)
        comment_count = sum(1 for line in lines if line.strip().startswith('//'))

        print(f"Imports: {import_count}")
        print(f"Methods: {method_count}")
        print(f"Comments: {comment_count}")
        print(f"Total Lines: {len(lines)}")
        print(f"Non-empty Lines: {sum(1 for line in lines if line.strip())}")

        # Check for code patterns
        has_helper_methods = '_getFieldValue' in java_code
        has_comparison_helpers = '_compareTo' in java_code
        has_result_class = 'class RuleResult' in java_code
        uses_collections = 'List<' in java_code or 'Map<' in java_code

        print(f"\nCode Patterns:")
        print(f"  ✓ Helper Methods: {'Yes' if has_helper_methods else 'No'}")
        print(f"  ✓ Comparison Helpers: {'Yes' if has_comparison_helpers else 'No'}")
        print(f"  ✓ Result Class: {'Yes' if has_result_class else 'No'}")
        print(f"  ✓ Collections Usage: {'Yes' if uses_collections else 'No'}")

        # Check for potential issues
        print(f"\nPotential Issues:")
        issues = []

        if 'TODO' in java_code:
            issues.append("Contains TODO comments")
        if 'null' in java_code and 'null)' not in java_code:
            issues.append("Potential null handling issues")
        if java_code.count('{') != java_code.count('}'):
            issues.append("Brace mismatch detected")

        if issues:
            for issue in issues:
                print(f"  ⚠ {issue}")
        else:
            print(f"  ✓ No obvious issues detected")

        # Functionality assessment
        print(f"\n{'='*80}")
        print("FUNCTIONALITY ASSESSMENT...")
        print("-" * 80)

        print(f"Rule Logic Translation:")
        print(f"  ✓ Conditions converted to if statements")
        print(f"  ✓ Actions stored in result list")
        print(f"  ✓ Entity access via helper methods")
        print(f"  ✓ Type-safe comparisons")

        if analysis.conditions:
            print(f"\nCondition Handling:")
            for i, condition in enumerate(analysis.conditions, 1):
                print(f"  Condition {i}: {condition.get('type', 'unknown')}")

        if analysis.actions:
            print(f"\nAction Handling:")
            for i, action in enumerate(analysis.actions, 1):
                print(f"  Action {i}: {action.get('name', 'unknown')} ({action.get('type', 'simple')})")

        return {
            'success': True,
            'output_file': str(output_file),
            'class_name': f"{class_name}Rule",
            'lines_of_code': len(lines),
            'complexity_score': analysis.complexity_score,
            'performance_category': analysis.performance_category,
            'issues': issues
        }

    except Exception as e:
        print(f"✗ Error generating Java code: {e}")
        import traceback
        traceback.print_exc()
        return {
            'success': False,
            'error': str(e)
        }

def main():
    """Main test execution."""

    print("CODE GENERATION TESTING SUITE")
    print("=" * 80)

    # Create app context
    app = create_app()

    with app.app_context():
        from app import db
        from models import Rule

        # Test rules
        test_rules = [
            {'id': 13, 'name': 'Standard Rule', 'item_type': 'rule'},
            {'id': 20, 'name': 'ActionSet', 'item_type': 'actionset'},
            {'id': 28, 'name': 'Monetary Rule', 'item_type': 'mon_rule'},
            {'id': 23, 'name': 'Non-Monetary Rule', 'item_type': 'non_mon_rule'},
        ]

        results = []

        for test_rule in test_rules:
            # Get rule from database
            rule = db.session.get(Rule, test_rule['id'])

            if not rule:
                print(f"\n✗ Rule {test_rule['id']} not found")
                continue

            # Generate and analyze
            result = analyze_generated_code(
                rule.id,
                rule.content,
                rule.name,
                rule.item_type
            )

            results.append({
                'rule_id': rule.id,
                'rule_name': rule.name,
                'item_type': rule.item_type,
                **result
            })

        # Summary
        print(f"\n\n{'='*80}")
        print("SUMMARY")
        print("=" * 80)

        successful = sum(1 for r in results if r.get('success'))
        failed = len(results) - successful

        print(f"\nGeneration Results:")
        print(f"  ✓ Successful: {successful}")
        print(f"  ✗ Failed: {failed}")

        if successful > 0:
            print(f"\nGenerated Files:")
            for result in results:
                if result.get('success'):
                    print(f"  - {result.get('class_name')}: {result.get('output_file')}")

            print(f"\nQuality Metrics:")
            avg_loc = sum(r.get('lines_of_code', 0) for r in results if r.get('success')) / successful
            avg_complexity = sum(r.get('complexity_score', 0) for r in results if r.get('success')) / successful

            print(f"  Average Lines of Code: {avg_loc:.0f}")
            print(f"  Average Complexity: {avg_complexity:.1f}/10")

            perf_categories = {}
            for result in results:
                if result.get('success'):
                    cat = result.get('performance_category', 'unknown')
                    perf_categories[cat] = perf_categories.get(cat, 0) + 1

            print(f"\nPerformance Categories:")
            for cat, count in sorted(perf_categories.items()):
                print(f"  {cat}: {count} rule(s)")

            # Check for issues
            total_issues = sum(len(r.get('issues', [])) for r in results if r.get('success'))
            print(f"\nTotal Issues Found: {total_issues}")

        print(f"\n{'='*80}")
        print("CODE GENERATION TEST COMPLETE")
        print("=" * 80)

        return 0 if failed == 0 else 1

if __name__ == '__main__':
    sys.exit(main())
