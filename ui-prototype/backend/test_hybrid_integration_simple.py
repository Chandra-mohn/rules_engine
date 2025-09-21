#!/usr/bin/env python3
"""
Simplified test of hybrid rules integration - focuses on source generation only.
"""

import sys
import os
import json
from pathlib import Path

# Add backend to path
sys.path.append(os.path.join(os.path.dirname(__file__)))

from services.hybrid_rules_integrator import HybridRulesIntegrator, ClientDeploymentSpec
from models import Rule, db
from app import create_app


def test_hybrid_source_generation():
    """Test just the source generation part without JAR building."""
    print("🚀 Testing Hybrid Rules Source Generation")
    print("=" * 50)

    # Fetch real rules from database
    print("1️⃣ Fetching rules from database...")
    app = create_app()

    with app.app_context():
        all_rules = Rule.query.filter(Rule.item_type.in_(['rule', 'actionset'])).limit(10).all()
        print(f"📊 Using {len(all_rules)} sample rules for testing")

        # Create simple deployment spec
        rules_data = []
        hot_rules = []

        for rule in all_rules:
            rule_data = {
                'id': str(rule.id),
                'name': rule.name,
                'content': rule.content,
                'item_type': rule.item_type,
                'status': rule.status,
                'complexity': 'simple' if len(rule.content or '') < 200 else 'complex'
            }
            rules_data.append(rule_data)

            # Make first 3 rules hot path
            if len(hot_rules) < 3 and rule_data['complexity'] == 'simple':
                hot_rules.append(rule_data['id'])

        deployment_spec = ClientDeploymentSpec(
            client_id="test_client",
            rules=rules_data,
            transaction_codes={'purchase', 'transfer'},
            daily_volume=1000000,
            hot_rules=hot_rules
        )

    print("2️⃣ Initializing hybrid integrator...")
    integrator = HybridRulesIntegrator(output_dir="/tmp/hybrid-source-test")

    try:
        print("3️⃣ Generating foundation...")
        foundation_artifacts = integrator._generate_foundation()

        print("4️⃣ Generating client rules...")
        client_artifacts = integrator._generate_client_rules(deployment_spec)

        print("5️⃣ Validating generated artifacts...")

        # Validate foundation
        foundation_classes = list(foundation_artifacts['sources'].keys())
        print(f"✅ Foundation: {len(foundation_classes)} classes generated")
        for class_path in foundation_classes[:3]:  # Show first 3
            class_name = Path(class_path).stem
            print(f"   • {class_name}")

        # Validate client
        client_classes = list(client_artifacts['sources'].keys())
        print(f"✅ Client: {len(client_classes)} classes generated")
        for class_path in client_classes[:3]:  # Show first 3
            class_name = Path(class_path).stem
            print(f"   • {class_name}")

        print("6️⃣ Realistic performance validation...")

        # Use the performance validator for realistic estimates
        from services.performance_validator import PerformanceValidator

        validator = PerformanceValidator()
        validation_result = validator.validate_hybrid_performance(
            foundation_artifacts,
            client_artifacts,
            daily_volume=1_000_000
        )

        meets_latency = validation_result.meets_latency_target
        meets_tps = validation_result.meets_tps_target

        print("7️⃣ Generated source samples:")

        # Show sample foundation code
        router_source = foundation_artifacts['sources'].get('com/rules/engine/core/UniversalTransactionRouter.java')
        if router_source:
            lines = router_source.split('\n')[:15]
            print("🔹 UniversalTransactionRouter.java (sample):")
            for i, line in enumerate(lines):
                print(f"   {i+1:2d}: {line[:80]}...")

        # Show sample client code
        first_client_source = list(client_artifacts['sources'].values())[0] if client_artifacts['sources'] else None
        if first_client_source:
            lines = first_client_source.split('\n')[:10]
            print("🔹 Sample Client Executor (sample):")
            for i, line in enumerate(lines):
                print(f"   {i+1:2d}: {line[:80]}...")

        overall_success = meets_latency and meets_tps
        print(f"\n🎯 OVERALL RESULT: {'✅ SUCCESS' if overall_success else '❌ NEEDS_OPTIMIZATION'}")

        return overall_success

    except Exception as e:
        print(f"❌ Error during generation: {e}")
        import traceback
        traceback.print_exc()
        return False


if __name__ == "__main__":
    success = test_hybrid_source_generation()

    if success:
        print("\n🎉 Hybrid source generation test PASSED!")
        print("Ready for JAR packaging and deployment!")
    else:
        print("\n💥 Hybrid source generation test FAILED!")

    sys.exit(0 if success else 1)