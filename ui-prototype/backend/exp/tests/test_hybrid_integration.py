#!/usr/bin/env python3
"""
Test the hybrid rules integration system with actual database rules.
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


def fetch_database_rules():
    """Fetch actual rules from database."""
    app = create_app()

    with app.app_context():
        # Get all rules from database
        all_rules = Rule.query.filter(Rule.item_type.in_(['rule', 'actionset'])).all()

        print(f"📊 Found {len(all_rules)} rules in database")

        # Organize by client (using process_group as client identifier)
        clients = {}
        for rule in all_rules:
            # Convert ProcessArea to clean client ID
            if rule.process_area:
                client_id = f"client_{rule.process_area.id}" if hasattr(rule.process_area, 'id') else f"client_{hash(str(rule.process_area)) % 1000}"
            else:
                client_id = 'default_client'
            if client_id not in clients:
                clients[client_id] = []

            clients[client_id].append({
                'id': str(rule.id),
                'name': rule.name,
                'content': rule.content,
                'item_type': rule.item_type,
                'status': rule.status,
                'complexity': 'simple' if len(rule.content or '') < 200 else 'complex'
            })

        return clients


def create_deployment_specs_from_db(clients_data):
    """Convert database rules into deployment specifications."""
    specs = []

    for client_id, rules in clients_data.items():
        # Determine hot rules (simple rules with basic conditions)
        hot_rules = [
            rule['id'] for rule in rules
            if rule['complexity'] == 'simple' and 'amount' in (rule['content'] or '')
        ]

        # Estimate transaction codes from rule content
        transaction_codes = set()
        for rule in rules:
            content = rule['content'] or ''
            if 'transaction' in content.lower():
                transaction_codes.add('transaction')
            if 'purchase' in content.lower():
                transaction_codes.add('purchase')
            if 'transfer' in content.lower():
                transaction_codes.add('transfer')
            if 'withdrawal' in content.lower():
                transaction_codes.add('withdrawal')

        if not transaction_codes:
            transaction_codes = {'transaction'}  # Default

        # Estimate daily volume based on client size
        daily_volume = len(rules) * 100000  # 100K per rule as baseline

        spec = ClientDeploymentSpec(
            client_id=client_id,
            rules=rules,
            transaction_codes=transaction_codes,
            daily_volume=daily_volume,
            hot_rules=hot_rules
        )

        specs.append(spec)

    return specs


def test_hybrid_system():
    """Test the complete hybrid system with database rules."""
    print("🚀 Testing Hybrid Rules Integration System")
    print("=" * 50)

    # Fetch real rules from database
    print("1️⃣ Fetching rules from database...")
    clients_data = fetch_database_rules()

    if not clients_data:
        print("❌ No rules found in database")
        return False

    # Create deployment specifications
    print("2️⃣ Creating deployment specifications...")
    deployment_specs = create_deployment_specs_from_db(clients_data)

    print(f"📋 Created specs for {len(deployment_specs)} clients:")
    for spec in deployment_specs:
        print(f"   • {spec.client_id}: {len(spec.rules)} rules, {len(spec.hot_rules)} hot")

    # Initialize integrator
    print("3️⃣ Initializing hybrid integrator...")
    integrator = HybridRulesIntegrator(output_dir="/tmp/hybrid-test")

    try:
        # Generate complete system
        print("4️⃣ Generating hybrid system...")
        artifacts = integrator.generate_complete_system(deployment_specs)

        # Validate performance
        print("5️⃣ Validating performance...")
        validation = integrator.validate_system_performance(artifacts)

        # Report results
        print("\n" + "🎯 RESULTS" + "=" * 43)
        print(f"Foundation JAR: {artifacts.foundation_jar_path}")
        print(f"Client JARs: {len(artifacts.client_jar_paths)}")

        for client_id, jar_path in artifacts.client_jar_paths.items():
            print(f"  • {client_id}: {jar_path}")

        print(f"\n📊 Performance Validation: {validation['overall_status'].upper()}")

        for metric, data in validation['performance_targets'].items():
            status_emoji = "✅" if data['status'] == 'pass' else "❌"
            print(f"  {status_emoji} {metric}: {data['estimated']:.1f} (target: {data['target']})")

        # Save results
        results_file = Path("/tmp/hybrid-test/integration_results.json")
        results_file.write_text(json.dumps({
            'deployment_specs': [
                {
                    'client_id': spec.client_id,
                    'rules_count': len(spec.rules),
                    'hot_rules_count': len(spec.hot_rules),
                    'daily_volume': spec.daily_volume,
                    'transaction_codes': list(spec.transaction_codes)
                }
                for spec in deployment_specs
            ],
            'artifacts': {
                'foundation_jar': str(artifacts.foundation_jar_path),
                'client_jars': {k: str(v) for k, v in artifacts.client_jar_paths.items()},
                'generated_sources': {k: str(v) for k, v in artifacts.generated_source_paths.items()}
            },
            'validation': validation
        }, indent=2))

        print(f"\n💾 Results saved to: {results_file}")

        return validation['overall_status'] == 'pass'

    except Exception as e:
        print(f"❌ Error during integration: {e}")
        import traceback
        traceback.print_exc()
        return False


def show_generated_code_samples():
    """Show samples of generated code for verification."""
    foundation_dir = Path("/tmp/hybrid-test/foundation")

    if foundation_dir.exists():
        print("\n📁 Generated Foundation Code Samples:")
        print("-" * 40)

        # Show UniversalTransactionRouter sample
        router_file = foundation_dir / "UniversalTransactionRouter.java"
        if router_file.exists():
            content = router_file.read_text()
            lines = content.split('\n')
            print("🔹 UniversalTransactionRouter.java (first 20 lines):")
            for i, line in enumerate(lines[:20]):
                print(f"   {i+1:2d}: {line}")
            print("   ...")

    # Show client code samples
    client_dirs = [d for d in Path("/tmp/hybrid-test").iterdir() if d.is_dir() and d.name.startswith("client_")]

    if client_dirs:
        sample_client = client_dirs[0]
        print(f"\n📁 Generated Client Code Sample ({sample_client.name}):")
        print("-" * 40)

        java_files = list(sample_client.glob("*.java"))
        if java_files:
            sample_file = java_files[0]
            content = sample_file.read_text()
            lines = content.split('\n')
            print(f"🔹 {sample_file.name} (first 15 lines):")
            for i, line in enumerate(lines[:15]):
                print(f"   {i+1:2d}: {line}")
            print("   ...")


if __name__ == "__main__":
    success = test_hybrid_system()

    if success:
        print("\n🎉 Hybrid integration test PASSED!")
        show_generated_code_samples()
    else:
        print("\n💥 Hybrid integration test FAILED!")

    sys.exit(0 if success else 1)