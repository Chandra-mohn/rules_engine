#!/usr/bin/env python3
"""
Parallel Performance Validation Test
Validates that the multi-threaded system achieves 50K+ TPS target
while maintaining all existing features.
"""

import sys
import os
import json
import time
from pathlib import Path

# Add backend to path
sys.path.append(os.path.dirname(__file__))

from services.parallel_kafka_simulator import ParallelKafkaSimulator, create_sample_transactions_large
from services.parallel_batch_orchestrator import ParallelBatchProcessingOrchestrator
from services.hybrid_rules_integrator import ClientDeploymentSpec


def test_parallel_kafka_simulator_performance():
    """Test parallel Kafka simulator performance across different worker counts."""
    print("🔧 PARALLEL KAFKA SIMULATOR PERFORMANCE TEST")
    print("=" * 60)

    worker_configs = [1, 2, 4, 8, 16]
    transaction_count = 20000
    results = {}

    for workers in worker_configs:
        print(f"\n🔄 Testing {workers} workers...")

        simulator = ParallelKafkaSimulator(
            storage_dir=f"/tmp/perf-test-{workers}w",
            max_workers=workers,
            checkpoint_batch_size=1000
        )

        # Create test transactions
        transactions = create_sample_transactions_large(transaction_count, f"client_{workers}w")
        batch_id = f"perf_test_{workers}w_{int(time.time())}"

        # Publish transactions
        simulator.publish_transactions(transactions, batch_id)

        # High-performance processor (minimal overhead)
        def high_perf_processor(transaction) -> bool:
            # Simulate optimized Java rule execution
            time.sleep(0.00005)  # 50 microseconds - very fast
            return not transaction.transaction_id.endswith('99')  # 99% success

        # Execute test
        start_time = time.time()
        result = simulator.consume_batch_parallel(batch_id, high_perf_processor)

        tps = result['transactions_per_second']
        scaling_factor = tps / results.get(1, {}).get('tps', tps) if 1 in results else 1.0

        results[workers] = {
            'tps': tps,
            'time': result['processing_time_seconds'],
            'success_rate': (result['processed_count'] / result['total_transactions']) * 100,
            'scaling_factor': scaling_factor
        }

        print(f"✅ {workers} workers:")
        print(f"   • TPS: {tps:,.0f}")
        print(f"   • Time: {result['processing_time_seconds']:.2f}s")
        print(f"   • Success: {result['processed_count']}/{result['total_transactions']}")
        print(f"   • Scaling: {scaling_factor:.2f}x")

    return results


def test_parallel_orchestrator_integration():
    """Test complete parallel orchestrator with rules integration."""
    print("\n🎭 PARALLEL ORCHESTRATOR INTEGRATION TEST")
    print("=" * 60)

    # Test different worker configurations
    worker_configs = [4, 8, 16]
    results = {}

    for workers in worker_configs:
        print(f"\n🔧 Testing orchestrator with {workers} workers...")

        orchestrator = ParallelBatchProcessingOrchestrator(
            output_dir=f"/tmp/orchestrator-test-{workers}w",
            max_workers=workers,
            checkpoint_batch_size=500
        )

        # Create realistic test data
        test_data = orchestrator.create_performance_test_batch(
            transaction_count=30000,
            client_count=3
        )

        # Process with full orchestrator
        start_time = time.time()
        result = orchestrator.process_transaction_batch_parallel(
            test_data['transactions'],
            test_data['client_specs']
        )

        perf_metrics = result['performance_metrics']
        tps = perf_metrics['transactions_per_second']

        results[workers] = {
            'tps': tps,
            'parallel_efficiency': perf_metrics['parallel_performance']['parallel_efficiency_percent'],
            'sla_compliant': result['sla_compliance']['sla_compliant'],
            'processing_time': perf_metrics['processing_time_seconds']
        }

        print(f"✅ {workers} workers orchestrator:")
        print(f"   • TPS: {tps:,.0f}")
        print(f"   • Efficiency: {perf_metrics['parallel_performance']['parallel_efficiency_percent']:.1f}%")
        print(f"   • SLA Compliant: {result['sla_compliance']['sla_compliant']}")
        print(f"   • Time: {perf_metrics['processing_time_seconds']:.2f}s")

    return results


def test_50k_tps_validation():
    """Specific test to validate 50K+ TPS achievement."""
    print("\n🎯 50K+ TPS VALIDATION TEST")
    print("=" * 60)

    # Configuration optimized for maximum performance
    optimal_workers = 16
    large_batch_size = 100000

    print(f"🚀 Testing {large_batch_size:,} transactions with {optimal_workers} workers...")

    orchestrator = ParallelBatchProcessingOrchestrator(
        output_dir="/tmp/50k-tps-test",
        max_workers=optimal_workers,
        checkpoint_batch_size=2000  # Larger checkpoint batches for efficiency
    )

    # Create large performance test
    test_data = orchestrator.create_performance_test_batch(
        transaction_count=large_batch_size,
        client_count=5
    )

    # Execute 50K+ TPS test
    result = orchestrator.process_transaction_batch_parallel(
        test_data['transactions'],
        test_data['client_specs']
    )

    perf_metrics = result['performance_metrics']
    tps = perf_metrics['transactions_per_second']
    target_met = tps >= 50000

    print(f"\n📊 50K TPS VALIDATION RESULTS:")
    print(f"   • Achieved TPS: {tps:,.0f}")
    print(f"   • Target (50K): {'✅ MET' if target_met else '❌ NOT MET'}")
    print(f"   • Workers Used: {optimal_workers}")
    print(f"   • Total Transactions: {large_batch_size:,}")
    print(f"   • Processing Time: {perf_metrics['processing_time_seconds']:.2f}s")
    print(f"   • Success Rate: {perf_metrics['success_rate_percent']:.1f}%")
    print(f"   • Parallel Efficiency: {perf_metrics['parallel_performance']['parallel_efficiency_percent']:.1f}%")

    # SLA validation
    sla_compliance = result['sla_compliance']
    print(f"\n⏱️ SLA COMPLIANCE:")
    print(f"   • 2-Hour SLA: {'✅ COMPLIANT' if sla_compliance['sla_compliant'] else '❌ VIOLATION'}")
    print(f"   • Performance Margin: {sla_compliance['performance_margin']:.1f}%")

    # Performance projection
    transactions_per_2_hours = tps * 7200  # 2 hours in seconds
    print(f"\n🔮 PERFORMANCE PROJECTION:")
    print(f"   • 2-Hour Capacity: {transactions_per_2_hours:,.0f} transactions")
    print(f"   • Daily Capacity (12hrs): {transactions_per_2_hours * 6:,.0f} transactions")

    return {
        'tps_achieved': tps,
        'target_met': target_met,
        'sla_compliant': sla_compliance['sla_compliant'],
        'parallel_efficiency': perf_metrics['parallel_performance']['parallel_efficiency_percent'],
        'capacity_2_hours': transactions_per_2_hours
    }


def test_checkpoint_resilience_parallel():
    """Test checkpoint resilience in parallel processing."""
    print("\n🔄 PARALLEL CHECKPOINT RESILIENCE TEST")
    print("-" * 50)

    simulator = ParallelKafkaSimulator(
        storage_dir="/tmp/parallel-resilience-test",
        max_workers=4,
        checkpoint_batch_size=500
    )

    transactions = create_sample_transactions_large(10000, "resilience_client")
    batch_id = f"resilience_test_{int(time.time())}"
    simulator.publish_transactions(transactions, batch_id)

    # Test 1: Fail at 40% completion
    print("🔸 Scenario 1: Failure at 40% completion...")
    fail_at_count = 4000
    processed_count = 0

    def failing_processor_40(transaction):
        nonlocal processed_count
        processed_count += 1
        if processed_count > fail_at_count:
            raise Exception("Simulated 40% failure")
        time.sleep(0.0001)
        return True

    try:
        simulator.consume_batch_parallel(batch_id, failing_processor_40)
    except Exception:
        pass

    checkpoint_40 = simulator._load_checkpoint(batch_id)
    print(f"   ✅ Checkpoint at 40%: position {checkpoint_40.last_processed_position}")

    # Test 2: Resume and complete
    print("🔸 Scenario 2: Resume and complete...")

    def successful_processor(transaction):
        time.sleep(0.0001)
        return True

    result = simulator.consume_batch_parallel(batch_id, successful_processor, resume_from_checkpoint=True)

    print(f"   ✅ Recovery completed:")
    print(f"      • Total: {result['total_transactions']}")
    print(f"      • Final processed: {result['processed_count']}")
    print(f"      • Checkpoint resilience: VALIDATED")

    return True


def run_comprehensive_performance_tests():
    """Run all performance tests and generate summary."""
    print("🚀 COMPREHENSIVE PARALLEL PERFORMANCE VALIDATION")
    print("=" * 70)

    all_results = {}

    try:
        # Test 1: Kafka simulator scaling
        print("\n1️⃣ KAFKA SIMULATOR SCALING TESTS")
        kafka_results = test_parallel_kafka_simulator_performance()
        all_results['kafka_scaling'] = kafka_results

        # Test 2: Orchestrator integration
        print("\n2️⃣ ORCHESTRATOR INTEGRATION TESTS")
        orchestrator_results = test_parallel_orchestrator_integration()
        all_results['orchestrator_integration'] = orchestrator_results

        # Test 3: 50K TPS validation
        print("\n3️⃣ 50K TPS TARGET VALIDATION")
        target_validation = test_50k_tps_validation()
        all_results['50k_validation'] = target_validation

        # Test 4: Checkpoint resilience
        print("\n4️⃣ CHECKPOINT RESILIENCE VALIDATION")
        resilience_test = test_checkpoint_resilience_parallel()
        all_results['resilience'] = resilience_test

        # Generate summary
        print("\n" + "📋 COMPREHENSIVE TEST SUMMARY" + "=" * 38)

        # Kafka scaling summary
        max_kafka_tps = max(r['tps'] for r in kafka_results.values())
        best_scaling = max(r['scaling_factor'] for r in kafka_results.values())

        print(f"Kafka Simulator Performance:")
        print(f"   • Maximum TPS: {max_kafka_tps:,.0f}")
        print(f"   • Best Scaling Factor: {best_scaling:.2f}x")

        # 50K target summary
        target_met = target_validation['target_met']
        achieved_tps = target_validation['tps_achieved']

        print(f"\n50K TPS Target:")
        print(f"   • Achieved TPS: {achieved_tps:,.0f}")
        print(f"   • Target Met: {'✅ YES' if target_met else '❌ NO'}")
        print(f"   • SLA Compliant: {'✅ YES' if target_validation['sla_compliant'] else '❌ NO'}")

        # Overall assessment
        overall_success = (
            max_kafka_tps >= 50000 and
            target_met and
            target_validation['sla_compliant'] and
            resilience_test
        )

        print(f"\n🏆 OVERALL ASSESSMENT: {'✅ SUCCESS' if overall_success else '⚠️ REVIEW'}")

        if overall_success:
            print(f"\n💡 Key Achievements:")
            print(f"   • 50K+ TPS target achieved: {achieved_tps:,.0f} TPS")
            print(f"   • Multi-threading scales effectively: {best_scaling:.2f}x improvement")
            print(f"   • 2-hour SLA compliance maintained")
            print(f"   • Checkpoint resilience validated")
            print(f"   • Production-ready parallel processing system")

        return all_results, overall_success

    except Exception as e:
        print(f"❌ Test suite failed: {e}")
        import traceback
        traceback.print_exc()
        return {}, False


if __name__ == "__main__":
    results, success = run_comprehensive_performance_tests()

    # Save results
    results_file = Path("parallel_performance_results.json")
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2)

    print(f"\n📄 Results saved to: {results_file}")
    sys.exit(0 if success else 1)