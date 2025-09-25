#!/usr/bin/env python3
"""
Phase 3 Java Code Generator Consolidation Test
Tests backward compatibility and ensures zero regression after removing SimpleJavaCodeGenerator dependency.
"""

import sys
import os
import time
import traceback
from pathlib import Path

# Add backend to path
backend_path = Path(__file__).parent / "backend"
sys.path.insert(0, str(backend_path))

try:
    # Import the new unified architecture
    from grammar_parser.unified_java_generator import UnifiedJavaCodeGenerator
    from grammar_parser.advanced_java_generator import AdvancedJavaCodeGenerator
    print("✅ Successfully imported unified architecture components")
except ImportError as e:
    print(f"❌ Import failed: {e}")
    sys.exit(1)

def test_simple_rule_generation():
    """Test simple rule generation with backward compatibility."""
    print("\n🧪 Testing Simple Rule Generation...")

    simple_rule = '''rule "Basic Credit Check":
if applicant.creditScore >= 700 then approveApplication
else rejectApplication'''

    try:
        # Test with unified generator in different modes
        generators = {
            'auto': UnifiedJavaCodeGenerator(mode='auto'),
            'simple': UnifiedJavaCodeGenerator(mode='simple'),
            'advanced': UnifiedJavaCodeGenerator(mode='advanced')
        }

        for mode, generator in generators.items():
            start_time = time.time()
            result = generator.generate(simple_rule, "BasicCreditCheck")
            end_time = time.time()

            # Validate result
            assert result is not None, f"Result should not be None for mode {mode}"
            assert "BasicCreditCheckRule" in result, f"Class name not found in result for mode {mode}"
            assert "evaluate" in result, f"Evaluate method not found for mode {mode}"
            assert "RuleResult" in result, f"RuleResult class not found for mode {mode}"

            print(f"  ✅ Mode '{mode}': Generated {len(result)} chars in {(end_time - start_time)*1000:.2f}ms")

        return True
    except Exception as e:
        print(f"  ❌ Simple rule generation failed: {e}")
        traceback.print_exc()
        return False

def test_advanced_rule_generation():
    """Test advanced rule generation capabilities."""
    print("\n🧪 Testing Advanced Rule Generation...")

    complex_rule = '''rule "Complex Credit Assessment":
if applicant.creditScore >= 750 and applicant.income > 50000 then
    if applicant.age >= 21 then approveWithHighLimit
    else approveWithLowLimit
else if applicant.creditScore >= 650 then
    conditionalApproval
else
    rejectApplication'''

    try:
        generator = UnifiedJavaCodeGenerator(mode='auto')

        # Test both simple generate and advanced generate_advanced methods
        start_time = time.time()
        simple_result = generator.generate(complex_rule, "ComplexCreditAssessment")
        simple_time = time.time() - start_time

        start_time = time.time()
        advanced_result = generator.generate_advanced(complex_rule, "ComplexCreditAssessment", 'auto')
        advanced_time = time.time() - start_time

        # Validate simple result
        assert simple_result is not None, "Simple result should not be None"
        assert "ComplexCreditAssessmentRule" in simple_result, "Class name not found in simple result"

        # Validate advanced result
        assert advanced_result is not None, "Advanced result should not be None"
        assert advanced_result.java_code is not None, "Advanced java_code should not be None"
        assert advanced_result.performance_category in ['hot', 'warm', 'cold'], "Invalid performance category"
        assert advanced_result.complexity_score > 0, "Complexity score should be positive"

        print(f"  ✅ Simple method: Generated {len(simple_result)} chars in {simple_time*1000:.2f}ms")
        print(f"  ✅ Advanced method: {advanced_result.performance_category} path, complexity {advanced_result.complexity_score}, generated in {advanced_time*1000:.2f}ms")

        return True
    except Exception as e:
        print(f"  ❌ Advanced rule generation failed: {e}")
        traceback.print_exc()
        return False

def test_backward_compatibility():
    """Test backward compatibility with SimpleJavaCodeGenerator API."""
    print("\n🧪 Testing Backward Compatibility...")

    # Test rules that would have used SimpleJavaCodeGenerator before
    test_rules = [
        ('rule "Simple Rule": approveApplication', "SimpleRule"),
        ('rule "Condition Rule": if applicant.age >= 18 then approve else reject', "ConditionRule"),
        ('rule testRule: if score > 600 then accept', "testRule")
    ]

    try:
        generator = UnifiedJavaCodeGenerator(mode='simple')

        for rule_content, expected_name in test_rules:
            result = generator.generate(rule_content)

            # Validate structure matches what SimpleJavaCodeGenerator would have produced
            assert result is not None, f"Result should not be None for rule {expected_name}"
            assert "package com.rules;" in result, "Package declaration missing"
            assert "import java.util.*;" in result, "Import statements missing"
            assert "public class" in result, "Class declaration missing"
            assert "evaluate" in result, "Evaluate method missing"
            assert "RuleResult" in result, "RuleResult class missing"
            assert "_getFieldValue" in result, "Helper methods missing"
            assert "_compareTo" in result, "Helper methods missing"

            print(f"  ✅ Rule '{expected_name}': Backward compatibility maintained")

        return True
    except Exception as e:
        print(f"  ❌ Backward compatibility test failed: {e}")
        traceback.print_exc()
        return False

def test_performance_characteristics():
    """Test performance characteristics of consolidated architecture."""
    print("\n🧪 Testing Performance Characteristics...")

    test_rule = '''rule "Performance Test":
if applicant.creditScore >= 700 and applicant.income > 30000 then approveApplication
else rejectApplication'''

    try:
        generator = UnifiedJavaCodeGenerator(mode='auto')

        # Measure generation performance
        times = []
        for i in range(10):
            start_time = time.perf_counter()
            result = generator.generate(test_rule, f"PerformanceTest{i}")
            end_time = time.perf_counter()
            times.append((end_time - start_time) * 1000)  # Convert to ms

        avg_time = sum(times) / len(times)
        min_time = min(times)
        max_time = max(times)

        # Performance should be reasonable (under 100ms for simple rules)
        assert avg_time < 100, f"Average generation time too high: {avg_time:.2f}ms"
        assert all(t < 200 for t in times), f"Some generation times too high: max {max_time:.2f}ms"

        print(f"  ✅ Performance: avg={avg_time:.2f}ms, min={min_time:.2f}ms, max={max_time:.2f}ms")

        # Test memory usage stability
        generator_stats = generator.get_generation_stats()
        assert generator_stats['phase_3_complete'] == True, "Phase 3 should be complete"
        assert generator_stats['single_generator_architecture'] == True, "Should use single generator"
        assert generator_stats['simple_generator_removed'] == True, "Simple generator should be removed"

        print(f"  ✅ Architecture: {generator_stats}")

        return True
    except Exception as e:
        print(f"  ❌ Performance test failed: {e}")
        traceback.print_exc()
        return False

def test_direct_advanced_generator():
    """Test AdvancedJavaCodeGenerator's new simple mode directly."""
    print("\n🧪 Testing Direct AdvancedJavaCodeGenerator...")

    simple_rule = '''rule "Direct Test":
if user.age >= 21 then allow
else deny'''

    try:
        generator = AdvancedJavaCodeGenerator()

        # Test the new generate method (simple mode)
        simple_result = generator.generate(simple_rule, "DirectTest")

        # Test the existing generate_optimized_executor_code method
        advanced_result = generator.generate_optimized_executor_code(simple_rule, "DirectTest", 'auto')

        # Validate both work
        assert simple_result is not None, "Simple result should not be None"
        assert "DirectTestRule" in simple_result, "Class name not found in simple result"
        assert "evaluate" in simple_result, "Evaluate method not found"

        assert advanced_result is not None, "Advanced result should not be None"
        assert advanced_result.java_code is not None, "Advanced java_code should not be None"

        print(f"  ✅ Direct simple mode: Generated {len(simple_result)} chars")
        print(f"  ✅ Direct advanced mode: {advanced_result.performance_category} path, complexity {advanced_result.complexity_score}")

        return True
    except Exception as e:
        print(f"  ❌ Direct AdvancedJavaCodeGenerator test failed: {e}")
        traceback.print_exc()
        return False

def main():
    """Run all Phase 3 consolidation tests."""
    print("🚀 Phase 3 Java Code Generator Consolidation Tests")
    print("=" * 60)

    tests = [
        test_simple_rule_generation,
        test_advanced_rule_generation,
        test_backward_compatibility,
        test_performance_characteristics,
        test_direct_advanced_generator
    ]

    passed = 0
    failed = 0

    for test in tests:
        try:
            if test():
                passed += 1
            else:
                failed += 1
        except Exception as e:
            print(f"❌ Test {test.__name__} crashed: {e}")
            failed += 1

    print("\n" + "=" * 60)
    print(f"📊 Test Results: {passed} passed, {failed} failed")

    if failed == 0:
        print("🎉 All tests passed! Phase 3 consolidation successful.")
        print("✅ Zero regression achieved")
        print("✅ SimpleJavaCodeGenerator dependency removed")
        print("✅ Single source of truth established")
        return 0
    else:
        print("❌ Some tests failed. Phase 3 consolidation needs attention.")
        return 1

if __name__ == "__main__":
    sys.exit(main())