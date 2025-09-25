"""
Comprehensive test suite for UnifiedJavaCodeGenerator production architecture.
Validates compatibility, performance, and correctness across all generation modes.
Production: Single-generator architecture with integrated simple mode.
"""

import time
import unittest
from typing import Dict, List
from grammar_parser.unified_java_generator import UnifiedJavaCodeGenerator
from grammar_parser.advanced_java_generator import AdvancedJavaCodeGenerator


class TestUnifiedGeneratorCompatibility(unittest.TestCase):
    """Test compatibility with production single-generator architecture."""

    def setUp(self):
        self.unified = UnifiedJavaCodeGenerator()
        self.advanced = AdvancedJavaCodeGenerator()
        self.test_rules = [
            '''rule "Simple Credit Check":
                if applicant.creditScore > 700
                then approve("Good credit")
            ''',
            '''rule "Complex Risk Assessment":
                if applicant.creditScore > 750 and applicant.income > 50000
                then riskScore = calculateRisk(applicant.creditScore, applicant.income)
                else reject("High risk applicant")
            ''',
            '''rule "Multi-Condition Rule":
                if (applicant.age >= 18 and applicant.age <= 65) and applicant.employmentStatus == "EMPLOYED"
                then processApplication(applicant)
                else defer("Age or employment requirements not met")
            '''
        ]

    def test_api_compatibility(self):
        """Test that unified generator maintains exact API compatibility."""
        for rule_content in self.test_rules:
            with self.subTest(rule=rule_content[:50]):
                # Test method signature compatibility
                unified_result = self.unified.generate(rule_content)
                advanced_result = self.advanced.generate(rule_content)

                # Both should return strings
                self.assertIsInstance(unified_result, str)
                self.assertIsInstance(advanced_result, str)

                # Both should generate non-empty results
                self.assertTrue(len(unified_result) > 100)
                self.assertTrue(len(advanced_result) > 100)

    def test_output_functional_equivalence(self):
        """Test that unified generator produces functionally equivalent code."""
        for rule_content in self.test_rules:
            with self.subTest(rule=rule_content[:50]):
                unified_result = self.unified.generate(rule_content, "TestRule")
                advanced_result = self.advanced.generate(rule_content, "TestRule")

                # Both should contain essential Java class structure
                self.assertIn("public class", unified_result)
                self.assertIn("public class", advanced_result)

                # Both should contain rule execution logic (evaluate or execute method)
                self.assertTrue("evaluate" in unified_result.lower() or "execute" in unified_result.lower())
                self.assertTrue("evaluate" in advanced_result.lower() or "execute" in advanced_result.lower())

    def test_performance_comparison(self):
        """Test that unified generator doesn't introduce significant performance regression."""
        rule_content = self.test_rules[0]  # Simple rule
        iterations = 100

        # Measure advanced generator performance
        start_time = time.time()
        for _ in range(iterations):
            self.advanced.generate(rule_content)
        advanced_time = time.time() - start_time

        # Measure unified generator performance
        start_time = time.time()
        for _ in range(iterations):
            self.unified.generate(rule_content)
        unified_time = time.time() - start_time

        # Unified should not be more than 50% slower (allows for some overhead)
        performance_ratio = unified_time / advanced_time
        self.assertLess(performance_ratio, 1.5,
                       f"Unified generator is {performance_ratio:.2f}x slower than advanced generator")

        print(f"Performance comparison: Advanced={advanced_time:.3f}s, Unified={unified_time:.3f}s, "
              f"Ratio={performance_ratio:.2f}x")

    def test_mode_selection(self):
        """Test different generation modes."""
        rule_content = self.test_rules[1]

        # Test simple mode
        unified_simple = UnifiedJavaCodeGenerator(mode='simple')
        result_simple = unified_simple.generate(rule_content)
        self.assertIsInstance(result_simple, str)

        # Test advanced mode
        unified_advanced = UnifiedJavaCodeGenerator(mode='advanced')
        result_advanced = unified_advanced.generate(rule_content)
        self.assertIsInstance(result_advanced, str)

        # Test auto mode (default)
        unified_auto = UnifiedJavaCodeGenerator(mode='auto')
        result_auto = unified_auto.generate(rule_content)
        self.assertIsInstance(result_auto, str)

    def test_advanced_api(self):
        """Test advanced API functionality."""
        rule_content = self.test_rules[2]

        result = self.unified.generate_advanced(rule_content, "TestRule", 'auto')

        # Should return OptimizedJavaCode object
        self.assertIsNotNone(result)
        self.assertTrue(hasattr(result, 'java_code'))
        self.assertTrue(hasattr(result, 'performance_category'))
        self.assertIsInstance(result.java_code, str)


class TestUnifiedGeneratorPerformance(unittest.TestCase):
    """Performance benchmarking for unified generator."""

    def setUp(self):
        self.unified = UnifiedJavaCodeGenerator()
        self.performance_rules = [
            # Simple rule
            'rule "Simple": if x > 5 then approve()',
            # Medium complexity
            '''rule "Medium":
                if applicant.creditScore > 700 and applicant.income > 30000
                then approve("Standard approval")
                else review("Needs manual review")
            ''',
            # High complexity
            '''rule "Complex":
                if (applicant.creditScore > 750 and applicant.income > 50000) or
                   (applicant.creditScore > 650 and applicant.income > 75000 and applicant.employmentYears > 2)
                then riskScore = calculateRisk(applicant) and processApplication(applicant)
                else reject("Complex criteria not met")
            '''
        ]

    def test_performance_benchmarks(self):
        """Benchmark unified generator performance."""
        results = {}

        for i, rule in enumerate(self.performance_rules):
            rule_name = f"Rule_{i+1}"

            start_time = time.time()
            for _ in range(50):
                self.unified.generate(rule, rule_name)
            total_time = time.time() - start_time
            avg_time = total_time / 50

            results[rule_name] = avg_time

            # Performance should be under reasonable thresholds
            self.assertLess(avg_time, 0.1, f"{rule_name} generation took {avg_time:.3f}s (too slow)")

        print("Performance Benchmarks:")
        for rule_name, avg_time in results.items():
            print(f"  {rule_name}: {avg_time:.4f}s average")


def run_production_validation():
    """Run comprehensive validation for production architecture."""
    print("=" * 60)
    print("UNIFIED JAVA CODE GENERATOR - PRODUCTION VALIDATION")
    print("=" * 60)

    # Run all tests
    unittest.main(verbosity=2, exit=False)

    print("\n" + "=" * 60)
    print("PRODUCTION VALIDATION COMPLETE")
    print("=" * 60)


if __name__ == '__main__':
    run_production_validation()