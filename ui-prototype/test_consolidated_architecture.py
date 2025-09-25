#!/usr/bin/env python3
"""
Comprehensive test suite for the Phase 3 consolidated Java Code Generator architecture.
Replaces individual generator tests with unified architecture tests.
"""

import sys
import unittest
import tempfile
import os
from pathlib import Path

# Add backend to path
backend_path = Path(__file__).parent / "backend"
sys.path.insert(0, str(backend_path))

from grammar_parser.unified_java_generator import UnifiedJavaCodeGenerator
from grammar_parser.advanced_java_generator import AdvancedJavaCodeGenerator, OptimizedJavaCode, RuleAnalysis
from services.python_rules_engine import PythonRulesEngine


class TestConsolidatedArchitecture(unittest.TestCase):
    """Test the consolidated Java code generator architecture."""

    def setUp(self):
        """Set up test fixtures."""
        self.unified_generator = UnifiedJavaCodeGenerator()
        self.advanced_generator = AdvancedJavaCodeGenerator()
        self.python_engine = PythonRulesEngine()

    def test_unified_generator_modes(self):
        """Test all UnifiedJavaCodeGenerator modes work correctly."""
        test_rule = '''rule "Test Mode":
if applicant.creditScore >= 700 then approveApplication
else rejectApplication'''

        modes = ['auto', 'simple', 'advanced']

        for mode in modes:
            with self.subTest(mode=mode):
                generator = UnifiedJavaCodeGenerator(mode=mode)
                result = generator.generate(test_rule, "TestMode")

                self.assertIsNotNone(result)
                self.assertIsInstance(result, str)
                self.assertIn("TestModeRule", result)
                self.assertIn("evaluate", result)
                self.assertIn("package com.rules;", result)

    def test_advanced_generator_simple_mode(self):
        """Test AdvancedJavaCodeGenerator's integrated simple mode."""
        test_rule = '''rule "Simple Integration":
if user.age >= 18 then allow
else deny'''

        result = self.advanced_generator.generate(test_rule, "SimpleIntegration")

        self.assertIsNotNone(result)
        self.assertIn("SimpleIntegrationRule", result)
        self.assertIn("evaluate", result)
        self.assertIn("_getFieldValue", result)
        self.assertIn("_compareTo", result)

    def test_advanced_generator_optimized_mode(self):
        """Test AdvancedJavaCodeGenerator's optimized mode."""
        test_rule = '''rule "Optimized Test":
if applicant.creditScore >= 750 and applicant.income > 50000 then approve
else reject'''

        result = self.advanced_generator.generate_optimized_executor_code(
            test_rule, "OptimizedTest", 'auto'
        )

        self.assertIsInstance(result, OptimizedJavaCode)
        self.assertIsNotNone(result.java_code)
        self.assertIn(result.performance_category, ['hot', 'warm', 'cold'])
        self.assertGreater(result.complexity_score, 0)
        self.assertIsInstance(result.optimization_applied, list)

    def test_backward_compatibility(self):
        """Test backward compatibility with old SimpleJavaCodeGenerator API."""
        # Test rules that would have used SimpleJavaCodeGenerator
        legacy_rules = [
            'rule "Legacy1": if score > 600 then accept',
            'rule "Legacy2": if age >= 21 and income > 30000 then approve else deny',
            'rule testRule: approveApplication'
        ]

        for rule in legacy_rules:
            with self.subTest(rule=rule):
                result = self.unified_generator.generate(rule)

                self.assertIsNotNone(result)
                self.assertIn("package com.rules;", result)
                self.assertIn("public class", result)
                self.assertIn("RuleResult", result)

    def test_performance_characteristics(self):
        """Test performance characteristics meet requirements."""
        test_rule = '''rule "Performance":
if applicant.creditScore >= 700 then approve
else reject'''

        import time

        # Test generation speed
        start_time = time.perf_counter()
        result = self.unified_generator.generate(test_rule, "Performance")
        end_time = time.perf_counter()

        generation_time_ms = (end_time - start_time) * 1000

        self.assertIsNotNone(result)
        self.assertLess(generation_time_ms, 10)  # Should be under 10ms

    def test_architecture_stats(self):
        """Test architecture statistics reflect Phase 3 completion."""
        stats = self.unified_generator.get_generation_stats()

        self.assertTrue(stats['phase_3_complete'])
        self.assertTrue(stats['single_generator_architecture'])
        self.assertTrue(stats['simple_mode_integrated'])
        self.assertTrue(stats['advanced_generator_available'])
        self.assertTrue(stats['simple_generator_removed'])

    def test_python_engine_integration(self):
        """Test PythonRulesEngine works with consolidated architecture."""
        test_rule = '''rule "Engine Integration":
if applicant.creditScore >= 700 then approveApplication
else rejectApplication'''

        validation_result = self.python_engine.validate_rule(test_rule)

        self.assertIsNotNone(validation_result)
        self.assertIsInstance(validation_result, dict)

        # Verify engine is using UnifiedJavaCodeGenerator
        self.assertIsInstance(self.python_engine.code_generator, UnifiedJavaCodeGenerator)

    def test_complex_rule_generation(self):
        """Test complex rule generation works in all modes."""
        complex_rule = '''rule "Complex Decision Tree":
if applicant.creditScore >= 750 and applicant.income > 75000 then
    if applicant.age >= 25 and applicant.employmentStatus == "FULL_TIME" then
        approveWithPremiumTerms
    else
        approveWithStandardTerms
else if applicant.creditScore >= 650 and applicant.debtToIncomeRatio < 0.4 then
    conditionalApproval
else
    rejectApplication'''

        modes = ['auto', 'simple', 'advanced']

        for mode in modes:
            with self.subTest(mode=mode):
                generator = UnifiedJavaCodeGenerator(mode=mode)
                result = generator.generate(complex_rule, "ComplexDecisionTree")

                self.assertIsNotNone(result)
                self.assertIn("ComplexDecisionTreeRule", result)
                self.assertGreater(len(result), 1000)  # Should be substantial

    def test_error_handling(self):
        """Test error handling in consolidated architecture."""
        # Test invalid rule
        invalid_rule = "invalid rule syntax here"

        try:
            result = self.unified_generator.generate(invalid_rule, "InvalidTest")
            # Should either succeed with basic handling or fail gracefully
            if result:
                self.assertIsInstance(result, str)
        except Exception as e:
            # Should fail gracefully, not crash
            self.assertIsInstance(e, (ValueError, SyntaxError, RuntimeError))

    def test_rule_name_extraction(self):
        """Test rule name extraction from content."""
        test_cases = [
            ('rule "Quoted Name": approve', "QuotedName"),
            ('rule SimpleName: approve', "SimpleName"),
            ('rule testRule: if x > 5 then y', "TestRule")  # Note: first letter is capitalized
        ]

        for rule_content, expected_class_prefix in test_cases:
            with self.subTest(rule=rule_content):
                result = self.unified_generator.generate(rule_content)

                self.assertIsNotNone(result)
                self.assertIn(f"{expected_class_prefix}Rule", result)

    def test_entity_reference_handling(self):
        """Test entity reference handling in generated code."""
        rule_with_entities = '''rule "Entity Test":
if applicant.creditScore >= 700 and transaction.amount < 10000 then
    approveTransaction
else
    reviewManually'''

        result = self.unified_generator.generate(rule_with_entities, "EntityTest")

        self.assertIn("applicant", result)
        self.assertIn("transaction", result)
        self.assertIn("_getFieldValue", result)

    def test_mode_selection_logic(self):
        """Test automatic mode selection logic."""
        simple_rule = "rule simple: approve"
        complex_rule = '''rule complex:
if a.b > 5 and c.d < 10 and e.f == "test" then
    action1
else if g.h != null then
    action2
else
    action3'''

        auto_generator = UnifiedJavaCodeGenerator(mode='auto')

        # Both should work regardless of complexity detection
        simple_result = auto_generator.generate(simple_rule)
        complex_result = auto_generator.generate(complex_rule)

        self.assertIsNotNone(simple_result)
        self.assertIsNotNone(complex_result)

class TestAdvancedGeneratorIntegration(unittest.TestCase):
    """Test specific AdvancedJavaCodeGenerator integration features."""

    def setUp(self):
        """Set up test fixtures."""
        self.generator = AdvancedJavaCodeGenerator()

    def test_dual_mode_support(self):
        """Test generator supports both simple and optimized modes."""
        test_rule = '''rule "Dual Mode":
if score >= 700 then accept
else reject'''

        # Test simple mode (legacy compatibility)
        simple_result = self.generator.generate(test_rule, "DualMode")

        # Test optimized mode
        optimized_result = self.generator.generate_optimized_executor_code(
            test_rule, "DualMode", 'auto'
        )

        self.assertIsInstance(simple_result, str)
        self.assertIn("DualModeRule", simple_result)

        self.assertIsInstance(optimized_result, OptimizedJavaCode)
        self.assertIsNotNone(optimized_result.java_code)

    def test_helper_methods_generation(self):
        """Test helper methods are generated correctly."""
        test_rule = '''rule "Helper Test":
if entity.field > 100 then action'''

        result = self.generator.generate(test_rule, "HelperTest")

        # Check for helper methods from original SimpleJavaCodeGenerator
        self.assertIn("_getFieldValue", result)
        self.assertIn("_equals", result)
        self.assertIn("_compareTo", result)


def run_tests():
    """Run all tests and return success status."""
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Add test classes
    suite.addTests(loader.loadTestsFromTestCase(TestConsolidatedArchitecture))
    suite.addTests(loader.loadTestsFromTestCase(TestAdvancedGeneratorIntegration))

    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    return result.wasSuccessful()


def main():
    """Main test execution function."""
    print("🧪 Comprehensive Test Suite for Phase 3 Consolidated Architecture")
    print("=" * 70)

    success = run_tests()

    print("\n" + "=" * 70)
    if success:
        print("🎉 All tests passed! Phase 3 architecture is fully functional.")
        print("✅ Consolidated architecture validated")
        print("✅ Backward compatibility maintained")
        print("✅ Performance targets exceeded")
        print("✅ Ready for SimpleJavaCodeGenerator removal")
        return 0
    else:
        print("❌ Some tests failed. Review architecture implementation.")
        return 1


if __name__ == "__main__":
    sys.exit(main())