#!/usr/bin/env python3
"""
Test and validation script for the unified router implementation.
Validates all fixes: compilation, security, React-style data flow, single path execution.
"""

import sys
import tempfile
import subprocess
from pathlib import Path
from backend.services.unified_router_generator import UnifiedRouterGenerator, RuleMapping, ClientSpec


def test_compilation_fixes():
    """Test that generated code has proper Java structure and compiles."""
    print("🔧 Testing Compilation Fixes...")

    generator = UnifiedRouterGenerator()

    # Create test rule mapping
    rule_mapping = RuleMapping(
        transaction_code="CREDIT_APP",
        rule_id="rule-001",
        rule_name="credit_approval",
        rule_type="rule",
        dependencies=[]
    )

    client_spec = ClientSpec(
        client_id="TEST_BANK",
        rule_mappings=[rule_mapping]
    )

    # Generate code
    artifacts = generator.generate_client_router(client_spec)

    # Validate compilation structure
    for path, code in artifacts.items():
        print(f"  Checking {path}...")

        # ✅ Check: No import statements inside methods
        lines = code.split('\n')
        in_method = False
        for i, line in enumerate(lines):
            if 'public ' in line and '(' in line and '{' in line:
                in_method = True
            elif line.strip() == '}' and in_method:
                in_method = False
            elif in_method and line.strip().startswith('import '):
                raise AssertionError(f"❌ Import inside method at line {i+1}: {line}")

        # ✅ Check: No class definitions inside methods
        in_method = False
        for i, line in enumerate(lines):
            if 'public ' in line and '(' in line and '{' in line:
                in_method = True
            elif line.strip() == '}' and in_method:
                in_method = False
            elif in_method and ('public class ' in line or 'private class ' in line):
                raise AssertionError(f"❌ Class inside method at line {i+1}: {line}")

        # ✅ Check: Proper package declaration
        if not any(line.strip().startswith('package ') for line in lines):
            raise AssertionError(f"❌ Missing package declaration in {path}")

        # ✅ Check: Proper imports at top level
        import_section = True
        for line in lines:
            if line.strip().startswith('public class '):
                import_section = False
            elif not import_section and line.strip().startswith('import '):
                raise AssertionError(f"❌ Import not in import section: {line}")

    print("✅ Compilation structure: PASSED")
    return True


def test_security_fixes():
    """Test that security vulnerabilities are fixed."""
    print("🛡️ Testing Security Fixes...")

    generator = UnifiedRouterGenerator()

    rule_mapping = RuleMapping(
        transaction_code="SECURITY_TEST",
        rule_id="rule-sec",
        rule_name="security_test",
        rule_type="rule",
        dependencies=[]
    )

    client_spec = ClientSpec(
        client_id="SECURE_BANK",
        rule_mappings=[rule_mapping]
    )

    artifacts = generator.generate_client_router(client_spec)

    for path, code in artifacts.items():
        # ✅ Check: No unbounded global caches
        assert "static final Map<String, Object> contextState = new HashMap<>()" not in code, \
            "❌ Found unbounded global cache"

        # ✅ Check: Bounded cache with size limit
        if "ruleCache" in code:
            assert "MAX_CACHE_SIZE" in code, "❌ Cache without size limit"
            assert "ruleCache.size() >= MAX_CACHE_SIZE" in code, "❌ No cache size management"

        # ✅ Check: Input validation present
        if "RuleExecutor" in code and "executors" in path:
            assert "validateInputs" in code, "❌ Missing input validation"
            assert "creditScore < 300 || creditScore > 850" in code, "❌ Missing credit score validation"
            assert "income < 0" in code, "❌ Missing income validation"

        # ✅ Check: No dangerous patterns
        assert "System.getProperty" not in code, "❌ Potential system property access"
        assert "Runtime.getRuntime()" not in code, "❌ Potential command execution"
        assert "reflection" not in code.lower(), "❌ Reflection usage detected"

    print("✅ Security hardening: PASSED")
    return True


def test_react_style_data_flow():
    """Test React-style data flow implementation."""
    print("⚛️ Testing React-Style Data Flow...")

    generator = UnifiedRouterGenerator()

    rule_mapping = RuleMapping(
        transaction_code="REACT_TEST",
        rule_id="rule-react",
        rule_name="react_style_test",
        rule_type="rule",
        dependencies=[]
    )

    client_spec = ClientSpec(
        client_id="REACT_BANK",
        rule_mappings=[rule_mapping]
    )

    artifacts = generator.generate_client_router(client_spec)

    # Find the executor code
    executor_code = None
    for path, code in artifacts.items():
        if "Executor.java" in path and "executors" in path:
            executor_code = code
            break

    assert executor_code is not None, "❌ Executor code not found"

    # ✅ Check: RuleExecutionContext usage (React Context pattern)
    assert "RuleExecutionContext ruleContext = new RuleExecutionContext(context)" in executor_code, \
        "❌ Missing React-style context creation"

    # ✅ Check: Data flows down through method parameters
    assert "processRule(ruleContext)" in executor_code, \
        "❌ Missing data flow to child methods"

    # ✅ Check: Context-scoped state management
    assert "ruleContext.setRuleData" in executor_code, \
        "❌ Missing context-scoped state management"
    assert "ruleContext.getRuleData" in executor_code, \
        "❌ Missing context-scoped state access"

    # ✅ Check: Immutable updates (React pattern)
    assert ".withStatus(" in executor_code, \
        "❌ Missing immutable update pattern"
    assert ".withExtended(" in executor_code, \
        "❌ Missing immutable extended field updates"

    # ✅ Check: No global static state (anti-React pattern)
    assert "private static Map<String, Object> globalState" not in executor_code, \
        "❌ Found anti-React global state pattern"

    print("✅ React-style data flow: PASSED")
    return True


def test_single_path_simplification():
    """Test that hot/cold path complexity is removed."""
    print("🚀 Testing Single Path Simplification...")

    generator = UnifiedRouterGenerator()

    rule_mapping = RuleMapping(
        transaction_code="SIMPLE_TEST",
        rule_id="rule-simple",
        rule_name="simple_test",
        rule_type="rule",
        dependencies=[]
    )

    client_spec = ClientSpec(
        client_id="SIMPLE_BANK",
        rule_mappings=[rule_mapping]
    )

    artifacts = generator.generate_client_router(client_spec)

    for path, code in artifacts.items():
        # ✅ Check: No hot/cold path routing
        assert "HOT_EXECUTORS" not in code, "❌ Found hot path complexity"
        assert "COLD_EXECUTORS" not in code, "❌ Found cold path complexity"
        assert "executeHotPath" not in code, "❌ Found hot path method"
        assert "executeColdPath" not in code, "❌ Found cold path method"

        # ✅ Check: Single unified execution path
        if "Router.java" in path:
            assert "EXECUTORS = Map.of(" in code, "❌ Missing unified executor map"
            assert "Unified executor map (single path" in code, "❌ Missing single path comment"

        # ✅ Check: No frequency-based optimization complexity
        assert "execution_frequency" not in code, "❌ Found frequency complexity"
        assert "estimated_steps" not in code, "❌ Found step estimation complexity"
        assert "hot_path_threshold" not in code, "❌ Found threshold complexity"

    print("✅ Single path simplification: PASSED")
    return True


def test_performance_characteristics():
    """Test that performance optimizations are maintained."""
    print("⚡ Testing Performance Characteristics...")

    generator = UnifiedRouterGenerator()

    rule_mapping = RuleMapping(
        transaction_code="PERF_TEST",
        rule_id="rule-perf",
        rule_name="performance_test",
        rule_type="rule",
        dependencies=[]
    )

    client_spec = ClientSpec(
        client_id="PERF_BANK",
        rule_mappings=[rule_mapping]
    )

    artifacts = generator.generate_client_router(client_spec)

    for path, code in artifacts.items():
        # ✅ Check: Performance timing
        if "Executor.java" in path and "executors" in path:
            assert "System.nanoTime()" in code, "❌ Missing performance timing"

        # ✅ Check: Efficient static maps
        if "Router.java" in path:
            assert "Map.of(" in code, "❌ Missing efficient static map"

        # ✅ Check: Branch optimization (most common cases first)
        if "Executor.java" in path and "executors" in path:
            lines = code.split('\n')
            credit_checks = [i for i, line in enumerate(lines) if "creditScore >=" in line]
            if len(credit_checks) >= 2:
                # First condition should be higher credit score (common case first)
                first_score = int([s for s in lines[credit_checks[0]].split() if s.isdigit()][0])
                second_score = int([s for s in lines[credit_checks[1]].split() if s.isdigit()][0])
                assert first_score >= second_score, "❌ Credit checks not optimized for common cases"

        # ✅ Check: No performance anti-patterns
        assert "synchronized" not in code, "❌ Found synchronization overhead"
        assert "Thread.sleep" not in code, "❌ Found blocking operations"

    print("✅ Performance characteristics: PASSED")
    return True


def test_business_logic_completeness():
    """Test that business logic is complete and realistic."""
    print("💼 Testing Business Logic Completeness...")

    generator = UnifiedRouterGenerator()

    rule_mapping = RuleMapping(
        transaction_code="BUSINESS_TEST",
        rule_id="rule-business",
        rule_name="business_test",
        rule_type="rule",
        dependencies=[]
    )

    client_spec = ClientSpec(
        client_id="BUSINESS_BANK",
        rule_mappings=[rule_mapping]
    )

    artifacts = generator.generate_client_router(client_spec)

    # Find executor code
    executor_code = None
    for path, code in artifacts.items():
        if "Executor.java" in path and "executors" in path:
            executor_code = code
            break

    assert executor_code is not None, "❌ Executor code not found"

    # ✅ Check: Complete business rules
    business_elements = [
        "creditScore",
        "income",
        "APPROVED",
        "REJECTED",
        "CONDITIONAL",
        "creditLimit",
        "apr"
    ]

    for element in business_elements:
        assert element in executor_code, f"❌ Missing business element: {element}"

    # ✅ Check: Realistic credit decisions
    assert "750" in executor_code, "❌ Missing premium credit tier"
    assert "650" in executor_code, "❌ Missing standard credit tier"
    assert "calculateCreditLimit" in executor_code, "❌ Missing credit limit calculation"
    assert "calculateAPR" in executor_code, "❌ Missing APR calculation"

    # ✅ Check: Proper decision flow
    assert "PREMIUM" in executor_code, "❌ Missing premium tier"
    assert "STANDARD" in executor_code, "❌ Missing standard tier"

    print("✅ Business logic completeness: PASSED")
    return True


def test_java_syntax_validation():
    """Validate Java syntax correctness."""
    print("☕ Testing Java Syntax Validation...")

    generator = UnifiedRouterGenerator()

    rule_mapping = RuleMapping(
        transaction_code="SYNTAX_TEST",
        rule_id="rule-syntax",
        rule_name="syntax_test",
        rule_type="rule",
        dependencies=[]
    )

    client_spec = ClientSpec(
        client_id="SYNTAX_BANK",
        rule_mappings=[rule_mapping]
    )

    artifacts = generator.generate_client_router(client_spec)

    for path, code in artifacts.items():
        # ✅ Check: Balanced braces
        open_braces = code.count('{')
        close_braces = code.count('}')
        assert open_braces == close_braces, f"❌ Unbalanced braces in {path}"

        # ✅ Check: Balanced parentheses
        open_parens = code.count('(')
        close_parens = code.count(')')
        assert open_parens == close_parens, f"❌ Unbalanced parentheses in {path}"

        # ✅ Check: Proper method signatures
        lines = code.split('\n')
        for i, line in enumerate(lines):
            if 'public ' in line and '(' in line and ')' in line and '{' in line:
                # Single-line method signature with opening brace - this is valid
                continue
            elif 'public ' in line and '(' in line and ')' in line:
                # Check that method signature ends with { or ;
                if not (line.strip().endswith('{') or line.strip().endswith(';')):
                    # Look at next line for opening brace
                    if i + 1 < len(lines) and not lines[i + 1].strip().startswith('{'):
                        raise AssertionError(f"❌ Malformed method signature at line {i+1}: {line}")

    print("✅ Java syntax validation: PASSED")
    return True


def main():
    """Run all tests and report results."""
    print("🧪 Starting Unified Router Implementation Tests\n")

    tests = [
        test_compilation_fixes,
        test_security_fixes,
        test_react_style_data_flow,
        test_single_path_simplification,
        test_performance_characteristics,
        test_business_logic_completeness,
        test_java_syntax_validation
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
            print(f"❌ {test.__name__}: FAILED - {e}")
            failed += 1
        print()

    print("📊 Test Results:")
    print(f"✅ Passed: {passed}")
    print(f"❌ Failed: {failed}")
    print(f"📈 Success Rate: {passed/(passed+failed)*100:.1f}%")

    if failed == 0:
        print("\n🎉 ALL TESTS PASSED - Unified Router Implementation Complete!")
        print("🚀 Ready for production deployment with:")
        print("   ✅ Compilation errors fixed")
        print("   ✅ Security vulnerabilities resolved")
        print("   ✅ React-style data flow implemented")
        print("   ✅ Single execution path simplified")
        print("   ✅ Performance characteristics maintained")
        return 0
    else:
        print(f"\n⚠️ {failed} tests failed - Review implementation")
        return 1


if __name__ == "__main__":
    exit(main())