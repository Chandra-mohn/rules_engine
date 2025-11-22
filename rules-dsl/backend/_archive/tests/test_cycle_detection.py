"""
Test cases for rule cycle detection.

Tests both static analysis (save-time) and runtime guards against cyclic rule calls.
"""

import pytest
from services.cycle_detector import RuleCycleDetector
from services.call_stack_guard import (
    CallStackGuard,
    CyclicCallException,
    MaxDepthExceededException
)
from services.rule_file_service import RuleFileService


class TestStaticCycleDetection:
    """Tests for static cycle detection at save-time."""

    def setup_method(self):
        """Set up test fixtures."""
        self.rule_service = RuleFileService()
        self.detector = RuleCycleDetector(self.rule_service)

    def test_extract_dependencies_simple(self):
        """Test extracting simple rule dependencies."""
        content = """rule checkCredit:
            if applicant.creditScore < 600 then lowCreditCheck
            else highCreditCheck
        """
        deps = self.detector.extract_dependencies(content)

        assert 'lowCreditCheck' in deps
        assert 'highCreditCheck' in deps
        assert 'checkCredit' not in deps  # Own name excluded
        assert 'applicant' not in deps  # Attribute excluded

    def test_extract_dependencies_quoted(self):
        """Test extracting dependencies with quoted identifiers."""
        content = """rule "Complex Check":
            if amount > 1000 then "High Value Check"
            else standardCheck
        """
        deps = self.detector.extract_dependencies(content)

        assert 'High Value Check' in deps
        assert 'standardCheck' in deps
        assert 'Complex Check' not in deps  # Own name excluded

    def test_extract_dependencies_ignore_primitives(self):
        """Test that primitive actions are ignored."""
        content = """rule approvalFlow:
            if score > 700 then approveTransaction
            else declineTransaction
        """
        deps = self.detector.extract_dependencies(content)

        # Primitive actions should be filtered out
        assert 'approveTransaction' not in deps
        assert 'declineTransaction' not in deps

    def test_extract_dependencies_ignore_single_quotes(self):
        """Test that single-quoted strings (literals) are ignored."""
        content = """rule testRule:
            if status == 'PENDING' then processApplication
            else 'someString'
        """
        deps = self.detector.extract_dependencies(content)

        assert 'PENDING' not in deps  # Single quote = literal
        assert 'someString' not in deps  # Single quote = literal
        assert 'processApplication' in deps

    def test_detect_direct_cycle(self):
        """Test detection of direct A→B→A cycle."""
        # This would require actual rule files to exist
        # For now, test the logic with mock content
        content_a = """rule ruleA:
            if condition then ruleB
        """

        result = self.detector.detect_cycles('ruleA', content_a)

        # Without ruleB existing, should not detect cycle
        assert result['has_cycle'] == False
        assert 'ruleB' in result['dependencies']

    def test_detect_self_reference(self):
        """Test detection of rule calling itself."""
        content = """rule recursiveRule:
            if depth > 0 then recursiveRule
            else done
        """

        result = self.detector.detect_cycles('recursiveRule', content)

        # Self-reference should be detected
        assert result['has_cycle'] == True
        assert 'recursiveRule' in result['cycle_path']

    def test_no_cycle_simple(self):
        """Test that rules without cycles are accepted."""
        content = """rule simpleRule:
            if applicant.score > 700 then approveApplication
            else rejectApplication
        """

        result = self.detector.detect_cycles('simpleRule', content)

        assert result['has_cycle'] == False
        assert result['cycle_path'] is None

    def test_no_cycle_no_dependencies(self):
        """Test rule with no dependencies."""
        content = """rule standaloneRule:
            if score > 700 then approveTransaction
            else declineTransaction
        """

        result = self.detector.detect_cycles('standaloneRule', content)

        assert result['has_cycle'] == False
        assert len(result['dependencies']) == 0


class TestRuntimeCallStackGuard:
    """Tests for runtime call stack protection."""

    def setup_method(self):
        """Set up test fixtures."""
        self.guard = CallStackGuard()
        self.guard.clear()  # Ensure clean state

    def teardown_method(self):
        """Clean up after tests."""
        self.guard.clear()

    def test_enter_exit_basic(self):
        """Test basic enter/exit functionality."""
        self.guard.enter('ruleA')
        assert self.guard.get_depth() == 1
        assert self.guard.get_call_stack() == ['ruleA']

        self.guard.exit('ruleA')
        assert self.guard.get_depth() == 0
        assert self.guard.get_call_stack() == []

    def test_nested_calls(self):
        """Test nested rule calls (A→B→C)."""
        self.guard.enter('ruleA')
        self.guard.enter('ruleB')
        self.guard.enter('ruleC')

        assert self.guard.get_depth() == 3
        assert self.guard.get_call_stack() == ['ruleA', 'ruleB', 'ruleC']

        self.guard.exit('ruleC')
        self.guard.exit('ruleB')
        self.guard.exit('ruleA')

        assert self.guard.get_depth() == 0

    def test_detect_direct_cycle(self):
        """Test detection of A→B→A cycle."""
        self.guard.enter('ruleA')
        self.guard.enter('ruleB')

        with pytest.raises(CyclicCallException) as exc_info:
            self.guard.enter('ruleA')  # Cycle!

        assert 'Cyclic rule call detected' in str(exc_info.value)
        assert 'ruleA' in str(exc_info.value)

    def test_detect_self_cycle(self):
        """Test detection of rule calling itself."""
        self.guard.enter('recursiveRule')

        with pytest.raises(CyclicCallException) as exc_info:
            self.guard.enter('recursiveRule')  # Self-cycle!

        assert 'Cyclic rule call detected' in str(exc_info.value)

    def test_max_depth_exceeded(self):
        """Test max depth protection."""
        # Enter rules up to max depth
        for i in range(CallStackGuard.MAX_DEPTH):
            self.guard.enter(f'rule{i}')

        assert self.guard.get_depth() == CallStackGuard.MAX_DEPTH

        # One more should exceed
        with pytest.raises(MaxDepthExceededException) as exc_info:
            self.guard.enter('oneMore')

        assert 'Maximum call depth' in str(exc_info.value)
        assert str(CallStackGuard.MAX_DEPTH) in str(exc_info.value)

    def test_exit_mismatch_error(self):
        """Test error on exiting wrong rule."""
        self.guard.enter('ruleA')
        self.guard.enter('ruleB')

        with pytest.raises(ValueError) as exc_info:
            self.guard.exit('ruleA')  # Should be ruleB!

        assert 'expected' in str(exc_info.value).lower()

    def test_exit_empty_stack_error(self):
        """Test error on exiting with empty stack."""
        with pytest.raises(ValueError) as exc_info:
            self.guard.exit('ruleA')

        assert 'empty' in str(exc_info.value).lower()

    def test_context_manager(self):
        """Test context manager usage."""
        with self.guard.guard('ruleA'):
            assert self.guard.get_depth() == 1

            with self.guard.guard('ruleB'):
                assert self.guard.get_depth() == 2

            assert self.guard.get_depth() == 1

        assert self.guard.get_depth() == 0

    def test_context_manager_with_exception(self):
        """Test that context manager cleans up even on exception."""
        try:
            with self.guard.guard('ruleA'):
                assert self.guard.get_depth() == 1
                raise RuntimeError("Test error")
        except RuntimeError:
            pass

        # Stack should still be cleaned up
        assert self.guard.get_depth() == 0

    def test_clear_stack(self):
        """Test clearing the call stack."""
        self.guard.enter('ruleA')
        self.guard.enter('ruleB')
        self.guard.enter('ruleC')

        assert self.guard.get_depth() == 3

        self.guard.clear()

        assert self.guard.get_depth() == 0
        assert self.guard.get_call_stack() == []


class TestIntegrationScenarios:
    """Integration tests combining static and runtime detection."""

    def setup_method(self):
        """Set up test fixtures."""
        self.rule_service = RuleFileService()
        self.detector = RuleCycleDetector(self.rule_service)
        self.guard = CallStackGuard()
        self.guard.clear()

    def teardown_method(self):
        """Clean up after tests."""
        self.guard.clear()

    def test_scenario_complex_cycle(self):
        """Test complex cycle: A→B→C→D→B."""
        # Static detection should catch this at save-time
        content_a = "rule ruleA: if true then ruleB"
        result = self.detector.detect_cycles('ruleA', content_a)

        # Without actual files, won't detect the full cycle
        # But in real scenario with files present, would detect
        assert 'ruleB' in result['dependencies']

    def test_scenario_allowed_chain(self):
        """Test allowed rule chain without cycles."""
        content = """rule entryPoint:
            if score > 700 then highTierProcessing
            else standardProcessing
        """

        result = self.detector.detect_cycles('entryPoint', content)
        assert result['has_cycle'] == False

        # Runtime execution would be:
        # entryPoint → highTierProcessing → done (no cycle)
        with self.guard.guard('entryPoint'):
            with self.guard.guard('highTierProcessing'):
                pass  # Executes successfully

    def test_scenario_depth_32_allowed(self):
        """Test that 32-level depth is allowed (at limit)."""
        # Exactly at max depth should work
        for i in range(CallStackGuard.MAX_DEPTH):
            self.guard.enter(f'level{i}')

        assert self.guard.get_depth() == CallStackGuard.MAX_DEPTH

        # Clean up
        for i in range(CallStackGuard.MAX_DEPTH - 1, -1, -1):
            self.guard.exit(f'level{i}')

    def test_scenario_depth_33_blocked(self):
        """Test that 33-level depth is blocked."""
        for i in range(CallStackGuard.MAX_DEPTH):
            self.guard.enter(f'level{i}')

        # 33rd level should fail
        with pytest.raises(MaxDepthExceededException):
            self.guard.enter('level33')
