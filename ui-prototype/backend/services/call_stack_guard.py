"""
Runtime guard against cyclic rule calls in Python execution.

Provides thread-local call stack tracking to prevent infinite recursion
when rules call other rules or actionsets during execution.
"""

import threading
from typing import List, Optional
from contextlib import contextmanager


class CallStackGuard:
    """
    Thread-safe runtime guard for rule execution cycles.

    Uses thread-local storage to track rule call chains and detect cycles.
    """

    # Maximum call depth allowed
    MAX_DEPTH = 32

    def __init__(self):
        self._local = threading.local()

    def _get_stack(self) -> List[str]:
        """Get current thread's call stack."""
        if not hasattr(self._local, 'stack'):
            self._local.stack = []
        return self._local.stack

    def _get_set(self) -> set:
        """Get current thread's call set for O(1) cycle detection."""
        if not hasattr(self._local, 'call_set'):
            self._local.call_set = set()
        return self._local.call_set

    def enter(self, rule_name: str):
        """
        Enter a rule execution context.

        Args:
            rule_name: Name of the rule being entered

        Raises:
            CyclicCallException: If the rule is already in the call stack
            MaxDepthExceededException: If MAX_DEPTH is exceeded
        """
        stack = self._get_stack()
        call_set = self._get_set()

        # Check for cycle
        if rule_name in call_set:
            cycle_path = self._build_cycle_path(stack, rule_name)
            raise CyclicCallException(
                f"Cyclic rule call detected: {cycle_path} → {rule_name}"
            )

        # Check max depth
        if len(stack) >= self.MAX_DEPTH:
            raise MaxDepthExceededException(
                f"Maximum call depth of {self.MAX_DEPTH} exceeded. "
                f"Current stack: {' → '.join(stack)}"
            )

        # Add to stack and set
        stack.append(rule_name)
        call_set.add(rule_name)

    def exit(self, rule_name: str):
        """
        Exit a rule execution context.

        Args:
            rule_name: Name of the rule being exited

        Raises:
            ValueError: If the rule name doesn't match the top of stack
        """
        stack = self._get_stack()
        call_set = self._get_set()

        if not stack:
            raise ValueError(
                f"Cannot exit rule '{rule_name}': call stack is empty"
            )

        top = stack.pop()
        if top != rule_name:
            # Restore stack state
            stack.append(top)
            raise ValueError(
                f"Cannot exit rule '{rule_name}': expected '{top}' at top of stack"
            )

        call_set.discard(rule_name)

    def get_depth(self) -> int:
        """Get current call depth."""
        return len(self._get_stack())

    def get_call_stack(self) -> List[str]:
        """Get current call stack as list."""
        return list(self._get_stack())

    def clear(self):
        """Clear the call stack for the current thread."""
        self._get_stack().clear()
        self._get_set().clear()

    @contextmanager
    def guard(self, rule_name: str):
        """
        Context manager for guarded rule execution.

        Usage:
            with call_stack_guard.guard("myRule"):
                # Execute rule logic
                pass

        Args:
            rule_name: Name of the rule being executed
        """
        self.enter(rule_name)
        try:
            yield
        finally:
            self.exit(rule_name)

    def _build_cycle_path(self, stack: List[str], cycle_to: str) -> str:
        """Build human-readable cycle path string."""
        # Find where the cycle starts
        try:
            start_idx = stack.index(cycle_to)
            cycle_segment = stack[start_idx:]
            return ' → '.join(cycle_segment)
        except ValueError:
            # Shouldn't happen, but fallback to full stack
            return ' → '.join(stack)


class CyclicCallException(Exception):
    """Exception raised when a cyclic rule call is detected."""
    pass


class MaxDepthExceededException(Exception):
    """Exception raised when maximum call depth is exceeded."""
    pass


# Global singleton instance
call_stack_guard = CallStackGuard()
