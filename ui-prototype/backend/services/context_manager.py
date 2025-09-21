"""
Immutable Context Management System
Provides Copy-on-Write semantics for transaction contexts with 5000+ fields.
Optimized for high-frequency mutations and memory efficiency.
"""

from typing import Dict, Any, Optional, List, Set
from dataclasses import dataclass, field
from threading import local
from queue import Queue
import json
from datetime import datetime
import weakref


@dataclass
class ContextChange:
    """Represents a single context field change for debug tracking."""
    field_name: str
    old_value: Any
    new_value: Any
    timestamp: datetime = field(default_factory=datetime.now)


@dataclass
class ContextSnapshot:
    """Immutable snapshot of context state for rollback capabilities."""
    snapshot_id: str
    core_data: Dict[str, Any]
    extended_data: Dict[str, Any]
    timestamp: datetime = field(default_factory=datetime.now)


class ContextPool:
    """
    Thread-local context object pool for high-frequency allocations.
    Reduces GC pressure for 80K+ TPS requirements.
    """

    def __init__(self, initial_size: int = 100, max_size: int = 1000):
        self.initial_size = initial_size
        self.max_size = max_size
        self.local = local()

    def _get_pool(self) -> Queue:
        """Get thread-local context pool."""
        if not hasattr(self.local, 'pool'):
            self.local.pool = Queue(maxsize=self.max_size)
            # Pre-populate with initial contexts
            for _ in range(self.initial_size):
                self.local.pool.put(TransactionContext._create_empty())
        return self.local.pool

    def acquire(self, transaction_id: str, **initial_fields) -> 'TransactionContext':
        """Acquire a context from the pool."""
        pool = self._get_pool()

        try:
            # Try to reuse existing context
            context = pool.get_nowait()
            return context._reset(transaction_id, **initial_fields)
        except:
            # Create new if pool is empty
            return TransactionContext(transaction_id, **initial_fields)

    def release(self, context: 'TransactionContext') -> None:
        """Return context to pool for reuse."""
        if context._can_be_pooled():
            pool = self._get_pool()
            try:
                pool.put_nowait(context._prepare_for_pooling())
            except:
                # Pool is full, let GC handle it
                pass


class TransactionContext:
    """
    Immutable Transaction Context with Copy-on-Write semantics.

    Designed for:
    - 5000+ field payloads with KB to MB sizes
    - High-frequency mutations (10-20 fields for non-mon, hundreds for mon)
    - Sub-millisecond performance requirements
    - Memory efficiency with object pooling
    """

    # Hot fields (frequently accessed - keep in core for CPU cache efficiency)
    HOT_FIELDS = {
        'transaction_id', 'credit_score', 'income', 'status', 'credit_limit',
        'apr', 'risk_score', 'approval_code', 'reason', 'amount'
    }

    # Global context pool
    _pool = ContextPool()

    def __init__(self, transaction_id: str, debug_mode: bool = False, **initial_fields):
        """
        Create new immutable transaction context.

        Args:
            transaction_id: Unique transaction identifier
            debug_mode: Enable change tracking for debugging
            **initial_fields: Initial field values
        """
        # Separate hot and cold fields for optimal memory layout
        self._core_data = {}
        self._extended_data = {}

        # Initialize with provided fields
        for field_name, value in initial_fields.items():
            if field_name in self.HOT_FIELDS:
                self._core_data[field_name] = value
            else:
                self._extended_data[field_name] = value

        # Always store transaction_id in core
        self._core_data['transaction_id'] = transaction_id

        # Change tracking for debug mode
        self._debug_mode = debug_mode
        self._changes: List[ContextChange] = [] if debug_mode else None

        # Snapshot management
        self._snapshots: Dict[str, ContextSnapshot] = {}

        # Object pooling support
        self._poolable = True
        self._pool_generation = 0

    @classmethod
    def acquire_from_pool(cls, transaction_id: str, **initial_fields) -> 'TransactionContext':
        """Acquire context from object pool for performance."""
        return cls._pool.acquire(transaction_id, **initial_fields)

    def release_to_pool(self) -> None:
        """Release context back to pool for reuse."""
        self._pool.release(self)

    # Hot field accessors (frequently used fields)
    def get_transaction_id(self) -> str:
        return self._core_data.get('transaction_id')

    def get_credit_score(self) -> Optional[int]:
        return self._core_data.get('credit_score')

    def get_income(self) -> Optional[float]:
        return self._core_data.get('income')

    def get_status(self) -> Optional[str]:
        return self._core_data.get('status')

    def get_credit_limit(self) -> Optional[int]:
        return self._core_data.get('credit_limit')

    def get_apr(self) -> Optional[float]:
        return self._core_data.get('apr')

    def get_risk_score(self) -> Optional[float]:
        return self._core_data.get('risk_score')

    def get_amount(self) -> Optional[float]:
        return self._core_data.get('amount')

    # Generic field access
    def get_field(self, field_name: str, default: Any = None) -> Any:
        """Get field value from hot or cold storage."""
        if field_name in self.HOT_FIELDS:
            return self._core_data.get(field_name, default)
        else:
            return self._extended_data.get(field_name, default)

    def has_field(self, field_name: str) -> bool:
        """Check if field exists in context."""
        if field_name in self.HOT_FIELDS:
            return field_name in self._core_data
        else:
            return field_name in self._extended_data

    # Copy-on-Write mutations for hot fields
    def with_credit_score(self, credit_score: int) -> 'TransactionContext':
        """Create new context with updated credit score."""
        return self._with_core_field('credit_score', credit_score)

    def with_status(self, status: str) -> 'TransactionContext':
        """Create new context with updated status."""
        return self._with_core_field('status', status)

    def with_credit_limit(self, credit_limit: int) -> 'TransactionContext':
        """Create new context with updated credit limit."""
        return self._with_core_field('credit_limit', credit_limit)

    def with_apr(self, apr: float) -> 'TransactionContext':
        """Create new context with updated APR."""
        return self._with_core_field('apr', apr)

    def with_risk_score(self, risk_score: float) -> 'TransactionContext':
        """Create new context with updated risk score."""
        return self._with_core_field('risk_score', risk_score)

    def with_amount(self, amount: float) -> 'TransactionContext':
        """Create new context with updated amount."""
        return self._with_core_field('amount', amount)

    def with_reason(self, reason: str) -> 'TransactionContext':
        """Create new context with updated reason."""
        return self._with_core_field('reason', reason)

    # Generic field mutations
    def with_field(self, field_name: str, value: Any) -> 'TransactionContext':
        """Create new context with updated field value."""
        if field_name in self.HOT_FIELDS:
            return self._with_core_field(field_name, value)
        else:
            return self._with_extended_field(field_name, value)

    def with_fields(self, **field_updates) -> 'TransactionContext':
        """Create new context with multiple field updates (batch operation)."""
        new_core = dict(self._core_data)
        new_extended = dict(self._extended_data)
        changes = []

        for field_name, value in field_updates.items():
            old_value = self.get_field(field_name)

            if field_name in self.HOT_FIELDS:
                new_core[field_name] = value
            else:
                new_extended[field_name] = value

            # Track change for debug mode
            if self._debug_mode:
                changes.append(ContextChange(field_name, old_value, value))

        return self._create_copy(new_core, new_extended, additional_changes=changes)

    def without_field(self, field_name: str) -> 'TransactionContext':
        """Create new context with field removed."""
        if field_name in self.HOT_FIELDS and field_name in self._core_data:
            new_core = dict(self._core_data)
            old_value = new_core.pop(field_name, None)
            change = ContextChange(field_name, old_value, None) if self._debug_mode else None
            return self._create_copy(new_core, self._extended_data,
                                   additional_changes=[change] if change else [])
        elif field_name in self._extended_data:
            new_extended = dict(self._extended_data)
            old_value = new_extended.pop(field_name, None)
            change = ContextChange(field_name, old_value, None) if self._debug_mode else None
            return self._create_copy(self._core_data, new_extended,
                                   additional_changes=[change] if change else [])
        else:
            return self

    # Snapshot management for rollback capabilities
    def create_snapshot(self, snapshot_id: str) -> 'TransactionContext':
        """Create a named snapshot of current context state."""
        snapshot = ContextSnapshot(
            snapshot_id=snapshot_id,
            core_data=dict(self._core_data),
            extended_data=dict(self._extended_data)
        )

        new_snapshots = dict(self._snapshots)
        new_snapshots[snapshot_id] = snapshot

        return self._create_copy(self._core_data, self._extended_data, snapshots=new_snapshots)

    def rollback_to_snapshot(self, snapshot_id: str) -> 'TransactionContext':
        """Rollback context to a previous snapshot."""
        if snapshot_id not in self._snapshots:
            raise ValueError(f"Snapshot '{snapshot_id}' not found")

        snapshot = self._snapshots[snapshot_id]
        return self._create_copy(snapshot.core_data, snapshot.extended_data, snapshots=self._snapshots)

    def list_snapshots(self) -> List[str]:
        """Get list of available snapshot IDs."""
        return list(self._snapshots.keys())

    # Bulk operations for memory efficiency
    def merge_context(self, other: 'TransactionContext') -> 'TransactionContext':
        """Merge another context into this one, with other taking precedence."""
        new_core = dict(self._core_data)
        new_extended = dict(self._extended_data)
        changes = []

        # Merge core data
        for field_name, value in other._core_data.items():
            if field_name != 'transaction_id':  # Preserve original transaction ID
                old_value = new_core.get(field_name)
                new_core[field_name] = value
                if self._debug_mode and old_value != value:
                    changes.append(ContextChange(field_name, old_value, value))

        # Merge extended data
        for field_name, value in other._extended_data.items():
            old_value = new_extended.get(field_name)
            new_extended[field_name] = value
            if self._debug_mode and old_value != value:
                changes.append(ContextChange(field_name, old_value, value))

        return self._create_copy(new_core, new_extended, additional_changes=changes)

    def filter_fields(self, field_names: Set[str]) -> 'TransactionContext':
        """Create new context with only specified fields."""
        new_core = {k: v for k, v in self._core_data.items() if k in field_names}
        new_extended = {k: v for k, v in self._extended_data.items() if k in field_names}

        return self._create_copy(new_core, new_extended)

    # Debug and introspection
    def get_changes(self) -> List[ContextChange]:
        """Get list of changes made to this context (debug mode only)."""
        return list(self._changes) if self._changes else []

    def get_all_fields(self) -> Dict[str, Any]:
        """Get all fields as a dictionary."""
        result = dict(self._core_data)
        result.update(self._extended_data)
        return result

    def get_hot_fields(self) -> Dict[str, Any]:
        """Get only hot fields."""
        return dict(self._core_data)

    def get_cold_fields(self) -> Dict[str, Any]:
        """Get only cold fields."""
        return dict(self._extended_data)

    def get_field_count(self) -> int:
        """Get total number of fields."""
        return len(self._core_data) + len(self._extended_data)

    def get_memory_estimate_kb(self) -> float:
        """Estimate memory usage in KB."""
        # Rough estimation based on JSON serialization
        data = self.get_all_fields()
        json_str = json.dumps(data, default=str)
        return len(json_str.encode('utf-8')) / 1024

    def to_json(self) -> str:
        """Serialize context to JSON."""
        return json.dumps(self.get_all_fields(), default=str, indent=2)

    @classmethod
    def from_json(cls, json_str: str, transaction_id: Optional[str] = None) -> 'TransactionContext':
        """Create context from JSON string."""
        data = json.loads(json_str)
        txn_id = transaction_id or data.get('transaction_id')
        if not txn_id:
            raise ValueError("Transaction ID required")

        return cls(txn_id, **{k: v for k, v in data.items() if k != 'transaction_id'})

    # Internal helper methods
    def _with_core_field(self, field_name: str, value: Any) -> 'TransactionContext':
        """Create new context with updated core field."""
        new_core = dict(self._core_data)
        old_value = new_core.get(field_name)
        new_core[field_name] = value

        change = ContextChange(field_name, old_value, value) if self._debug_mode else None
        return self._create_copy(new_core, self._extended_data,
                               additional_changes=[change] if change else [])

    def _with_extended_field(self, field_name: str, value: Any) -> 'TransactionContext':
        """Create new context with updated extended field."""
        new_extended = dict(self._extended_data)
        old_value = new_extended.get(field_name)
        new_extended[field_name] = value

        change = ContextChange(field_name, old_value, value) if self._debug_mode else None
        return self._create_copy(self._core_data, new_extended,
                               additional_changes=[change] if change else [])

    def _create_copy(self, core_data: Dict[str, Any], extended_data: Dict[str, Any],
                    additional_changes: List[ContextChange] = None,
                    snapshots: Dict[str, ContextSnapshot] = None) -> 'TransactionContext':
        """Create a copy of the context with new data."""
        new_context = TransactionContext.__new__(TransactionContext)
        new_context._core_data = core_data
        new_context._extended_data = extended_data
        new_context._debug_mode = self._debug_mode
        new_context._poolable = False  # Derived contexts cannot be pooled

        # Copy change history and add new changes
        if self._debug_mode:
            new_context._changes = list(self._changes) if self._changes else []
            if additional_changes:
                new_context._changes.extend(additional_changes)
        else:
            new_context._changes = None

        # Copy or update snapshots
        new_context._snapshots = snapshots if snapshots is not None else dict(self._snapshots)

        return new_context

    @classmethod
    def _create_empty(cls) -> 'TransactionContext':
        """Create empty context for object pooling."""
        context = cls.__new__(cls)
        context._core_data = {}
        context._extended_data = {}
        context._debug_mode = False
        context._changes = None
        context._snapshots = {}
        context._poolable = True
        context._pool_generation = 0
        return context

    def _reset(self, transaction_id: str, **initial_fields) -> 'TransactionContext':
        """Reset pooled context with new data."""
        self._core_data.clear()
        self._extended_data.clear()
        self._snapshots.clear()

        # Set new data
        for field_name, value in initial_fields.items():
            if field_name in self.HOT_FIELDS:
                self._core_data[field_name] = value
            else:
                self._extended_data[field_name] = value

        self._core_data['transaction_id'] = transaction_id
        self._pool_generation += 1
        return self

    def _can_be_pooled(self) -> bool:
        """Check if context can be returned to pool."""
        return self._poolable and self.get_field_count() < 100  # Don't pool large contexts

    def _prepare_for_pooling(self) -> 'TransactionContext':
        """Prepare context for return to pool."""
        self._debug_mode = False
        self._changes = None
        return self

    def __str__(self) -> str:
        """String representation for debugging."""
        return f"TransactionContext(id={self.get_transaction_id()}, fields={self.get_field_count()})"

    def __repr__(self) -> str:
        """Detailed string representation."""
        return (f"TransactionContext(transaction_id={self.get_transaction_id()}, "
                f"core_fields={len(self._core_data)}, "
                f"extended_fields={len(self._extended_data)}, "
                f"debug_mode={self._debug_mode})")


class ContextManager:
    """
    High-level context management with performance monitoring.
    Provides convenience methods for common context operations.
    """

    def __init__(self):
        self.active_contexts: Dict[str, weakref.ReferenceType] = {}
        self.performance_stats = {
            'contexts_created': 0,
            'contexts_pooled': 0,
            'total_mutations': 0,
            'total_snapshots': 0
        }

    def create_context(self, transaction_id: str, use_pool: bool = True, **initial_fields) -> TransactionContext:
        """Create new transaction context with optional pooling."""
        if use_pool:
            context = TransactionContext.acquire_from_pool(transaction_id, **initial_fields)
            self.performance_stats['contexts_pooled'] += 1
        else:
            context = TransactionContext(transaction_id, **initial_fields)

        self.performance_stats['contexts_created'] += 1

        # Track active context
        self.active_contexts[transaction_id] = weakref.ref(context)

        return context

    def get_context(self, transaction_id: str) -> Optional[TransactionContext]:
        """Get active context by transaction ID."""
        ref = self.active_contexts.get(transaction_id)
        return ref() if ref else None

    def release_context(self, context: TransactionContext) -> None:
        """Release context back to pool."""
        context.release_to_pool()
        transaction_id = context.get_transaction_id()
        if transaction_id in self.active_contexts:
            del self.active_contexts[transaction_id]

    def get_performance_stats(self) -> Dict[str, Any]:
        """Get performance statistics."""
        stats = dict(self.performance_stats)
        stats['active_contexts'] = len(self.active_contexts)
        return stats

    def cleanup_inactive_contexts(self) -> int:
        """Clean up references to garbage collected contexts."""
        initial_count = len(self.active_contexts)
        self.active_contexts = {k: v for k, v in self.active_contexts.items() if v() is not None}
        return initial_count - len(self.active_contexts)


# Global context manager instance
default_context_manager = ContextManager()


def create_context(transaction_id: str, **initial_fields) -> TransactionContext:
    """Convenience function to create context using default manager."""
    return default_context_manager.create_context(transaction_id, **initial_fields)


def get_context(transaction_id: str) -> Optional[TransactionContext]:
    """Convenience function to get context using default manager."""
    return default_context_manager.get_context(transaction_id)


def release_context(context: TransactionContext) -> None:
    """Convenience function to release context using default manager."""
    default_context_manager.release_context(context)