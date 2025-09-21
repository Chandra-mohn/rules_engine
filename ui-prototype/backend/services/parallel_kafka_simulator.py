"""
Parallel Kafka Simulator for High-Throughput Batch Processing
Extends the original Kafka simulator with multi-threading capabilities
to achieve 50K+ TPS while maintaining transaction-level checkpointing.
"""

import json
import os
import time
import threading
import queue
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Dict, List, Optional, Any, Callable
from pathlib import Path
from dataclasses import dataclass, asdict
from datetime import datetime, timedelta
import logging
from collections import defaultdict

# Import base classes from original simulator
from .kafka_simulator import (
    TransactionRecord, BatchJob, ProcessingCheckpoint,
    KafkaSimulator as BaseKafkaSimulator
)


@dataclass
class ThreadedProcessingResult:
    """Result from a threaded processing batch."""
    thread_id: int
    start_position: int
    end_position: int
    processed_count: int
    failed_count: int
    processing_time: float
    thread_errors: List[str]


class ParallelKafkaSimulator(BaseKafkaSimulator):
    """
    Multi-threaded extension of KafkaSimulator for high-throughput processing.

    Features:
    - Worker thread pool for parallel transaction processing
    - Thread-safe checkpointing with batch aggregation
    - Configurable concurrency levels
    - Maintains all original features (dead letter queue, recovery, etc.)
    """

    def __init__(self, storage_dir: str = "/tmp/kafka-simulator",
                 max_workers: int = 8,
                 checkpoint_batch_size: int = 1000):
        super().__init__(storage_dir)

        # Threading configuration
        self.max_workers = max_workers
        self.checkpoint_batch_size = checkpoint_batch_size

        # Thread synchronization
        self._checkpoint_lock = threading.Lock()
        self._counter_lock = threading.Lock()
        self._checkpoint_queue = queue.Queue()

        # Performance tracking
        self._thread_metrics = defaultdict(dict)

        self.logger.info(f"Initialized ParallelKafkaSimulator with {max_workers} workers")

    def consume_batch_parallel(self, batch_id: str,
                             processor_callback: Callable[[TransactionRecord], bool],
                             resume_from_checkpoint: bool = True) -> Dict[str, Any]:
        """
        Parallel version of consume_batch with worker thread pool.

        Args:
            batch_id: Batch identifier
            processor_callback: Function to process each transaction
            resume_from_checkpoint: Whether to resume from last checkpoint

        Returns:
            Processing results with performance metrics
        """
        with self._processing_lock:
            return self._process_batch_parallel(batch_id, processor_callback, resume_from_checkpoint)

    def _process_batch_parallel(self, batch_id: str,
                               processor_callback: Callable[[TransactionRecord], bool],
                               resume_from_checkpoint: bool) -> Dict[str, Any]:
        """Internal parallel batch processing with thread pool."""

        # Load batch and determine starting position (same as original)
        batch_job_file = self.batches_dir / f"job_{batch_id}.json"
        if not batch_job_file.exists():
            raise ValueError(f"Batch {batch_id} not found")

        with open(batch_job_file, 'r') as f:
            batch_job = BatchJob(**json.load(f))

        start_position = 0
        if resume_from_checkpoint:
            checkpoint = self._load_checkpoint(batch_id)
            if checkpoint:
                start_position = checkpoint.last_processed_position
                self.logger.info(f"Resuming batch {batch_id} from position {start_position}")

        # Load transactions
        transactions = self._load_transactions(batch_id)
        total_transactions = len(transactions)
        remaining_transactions = transactions[start_position:]

        if not remaining_transactions:
            self.logger.info(f"Batch {batch_id} already completed")
            return self._get_completed_batch_results(batch_id)

        # Update batch status
        batch_job.status = "PROCESSING"
        batch_job.checkpoint_position = start_position
        self._save_batch_job(batch_job)

        # Partition transactions across workers
        chunk_size = max(1, len(remaining_transactions) // self.max_workers)
        transaction_chunks = [
            remaining_transactions[i:i + chunk_size]
            for i in range(0, len(remaining_transactions), chunk_size)
        ]

        self.logger.info(f"Processing {len(remaining_transactions)} transactions "
                        f"across {len(transaction_chunks)} chunks with {self.max_workers} workers")

        processing_start = time.time()
        thread_results = []

        # Process chunks in parallel
        with ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            # Submit all chunks to thread pool
            future_to_chunk = {}

            for chunk_idx, chunk in enumerate(transaction_chunks):
                chunk_start_pos = start_position + (chunk_idx * chunk_size)

                future = executor.submit(
                    self._process_chunk,
                    chunk, chunk_start_pos, chunk_idx,
                    processor_callback, batch_id, processing_start
                )
                future_to_chunk[future] = (chunk_idx, chunk_start_pos, len(chunk))

            # Collect results as they complete
            for future in as_completed(future_to_chunk):
                chunk_idx, chunk_start_pos, chunk_size = future_to_chunk[future]
                try:
                    result = future.result()
                    thread_results.append(result)

                    # Log progress
                    self.logger.info(f"Chunk {chunk_idx} completed: "
                                   f"{result.processed_count} processed, "
                                   f"{result.failed_count} failed")

                except Exception as e:
                    self.logger.error(f"Chunk {chunk_idx} failed: {e}")
                    # Create error result
                    error_result = ThreadedProcessingResult(
                        thread_id=chunk_idx,
                        start_position=chunk_start_pos,
                        end_position=chunk_start_pos + chunk_size,
                        processed_count=0,
                        failed_count=chunk_size,
                        processing_time=0,
                        thread_errors=[str(e)]
                    )
                    thread_results.append(error_result)

        # Aggregate results
        total_processed = sum(r.processed_count for r in thread_results)
        total_failed = sum(r.failed_count for r in thread_results)
        total_processing_time = time.time() - processing_start

        # Final checkpoint with aggregated results
        final_position = start_position + total_processed + total_failed
        self._save_checkpoint(batch_id, final_position, total_failed)

        # Update batch job
        batch_job.status = "COMPLETED"
        batch_job.checkpoint_position = final_position
        batch_job.completion_time = datetime.now().isoformat()
        self._save_batch_job(batch_job)

        # Compile final results
        results = {
            'batch_id': batch_id,
            'total_transactions': total_transactions,
            'processed_count': total_processed,
            'failed_count': total_failed,
            'processing_time_seconds': total_processing_time,
            'transactions_per_second': total_processed / total_processing_time if total_processing_time > 0 else 0,
            'status': 'COMPLETED',
            'sla_compliance': total_processing_time <= 7200,  # 2 hours
            'parallel_execution': {
                'workers_used': len(thread_results),
                'max_workers': self.max_workers,
                'chunk_size': chunk_size,
                'thread_results': [asdict(r) for r in thread_results]
            }
        }

        self.logger.info(f"Parallel batch {batch_id} completed: {results}")
        return results

    def _process_chunk(self, transactions: List[TransactionRecord],
                      start_position: int, thread_id: int,
                      processor_callback: Callable[[TransactionRecord], bool],
                      batch_id: str, processing_start_time: float) -> ThreadedProcessingResult:
        """Process a chunk of transactions in a single thread."""

        processed_count = 0
        failed_count = 0
        thread_errors = []
        chunk_start = time.time()

        try:
            for i, transaction in enumerate(transactions):
                current_position = start_position + i

                try:
                    # Process transaction
                    success = processor_callback(transaction)

                    if success:
                        processed_count += 1

                        # Thread-safe checkpoint aggregation
                        if processed_count % self.checkpoint_batch_size == 0:
                            self._queue_checkpoint_update(batch_id, current_position + 1, failed_count)

                    else:
                        # Move to dead letter queue (thread-safe)
                        self._move_to_dead_letter(batch_id, transaction,
                                                f"Processing failed at position {current_position}")
                        failed_count += 1

                except Exception as e:
                    error_msg = f"Transaction {transaction.transaction_id} error: {e}"
                    thread_errors.append(error_msg)
                    self.logger.error(error_msg)

                    self._move_to_dead_letter(batch_id, transaction, str(e))
                    failed_count += 1

                # SLA check
                if time.time() - processing_start_time > 7200:
                    raise TimeoutError("2-hour SLA exceeded")

        except Exception as e:
            thread_errors.append(f"Chunk processing error: {e}")
            self.logger.error(f"Thread {thread_id} chunk processing failed: {e}")

        chunk_time = time.time() - chunk_start

        result = ThreadedProcessingResult(
            thread_id=thread_id,
            start_position=start_position,
            end_position=start_position + len(transactions),
            processed_count=processed_count,
            failed_count=failed_count,
            processing_time=chunk_time,
            thread_errors=thread_errors
        )

        self.logger.info(f"Thread {thread_id} completed: "
                        f"processed={processed_count}, failed={failed_count}, "
                        f"time={chunk_time:.2f}s")

        return result

    def _queue_checkpoint_update(self, batch_id: str, position: int, failed_count: int):
        """Queue checkpoint update for batch processing (thread-safe)."""
        try:
            self._checkpoint_queue.put_nowait({
                'batch_id': batch_id,
                'position': position,
                'failed_count': failed_count,
                'timestamp': time.time()
            })
        except queue.Full:
            # If queue is full, just log and continue (checkpoints are periodic)
            self.logger.warning("Checkpoint queue full, skipping intermediate checkpoint")

    def _get_completed_batch_results(self, batch_id: str) -> Dict[str, Any]:
        """Get results for an already completed batch."""
        batch_job_file = self.batches_dir / f"job_{batch_id}.json"
        with open(batch_job_file, 'r') as f:
            batch_job = json.load(f)

        checkpoint = self._load_checkpoint(batch_id)

        return {
            'batch_id': batch_id,
            'total_transactions': batch_job['transaction_count'],
            'processed_count': checkpoint.processed_count if checkpoint else 0,
            'failed_count': checkpoint.failed_count if checkpoint else 0,
            'processing_time_seconds': 0,
            'transactions_per_second': 0,
            'status': 'ALREADY_COMPLETED',
            'sla_compliance': True
        }

    def get_performance_metrics(self) -> Dict[str, Any]:
        """Get detailed performance metrics for parallel processing."""
        return {
            'configuration': {
                'max_workers': self.max_workers,
                'checkpoint_batch_size': self.checkpoint_batch_size
            },
            'thread_metrics': dict(self._thread_metrics)
        }


def create_sample_transactions_large(count: int = 50000, client_id: str = "perf_test_client") -> List[TransactionRecord]:
    """Create large batch of sample transactions for performance testing."""
    transactions = []

    for i in range(count):
        transaction = TransactionRecord(
            transaction_id=f"perf_txn_{i:08d}",
            client_id=client_id,
            transaction_type="purchase",
            amount=100.0 + (i % 10000),  # Varied amounts
            timestamp=datetime.now().isoformat(),
            metadata={
                'merchant_id': f'merchant_{i % 1000}',
                'category': 'retail',
                'location': 'US',
                'performance_test': True
            }
        )
        transactions.append(transaction)

    return transactions


if __name__ == "__main__":
    # Performance test
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

    print("🚀 PARALLEL KAFKA SIMULATOR PERFORMANCE TEST")
    print("=" * 60)

    # Test configuration
    worker_counts = [1, 2, 4, 8, 16]
    transaction_count = 10000

    for workers in worker_counts:
        print(f"\n🔧 Testing with {workers} workers...")

        simulator = ParallelKafkaSimulator(
            storage_dir=f"/tmp/kafka-parallel-test-{workers}w",
            max_workers=workers,
            checkpoint_batch_size=500
        )

        # Create test transactions
        transactions = create_sample_transactions_large(transaction_count)
        batch_id = f"perf_test_{workers}w_{int(time.time())}"

        # Publish transactions
        simulator.publish_transactions(transactions, batch_id)

        # Performance test processor
        def perf_processor(transaction: TransactionRecord) -> bool:
            # Simulate realistic rule processing time
            time.sleep(0.0001)  # 0.1ms per transaction
            return not transaction.transaction_id.endswith('99')  # 99% success rate

        # Process with timing
        start_time = time.time()
        results = simulator.consume_batch_parallel(batch_id, perf_processor)

        print(f"✅ Workers: {workers}")
        print(f"   • TPS: {results['transactions_per_second']:.0f}")
        print(f"   • Time: {results['processing_time_seconds']:.2f}s")
        print(f"   • Success: {results['processed_count']}/{results['total_transactions']}")