"""
Kafka Simulator for Batch Processing
Simulates Kafka's transaction queuing and checkpointing capabilities
using file-based storage for development and testing.
"""

import json
import os
import time
import threading
from typing import Dict, List, Optional, Any, Callable
from pathlib import Path
from dataclasses import dataclass, asdict
from datetime import datetime, timedelta
import logging


@dataclass
class TransactionRecord:
    """Represents a single transaction for processing."""
    transaction_id: str
    client_id: str
    transaction_type: str
    amount: float
    timestamp: str
    metadata: Dict[str, Any]


@dataclass
class BatchJob:
    """Represents a batch processing job."""
    batch_id: str
    transaction_count: int
    start_time: str
    status: str  # PENDING, PROCESSING, COMPLETED, FAILED
    checkpoint_position: int
    error_message: Optional[str] = None
    completion_time: Optional[str] = None


@dataclass
class ProcessingCheckpoint:
    """Checkpoint information for batch resume."""
    batch_id: str
    last_processed_position: int
    processed_count: int
    failed_count: int
    timestamp: str


class KafkaSimulator:
    """
    Simulates Kafka's key capabilities for batch processing:
    - Transaction queuing with file-based storage
    - Consumer group offset management (checkpointing)
    - Dead letter queue for failed transactions
    - Batch processing with resume capability
    """

    def __init__(self, storage_dir: str = "/tmp/kafka-simulator"):
        self.storage_dir = Path(storage_dir)
        self.storage_dir.mkdir(parents=True, exist_ok=True)

        # Directory structure
        self.transactions_dir = self.storage_dir / "transactions"
        self.checkpoints_dir = self.storage_dir / "checkpoints"
        self.batches_dir = self.storage_dir / "batches"
        self.dead_letter_dir = self.storage_dir / "dead_letter"

        for dir_path in [self.transactions_dir, self.checkpoints_dir,
                        self.batches_dir, self.dead_letter_dir]:
            dir_path.mkdir(exist_ok=True)

        self.logger = logging.getLogger(__name__)
        self._processing_lock = threading.Lock()

    def publish_transactions(self, transactions: List[TransactionRecord], batch_id: str) -> str:
        """
        Publish transactions to the simulated topic.
        Equivalent to Kafka producer.send().
        """
        batch_file = self.transactions_dir / f"batch_{batch_id}.jsonl"

        with open(batch_file, 'w') as f:
            for transaction in transactions:
                f.write(json.dumps(asdict(transaction)) + '\n')

        # Create batch job record
        batch_job = BatchJob(
            batch_id=batch_id,
            transaction_count=len(transactions),
            start_time=datetime.now().isoformat(),
            status="PENDING",
            checkpoint_position=0
        )

        batch_job_file = self.batches_dir / f"job_{batch_id}.json"
        with open(batch_job_file, 'w') as f:
            json.dump(asdict(batch_job), f, indent=2)

        self.logger.info(f"Published {len(transactions)} transactions to batch {batch_id}")
        return batch_id

    def consume_batch(self, batch_id: str,
                     processor_callback: Callable[[TransactionRecord], bool],
                     resume_from_checkpoint: bool = True) -> Dict[str, Any]:
        """
        Consume and process transactions from a batch.
        Simulates Kafka consumer with automatic offset management.

        Args:
            batch_id: Batch identifier
            processor_callback: Function to process each transaction (returns True for success)
            resume_from_checkpoint: Whether to resume from last checkpoint

        Returns:
            Processing results summary
        """
        with self._processing_lock:
            return self._process_batch_internal(batch_id, processor_callback, resume_from_checkpoint)

    def _process_batch_internal(self, batch_id: str,
                               processor_callback: Callable[[TransactionRecord], bool],
                               resume_from_checkpoint: bool) -> Dict[str, Any]:
        """Internal batch processing with checkpointing."""

        # Load batch job
        batch_job_file = self.batches_dir / f"job_{batch_id}.json"
        if not batch_job_file.exists():
            raise ValueError(f"Batch {batch_id} not found")

        with open(batch_job_file, 'r') as f:
            batch_job = BatchJob(**json.load(f))

        # Determine starting position
        start_position = 0
        if resume_from_checkpoint:
            checkpoint = self._load_checkpoint(batch_id)
            if checkpoint:
                start_position = checkpoint.last_processed_position
                self.logger.info(f"Resuming batch {batch_id} from position {start_position}")

        # Update batch status
        batch_job.status = "PROCESSING"
        batch_job.checkpoint_position = start_position
        self._save_batch_job(batch_job)

        # Load transactions
        transactions = self._load_transactions(batch_id)
        total_transactions = len(transactions)

        processed_count = start_position
        failed_count = 0
        processing_start = time.time()

        try:
            # Process transactions from start_position
            for i, transaction in enumerate(transactions[start_position:], start_position):
                try:
                    # Process transaction
                    success = processor_callback(transaction)

                    if success:
                        processed_count = i + 1

                        # Checkpoint every 100 transactions (simulates Kafka auto-commit)
                        if processed_count % 100 == 0:
                            self._save_checkpoint(batch_id, processed_count, failed_count)
                            batch_job.checkpoint_position = processed_count
                            self._save_batch_job(batch_job)

                            self.logger.info(f"Checkpoint: {processed_count}/{total_transactions} processed")
                    else:
                        # Move failed transaction to dead letter queue
                        self._move_to_dead_letter(batch_id, transaction,
                                                f"Processing failed at position {i}")
                        failed_count += 1

                except Exception as e:
                    # Handle processing exception
                    self.logger.error(f"Error processing transaction {transaction.transaction_id}: {e}")
                    self._move_to_dead_letter(batch_id, transaction, str(e))
                    failed_count += 1

                # Check 2-hour SLA (7200 seconds)
                if time.time() - processing_start > 7200:
                    raise TimeoutError("2-hour SLA exceeded")

            # Final checkpoint
            self._save_checkpoint(batch_id, processed_count, failed_count)

            # Mark batch as completed
            batch_job.status = "COMPLETED"
            batch_job.checkpoint_position = processed_count
            batch_job.completion_time = datetime.now().isoformat()
            self._save_batch_job(batch_job)

            processing_time = time.time() - processing_start

            results = {
                'batch_id': batch_id,
                'total_transactions': total_transactions,
                'processed_count': processed_count,
                'failed_count': failed_count,
                'processing_time_seconds': processing_time,
                'transactions_per_second': processed_count / processing_time if processing_time > 0 else 0,
                'status': 'COMPLETED',
                'sla_compliance': processing_time <= 7200  # 2 hours
            }

            self.logger.info(f"Batch {batch_id} completed: {results}")
            return results

        except Exception as e:
            # Mark batch as failed
            batch_job.status = "FAILED"
            batch_job.error_message = str(e)
            batch_job.completion_time = datetime.now().isoformat()
            self._save_batch_job(batch_job)

            self.logger.error(f"Batch {batch_id} failed: {e}")
            raise

    def _load_transactions(self, batch_id: str) -> List[TransactionRecord]:
        """Load transactions from batch file."""
        batch_file = self.transactions_dir / f"batch_{batch_id}.jsonl"
        transactions = []

        with open(batch_file, 'r') as f:
            for line in f:
                transaction_data = json.loads(line.strip())
                transactions.append(TransactionRecord(**transaction_data))

        return transactions

    def _save_checkpoint(self, batch_id: str, processed_count: int, failed_count: int):
        """Save processing checkpoint."""
        checkpoint = ProcessingCheckpoint(
            batch_id=batch_id,
            last_processed_position=processed_count,
            processed_count=processed_count,
            failed_count=failed_count,
            timestamp=datetime.now().isoformat()
        )

        checkpoint_file = self.checkpoints_dir / f"checkpoint_{batch_id}.json"
        with open(checkpoint_file, 'w') as f:
            json.dump(asdict(checkpoint), f, indent=2)

    def _load_checkpoint(self, batch_id: str) -> Optional[ProcessingCheckpoint]:
        """Load existing checkpoint if available."""
        checkpoint_file = self.checkpoints_dir / f"checkpoint_{batch_id}.json"

        if checkpoint_file.exists():
            with open(checkpoint_file, 'r') as f:
                return ProcessingCheckpoint(**json.load(f))

        return None

    def _save_batch_job(self, batch_job: BatchJob):
        """Save batch job state."""
        batch_job_file = self.batches_dir / f"job_{batch_job.batch_id}.json"
        with open(batch_job_file, 'w') as f:
            json.dump(asdict(batch_job), f, indent=2)

    def _move_to_dead_letter(self, batch_id: str, transaction: TransactionRecord, error_reason: str):
        """Move failed transaction to dead letter queue."""
        dead_letter_record = {
            'batch_id': batch_id,
            'transaction': asdict(transaction),
            'error_reason': error_reason,
            'failed_at': datetime.now().isoformat()
        }

        dead_letter_file = self.dead_letter_dir / f"failed_{batch_id}_{transaction.transaction_id}.json"
        with open(dead_letter_file, 'w') as f:
            json.dump(dead_letter_record, f, indent=2)

    def get_batch_status(self, batch_id: str) -> Optional[Dict[str, Any]]:
        """Get current status of a batch job."""
        batch_job_file = self.batches_dir / f"job_{batch_id}.json"

        if batch_job_file.exists():
            with open(batch_job_file, 'r') as f:
                batch_job = json.load(f)

            # Add checkpoint information
            checkpoint = self._load_checkpoint(batch_id)
            if checkpoint:
                batch_job['checkpoint_info'] = asdict(checkpoint)

            return batch_job

        return None

    def list_failed_transactions(self, batch_id: Optional[str] = None) -> List[Dict[str, Any]]:
        """List transactions in dead letter queue."""
        failed_transactions = []

        for dead_letter_file in self.dead_letter_dir.glob("failed_*.json"):
            with open(dead_letter_file, 'r') as f:
                record = json.load(f)

                if batch_id is None or record['batch_id'] == batch_id:
                    failed_transactions.append(record)

        return failed_transactions

    def cleanup_completed_batches(self, older_than_hours: int = 24):
        """Clean up completed batch files older than specified hours."""
        cutoff_time = datetime.now() - timedelta(hours=older_than_hours)
        cleaned_count = 0

        for batch_file in self.batches_dir.glob("job_*.json"):
            with open(batch_file, 'r') as f:
                batch_job = json.load(f)

            if (batch_job['status'] == 'COMPLETED' and
                batch_job.get('completion_time')):

                completion_time = datetime.fromisoformat(batch_job['completion_time'])
                if completion_time < cutoff_time:
                    # Remove batch files
                    batch_id = batch_job['batch_id']

                    for file_pattern in [
                        f"batch_{batch_id}.jsonl",
                        f"job_{batch_id}.json",
                        f"checkpoint_{batch_id}.json"
                    ]:
                        for dir_path in [self.transactions_dir, self.batches_dir, self.checkpoints_dir]:
                            file_path = dir_path / file_pattern
                            if file_path.exists():
                                file_path.unlink()

                    cleaned_count += 1

        self.logger.info(f"Cleaned up {cleaned_count} completed batches")
        return cleaned_count


def create_sample_transactions(count: int = 1000, client_id: str = "test_client") -> List[TransactionRecord]:
    """Create sample transactions for testing."""
    transactions = []

    for i in range(count):
        transaction = TransactionRecord(
            transaction_id=f"txn_{i:06d}",
            client_id=client_id,
            transaction_type="purchase",
            amount=100.0 + (i % 1000),
            timestamp=datetime.now().isoformat(),
            metadata={
                'merchant_id': f'merchant_{i % 100}',
                'category': 'retail',
                'location': 'US'
            }
        )
        transactions.append(transaction)

    return transactions


if __name__ == "__main__":
    # Test the Kafka simulator
    logging.basicConfig(level=logging.INFO)

    # Create simulator
    simulator = KafkaSimulator()

    # Create sample transactions
    transactions = create_sample_transactions(1000)

    # Publish transactions
    batch_id = f"test_batch_{int(time.time())}"
    simulator.publish_transactions(transactions, batch_id)

    # Simple processor callback for testing
    def mock_processor(transaction: TransactionRecord) -> bool:
        # Simulate processing time
        time.sleep(0.001)  # 1ms per transaction

        # Simulate 95% success rate
        return transaction.transaction_id[-1] != '7'  # Fail transactions ending in 7

    # Process batch
    try:
        results = simulator.consume_batch(batch_id, mock_processor)
        print(f"Processing results: {results}")

        # Check batch status
        status = simulator.get_batch_status(batch_id)
        print(f"Batch status: {status}")

        # List failed transactions
        failed = simulator.list_failed_transactions(batch_id)
        print(f"Failed transactions: {len(failed)}")

    except Exception as e:
        print(f"Processing failed: {e}")