package com.rules.batch;

import java.util.concurrent.atomic.LongAdder;

/**
 * Batch processing metrics for monitoring and performance validation
 */
public final class BatchMetrics {

    private final LongAdder batchesStarted = new LongAdder();
    private final LongAdder batchesCompleted = new LongAdder();
    private final LongAdder filesProcessed = new LongAdder();
    private final LongAdder transactionsProcessed = new LongAdder();
    private final LongAdder errors = new LongAdder();
    private volatile long currentBatchStartTime;

    public void startBatch() {
        batchesStarted.increment();
        currentBatchStartTime = System.currentTimeMillis();
    }

    public void completeBatch() {
        batchesCompleted.increment();
    }

    public void recordFileProcessed() {
        filesProcessed.increment();
    }

    public void recordTransactionsProcessed(long count) {
        transactionsProcessed.add(count);
    }

    public void recordError() {
        errors.increment();
    }

    // Getters
    public long getBatchesStarted() { return batchesStarted.sum(); }
    public long getBatchesCompleted() { return batchesCompleted.sum(); }
    public long getFilesProcessed() { return filesProcessed.sum(); }
    public long getTransactionsProcessed() { return transactionsProcessed.sum(); }
    public long getErrors() { return errors.sum(); }

    public double getCurrentBatchDuration() {
        return (System.currentTimeMillis() - currentBatchStartTime) / 1000.0;
    }

    /**
     * Generate metrics report
     */
    public String generateReport() {
        return String.format("""
            📊 BATCH PROCESSING METRICS
            ============================
            Batches Started: %,d
            Batches Completed: %,d
            Files Processed: %,d
            Transactions Processed: %,d
            Errors: %,d
            Error Rate: %.2f%%
            Current Batch Duration: %.2f seconds
            """,
            getBatchesStarted(),
            getBatchesCompleted(),
            getFilesProcessed(),
            getTransactionsProcessed(),
            getErrors(),
            getTransactionsProcessed() > 0 ? (getErrors() * 100.0 / getTransactionsProcessed()) : 0.0,
            getCurrentBatchDuration()
        );
    }
}