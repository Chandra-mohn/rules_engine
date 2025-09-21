package com.rules.orchestration.core;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;

/**
 * High-performance metrics collection optimized for 80K+ TPS
 *
 * Uses lock-free data structures to minimize contention.
 * Designed for minimal overhead in hot paths.
 */
public final class PerformanceMetrics {

    private final String identifier;

    // Counters using LongAdder for high-contention scenarios
    private final LongAdder totalTransactions = new LongAdder();
    private final LongAdder successfulTransactions = new LongAdder();
    private final LongAdder rejectedTransactions = new LongAdder();
    private final LongAdder errors = new LongAdder();
    private final LongAdder unknownTransactions = new LongAdder();
    private final LongAdder unknownClients = new LongAdder();
    private final LongAdder clientRegistrations = new LongAdder();

    // Execution time tracking (nanoseconds)
    private final AtomicLong totalExecutionTimeNanos = new AtomicLong();
    private final AtomicLong minExecutionTimeNanos = new AtomicLong(Long.MAX_VALUE);
    private final AtomicLong maxExecutionTimeNanos = new AtomicLong();

    public PerformanceMetrics(String identifier) {
        this.identifier = identifier;
    }

    // Increment methods (optimized for hot path)
    public void incrementTotalTransactions() { totalTransactions.increment(); }
    public void incrementSuccessfulTransactions() { successfulTransactions.increment(); }
    public void incrementRejectedTransactions() { rejectedTransactions.increment(); }
    public void incrementErrors() { errors.increment(); }
    public void incrementUnknownTransactions() { unknownTransactions.increment(); }
    public void incrementUnknownClients() { unknownClients.increment(); }
    public void incrementClientRegistrations() { clientRegistrations.increment(); }

    /**
     * Record execution time with lock-free min/max tracking
     */
    public void recordExecutionTime(long nanos) {
        totalExecutionTimeNanos.addAndGet(nanos);

        // Update min with lock-free CAS
        long currentMin = minExecutionTimeNanos.get();
        while (nanos < currentMin && !minExecutionTimeNanos.compareAndSet(currentMin, nanos)) {
            currentMin = minExecutionTimeNanos.get();
        }

        // Update max with lock-free CAS
        long currentMax = maxExecutionTimeNanos.get();
        while (nanos > currentMax && !maxExecutionTimeNanos.compareAndSet(currentMax, nanos)) {
            currentMax = maxExecutionTimeNanos.get();
        }
    }

    // Getters
    public String getIdentifier() { return identifier; }
    public long getTotalTransactions() { return totalTransactions.sum(); }
    public long getSuccessfulTransactions() { return successfulTransactions.sum(); }
    public long getRejectedTransactions() { return rejectedTransactions.sum(); }
    public long getErrors() { return errors.sum(); }
    public long getUnknownTransactions() { return unknownTransactions.sum(); }
    public long getUnknownClients() { return unknownClients.sum(); }
    public long getClientRegistrations() { return clientRegistrations.sum(); }

    // Calculated metrics
    public double getSuccessRate() {
        long total = getTotalTransactions();
        return total > 0 ? (double) getSuccessfulTransactions() / total : 0.0;
    }

    public double getErrorRate() {
        long total = getTotalTransactions();
        return total > 0 ? (double) getErrors() / total : 0.0;
    }

    public long getAverageExecutionTimeNanos() {
        long total = getTotalTransactions();
        return total > 0 ? totalExecutionTimeNanos.get() / total : 0;
    }

    public long getMinExecutionTimeNanos() {
        long min = minExecutionTimeNanos.get();
        return min == Long.MAX_VALUE ? 0 : min;
    }

    public long getMaxExecutionTimeNanos() {
        return maxExecutionTimeNanos.get();
    }

    // Performance validation for 80K+ TPS requirement
    public boolean meetsPerformanceTargets() {
        return getAverageExecutionTimeNanos() < 1_000_000 && // < 1ms average
               getErrorRate() < 0.001; // < 0.1% error rate
    }
}