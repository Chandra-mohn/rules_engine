package com.rules.orchestration.core;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;

/**
 * Real-time throughput monitoring for 80K+ TPS validation
 *
 * Tracks requests per second and validates performance targets.
 */
public final class ThroughputMonitor {

    private final LongAdder requestCount = new LongAdder();
    private final LongAdder successCount = new LongAdder();
    private final LongAdder errorCount = new LongAdder();

    private final AtomicLong lastResetTime = new AtomicLong(System.currentTimeMillis());
    private final AtomicLong windowStartTime = new AtomicLong(System.currentTimeMillis());

    // Performance thresholds
    private static final long TARGET_TPS = 80_000;
    private static final long WINDOW_SIZE_MS = 1000; // 1 second window

    public void recordRequest() {
        requestCount.increment();
        maybeResetWindow();
    }

    public void recordSuccess(long executionTimeNanos) {
        successCount.increment();
    }

    public void recordError(long executionTimeNanos) {
        errorCount.increment();
    }

    /**
     * Get current throughput (requests per second)
     */
    public double getCurrentTPS() {
        long now = System.currentTimeMillis();
        long windowStart = windowStartTime.get();
        long windowDuration = now - windowStart;

        if (windowDuration <= 0) return 0.0;

        long requests = requestCount.sum();
        return (double) requests * 1000.0 / windowDuration;
    }

    /**
     * Check if system is meeting 80K+ TPS target
     */
    public boolean isHealthy() {
        double currentTPS = getCurrentTPS();
        double errorRate = getErrorRate();

        return currentTPS >= TARGET_TPS * 0.8 && // Within 80% of target
               errorRate < 0.001; // Less than 0.1% errors
    }

    /**
     * Get current error rate
     */
    public double getErrorRate() {
        long total = requestCount.sum();
        long errors = errorCount.sum();

        return total > 0 ? (double) errors / total : 0.0;
    }

    /**
     * Reset monitoring window (called periodically)
     */
    private void maybeResetWindow() {
        long now = System.currentTimeMillis();
        long lastReset = lastResetTime.get();

        if (now - lastReset >= WINDOW_SIZE_MS) {
            if (lastResetTime.compareAndSet(lastReset, now)) {
                // Reset counters for new window
                requestCount.reset();
                successCount.reset();
                errorCount.reset();
                windowStartTime.set(now);
            }
        }
    }

    /**
     * Get throughput statistics
     */
    public ThroughputStats getStats() {
        return new ThroughputStats(
            getCurrentTPS(),
            requestCount.sum(),
            successCount.sum(),
            errorCount.sum(),
            getErrorRate(),
            isHealthy()
        );
    }

    public static class ThroughputStats {
        public final double currentTPS;
        public final long totalRequests;
        public final long successCount;
        public final long errorCount;
        public final double errorRate;
        public final boolean isHealthy;

        ThroughputStats(double currentTPS, long totalRequests, long successCount,
                       long errorCount, double errorRate, boolean isHealthy) {
            this.currentTPS = currentTPS;
            this.totalRequests = totalRequests;
            this.successCount = successCount;
            this.errorCount = errorCount;
            this.errorRate = errorRate;
            this.isHealthy = isHealthy;
        }
    }
}