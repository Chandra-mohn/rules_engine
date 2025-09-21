package com.rules.streaming;

import java.util.concurrent.atomic.LongAdder;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Real-time streaming metrics for performance monitoring and SLA validation
 */
public final class StreamingMetrics {

    // Message processing counters
    private final LongAdder messagesProcessed = new LongAdder();
    private final LongAdder errors = new LongAdder();
    private final LongAdder batchesProcessed = new LongAdder();

    // Timing metrics
    private final AtomicLong totalProcessingNanos = new AtomicLong();
    private final AtomicLong minLatencyNanos = new AtomicLong(Long.MAX_VALUE);
    private final AtomicLong maxLatencyNanos = new AtomicLong();

    // Throughput tracking
    private volatile long streamingStartTime;
    private volatile long lastThroughputCheck;
    private volatile long lastMessageCount;
    private volatile double currentTPS;

    public void startStreaming() {
        streamingStartTime = System.currentTimeMillis();
        lastThroughputCheck = streamingStartTime;
        lastMessageCount = 0;
    }

    /**
     * Record processing of a single message
     */
    public void recordMessage(long processingNanos) {
        messagesProcessed.increment();
        totalProcessingNanos.addAndGet(processingNanos);

        // Update latency bounds
        updateLatencyBounds(processingNanos);

        // Update throughput calculation periodically
        updateThroughput();
    }

    /**
     * Record processing of a message batch
     */
    public void recordBatch(int messageCount, long batchProcessingNanos) {
        batchesProcessed.increment();
        messagesProcessed.add(messageCount);
        totalProcessingNanos.addAndGet(batchProcessingNanos);

        // Update throughput
        updateThroughput();
    }

    /**
     * Record processing error
     */
    public void recordError() {
        errors.increment();
    }

    /**
     * Update latency bounds atomically
     */
    private void updateLatencyBounds(long latencyNanos) {
        // Update minimum
        long currentMin = minLatencyNanos.get();
        while (latencyNanos < currentMin && !minLatencyNanos.compareAndSet(currentMin, latencyNanos)) {
            currentMin = minLatencyNanos.get();
        }

        // Update maximum
        long currentMax = maxLatencyNanos.get();
        while (latencyNanos > currentMax && !maxLatencyNanos.compareAndSet(currentMax, latencyNanos)) {
            currentMax = maxLatencyNanos.get();
        }
    }

    /**
     * Update throughput calculation
     */
    private synchronized void updateThroughput() {
        long currentTime = System.currentTimeMillis();
        long currentMessages = messagesProcessed.sum();

        // Update TPS every second
        if (currentTime - lastThroughputCheck >= 1000) {
            long timeDiff = currentTime - lastThroughputCheck;
            long messageDiff = currentMessages - lastMessageCount;

            if (timeDiff > 0) {
                currentTPS = (messageDiff * 1000.0) / timeDiff;
            }

            lastThroughputCheck = currentTime;
            lastMessageCount = currentMessages;
        }
    }

    // Getters
    public long getMessagesProcessed() { return messagesProcessed.sum(); }
    public long getErrors() { return errors.sum(); }
    public long getBatchesProcessed() { return batchesProcessed.sum(); }

    public double getAverageLatencyNanos() {
        long messages = messagesProcessed.sum();
        return messages > 0 ? (double) totalProcessingNanos.get() / messages : 0;
    }

    public double getAverageLatencyMillis() {
        return getAverageLatencyNanos() / 1_000_000.0;
    }

    public double getMinLatencyMillis() {
        long min = minLatencyNanos.get();
        return min == Long.MAX_VALUE ? 0 : min / 1_000_000.0;
    }

    public double getMaxLatencyMillis() {
        return maxLatencyNanos.get() / 1_000_000.0;
    }

    public double getCurrentTPS() {
        return currentTPS;
    }

    public double getOverallTPS() {
        if (streamingStartTime == 0) return 0;

        long totalTime = System.currentTimeMillis() - streamingStartTime;
        return totalTime > 0 ? (messagesProcessed.sum() * 1000.0) / totalTime : 0;
    }

    public double getErrorRate() {
        long total = messagesProcessed.sum();
        return total > 0 ? (errors.sum() * 100.0) / total : 0;
    }

    public double getStreamingDurationSeconds() {
        return streamingStartTime > 0 ? (System.currentTimeMillis() - streamingStartTime) / 1000.0 : 0;
    }

    /**
     * Check if performance meets SLA requirements
     */
    public boolean meetsSLA() {
        return getCurrentTPS() >= 225000 && // 225K+ TPS
               getAverageLatencyMillis() <= 5 && // < 5ms average latency
               getErrorRate() <= 1.0; // < 1% error rate
    }

    /**
     * Generate comprehensive metrics report
     */
    public String generateReport() {
        return String.format("""
            🚀 STREAMING PERFORMANCE METRICS
            ================================
            Messages Processed: %,d
            Batches Processed: %,d
            Streaming Duration: %.2f seconds

            📊 THROUGHPUT
            Current TPS: %,.0f
            Overall TPS: %,.0f
            SLA Target: 225,000 TPS
            SLA Status: %s

            ⏱️ LATENCY
            Average: %.3f ms
            Minimum: %.3f ms
            Maximum: %.3f ms
            SLA Target: < 5ms average

            ❌ ERROR METRICS
            Total Errors: %,d
            Error Rate: %.2f%%
            SLA Target: < 1%%

            🎯 OVERALL SLA: %s
            """,
            getMessagesProcessed(),
            getBatchesProcessed(),
            getStreamingDurationSeconds(),
            getCurrentTPS(),
            getOverallTPS(),
            getCurrentTPS() >= 225000 ? "✅ ACHIEVED" : "❌ MISSED",
            getAverageLatencyMillis(),
            getMinLatencyMillis(),
            getMaxLatencyMillis(),
            getErrors(),
            getErrorRate(),
            meetsSLA() ? "✅ ACHIEVED" : "❌ MISSED"
        );
    }
}