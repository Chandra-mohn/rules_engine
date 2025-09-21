package com.rules.orchestration.core;

/**
 * Memory optimization utilities for high-throughput operation
 */
public final class MemoryOptimizer {

    // Memory thresholds
    private static final long HEAP_WARNING_THRESHOLD = 0.8; // 80% heap usage
    private static final long GC_TIME_WARNING_MS = 50; // 50ms GC pause

    /**
     * Check if memory pressure is high
     */
    public static boolean isMemoryPressureHigh() {
        Runtime runtime = Runtime.getRuntime();
        long totalMemory = runtime.totalMemory();
        long freeMemory = runtime.freeMemory();
        long usedMemory = totalMemory - freeMemory;

        double usageRatio = (double) usedMemory / runtime.maxMemory();
        return usageRatio > HEAP_WARNING_THRESHOLD;
    }

    /**
     * Suggest GC if memory pressure is high
     */
    public static void suggestGCIfNeeded() {
        if (isMemoryPressureHigh()) {
            System.gc(); // Suggestion only - JVM may ignore
        }
    }

    /**
     * Get memory statistics
     */
    public static MemoryStats getMemoryStats() {
        Runtime runtime = Runtime.getRuntime();
        return new MemoryStats(
            runtime.maxMemory(),
            runtime.totalMemory(),
            runtime.freeMemory()
        );
    }

    public static class MemoryStats {
        public final long maxMemory;
        public final long totalMemory;
        public final long freeMemory;
        public final long usedMemory;
        public final double usagePercentage;

        MemoryStats(long maxMemory, long totalMemory, long freeMemory) {
            this.maxMemory = maxMemory;
            this.totalMemory = totalMemory;
            this.freeMemory = freeMemory;
            this.usedMemory = totalMemory - freeMemory;
            this.usagePercentage = (double) usedMemory / maxMemory * 100.0;
        }
    }
}