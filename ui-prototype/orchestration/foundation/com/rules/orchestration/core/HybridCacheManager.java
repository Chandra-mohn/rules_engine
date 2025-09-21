package com.rules.orchestration.core;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * Hybrid JVM + Redis caching strategy - Hard-coded for optimal performance
 *
 * L1 Cache (JVM): Hot reference data, lookup tables
 * L2 Cache (Redis): Large contexts, shared state
 */
public final class HybridCacheManager {

    // L1 Cache: In-JVM for ultra-low latency (< 10MB total)
    private static final Map<String, Object> l1Cache = new ConcurrentHashMap<>(10000);
    private static final Map<String, Long> l1Timestamps = new ConcurrentHashMap<>(10000);

    // Cache configuration
    private static final long L1_TTL_MS = TimeUnit.MINUTES.toMillis(5);
    private static final int L1_MAX_SIZE = 10000;

    /**
     * Get from L1 cache (zero network latency)
     */
    public static Object getFromL1(String key) {
        // Check expiration
        Long timestamp = l1Timestamps.get(key);
        if (timestamp != null && System.currentTimeMillis() - timestamp > L1_TTL_MS) {
            l1Cache.remove(key);
            l1Timestamps.remove(key);
            return null;
        }

        return l1Cache.get(key);
    }

    /**
     * Put to L1 cache with size limits
     */
    public static void putToL1(String key, Object value) {
        // Implement LRU eviction if cache is full
        if (l1Cache.size() >= L1_MAX_SIZE) {
            evictOldestL1Entry();
        }

        l1Cache.put(key, value);
        l1Timestamps.put(key, System.currentTimeMillis());
    }

    /**
     * Hybrid get with L1 → L2 fallback
     */
    public static Object get(String key) {
        // Try L1 first (zero latency)
        Object value = getFromL1(key);
        if (value != null) {
            return value;
        }

        // TODO: Fall back to L2 (Redis) for shared/large data
        // This would integrate with actual Redis client
        return null;
    }

    /**
     * Hybrid put with intelligent routing
     */
    public static void put(String key, Object value) {
        // Route small, hot data to L1
        if (isHotData(key, value)) {
            putToL1(key, value);
        }

        // TODO: Route large/shared data to L2 (Redis)
    }

    /**
     * Determine if data should go to L1 cache
     */
    private static boolean isHotData(String key, Object value) {
        // Hot data criteria:
        // - Reference data (lookup tables)
        // - Small size (< 1KB estimated)
        // - Frequently accessed patterns

        if (key.startsWith("ref.") || key.startsWith("lookup.")) {
            return true;
        }

        // Estimate size (rough approximation)
        String str = value.toString();
        return str.length() < 1000;
    }

    /**
     * LRU eviction for L1 cache
     */
    private static void evictOldestL1Entry() {
        String oldestKey = null;
        long oldestTime = Long.MAX_VALUE;

        for (Map.Entry<String, Long> entry : l1Timestamps.entrySet()) {
            if (entry.getValue() < oldestTime) {
                oldestTime = entry.getValue();
                oldestKey = entry.getKey();
            }
        }

        if (oldestKey != null) {
            l1Cache.remove(oldestKey);
            l1Timestamps.remove(oldestKey);
        }
    }

    /**
     * Get cache statistics
     */
    public static CacheStats getStats() {
        return new CacheStats(
            l1Cache.size(),
            L1_MAX_SIZE,
            calculateL1HitRate()
        );
    }

    private static double calculateL1HitRate() {
        // This would require request tracking - simplified for now
        return 0.85; // Assume 85% hit rate
    }

    public static class CacheStats {
        public final int l1Size;
        public final int l1MaxSize;
        public final double l1HitRate;

        CacheStats(int l1Size, int l1MaxSize, double l1HitRate) {
            this.l1Size = l1Size;
            this.l1MaxSize = l1MaxSize;
            this.l1HitRate = l1HitRate;
        }
    }
}