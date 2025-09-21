package com.rules.batch;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Batch processing scheduler for automated file processing
 */
public final class BatchScheduler {

    private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(2);
    private volatile boolean isRunning = false;

    /**
     * Schedule batch processing at fixed intervals
     */
    public void scheduleAtFixedRate(Runnable batchTask, long initialDelay, long period, TimeUnit unit) {
        System.out.printf("📅 Scheduling batch processing: initial delay %d %s, period %d %s%n",
            initialDelay, unit.toString().toLowerCase(),
            period, unit.toString().toLowerCase());

        scheduler.scheduleAtFixedRate(() -> {
            try {
                isRunning = true;
                String timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
                System.out.printf("🚀 Starting scheduled batch processing at %s%n", timestamp);

                batchTask.run();

                System.out.printf("✅ Scheduled batch processing completed at %s%n",
                    LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
            } catch (Exception e) {
                System.err.printf("❌ Scheduled batch processing failed: %s%n", e.getMessage());
                e.printStackTrace();
            } finally {
                isRunning = false;
            }
        }, initialDelay, period, unit);
    }

    /**
     * Schedule one-time batch processing
     */
    public void scheduleOnce(Runnable batchTask, long delay, TimeUnit unit) {
        System.out.printf("⏰ Scheduling one-time batch processing in %d %s%n",
            delay, unit.toString().toLowerCase());

        scheduler.schedule(() -> {
            try {
                isRunning = true;
                String timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
                System.out.printf("🚀 Starting one-time batch processing at %s%n", timestamp);

                batchTask.run();

                System.out.printf("✅ One-time batch processing completed at %s%n",
                    LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
            } catch (Exception e) {
                System.err.printf("❌ One-time batch processing failed: %s%n", e.getMessage());
                e.printStackTrace();
            } finally {
                isRunning = false;
            }
        }, delay, unit);
    }

    /**
     * Check if batch processing is currently running
     */
    public boolean isRunning() {
        return isRunning;
    }

    /**
     * Shutdown scheduler
     */
    public void shutdown() {
        System.out.println("🛑 Shutting down batch scheduler...");
        scheduler.shutdown();
        try {
            if (!scheduler.awaitTermination(60, TimeUnit.SECONDS)) {
                scheduler.shutdownNow();
            }
        } catch (InterruptedException e) {
            scheduler.shutdownNow();
        }
    }
}