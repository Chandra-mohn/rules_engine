package com.rules.streaming;

import com.rules.orchestration.core.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.CountDownLatch;

/**
 * Streaming Kafka Processing Orchestrator
 *
 * Real-time transaction processing with sub-millisecond latency targeting.
 * Optimized for 225K+ TPS with Kafka integration.
 */
public final class StreamingOrchestrator {

    private final KafkaConsumerManager kafkaManager;
    private final StreamProcessor streamProcessor;
    private final StreamingMetrics metrics;
    private final AtomicBoolean isRunning = new AtomicBoolean(false);
    private final CountDownLatch shutdownLatch = new CountDownLatch(1);

    public StreamingOrchestrator(String bootstrapServers, String topicName, String consumerGroup) {
        this.metrics = new StreamingMetrics();
        this.streamProcessor = new StreamProcessor(metrics);
        this.kafkaManager = new KafkaConsumerManager(
            bootstrapServers,
            topicName,
            consumerGroup,
            streamProcessor,
            metrics
        );
    }

    /**
     * Main entry point for streaming processing
     */
    public static void main(String[] args) {
        if (args.length < 3) {
            System.err.println("Usage: StreamingOrchestrator <bootstrap-servers> <topic> <consumer-group>");
            System.err.println("Example: StreamingOrchestrator localhost:9092 transactions rules-engine-group");
            System.exit(1);
        }

        String bootstrapServers = args[0];
        String topicName = args[1];
        String consumerGroup = args[2];

        StreamingOrchestrator orchestrator = new StreamingOrchestrator(
            bootstrapServers, topicName, consumerGroup
        );

        // Graceful shutdown hook
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            System.out.println("🛑 Shutting down streaming orchestrator...");
            orchestrator.stop();
        }));

        try {
            orchestrator.start();
        } catch (Exception e) {
            System.err.println("Streaming orchestrator failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Start streaming processing
     */
    public void start() {
        if (isRunning.compareAndSet(false, true)) {
            System.out.println("🚀 Starting streaming orchestrator...");

            // Initialize metrics tracking
            metrics.startStreaming();

            // Start Kafka consumer
            kafkaManager.startConsuming();

            System.out.println("✅ Streaming orchestrator started successfully");
            System.out.println("📊 Monitoring real-time performance metrics...");

            // Start metrics reporting
            startMetricsReporting();

            try {
                // Wait for shutdown signal
                shutdownLatch.await();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                System.out.println("⚠️ Streaming orchestrator interrupted");
            }
        } else {
            System.out.println("⚠️ Streaming orchestrator is already running");
        }
    }

    /**
     * Stop streaming processing
     */
    public void stop() {
        if (isRunning.compareAndSet(true, false)) {
            System.out.println("🛑 Stopping streaming orchestrator...");

            // Stop Kafka consumer
            kafkaManager.stopConsuming();

            // Generate final metrics report
            System.out.println("📊 Final performance report:");
            System.out.println(metrics.generateReport());

            // Check SLA compliance
            double currentTPS = metrics.getCurrentTPS();
            if (currentTPS >= 225000) {
                System.out.printf("🎯 SLA ACHIEVED: %.0f TPS (target: 225K+ TPS)%n", currentTPS);
            } else {
                System.out.printf("⚠️ SLA STATUS: %.0f TPS (target: 225K+ TPS)%n", currentTPS);
            }

            shutdownLatch.countDown();
            System.out.println("✅ Streaming orchestrator stopped");
        }
    }

    /**
     * Start real-time metrics reporting
     */
    private void startMetricsReporting() {
        Thread metricsThread = new Thread(() -> {
            while (isRunning.get()) {
                try {
                    Thread.sleep(10000); // Report every 10 seconds

                    if (metrics.getMessagesProcessed() > 0) {
                        System.out.printf("📊 Real-time metrics: %.0f TPS | %.2f ms avg latency | %,d total processed%n",
                            metrics.getCurrentTPS(),
                            metrics.getAverageLatencyMillis(),
                            metrics.getMessagesProcessed()
                        );

                        // Performance alert
                        double currentTPS = metrics.getCurrentTPS();
                        if (currentTPS > 0 && currentTPS < 225000) {
                            System.out.printf("⚠️ PERFORMANCE ALERT: TPS below target (%.0f < 225K)%n", currentTPS);
                        }
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        });

        metricsThread.setDaemon(true);
        metricsThread.setName("metrics-reporter");
        metricsThread.start();
    }

    /**
     * Get current streaming metrics
     */
    public StreamingMetrics getMetrics() {
        return metrics;
    }

    /**
     * Check if orchestrator is running
     */
    public boolean isRunning() {
        return isRunning.get();
    }
}