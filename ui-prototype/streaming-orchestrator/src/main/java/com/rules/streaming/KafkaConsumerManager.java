package com.rules.streaming;

import org.apache.kafka.clients.consumer.*;
import org.apache.kafka.common.errors.WakeupException;
import org.apache.kafka.common.serialization.StringDeserializer;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * High-performance Kafka consumer manager
 *
 * Optimized for low-latency, high-throughput message consumption
 * with automatic partition management and error handling.
 */
public final class KafkaConsumerManager {

    private final String bootstrapServers;
    private final String topicName;
    private final String consumerGroup;
    private final StreamProcessor streamProcessor;
    private final StreamingMetrics metrics;
    private final AtomicBoolean isConsuming = new AtomicBoolean(false);

    private KafkaConsumer<String, String> consumer;
    private Thread consumerThread;

    public KafkaConsumerManager(String bootstrapServers, String topicName, String consumerGroup,
                               StreamProcessor streamProcessor, StreamingMetrics metrics) {
        this.bootstrapServers = bootstrapServers;
        this.topicName = topicName;
        this.consumerGroup = consumerGroup;
        this.streamProcessor = streamProcessor;
        this.metrics = metrics;
    }

    /**
     * Start consuming messages from Kafka
     */
    public void startConsuming() {
        if (isConsuming.compareAndSet(false, true)) {
            System.out.printf("🔄 Starting Kafka consumer for topic '%s'...%n", topicName);

            // Create consumer
            consumer = createOptimizedConsumer();

            // Subscribe to topic
            consumer.subscribe(Collections.singletonList(topicName));

            // Start consumer thread
            consumerThread = new Thread(this::consumeMessages, "kafka-consumer");
            consumerThread.start();

            System.out.println("✅ Kafka consumer started successfully");
        } else {
            System.out.println("⚠️ Kafka consumer is already running");
        }
    }

    /**
     * Stop consuming messages
     */
    public void stopConsuming() {
        if (isConsuming.compareAndSet(true, false)) {
            System.out.println("🛑 Stopping Kafka consumer...");

            if (consumer != null) {
                consumer.wakeup(); // Interrupt the poll loop
            }

            if (consumerThread != null) {
                try {
                    consumerThread.join(5000); // Wait up to 5 seconds
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }

            if (consumer != null) {
                consumer.close();
            }

            System.out.println("✅ Kafka consumer stopped");
        }
    }

    /**
     * Create optimized Kafka consumer
     */
    private KafkaConsumer<String, String> createOptimizedConsumer() {
        Properties props = new Properties();

        // Connection settings
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers);
        props.put(ConsumerConfig.GROUP_ID_CONFIG, consumerGroup);
        props.put(ConsumerConfig.CLIENT_ID_CONFIG, "rules-engine-consumer");

        // Serialization
        props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());

        // Performance optimizations
        props.put(ConsumerConfig.FETCH_MIN_BYTES_CONFIG, "50000");        // Batch fetch size
        props.put(ConsumerConfig.FETCH_MAX_WAIT_MS_CONFIG, "100");        // Low latency
        props.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "1000");        // Process in batches
        props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false");     // Manual commit for reliability
        props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "latest");     // Start from latest

        // Connection optimization
        props.put(ConsumerConfig.CONNECTIONS_MAX_IDLE_MS_CONFIG, "180000");
        props.put(ConsumerConfig.REQUEST_TIMEOUT_MS_CONFIG, "30000");
        props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "10000");
        props.put(ConsumerConfig.HEARTBEAT_INTERVAL_MS_CONFIG, "3000");

        return new KafkaConsumer<>(props);
    }

    /**
     * Main message consumption loop
     */
    private void consumeMessages() {
        try {
            while (isConsuming.get()) {
                try {
                    // Poll for messages with short timeout for responsiveness
                    ConsumerRecords<String, String> records = consumer.poll(Duration.ofMillis(100));

                    if (!records.isEmpty()) {
                        processBatch(records);

                        // Manual commit for reliability
                        consumer.commitSync();
                    }

                } catch (Exception e) {
                    System.err.printf("❌ Error processing Kafka messages: %s%n", e.getMessage());
                    metrics.recordError();

                    // Add small delay before retrying
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                        break;
                    }
                }
            }
        } catch (WakeupException e) {
            // Expected when stopping consumer
            System.out.println("🔄 Kafka consumer wakeup received");
        } catch (Exception e) {
            System.err.printf("❌ Fatal error in Kafka consumer: %s%n", e.getMessage());
            e.printStackTrace();
        } finally {
            System.out.println("🔄 Kafka consumer thread exiting");
        }
    }

    /**
     * Process a batch of messages with high-throughput optimization
     */
    private void processBatch(ConsumerRecords<String, String> records) {
        long batchStartTime = System.nanoTime();
        int processedCount = 0;

        for (ConsumerRecord<String, String> record : records) {
            try {
                // Extract message metadata
                String partition = String.valueOf(record.partition());
                String offset = String.valueOf(record.offset());
                long timestamp = record.timestamp();

                // Process message through stream processor
                streamProcessor.processMessage(
                    record.key(),
                    record.value(),
                    partition,
                    offset,
                    timestamp
                );

                processedCount++;

            } catch (Exception e) {
                System.err.printf("❌ Error processing message at partition %d, offset %d: %s%n",
                    record.partition(), record.offset(), e.getMessage());
                metrics.recordError();
            }
        }

        // Record batch metrics
        long batchTime = System.nanoTime() - batchStartTime;
        metrics.recordBatch(processedCount, batchTime);

        if (processedCount > 0) {
            double batchThroughput = (processedCount * 1_000_000_000.0) / batchTime;
            System.out.printf("📦 Processed batch: %d messages in %.2f ms (%.0f TPS)%n",
                processedCount, batchTime / 1_000_000.0, batchThroughput);
        }
    }
}