package com.rules.streaming;

import com.rules.orchestration.core.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Map;

/**
 * High-performance stream processor for real-time transaction processing
 *
 * Processes individual messages with sub-millisecond latency targeting
 * through optimized orchestration layer integration.
 */
public final class StreamProcessor {

    private final ObjectMapper jsonMapper = new ObjectMapper();
    private final ContextPool contextPool = new ContextPool();
    private final StreamingMetrics metrics;

    public StreamProcessor(StreamingMetrics metrics) {
        this.metrics = metrics;
    }

    /**
     * Process a single streaming message with maximum performance
     */
    public void processMessage(String messageKey, String messageValue, String partition, String offset, long timestamp) {
        long startNanos = System.nanoTime();

        try {
            // Parse message as transaction
            Map<String, Object> transaction = jsonMapper.readValue(messageValue, Map.class);

            // Extract required fields
            String clientId = (String) transaction.get("clientId");
            String transactionCode = (String) transaction.get("transactionCode");

            if (clientId == null || transactionCode == null) {
                throw new IllegalArgumentException("Missing required fields: clientId or transactionCode");
            }

            // Create transaction context from message with COW semantics
            TransactionContext context = contextPool.acquireContext("stream-" + offset);

            // Apply all transaction data using immutable withExtendedFields
            context = context.withExtendedFields(transaction);

            // Extract standard fields if present
            if (transaction.containsKey("creditScore")) {
                context = context.withCreditScore(((Number) transaction.get("creditScore")).intValue());
            }
            if (transaction.containsKey("income")) {
                context = context.withIncome(((Number) transaction.get("income")).doubleValue());
            }
            if (transaction.containsKey("creditLimit")) {
                context = context.withCreditLimit(((Number) transaction.get("creditLimit")).intValue());
            }
            if (transaction.containsKey("amount")) {
                context = context.withAmount(((Number) transaction.get("amount")).doubleValue());
            }

            // Add Kafka metadata for tracking
            Map<String, Object> kafkaMetadata = Map.of(
                "kafka.partition", partition,
                "kafka.offset", offset,
                "kafka.timestamp", timestamp,
                "kafka.key", messageKey != null ? messageKey : ""
            );
            context = context.withExtendedFields(kafkaMetadata);

            // Execute rule through shared orchestration layer
            RuleResult result = TransactionRouter.route(clientId, transactionCode, context);

            // Return context to pool
            contextPool.releaseContext(context);

            // Record execution metrics
            long executionNanos = System.nanoTime() - startNanos;
            metrics.recordMessage(executionNanos);

            // Log successful processing (optional, can be disabled for performance)
            if (shouldLogResult(result)) {
                System.out.printf("✅ Processed: clientId=%s, code=%s, result=%s, time=%.3fms%n",
                    clientId, transactionCode, result.getMessage(), executionNanos / 1_000_000.0);
            }

        } catch (Exception e) {
            long executionNanos = System.nanoTime() - startNanos;
            metrics.recordError();

            System.err.printf("❌ Processing error for message key=%s, partition=%s, offset=%s: %s%n",
                messageKey, partition, offset, e.getMessage());

            // Optional: Send to dead letter queue or error topic
            // handleProcessingError(messageKey, messageValue, partition, offset, e);
        }
    }

    /**
     * Determine if result should be logged (can be filtered for performance)
     */
    private boolean shouldLogResult(RuleResult result) {
        // Only log interesting results to reduce console noise
        // For performance, you can add execution time filtering here if needed
        return !result.isSuccess() || result.getStatus() != RuleResult.Status.SUCCESS;
    }

    /**
     * Handle processing errors (placeholder for error handling strategy)
     */
    private void handleProcessingError(String messageKey, String messageValue, String partition, String offset, Exception error) {
        // Implementation could include:
        // - Send to dead letter queue
        // - Write to error log
        // - Send alert/notification
        // - Update error metrics

        // For now, just record in metrics (already done in processMessage)
        System.err.printf("🔍 Error details - Key: %s, Partition: %s, Offset: %s%n",
            messageKey, partition, offset);
    }

    /**
     * Get processor metrics
     */
    public StreamingMetrics getMetrics() {
        return metrics;
    }
}