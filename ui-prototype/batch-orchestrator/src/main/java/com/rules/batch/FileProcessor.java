package com.rules.batch;

import com.rules.orchestration.core.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * High-performance file processor for transaction data
 *
 * Optimized for batch processing with minimal memory allocation
 * and maximum throughput.
 */
public final class FileProcessor {

    private final ObjectMapper jsonMapper = new ObjectMapper();
    private final ContextPool contextPool = new ContextPool();

    /**
     * Process a single transaction file
     */
    public FileProcessingResult processFile(Path inputFile, Path outputDir) {
        long startTime = System.nanoTime();
        String fileName = inputFile.getFileName().toString();

        try {
            if (fileName.endsWith(".json")) {
                return processJsonFile(inputFile, outputDir);
            } else if (fileName.endsWith(".csv")) {
                return processCsvFile(inputFile, outputDir);
            } else {
                throw new IllegalArgumentException("Unsupported file format: " + fileName);
            }
        } catch (Exception e) {
            long processingTime = System.nanoTime() - startTime;
            return FileProcessingResult.error(inputFile, e.getMessage(), processingTime);
        }
    }

    /**
     * Process JSON transaction file
     */
    private FileProcessingResult processJsonFile(Path inputFile, Path outputDir) throws IOException {
        long startTime = System.nanoTime();
        int transactionCount = 0;
        List<String> errors = new ArrayList<>();

        // Prepare output file
        Path outputFile = outputDir.resolve(
            inputFile.getFileName().toString().replace(".json", "_results.json")
        );

        try (BufferedReader reader = Files.newBufferedReader(inputFile);
             BufferedWriter writer = Files.newBufferedWriter(outputFile)) {

            writer.write("[\n");
            String line;
            boolean firstResult = true;

            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("#")) continue;

                try {
                    // Parse transaction
                    Map<String, Object> transaction = jsonMapper.readValue(line, Map.class);

                    // Extract required fields
                    String clientId = (String) transaction.get("clientId");
                    String transactionCode = (String) transaction.get("transactionCode");

                    if (clientId == null || transactionCode == null) {
                        errors.add("Missing required fields in transaction: " + line);
                        continue;
                    }

                    // Create transaction context with COW semantics
                    TransactionContext context = contextPool.acquireContext("batch-txn-" + transactionCount);

                    // Apply transaction data using immutable withExtendedFields
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

                    // Execute rule through orchestration layer
                    long ruleStartTime = System.nanoTime();
                    RuleResult result = TransactionRouter.route(clientId, transactionCode, context);
                    long ruleExecutionTime = System.nanoTime() - ruleStartTime;

                    // Write result
                    if (!firstResult) {
                        writer.write(",\n");
                    }
                    String resultJson = jsonMapper.writeValueAsString(Map.of(
                        "transactionId", transaction.get("transactionId"),
                        "clientId", clientId,
                        "transactionCode", transactionCode,
                        "result", result.getMessage(),
                        "status", result.getStatus().toString(),
                        "executionTime", ruleExecutionTime / 1_000_000.0, // Convert to milliseconds
                        "timestamp", System.currentTimeMillis()
                    ));
                    writer.write("  " + resultJson);
                    firstResult = false;

                    // Return context to pool
                    contextPool.releaseContext(context);
                    transactionCount++;

                } catch (Exception e) {
                    errors.add("Processing error for line: " + line + " - " + e.getMessage());
                }
            }

            writer.write("\n]");
        }

        long processingTime = System.nanoTime() - startTime;

        return new FileProcessingResult(
            inputFile,
            outputFile,
            transactionCount,
            processingTime,
            errors.isEmpty(),
            errors
        );
    }

    /**
     * Process CSV transaction file
     */
    private FileProcessingResult processCsvFile(Path inputFile, Path outputDir) throws IOException {
        long startTime = System.nanoTime();
        int transactionCount = 0;
        List<String> errors = new ArrayList<>();

        // Prepare output file
        Path outputFile = outputDir.resolve(
            inputFile.getFileName().toString().replace(".csv", "_results.csv")
        );

        try (BufferedReader reader = Files.newBufferedReader(inputFile);
             BufferedWriter writer = Files.newBufferedWriter(outputFile)) {

            // Read header
            String headerLine = reader.readLine();
            if (headerLine == null) {
                throw new IOException("Empty CSV file");
            }

            String[] headers = headerLine.split(",");
            int clientIdIndex = findColumnIndex(headers, "clientId");
            int transactionCodeIndex = findColumnIndex(headers, "transactionCode");

            if (clientIdIndex == -1 || transactionCodeIndex == -1) {
                throw new IOException("Required columns 'clientId' and 'transactionCode' not found");
            }

            // Write output header
            writer.write(headerLine + ",result,status,executionTime,timestamp\n");

            String line;
            while ((line = reader.readLine()) != null) {
                try {
                    String[] values = line.split(",");

                    if (values.length <= Math.max(clientIdIndex, transactionCodeIndex)) {
                        errors.add("Insufficient columns in line: " + line);
                        continue;
                    }

                    String clientId = values[clientIdIndex].trim();
                    String transactionCode = values[transactionCodeIndex].trim();

                    // Create transaction context from CSV data
                    TransactionContext context = contextPool.acquireContext("batch-csv-" + transactionCount);

                    // Build extended fields map for CSV data
                    Map<String, Object> csvData = new HashMap<>();
                    for (int i = 0; i < Math.min(headers.length, values.length); i++) {
                        String header = headers[i].trim();
                        String value = values[i].trim();
                        csvData.put(header, value);

                        // Apply to standard fields if applicable
                        try {
                            switch (header.toLowerCase()) {
                                case "creditscore":
                                    context = context.withCreditScore(Integer.parseInt(value));
                                    break;
                                case "income":
                                    context = context.withIncome(Double.parseDouble(value));
                                    break;
                                case "creditlimit":
                                    context = context.withCreditLimit(Integer.parseInt(value));
                                    break;
                                case "amount":
                                    context = context.withAmount(Double.parseDouble(value));
                                    break;
                                case "status":
                                    context = context.withStatus(value);
                                    break;
                            }
                        } catch (NumberFormatException ignored) {
                            // Keep as string in extended fields
                        }
                    }

                    // Apply all CSV data as extended fields
                    context = context.withExtendedFields(csvData);

                    // Execute rule
                    long ruleStartTime = System.nanoTime();
                    RuleResult result = TransactionRouter.route(clientId, transactionCode, context);
                    long ruleExecutionTime = System.nanoTime() - ruleStartTime;

                    // Write result
                    writer.write(line + "," + result.getMessage() + "," + result.getStatus() + "," +
                               (ruleExecutionTime / 1_000_000.0) + "," + System.currentTimeMillis() + "\n");

                    contextPool.releaseContext(context);
                    transactionCount++;

                } catch (Exception e) {
                    errors.add("Processing error for line: " + line + " - " + e.getMessage());
                }
            }
        }

        long processingTime = System.nanoTime() - startTime;

        return new FileProcessingResult(
            inputFile,
            outputFile,
            transactionCount,
            processingTime,
            errors.isEmpty(),
            errors
        );
    }

    /**
     * Find column index by name
     */
    private int findColumnIndex(String[] headers, String columnName) {
        for (int i = 0; i < headers.length; i++) {
            if (headers[i].trim().equalsIgnoreCase(columnName)) {
                return i;
            }
        }
        return -1;
    }
}