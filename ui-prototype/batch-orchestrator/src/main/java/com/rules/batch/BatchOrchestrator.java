package com.rules.batch;

import com.rules.orchestration.core.*;
import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Stream;

/**
 * Batch File Processing Orchestrator
 *
 * Optimized for high-throughput file processing with 225K+ TPS capability.
 * Processes transaction files and executes rules through shared orchestration layer.
 */
public final class BatchOrchestrator {

    private final FileProcessor fileProcessor;
    private final BatchScheduler scheduler;
    private final BatchMetrics metrics;
    private final ExecutorService processingPool;

    public BatchOrchestrator() {
        this.fileProcessor = new FileProcessor();
        this.scheduler = new BatchScheduler();
        this.metrics = new BatchMetrics();
        this.processingPool = Executors.newFixedThreadPool(
            Runtime.getRuntime().availableProcessors() * 2
        );
    }

    /**
     * Main entry point for batch processing
     */
    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: BatchOrchestrator <input-directory> <output-directory>");
            System.exit(1);
        }

        BatchOrchestrator orchestrator = new BatchOrchestrator();

        try {
            orchestrator.processBatch(
                Paths.get(args[0]),
                Paths.get(args[1])
            );
        } catch (Exception e) {
            System.err.println("Batch processing failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Process all files in a directory with high-throughput optimization
     */
    public BatchResult processBatch(Path inputDir, Path outputDir) throws IOException {
        long startTime = System.currentTimeMillis();
        metrics.startBatch();

        // Validate directories
        if (!Files.exists(inputDir) || !Files.isDirectory(inputDir)) {
            throw new IllegalArgumentException("Input directory does not exist: " + inputDir);
        }
        Files.createDirectories(outputDir);

        // Discover transaction files
        List<Path> transactionFiles = discoverTransactionFiles(inputDir);
        System.out.printf("🔍 Discovered %d transaction files for processing%n", transactionFiles.size());

        // Process files in parallel with controlled concurrency
        List<Future<FileProcessingResult>> futures = new ArrayList<>();

        for (Path file : transactionFiles) {
            futures.add(processingPool.submit(() ->
                fileProcessor.processFile(file, outputDir)
            ));
        }

        // Collect results with progress tracking
        List<FileProcessingResult> results = new ArrayList<>();
        int completed = 0;

        for (Future<FileProcessingResult> future : futures) {
            try {
                FileProcessingResult result = future.get();
                results.add(result);
                completed++;

                if (completed % 10 == 0) {
                    System.out.printf("📊 Progress: %d/%d files processed%n",
                        completed, transactionFiles.size());
                }
            } catch (ExecutionException | InterruptedException e) {
                metrics.recordError();
                System.err.println("File processing error: " + e.getMessage());
            }
        }

        long totalTime = System.currentTimeMillis() - startTime;

        // Calculate aggregate metrics
        long totalTransactions = results.stream()
            .mapToLong(FileProcessingResult::transactionCount)
            .sum();

        double throughputTPS = (totalTransactions * 1000.0) / totalTime;

        BatchResult batchResult = new BatchResult(
            transactionFiles.size(),
            totalTransactions,
            totalTime,
            throughputTPS,
            results
        );

        // Performance validation
        System.out.printf("✅ Batch processing completed%n");
        System.out.printf("   📁 Files processed: %d%n", batchResult.getFilesProcessed());
        System.out.printf("   📊 Total transactions: %,d%n", batchResult.getTotalTransactions());
        System.out.printf("   ⏱️  Processing time: %,.2f seconds%n", totalTime / 1000.0);
        System.out.printf("   🚀 Throughput: %,.0f TPS%n", throughputTPS);

        if (throughputTPS >= 225000) {
            System.out.printf("🎯 SLA ACHIEVED: %,.0f TPS (target: 225K+ TPS)%n", throughputTPS);
        } else {
            System.out.printf("⚠️  SLA MISSED: %,.0f TPS (target: 225K+ TPS)%n", throughputTPS);
        }

        return batchResult;
    }

    /**
     * Discover transaction files in input directory
     */
    private List<Path> discoverTransactionFiles(Path inputDir) throws IOException {
        try (Stream<Path> files = Files.walk(inputDir)) {
            return files
                .filter(Files::isRegularFile)
                .filter(path -> path.getFileName().toString().endsWith(".json") ||
                               path.getFileName().toString().endsWith(".csv"))
                .sorted()
                .toList();
        }
    }

    /**
     * Graceful shutdown
     */
    public void shutdown() {
        processingPool.shutdown();
        try {
            if (!processingPool.awaitTermination(60, TimeUnit.SECONDS)) {
                processingPool.shutdownNow();
            }
        } catch (InterruptedException e) {
            processingPool.shutdownNow();
        }
    }
}