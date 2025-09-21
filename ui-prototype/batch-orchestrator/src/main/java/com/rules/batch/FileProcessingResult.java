package com.rules.batch;

import java.nio.file.Path;
import java.util.List;

/**
 * Result of processing a single file
 */
public record FileProcessingResult(
    Path inputFile,
    Path outputFile,
    int transactionCount,
    long processingTimeNanos,
    boolean success,
    List<String> errors
) {

    public double getProcessingTimeMillis() {
        return processingTimeNanos / 1_000_000.0;
    }

    public double getThroughputTPS() {
        double timeSeconds = processingTimeNanos / 1_000_000_000.0;
        return timeSeconds > 0 ? transactionCount / timeSeconds : 0;
    }

    public static FileProcessingResult error(Path inputFile, String errorMessage, long processingTimeNanos) {
        return new FileProcessingResult(
            inputFile,
            null,
            0,
            processingTimeNanos,
            false,
            List.of(errorMessage)
        );
    }

    @Override
    public String toString() {
        return String.format(
            "FileProcessingResult{file=%s, transactions=%d, time=%.2fms, throughput=%.0f TPS, success=%s}",
            inputFile.getFileName(),
            transactionCount,
            getProcessingTimeMillis(),
            getThroughputTPS(),
            success
        );
    }
}

/**
 * Overall batch processing result
 */
class BatchResult {
    private final int filesProcessed;
    private final long totalTransactions;
    private final long totalTimeMillis;
    private final double throughputTPS;
    private final List<FileProcessingResult> fileResults;

    public BatchResult(int filesProcessed, long totalTransactions, long totalTimeMillis,
                      double throughputTPS, List<FileProcessingResult> fileResults) {
        this.filesProcessed = filesProcessed;
        this.totalTransactions = totalTransactions;
        this.totalTimeMillis = totalTimeMillis;
        this.throughputTPS = throughputTPS;
        this.fileResults = fileResults;
    }

    // Getters
    public int getFilesProcessed() { return filesProcessed; }
    public long getTotalTransactions() { return totalTransactions; }
    public long getTotalTimeMillis() { return totalTimeMillis; }
    public double getThroughputTPS() { return throughputTPS; }
    public List<FileProcessingResult> getFileResults() { return fileResults; }

    public boolean achievedSLA() {
        return throughputTPS >= 225000; // 225K+ TPS target
    }

    @Override
    public String toString() {
        return String.format(
            "BatchResult{files=%d, transactions=%,d, time=%,.2fs, throughput=%,.0f TPS, SLA=%s}",
            filesProcessed,
            totalTransactions,
            totalTimeMillis / 1000.0,
            throughputTPS,
            achievedSLA() ? "✅ ACHIEVED" : "❌ MISSED"
        );
    }
}