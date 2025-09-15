package com.rules.runtime;

import com.rules.codegen.DirectJavaCodeGenerator;
import com.rules.context.RuleContext;
import com.rules.engine.Rule;
import com.rules.engine.RuleResult;
import com.rules.grammar.RulesLexer;
import com.rules.grammar.RulesParser;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Main execution engine for dynamically compiled rules.
 * Handles the complete pipeline: DSL -> Java -> Bytecode -> Execution
 */
public class RuleExecutionEngine {
    
    private final RuleCompiler compiler;
    private final HotRuleClassLoader classLoader;
    private final DirectJavaCodeGenerator codeGenerator;
    
    // Thread-safe rule registry
    private final ConcurrentHashMap<String, CompiledRuleInfo> compiledRules = new ConcurrentHashMap<>();
    private final ReadWriteLock engineLock = new ReentrantReadWriteLock();
    
    // Performance tracking
    private final AtomicLong totalExecutions = new AtomicLong(0);
    private final AtomicLong totalExecutionTimeMs = new AtomicLong(0);
    
    public RuleExecutionEngine() {
        this.compiler = new RuleCompiler();
        this.classLoader = new HotRuleClassLoader();
        this.codeGenerator = new DirectJavaCodeGenerator();
    }
    
    /**
     * Compile a rule from DSL and make it ready for execution
     * @param ruleId Unique identifier for the rule
     * @param ruleDsl Rule DSL source code
     * @return CompilationResult with success status and details
     */
    public CompilationResult compileRule(String ruleId, String ruleDsl) {
        engineLock.writeLock().lock();
        try {
            long startTime = System.currentTimeMillis();
            
            // Step 1: Parse DSL and generate Java code
            String javaCode;
            try {
                var input = CharStreams.fromString(ruleDsl);
                var lexer = new RulesLexer(input);
                var tokens = new CommonTokenStream(lexer);
                var parser = new RulesParser(tokens);
                var parseTree = parser.ruleSet();
                
                javaCode = codeGenerator.generateCode(parseTree);
            } catch (Exception e) {
                return CompilationResult.failure("DSL parsing failed: " + e.getMessage());
            }
            
            // Step 2: Compile Java to bytecode
            String className = extractClassName(javaCode);
            RuleCompiler.CompiledClass compiledClass;
            try {
                compiledClass = compiler.compileRule(className, javaCode);
            } catch (RuleCompiler.CompilationException e) {
                return CompilationResult.failure("Java compilation failed: " + e.getMessage());
            }
            
            // Step 3: Load class into ClassLoader
            Class<?> ruleClass;
            try {
                ruleClass = classLoader.defineRuleClass(className, compiledClass.bytecode);
            } catch (Exception e) {
                return CompilationResult.failure("Class loading failed: " + e.getMessage());
            }
            
            // Step 4: Store compiled rule info
            CompiledRuleInfo ruleInfo = new CompiledRuleInfo(
                ruleId, ruleDsl, javaCode, className, ruleClass, 
                System.currentTimeMillis() - startTime
            );
            
            // Remove old version if exists
            CompiledRuleInfo oldRule = compiledRules.put(ruleId, ruleInfo);
            if (oldRule != null) {
                classLoader.unloadClass(oldRule.className);
            }
            
            return CompilationResult.success(ruleId, className, ruleInfo.compilationTimeMs);

        } finally {
            engineLock.writeLock().unlock();
        }
    }
    
    /**
     * Execute a compiled rule
     * @param ruleId Unique rule identifier
     * @param context Execution context with data
     * @return ExecutionResult with outcome and performance metrics
     */
    public ExecutionResult executeRule(String ruleId, RuleContext context) {
        engineLock.readLock().lock();
        try {
            long startTime = System.nanoTime();
            
            CompiledRuleInfo ruleInfo = compiledRules.get(ruleId);
            if (ruleInfo == null) {
                return ExecutionResult.failure("Rule not found: " + ruleId);
            }
            
            try {
                Rule ruleInstance = classLoader.createRuleInstance(ruleInfo.className);
                RuleResult result = ruleInstance.execute(context);
                
                long executionTimeNs = System.nanoTime() - startTime;
                long executionTimeMs = executionTimeNs / 1_000_000;
                
                // Update statistics
                totalExecutions.incrementAndGet();
                totalExecutionTimeMs.addAndGet(executionTimeMs);
                
                return ExecutionResult.success(ruleId, result, executionTimeMs);
                
            } catch (Exception e) {
                long executionTimeNs = System.nanoTime() - startTime;
                return ExecutionResult.failure("Rule execution failed: " + e.getMessage(), 
                                             executionTimeNs / 1_000_000);
            }
            
        } finally {
            engineLock.readLock().unlock();
        }
    }
    
    /**
     * Check if a rule is compiled and ready
     * @param ruleId Rule identifier
     * @return true if ready for execution
     */
    public boolean isRuleReady(String ruleId) {
        return compiledRules.containsKey(ruleId);
    }
    
    /**
     * Get information about a compiled rule
     * @param ruleId Rule identifier
     * @return CompiledRuleInfo or null if not found
     */
    public CompiledRuleInfo getRuleInfo(String ruleId) {
        return compiledRules.get(ruleId);
    }
    
    /**
     * Remove a rule from the engine
     * @param ruleId Rule identifier
     * @return true if rule was removed
     */
    public boolean unloadRule(String ruleId) {
        engineLock.writeLock().lock();
        try {
            CompiledRuleInfo ruleInfo = compiledRules.remove(ruleId);
            if (ruleInfo != null) {
                classLoader.unloadClass(ruleInfo.className);
                return true;
            }
            return false;
        } finally {
            engineLock.writeLock().unlock();
        }
    }
    
    /**
     * Get engine performance statistics
     * @return EngineStats with current metrics
     */
    public EngineStats getStats() {
        return new EngineStats(
            compiledRules.size(),
            totalExecutions.get(),
            totalExecutionTimeMs.get(),
            classLoader.getStats(),
            compiler.getStats()
        );
    }
    
    /**
     * Extract class name from generated Java code
     */
    private String extractClassName(String javaCode) {
        // Look for "public class ClassName"
        String[] lines = javaCode.split("\n");
        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.startsWith("public class ")) {
                String[] parts = trimmed.split("\\s+");
                if (parts.length >= 3) {
                    return "com.rules.generated." + parts[2]; // Add package prefix
                }
            }
        }
        throw new RuntimeException("Could not extract class name from generated code");
    }
    
    /**
     * Information about a compiled rule
     */
    public static class CompiledRuleInfo {
        public final String ruleId;
        public final String originalDsl;
        public final String generatedJavaCode;
        public final String className;
        public final Class<?> ruleClass;
        public final long compilationTimeMs;
        public final long timestamp;
        
        public CompiledRuleInfo(String ruleId, String originalDsl, String generatedJavaCode,
                               String className, Class<?> ruleClass, long compilationTimeMs) {
            this.ruleId = ruleId;
            this.originalDsl = originalDsl;
            this.generatedJavaCode = generatedJavaCode;
            this.className = className;
            this.ruleClass = ruleClass;
            this.compilationTimeMs = compilationTimeMs;
            this.timestamp = System.currentTimeMillis();
        }
    }
    
    /**
     * Result of rule compilation
     */
    public static class CompilationResult {
        public final boolean success;
        public final String message;
        public final String ruleId;
        public final String className;
        public final long compilationTimeMs;
        
        private CompilationResult(boolean success, String message, String ruleId, 
                                String className, long compilationTimeMs) {
            this.success = success;
            this.message = message;
            this.ruleId = ruleId;
            this.className = className;
            this.compilationTimeMs = compilationTimeMs;
        }
        
        public static CompilationResult success(String ruleId, String className, long compilationTimeMs) {
            return new CompilationResult(true, "Compilation successful", ruleId, className, compilationTimeMs);
        }
        
        public static CompilationResult failure(String message) {
            return new CompilationResult(false, message, null, null, 0);
        }
    }
    
    /**
     * Result of rule execution
     */
    public static class ExecutionResult {
        public final boolean success;
        public final String message;
        public final String ruleId;
        public final RuleResult ruleResult;
        public final long executionTimeMs;
        
        private ExecutionResult(boolean success, String message, String ruleId, 
                              RuleResult ruleResult, long executionTimeMs) {
            this.success = success;
            this.message = message;
            this.ruleId = ruleId;
            this.ruleResult = ruleResult;
            this.executionTimeMs = executionTimeMs;
        }
        
        public static ExecutionResult success(String ruleId, RuleResult ruleResult, long executionTimeMs) {
            return new ExecutionResult(true, "Execution successful", ruleId, ruleResult, executionTimeMs);
        }
        
        public static ExecutionResult failure(String message) {
            return new ExecutionResult(false, message, null, null, 0);
        }
        
        public static ExecutionResult failure(String message, long executionTimeMs) {
            return new ExecutionResult(false, message, null, null, executionTimeMs);
        }
    }
    
    /**
     * Engine performance statistics
     */
    public static class EngineStats {
        public final int compiledRulesCount;
        public final long totalExecutions;
        public final long totalExecutionTimeMs;
        public final HotRuleClassLoader.ClassLoaderStats classLoaderStats;
        public final RuleCompiler.CompilerStats compilerStats;
        
        public EngineStats(int compiledRulesCount, long totalExecutions, long totalExecutionTimeMs,
                          HotRuleClassLoader.ClassLoaderStats classLoaderStats,
                          RuleCompiler.CompilerStats compilerStats) {
            this.compiledRulesCount = compiledRulesCount;
            this.totalExecutions = totalExecutions;
            this.totalExecutionTimeMs = totalExecutionTimeMs;
            this.classLoaderStats = classLoaderStats;
            this.compilerStats = compilerStats;
        }
        
        public double getAverageExecutionTimeMs() {
            return totalExecutions > 0 ? (double) totalExecutionTimeMs / totalExecutions : 0.0;
        }
    }
}