package com.rules.runtime;

import javax.tools.*;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * In-memory Java compiler for rule classes.
 * Compiles Java source code to bytecode without file system writes.
 */
public class RuleCompiler {
    
    private final JavaCompiler compiler;
    private final StandardJavaFileManager standardFileManager;
    private final Map<String, CompiledClass> compiledClasses = new ConcurrentHashMap<>();
    
    public RuleCompiler() {
        this.compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new RuntimeException("Java Compiler not available. Make sure you're running with JDK, not JRE.");
        }
        
        this.standardFileManager = compiler.getStandardFileManager(null, null, null);
    }
    
    /**
     * Compile Java source code to bytecode
     * @param className Fully qualified class name
     * @param javaSource Java source code
     * @return CompiledClass containing bytecode and metadata
     * @throws CompilationException If compilation fails
     */
    public CompiledClass compileRule(String className, String javaSource) throws CompilationException {
        long startTime = System.currentTimeMillis();
        
        // Create in-memory file manager
        InMemoryFileManager fileManager = new InMemoryFileManager(standardFileManager);
        
        // Create source file object
        JavaFileObject sourceFile = new InMemoryJavaFileObject(className, javaSource);
        
        // Build comprehensive classpath
        StringBuilder classPath = new StringBuilder();
        
        // Add system classpath
        String systemClassPath = System.getProperty("java.class.path");
        if (systemClassPath != null) {
            classPath.append(systemClassPath);
        }
        
        // Add target/classes directory (where our compiled classes are)
        String userDir = System.getProperty("user.dir");
        String targetClasses = userDir + "/target/classes";
        if (classPath.length() > 0) {
            classPath.append(System.getProperty("path.separator"));
        }
        classPath.append(targetClasses);
        
        // Set up compilation task
        List<String> options = Arrays.asList(
            "-cp", classPath.toString(),
            "-target", "17",
            "-source", "17"
        );
        
        // Collect diagnostic information
        List<String> errors = new ArrayList<>();
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
        
        JavaCompiler.CompilationTask task = compiler.getTask(
            null,                          // Writer for output
            fileManager,                   // File manager
            diagnostics,                  // Diagnostic listener to collect errors
            options,                      // Compiler options
            null,                         // Classes to process for annotation processing
            Collections.singletonList(sourceFile)  // Source files
        );
        
        // Compile
        boolean success = task.call();
        
        if (!success) {
            StringBuilder errorMsg = new StringBuilder("Compilation failed for class: " + className);
            for (Diagnostic<? extends JavaFileObject> diagnostic : diagnostics.getDiagnostics()) {
                if (diagnostic.getKind() == Diagnostic.Kind.ERROR) {
                    errorMsg.append("\n  Line ").append(diagnostic.getLineNumber())
                           .append(": ").append(diagnostic.getMessage(null));
                }
            }
            throw new CompilationException(errorMsg.toString());
        }
        
        // Get compiled bytecode
        InMemoryJavaFileObject compiledClass = fileManager.getCompiledClass(className);
        if (compiledClass == null) {
            throw new CompilationException("No bytecode generated for class: " + className);
        }
        
        byte[] bytecode = compiledClass.getBytecode();
        long compilationTime = System.currentTimeMillis() - startTime;
        
        CompiledClass result = new CompiledClass(className, javaSource, bytecode, compilationTime);
        compiledClasses.put(className, result);
        
        return result;
    }
    
    /**
     * Get a previously compiled class
     * @param className Fully qualified class name
     * @return CompiledClass if found, null otherwise
     */
    public CompiledClass getCompiledClass(String className) {
        return compiledClasses.get(className);
    }
    
    /**
     * Check if a class has been compiled
     * @param className Fully qualified class name
     * @return true if compiled, false otherwise
     */
    public boolean isCompiled(String className) {
        return compiledClasses.containsKey(className);
    }
    
    /**
     * Get compilation statistics
     * @return CompilerStats object
     */
    public CompilerStats getStats() {
        return new CompilerStats(
            compiledClasses.size(),
            compiledClasses.values().stream()
                .mapToLong(cc -> cc.compilationTimeMs)
                .sum()
        );
    }
    
    /**
     * Represents a compiled rule class
     */
    public static class CompiledClass {
        public final String className;
        public final String sourceCode;
        public final byte[] bytecode;
        public final long compilationTimeMs;
        public final long timestamp;
        
        public CompiledClass(String className, String sourceCode, byte[] bytecode, long compilationTimeMs) {
            this.className = className;
            this.sourceCode = sourceCode;
            this.bytecode = bytecode.clone();
            this.compilationTimeMs = compilationTimeMs;
            this.timestamp = System.currentTimeMillis();
        }
        
        public int getBytecodeSize() {
            return bytecode.length;
        }
    }
    
    /**
     * Compiler statistics
     */
    public static class CompilerStats {
        public final int compiledClassCount;
        public final long totalCompilationTimeMs;
        
        public CompilerStats(int compiledClassCount, long totalCompilationTimeMs) {
            this.compiledClassCount = compiledClassCount;
            this.totalCompilationTimeMs = totalCompilationTimeMs;
        }
        
        public double getAverageCompilationTimeMs() {
            return compiledClassCount > 0 ? (double) totalCompilationTimeMs / compiledClassCount : 0.0;
        }
    }
    
    /**
     * Exception thrown when compilation fails
     */
    public static class CompilationException extends Exception {
        public CompilationException(String message) {
            super(message);
        }
        
        public CompilationException(String message, Throwable cause) {
            super(message, cause);
        }
    }
    
    /**
     * In-memory file manager for compilation
     */
    private static class InMemoryFileManager extends ForwardingJavaFileManager<StandardJavaFileManager> {
        private final Map<String, InMemoryJavaFileObject> compiledClasses = new ConcurrentHashMap<>();
        
        protected InMemoryFileManager(StandardJavaFileManager fileManager) {
            super(fileManager);
        }
        
        @Override
        public JavaFileObject getJavaFileForOutput(Location location, String className,
                                                  JavaFileObject.Kind kind, FileObject sibling) throws IOException {
            InMemoryJavaFileObject fileObject = new InMemoryJavaFileObject(className);
            compiledClasses.put(className, fileObject);
            return fileObject;
        }
        
        public InMemoryJavaFileObject getCompiledClass(String className) {
            return compiledClasses.get(className);
        }
    }
    
    /**
     * In-memory representation of a Java file
     */
    private static class InMemoryJavaFileObject extends SimpleJavaFileObject {
        private final String sourceCode;
        private ByteArrayOutputStream bytecode;
        
        // Constructor for source files
        public InMemoryJavaFileObject(String className, String sourceCode) {
            super(URI.create("string:///" + className.replace('.', '/') + Kind.SOURCE.extension), 
                  Kind.SOURCE);
            this.sourceCode = sourceCode;
        }
        
        // Constructor for compiled class files
        public InMemoryJavaFileObject(String className) {
            super(URI.create("mem:///" + className.replace('.', '/') + Kind.CLASS.extension), 
                  Kind.CLASS);
            this.sourceCode = null;
        }
        
        @Override
        public CharSequence getCharContent(boolean ignoreEncodingErrors) {
            return sourceCode;
        }
        
        @Override
        public OutputStream openOutputStream() throws IOException {
            bytecode = new ByteArrayOutputStream();
            return bytecode;
        }
        
        public byte[] getBytecode() {
            return bytecode != null ? bytecode.toByteArray() : null;
        }
    }
}