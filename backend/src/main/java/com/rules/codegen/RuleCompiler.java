package com.rules.codegen;

import com.rules.ast.RuleSetNode;
import com.rules.engine.Rule;

import javax.tools.*;
import java.io.*;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.*;

/**
 * Compiles generated Java code into executable Rule classes.
 * Handles in-memory compilation and class loading.
 */
public class RuleCompiler {
    
    private final JavaCompiler compiler;
    private final StandardJavaFileManager fileManager;
    private final Path generatedSourcesDirectory;
    private final Path compiledClassesDirectory;
    private final Map<String, Class<? extends Rule>> compiledRules = new HashMap<>();
    
    public RuleCompiler() throws IOException {
        this.compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new IllegalStateException("Java compiler not available. Make sure you're running on JDK, not JRE.");
        }
        
        this.fileManager = compiler.getStandardFileManager(null, null, null);
        
        // Use project directories instead of temp directories
        Path projectRoot = findProjectRoot();
        this.generatedSourcesDirectory = projectRoot.resolve("backend/src/main/java/com/rules/generated");
        this.compiledClassesDirectory = projectRoot.resolve("backend/target/classes");
        
        // Ensure directories exist
        Files.createDirectories(generatedSourcesDirectory);
        Files.createDirectories(compiledClassesDirectory);
        
        // Set up classpath
        setupClasspath();
    }
    
    /**
     * Find the project root directory by looking for pom.xml
     */
    private Path findProjectRoot() {
        Path current = Paths.get("").toAbsolutePath();
        while (current != null) {
            if (Files.exists(current.resolve("backend/pom.xml"))) {
                return current;
            }
            if (Files.exists(current.resolve("pom.xml"))) {
                return current.getParent() != null ? current.getParent() : current;
            }
            current = current.getParent();
        }
        // Fallback to current directory
        return Paths.get("").toAbsolutePath();
    }
    
    /**
     * Compile rules from AST to executable classes.
     * 
     * @param ruleSet The rule set AST
     * @return Map of rule name to compiled Rule class
     * @throws CompilationException if compilation fails
     */
    public Map<String, Class<? extends Rule>> compileRules(RuleSetNode ruleSet) throws CompilationException {
        try {
            // Generate Java code for each rule
            JavaCodeGenerator generator = new JavaCodeGenerator();
            Map<String, String> generatedCode = new HashMap<>();
            
            for (var rule : ruleSet.getRules()) {
                String className = toClassName(rule.getRuleName());
                
                // Create a single-rule RuleSetNode for code generation
                RuleSetNode singleRuleSet = new RuleSetNode();
                singleRuleSet.addRule(rule);
                
                // Generate code for this rule
                String code = generator.generateCode(singleRuleSet);
                generatedCode.put(className, code);
            }
            
            // Save generated source files to project directory
            saveGeneratedSourceFiles(generatedCode);
            
            // Compile generated code
            Map<String, Class<? extends Rule>> compiled = compileJavaCode(generatedCode);
            
            // Cache compiled rules
            compiledRules.putAll(compiled);
            
            return compiled;
            
        } catch (Exception e) {
            throw new CompilationException("Failed to compile rules", e);
        }
    }
    
    /**
     * Save generated Java source files to the project directory for version control.
     */
    private void saveGeneratedSourceFiles(Map<String, String> generatedCode) throws IOException {
        for (Map.Entry<String, String> entry : generatedCode.entrySet()) {
            String className = entry.getKey();
            String code = entry.getValue();
            
            Path sourceFile = generatedSourcesDirectory.resolve(className + ".java");
            Files.write(sourceFile, code.getBytes(), 
                       StandardOpenOption.CREATE, 
                       StandardOpenOption.TRUNCATE_EXISTING);
            
            System.out.println("Generated rule source: " + sourceFile.toAbsolutePath());
        }
    }
    
    /**
     * Get a compiled rule by name.
     * 
     * @param ruleName Name of the rule
     * @return Compiled Rule class, or null if not found
     */
    public Class<? extends Rule> getCompiledRule(String ruleName) {
        return compiledRules.get(toClassName(ruleName));
    }
    
    /**
     * Get all compiled rules.
     * 
     * @return Map of rule name to compiled Rule class
     */
    public Map<String, Class<? extends Rule>> getAllCompiledRules() {
        return new HashMap<>(compiledRules);
    }
    
    /**
     * Clear compiled rules cache.
     */
    public void clearCache() {
        compiledRules.clear();
    }
    

    
    @SuppressWarnings("unchecked")
    private Map<String, Class<? extends Rule>> compileJavaCode(Map<String, String> sourceCode) 
            throws IOException, ClassNotFoundException {
        
        // Create source files
        List<JavaFileObject> sourceFiles = new ArrayList<>();
        for (Map.Entry<String, String> entry : sourceCode.entrySet()) {
            String className = entry.getKey();
            String code = entry.getValue();
            
            JavaFileObject sourceFile = new StringJavaFileObject(
                "com.rules.generated." + className, code);
            sourceFiles.add(sourceFile);
        }
        
        // Set up compilation options
        List<String> options = Arrays.asList(
            "-d", compiledClassesDirectory.toString(),
            "-cp", System.getProperty("java.class.path")
        );
        
        // Compile
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
        JavaCompiler.CompilationTask task = compiler.getTask(
            null, fileManager, diagnostics, options, null, sourceFiles);
        
        boolean success = task.call();
        
        if (!success) {
            StringBuilder errors = new StringBuilder();
            for (Diagnostic<? extends JavaFileObject> diagnostic : diagnostics.getDiagnostics()) {
                errors.append(diagnostic.toString()).append("\n");
            }
            throw new RuntimeException("Compilation failed:\n" + errors.toString());
        }
        
        // Load compiled classes
        URL[] urls = {compiledClassesDirectory.toUri().toURL()};
        URLClassLoader classLoader = new URLClassLoader(urls, getClass().getClassLoader());
        
        Map<String, Class<? extends Rule>> compiledClasses = new HashMap<>();
        for (String className : sourceCode.keySet()) {
            String fullClassName = "com.rules.generated." + className;
            Class<?> clazz = classLoader.loadClass(fullClassName);
            compiledClasses.put(className, (Class<? extends Rule>) clazz);
        }
        
        return compiledClasses;
    }
    
    private void setupClasspath() throws IOException {
        // Add current classpath to file manager
        List<File> classpath = new ArrayList<>();
        
        // Add current classpath
        String cp = System.getProperty("java.class.path");
        for (String path : cp.split(File.pathSeparator)) {
            classpath.add(new File(path));
        }
        
        fileManager.setLocation(StandardLocation.CLASS_PATH, classpath);
        fileManager.setLocation(StandardLocation.CLASS_OUTPUT, Arrays.asList(compiledClassesDirectory.toFile()));
    }
    
    private String toClassName(String ruleName) {
        StringBuilder sb = new StringBuilder();
        boolean capitalizeNext = true;
        
        for (char c : ruleName.toCharArray()) {
            if (Character.isLetterOrDigit(c)) {
                if (capitalizeNext) {
                    sb.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    sb.append(c);
                }
            } else {
                capitalizeNext = true;
            }
        }
        
        sb.append("Rule");
        return sb.toString();
    }
    
    /**
     * Clean up resources (no longer needed since we use project directories).
     */
    public void cleanup() {
        // No cleanup needed - generated files are kept in project directory
        // for version control and production deployment
        try {
            fileManager.close();
        } catch (IOException e) {
            System.err.println("Warning: Failed to close file manager: " + e.getMessage());
        }
    }
    
    /**
     * In-memory Java source file implementation.
     */
    private static class StringJavaFileObject extends SimpleJavaFileObject {
        private final String code;
        
        public StringJavaFileObject(String className, String code) {
            super(URI.create("string:///" + className.replace('.', '/') + Kind.SOURCE.extension),
                  Kind.SOURCE);
            this.code = code;
        }
        
        @Override
        public CharSequence getCharContent(boolean ignoreEncodingErrors) {
            return code;
        }
    }
    
    /**
     * Exception thrown when compilation fails.
     */
    public static class CompilationException extends Exception {
        public CompilationException(String message) {
            super(message);
        }
        
        public CompilationException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}