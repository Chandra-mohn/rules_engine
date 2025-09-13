package com.rules.server;

import com.rules.cli.RuleTester;
import com.rules.codegen.DirectJavaCodeGenerator;
import com.rules.context.RuleContext;
import com.rules.grammar.RulesLexer;
import com.rules.grammar.RulesParser;
import com.rules.runtime.RuleExecutionEngine;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * Simple HTTP server for rule testing and validation.
 * Provides REST API endpoints for the existing RuleTester functionality.
 */
public class RulesApiServer {
    
    private static final int PORT = 8081; // Different from Python backend (5001)
    private static final RuleExecutionEngine ruleEngine = new RuleExecutionEngine();
    
    public static void main(String[] args) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(PORT), 0);
        
        // Rule testing endpoint
        server.createContext("/api/rules/test", new RuleTestHandler());
        
        // Rule validation endpoint  
        server.createContext("/api/rules/validate", new RuleValidationHandler());
        
        // Code generation endpoints
        server.createContext("/api/rules/generate", new CodeGenerationHandler());
        server.createContext("/api/rules/compile", new RuleCompilationHandler());
        
        // Hot compilation and execution endpoints
        server.createContext("/api/rules/execute", new RuleExecutionHandler());
        server.createContext("/api/engine/stats", new EngineStatsHandler());
        
        // Health check
        server.createContext("/api/health", new HealthCheckHandler());
        
        // Set executor and start
        server.setExecutor(null);
        server.start();
        
        System.out.println("Rules API Server started on port " + PORT);
        System.out.println("Endpoints:");
        System.out.println("  POST /api/rules/test - Test rule execution");
        System.out.println("  POST /api/rules/validate - Validate rule syntax");
        System.out.println("  POST /api/rules/generate - Generate Java code from rule DSL");
        System.out.println("  POST /api/rules/compile - Compile rule to bytecode and load");
        System.out.println("  POST /api/rules/execute - Execute compiled rule with data");
        System.out.println("  GET  /api/engine/stats - Engine performance statistics");
        System.out.println("  GET  /api/health - Health check");
    }
    
    /**
     * Handler for rule testing - executes rules against test data
     */
    static class RuleTestHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\": \"Method not allowed\"}");
                return;
            }
            
            try {
                // Enable CORS
                setCorsHeaders(exchange);
                
                // Read request body
                String requestBody = readRequestBody(exchange);
                JSONObject request = new JSONObject(requestBody);
                
                // Extract rule content and test data
                String ruleContent = request.getString("ruleContent");
                JSONObject testData = request.getJSONObject("testData");
                
                // Execute rule using existing RuleTester
                RuleTester.TestResult result = RuleTester.executeRule(ruleContent, testData);
                
                // Convert result to JSON and send response
                JSONObject jsonResult = RuleTester.testResultToJson(result);
                sendResponse(exchange, 200, jsonResult.toString());
                
            } catch (Exception e) {
                String errorResponse = String.format(
                    "{\"error\": \"Rule execution failed\", \"message\": \"%s\"}", 
                    e.getMessage().replace("\"", "\\\"")
                );
                sendResponse(exchange, 500, errorResponse);
            }
        }
    }
    
    /**
     * Handler for rule validation - checks syntax without execution
     */
    static class RuleValidationHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\": \"Method not allowed\"}");
                return;
            }
            
            try {
                setCorsHeaders(exchange);
                
                String requestBody = readRequestBody(exchange);
                JSONObject request = new JSONObject(requestBody);
                String ruleContent = request.getString("ruleContent");
                
                // Use full ANTLR parsing for validation
                ValidationResult validation = validateRuleWithANTLR(ruleContent);
                
                JSONObject response = new JSONObject();
                response.put("valid", validation.valid);
                response.put("message", validation.message);
                if (!validation.errors.isEmpty()) {
                    response.put("errors", validation.errors);
                }
                
                sendResponse(exchange, 200, response.toString());
                
            } catch (Exception e) {
                String errorResponse = String.format(
                    "{\"valid\": false, \"message\": \"Validation failed: %s\"}", 
                    e.getMessage().replace("\"", "\\\"")
                );
                sendResponse(exchange, 500, errorResponse);
            }
        }
    }
    
    /**
     * Health check handler
     */
    static class HealthCheckHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            setCorsHeaders(exchange);
            
            JSONObject health = new JSONObject();
            health.put("status", "healthy");
            health.put("service", "Rules API Server");
            health.put("version", "1.0.0");
            
            sendResponse(exchange, 200, health.toString());
        }
    }
    
    /**
     * Set CORS headers for cross-origin requests
     */
    private static void setCorsHeaders(HttpExchange exchange) {
        exchange.getResponseHeaders().add("Access-Control-Allow-Origin", "*");
        exchange.getResponseHeaders().add("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
        exchange.getResponseHeaders().add("Access-Control-Allow-Headers", "Content-Type, Authorization");
    }
    
    /**
     * Read request body as string
     */
    private static String readRequestBody(HttpExchange exchange) throws IOException {
        InputStream inputStream = exchange.getRequestBody();
        return new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
    }
    
    /**
     * Send HTTP response
     */
    private static void sendResponse(HttpExchange exchange, int statusCode, String response) throws IOException {
        exchange.getResponseHeaders().add("Content-Type", "application/json");
        byte[] responseBytes = response.getBytes(StandardCharsets.UTF_8);
        exchange.sendResponseHeaders(statusCode, responseBytes.length);
        
        try (OutputStream outputStream = exchange.getResponseBody()) {
            outputStream.write(responseBytes);
        }
    }
    
    /**
     * Validate rule syntax using full ANTLR parsing
     */
    private static ValidationResult validateRuleWithANTLR(String ruleContent) {
        if (ruleContent == null || ruleContent.trim().isEmpty()) {
            return new ValidationResult(false, "Rule content is empty", List.of("Rule content cannot be empty"));
        }
        
        try {
            // Create ANTLR input stream (using non-deprecated method)
            CharStream input = CharStreams.fromString(ruleContent);
            
            // Create lexer
            RulesLexer lexer = new RulesLexer(input);
            
            // Create token stream
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            
            // Create parser
            RulesParser parser = new RulesParser(tokens);
            
            // Custom error listener to collect errors
            List<String> errors = new ArrayList<>();
            parser.removeErrorListeners(); // Remove default console error listener
            parser.addErrorListener(new BaseErrorListener() {
                @Override
                public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                                      int line, int charPositionInLine, String msg, RecognitionException e) {
                    errors.add(String.format("Line %d:%d - %s", line, charPositionInLine, msg));
                }
            });
            
            // Parse the rule - this will trigger syntax validation
            ParseTree tree = parser.ruleSet();
            
            if (errors.isEmpty()) {
                return new ValidationResult(true, "Rule syntax is valid", errors);
            } else {
                return new ValidationResult(false, "Rule has syntax errors", errors);
            }
            
        } catch (Exception e) {
            return new ValidationResult(false, "Validation failed: " + e.getMessage(), 
                                      List.of("Exception during validation: " + e.getMessage()));
        }
    }
    
    /**
     * Handler for code generation - converts rule DSL to Java source
     */
    static class CodeGenerationHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\": \"Method not allowed\"}");
                return;
            }
            
            try {
                setCorsHeaders(exchange);
                
                String requestBody = readRequestBody(exchange);
                JSONObject request = new JSONObject(requestBody);
                String ruleContent = request.getString("ruleContent");
                
                // Parse rule with ANTLR
                CharStream input = CharStreams.fromString(ruleContent);
                RulesLexer lexer = new RulesLexer(input);
                CommonTokenStream tokens = new CommonTokenStream(lexer);
                RulesParser parser = new RulesParser(tokens);
                RulesParser.RuleSetContext parseTree = parser.ruleSet();
                
                // Generate Java code
                DirectJavaCodeGenerator generator = new DirectJavaCodeGenerator();
                String javaCode = generator.generateCode(parseTree);
                
                JSONObject response = new JSONObject();
                response.put("success", true);
                response.put("javaCode", javaCode);
                response.put("message", "Code generated successfully");
                
                sendResponse(exchange, 200, response.toString());
                
            } catch (Exception e) {
                JSONObject error = new JSONObject();
                error.put("success", false);
                error.put("error", "Code generation failed");
                error.put("message", e.getMessage());
                sendResponse(exchange, 500, error.toString());
            }
        }
    }
    
    /**
     * Handler for rule compilation - compiles rule DSL to bytecode and loads for execution
     */
    static class RuleCompilationHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\": \"Method not allowed\"}");
                return;
            }
            
            try {
                setCorsHeaders(exchange);
                System.out.println("DEBUG: Starting compilation request");
                
                String requestBody = readRequestBody(exchange);
                System.out.println("DEBUG: Request body: " + requestBody);
                JSONObject request = new JSONObject(requestBody);
                String ruleContent = request.getString("ruleContent");
                String ruleId = request.optString("ruleId", "rule_" + System.currentTimeMillis());
                System.out.println("DEBUG: Parsed ruleId=" + ruleId + ", ruleContent=" + ruleContent);
                
                // Compile the rule using the execution engine
                System.out.println("DEBUG: About to call ruleEngine.compileRule");
                RuleExecutionEngine.CompilationResult result = ruleEngine.compileRule(ruleId, ruleContent);
                System.out.println("DEBUG: CompileRule returned: " + result);
                
                JSONObject response = new JSONObject();
                if (result.success) {
                    response.put("success", true);
                    response.put("ruleId", result.ruleId);
                    response.put("className", result.className);
                    response.put("compilationTimeMs", result.compilationTimeMs);
                    response.put("message", "Rule compiled and loaded successfully");
                    response.put("ready", true);
                    sendResponse(exchange, 200, response.toString());
                } else {
                    response.put("success", false);
                    response.put("error", "Compilation failed");
                    response.put("message", result.message);
                    sendResponse(exchange, 500, response.toString());
                }
                
            } catch (Exception e) {
                e.printStackTrace(); // Log the full stack trace
                System.err.println("Compilation error: " + e.getMessage());
                
                JSONObject error = new JSONObject();
                error.put("success", false);
                error.put("error", "Rule compilation failed");
                error.put("message", e.getMessage());
                sendResponse(exchange, 500, error.toString());
            }
        }
    }

    /**
     * Handler for rule execution - executes compiled rules with data
     */
    static class RuleExecutionHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                sendResponse(exchange, 405, "{\"error\": \"Method not allowed\"}");
                return;
            }
            
            try {
                setCorsHeaders(exchange);
                
                String requestBody = readRequestBody(exchange);
                JSONObject request = new JSONObject(requestBody);
                String ruleId = request.getString("ruleId");
                JSONObject contextData = request.getJSONObject("contextData");
                
                // Create execution context
                RuleContext context = new RuleContext(contextData.toString());
                
                // Execute the rule
                RuleExecutionEngine.ExecutionResult result = ruleEngine.executeRule(ruleId, context);
                
                JSONObject response = new JSONObject();
                if (result.success) {
                    response.put("success", true);
                    response.put("ruleId", result.ruleId);
                    response.put("executionTimeMs", result.executionTimeMs);
                    
                    // Add rule result details
                    if (result.ruleResult != null) {
                        response.put("matched", result.ruleResult.wasExecuted());
                        response.put("hasActions", result.ruleResult.hasActions());
                        if (result.ruleResult.hasActions()) {
                            response.put("actions", result.ruleResult.getActions());
                            // Get first action as the final action for backward compatibility
                            if (!result.ruleResult.getActions().isEmpty()) {
                                response.put("finalAction", result.ruleResult.getActions().get(0));
                            }
                        }
                        if (result.ruleResult.hasError()) {
                            response.put("error", result.ruleResult.getError().getMessage());
                        }
                    }
                    
                    sendResponse(exchange, 200, response.toString());
                } else {
                    response.put("success", false);
                    response.put("error", "Execution failed");
                    response.put("message", result.message);
                    if (result.executionTimeMs > 0) {
                        response.put("executionTimeMs", result.executionTimeMs);
                    }
                    sendResponse(exchange, 500, response.toString());
                }
                
            } catch (Exception e) {
                JSONObject error = new JSONObject();
                error.put("success", false);
                error.put("error", "Rule execution failed");
                error.put("message", e.getMessage());
                sendResponse(exchange, 500, error.toString());
            }
        }
    }
    
    /**
     * Handler for engine statistics
     */
    static class EngineStatsHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            setCorsHeaders(exchange);
            
            try {
                RuleExecutionEngine.EngineStats stats = ruleEngine.getStats();
                
                JSONObject response = new JSONObject();
                response.put("compiledRulesCount", stats.compiledRulesCount);
                response.put("totalExecutions", stats.totalExecutions);
                response.put("totalExecutionTimeMs", stats.totalExecutionTimeMs);
                response.put("averageExecutionTimeMs", stats.getAverageExecutionTimeMs());
                
                // ClassLoader stats
                JSONObject classLoaderStats = new JSONObject();
                classLoaderStats.put("loadedClasses", stats.classLoaderStats.loadedClassCount);
                classLoaderStats.put("totalBytecodeSize", stats.classLoaderStats.totalBytecodeSize);
                classLoaderStats.put("highestVersion", stats.classLoaderStats.highestVersion);
                response.put("classLoaderStats", classLoaderStats);
                
                // Compiler stats
                JSONObject compilerStats = new JSONObject();
                compilerStats.put("compiledClassCount", stats.compilerStats.compiledClassCount);
                compilerStats.put("totalCompilationTimeMs", stats.compilerStats.totalCompilationTimeMs);
                compilerStats.put("averageCompilationTimeMs", stats.compilerStats.getAverageCompilationTimeMs());
                response.put("compilerStats", compilerStats);
                
                sendResponse(exchange, 200, response.toString());
                
            } catch (Exception e) {
                JSONObject error = new JSONObject();
                error.put("error", "Failed to get engine stats");
                error.put("message", e.getMessage());
                sendResponse(exchange, 500, error.toString());
            }
        }
    }

    /**
     * Result class for validation operations
     */
    static class ValidationResult {
        public final boolean valid;
        public final String message;
        public final List<String> errors;
        
        public ValidationResult(boolean valid, String message, List<String> errors) {
            this.valid = valid;
            this.message = message;
            this.errors = errors;
        }
    }
}