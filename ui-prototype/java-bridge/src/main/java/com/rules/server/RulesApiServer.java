package com.rules.server;

import com.rules.cli.RuleTester;
import com.rules.grammar.RulesLexer;
import com.rules.grammar.RulesParser;
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
    
    public static void main(String[] args) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(PORT), 0);
        
        // Rule testing endpoint
        server.createContext("/api/rules/test", new RuleTestHandler());
        
        // Rule validation endpoint  
        server.createContext("/api/rules/validate", new RuleValidationHandler());
        
        // Health check
        server.createContext("/api/health", new HealthCheckHandler());
        
        // Set executor and start
        server.setExecutor(null);
        server.start();
        
        System.out.println("Rules API Server started on port " + PORT);
        System.out.println("Endpoints:");
        System.out.println("  POST /api/rules/test - Test rule execution");
        System.out.println("  POST /api/rules/validate - Validate rule syntax");
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