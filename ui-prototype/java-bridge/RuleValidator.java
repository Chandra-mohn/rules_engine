package com.rules.cli;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Command-line rule validator for the UI prototype.
 * This is a simplified version that provides basic validation feedback.
 */
public class RuleValidator {
    
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Usage: java RuleValidator <rule-file>");
            System.exit(1);
        }
        
        String ruleFile = args[0];
        
        try {
            // Read rule content
            String content = Files.readString(Paths.get(ruleFile));
            
            // Perform basic validation
            ValidationResult result = validateRule(content);
            
            if (result.isValid()) {
                System.out.println("{\"valid\": true, \"message\": \"" + result.getMessage() + "\"}");
                System.exit(0);
            } else {
                System.err.println("Validation failed: " + result.getMessage());
                System.exit(1);
            }
            
        } catch (IOException e) {
            System.err.println("Error reading rule file: " + e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.err.println("Validation error: " + e.getMessage());
            System.exit(1);
        }
    }
    
    private static ValidationResult validateRule(String content) {
        // Basic validation logic
        content = content.trim();
        
        if (content.isEmpty()) {
            return new ValidationResult(false, "Rule content cannot be empty");
        }
        
        // Check for basic rule structure
        if (!content.contains("rule ")) {
            return new ValidationResult(false, "Rule must start with 'rule' keyword");
        }
        
        if (!content.contains(":")) {
            return new ValidationResult(false, "Rule must have a colon after the rule name");
        }
        
        if (!content.contains("if ")) {
            return new ValidationResult(false, "Rule must contain at least one 'if' condition");
        }
        
        if (!content.contains(" then ")) {
            return new ValidationResult(false, "Rule must contain 'then' clause");
        }
        
        // Check for balanced parentheses
        int parenCount = 0;
        for (char c : content.toCharArray()) {
            if (c == '(') parenCount++;
            else if (c == ')') parenCount--;
            if (parenCount < 0) {
                return new ValidationResult(false, "Unmatched closing parenthesis");
            }
        }
        if (parenCount != 0) {
            return new ValidationResult(false, "Unmatched opening parenthesis");
        }
        
        // Basic syntax checks passed
        return new ValidationResult(true, "Rule syntax is valid");
    }
    
    private static class ValidationResult {
        private final boolean valid;
        private final String message;
        
        public ValidationResult(boolean valid, String message) {
            this.valid = valid;
            this.message = message;
        }
        
        public boolean isValid() {
            return valid;
        }
        
        public String getMessage() {
            return message;
        }
    }
}