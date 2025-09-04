package com.rules.cli;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Simple rule tester that simulates rule execution with test data.
 * This is a prototype implementation for demonstration purposes.
 */
public class RuleTester {
    
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: java RuleTester <rule-file> <test-data-file>");
            System.exit(1);
        }
        
        String ruleFile = args[0];
        String testDataFile = args[1];
        
        try {
            // Read rule content and test data
            String ruleContent = Files.readString(Paths.get(ruleFile));
            String testDataJson = Files.readString(Paths.get(testDataFile));
            
            // Execute rule
            TestResult result = executeRule(ruleContent, testDataJson);
            
            // Output result as JSON
            System.out.println(result.toJsonString());
            System.exit(0);
            
        } catch (IOException e) {
            System.err.println("Error reading files: " + e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.err.println("Rule execution error: " + e.getMessage());
            System.exit(1);
        }
    }
    
    private static TestResult executeRule(String ruleContent, String testDataJson) {
        TestResult result = new TestResult();
        result.success = true;
        result.timestamp = new java.util.Date().toString();
        
        try {
            // Extract rule name (handle multiline)
            Pattern ruleNamePattern = Pattern.compile("rule\\s+(\\w+):", Pattern.MULTILINE | Pattern.DOTALL);
            Matcher ruleNameMatcher = ruleNamePattern.matcher(ruleContent);
            if (ruleNameMatcher.find()) {
                result.ruleName = ruleNameMatcher.group(1);
            } else {
                result.ruleName = "Unnamed Rule";
            }
            
            // Extract and evaluate if-then statements (handle multiline)
            Pattern ifThenPattern = Pattern.compile("if\\s+(.+?)\\s+then\\s+(\\w+)", Pattern.MULTILINE | Pattern.DOTALL);
            Matcher ifThenMatcher = ifThenPattern.matcher(ruleContent);
            
            StringBuilder conditionsJson = new StringBuilder();
            conditionsJson.append("[");
            StringBuilder actionsJson = new StringBuilder();
            actionsJson.append("[");
            
            List<String> executedActions = new ArrayList<>();
            List<String> allAvailableActions = new ArrayList<>();
            boolean firstCondition = true;
            boolean firstAction = true;
            
            while (ifThenMatcher.find()) {
                String condition = ifThenMatcher.group(1).trim();
                String action = ifThenMatcher.group(2).trim();
                
                // Track all possible actions in the rule
                if (!allAvailableActions.contains(action)) {
                    allAvailableActions.add(action);
                }
                
                // Evaluate condition (simplified)
                boolean conditionMet = evaluateCondition(condition, testDataJson);
                
                // Build conditions JSON
                if (!firstCondition) {
                    conditionsJson.append(",");
                }
                
                conditionsJson.append("{");
                conditionsJson.append("\"condition\":\"").append(escapeJson(condition)).append("\",");
                conditionsJson.append("\"action\":\"").append(action).append("\",");
                conditionsJson.append("\"evaluated\":").append(conditionMet).append(",");
                conditionsJson.append("\"executed\":").append(conditionMet);
                conditionsJson.append("}");
                
                firstCondition = false;
                
                // If condition is met, add to executed actions
                if (conditionMet) {
                    executedActions.add(action);
                    
                    // Build executed actions JSON
                    if (!firstAction) {
                        actionsJson.append(",");
                    }
                    
                    actionsJson.append("{");
                    actionsJson.append("\"action\":\"").append(action).append("\",");
                    actionsJson.append("\"reason\":\"").append(escapeJson(condition)).append("\",");
                    actionsJson.append("\"description\":\"").append(getActionDescription(action)).append("\"");
                    actionsJson.append("}");
                    
                    firstAction = false;
                }
            }
            
            conditionsJson.append("]");
            actionsJson.append("]");
            
            result.conditionsJson = conditionsJson.toString();
            result.actionsJson = actionsJson.toString();
            result.executedActionsCount = executedActions.size();
            result.totalAvailableActions = allAvailableActions.size();
            
            // Set primary action (first executed action for backward compatibility)
            result.finalAction = executedActions.isEmpty() ? null : executedActions.get(0);
            result.actionExecuted = !executedActions.isEmpty();
            
            // Set comprehensive message
            if (!executedActions.isEmpty()) {
                if (executedActions.size() == 1) {
                    result.message = "✅ Action to Execute: " + executedActions.get(0);
                } else {
                    result.message = "✅ " + executedActions.size() + " Actions to Execute: " + 
                                    String.join(", ", executedActions);
                }
            } else {
                result.message = "ℹ️ No actions will be executed - no conditions were met";
            }
            
        } catch (Exception e) {
            result.success = false;
            result.message = "Rule execution failed: " + e.getMessage();
        }
        
        return result;
    }
    
    private static boolean evaluateCondition(String condition, String testDataJson) {
        try {
            // Simple condition evaluation for prototype
            // This evaluates basic numeric comparisons with applicant data
            
            if (condition.contains("applicant.creditScore")) {
                return evaluateNumericCondition(condition, "creditScore", testDataJson);
            } else if (condition.contains("applicant.age")) {
                return evaluateNumericCondition(condition, "age", testDataJson);
            } else if (condition.contains("applicant.annualIncome")) {
                return evaluateNumericCondition(condition, "annualIncome", testDataJson);
            } else if (condition.contains("applicant.applicationDate")) {
                // For date comparisons, return true for demo
                return true;
            } else if (condition.contains("transaction.amount")) {
                return evaluateNumericCondition(condition, "amount", testDataJson);
            }
            
            // Default: return true for demo purposes
            return true;
            
        } catch (Exception e) {
            return false;
        }
    }
    
    private static boolean evaluateNumericCondition(String condition, String property, String testDataJson) {
        try {
            // Extract value from JSON (simple string parsing)
            Pattern valuePattern = Pattern.compile("\"" + property + "\":\\s*(\\d+(?:\\.\\d+)?)");
            Matcher valueMatcher = valuePattern.matcher(testDataJson);
            
            if (!valueMatcher.find()) {
                return false;
            }
            
            double actualValue = Double.parseDouble(valueMatcher.group(1));
            
            // Parse condition to get operator and expected value
            String[] operators = {">=", "<=", ">", "<", "="};
            
            for (String op : operators) {
                if (condition.contains(op)) {
                    String[] parts = condition.split(Pattern.quote(op));
                    if (parts.length == 2) {
                        String rightSide = parts[1].trim();
                        
                        try {
                            double expectedValue = Double.parseDouble(rightSide);
                            
                            switch (op) {
                                case ">=": return actualValue >= expectedValue;
                                case "<=": return actualValue <= expectedValue;
                                case ">": return actualValue > expectedValue;
                                case "<": return actualValue < expectedValue;
                                case "=": return actualValue == expectedValue;
                            }
                        } catch (NumberFormatException e) {
                            // Right side is not a number
                        }
                    }
                    break;
                }
            }
            
        } catch (Exception e) {
            // Evaluation failed
        }
        
        return false;
    }
    
    private static String escapeJson(String str) {
        return str.replace("\"", "\\\"").replace("\\", "\\\\");
    }
    
    private static String getActionDescription(String action) {
        // Provide user-friendly descriptions for common actions
        switch (action) {
            case "approveApplication":
                return "Approve the credit card application";
            case "rejectApplication":
                return "Reject the credit card application";
            case "conditionalApproval":
                return "Approve with conditions or lower limit";
            case "instantApproval":
                return "Instant approval for qualified applicants";
            case "manualReview":
                return "Require manual review by underwriter";
            case "requireManualReview":
                return "Flag for manual review";
            case "approveTransaction":
                return "Approve the transaction";
            case "declineTransaction":
                return "Decline the transaction";
            case "flagForReview":
                return "Flag transaction for review";
            case "sendAlert":
                return "Send fraud alert";
            case "requestVerification":
                return "Request additional verification";
            case "setLimit":
                return "Set specific credit limit";
            default:
                return "Execute " + action + " action";
        }
    }
    
    private static class TestResult {
        public boolean success;
        public String message;
        public String ruleName;
        public String conditionsJson;
        public String actionsJson;
        public String finalAction;
        public boolean actionExecuted;
        public int executedActionsCount;
        public int totalAvailableActions;
        public String timestamp;
        
        public String toJsonString() {
            StringBuilder json = new StringBuilder();
            json.append("{");
            json.append("\"success\":").append(success).append(",");
            json.append("\"message\":\"").append(escapeJson(message != null ? message : "")).append("\",");
            json.append("\"ruleName\":\"").append(ruleName != null ? ruleName : "").append("\",");
            json.append("\"conditions\":").append(conditionsJson != null ? conditionsJson : "[]").append(",");
            json.append("\"executedActions\":").append(actionsJson != null ? actionsJson : "[]").append(",");
            json.append("\"finalAction\":\"").append(finalAction != null ? finalAction : "").append("\",");
            json.append("\"actionExecuted\":").append(actionExecuted).append(",");
            json.append("\"executedActionsCount\":").append(executedActionsCount).append(",");
            json.append("\"totalAvailableActions\":").append(totalAvailableActions).append(",");
            json.append("\"timestamp\":\"").append(escapeJson(timestamp != null ? timestamp : "")).append("\"");
            json.append("}");
            return json.toString();
        }
    }
}