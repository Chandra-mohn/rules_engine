package com.rules.cli;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Command-line rule tester that executes rules against sample data.
 */
public class RuleTester {
    
    private static final ObjectMapper mapper = new ObjectMapper();
    
    // Action descriptions for better user feedback
    private static final Map<String, String> ACTION_DESCRIPTIONS = new HashMap<>();
    static {
        ACTION_DESCRIPTIONS.put("approveApplication", "Approve the credit card application");
        ACTION_DESCRIPTIONS.put("rejectApplication", "Reject the credit card application");
        ACTION_DESCRIPTIONS.put("conditionalApproval", "Approve with conditions");
        ACTION_DESCRIPTIONS.put("instantApproval", "Instantly approve the application");
        ACTION_DESCRIPTIONS.put("manualReview", "Send for manual review");
        ACTION_DESCRIPTIONS.put("requireManualReview", "Require manual review before decision");
        ACTION_DESCRIPTIONS.put("approveTransaction", "Approve the transaction");
        ACTION_DESCRIPTIONS.put("declineTransaction", "Decline the transaction");
        ACTION_DESCRIPTIONS.put("flagForReview", "Flag transaction for review");
        ACTION_DESCRIPTIONS.put("sendAlert", "Send security alert");
        ACTION_DESCRIPTIONS.put("requestVerification", "Request additional verification");
        ACTION_DESCRIPTIONS.put("setLimit", "Set credit limit");
    }
    
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: java RuleTester <rule-file> <data-file>");
            System.exit(1);
        }
        
        String ruleFile = args[0];
        String dataFile = args[1];
        
        try {
            // Read rule and data files
            String ruleContent = Files.readString(Paths.get(ruleFile));
            String dataContent = Files.readString(Paths.get(dataFile));
            
            // Parse data JSON
            JsonNode testData = mapper.readTree(dataContent);
            
            // Execute rule
            TestResult result = executeRule(ruleContent, testData);
            
            // Output result as JSON
            String jsonResult = mapper.writeValueAsString(result);
            System.out.println(jsonResult);
            
        } catch (IOException e) {
            System.err.println("Error reading files: " + e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.err.println("Execution error: " + e.getMessage());
            System.exit(1);
        }
    }
    
    private static TestResult executeRule(String ruleContent, JsonNode testData) {
        TestResult result = new TestResult();
        
        try {
            // Extract rule name
            Pattern ruleNamePattern = Pattern.compile("rule\\s+(\\w+)\\s*:");
            Matcher ruleNameMatcher = ruleNamePattern.matcher(ruleContent);
            String ruleName = ruleNameMatcher.find() ? ruleNameMatcher.group(1) : "unknownRule";
            result.ruleName = ruleName;
            
            // Extract conditions and actions
            Pattern conditionPattern = Pattern.compile("if\\s+(.+?)\\s+then\\s+(\\w+)");
            Matcher conditionMatcher = conditionPattern.matcher(ruleContent);
            
            List<ConditionResult> conditions = new ArrayList<>();
            List<ActionResult> executedActions = new ArrayList<>();
            String finalAction = null;
            
            while (conditionMatcher.find()) {
                String conditionExpr = conditionMatcher.group(1).trim();
                String actionName = conditionMatcher.group(2).trim();
                
                // Evaluate condition
                boolean conditionMet = evaluateCondition(conditionExpr, testData);
                
                ConditionResult condResult = new ConditionResult();
                condResult.condition = conditionExpr;
                condResult.action = actionName;
                condResult.evaluated = true;
                condResult.executed = conditionMet;
                conditions.add(condResult);
                
                // If condition is met, prepare to execute action
                if (conditionMet) {
                    ActionResult actionResult = new ActionResult();
                    actionResult.action = actionName;
                    actionResult.reason = conditionExpr;
                    actionResult.description = ACTION_DESCRIPTIONS.getOrDefault(actionName, "Execute " + actionName);
                    executedActions.add(actionResult);
                    
                    // Set final action (first matching condition wins)
                    if (finalAction == null) {
                        finalAction = actionName;
                    }
                }
            }
            
            // Set results
            result.conditions = conditions;
            result.executedActions = executedActions;
            result.finalAction = finalAction;
            result.actionExecuted = finalAction != null;
            result.executedActionsCount = executedActions.size();
            result.totalAvailableActions = conditions.size();
            result.timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("E MMM dd HH:mm:ss z yyyy"));
            
            if (finalAction != null) {
                result.success = true;
                result.message = "✅ Action to Execute: " + finalAction;
            } else {
                result.success = true;
                result.message = "ℹ️ No conditions matched - no action executed";
            }
            
        } catch (Exception e) {
            result.success = false;
            result.message = "❌ Rule execution failed: " + e.getMessage();
        }
        
        return result;
    }
    
    private static boolean evaluateCondition(String condition, JsonNode testData) {
        try {
            // Handle simple comparisons like "applicant.creditScore >= 750"
            Pattern comparisonPattern = Pattern.compile("(\\w+\\.\\w+)\\s*([<>=!]+)\\s*(.+)");
            Matcher matcher = comparisonPattern.matcher(condition);
            
            if (matcher.find()) {
                String attributePath = matcher.group(1);
                String operator = matcher.group(2);
                String valueStr = matcher.group(3).trim();
                
                // Get value from test data
                Object actualValue = getValueFromPath(testData, attributePath);
                if (actualValue == null) {
                    return false;
                }
                
                // Parse expected value
                Object expectedValue = parseValue(valueStr);
                
                // Perform comparison
                return performComparison(actualValue, operator, expectedValue);
            }
            
            // Handle function calls like "business_date()"
            if (condition.contains("business_date()")) {
                return true; // For demo purposes, assume business date functions pass
            }
            
            // Default to false for unhandled conditions
            return false;
            
        } catch (Exception e) {
            System.err.println("Error evaluating condition '" + condition + "': " + e.getMessage());
            return false;
        }
    }
    
    private static Object getValueFromPath(JsonNode data, String path) {
        String[] parts = path.split("\\.");
        JsonNode current = data;
        
        for (String part : parts) {
            if (current.has(part)) {
                current = current.get(part);
            } else {
                return null;
            }
        }
        
        if (current.isNumber()) {
            return current.asDouble();
        } else if (current.isTextual()) {
            return current.asText();
        } else if (current.isBoolean()) {
            return current.asBoolean();
        }
        
        return current.asText();
    }
    
    private static Object parseValue(String valueStr) {
        valueStr = valueStr.trim();
        
        // Remove quotes if present
        if ((valueStr.startsWith("\"") && valueStr.endsWith("\"")) ||
            (valueStr.startsWith("'") && valueStr.endsWith("'"))) {
            return valueStr.substring(1, valueStr.length() - 1);
        }
        
        // Try to parse as number
        try {
            if (valueStr.contains(".")) {
                return Double.parseDouble(valueStr);
            } else {
                return Integer.parseInt(valueStr);
            }
        } catch (NumberFormatException e) {
            // Return as string
            return valueStr;
        }
    }
    
    private static boolean performComparison(Object actual, String operator, Object expected) {
        try {
            // Convert both to comparable types
            if (actual instanceof Number && expected instanceof Number) {
                double actualNum = ((Number) actual).doubleValue();
                double expectedNum = ((Number) expected).doubleValue();
                
                switch (operator) {
                    case ">": return actualNum > expectedNum;
                    case ">=": return actualNum >= expectedNum;
                    case "<": return actualNum < expectedNum;
                    case "<=": return actualNum <= expectedNum;
                    case "=": case "==": return Math.abs(actualNum - expectedNum) < 0.0001;
                    case "!=": return Math.abs(actualNum - expectedNum) >= 0.0001;
                    default: return false;
                }
            } else {
                // String comparison
                String actualStr = actual.toString();
                String expectedStr = expected.toString();
                
                switch (operator) {
                    case "=": case "==": return actualStr.equals(expectedStr);
                    case "!=": return !actualStr.equals(expectedStr);
                    default: return false;
                }
            }
        } catch (Exception e) {
            return false;
        }
    }
    
    public static class TestResult {
        public boolean success;
        public String message;
        public String ruleName;
        public List<ConditionResult> conditions = new ArrayList<>();
        public List<ActionResult> executedActions = new ArrayList<>();
        public String finalAction;
        public boolean actionExecuted;
        public int executedActionsCount;
        public int totalAvailableActions;
        public String timestamp;
    }
    
    public static class ConditionResult {
        public String condition;
        public String action;
        public boolean evaluated;
        public boolean executed;
    }
    
    public static class ActionResult {
        public String action;
        public String reason;
        public String description;
    }
}