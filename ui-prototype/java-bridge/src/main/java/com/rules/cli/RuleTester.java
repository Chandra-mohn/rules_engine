package com.rules.cli;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.json.JSONObject;
import org.json.JSONArray;

/**
 * Command-line rule tester that executes rules against sample data.
 */
public class RuleTester {
    
    // Using minimal org.json library instead of Jackson
    
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
        
        // Legacy action descriptions
        ACTION_DESCRIPTIONS.put("MIN PAY", "Trigger minimum payment calculation");
        ACTION_DESCRIPTIONS.put("MIN PAY DUE", "Set minimum payment due amount");
        ACTION_DESCRIPTIONS.put("LATE FEE WAIVER", "Waive late fee charges");
        ACTION_DESCRIPTIONS.put("ACCOUNT CLOSURE", "Close customer account");
        ACTION_DESCRIPTIONS.put("CREDIT ADJUSTMENT", "Apply credit adjustment to account");
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
            JSONObject testData = new JSONObject(dataContent);
            
            // Execute rule
            TestResult result = executeRule(ruleContent, testData);
            
            // Output result as JSON
            JSONObject jsonResult = testResultToJson(result);
            System.out.println(jsonResult.toString());
            
        } catch (IOException e) {
            System.err.println("Error reading files: " + e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.err.println("Execution error: " + e.getMessage());
            System.exit(1);
        }
    }
    
    private static TestResult executeRule(String ruleContent, JSONObject testData) {
        TestResult result = new TestResult();
        
        try {
            // Extract rule name
            Pattern ruleNamePattern = Pattern.compile("rule\\s+(\\w+)\\s*:");
            Matcher ruleNameMatcher = ruleNamePattern.matcher(ruleContent);
            String ruleName = ruleNameMatcher.find() ? ruleNameMatcher.group(1) : "unknownRule";
            result.ruleName = ruleName;
            
            // Extract conditions and actions (supports both identifiers and quoted strings)
            Pattern conditionPattern = Pattern.compile("if\\s+(.+?)\\s+then\\s+(\\w+|\"[^\"]+\")");
            Matcher conditionMatcher = conditionPattern.matcher(ruleContent);
            
            List<ConditionResult> conditions = new ArrayList<>();
            List<ActionResult> executedActions = new ArrayList<>();
            String finalAction = null;
            
            while (conditionMatcher.find()) {
                String conditionExpr = conditionMatcher.group(1).trim();
                String actionName = conditionMatcher.group(2).trim();
                
                // Remove quotes from action name if present
                if (actionName.startsWith("\"") && actionName.endsWith("\"")) {
                    actionName = actionName.substring(1, actionName.length() - 1);
                }
                
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
            result.timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            
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
    
    private static boolean evaluateCondition(String condition, JSONObject testData) {
        try {
            // Handle simple comparisons like "applicant.creditScore >= 750" or quoted attributes like "CH CO CALL" = "Y"
            Pattern comparisonPattern = Pattern.compile("(\\w+\\.\\w+|\"[^\"]+\")\\s*([<>=!]+)\\s*(.+)");
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
            
            // Handle function calls like "business_date()" - evaluate properly
            if (condition.contains("business_date()")) {
                // Replace business_date() with current date for comparison
                String currentDate = java.time.LocalDate.now().toString();
                String evaluableCondition = condition.replace("business_date()", "\"" + currentDate + "\"");
                
                // Re-parse and evaluate the condition with actual date
                Pattern businessDatePattern = Pattern.compile("(\\w+\\.\\w+)\\s*([<>=!]+)\\s*(.+)");
                Matcher businessDateMatcher = businessDatePattern.matcher(evaluableCondition);
                
                if (businessDateMatcher.find()) {
                    String attributePath = businessDateMatcher.group(1);
                    String operator = businessDateMatcher.group(2);
                    String valueStr = businessDateMatcher.group(3).trim();
                    
                    Object actualValue = getValueFromPath(testData, attributePath);
                    if (actualValue == null) {
                        return false;
                    }
                    
                    Object expectedValue = parseValue(valueStr);
                    return performComparison(actualValue, operator, expectedValue);
                }
                
                return false; // If we can't parse the condition properly
            }
            
            // Default to false for unhandled conditions
            return false;
            
        } catch (Exception e) {
            System.err.println("Error evaluating condition '" + condition + "': " + e.getMessage());
            return false;
        }
    }
    
    private static Object getValueFromPath(JSONObject data, String path) {
        // Handle quoted attribute names (legacy style)
        if (path.startsWith("\"") && path.endsWith("\"")) {
            String key = path.substring(1, path.length() - 1);
            return data.has(key) ? data.get(key) : null;
        }
        
        // Handle dotted paths like "applicant.creditScore"
        String[] parts = path.split("\\.");
        Object current = data;
        
        for (String part : parts) {
            if (current instanceof JSONObject) {
                JSONObject obj = (JSONObject) current;
                if (obj.has(part)) {
                    current = obj.get(part);
                } else {
                    return null;
                }
            } else {
                return null;
            }
        }
        
        return current;
    }
    
    private static JSONObject testResultToJson(TestResult result) {
        JSONObject json = new JSONObject();
        json.put("success", result.success);
        json.put("message", result.message);
        json.put("ruleName", result.ruleName);
        json.put("finalAction", result.finalAction);
        json.put("actionExecuted", result.actionExecuted);
        json.put("executedActionsCount", result.executedActionsCount);
        json.put("totalAvailableActions", result.totalAvailableActions);
        json.put("timestamp", result.timestamp);
        
        // Convert conditions
        JSONArray conditionsArray = new JSONArray();
        for (ConditionResult cond : result.conditions) {
            JSONObject condJson = new JSONObject();
            condJson.put("condition", cond.condition);
            condJson.put("action", cond.action);
            condJson.put("evaluated", cond.evaluated);
            condJson.put("executed", cond.executed);
            conditionsArray.put(condJson);
        }
        json.put("conditions", conditionsArray);
        
        // Convert executed actions
        JSONArray actionsArray = new JSONArray();
        for (ActionResult action : result.executedActions) {
            JSONObject actionJson = new JSONObject();
            actionJson.put("action", action.action);
            actionJson.put("reason", action.reason);
            actionJson.put("description", action.description);
            actionsArray.put(actionJson);
        }
        json.put("executedActions", actionsArray);
        
        return json;
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
                    case "!=": case "<>": return Math.abs(actualNum - expectedNum) >= 0.0001;
                    default: return false;
                }
            } else {
                // String comparison - check if they look like dates
                String actualStr = actual.toString();
                String expectedStr = expected.toString();
                
                // If both look like dates (YYYY-MM-DD format), do date comparison
                if (actualStr.matches("\\d{4}-\\d{2}-\\d{2}") && expectedStr.matches("\\d{4}-\\d{2}-\\d{2}")) {
                    try {
                        java.time.LocalDate actualDate = java.time.LocalDate.parse(actualStr);
                        java.time.LocalDate expectedDate = java.time.LocalDate.parse(expectedStr);
                        
                        switch (operator) {
                            case ">": return actualDate.isAfter(expectedDate);
                            case ">=": return actualDate.isAfter(expectedDate) || actualDate.equals(expectedDate);
                            case "<": return actualDate.isBefore(expectedDate);
                            case "<=": return actualDate.isBefore(expectedDate) || actualDate.equals(expectedDate);
                            case "=": case "==": return actualDate.equals(expectedDate);
                            case "!=": case "<>": return !actualDate.equals(expectedDate);
                            default: return false;
                        }
                    } catch (Exception e) {
                        // Fall back to string comparison if date parsing fails
                    }
                }
                
                // Regular string comparison
                switch (operator) {
                    case "=": case "==": return actualStr.equals(expectedStr);
                    case "!=": case "<>": return !actualStr.equals(expectedStr);
                    case ">": return actualStr.compareTo(expectedStr) > 0;
                    case ">=": return actualStr.compareTo(expectedStr) >= 0;
                    case "<": return actualStr.compareTo(expectedStr) < 0;
                    case "<=": return actualStr.compareTo(expectedStr) <= 0;
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