package com.rules.cli;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Command-line rule validator for the UI prototype.
 * This is a simplified version that provides basic validation feedback.
 */
public class RuleValidator {
    
    // Valid keywords
    private static final Set<String> VALID_KEYWORDS = new HashSet<>(Arrays.asList(
        "rule", "if", "then", "else", "and", "or", "not",
        "true", "false", "null", "before", "after", "between", "within",
        "in", "is_weekend", "is_weekday", "is_holiday"
    ));
    
    // Valid operators
    private static final Set<String> VALID_OPERATORS = new HashSet<>(Arrays.asList(
        "=", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/", "%"
    ));
    
    // Valid actions
    private static final Set<String> VALID_ACTIONS = new HashSet<>(Arrays.asList(
        "approveApplication", "rejectApplication", "conditionalApproval", "instantApproval",
        "manualReview", "requireManualReview", "approveTransaction", "declineTransaction",
        "flagForReview", "sendAlert", "requestVerification", "setLimit"
    ));
    
    // Valid functions
    private static final Set<String> VALID_FUNCTIONS = new HashSet<>(Arrays.asList(
        "now", "today", "business_date", "year_of", "month_of", "day_of", "day_of_week",
        "day_of_year", "week_of_year", "contains", "starts_with", "ends_with", "matches"
    ));
    
    // Valid attributes (entity.property pattern)
    private static final Set<String> VALID_ENTITIES = new HashSet<>(Arrays.asList(
        "applicant", "transaction", "account"
    ));
    
    private static final Set<String> VALID_APPLICANT_PROPS = new HashSet<>(Arrays.asList(
        "creditScore", "age", "annualIncome", "monthlyIncome", "employmentStatus",
        "employmentYears", "applicationDate", "birthDate", "requestedLimit",
        "existingDebt", "bankruptcyHistory", "ssn"
    ));
    
    private static final Set<String> VALID_TRANSACTION_PROPS = new HashSet<>(Arrays.asList(
        "amount", "timestamp", "merchantCategory", "location", "type", "isOnline"
    ));
    
    private static final Set<String> VALID_ACCOUNT_PROPS = new HashSet<>(Arrays.asList(
        "currentBalance", "creditLimit", "availableCredit", "paymentHistory", "accountAge"
    ));
    
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
        
        // Check for "then" keyword (more flexible to catch typos first)
        if (!content.matches(".*\\bthen\\b.*")) {
            // Don't fail here - let the enhanced validation catch the typo
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
        
        // Enhanced validation: check for invalid keywords and actions
        ValidationResult keywordCheck = validateKeywordsAndActions(content);
        if (!keywordCheck.isValid()) {
            return keywordCheck;
        }
        
        // Enhanced validation: check for invalid attributes
        ValidationResult attributeCheck = validateAttributes(content);
        if (!attributeCheck.isValid()) {
            return attributeCheck;
        }
        
        // Basic syntax checks passed
        return new ValidationResult(true, "Rule syntax is valid");
    }
    
    private static ValidationResult validateKeywordsAndActions(String content) {
        // First, validate structure with better tokenization
        ValidationResult structureResult = validateRuleStructure(content);
        if (!structureResult.isValid()) {
            return structureResult;
        }
        
        // Tokenize more intelligently, preserving word boundaries and context
        String[] tokens = content.split("\\s+|(?<=[^a-zA-Z_])(?=[a-zA-Z_])|(?<=[a-zA-Z_])(?=[^a-zA-Z_])");
        
        for (int i = 0; i < tokens.length; i++) {
            String token = tokens[i].trim();
            if (token.isEmpty() || isSkippableToken(token)) {
                continue;
            }
            
            String cleanToken = token.replaceAll("[^a-zA-Z_]", "");
            if (cleanToken.isEmpty()) continue;
            
            // Skip known valid tokens
            if (isValidToken(cleanToken, content)) {
                continue;
            }
            
            // Context-aware validation
            String context = getTokenContext(tokens, i);
            
            // Check for keyword typos
            String keywordSuggestion = findClosestKeyword(cleanToken);
            if (keywordSuggestion != null && shouldBeKeyword(cleanToken, context)) {
                return new ValidationResult(false, "Invalid keyword '" + cleanToken + "'. Did you mean '" + keywordSuggestion + "'?");
            }
            
            // Check for action typos
            if (isInActionContext(context)) {
                String actionSuggestion = findClosestAction(cleanToken);
                if (actionSuggestion != null) {
                    return new ValidationResult(false, "Invalid action '" + cleanToken + "'. Did you mean '" + actionSuggestion + "'?");
                } else if (!isAttributeReference(cleanToken)) {
                    return new ValidationResult(false, "Invalid action '" + cleanToken + "'. Valid actions are: " + 
                                              String.join(", ", VALID_ACTIONS));
                }
            }
        }
        
        return new ValidationResult(true, "Keywords and actions are valid");
    }
    
    private static ValidationResult validateRuleStructure(String content) {
        // Check for required keywords with better pattern matching
        if (!content.matches("(?s).*\\brule\\s+\\w+\\s*:.*")) {
            return new ValidationResult(false, "Rule must start with 'rule <name>:'");
        }
        
        if (!content.matches("(?s).*\\bif\\b.*")) {
            return new ValidationResult(false, "Rule must contain 'if' condition");
        }
        
        // More flexible "then" check that can catch typos
        if (!content.matches("(?s).*\\b(then|thn|thne|thien|ten)\\b.*")) {
            return new ValidationResult(false, "Rule must contain 'then' clause");
        }
        
        // If we found a "then" typo, report it specifically
        if (content.matches("(?s).*\\b(thn|thne|thien|ten)\\b.*") && 
            !content.matches("(?s).*\\bthen\\b.*")) {
            Pattern typoPattern = Pattern.compile("\\b(thn|thne|thien|ten)\\b");
            Matcher matcher = typoPattern.matcher(content);
            if (matcher.find()) {
                String typo = matcher.group(1);
                return new ValidationResult(false, "Invalid keyword '" + typo + "'. Did you mean 'then'?");
            }
        }
        
        // Check for invalid grammar structures
        ValidationResult grammarResult = validateGrammarStructure(content);
        if (!grammarResult.isValid()) {
            return grammarResult;
        }
        
        return new ValidationResult(true, "Rule structure is valid");
    }
    
    private static ValidationResult validateGrammarStructure(String content) {
        // Check for invalid "then action and" pattern
        Pattern invalidThenAnd = Pattern.compile("\\bthen\\s+\\w+\\s+and\\s+if\\b");
        if (invalidThenAnd.matcher(content).find()) {
            return new ValidationResult(false, "Invalid syntax: 'then action and if'. Each if-then should be a separate statement.");
        }
        
        // Check for invalid "action and if" pattern (catches variations)
        Pattern actionAndIf = Pattern.compile("\\bthen\\s+\\w+\\s+and\\s+");
        if (actionAndIf.matcher(content).find()) {
            return new ValidationResult(false, "Invalid syntax after action. Use separate if-then statements instead of 'and'.");
        }
        
        // Check for invalid operators with spaces
        ValidationResult operatorResult = validateOperators(content);
        if (!operatorResult.isValid()) {
            return operatorResult;
        }
        
        // Validate if-then statement structure
        String[] lines = content.split("\\n");
        boolean inRule = false;
        
        for (String line : lines) {
            line = line.trim();
            
            if (line.matches("\\s*rule\\s+\\w+\\s*:.*")) {
                inRule = true;
                continue;
            }
            
            if (inRule && !line.isEmpty()) {
                // Each non-empty line in a rule should be an if-then statement
                if (line.matches("\\s*if\\s+.*\\s+then\\s+\\w+\\s*")) {
                    continue; // Valid if-then statement
                } else if (line.matches("\\s*if\\s+.*")) {
                    return new ValidationResult(false, "Incomplete if statement: '" + line + "'. Missing 'then' clause.");
                } else if (!line.matches("\\s*//.*") && !line.matches("\\s*/\\*.*")) { // Skip comments
                    return new ValidationResult(false, "Invalid statement: '" + line + "'. Expected 'if condition then action'.");
                }
            }
        }
        
        return new ValidationResult(true, "Grammar structure is valid");
    }
    
    private static ValidationResult validateOperators(String content) {
        // Check for invalid operators with spaces
        Pattern[] invalidOperatorPatterns = {
            Pattern.compile("\\s<\\s+=\\s"),        // < =
            Pattern.compile("\\s>\\s+=\\s"),        // > =
            Pattern.compile("\\s!\\s+=\\s"),        // ! =
            Pattern.compile("\\s=\\s+=\\s"),        // = =
            Pattern.compile("\\s<\\s+>\\s"),        // < >
        };
        
        String[] operatorNames = {
            "<=", ">=", "!=", "==", "!="
        };
        
        for (int i = 0; i < invalidOperatorPatterns.length; i++) {
            Matcher matcher = invalidOperatorPatterns[i].matcher(content);
            if (matcher.find()) {
                String found = matcher.group().trim();
                return new ValidationResult(false, "Invalid operator '" + found + "'. Did you mean '" + operatorNames[i] + "'?");
            }
        }
        
        // Check for other common spacing issues in operators
        Pattern spacedEquals = Pattern.compile("\\w\\s+=\\s+\\w");
        Matcher matcher = spacedEquals.matcher(content);
        if (matcher.find()) {
            String context = matcher.group();
            // Only flag if it's not part of a comparison like "age = 20" (which is valid)
            // But flag "< =" type patterns
            if (context.matches(".*[<>!]\\s+=\\s+.*")) {
                return new ValidationResult(false, "Invalid operator with spaces. Remove spaces in comparison operators.");
            }
        }
        
        return new ValidationResult(true, "Operators are valid");
    }
    
    private static boolean isSkippableToken(String token) {
        return token.matches("[\\p{Punct}&&[^_]]+") || // Punctuation except underscore
               token.matches("\\d+") || // Numbers
               token.matches("\".*\"") || // Quoted strings
               token.matches("'.*'"); // Single quoted strings
    }
    
    private static boolean isValidToken(String token, String content) {
        return VALID_KEYWORDS.contains(token) || 
               VALID_ACTIONS.contains(token) || 
               VALID_FUNCTIONS.contains(token) ||
               isAttributeReference(token) ||
               isRuleName(token, content);
    }
    
    private static boolean isAttributeReference(String token) {
        // Check if it's part of an entity.property pattern or a known property
        return VALID_ENTITIES.contains(token) ||
               VALID_APPLICANT_PROPS.contains(token) ||
               VALID_TRANSACTION_PROPS.contains(token) ||
               VALID_ACCOUNT_PROPS.contains(token);
    }
    
    private static boolean isRuleName(String token, String content) {
        // Check if this token appears as a rule name after "rule"
        Pattern rulePattern = Pattern.compile("\\brule\\s+" + Pattern.quote(token) + "\\s*:");
        return rulePattern.matcher(content).find();
    }
    
    private static String getTokenContext(String[] tokens, int index) {
        StringBuilder context = new StringBuilder();
        
        // Get 2 tokens before and after for context
        int start = Math.max(0, index - 2);
        int end = Math.min(tokens.length, index + 3);
        
        for (int i = start; i < end; i++) {
            if (i == index) {
                context.append("[").append(tokens[i]).append("] ");
            } else {
                context.append(tokens[i]).append(" ");
            }
        }
        
        return context.toString().trim();
    }
    
    private static boolean shouldBeKeyword(String token, String context) {
        // More intelligent context checking
        return context.contains("then") || // appears after "then"
               context.contains("if") ||   // appears after "if"
               context.matches(".*\\w+\\s+\\[" + Pattern.quote(token) + "\\]\\s+\\w+.*"); // between other words
    }
    
    private static boolean isInActionContext(String context) {
        return context.matches(".*\\b(then|thn|thne|thien)\\s+\\[.*") || // after "then" variants
               context.matches(".*\\]\\s*$"); // at end of rule
    }

    
    private static String findClosestKeyword(String input) {
        String bestMatch = null;
        int bestScore = Integer.MAX_VALUE;
        
        for (String keyword : VALID_KEYWORDS) {
            int score = calculateLevenshteinDistance(input.toLowerCase(), keyword.toLowerCase());
            if (score <= 2 && score < bestScore) { // Allow up to 2 character differences
                bestScore = score;
                bestMatch = keyword;
            }
        }
        
        return bestMatch;
    }
    
    private static ValidationResult validateAttributes(String content) {
        // Check for entity.property patterns
        Pattern attributePattern = Pattern.compile("(\\w+)\\.(\\w+)");
        Matcher matcher = attributePattern.matcher(content);
        while (matcher.find()) {
            String entity = matcher.group(1);
            String property = matcher.group(2);
            
            if (!VALID_ENTITIES.contains(entity)) {
                return new ValidationResult(false, "Invalid entity '" + entity + "'. Valid entities are: applicant, transaction, account");
            }
            
            Set<String> validProps = getValidPropertiesForEntity(entity);
            if (!validProps.contains(property)) {
                return new ValidationResult(false, "Invalid property '" + property + "' for entity '" + entity + "'");
            }
        }
        
        return new ValidationResult(true, "Attributes are valid");
    }
    
    private static Set<String> getValidPropertiesForEntity(String entity) {
        switch (entity) {
            case "applicant": return VALID_APPLICANT_PROPS;
            case "transaction": return VALID_TRANSACTION_PROPS;
            case "account": return VALID_ACCOUNT_PROPS;
            default: return new HashSet<>();
        }
    }

    
    private static String findClosestAction(String input) {
        String bestMatch = null;
        int bestScore = Integer.MAX_VALUE;
        
        for (String action : VALID_ACTIONS) {
            int score = calculateLevenshteinDistance(input.toLowerCase(), action.toLowerCase());
            if (score <= 2 && score < bestScore) { // Allow up to 2 character differences
                bestScore = score;
                bestMatch = action;
            }
        }
        
        return bestMatch;
    }
    
    private static int calculateLevenshteinDistance(String s1, String s2) {
        int[][] dp = new int[s1.length() + 1][s2.length() + 1];
        
        for (int i = 0; i <= s1.length(); i++) {
            dp[i][0] = i;
        }
        
        for (int j = 0; j <= s2.length(); j++) {
            dp[0][j] = j;
        }
        
        for (int i = 1; i <= s1.length(); i++) {
            for (int j = 1; j <= s2.length(); j++) {
                if (s1.charAt(i - 1) == s2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1];
                } else {
                    dp[i][j] = 1 + Math.min(Math.min(dp[i - 1][j], dp[i][j - 1]), dp[i - 1][j - 1]);
                }
            }
        }
        
        return dp[s1.length()][s2.length()];
    }
    
    public static class ValidationResult {
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