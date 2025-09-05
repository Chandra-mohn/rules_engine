package com.rules.cli;

import com.rules.grammar.*;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

/**
 * ANTLR-based rule validator that provides proper parsing and semantic validation.
 */
public class RuleValidatorANTLR {
    
    // Valid actions from the schema
    private static final Set<String> VALID_ACTIONS = new HashSet<>();
    static {
        VALID_ACTIONS.add("approveApplication");
        VALID_ACTIONS.add("rejectApplication");
        VALID_ACTIONS.add("conditionalApproval");
        VALID_ACTIONS.add("instantApproval");
        VALID_ACTIONS.add("manualReview");
        VALID_ACTIONS.add("requireManualReview");
        VALID_ACTIONS.add("approveTransaction");
        VALID_ACTIONS.add("declineTransaction");
        VALID_ACTIONS.add("flagForReview");
        VALID_ACTIONS.add("sendAlert");
        VALID_ACTIONS.add("requestVerification");
        VALID_ACTIONS.add("setLimit");
    }
    
    // Valid entities and their properties
    private static final Set<String> VALID_ENTITIES = new HashSet<>();
    static {
        VALID_ENTITIES.add("applicant");
        VALID_ENTITIES.add("transaction");
        VALID_ENTITIES.add("account");
    }
    
    private static final Set<String> VALID_APPLICANT_PROPS = new HashSet<>();
    static {
        VALID_APPLICANT_PROPS.add("creditScore");
        VALID_APPLICANT_PROPS.add("age");
        VALID_APPLICANT_PROPS.add("annualIncome");
        VALID_APPLICANT_PROPS.add("monthlyIncome");
        VALID_APPLICANT_PROPS.add("employmentStatus");
        VALID_APPLICANT_PROPS.add("employmentYears");
        VALID_APPLICANT_PROPS.add("applicationDate");
        VALID_APPLICANT_PROPS.add("birthDate");
        VALID_APPLICANT_PROPS.add("requestedLimit");
        VALID_APPLICANT_PROPS.add("existingDebt");
        VALID_APPLICANT_PROPS.add("bankruptcyHistory");
        VALID_APPLICANT_PROPS.add("ssn");
    }
    
    private static final Set<String> VALID_TRANSACTION_PROPS = new HashSet<>();
    static {
        VALID_TRANSACTION_PROPS.add("amount");
        VALID_TRANSACTION_PROPS.add("timestamp");
        VALID_TRANSACTION_PROPS.add("merchantCategory");
        VALID_TRANSACTION_PROPS.add("location");
        VALID_TRANSACTION_PROPS.add("type");
        VALID_TRANSACTION_PROPS.add("isOnline");
    }
    
    private static final Set<String> VALID_ACCOUNT_PROPS = new HashSet<>();
    static {
        VALID_ACCOUNT_PROPS.add("currentBalance");
        VALID_ACCOUNT_PROPS.add("creditLimit");
        VALID_ACCOUNT_PROPS.add("availableCredit");
        VALID_ACCOUNT_PROPS.add("paymentHistory");
        VALID_ACCOUNT_PROPS.add("accountAge");
    }
    
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Usage: java RuleValidatorANTLR <rule-file>");
            System.exit(1);
        }
        
        String ruleFile = args[0];
        
        try {
            String content = Files.readString(Paths.get(ruleFile));
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
        content = content.trim();
        
        if (content.isEmpty()) {
            return new ValidationResult(false, "Rule content cannot be empty");
        }
        
        try {
            // Create ANTLR input stream
            CharStream input = CharStreams.fromString(content);
            
            // Create lexer
            RulesLexer lexer = new RulesLexer(input);
            
            // Custom error listener to collect lexical errors
            CollectingErrorListener lexerErrorListener = new CollectingErrorListener();
            lexer.removeErrorListeners(); // Remove default error listeners
            lexer.addErrorListener(lexerErrorListener);
            
            // Create token stream
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            
            // Create parser
            RulesParser parser = new RulesParser(tokens);
            
            // Custom error listener to collect parsing errors
            CollectingErrorListener parserErrorListener = new CollectingErrorListener();
            parser.removeErrorListeners(); // Remove default error listeners
            parser.addErrorListener(parserErrorListener);
            
            // Parse the input
            ParseTree tree = parser.ruleSet();
            
            // Check for lexical errors
            if (!lexerErrorListener.getErrors().isEmpty()) {
                String firstError = lexerErrorListener.getErrors().get(0);
                return new ValidationResult(false, "Lexical error: " + firstError);
            }
            
            // Check for parsing errors
            if (!parserErrorListener.getErrors().isEmpty()) {
                String firstError = parserErrorListener.getErrors().get(0);
                return new ValidationResult(false, "Syntax error: " + firstError);
            }
            
            // Perform semantic validation using a visitor
            SemanticValidator validator = new SemanticValidator();
            ParseTreeWalker walker = new ParseTreeWalker();
            walker.walk(validator, tree);
            
            if (!validator.getErrors().isEmpty()) {
                String firstError = validator.getErrors().get(0);
                return new ValidationResult(false, firstError);
            }
            
            return new ValidationResult(true, "Rule syntax and semantics are valid");
            
        } catch (Exception e) {
            return new ValidationResult(false, "Parser error: " + e.getMessage());
        }
    }
    
    /**
     * Custom error listener that collects error messages instead of printing them.
     */
    private static class CollectingErrorListener extends BaseErrorListener {
        private final List<String> errors = new ArrayList<>();
        
        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                               int line, int charPositionInLine, String msg, RecognitionException e) {
            errors.add("Line " + line + ":" + charPositionInLine + " " + msg);
        }
        
        public List<String> getErrors() {
            return errors;
        }
    }
    
    /**
     * Semantic validator that walks the parse tree and validates semantics.
     */
    private static class SemanticValidator extends RulesBaseListener {
        private final List<String> errors = new ArrayList<>();
        
        @Override
        public void enterAction(RulesParser.ActionContext ctx) {
            String actionName = ctx.IDENTIFIER().getText();
            
            if (!VALID_ACTIONS.contains(actionName)) {
                String suggestion = findClosestAction(actionName);
                if (suggestion != null) {
                    errors.add("Invalid action '" + actionName + "'. Did you mean '" + suggestion + "'?");
                } else {
                    errors.add("Invalid action '" + actionName + "'. Valid actions are: " + 
                              String.join(", ", VALID_ACTIONS));
                }
            }
        }
        
        @Override
        public void enterAttribute(RulesParser.AttributeContext ctx) {
            List<TerminalNode> identifiers = ctx.IDENTIFIER();
            
            if (identifiers.size() >= 2) {
                String entity = identifiers.get(0).getText();
                String property = identifiers.get(1).getText();
                
                if (!VALID_ENTITIES.contains(entity)) {
                    errors.add("Invalid entity '" + entity + "'. Valid entities are: applicant, transaction, account");
                    return;
                }
                
                Set<String> validProps = getValidPropertiesForEntity(entity);
                if (!validProps.contains(property)) {
                    errors.add("Invalid property '" + property + "' for entity '" + entity + "'");
                }
            }
        }
        
        public List<String> getErrors() {
            return errors;
        }
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
            if (score <= 2 && score < bestScore) {
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