package com.rules.parser;

import com.rules.ast.RuleSetNode;
import com.rules.grammar.RulesLexer;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

/**
 * Main parser class for rules DSL.
 * Provides high-level parsing interface.
 */
public class RulesParser {
    
    /**
     * Parse rules from string input.
     * 
     * @param input Rules DSL input
     * @return AST root node
     * @throws ParseException if parsing fails
     */
    public static RuleSetNode parse(String input) throws ParseException {
        try {
            // Create lexer
            CharStream charStream = CharStreams.fromString(input);
            RulesLexer lexer = new RulesLexer(charStream);
            
            // Create token stream
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            
            // Create parser
            com.rules.grammar.RulesParser parser = new com.rules.grammar.RulesParser(tokens);
            
            // Add error listener
            parser.removeErrorListeners();
            parser.addErrorListener(new ThrowingErrorListener());
            
            // Parse
            ParseTree tree = parser.ruleSet();
            
            // Build AST
            ASTBuilder builder = new ASTBuilder();
            return (RuleSetNode) builder.visit(tree);
            
        } catch (Exception e) {
            throw new ParseException("Failed to parse rules: " + e.getMessage(), e);
        }
    }
    
    /**
     * Parse rules from file.
     * 
     * @param filename Path to rules file
     * @return AST root node
     * @throws ParseException if parsing fails
     */
    public static RuleSetNode parseFile(String filename) throws ParseException {
        try {
            // Create lexer from file
            CharStream charStream = CharStreams.fromFileName(filename);
            RulesLexer lexer = new RulesLexer(charStream);
            
            // Create token stream
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            
            // Create parser
            com.rules.grammar.RulesParser parser = new com.rules.grammar.RulesParser(tokens);
            
            // Add error listener
            parser.removeErrorListeners();
            parser.addErrorListener(new ThrowingErrorListener());
            
            // Parse
            ParseTree tree = parser.ruleSet();
            
            // Build AST
            ASTBuilder builder = new ASTBuilder();
            return (RuleSetNode) builder.visit(tree);
            
        } catch (Exception e) {
            throw new ParseException("Failed to parse rules file '" + filename + "': " + e.getMessage(), e);
        }
    }
    
    /**
     * Custom error listener that throws exceptions instead of printing to stderr.
     */
    private static class ThrowingErrorListener extends BaseErrorListener {
        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                               int line, int charPositionInLine, String msg, RecognitionException e) {
            throw new RuntimeException("Syntax error at line " + line + ":" + charPositionInLine + " - " + msg);
        }
    }
    
    /**
     * Exception thrown when parsing fails.
     */
    public static class ParseException extends Exception {
        public ParseException(String message) {
            super(message);
        }
        
        public ParseException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}