package com.rules.codegen;

import com.rules.grammar.RulesLexer;
import com.rules.grammar.RulesParser;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import org.junit.jupiter.api.Test;

public class DirectJavaCodeGeneratorTest {
    
    @Test
    public void testSimpleRuleCodeGeneration() {
        String ruleContent = "rule creditCheck: if CREDIT_SCORE >= 750 then APPROVE";
        
        try {
            // Parse the rule
            CharStream input = CharStreams.fromString(ruleContent);
            RulesLexer lexer = new RulesLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            RulesParser parser = new RulesParser(tokens);
            RulesParser.RuleSetContext parseTree = parser.ruleSet();
            
            // Generate Java code
            DirectJavaCodeGenerator generator = new DirectJavaCodeGenerator();
            String generatedCode = generator.generateCode(parseTree);
            
            // Print the generated code
            System.out.println("Generated Java code:");
            System.out.println("===================");
            System.out.println(generatedCode);
            
            // Basic assertions
            assert generatedCode.contains("class CreditCheckRule implements Rule");
            assert generatedCode.contains("ctx.getValue(\"CREDIT_SCORE\")");
            assert generatedCode.contains("RuleResult.action(\"APPROVE\")");
            
            System.out.println("✅ Code generation test passed!");
            
        } catch (Exception e) {
            System.err.println("❌ Test failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    @Test
    public void testComplexRuleCodeGeneration() {
        String ruleContent = "rule complexCheck: " +
            "if applicant.creditScore >= 750 AND applicant.age > 21 then approveApplication " +
            "if applicant.creditScore < 600 then rejectApplication";
        
        try {
            // Parse the rule
            CharStream input = CharStreams.fromString(ruleContent);
            RulesLexer lexer = new RulesLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            RulesParser parser = new RulesParser(tokens);
            RulesParser.RuleSetContext parseTree = parser.ruleSet();
            
            // Generate Java code
            DirectJavaCodeGenerator generator = new DirectJavaCodeGenerator();
            String generatedCode = generator.generateCode(parseTree);
            
            // Print the generated code
            System.out.println("\nGenerated Java code for complex rule:");
            System.out.println("=====================================");
            System.out.println(generatedCode);
            
            // Basic assertions
            assert generatedCode.contains("class ComplexCheckRule implements Rule");
            assert generatedCode.contains("ctx.getValue(\"applicant.creditScore\")");
            assert generatedCode.contains("ctx.getValue(\"applicant.age\")");
            assert generatedCode.contains("&&"); // AND condition
            
            System.out.println("✅ Complex code generation test passed!");
            
        } catch (Exception e) {
            System.err.println("❌ Complex test failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
}