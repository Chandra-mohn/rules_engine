package com.rules.codegen;

import com.rules.grammar.RulesBaseVisitor;
import com.rules.grammar.RulesParser;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Generates Java code directly from ANTLR parse tree.
 * Works with the main Rules.g4 grammar without intermediate AST.
 */
public class DirectJavaCodeGenerator extends RulesBaseVisitor<String> {
    
    private final StringBuilder code = new StringBuilder();
    private int indentLevel = 0;
    private String currentRuleName = "";
    
    /**
     * Generate Java code for a complete rule set.
     */
    public String generateCode(RulesParser.RuleSetContext ctx) {
        code.setLength(0);
        indentLevel = 0;
        
        return visit(ctx);
    }
    
    @Override
    public String visitRuleSet(RulesParser.RuleSetContext ctx) {
        StringBuilder allRules = new StringBuilder();
        
        for (RulesParser.NamedRuleContext ruleCtx : ctx.namedRule()) {
            String ruleCode = visit(ruleCtx);
            allRules.append(ruleCode).append("\n\n");
        }
        
        return allRules.toString();
    }
    
    @Override
    public String visitNamedRule(RulesParser.NamedRuleContext ctx) {
        code.setLength(0);
        indentLevel = 0;
        
        // Extract rule name
        currentRuleName = ctx.ruleName().getText();
        String className = capitalize(currentRuleName) + "Rule";
        
        // Generate class header
        appendLine("package com.rules.generated;");
        appendLine("");
        appendLine("import com.rules.engine.Rule;");
        appendLine("import com.rules.engine.RuleResult;");
        appendLine("import com.rules.context.RuleContext;");
        appendLine("import java.util.Objects;");
        appendLine("");
        appendLine("/**");
        appendLine(" * Generated rule class for: " + currentRuleName);
        appendLine(" * Auto-generated - do not modify manually");
        appendLine(" */");
        appendLine("public class " + className + " implements Rule {");
        
        indentLevel++;
        appendLine("");
        appendLine("@Override");
        appendLine("public RuleResult execute(RuleContext ctx) {");
        indentLevel++;
        
        // Add utility method for comparing values inline
        appendLine("// Utility method for value comparison");
        appendLine("java.util.function.BiFunction<Object, Object, Integer> compareValues = (a, b) -> {");
        appendLine("    if (a == null && b == null) return 0;");
        appendLine("    if (a == null) return -1;");
        appendLine("    if (b == null) return 1;");
        appendLine("    if (a instanceof Number && b instanceof Number) {");
        appendLine("        double da = ((Number) a).doubleValue();");
        appendLine("        double db = ((Number) b).doubleValue();");
        appendLine("        return Double.compare(da, db);");
        appendLine("    }");
        appendLine("    return a.toString().compareTo(b.toString());");
        appendLine("};");
        appendLine("");
        
        // Generate condition checks for each step
        boolean firstStep = true;
        for (RulesParser.StepContext stepCtx : ctx.step()) {
            if (!firstStep) {
                appendLine("");
            }
            String stepCode = visit(stepCtx);
            code.append(stepCode);
            firstStep = false;
        }
        
        // Default return if no conditions match
        appendLine("");
        appendLine("return RuleResult.noMatch();");
        
        indentLevel--;
        appendLine("}");
        
        // Add getRuleName method
        appendLine("");
        appendLine("@Override");
        appendLine("public String getRuleName() {");
        indentLevel++;
        appendLine("return \"" + currentRuleName + "\";");
        indentLevel--;
        appendLine("}");
        
        indentLevel--;
        appendLine("}");
        
        return code.toString();
    }
    
    @Override
    public String visitStep(RulesParser.StepContext ctx) {
        StringBuilder stepCode = new StringBuilder();
        
        // Generate if condition
        String conditionCode = visit(ctx.condition());
        String actionCode = visit(ctx.action(0)); // Get first action
        
        stepCode.append(indent()).append("if (").append(conditionCode).append(") {\n");
        stepCode.append(indent()).append("    return RuleResult.action(\"").append(actionCode).append("\");\n");
        stepCode.append(indent()).append("}\n");
        
        return stepCode.toString();
    }
    
    @Override
    public String visitCondition(RulesParser.ConditionContext ctx) {
        return visit(ctx.orExpression());
    }
    
    @Override
    public String visitOrExpression(RulesParser.OrExpressionContext ctx) {
        List<RulesParser.AndExpressionContext> andExpressions = ctx.andExpression();
        
        if (andExpressions.size() == 1) {
            return visit(andExpressions.get(0));
        }
        
        return andExpressions.stream()
                .map(this::visit)
                .collect(Collectors.joining(" || "));
    }
    
    @Override
    public String visitAndExpression(RulesParser.AndExpressionContext ctx) {
        List<RulesParser.NotExpressionContext> notExpressions = ctx.notExpression();
        
        if (notExpressions.size() == 1) {
            return visit(notExpressions.get(0));
        }
        
        return notExpressions.stream()
                .map(this::visit)
                .collect(Collectors.joining(" && "));
    }
    
    @Override
    public String visitNotExpression(RulesParser.NotExpressionContext ctx) {
        String expression = visit(ctx.primaryExpression());
        
        if (ctx.NOT() != null) {
            return "!(" + expression + ")";
        }
        
        return expression;
    }
    
    @Override
    public String visitPrimaryExpression(RulesParser.PrimaryExpressionContext ctx) {
        if (ctx.comparison() != null) {
            return visit(ctx.comparison());
        }
        
        if (ctx.orExpression() != null) {
            return "(" + visit(ctx.orExpression()) + ")";
        }
        
        return "true"; // fallback
    }
    
    @Override
    public String visitComparison(RulesParser.ComparisonContext ctx) {
        String attribute = visit(ctx.attribute());
        String operator = visit(ctx.operator());
        String operand = visit(ctx.operand());
        
        return generateComparison(attribute, operator, operand);
    }
    
    @Override
    public String visitAttribute(RulesParser.AttributeContext ctx) {
        List<TerminalNode> identifiers = ctx.IDENTIFIER();
        
        if (identifiers.size() == 1) {
            // Simple attribute like "CREDIT_SCORE"
            return "ctx.getValue(\"" + identifiers.get(0).getText() + "\")";
        } else {
            // Dotted attribute like "applicant.creditScore"
            String path = identifiers.stream()
                    .map(TerminalNode::getText)
                    .collect(Collectors.joining("."));
            return "ctx.getValue(\"" + path + "\")";
        }
    }
    
    @Override
    public String visitOperator(RulesParser.OperatorContext ctx) {
        if (ctx.EQ() != null) return "=";
        if (ctx.NE() != null) return "!=";
        if (ctx.LT() != null) return "<";
        if (ctx.LE() != null) return "<=";
        if (ctx.GT() != null) return ">";
        if (ctx.GE() != null) return ">=";
        return "="; // fallback
    }
    
    @Override
    public String visitOperand(RulesParser.OperandContext ctx) {
        if (ctx.attribute() != null) {
            return visit(ctx.attribute());
        }
        if (ctx.value() != null) {
            return visit(ctx.value());
        }
        return "null";
    }
    
    @Override
    public String visitValue(RulesParser.ValueContext ctx) {
        if (ctx.STRING() != null) {
            String str = ctx.STRING().getText();
            // Remove quotes and escape for Java string
            return "\"" + str.substring(1, str.length() - 1).replace("\"", "\\\"") + "\"";
        }
        if (ctx.NUMBER() != null) {
            return ctx.NUMBER().getText();
        }
        if (ctx.BOOLEAN() != null) {
            return ctx.BOOLEAN().getText().toLowerCase();
        }
        if (ctx.NULL() != null) {
            return "null";
        }
        return "null";
    }
    
    @Override
    public String visitAction(RulesParser.ActionContext ctx) {
        if (ctx.IDENTIFIER() != null) {
            return ctx.IDENTIFIER().getText();
        }
        if (ctx.STRING() != null) {
            String str = ctx.STRING().getText();
            // Remove quotes
            return str.substring(1, str.length() - 1);
        }
        return "unknown";
    }
    
    /**
     * Generate comparison code based on attribute, operator and operand
     */
    private String generateComparison(String attribute, String operator, String operand) {
        switch (operator) {
            case "=":
                return "Objects.equals(" + attribute + ", " + operand + ")";
            case "!=":
                return "!Objects.equals(" + attribute + ", " + operand + ")";
            case "<":
                return "compareValues.apply(" + attribute + ", " + operand + ") < 0";
            case "<=":
                return "compareValues.apply(" + attribute + ", " + operand + ") <= 0";
            case ">":
                return "compareValues.apply(" + attribute + ", " + operand + ") > 0";
            case ">=":
                return "compareValues.apply(" + attribute + ", " + operand + ") >= 0";
            default:
                return "Objects.equals(" + attribute + ", " + operand + ")";
        }
    }
    
    // Utility methods
    private void appendLine(String line) {
        if (!line.isEmpty()) {
            code.append(indent()).append(line);
        }
        code.append("\n");
    }
    
    private String indent() {
        return "    ".repeat(indentLevel);
    }
    
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) return str;
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}