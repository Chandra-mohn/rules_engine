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

        for (RulesParser.DefinitionContext defCtx : ctx.definition()) {
            if (defCtx.unifiedRule() != null) {
                String ruleCode = visit(defCtx.unifiedRule());
                allRules.append(ruleCode).append("\n\n");
            }
        }

        return allRules.toString();
    }
    
    @Override
    public String visitUnifiedRule(RulesParser.UnifiedRuleContext ctx) {
        code.setLength(0);
        indentLevel = 0;
        
        // Extract rule name
        currentRuleName = ctx.ruleName().getText();
        String className = capitalize(currentRuleName) + "Rule";

        // Generate class header
        generateClassHeader(className, currentRuleName, false);
        
        indentLevel++;
        appendLine("");
        appendLine("@Override");
        appendLine("public RuleResult execute(RuleContext ctx) {");
        indentLevel++;

        // Add utility method for comparing values
        generateCompareValuesUtility();
        
        // Generate condition checks for each step
        boolean firstStep = true;
        for (RulesParser.RuleStepContext stepCtx : ctx.ruleStep()) {
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
    public String visitRuleStep(RulesParser.RuleStepContext ctx) {
        StringBuilder stepCode = new StringBuilder();

        if (ctx.condition() != null) {
            // RuleStep with condition: IF condition THEN actionList (ELSE actionList)?
            String conditionCode = visit(ctx.condition());
            String thenActions = generateActionListCode(ctx.actionList(0));

            stepCode.append(indent()).append("if (").append(conditionCode).append(") {\n");
            stepCode.append(thenActions);
            stepCode.append(indent()).append("}");

            // Handle ELSE clause if present
            if (ctx.actionList().size() > 1) {
                String elseActions = generateActionListCode(ctx.actionList(1));
                stepCode.append(" else {\n");
                stepCode.append(elseActions);
                stepCode.append(indent()).append("}");
            }
            stepCode.append("\n");
        } else {
            // RuleStep without condition: just actionList (standalone actions)
            String actions = generateActionListCode(ctx.actionList(0));
            stepCode.append(actions);
        }

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
        List<RulesParser.AttributeIdentifierContext> identifiers = ctx.attributeIdentifier();

        if (identifiers.size() == 1) {
            // Simple attribute like "CREDIT_SCORE" or "credit score"
            String attrName = getIdentifierText(identifiers.get(0));
            return "ctx.getValue(\"" + attrName + "\")";
        } else {
            // Dotted attribute like "applicant.creditScore"
            String path = identifiers.stream()
                    .map(this::getIdentifierText)
                    .collect(Collectors.joining("."));
            return "ctx.getValue(\"" + path + "\")";
        }
    }

    private String getIdentifierText(RulesParser.AttributeIdentifierContext ctx) {
        if (ctx.IDENTIFIER() != null) {
            return ctx.IDENTIFIER().getText();
        } else if (ctx.STRING() != null) {
            // Remove quotes from string
            String text = ctx.STRING().getText();
            return text.substring(1, text.length() - 1);
        }
        return "";
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
        String actionName;

        if (ctx.IDENTIFIER() != null) {
            actionName = ctx.IDENTIFIER().getText();
        } else if (ctx.STRING() != null) {
            String str = ctx.STRING().getText();
            // Remove quotes
            actionName = str.substring(1, str.length() - 1);
        } else {
            actionName = "unknown";
        }

        // Handle parameterized actions
        if (ctx.parameterList() != null) {
            StringBuilder params = new StringBuilder();
            List<RulesParser.ParameterContext> parameters = ctx.parameterList().parameter();

            for (int i = 0; i < parameters.size(); i++) {
                if (i > 0) params.append(", ");
                params.append(visit(parameters.get(i)));
            }

            return actionName + "(" + params.toString() + ")";
        }

        return actionName;
    }


    @Override
    public String visitParameter(RulesParser.ParameterContext ctx) {
        if (ctx.value() != null) {
            return visit(ctx.value());
        } else if (ctx.attribute() != null) {
            return visit(ctx.attribute());
        }
        return "null";
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


    /**
     * Generate code for executing a list of actions
     */
    private String generateActionListCode(RulesParser.ActionListContext ctx) {
        StringBuilder actionCode = new StringBuilder();

        for (RulesParser.ActionContext actionCtx : ctx.action()) {
            String action = visit(actionCtx);
            // Escape quotes in the action string for Java
            String escapedAction = action.replace("\"", "\\\"");
            actionCode.append(indent()).append("    executedActions.add(\"").append(escapedAction).append("\");\n");
        }

        return actionCode.toString();
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

    /**
     * Generate common class header for Rules and ActionSets
     */
    private void generateClassHeader(String className, String ruleName, boolean isActionSet) {
        appendLine("package com.rules.generated;");
        appendLine("");
        appendLine("import com.rules.engine.Rule;");
        appendLine("import com.rules.engine.RuleResult;");
        appendLine("import com.rules.context.RuleContext;");
        appendLine("import java.util.Objects;");
        if (isActionSet) {
            appendLine("import java.util.List;");
            appendLine("import java.util.ArrayList;");
        }
        appendLine("");
        appendLine("/**");
        appendLine(" * Generated " + (isActionSet ? "ActionSet" : "rule") + " class for: " + ruleName);
        appendLine(" * Auto-generated - do not modify manually");
        appendLine(" */");
        appendLine("public class " + className + " implements Rule {");
    }

    /**
     * Generate the compareValues utility method for value comparison
     */
    private void generateCompareValuesUtility() {
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
    }
}