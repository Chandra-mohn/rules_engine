package com.rules.parser;

import com.rules.ast.*;
import com.rules.grammar.RulesBaseVisitor;
import com.rules.grammar.RulesParser;
import org.antlr.v4.runtime.tree.ParseTree;

import java.util.ArrayList;
import java.util.List;

/**
 * Builds AST from ANTLR parse tree.
 * Converts ANTLR-generated parse tree into our custom AST nodes.
 */
public class ASTBuilder extends RulesBaseVisitor<ASTNode> {
    
    @Override
    public ASTNode visitRuleSet(RulesParser.RuleSetContext ctx) {
        RuleSetNode ruleSet = new RuleSetNode();
        
        for (RulesParser.NamedRuleContext ruleCtx : ctx.namedRule()) {
            NamedRuleNode rule = (NamedRuleNode) visit(ruleCtx);
            ruleSet.addRule(rule);
        }
        
        return ruleSet;
    }
    
    @Override
    public ASTNode visitNamedRule(RulesParser.NamedRuleContext ctx) {
        String ruleName = ctx.ruleName().IDENTIFIER().getText();
        NamedRuleNode rule = new NamedRuleNode(ruleName);
        
        for (RulesParser.StepContext stepCtx : ctx.step()) {
            StepNode step = (StepNode) visit(stepCtx);
            rule.addStep(step);
        }
        
        return rule;
    }
    
    @Override
    public ASTNode visitStep(RulesParser.StepContext ctx) {
        ASTNode condition = visit(ctx.condition());
        ActionNode thenAction = (ActionNode) visit(ctx.action(0));
        ActionNode elseAction = null;
        
        if (ctx.action().size() > 1) {
            elseAction = (ActionNode) visit(ctx.action(1));
        }
        
        return new StepNode(condition, thenAction, elseAction);
    }
    
    @Override
    public ASTNode visitCondition(RulesParser.ConditionContext ctx) {
        return visit(ctx.orExpression());
    }
    
    @Override
    public ASTNode visitOrExpression(RulesParser.OrExpressionContext ctx) {
        List<RulesParser.AndExpressionContext> andExpressions = ctx.andExpression();
        
        if (andExpressions.size() == 1) {
            // Single AND expression, no OR needed
            return visit(andExpressions.get(0));
        }
        
        // Multiple AND expressions connected by OR
        OrExpressionNode orNode = new OrExpressionNode();
        for (RulesParser.AndExpressionContext andCtx : andExpressions) {
            ASTNode andNode = visit(andCtx);
            orNode.addOperand(andNode);
        }
        
        return orNode;
    }
    
    @Override
    public ASTNode visitAndExpression(RulesParser.AndExpressionContext ctx) {
        List<RulesParser.NotExpressionContext> notExpressions = ctx.notExpression();
        
        if (notExpressions.size() == 1) {
            // Single NOT expression, no AND needed
            return visit(notExpressions.get(0));
        }
        
        // Multiple NOT expressions connected by AND
        AndExpressionNode andNode = new AndExpressionNode();
        for (RulesParser.NotExpressionContext notCtx : notExpressions) {
            ASTNode notNode = visit(notCtx);
            andNode.addOperand(notNode);
        }
        
        return andNode;
    }
    
    @Override
    public ASTNode visitNotExpression(RulesParser.NotExpressionContext ctx) {
        boolean negated = ctx.NOT() != null;
        ASTNode operand = visit(ctx.primaryExpression());
        
        return new NotExpressionNode(operand, negated);
    }
    
    @Override
    public ASTNode visitPrimaryExpression(RulesParser.PrimaryExpressionContext ctx) {
        if (ctx.comparison() != null) {
            return visit(ctx.comparison());
        } else if (ctx.orExpression() != null) {
            // Parenthesized expression
            return visit(ctx.orExpression());
        }
        
        throw new IllegalStateException("Unknown primary expression type");
    }
    
    @Override
    public ASTNode visitComparison(RulesParser.ComparisonContext ctx) {
        ASTNode leftExpression = visit(ctx.expression(0));
        ComparisonNode.ComparisonOperator operator = parseOperator(ctx.operator());
        ASTNode rightExpression = visit(ctx.expression(1));
        
        return new ComparisonNode(leftExpression, operator, rightExpression);
    }
    
    @Override
    public ASTNode visitExpression(RulesParser.ExpressionContext ctx) {
        List<RulesParser.MultiplicativeExpressionContext> multiplicativeExpressions = ctx.multiplicativeExpression();
        
        if (multiplicativeExpressions.size() == 1) {
            // Single multiplicative expression, no addition/subtraction
            return visit(multiplicativeExpressions.get(0));
        }
        
        // Multiple multiplicative expressions connected by +/-
        ArithmeticExpressionNode arithmeticNode = new ArithmeticExpressionNode(visit(multiplicativeExpressions.get(0)));
        
        for (int i = 1; i < multiplicativeExpressions.size(); i++) {
            // Determine operator (+ or -)
            String operatorText = ctx.getChild(2 * i - 1).getText(); // Get operator token
            ArithmeticExpressionNode.ArithmeticOperator operator = 
                ArithmeticExpressionNode.ArithmeticOperator.fromString(operatorText);
            
            ASTNode operand = visit(multiplicativeExpressions.get(i));
            arithmeticNode.addOperand(operator, operand);
        }
        
        return arithmeticNode;
    }
    
    @Override
    public ASTNode visitMultiplicativeExpression(RulesParser.MultiplicativeExpressionContext ctx) {
        List<RulesParser.AtomicExpressionContext> atomicExpressions = ctx.atomicExpression();
        
        if (atomicExpressions.size() == 1) {
            // Single atomic expression, no multiplication/division
            return visit(atomicExpressions.get(0));
        }
        
        // Multiple atomic expressions connected by *,/,%
        MultiplicativeExpressionNode multiplicativeNode = new MultiplicativeExpressionNode(visit(atomicExpressions.get(0)));
        
        for (int i = 1; i < atomicExpressions.size(); i++) {
            // Determine operator (*, /, %)
            String operatorText = ctx.getChild(2 * i - 1).getText(); // Get operator token
            ArithmeticExpressionNode.ArithmeticOperator operator = 
                ArithmeticExpressionNode.ArithmeticOperator.fromString(operatorText);
            
            ASTNode operand = visit(atomicExpressions.get(i));
            multiplicativeNode.addOperand(operator, operand);
        }
        
        return multiplicativeNode;
    }
    
    @Override
    public ASTNode visitAtomicExpression(RulesParser.AtomicExpressionContext ctx) {
        if (ctx.attribute() != null) {
            return visit(ctx.attribute());
        } else if (ctx.value() != null) {
            return visit(ctx.value());
        } else if (ctx.expression() != null) {
            // Parenthesized expression
            return visit(ctx.expression());
        }
        
        throw new IllegalStateException("Unknown atomic expression type");
    }
    
    @Override
    public ASTNode visitAttribute(RulesParser.AttributeContext ctx) {
        List<String> pathElements = new ArrayList<>();
        
        for (int i = 0; i < ctx.IDENTIFIER().size(); i++) {
            pathElements.add(ctx.IDENTIFIER(i).getText());
        }
        
        return new AttributeNode(pathElements);
    }
    
    @Override
    public ASTNode visitValue(RulesParser.ValueContext ctx) {
        if (ctx.STRING() != null) {
            String text = ctx.STRING().getText();
            // Remove quotes
            String value = text.substring(1, text.length() - 1);
            // Handle escaped quotes
            value = value.replace("\"\"", "\"").replace("''", "'");
            return new ValueNode(value);
        } else if (ctx.NUMBER() != null) {
            String text = ctx.NUMBER().getText();
            if (text.contains(".") || text.contains("e") || text.contains("E")) {
                return new ValueNode(Double.parseDouble(text));
            } else {
                try {
                    return new ValueNode(Integer.parseInt(text));
                } catch (NumberFormatException e) {
                    return new ValueNode(Long.parseLong(text));
                }
            }
        } else if (ctx.BOOLEAN() != null) {
            String text = ctx.BOOLEAN().getText().toLowerCase();
            return new ValueNode(Boolean.parseBoolean(text));
        } else if (ctx.NULL() != null) {
            return ValueNode.nullValue();
        } else if (ctx.list() != null) {
            return visit(ctx.list());
        } else if (ctx.dateTime() != null) {
            return visit(ctx.dateTime());
        } else if (ctx.duration() != null) {
            return visit(ctx.duration());
        }
        
        throw new IllegalStateException("Unknown value type");
    }
    
    @Override
    public ASTNode visitList(RulesParser.ListContext ctx) {
        List<ValueNode> values = new ArrayList<>();
        
        if (ctx.value() != null) {
            for (RulesParser.ValueContext valueCtx : ctx.value()) {
                ValueNode value = (ValueNode) visit(valueCtx);
                values.add(value);
            }
        }
        
        return new ValueNode(values);
    }
    
    @Override
    public ASTNode visitAction(RulesParser.ActionContext ctx) {
        String actionName = ctx.IDENTIFIER().getText();
        return new ActionNode(actionName);
    }
    
    private ComparisonNode.ComparisonOperator parseOperator(RulesParser.OperatorContext ctx) {
        if (ctx.EQ() != null) {
            return ComparisonNode.ComparisonOperator.EQUALS;
        } else if (ctx.NE() != null) {
            return ComparisonNode.ComparisonOperator.NOT_EQUALS;
        } else if (ctx.LT() != null) {
            return ComparisonNode.ComparisonOperator.LESS_THAN;
        } else if (ctx.LE() != null) {
            return ComparisonNode.ComparisonOperator.LESS_THAN_OR_EQUAL;
        } else if (ctx.GT() != null) {
            return ComparisonNode.ComparisonOperator.GREATER_THAN;
        } else if (ctx.GE() != null) {
            return ComparisonNode.ComparisonOperator.GREATER_THAN_OR_EQUAL;
        } else if (ctx.CONTAINS() != null) {
            return ComparisonNode.ComparisonOperator.CONTAINS;
        } else if (ctx.STARTS_WITH() != null) {
            return ComparisonNode.ComparisonOperator.STARTS_WITH;
        } else if (ctx.ENDS_WITH() != null) {
            return ComparisonNode.ComparisonOperator.ENDS_WITH;
        } else if (ctx.IN() != null) {
            return ComparisonNode.ComparisonOperator.IN;
        } else if (ctx.NOT_IN() != null) {
            return ComparisonNode.ComparisonOperator.NOT_IN;
        } else if (ctx.IS_NULL() != null) {
            return ComparisonNode.ComparisonOperator.IS_NULL;
        } else if (ctx.IS_NOT_NULL() != null) {
            return ComparisonNode.ComparisonOperator.IS_NOT_NULL;
        } else if (ctx.MATCHES() != null) {
            return ComparisonNode.ComparisonOperator.MATCHES;
        } else if (ctx.BEFORE() != null) {
            return ComparisonNode.ComparisonOperator.BEFORE;
        } else if (ctx.AFTER() != null) {
            return ComparisonNode.ComparisonOperator.AFTER;
        } else if (ctx.BETWEEN() != null) {
            return ComparisonNode.ComparisonOperator.BETWEEN;
        } else if (ctx.WITHIN() != null) {
            return ComparisonNode.ComparisonOperator.WITHIN;
        }
        
        throw new IllegalStateException("Unknown operator");
    }
    
    @Override
    public ASTNode visitDateTimeExpression(RulesParser.DateTimeExpressionContext ctx) {
        ASTNode dateTime = visit(ctx.dateTime());
        
        if (ctx.duration() != null) {
            // Has arithmetic operation
            String operator = ctx.PLUS() != null ? "+" : "-";
            ASTNode duration = visit(ctx.duration());
            return new DateTimeExpressionNode(dateTime, operator, duration);
        } else {
            // Simple date/time expression
            return new DateTimeExpressionNode(dateTime);
        }
    }
    
    @Override
    public ASTNode visitDateTime(RulesParser.DateTimeContext ctx) {
        if (ctx.DATE_LITERAL() != null) {
            String text = ctx.DATE_LITERAL().getText();
            return new DateTimeLiteralNode(text, DateTimeLiteralNode.DateTimeType.DATE);
        } else if (ctx.DATETIME_LITERAL() != null) {
            String text = ctx.DATETIME_LITERAL().getText();
            return new DateTimeLiteralNode(text, DateTimeLiteralNode.DateTimeType.DATETIME);
        } else if (ctx.TIME_LITERAL() != null) {
            String text = ctx.TIME_LITERAL().getText();
            return new DateTimeLiteralNode(text, DateTimeLiteralNode.DateTimeType.TIME);
        } else if (ctx.NOW() != null) {
            return new DateTimeLiteralNode("NOW", DateTimeLiteralNode.DateTimeType.NOW);
        } else if (ctx.TODAY() != null) {
            return new DateTimeLiteralNode("TODAY", DateTimeLiteralNode.DateTimeType.TODAY);
        } else if (ctx.BUSINESS_DATE() != null) {
            return new DateTimeLiteralNode("BUSINESS_DATE", DateTimeLiteralNode.DateTimeType.BUSINESS_DATE);
        } else if (ctx.dateTimeFunction() != null) {
            return visit(ctx.dateTimeFunction());
        }
        
        throw new IllegalStateException("Unknown dateTime type");
    }
    
    @Override
    public ASTNode visitDuration(RulesParser.DurationContext ctx) {
        if (ctx.DURATION_LITERAL() != null) {
            String text = ctx.DURATION_LITERAL().getText();
            return new DurationNode(text);
        } else if (ctx.NUMBER() != null) {
            double value = Double.parseDouble(ctx.NUMBER().getText());
            
            DurationNode.DurationUnit unit;
            if (ctx.YEARS() != null) {
                unit = DurationNode.DurationUnit.YEARS;
            } else if (ctx.MONTHS() != null) {
                unit = DurationNode.DurationUnit.MONTHS;
            } else if (ctx.WEEKS() != null) {
                unit = DurationNode.DurationUnit.WEEKS;
            } else if (ctx.DAYS() != null) {
                unit = DurationNode.DurationUnit.DAYS;
            } else if (ctx.HOURS() != null) {
                unit = DurationNode.DurationUnit.HOURS;
            } else if (ctx.MINUTES() != null) {
                unit = DurationNode.DurationUnit.MINUTES;
            } else if (ctx.SECONDS() != null) {
                unit = DurationNode.DurationUnit.SECONDS;
            } else if (ctx.MILLIS() != null) {
                unit = DurationNode.DurationUnit.MILLIS;
            } else {
                throw new IllegalStateException("Unknown duration unit");
            }
            
            return new DurationNode(value, unit);
        }
        
        throw new IllegalStateException("Unknown duration type");
    }
    
    @Override
    public ASTNode visitDateTimeFunction(RulesParser.DateTimeFunctionContext ctx) {
        if (ctx.DATE_OF() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.DATE_OF, arg);
        } else if (ctx.TIME_OF() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.TIME_OF, arg);
        } else if (ctx.YEAR_OF() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.YEAR_OF, arg);
        } else if (ctx.MONTH_OF() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.MONTH_OF, arg);
        } else if (ctx.DAY_OF() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.DAY_OF, arg);
        } else if (ctx.HOUR_OF() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.HOUR_OF, arg);
        } else if (ctx.MINUTE_OF() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.MINUTE_OF, arg);
        } else if (ctx.SECOND_OF() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.SECOND_OF, arg);
        } else if (ctx.DAY_OF_WEEK() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.DAY_OF_WEEK, arg);
        } else if (ctx.DAY_OF_YEAR() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.DAY_OF_YEAR, arg);
        } else if (ctx.WEEK_OF_YEAR() != null) {
            ASTNode arg = visit(ctx.expression(0));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.WEEK_OF_YEAR, arg);
        } else if (ctx.PARSE_DATE() != null) {
            ASTNode arg1 = visit(ctx.expression(0));
            ASTNode arg2 = visit(ctx.expression(1));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.PARSE_DATE, arg1, arg2);
        } else if (ctx.FORMAT_DATE() != null) {
            ASTNode arg1 = visit(ctx.expression(0));
            ASTNode arg2 = visit(ctx.expression(1));
            return new DateTimeFunctionNode(DateTimeFunctionNode.FunctionType.FORMAT_DATE, arg1, arg2);
        }
        
        throw new IllegalStateException("Unknown dateTime function");
    }
}