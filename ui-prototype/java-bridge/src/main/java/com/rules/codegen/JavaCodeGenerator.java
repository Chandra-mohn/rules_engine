package com.rules.codegen;

import com.rules.ast.*;
import java.util.List;

/**
 * Generates optimized Java code from AST.
 * Implements the visitor pattern to traverse AST and generate executable Java code.
 */
public class JavaCodeGenerator implements ASTVisitor<String> {
    
    private final StringBuilder code = new StringBuilder();
    private int indentLevel = 0;
    
    /**
     * Generate Java code for a rule set.
     * 
     * @param ruleSet The rule set AST
     * @return Generated Java code
     */
    public String generateCode(RuleSetNode ruleSet) {
        code.setLength(0);
        indentLevel = 0;
        
        visit(ruleSet);
        return code.toString();
    }
    
    @Override
    public String visitRuleSet(RuleSetNode node) {
        // Generate individual rule classes
        for (NamedRuleNode rule : node.getRules()) {
            generateRuleClass(rule);
            code.append("\n\n");
        }
        
        return code.toString();
    }
    
    private void generateRuleClass(NamedRuleNode rule) {
        String className = toClassName(rule.getRuleName());
        
        // Package and imports
        appendLine("package com.rules.generated;");
        appendLine("");
        appendLine("import com.rules.engine.Rule;");
        appendLine("import com.rules.engine.RuleResult;");
        appendLine("import com.rules.context.RuleContext;");
        appendLine("import java.util.List;");
        appendLine("import java.util.ArrayList;");
        appendLine("import java.util.Objects;");
        appendLine("import java.util.Arrays;");
        appendLine("import java.time.*;");
        appendLine("import java.time.format.DateTimeFormatter;");
        appendLine("import java.time.temporal.ChronoUnit;");
        appendLine("import java.time.temporal.WeekFields;");
        appendLine("import java.util.Locale;");
        appendLine("");
        
        // Class declaration with comprehensive documentation
        appendLine("/**");
        appendLine(" * Generated rule class for: " + rule.getRuleName());
        appendLine(" * ");
        appendLine(" * This class is auto-generated from the rules DSL and should not be modified manually.");
        appendLine(" * Any changes will be overwritten when rules are regenerated.");
        appendLine(" * ");
        appendLine(" * Generated on: " + java.time.LocalDateTime.now().toString());
        appendLine(" * Generator version: 1.0.0");
        appendLine(" * ");
        appendLine(" * @author Rules Engine Code Generator");
        appendLine(" * @version 1.0.0");
        appendLine(" * @since 1.0.0");
        appendLine(" */");
        appendLine("@SuppressWarnings({\"unused\", \"unchecked\"})");
        appendLine("public class " + className + " implements Rule {");
        
        indentLevel++;
        
        // Rule name constant
        appendLine("");
        appendLine("private static final String RULE_NAME = \"" + rule.getRuleName() + "\";");
        
        // Execute method
        appendLine("");
        appendLine("@Override");
        appendLine("public RuleResult execute(RuleContext context) {");
        indentLevel++;
        
        appendLine("long startTime = System.nanoTime();");
        appendLine("List<String> actions = new ArrayList<>();");
        appendLine("");
        
        // Generate step logic
        for (StepNode step : rule.getSteps()) {
            generateStep(step);
        }
        
        appendLine("");
        appendLine("long executionTime = System.nanoTime() - startTime;");
        appendLine("return RuleResult.withTiming(RULE_NAME, actions, !actions.isEmpty(), executionTime);");
        
        indentLevel--;
        appendLine("}");
        
        // Rule name method
        appendLine("");
        appendLine("@Override");
        appendLine("public String getRuleName() {");
        indentLevel++;
        appendLine("return RULE_NAME;");
        indentLevel--;
        appendLine("}");
        
        // Add utility methods
        generateUtilityMethods();
        
        indentLevel--;
        appendLine("}");
    }
    
    private void generateStep(StepNode step) {
        appendLine("// Step: " + step.toString());
        append("if (");
        
        // Generate condition
        String condition = visit(step.getCondition());
        append(condition);
        
        appendLine(") {");
        indentLevel++;
        
        // Generate then action
        String thenAction = step.getThenAction().getActionName();
        appendLine("actions.add(\"" + thenAction + "\");");
        
        indentLevel--;
        
        // Generate else action if present
        if (step.hasElseAction()) {
            appendLine("} else {");
            indentLevel++;
            String elseAction = step.getElseAction().getActionName();
            appendLine("actions.add(\"" + elseAction + "\");");
            indentLevel--;
        }
        
        appendLine("}");
        appendLine("");
    }
    
    @Override
    public String visitNamedRule(NamedRuleNode node) {
        // This is handled by generateRuleClass
        return "";
    }
    
    @Override
    public String visitStep(StepNode node) {
        // This is handled by generateStep
        return "";
    }
    
    @Override
    public String visitOrExpression(OrExpressionNode node) {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        
        for (int i = 0; i < node.getOperands().size(); i++) {
            if (i > 0) {
                sb.append(" || ");
            }
            sb.append(visit(node.getOperands().get(i)));
        }
        
        sb.append(")");
        return sb.toString();
    }
    
    @Override
    public String visitAndExpression(AndExpressionNode node) {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        
        for (int i = 0; i < node.getOperands().size(); i++) {
            if (i > 0) {
                sb.append(" && ");
            }
            sb.append(visit(node.getOperands().get(i)));
        }
        
        sb.append(")");
        return sb.toString();
    }
    
    @Override
    public String visitNotExpression(NotExpressionNode node) {
        StringBuilder sb = new StringBuilder();
        
        if (node.isNegated()) {
            sb.append("!(");
            sb.append(visit(node.getOperand()));
            sb.append(")");
        } else {
            sb.append(visit(node.getOperand()));
        }
        
        return sb.toString();
    }
    
    @Override
    public String visitComparison(ComparisonNode node) {
        StringBuilder sb = new StringBuilder();
        
        String leftSide = visit(node.getLeftExpression());
        String operator = generateOperator(node.getOperator());
        String rightSide = visit(node.getRightExpression());
        
        // Handle different operator types
        switch (node.getOperator()) {
            case EQUALS:
            case NOT_EQUALS:
                sb.append("Objects.equals(").append(leftSide).append(", ").append(rightSide).append(")");
                if (node.getOperator() == ComparisonNode.ComparisonOperator.NOT_EQUALS) {
                    sb.insert(0, "!");
                }
                break;
                
            case LESS_THAN:
            case LESS_THAN_OR_EQUAL:
            case GREATER_THAN:
            case GREATER_THAN_OR_EQUAL:
                sb.append("compareNumbers(").append(leftSide).append(", ").append(rightSide).append(") ");
                sb.append(operator).append(" 0");
                break;
                
            case CONTAINS:
                sb.append("containsValue(").append(leftSide).append(", ").append(rightSide).append(")");
                break;
                
            case STARTS_WITH:
                sb.append("startsWithValue(").append(leftSide).append(", ").append(rightSide).append(")");
                break;
                
            case ENDS_WITH:
                sb.append("endsWithValue(").append(leftSide).append(", ").append(rightSide).append(")");
                break;
                
            case IN:
                sb.append("inList(").append(leftSide).append(", ").append(rightSide).append(")");
                break;
                
            case NOT_IN:
                sb.append("!inList(").append(leftSide).append(", ").append(rightSide).append(")");
                break;
                
            case IS_NULL:
                sb.append(leftSide).append(" == null");
                break;
                
            case IS_NOT_NULL:
                sb.append(leftSide).append(" != null");
                break;
                
            case MATCHES:
                sb.append("matchesPattern(").append(leftSide).append(", ").append(rightSide).append(")");
                break;
                
            // Date/Time operators
            case BEFORE:
                sb.append("isDateBefore(").append(leftSide).append(", ").append(rightSide).append(")");
                break;
                
            case AFTER:
                sb.append("isDateAfter(").append(leftSide).append(", ").append(rightSide).append(")");
                break;
                
            case IS_WEEKEND:
                sb.append("isWeekend(").append(leftSide).append(")");
                break;
                
            case IS_WEEKDAY:
                sb.append("isWeekday(").append(leftSide).append(")");
                break;
                
            case AGE_YEARS:
                sb.append("getAgeInYears(").append(leftSide).append(") ").append(operator).append(" ").append(rightSide);
                break;
                
            case AGE_MONTHS:
                sb.append("getAgeInMonths(").append(leftSide).append(") ").append(operator).append(" ").append(rightSide);
                break;
                
            case AGE_DAYS:
                sb.append("getAgeInDays(").append(leftSide).append(") ").append(operator).append(" ").append(rightSide);
                break;
                
            // TODO: Implement BETWEEN, WITHIN, IS_HOLIDAY
            case BETWEEN:
            case WITHIN:
            case IS_HOLIDAY:
                sb.append("false"); // Placeholder
                break;
        }
        
        return sb.toString();
    }
    
    @Override
    public String visitArithmeticExpression(ArithmeticExpressionNode node) {
        if (node.isSimple()) {
            return visit(node.getSimpleOperand());
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        
        List<ASTNode> operands = node.getOperands();
        List<ArithmeticExpressionNode.ArithmeticOperator> operators = node.getOperators();
        
        sb.append("evaluateArithmetic(").append(visit(operands.get(0)));
        
        for (int i = 0; i < operators.size(); i++) {
            sb.append(", \"").append(operators.get(i).getSymbol()).append("\", ");
            sb.append(visit(operands.get(i + 1)));
        }
        
        sb.append("))");
        return sb.toString();
    }
    
    @Override
    public String visitMultiplicativeExpression(MultiplicativeExpressionNode node) {
        List<ASTNode> operands = node.getOperands();
        List<ArithmeticExpressionNode.ArithmeticOperator> operators = node.getOperators();
        
        if (operands.size() == 1) {
            return visit(operands.get(0));
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append("evaluateArithmetic(").append(visit(operands.get(0)));
        
        for (int i = 0; i < operators.size(); i++) {
            sb.append(", \"").append(operators.get(i).getSymbol()).append("\"");
            sb.append(", ").append(visit(operands.get(i + 1)));
        }
        
        sb.append(")");
        return sb.toString();
    }
    
    @Override
    public String visitDateTimeExpression(DateTimeExpressionNode node) {
        if (!node.hasOperation()) {
            return visit(node.getDateTime());
        }
        
        String dateTimeExpr = visit(node.getDateTime());
        String durationExpr = visit(node.getDuration());
        String operator = node.getOperator();
        
        if ("+".equals(operator)) {
            return String.format("addDuration(%s, %s)", dateTimeExpr, durationExpr);
        } else if ("-".equals(operator)) {
            return String.format("subtractDuration(%s, %s)", dateTimeExpr, durationExpr);
        }
        
        return dateTimeExpr;
    }
    
    @Override
    public String visitDateTimeLiteral(DateTimeLiteralNode node) {
        switch (node.getType()) {
            case NOW:
                return "getCurrentDateTime()";
            case TODAY:
                return "getTodayAtMidnight()";
            case BUSINESS_DATE:
                return "getBusinessDate()";
            case DATE:
            case TIME:
            case DATETIME:
                return String.format("parseDateTime(\"%s\")", node.getValue());
            default:
                return "null";
        }
    }
    
    @Override
    public String visitDateTimeFunction(DateTimeFunctionNode node) {
        switch (node.getFunctionType()) {
            case YEAR_OF:
                return String.format("getYear(%s)", visit(node.getArgument(0)));
            case MONTH_OF:
                return String.format("getMonth(%s)", visit(node.getArgument(0)));
            case DAY_OF:
                return String.format("getDay(%s)", visit(node.getArgument(0)));
            case HOUR_OF:
                return String.format("getHour(%s)", visit(node.getArgument(0)));
            case MINUTE_OF:
                return String.format("getMinute(%s)", visit(node.getArgument(0)));
            case SECOND_OF:
                return String.format("getSecond(%s)", visit(node.getArgument(0)));
            case DAY_OF_WEEK:
                return String.format("getDayOfWeek(%s)", visit(node.getArgument(0)));
            case DAY_OF_YEAR:
                return String.format("getDayOfYear(%s)", visit(node.getArgument(0)));
            case WEEK_OF_YEAR:
                return String.format("getWeekOfYear(%s)", visit(node.getArgument(0)));
            case PARSE_DATE:
                return String.format("parseDateTime(%s)", visit(node.getArgument(0)));
            case FORMAT_DATE:
                return String.format("formatDate(%s, %s)", 
                                   visit(node.getArgument(0)), 
                                   visit(node.getArgument(1)));
            case AGE_YEARS:
                return String.format("getAgeInYears(%s)", visit(node.getArgument(0)));
            case AGE_MONTHS:
                return String.format("getAgeInMonths(%s)", visit(node.getArgument(0)));
            case AGE_DAYS:
                return String.format("getAgeInDays(%s)", visit(node.getArgument(0)));
            case IS_WEEKEND:
                return String.format("isWeekend(%s)", visit(node.getArgument(0)));
            case IS_WEEKDAY:
                return String.format("isWeekday(%s)", visit(node.getArgument(0)));
            case IS_HOLIDAY:
                return String.format("isHoliday(%s)", visit(node.getArgument(0)));
            default:
                return "null";
        }
    }
    
    @Override
    public String visitDuration(DurationNode node) {
        if (node.isLiteral()) {
            return String.format("\"%s\"", node.getLiteralValue());
        } else {
            return String.format("%.2f, \"%s\"", node.getValue(), node.getUnit().getUnitName());
        }
    }
    
    @Override
    public String visitAttribute(AttributeNode node) {
        return generateAttributeAccess(node);
    }
    
    @Override
    public String visitValue(ValueNode node) {
        switch (node.getType()) {
            case STRING:
                return "\"" + escapeString(node.getStringValue()) + "\"";
            case NUMBER:
                return node.getNumberValue().toString();
            case BOOLEAN:
                return node.getBooleanValue().toString();
            case NULL:
                return "null";
            case LIST:
                StringBuilder sb = new StringBuilder();
                sb.append("Arrays.asList(");
                List<ValueNode> values = node.getListValue();
                for (int i = 0; i < values.size(); i++) {
                    if (i > 0) sb.append(", ");
                    sb.append(visit(values.get(i)));
                }
                sb.append(")");
                return sb.toString();
            default:
                return "null";
        }
    }
    
    @Override
    public String visitAction(ActionNode node) {
        return "\"" + node.getActionName() + "\"";
    }
    
    private String visit(ASTNode node) {
        return node.accept(this);
    }
    
    private void appendLine(String line) {
        append(line);
        code.append("\n");
    }
    
    private void append(String text) {
        if (code.length() == 0 || code.charAt(code.length() - 1) == '\n') {
            // Add indentation
            for (int i = 0; i < indentLevel; i++) {
                code.append("    ");
            }
        }
        code.append(text);
    }
    
    private String toClassName(String ruleName) {
        // Convert rule name to valid Java class name
        StringBuilder sb = new StringBuilder();
        boolean capitalizeNext = true;
        
        for (char c : ruleName.toCharArray()) {
            if (Character.isLetterOrDigit(c)) {
                if (capitalizeNext) {
                    sb.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    sb.append(c);
                }
            } else {
                capitalizeNext = true;
            }
        }
        
        sb.append("Rule");
        return sb.toString();
    }
    
    private String escapeString(String str) {
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }
    
    private void generateUtilityMethods() {
        appendLine("");
        appendLine("// Utility methods for rule evaluation");
        
        // Number comparison method
        appendLine("");
        appendLine("private static int compareNumbers(Object left, Object right) {");
        indentLevel++;
        appendLine("if (left == null || right == null) return 0;");
        appendLine("if (left instanceof Number && right instanceof Number) {");
        indentLevel++;
        appendLine("double leftVal = ((Number) left).doubleValue();");
        appendLine("double rightVal = ((Number) right).doubleValue();");
        appendLine("return Double.compare(leftVal, rightVal);");
        indentLevel--;
        appendLine("}");
        appendLine("// Handle LocalDateTime comparisons");
        appendLine("if (left instanceof LocalDateTime && right instanceof LocalDateTime) {");
        indentLevel++;
        appendLine("return ((LocalDateTime) left).compareTo((LocalDateTime) right);");
        indentLevel--;
        appendLine("}");
        appendLine("// Handle mixed LocalDateTime vs String comparisons");
        appendLine("if (left instanceof LocalDateTime && !(right instanceof LocalDateTime)) {");
        indentLevel++;
        appendLine("LocalDateTime rightDt = parseDateTime(right);");
        appendLine("if (rightDt != null) {");
        indentLevel++;
        appendLine("return ((LocalDateTime) left).compareTo(rightDt);");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        appendLine("if (right instanceof LocalDateTime && !(left instanceof LocalDateTime)) {");
        indentLevel++;
        appendLine("LocalDateTime leftDt = parseDateTime(left);");
        appendLine("if (leftDt != null) {");
        indentLevel++;
        appendLine("return leftDt.compareTo((LocalDateTime) right);");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        appendLine("return 0;");
        indentLevel--;
        appendLine("}");
        
        // Arithmetic evaluation method
        appendLine("");
        appendLine("private static Object evaluateArithmetic(Object... operands) {");
        indentLevel++;
        appendLine("if (operands.length < 3 || (operands.length % 2 == 0 && operands.length != 4)) return operands[0];");
        appendLine("");
        appendLine("// Check if this is date arithmetic (has time unit as last operand)");
        appendLine("if (operands.length == 4 && operands[3] instanceof String) {");
        indentLevel++;
        appendLine("String unit = (String) operands[3];");
        appendLine("if (isTimeUnit(unit)) {");
        indentLevel++;
        appendLine("String operator = (String) operands[1];");
        appendLine("Object amount = operands[2];");
        appendLine("if (\"+\".equals(operator)) {");
        indentLevel++;
        appendLine("return addDuration(operands[0], amount, unit);");
        indentLevel--;
        appendLine("} else if (\"-\".equals(operator)) {");
        indentLevel++;
        appendLine("return subtractDuration(operands[0], amount, unit);");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        appendLine("");
        appendLine("// Regular numeric arithmetic");
        appendLine("double result = toDouble(operands[0]);");
        appendLine("for (int i = 1; i < operands.length; i += 2) {");
        indentLevel++;
        appendLine("String operator = (String) operands[i];");
        appendLine("double operand = toDouble(operands[i + 1]);");
        appendLine("switch (operator) {");
        indentLevel++;
        appendLine("case \"+\": result += operand; break;");
        appendLine("case \"-\": result -= operand; break;");
        appendLine("case \"*\": result *= operand; break;");
        appendLine("case \"/\": result = operand != 0 ? result / operand : 0; break;");
        appendLine("case \"%\": result = operand != 0 ? result % operand : 0; break;");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        appendLine("");
        appendLine("// Return as integer if result is a whole number");
        appendLine("if (result == Math.floor(result)) {");
        indentLevel++;
        appendLine("return (int) result;");
        indentLevel--;
        appendLine("} else {");
        indentLevel++;
        appendLine("return result;");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        
        // Helper method to convert to double
        appendLine("");
        appendLine("private static double toDouble(Object value) {");
        indentLevel++;
        appendLine("if (value instanceof Number) {");
        indentLevel++;
        appendLine("return ((Number) value).doubleValue();");
        indentLevel--;
        appendLine("}");
        appendLine("return 0.0;");
        indentLevel--;
        appendLine("}");
        
        // Helper method to check if string is a time unit
        appendLine("");
        appendLine("private static boolean isTimeUnit(String unit) {");
        indentLevel++;
        appendLine("if (unit == null) return false;");
        appendLine("String lowerUnit = unit.toLowerCase();");
        appendLine("return lowerUnit.equals(\"years\") || lowerUnit.equals(\"year\") ||");
        appendLine("       lowerUnit.equals(\"months\") || lowerUnit.equals(\"month\") ||");
        appendLine("       lowerUnit.equals(\"weeks\") || lowerUnit.equals(\"week\") ||");
        appendLine("       lowerUnit.equals(\"days\") || lowerUnit.equals(\"day\") ||");
        appendLine("       lowerUnit.equals(\"hours\") || lowerUnit.equals(\"hour\") ||");
        appendLine("       lowerUnit.equals(\"minutes\") || lowerUnit.equals(\"minute\") ||");
        appendLine("       lowerUnit.equals(\"seconds\") || lowerUnit.equals(\"second\");");
        indentLevel--;
        appendLine("}");
        
        // String operations
        appendLine("");
        appendLine("private static boolean containsValue(Object left, Object right) {");
        indentLevel++;
        appendLine("if (left == null || right == null) return false;");
        appendLine("return left.toString().contains(right.toString());");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static boolean startsWithValue(Object left, Object right) {");
        indentLevel++;
        appendLine("if (left == null || right == null) return false;");
        appendLine("return left.toString().startsWith(right.toString());");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static boolean endsWithValue(Object left, Object right) {");
        indentLevel++;
        appendLine("if (left == null || right == null) return false;");
        appendLine("return left.toString().endsWith(right.toString());");
        indentLevel--;
        appendLine("}");
        
        // List operations
        appendLine("");
        appendLine("@SuppressWarnings(\"unchecked\")");
        appendLine("private static boolean inList(Object value, Object list) {");
        indentLevel++;
        appendLine("if (list instanceof List) {");
        indentLevel++;
        appendLine("return ((List<Object>) list).contains(value);");
        indentLevel--;
        appendLine("}");
        appendLine("return false;");
        indentLevel--;
        appendLine("}");
        
        // Pattern matching
        appendLine("");
        appendLine("private static boolean matchesPattern(Object value, Object pattern) {");
        indentLevel++;
        appendLine("if (value == null || pattern == null) return false;");
        appendLine("try {");
        indentLevel++;
        appendLine("return value.toString().matches(pattern.toString());");
        indentLevel--;
        appendLine("} catch (Exception e) {");
        indentLevel++;
        appendLine("return false;");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        
        // Date/Time utility methods
        generateDateTimeUtilityMethods();
    }
    
    /**
     * Generate comprehensive date/time utility methods.
     */
    private void generateDateTimeUtilityMethods() {
        // Date/Time parsing and formatting
        appendLine("");
        appendLine("// Date/Time Utility Methods");
        appendLine("");
        appendLine("private static LocalDateTime parseDateTime(Object value) {");
        indentLevel++;
        appendLine("if (value == null) return null;");
        appendLine("String str = value.toString();");
        appendLine("try {");
        indentLevel++;
        appendLine("// Try ISO format first");
        appendLine("if (str.contains(\"T\")) {");
        indentLevel++;
        appendLine("// Handle ISO format with Z suffix");
        appendLine("if (str.endsWith(\"Z\")) {");
        indentLevel++;
        appendLine("return LocalDateTime.parse(str, DateTimeFormatter.ISO_DATE_TIME).atZone(ZoneId.of(\"UTC\")).toLocalDateTime();");
        indentLevel--;
        appendLine("} else {");
        indentLevel++;
        appendLine("return LocalDateTime.parse(str, DateTimeFormatter.ISO_LOCAL_DATE_TIME);");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        appendLine("// Try date only");
        appendLine("if (str.matches(\"\\\\d{4}-\\\\d{2}-\\\\d{2}\")) {");
        indentLevel++;
        appendLine("return LocalDate.parse(str).atStartOfDay();");
        indentLevel--;
        appendLine("}");
        appendLine("// Try timestamp");
        appendLine("if (str.matches(\"\\\\d+\")) {");
        indentLevel++;
        appendLine("return LocalDateTime.ofInstant(Instant.ofEpochMilli(Long.parseLong(str)), ZoneId.systemDefault());");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("} catch (Exception e) {");
        indentLevel++;
        appendLine("// Return null for invalid dates");
        indentLevel--;
        appendLine("}");
        appendLine("return null;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static LocalDateTime getCurrentDateTime() {");
        indentLevel++;
        appendLine("return LocalDateTime.now();");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static LocalDateTime getTodayAtMidnight() {");
        indentLevel++;
        appendLine("return LocalDate.now().atStartOfDay();");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static LocalDateTime getBusinessDate() {");
        indentLevel++;
        appendLine("// Mock implementation: returns current business date");
        appendLine("// In production, this would integrate with a business calendar service");
        appendLine("LocalDate today = LocalDate.now();");
        appendLine("DayOfWeek dayOfWeek = today.getDayOfWeek();");
        appendLine("");
        appendLine("// If today is Saturday, return Friday");
        appendLine("if (dayOfWeek == DayOfWeek.SATURDAY) {");
        indentLevel++;
        appendLine("return today.minusDays(1).atStartOfDay();");
        indentLevel--;
        appendLine("}");
        appendLine("// If today is Sunday, return Friday");
        appendLine("if (dayOfWeek == DayOfWeek.SUNDAY) {");
        indentLevel++;
        appendLine("return today.minusDays(2).atStartOfDay();");
        indentLevel--;
        appendLine("}");
        appendLine("// For weekdays, return today");
        appendLine("// TODO: Add holiday calendar integration");
        appendLine("return today.atStartOfDay();");
        indentLevel--;
        appendLine("}");
        
        // Date/Time comparison methods
        appendLine("");
        appendLine("private static int compareDateTimes(Object left, Object right) {");
        indentLevel++;
        appendLine("LocalDateTime leftDt = parseDateTime(left);");
        appendLine("LocalDateTime rightDt = parseDateTime(right);");
        appendLine("if (leftDt == null || rightDt == null) return 0;");
        appendLine("return leftDt.compareTo(rightDt);");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static boolean isDateBefore(Object date, Object reference) {");
        indentLevel++;
        appendLine("return compareDateTimes(date, reference) < 0;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static boolean isDateAfter(Object date, Object reference) {");
        indentLevel++;
        appendLine("return compareDateTimes(date, reference) > 0;");
        indentLevel--;
        appendLine("}");
        
        // Date/Time arithmetic
        appendLine("");
        appendLine("private static LocalDateTime addDuration(Object dateTime, Object amount, String unit) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("if (dt == null) return null;");
        appendLine("long value = (long) toDouble(amount);");
        appendLine("switch (unit.toLowerCase()) {");
        indentLevel++;
        appendLine("case \"years\": case \"year\": return dt.plusYears(value);");
        appendLine("case \"months\": case \"month\": return dt.plusMonths(value);");
        appendLine("case \"weeks\": case \"week\": return dt.plusWeeks(value);");
        appendLine("case \"days\": case \"day\": return dt.plusDays(value);");
        appendLine("case \"hours\": case \"hour\": return dt.plusHours(value);");
        appendLine("case \"minutes\": case \"minute\": return dt.plusMinutes(value);");
        appendLine("case \"seconds\": case \"second\": return dt.plusSeconds(value);");
        appendLine("default: return dt;");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static LocalDateTime subtractDuration(Object dateTime, Object amount, String unit) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("if (dt == null) return null;");
        appendLine("long value = (long) toDouble(amount);");
        appendLine("switch (unit.toLowerCase()) {");
        indentLevel++;
        appendLine("case \"years\": case \"year\": return dt.minusYears(value);");
        appendLine("case \"months\": case \"month\": return dt.minusMonths(value);");
        appendLine("case \"weeks\": case \"week\": return dt.minusWeeks(value);");
        appendLine("case \"days\": case \"day\": return dt.minusDays(value);");
        appendLine("case \"hours\": case \"hour\": return dt.minusHours(value);");
        appendLine("case \"minutes\": case \"minute\": return dt.minusMinutes(value);");
        appendLine("case \"seconds\": case \"second\": return dt.minusSeconds(value);");
        appendLine("default: return dt;");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
        
        // Date/Time extraction functions
        appendLine("");
        appendLine("private static int getYear(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("return dt != null ? dt.getYear() : 0;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static int getMonth(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("return dt != null ? dt.getMonthValue() : 0;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static int getDay(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("return dt != null ? dt.getDayOfMonth() : 0;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static int getHour(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("return dt != null ? dt.getHour() : 0;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static int getMinute(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("return dt != null ? dt.getMinute() : 0;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static int getSecond(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("return dt != null ? dt.getSecond() : 0;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static int getDayOfWeek(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("return dt != null ? dt.getDayOfWeek().getValue() : 0;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static int getDayOfYear(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("return dt != null ? dt.getDayOfYear() : 0;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static int getWeekOfYear(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("if (dt == null) return 0;");
        appendLine("WeekFields weekFields = WeekFields.of(Locale.getDefault());");
        appendLine("return dt.get(weekFields.weekOfWeekBasedYear());");
        indentLevel--;
        appendLine("}");
        
        // Age calculation methods
        appendLine("");
        appendLine("private static long getAgeInYears(Object birthDate) {");
        indentLevel++;
        appendLine("LocalDateTime birth = parseDateTime(birthDate);");
        appendLine("if (birth == null) return 0;");
        appendLine("return ChronoUnit.YEARS.between(birth.toLocalDate(), LocalDate.now());");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static long getAgeInMonths(Object birthDate) {");
        indentLevel++;
        appendLine("LocalDateTime birth = parseDateTime(birthDate);");
        appendLine("if (birth == null) return 0;");
        appendLine("return ChronoUnit.MONTHS.between(birth.toLocalDate(), LocalDate.now());");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static long getAgeInDays(Object birthDate) {");
        indentLevel++;
        appendLine("LocalDateTime birth = parseDateTime(birthDate);");
        appendLine("if (birth == null) return 0;");
        appendLine("return ChronoUnit.DAYS.between(birth.toLocalDate(), LocalDate.now());");
        indentLevel--;
        appendLine("}");
        
        // Weekend/weekday checks
        appendLine("");
        appendLine("private static boolean isWeekend(Object dateTime) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("if (dt == null) return false;");
        appendLine("DayOfWeek day = dt.getDayOfWeek();");
        appendLine("return day == DayOfWeek.SATURDAY || day == DayOfWeek.SUNDAY;");
        indentLevel--;
        appendLine("}");
        
        appendLine("");
        appendLine("private static boolean isWeekday(Object dateTime) {");
        indentLevel++;
        appendLine("return !isWeekend(dateTime);");
        indentLevel--;
        appendLine("}");
        
        // Date formatting
        appendLine("");
        appendLine("private static String formatDate(Object dateTime, String pattern) {");
        indentLevel++;
        appendLine("LocalDateTime dt = parseDateTime(dateTime);");
        appendLine("if (dt == null) return \"\";");
        appendLine("try {");
        indentLevel++;
        appendLine("DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern);");
        appendLine("return dt.format(formatter);");
        indentLevel--;
        appendLine("} catch (Exception e) {");
        indentLevel++;
        appendLine("return dt.toString();");
        indentLevel--;
        appendLine("}");
        indentLevel--;
        appendLine("}");
    }
    
    /**
     * Generate operator string for comparison operators.
     */
    private String generateOperator(ComparisonNode.ComparisonOperator operator) {
        switch (operator) {
            case EQUALS:
                return "==";
            case NOT_EQUALS:
                return "!=";
            case LESS_THAN:
                return "<";
            case LESS_THAN_OR_EQUAL:
                return "<=";
            case GREATER_THAN:
                return ">";
            case GREATER_THAN_OR_EQUAL:
                return ">=";
            case CONTAINS:
                return "contains";
            case STARTS_WITH:
                return "startsWith";
            case ENDS_WITH:
                return "endsWith";
            case IN:
                return "in";
            case MATCHES:
                return "matches";
            case BEFORE:
                return "before";
            case AFTER:
                return "after";
            case IS_WEEKEND:
                return "isWeekend";
            default:
                throw new IllegalArgumentException("Unknown operator: " + operator);
        }
    }
    
    /**
     * Generate attribute access code.
     */
    private String generateAttributeAccess(AttributeNode node) {
        StringBuilder sb = new StringBuilder();
        sb.append("context.getValue(\"").append(node.getPathString()).append("\")");
        return sb.toString();
    }
}