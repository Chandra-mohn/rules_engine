package com.rules.generated;

import com.rules.engine.Rule;
import com.rules.engine.RuleResult;
import com.rules.context.RuleContext;
import java.util.List;
import java.util.ArrayList;
import java.util.Objects;
import java.util.Arrays;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.time.temporal.WeekFields;
import java.util.Locale;

/**
 * Generated rule class for: applicationProcessing
 * 
 * This class is auto-generated from the rules DSL and should not be modified manually.
 * Any changes will be overwritten when rules are regenerated.
 * 
 * Generated on: 2025-08-26T09:47:34.518656
 * Generator version: 1.0.0
 * 
 * @author Rules Engine Code Generator
 * @version 1.0.0
 * @since 1.0.0
 */
@SuppressWarnings({"unused", "unchecked"})
public class ApplicationProcessingRule implements Rule {
    
    private static final String RULE_NAME = "applicationProcessing";
    
    @Override
    public RuleResult execute(RuleContext context) {
        long startTime = System.nanoTime();
        List<String> actions = new ArrayList<>();
        
        // Step: Step[condition=NotExpression[negated=false, operand=Comparison[left=Attribute[applicant.age], operator=>=, right=Value[type=NUMBER, value=18]]], then=Action[instantApproval]]
        if (compareNumbers(context.getValue("applicant.age"), 18) >= 0) {
            actions.add("instantApproval");
        }
        
        // Step: Step[condition=NotExpression[negated=false, operand=Comparison[left=Attribute[applicant.totalDebt], operator=>, right=Value[type=NUMBER, value=30000]]], then=Action[requireManualReview]]
        if (compareNumbers(context.getValue("applicant.totalDebt"), 30000) > 0) {
            actions.add("requireManualReview");
        }
        
        
        long executionTime = System.nanoTime() - startTime;
        return RuleResult.withTiming(RULE_NAME, actions, !actions.isEmpty(), executionTime);
    }
    
    @Override
    public String getRuleName() {
        return RULE_NAME;
    }
    
    // Utility methods for rule evaluation
    
    private static int compareNumbers(Object left, Object right) {
        if (left == null || right == null) return 0;
        if (left instanceof Number && right instanceof Number) {
            double leftVal = ((Number) left).doubleValue();
            double rightVal = ((Number) right).doubleValue();
            return Double.compare(leftVal, rightVal);
        }
        // Handle LocalDateTime comparisons
        if (left instanceof LocalDateTime && right instanceof LocalDateTime) {
            return ((LocalDateTime) left).compareTo((LocalDateTime) right);
        }
        // Handle mixed LocalDateTime vs String comparisons
        if (left instanceof LocalDateTime && !(right instanceof LocalDateTime)) {
            LocalDateTime rightDt = parseDateTime(right);
            if (rightDt != null) {
                return ((LocalDateTime) left).compareTo(rightDt);
            }
        }
        if (right instanceof LocalDateTime && !(left instanceof LocalDateTime)) {
            LocalDateTime leftDt = parseDateTime(left);
            if (leftDt != null) {
                return leftDt.compareTo((LocalDateTime) right);
            }
        }
        return 0;
    }
    
    private static Object evaluateArithmetic(Object... operands) {
        if (operands.length < 3 || (operands.length % 2 == 0 && operands.length != 4)) return operands[0];
        
        // Check if this is date arithmetic (has time unit as last operand)
        if (operands.length == 4 && operands[3] instanceof String) {
            String unit = (String) operands[3];
            if (isTimeUnit(unit)) {
                String operator = (String) operands[1];
                Object amount = operands[2];
                if ("+".equals(operator)) {
                    return addDuration(operands[0], amount, unit);
                } else if ("-".equals(operator)) {
                    return subtractDuration(operands[0], amount, unit);
                }
            }
        }
        
        // Regular numeric arithmetic
        double result = toDouble(operands[0]);
        for (int i = 1; i < operands.length; i += 2) {
            String operator = (String) operands[i];
            double operand = toDouble(operands[i + 1]);
            switch (operator) {
                case "+": result += operand; break;
                case "-": result -= operand; break;
                case "*": result *= operand; break;
                case "/": result = operand != 0 ? result / operand : 0; break;
                case "%": result = operand != 0 ? result % operand : 0; break;
            }
        }
        
        // Return as integer if result is a whole number
        if (result == Math.floor(result)) {
            return (int) result;
        } else {
            return result;
        }
    }
    
    private static double toDouble(Object value) {
        if (value instanceof Number) {
            return ((Number) value).doubleValue();
        }
        return 0.0;
    }
    
    private static boolean isTimeUnit(String unit) {
        if (unit == null) return false;
        String lowerUnit = unit.toLowerCase();
        return lowerUnit.equals("years") || lowerUnit.equals("year") ||
               lowerUnit.equals("months") || lowerUnit.equals("month") ||
               lowerUnit.equals("weeks") || lowerUnit.equals("week") ||
               lowerUnit.equals("days") || lowerUnit.equals("day") ||
               lowerUnit.equals("hours") || lowerUnit.equals("hour") ||
               lowerUnit.equals("minutes") || lowerUnit.equals("minute") ||
               lowerUnit.equals("seconds") || lowerUnit.equals("second");
    }
    
    private static boolean containsValue(Object left, Object right) {
        if (left == null || right == null) return false;
        return left.toString().contains(right.toString());
    }
    
    private static boolean startsWithValue(Object left, Object right) {
        if (left == null || right == null) return false;
        return left.toString().startsWith(right.toString());
    }
    
    private static boolean endsWithValue(Object left, Object right) {
        if (left == null || right == null) return false;
        return left.toString().endsWith(right.toString());
    }
    
    @SuppressWarnings("unchecked")
    private static boolean inList(Object value, Object list) {
        if (list instanceof List) {
            return ((List<Object>) list).contains(value);
        }
        return false;
    }
    
    private static boolean matchesPattern(Object value, Object pattern) {
        if (value == null || pattern == null) return false;
        try {
            return value.toString().matches(pattern.toString());
        } catch (Exception e) {
            return false;
        }
    }
    
    // Date/Time Utility Methods
    
    private static LocalDateTime parseDateTime(Object value) {
        if (value == null) return null;
        String str = value.toString();
        try {
            // Try ISO format first
            if (str.contains("T")) {
                // Handle ISO format with Z suffix
                if (str.endsWith("Z")) {
                    return LocalDateTime.parse(str, DateTimeFormatter.ISO_DATE_TIME).atZone(ZoneId.of("UTC")).toLocalDateTime();
                } else {
                    return LocalDateTime.parse(str, DateTimeFormatter.ISO_LOCAL_DATE_TIME);
                }
            }
            // Try date only
            if (str.matches("\\d{4}-\\d{2}-\\d{2}")) {
                return LocalDate.parse(str).atStartOfDay();
            }
            // Try timestamp
            if (str.matches("\\d+")) {
                return LocalDateTime.ofInstant(Instant.ofEpochMilli(Long.parseLong(str)), ZoneId.systemDefault());
            }
        } catch (Exception e) {
            // Return null for invalid dates
        }
        return null;
    }
    
    private static LocalDateTime getCurrentDateTime() {
        return LocalDateTime.now();
    }
    
    private static LocalDateTime getTodayAtMidnight() {
        return LocalDate.now().atStartOfDay();
    }
    
    private static LocalDateTime getBusinessDate() {
        // Mock implementation: returns current business date
        // In production, this would integrate with a business calendar service
        LocalDate today = LocalDate.now();
        DayOfWeek dayOfWeek = today.getDayOfWeek();
        
        // If today is Saturday, return Friday
        if (dayOfWeek == DayOfWeek.SATURDAY) {
            return today.minusDays(1).atStartOfDay();
        }
        // If today is Sunday, return Friday
        if (dayOfWeek == DayOfWeek.SUNDAY) {
            return today.minusDays(2).atStartOfDay();
        }
        // For weekdays, return today
        // TODO: Add holiday calendar integration
        return today.atStartOfDay();
    }
    
    private static int compareDateTimes(Object left, Object right) {
        LocalDateTime leftDt = parseDateTime(left);
        LocalDateTime rightDt = parseDateTime(right);
        if (leftDt == null || rightDt == null) return 0;
        return leftDt.compareTo(rightDt);
    }
    
    private static boolean isDateBefore(Object date, Object reference) {
        return compareDateTimes(date, reference) < 0;
    }
    
    private static boolean isDateAfter(Object date, Object reference) {
        return compareDateTimes(date, reference) > 0;
    }
    
    private static LocalDateTime addDuration(Object dateTime, Object amount, String unit) {
        LocalDateTime dt = parseDateTime(dateTime);
        if (dt == null) return null;
        long value = (long) toDouble(amount);
        switch (unit.toLowerCase()) {
            case "years": case "year": return dt.plusYears(value);
            case "months": case "month": return dt.plusMonths(value);
            case "weeks": case "week": return dt.plusWeeks(value);
            case "days": case "day": return dt.plusDays(value);
            case "hours": case "hour": return dt.plusHours(value);
            case "minutes": case "minute": return dt.plusMinutes(value);
            case "seconds": case "second": return dt.plusSeconds(value);
            default: return dt;
        }
    }
    
    private static LocalDateTime subtractDuration(Object dateTime, Object amount, String unit) {
        LocalDateTime dt = parseDateTime(dateTime);
        if (dt == null) return null;
        long value = (long) toDouble(amount);
        switch (unit.toLowerCase()) {
            case "years": case "year": return dt.minusYears(value);
            case "months": case "month": return dt.minusMonths(value);
            case "weeks": case "week": return dt.minusWeeks(value);
            case "days": case "day": return dt.minusDays(value);
            case "hours": case "hour": return dt.minusHours(value);
            case "minutes": case "minute": return dt.minusMinutes(value);
            case "seconds": case "second": return dt.minusSeconds(value);
            default: return dt;
        }
    }
    
    private static int getYear(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        return dt != null ? dt.getYear() : 0;
    }
    
    private static int getMonth(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        return dt != null ? dt.getMonthValue() : 0;
    }
    
    private static int getDay(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        return dt != null ? dt.getDayOfMonth() : 0;
    }
    
    private static int getHour(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        return dt != null ? dt.getHour() : 0;
    }
    
    private static int getMinute(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        return dt != null ? dt.getMinute() : 0;
    }
    
    private static int getSecond(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        return dt != null ? dt.getSecond() : 0;
    }
    
    private static int getDayOfWeek(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        return dt != null ? dt.getDayOfWeek().getValue() : 0;
    }
    
    private static int getDayOfYear(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        return dt != null ? dt.getDayOfYear() : 0;
    }
    
    private static int getWeekOfYear(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        if (dt == null) return 0;
        WeekFields weekFields = WeekFields.of(Locale.getDefault());
        return dt.get(weekFields.weekOfWeekBasedYear());
    }
    
    private static long getAgeInYears(Object birthDate) {
        LocalDateTime birth = parseDateTime(birthDate);
        if (birth == null) return 0;
        return ChronoUnit.YEARS.between(birth.toLocalDate(), LocalDate.now());
    }
    
    private static long getAgeInMonths(Object birthDate) {
        LocalDateTime birth = parseDateTime(birthDate);
        if (birth == null) return 0;
        return ChronoUnit.MONTHS.between(birth.toLocalDate(), LocalDate.now());
    }
    
    private static long getAgeInDays(Object birthDate) {
        LocalDateTime birth = parseDateTime(birthDate);
        if (birth == null) return 0;
        return ChronoUnit.DAYS.between(birth.toLocalDate(), LocalDate.now());
    }
    
    private static boolean isWeekend(Object dateTime) {
        LocalDateTime dt = parseDateTime(dateTime);
        if (dt == null) return false;
        DayOfWeek day = dt.getDayOfWeek();
        return day == DayOfWeek.SATURDAY || day == DayOfWeek.SUNDAY;
    }
    
    private static boolean isWeekday(Object dateTime) {
        return !isWeekend(dateTime);
    }
    
    private static String formatDate(Object dateTime, String pattern) {
        LocalDateTime dt = parseDateTime(dateTime);
        if (dt == null) return "";
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(pattern);
            return dt.format(formatter);
        } catch (Exception e) {
            return dt.toString();
        }
    }
}


