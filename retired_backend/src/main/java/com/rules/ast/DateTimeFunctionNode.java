package com.rules.ast;

import java.util.List;
import java.util.Objects;
import java.util.ArrayList;

/**
 * AST node representing date/time functions.
 * Supports functions like: year_of(date), parse_date(string, format), etc.
 */
public class DateTimeFunctionNode extends ASTNode {
    
    public enum FunctionType {
        DATE_OF,        // Extract date part
        TIME_OF,        // Extract time part
        YEAR_OF,        // Extract year
        MONTH_OF,       // Extract month
        DAY_OF,         // Extract day
        HOUR_OF,        // Extract hour
        MINUTE_OF,      // Extract minute
        SECOND_OF,      // Extract second
        DAY_OF_WEEK,    // Day of week (1-7)
        DAY_OF_YEAR,    // Day of year (1-366)
        WEEK_OF_YEAR,   // Week of year (1-53)
        PARSE_DATE,     // Parse string to date
        FORMAT_DATE,    // Format date to string
        AGE_YEARS,      // Calculate age in years
        AGE_MONTHS,     // Calculate age in months
        AGE_DAYS,       // Calculate age in days
        IS_WEEKEND,     // Check if date is weekend
        IS_WEEKDAY,     // Check if date is weekday
        IS_HOLIDAY      // Check if date is holiday
    }
    
    private final FunctionType functionType;
    private final List<ASTNode> arguments;
    
    public DateTimeFunctionNode(FunctionType functionType, List<ASTNode> arguments) {
        this.functionType = Objects.requireNonNull(functionType, "Function type cannot be null");
        this.arguments = new ArrayList<>(Objects.requireNonNull(arguments, "Arguments cannot be null"));
    }
    
    public DateTimeFunctionNode(FunctionType functionType, ASTNode argument) {
        this.functionType = Objects.requireNonNull(functionType, "Function type cannot be null");
        this.arguments = new ArrayList<>();
        this.arguments.add(Objects.requireNonNull(argument, "Argument cannot be null"));
    }
    
    public DateTimeFunctionNode(FunctionType functionType, ASTNode arg1, ASTNode arg2) {
        this.functionType = Objects.requireNonNull(functionType, "Function type cannot be null");
        this.arguments = new ArrayList<>();
        this.arguments.add(Objects.requireNonNull(arg1, "First argument cannot be null"));
        this.arguments.add(Objects.requireNonNull(arg2, "Second argument cannot be null"));
    }
    
    public FunctionType getFunctionType() {
        return functionType;
    }
    
    public List<ASTNode> getArguments() {
        return new ArrayList<>(arguments);
    }
    
    public ASTNode getArgument(int index) {
        if (index < 0 || index >= arguments.size()) {
            throw new IndexOutOfBoundsException("Argument index out of bounds: " + index);
        }
        return arguments.get(index);
    }
    
    public int getArgumentCount() {
        return arguments.size();
    }
    
    @Override
    public <T> T accept(ASTVisitor<T> visitor) {
        return visitor.visitDateTimeFunction(this);
    }
    
    @Override
    public NodeType getNodeType() {
        return NodeType.DATETIME_FUNCTION;
    }
    
    @Override
    public String toString() {
        return String.format("DateTimeFunction[%s(%s)]", 
                           functionType, 
                           arguments.size());
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        DateTimeFunctionNode that = (DateTimeFunctionNode) obj;
        return functionType == that.functionType && 
               Objects.equals(arguments, that.arguments);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(functionType, arguments);
    }
}