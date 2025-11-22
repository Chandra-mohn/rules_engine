# Rules Engine Java Components

This directory contains the Java-based validation and execution engine for the Rules Engine UI, built with Maven and ANTLR.

## Architecture

- **Maven-based build system** with proper dependency management
- **ANTLR 4.13.1** for grammar-based rule parsing
- **JUnit 5** for comprehensive testing
- **Jackson** for JSON processing
- **Unified CLI** for validation and testing

## Project Structure

```
java-bridge/
├── pom.xml                          # Maven configuration
├── src/
│   ├── main/
│   │   ├── java/com/rules/cli/      # Java source code
│   │   │   ├── RuleValidator.java       # Basic validation
│   │   │   ├── RuleValidatorANTLR.java  # ANTLR-based validation  
│   │   │   ├── RuleTester.java          # Rule execution engine
│   │   │   └── RulesEngineCLI.java      # Main CLI entry point
│   │   └── antlr4/com/rules/grammar/
│   │       └── Rules.g4             # ANTLR grammar definition
│   └── test/
│       └── java/com/rules/cli/      # Unit tests
├── target/
│   ├── classes/                     # Compiled Java classes
│   ├── generated-sources/           # ANTLR-generated code
│   └── rules-engine-java-1.0.0.jar # Executable JAR
└── README.md                        # This file
```

## Build Commands

### Prerequisites
- **Java 17+** (required)
- **Maven 3.6+** (required)

### Development Workflow

```bash
# Quick clean build (recommended)
./build.sh

# Standard Maven commands
mvn clean compile    # Compile only
mvn test             # Run tests  
mvn package          # Build JAR
mvn clean            # Clean everything

# Clean build with minimal output
mvn package -q
```

### Production Build

```bash
# Build optimized production JAR
mvn clean package -P prod

# Skip tests during build (faster)
mvn package -DskipTests
```

## Usage

### Command Line Interface

```bash
# Show help
java -jar target/rules-engine-java-1.0.0.jar help

# Validate a rule file
java -jar target/rules-engine-java-1.0.0.jar validate my-rule.rules

# Test rule with sample data  
java -jar target/rules-engine-java-1.0.0.jar test my-rule.rules test-data.json
```

### Python Integration

The Python backend automatically uses the Maven JAR when available:

```python
from services.java_bridge import JavaBridge
from pathlib import Path

# Initialize bridge (automatically detects Maven JAR)
bridge = JavaBridge(Path('java-bridge'))

# Validate rule
result = bridge.validate_rule("rule test: if applicant.age >= 18 then approve")

# Test rule execution  
test_data = {"applicant": {"age": 25}}
result = bridge.test_rule(rule_content, test_data)
```

## Features

### Validation Engine
- **Dual validation**: Basic + ANTLR-based parsing
- **Semantic validation**: Entity/property checking
- **Typo detection**: Suggestions for common mistakes
- **Grammar enforcement**: Full syntax tree validation

### Testing Engine
- **Rule execution**: Evaluate rules against sample data
- **Action simulation**: Determine which actions would execute
- **Detailed results**: Condition-by-condition evaluation
- **JSON output**: Structured results for UI integration

### ANTLR Grammar
- **Complete DSL support**: Full business rules language
- **Entity.property syntax**: `applicant.creditScore >= 750`
- **Operators**: `>=`, `<=`, `!=`, `and`, `or`, `not`
- **Functions**: `business_date()`, `year_of()`, etc.
- **Actions**: `approveApplication`, `rejectApplication`, etc.

## Dependencies

- **ANTLR 4.13.1**: Grammar parsing and lexical analysis
- **Jackson 2.15.2**: JSON processing for data exchange
- **JUnit 5.10.0**: Unit testing framework

## Performance

- **Fast compilation**: Maven handles incremental builds
- **Optimized JAR**: Single executable with all dependencies
- **Memory efficient**: Proper resource management
- **Cross-platform**: Works on Windows/Mac/Linux

## Development

### Adding New Validators

1. Create validator class in `src/main/java/com/rules/cli/`
2. Add unit tests in `src/test/java/com/rules/cli/`
3. Update `RulesEngineCLI.java` for new command
4. Run `mvn test` to verify

### Modifying Grammar

1. Edit `src/main/antlr4/com/rules/grammar/Rules.g4`
2. Run `mvn compile` to regenerate parser
3. Update `RuleValidatorANTLR.java` for new grammar
4. Add tests for new syntax

### Testing

```bash
# Run all tests
mvn test

# Run specific test class  
mvn test -Dtest=RuleValidatorTest

# Run with verbose output
mvn test -X
```

## Troubleshooting

### Common Issues

1. **"Class not found" errors**
   - Solution: Run `mvn clean compile`

2. **ANTLR parsing errors**  
   - Solution: Check grammar syntax in `Rules.g4`

3. **Memory issues with large rules**
   - Solution: Increase JVM heap: `java -Xmx2g -jar ...`

4. **Windows path issues**
   - Solution: Use forward slashes in file paths

### Build Issues

```bash
# Clean and rebuild everything
mvn clean install -U

# Check dependency tree
mvn dependency:tree

# Validate project structure
mvn validate
```

## Integration with Rules Engine UI

The Maven build integrates seamlessly with the broader Rules Engine UI:

1. **Python Backend**: Automatically detects and uses Maven JAR
2. **React Frontend**: Receives validation results via API
3. **Development Workflow**: `mvn package` updates the executable
4. **CI/CD Ready**: Standard Maven build for automation

## Windows Setup

For Windows users with Git Bash:

```bash
# Download and install Maven
# Ensure JAVA_HOME is set

# Build the project
mvn clean package

# Test the JAR
java -jar target/rules-engine-java-1.0.0.jar help
```

The Python bridge handles Windows path conversion automatically.

---

This Maven-based approach provides a robust, scalable foundation for the Rules Engine's Java components with proper dependency management, testing, and cross-platform compatibility.