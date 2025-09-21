# Standard Application Workflow Rule Library

This is a generated rule library JAR for rule: **Standard Application Workflow** (ID: `14`)

## Usage

This library is designed to be loaded into a main rules engine service, not run as a standalone microservice.

### Building the Library

```bash
mvn clean package
```

This will create:
- `target/14-rule-1.0.0.jar` - Main library JAR
- `target/14-rule-1.0.0-jar-with-dependencies.jar` - JAR with all dependencies

### Integration

Add this library to your main rules engine service classpath and invoke:

```java
import com.rules.generated.StandardApplicationWorkflowActionSet;
import com.rules.context.RuleContext;
import com.rules.runtime.RuleResult;

// Execute the rule
RuleContext context = new RuleContext(jsonData);
RuleResult result = StandardApplicationWorkflowActionSet.execute(context);

// Check result
if (result.wasExecuted()) {
    System.out.println("Rule executed successfully");
    if (result.hasActions()) {
        System.out.println("Actions: " + result.getActions());
    }
}
```

### Deployment

1. Build the JAR: `mvn clean package`
2. Copy the JAR to your rules engine service
3. Add it to the classpath
4. Register the rule with your engine

### Architecture

This follows the **library JAR** pattern rather than microservices:
- ✅ No network overhead between rules
- ✅ Easy to manage thousands of rules
- ✅ Simple deployment and versioning
- ✅ Centralized rule execution engine

## Generated Files

- `src/main/java/.../StandardApplicationWorkflowActionSet.java` - Rule implementation
- `pom.xml` - Maven build configuration
- `README.md` - This documentation
- `Dockerfile` - Container setup for rules engine service
- `rule-metadata.json` - Rule metadata
