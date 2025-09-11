# Generated Rules Directory

This directory contains **auto-generated Java classes** from the Rules Engine DSL.

## ⚠️ **Important Notice**

**DO NOT MODIFY THESE FILES MANUALLY**

All files in this directory are automatically generated from `.rules` files and will be **overwritten** whenever rules are recompiled.

## 📁 **File Structure**

```
generated/
├── README.md                    # This file
├── [RuleName]Rule.java         # Generated rule classes
└── ...                         # Additional generated files
```

## 🔄 **Generation Process**

1. **Source**: Rules defined in `.rules` files (e.g., `sample_rules_demo.rules`)
2. **Parser**: ANTLR grammar (`Rules.g4`) parses DSL into AST
3. **Generator**: `JavaCodeGenerator` converts AST to Java source code
4. **Compiler**: `RuleCompiler` compiles Java source to bytecode
5. **Output**: Executable rule classes in this directory

## 🎯 **Generated Class Structure**

Each rule generates a class like:

```java
@SuppressWarnings({"unused", "unchecked"})
public class AgeVerificationRule implements Rule {
    
    private static final String RULE_NAME = "ageVerification";
    
    @Override
    public RuleResult execute(RuleContext context) {
        // Generated rule logic
    }
    
    @Override
    public String getName() {
        return RULE_NAME;
    }
}
```

## 🚀 **Production Deployment**

These generated files are:
- ✅ **Version controlled** (committed to git)
- ✅ **Production ready** (optimized bytecode)
- ✅ **Self-documenting** (comprehensive JavaDoc)
- ✅ **Type safe** (compile-time validation)

## 🔧 **Regeneration**

To regenerate these files:

```bash
# Run any test that loads rules
mvn test

# Or use the rules engine programmatically
RulesEngine engine = new RulesEngine(actionRegistry);
engine.loadRules(rulesDSL);  // Triggers regeneration
```

## 📊 **Metadata**

- **Generator**: Rules Engine Code Generator v1.0.0
- **Target Package**: `com.rules.generated`
- **Base Interface**: `com.rules.engine.Rule`
- **Context Type**: `com.rules.context.RuleContext`