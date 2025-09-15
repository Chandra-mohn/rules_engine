# Architecture Consolidation Plan: Java Server → Python Backend

## Problem Statement
Current dual-server architecture (Python backend + Java server) introduces:
- CORS complexity between frontend and multiple backends
- Deployment and operational overhead
- Configuration management issues (multiple API URLs)
- Environment variable mismatches

## Proposed Solution: Consolidated Python Backend

### Architecture Changes
**Current:** Frontend → Python Backend (CRUD) + Java Server (validation/compilation/execution)
**Proposed:** Frontend → Python Backend (CRUD + validation + compilation + execution via subprocess)

### Technical Implementation Plan

#### Phase 1: Move Validation + Code Generation to Python
1. **ANTLR Integration in Python**
   - Install `antlr4-python3-runtime`
   - Port existing ANTLR grammar to Python parser
   - Implement rule validation using Python ANTLR runtime

2. **Java Code Generation**
   - Use plain Python string manipulation (NOT Jinja2 templates)
   - Port existing `DirectJavaCodeGenerator` logic to Python
   - Example approach:
   ```python
   def generate_java_class(rule_name, conditions, actions):
       java_code = f"""
   package com.rules.generated;

   public class {rule_name}Rule implements Rule {{
       @Override
       public RuleResult execute(RuleContext ctx) {{
           {generate_conditions(conditions)}
           {generate_actions(actions)}
           return RuleResult.noMatch();
       }}
   }}
   """
       return java_code
   ```

#### Phase 2: Subprocess-based Compilation & Execution
1. **Maven Integration**
   - Keep Maven for dependency management
   - Extract classpath: `mvn dependency:build-classpath`
   - Use Maven-managed JAR for runtime dependencies

2. **Compilation Pipeline**
   ```python
   def compile_java_rule(java_source, rule_name):
       # Write source to temp file
       # Call: javac -cp <maven_classpath> RuleClass.java
       # Return compilation success/errors
   ```

3. **Execution Pipeline**
   ```python
   def execute_java_rule(class_name, test_data):
       # Call: java -cp <maven_classpath> RuleClass
       # Parse execution results
       # Return structured response
   ```

### Benefits
✅ **Simplified Architecture**: Single backend server
✅ **No CORS Issues**: All API calls to one server
✅ **Easier Deployment**: One service instead of two
✅ **Unified Configuration**: Single API URL
✅ **Maven Integration**: Proper Java dependency management
✅ **Plain Python**: No template engine complexity
✅ **Subprocess Reliability**: Clean separation, no classloader issues

### Performance Considerations
- **Authoring Time**: 100-500ms compilation + execution acceptable
- **Production Time**: Generated Java code still optimized for sub-millisecond execution
- **Subprocess Overhead**: 50-100ms negligible for authoring workflow

### Implementation Steps
1. Install Python ANTLR runtime in backend
2. Port ANTLR grammar and validation logic
3. Implement plain Python code generation
4. Create subprocess compilation pipeline
5. Create subprocess execution pipeline
6. Update frontend to use only Python backend APIs
7. Remove Java server entirely
8. Update environment configuration

### Files to Modify
- `backend/requirements.txt` - Add ANTLR dependency
- `backend/services/` - Add new validation/compilation services
- `backend/app.py` - Add new API endpoints
- `frontend/.env` - Remove REACT_APP_JAVA_API_URL
- `frontend/src/services/api.js` - Remove javaApi client

### Migration Strategy
1. Implement new Python services alongside existing Java server
2. Update frontend to use Python endpoints
3. Test thoroughly with existing rules
4. Remove Java server once confident
5. Clean up configuration and documentation

## Decision Rationale
- User preference for simplified architecture
- Acceptable performance trade-offs for authoring use case
- Maven integration maintains Java ecosystem benefits
- Plain Python preferred over template engines
- Subprocess approach provides clean separation and reliability

## Future Considerations
- Monitor performance during heavy rule testing
- Consider optimization if compilation becomes bottleneck
- Evaluate production deployment patterns for generated Java code