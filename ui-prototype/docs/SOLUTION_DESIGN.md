# Rules Engine UI - End-to-End Solution Design

## Overview

This document describes the complete architecture and design of the Rules Authoring System, a web-based application that enables business users to create, validate, test, and manage business rules using a domain-specific language (DSL).

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         Frontend (React)                        │
├─────────────────────────────────────────────────────────────────┤
│  • Rule Editor (Monaco)    • Sample Data Editor                │
│  • Rules List Management   • Schema Viewer                     │
│  • Test Results Display    • Validation Feedback               │
└─────────────────┬───────────────────────────────────────────────┘
                  │ HTTP/JSON API
┌─────────────────▼───────────────────────────────────────────────┐
│                      Backend (Flask)                           │
├─────────────────────────────────────────────────────────────────┤
│  • REST API Endpoints      • Rule CRUD Operations              │
│  • Java Bridge Service     • Sample Data Management            │
│  • Database Integration    • Error Handling                    │
└─────────────────┬───────────────────────────────────────────────┘
                  │ Subprocess Calls
┌─────────────────▼───────────────────────────────────────────────┐
│                   Java Validation Layer                        │
├─────────────────────────────────────────────────────────────────┤
│  • RuleValidator (Basic)    • RuleTester (Execution Engine)    │
│  • RuleValidatorANTLR       • ANTLR Grammar Parser             │
│  • Semantic Validation     • Action Execution                  │
└─────────────────┬───────────────────────────────────────────────┘
                  │ File I/O
┌─────────────────▼───────────────────────────────────────────────┐
│                       Data Layer                               │
├─────────────────────────────────────────────────────────────────┤
│  • SQLite Database         • Rule Storage                       │
│  • Sample Data Storage     • Configuration Files               │
│  • Temporary Rule Files    • ANTLR Runtime JARs                │
└─────────────────────────────────────────────────────────────────┘
```

## Component Details

### 1. Frontend Layer (React)

#### **Rule Editor Component** (`RuleEditor.jsx`)
- **Purpose**: Primary interface for rule creation and editing
- **Technology**: Monaco Editor with custom language support
- **Features**:
  - Syntax highlighting for custom DSL
  - Real-time validation feedback
  - Auto-completion suggestions
  - Error highlighting and tooltips
  - Rule saving and loading

#### **Sample Data Editor Component** (`SampleDataEditor.jsx`)
- **Purpose**: Create and manage test datasets
- **Features**:
  - JSON editor with validation
  - Pre-defined data templates
  - Multiple test scenarios
  - Data format validation

#### **Rules List Component** (`RulesList.jsx`)
- **Purpose**: Browse and manage existing rules
- **Features**:
  - Rule listing with metadata
  - Search and filter capabilities
  - Rule selection for editing
  - Rule deletion and duplication

#### **Schema Viewer Component** (`SchemaViewer.jsx`)
- **Purpose**: Display available attributes and actions
- **Features**:
  - Entity relationship display
  - Attribute documentation
  - Action reference guide
  - Usage examples

### 2. Backend Layer (Flask)

#### **API Endpoints** (`api/rules.py`)
```
GET    /api/rules              # List all rules
POST   /api/rules              # Create new rule
GET    /api/rules/{id}         # Get specific rule
PUT    /api/rules/{id}         # Update rule
DELETE /api/rules/{id}         # Delete rule
POST   /api/rules/validate     # Validate rule syntax
POST   /api/rules/test         # Test rule with sample data
GET    /api/schema             # Get schema information
```

#### **Java Bridge Service** (`services/java_bridge.py`)
- **Purpose**: Interface between Python backend and Java validation
- **Key Methods**:
  - `validate_rule()`: Syntax and semantic validation
  - `test_rule()`: Execute rule against sample data
  - `get_autocomplete_suggestions()`: IDE-like suggestions

#### **Rule Service** (`services/rule_service.py`)
- **Purpose**: Business logic for rule management
- **Responsibilities**:
  - Rule CRUD operations
  - Database interactions
  - Data validation
  - Error handling

### 3. Java Validation Layer

#### **Basic Rule Validator** (`RuleValidator.java`)
- **Purpose**: Lightweight validation without ANTLR dependencies
- **Features**:
  - Basic syntax checking
  - Keyword validation
  - Entity/attribute validation
  - Action validation
  - Typo detection with suggestions

#### **ANTLR-Based Validator** (`RuleValidatorANTLR.java`)
- **Purpose**: Advanced parsing with full grammar support
- **Features**:
  - Complete syntax tree parsing
  - Semantic analysis
  - Context-aware error messages
  - Grammar rule enforcement

#### **Rule Tester** (`RuleTester.java`)
- **Purpose**: Execute rules against test data
- **Features**:
  - Rule evaluation engine
  - Action execution simulation
  - Result formatting
  - Performance metrics

#### **ANTLR Grammar Files**
- `RulesLexer.java`: Tokenization
- `RulesParser.java`: Grammar parsing
- `RulesListener.java`: Parse tree walking
- `RulesBaseListener.java`: Event handling

### 4. Data Layer

#### **Database Schema** (SQLite)
```sql
-- Rules table
CREATE TABLE rules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR(255) NOT NULL UNIQUE,
    description TEXT,
    content TEXT NOT NULL,
    is_active BOOLEAN DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Sample data table
CREATE TABLE sample_data (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    rule_id INTEGER,
    name VARCHAR(255) NOT NULL,
    data_json TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (rule_id) REFERENCES rules (id)
);
```

#### **Configuration Schema** (`schema/rules_schema.py`)
- Entity definitions (applicant, transaction, account)
- Attribute specifications with types
- Action definitions with parameters
- Function library
- Validation rules

## Domain-Specific Language (DSL)

### **Grammar Structure**
```
rule <name>:
    if <condition> then <action>
    if <condition> then <action>
    ...

<condition> := <attribute> <operator> <value>
             | <condition> and <condition>
             | <condition> or <condition>
             | not <condition>
             | (<condition>)

<attribute> := <entity>.<property>
             | <function>(<parameters>)

<action> := <actionName>
          | <actionName>(<parameters>)
```

### **Example Rule**
```
rule creditScoreCheck:
    if applicant.creditScore >= 750 then approveApplication
    if applicant.creditScore < 600 then rejectApplication
    if applicant.age < 18 then rejectApplication
```

## Data Flow

### **Rule Creation Flow**
1. User types rule in Monaco editor
2. Real-time syntax validation via Java bridge
3. Auto-completion suggestions from schema
4. Rule saved to database on user action
5. Validation results displayed in UI

### **Rule Testing Flow**
1. User selects rule and sample data
2. Frontend sends test request to backend
3. Backend calls Java RuleTester
4. Java executes rule logic
5. Results returned through chain
6. UI displays execution results and actions

### **Validation Pipeline**
```
Rule Content → Basic Validation → ANTLR Parsing → Semantic Analysis → Result
                     ↓                ↓              ↓
                Syntax Check    Grammar Parse   Entity/Action
                Typo Detection  Tree Building   Validation
```

## Technology Stack

### **Frontend**
- **React 18**: Component framework
- **Monaco Editor**: Code editor with IntelliSense
- **Axios**: HTTP client for API calls
- **CSS Modules**: Scoped styling

### **Backend**
- **Flask 2.3**: Web framework
- **SQLAlchemy**: ORM for database operations
- **Flask-CORS**: Cross-origin resource sharing
- **Python 3.8+**: Runtime environment

### **Java Layer**
- **Java 17**: Runtime and compilation
- **ANTLR 4.13**: Parser generator
- **JSON Processing**: Data serialization

### **Database**
- **SQLite**: Embedded database
- **Schema versioning**: Migration support

## Deployment Architecture

### **Development Environment**
```bash
# Frontend (React Dev Server)
npm start → localhost:3000

# Backend (Flask Dev Server)  
python app.py → localhost:5000

# Java Bridge (Compiled Classes)
java-bridge/classes/ → Local execution
```

### **Production Considerations**
- **Frontend**: Static build served by nginx/Apache
- **Backend**: WSGI server (Gunicorn/uWSGI)
- **Database**: PostgreSQL/MySQL for production
- **Java**: Containerized execution environment
- **Monitoring**: Application and performance metrics

## Security Considerations

### **Input Validation**
- Rule content sanitization
- SQL injection prevention
- File path validation for temp files
- JSON schema validation

### **Access Control**
- API authentication (future enhancement)
- Role-based rule management
- Audit logging for rule changes

### **Execution Safety**
- Sandboxed Java execution
- Timeout controls for rule testing
- Resource usage limits

## Performance Considerations

### **Frontend Optimization**
- Code splitting for large rule sets
- Debounced validation requests
- Virtual scrolling for rule lists
- Caching of schema information

### **Backend Optimization**
- Connection pooling
- Query optimization
- Async processing for long operations
- Response caching

### **Java Layer Optimization**
- JVM warmup considerations
- Process reuse vs. isolation
- Memory management for large rule sets
- Compilation caching

## Monitoring and Observability

### **Metrics to Track**
- Rule validation response times
- Test execution performance
- Error rates by component
- User interaction patterns

### **Logging Strategy**
- Structured logging (JSON format)
- Request/response correlation IDs
- Rule execution audit trails
- Performance benchmarks

### **Health Checks**
- API endpoint availability
- Database connectivity
- Java bridge functionality
- File system permissions

## Extension Points

### **Future Enhancements**
1. **Rule Templates**: Pre-built rule patterns
2. **Version Control**: Rule change history
3. **Import/Export**: Rule set management
4. **Advanced Testing**: A/B testing framework
5. **Performance Analytics**: Rule execution metrics
6. **Integration APIs**: External system connectivity
7. **Multi-tenancy**: Organization-based rule isolation
8. **Rule Scheduling**: Time-based rule activation

### **Customization Options**
- Custom entity types
- Additional action implementations
- Extended function library
- Custom validation rules
- Theme and UI customization

## Development Guidelines

### **Code Organization**
- Component-based architecture
- Clear separation of concerns
- Consistent naming conventions
- Comprehensive error handling

### **Testing Strategy**
- Unit tests for business logic
- Integration tests for API endpoints
- E2E tests for user workflows
- Performance tests for rule execution

### **Documentation Standards**
- API documentation (OpenAPI/Swagger)
- Component documentation (JSDoc)
- User guides and tutorials
- Developer setup instructions

This design document serves as the authoritative reference for the Rules Engine UI system architecture and implementation details.