# Rules Engine Backend Integration Guide

**Version**: 1.0
**Last Updated**: 2025-10-23
**Audience**: Backend Developers, System Integrators, DevOps Engineers

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Installation & Setup](#installation--setup)
3. [API Reference](#api-reference)
4. [Rules Processing](#rules-processing)
5. [Context Management](#context-management)
6. [Schema Management](#schema-management)
7. [Code Generation](#code-generation)
8. [Integration Patterns](#integration-patterns)
9. [Performance Optimization](#performance-optimization)
10. [Security Considerations](#security-considerations)
11. [Deployment](#deployment)
12. [Troubleshooting](#troubleshooting)

---

## Architecture Overview

### System Components

```
┌─────────────────┐       ┌─────────────────┐       ┌─────────────────┐
│  React Frontend │──────▶│  Flask Backend  │──────▶│  Java Engine    │
│  (Port 3000)    │ HTTP  │  (Port 5001)    │ CLI   │  (ANTLR Parser) │
└─────────────────┘       └─────────────────┘       └─────────────────┘
                                    │
                                    ▼
                          ┌─────────────────┐
                          │   File Storage  │
                          │   (JSON-based)  │
                          └─────────────────┘
```

### Technology Stack

| Component | Technology | Purpose |
|-----------|------------|---------|
| **API Server** | Flask 2.3.3 | REST API endpoints |
| **Data Storage** | File-based (JSON) | Rules, contexts, schemas |
| **ORM** | SQLAlchemy 3.0.5 | Data modeling (legacy support) |
| **Parser** | ANTLR 4.13.1 | Grammar parsing and validation |
| **Code Generator** | Java 17 | Bytecode generation |
| **Build Tool** | Maven 3.x | Java compilation and testing |

### Directory Structure

```
backend/
├── app.py                      # Application entry point
├── config.py                   # Configuration management
├── requirements.txt            # Python dependencies
├── models.py                   # Data models
├── schema/
│   └── rules_schema.py         # Data validation schemas
├── api/
│   ├── rules.py                # Rules REST endpoints
│   ├── contexts.py             # Context management endpoints
│   └── schema.py               # Schema endpoints
├── services/
│   ├── rule_service.py         # Business logic for rules
│   ├── java_bridge.py          # Java engine integration
│   ├── context_service.py      # Context management
│   └── schema_service.py       # Schema operations
├── sample_java_actions/        # Example action implementations
└── data/
    ├── rules/                  # Rules storage (JSON)
    ├── contexts/               # Test contexts (JSON)
    ├── schemas/                # Schema definitions (JSON)
    └── hierarchy/              # Organizational hierarchy (JSON)
```

---

## Installation & Setup

### Prerequisites

- Python 3.8 or higher
- Java 17 or higher (JDK)
- Maven 3.6 or higher
- Git

### Backend Setup

```bash
# Clone repository
cd /path/to/rules_engine/ui-prototype/backend

# Create virtual environment
python -m venv venv

# Activate virtual environment
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Verify installation
python -c "import flask; print(flask.__version__)"
```

### Java Bridge Setup

```bash
# Navigate to Java bridge
cd ../java-bridge

# Compile ANTLR grammar and Java code
mvn clean compile

# Package shaded JAR with all dependencies
mvn package

# Verify JAR creation
ls -lh target/rules-engine-*.jar
```

### Configuration

Edit `backend/config.py`:

```python
class Config:
    # API Configuration
    API_HOST = '0.0.0.0'
    API_PORT = 5001
    DEBUG = True  # Set to False in production

    # File Storage Paths (absolute paths)
    BASE_DIR = '/Users/chandramohn/workspace/rules_engine/ui-prototype'
    RULES_DIR = os.path.join(BASE_DIR, 'backend/data/rules')
    CONTEXTS_DIR = os.path.join(BASE_DIR, 'backend/data/contexts')
    SCHEMAS_DIR = os.path.join(BASE_DIR, 'backend/data/schemas')

    # Java Bridge Configuration
    JAVA_BRIDGE_JAR = os.path.join(BASE_DIR, 'java-bridge/target/rules-engine-1.0-SNAPSHOT.jar')
    JAVA_HOME = '/path/to/jdk-17'  # Set your Java path

    # Code Generation Output
    GENERATED_RULES_DIR = os.path.join(BASE_DIR, 'generated-rules')

    # Performance Settings
    MAX_RULES_PER_PAGE = 100
    DEFAULT_PAGE_SIZE = 50
    VALIDATION_TIMEOUT_MS = 5000
    COMPILATION_TIMEOUT_MS = 10000
```

### Running the Backend

```bash
# From backend directory
python app.py

# Server starts on http://localhost:5001
# Health check: http://localhost:5001/health
```

Expected output:
```
 * Running on http://0.0.0.0:5001
 * Debug mode: on
```

---

## API Reference

### Base URL

```
http://localhost:5001/api
```

### Authentication

Currently, the API does not require authentication. For production deployment, implement:
- JWT tokens
- API keys
- OAuth2

### Error Response Format

All errors return JSON:

```json
{
  "error": "Error description",
  "details": "Additional context",
  "status": 400
}
```

### Rules API

#### List All Rules

```http
GET /api/rules
```

**Query Parameters**:
- `page` (int, default: 1): Page number
- `limit` (int, default: 50): Items per page
- `process_area_id` (string): Filter by process area
- `status` (string): Filter by status (DRAFT, VALID, PROD, etc.)
- `item_type` (string): Filter by type (rule, action, actionset)

**Response** (200 OK):
```json
{
  "rules": [
    {
      "id": "rule-12345",
      "name": "creditApprovalRule",
      "description": "Approve applications with high credit scores",
      "item_type": "rule",
      "status": "VALID",
      "content": "rule creditApprovalRule:\n    if applicant.creditScore >= 700 then approve",
      "validation_status": "valid",
      "process_area_id": "CREDIT_DECISIONING",
      "context_id": "test_context_1",
      "created_at": "2023-09-17T10:30:00",
      "updated_at": "2023-09-17T14:20:00"
    }
  ],
  "pagination": {
    "page": 1,
    "limit": 50,
    "total_rules": 125,
    "total_pages": 3
  }
}
```

#### Get Single Rule

```http
GET /api/rules/{rule_id}
```

**Response** (200 OK):
```json
{
  "id": "rule-12345",
  "name": "creditApprovalRule",
  "content": "rule creditApprovalRule:\n    if applicant.creditScore >= 700 then approve",
  "description": "Approve high credit score applications",
  "item_type": "rule",
  "status": "VALID",
  "validation_status": "valid",
  "validation_message": "Rule is valid and ready for deployment",
  "process_area_id": "CREDIT_DECISIONING",
  "context_id": "test_context_1",
  "schema_version": "modern",
  "hierarchy": {
    "client_code": "CC_BANK",
    "process_group_code": "APPLICATIONS",
    "process_area_code": "CREDIT_DECISIONING"
  }
}
```

#### Create New Rule

```http
POST /api/rules
Content-Type: application/json
```

**Request Body**:
```json
{
  "name": "newApprovalRule",
  "content": "rule newApprovalRule:\n    if condition then action",
  "description": "Rule description",
  "process_area_id": "CREDIT_DECISIONING",
  "context_id": "test_context_1",
  "status": "DRAFT",
  "item_type": "rule"
}
```

**Response** (201 Created):
```json
{
  "id": "rule-67890",
  "name": "newApprovalRule",
  "status": "DRAFT",
  "validation": {
    "valid": true,
    "message": "Rule syntax is valid"
  }
}
```

**Error Responses**:
- `409 Conflict`: Rule with same name already exists
- `400 Bad Request`: Invalid syntax or missing required fields
- `400 Bad Request` (Cyclic Dependency): Rule creates circular dependency

#### Update Existing Rule

```http
PUT /api/rules/{rule_id}
Content-Type: application/json
```

**Request Body**:
```json
{
  "content": "rule updatedRule:\n    if newCondition then newAction",
  "description": "Updated description",
  "status": "VALID"
}
```

**Response** (200 OK):
```json
{
  "id": "rule-12345",
  "name": "updatedRule",
  "status": "VALID",
  "validation": {
    "valid": true,
    "message": "Rule updated successfully"
  }
}
```

#### Delete Rule

```http
DELETE /api/rules/{rule_id}
```

**Response** (200 OK):
```json
{
  "message": "Rule deleted successfully",
  "id": "rule-12345"
}
```

#### Validate Rule Content

```http
POST /api/rules/validate
Content-Type: application/json
```

**Request Body**:
```json
{
  "content": "rule testRule:\n    if condition then action"
}
```

**Response** (200 OK - Valid):
```json
{
  "valid": true,
  "message": "Rule syntax is valid",
  "parsed_rule_name": "testRule"
}
```

**Response** (200 OK - Invalid):
```json
{
  "valid": false,
  "message": "Syntax error in rule",
  "errors": [
    {
      "type": "grammar",
      "message": "Missing 'endif' at line 2",
      "line": 2,
      "column": 35,
      "severity": "error"
    }
  ],
  "warnings": [
    {
      "type": "missing_action",
      "message": "Unknown action: unknownAction",
      "severity": "warning"
    }
  ],
  "undefined_attributes": ["unknownAttr"],
  "undefined_actions": ["unknownAction"]
}
```

### Code Generation API

#### Generate Production Code

```http
POST /api/rules/generate-production-code
Content-Type: application/json
```

**Request Body**:
```json
{
  "ruleId": "rule-12345",
  "ruleName": "creditApprovalRule",
  "ruleContent": "rule creditApprovalRule:\n    if applicant.creditScore >= 700 then approve",
  "packageName": "com.rules.rule12345",
  "itemType": "rule"
}
```

**Response** (200 OK):
```json
{
  "success": true,
  "artifactCount": 3,
  "ruleId": "rule-12345",
  "ruleName": "creditApprovalRule",
  "packageName": "com.rules.rule12345",
  "outputDirectory": "/path/to/generated-rules/rule-creditApprovalRule",
  "files": {
    "CreditApprovalRule.java": "package com.rules.rule12345;...",
    "pom.xml": "<?xml version=\"1.0\"?>...",
    "README.md": "# Rule: creditApprovalRule..."
  }
}
```

#### Build Rule Code

```http
POST /api/rules/{rule_id}/build
```

**Response** (200 OK):
```json
{
  "success": true,
  "build_time_ms": 1243,
  "rule_directory": "/path/to/generated-rules/rule-creditApprovalRule",
  "build_log": "[INFO] Building...\n[INFO] BUILD SUCCESS\n"
}
```

**Response** (200 OK - Failed):
```json
{
  "success": false,
  "error": "Compilation failed",
  "build_time_ms": 876,
  "build_log": "[ERROR] Compilation error at line 15..."
}
```

#### Test Rule Code

```http
POST /api/rules/{rule_id}/test
```

**Response** (200 OK):
```json
{
  "success": true,
  "test_time_ms": 456,
  "rule_directory": "/path/to/generated-rules/rule-creditApprovalRule",
  "test_summary": {
    "tests_run": 5,
    "failures": 0,
    "errors": 0
  },
  "test_log": "[INFO] Running tests...\n[INFO] Tests run: 5, Failures: 0\n"
}
```

### Context API

#### List Contexts

```http
GET /api/contexts
```

**Query Parameters**:
- `limit` (int, default: 100): Max contexts to return

**Response** (200 OK):
```json
{
  "contexts": [
    {
      "name": "test_context_1",
      "description": "High credit score applicant",
      "schema_name": "applicant_schema",
      "is_schema_template": false,
      "created_at": "2023-09-15T08:00:00"
    }
  ]
}
```

#### Get Single Context

```http
GET /api/contexts/{context_name}
```

**Response** (200 OK):
```json
{
  "name": "test_context_1",
  "description": "High credit score applicant",
  "schema_name": "applicant_schema",
  "context_data": {
    "applicant": {
      "creditScore": 750,
      "income": 75000,
      "age": 35
    },
    "transaction": {
      "amount": 1500,
      "timestamp": "2023-09-17T14:30:00"
    }
  }
}
```

#### Create Context

```http
POST /api/contexts
Content-Type: application/json
```

**Request Body**:
```json
{
  "name": "new_test_context",
  "description": "Context description",
  "schema_name": "applicant_schema",
  "context_data": {
    "applicant": {
      "creditScore": 800,
      "income": 100000
    }
  }
}
```

**Response** (201 Created):
```json
{
  "name": "new_test_context",
  "message": "Context created successfully"
}
```

### Schema API

#### Get Schema Attributes

```http
GET /api/schema/{schema_version}/attributes
```

**Parameters**:
- `schema_version`: "modern" or "legacy"

**Response** (200 OK):
```json
{
  "attributes": [
    {
      "label": "applicant.creditScore",
      "kind": "property",
      "detail": "Applicant credit score (300-850)",
      "documentation": "Numeric credit score from credit bureau"
    },
    {
      "label": "applicant.income",
      "kind": "property",
      "detail": "Annual income in USD",
      "documentation": "Gross annual income"
    }
  ]
}
```

#### Get Schema Actions

```http
GET /api/schema/{schema_version}/actions
```

**Response** (200 OK):
```json
{
  "actions": [
    {
      "label": "approveApplication",
      "kind": "function",
      "detail": "Approve credit card application",
      "documentation": "Sets application status to APPROVED"
    }
  ]
}
```

---

## Rules Processing

### Validation Pipeline

```
1. Syntax Validation (ANTLR)
   ↓
2. Semantic Validation (Attributes/Actions check)
   ↓
3. Dependency Validation (Cycle detection)
   ↓
4. Status Update
```

### Validation Example (Python)

```python
from services.java_bridge import JavaBridge

# Initialize Java bridge
java_bridge = JavaBridge()

# Validate rule content
rule_content = """
rule myRule:
    if applicant.creditScore >= 700 then approveApplication
"""

result = java_bridge.validate_rule(rule_content)

if result['valid']:
    print("Rule is valid!")
    print(f"Parsed name: {result.get('parsed_rule_name')}")
else:
    print("Validation errors:")
    for error in result.get('errors', []):
        print(f"  - {error['message']} at line {error['line']}")
```

### Compilation Process

```python
from services.java_bridge import JavaBridge

java_bridge = JavaBridge()

# Compile rule to Java bytecode
compilation_result = java_bridge.compile_rule(
    rule_id="rule-12345",
    rule_name="creditApprovalRule",
    rule_content=rule_content,
    package_name="com.rules.creditapproval"
)

if compilation_result['success']:
    print(f"Compiled to: {compilation_result['output_directory']}")
else:
    print(f"Compilation failed: {compilation_result['error']}")
```

### Execution Example

```python
import subprocess
import json

# Execute compiled rule
rule_jar = "/path/to/generated-rules/rule-creditApprovalRule/target/creditApprovalRule-1.0.jar"

context_data = {
    "applicant": {
        "creditScore": 750,
        "income": 75000
    }
}

result = subprocess.run(
    ['java', '-jar', rule_jar, json.dumps(context_data)],
    capture_output=True,
    text=True
)

execution_result = json.loads(result.stdout)
print(f"Rule result: {execution_result}")
```

---

## Context Management

### What are Contexts?

Contexts provide test data for rule validation and testing. They define sample attribute values that rules can be evaluated against.

### Context File Structure

Location: `backend/data/contexts/{context_name}.json`

```json
{
  "name": "high_score_applicant",
  "description": "Applicant with excellent credit",
  "schema_name": "applicant_schema",
  "is_schema_template": false,
  "context_data": {
    "applicant": {
      "creditScore": 800,
      "income": 100000,
      "age": 35,
      "employmentStatus": "EMPLOYED",
      "address": {
        "zipCode": "10001",
        "state": "NY"
      }
    },
    "transaction": {
      "amount": 2500,
      "timestamp": "2023-09-17T14:30:00",
      "merchantCategory": "RETAIL"
    }
  }
}
```

### Creating Contexts Programmatically

```python
from services.context_service import ContextService

context_service = ContextService()

# Create new context
context_service.create_context(
    name="vip_customer",
    description="VIP customer with high value",
    schema_name="customer_schema",
    context_data={
        "customer": {
            "vipStatus": True,
            "accountAge": 1825,  # 5 years in days
            "lifetimeValue": 500000
        }
    }
)
```

### Schema Templates

Schema templates are special contexts that define data structures without values:

```json
{
  "name": "applicant_schema_template",
  "description": "Schema definition for applicant data",
  "is_schema_template": true,
  "schema_definition": {
    "entities": [
      {
        "name": "applicant",
        "description": "Credit card applicant",
        "attributes": [
          {
            "name": "creditScore",
            "data_type": "integer",
            "description": "Credit score (300-850)",
            "is_required": true,
            "validation_rules": "range(300, 850)"
          }
        ]
      }
    ]
  }
}
```

---

## Schema Management

### Schema Versions

The system supports multiple schema versions:

- **modern**: Dot-notation attributes (`applicant.creditScore`)
- **legacy**: UPPERCASE attributes (`CREDIT_SCORE`, `ANNUAL_INCOME`)

### Schema File Structure

Location: `backend/data/schemas/{schema_name}.json`

```json
{
  "schema_name": "credit_decisioning_schema",
  "version": "1.0",
  "entities": [
    {
      "name": "applicant",
      "description": "Credit card applicant entity",
      "attributes": [
        {
          "name": "creditScore",
          "qualified_name": "applicant.creditScore",
          "data_type": "integer",
          "description": "Credit score from credit bureau",
          "is_required": true,
          "min_value": 300,
          "max_value": 850
        },
        {
          "name": "income",
          "qualified_name": "applicant.income",
          "data_type": "decimal",
          "description": "Annual income in USD",
          "is_required": true,
          "min_value": 0
        }
      ]
    }
  ],
  "actions": [
    {
      "name": "approveApplication",
      "description": "Approve the credit card application",
      "item_type": "action",
      "java_file_path": "actions/ApprovalAction.java",
      "parameters": []
    }
  ]
}
```

### Loading Schema Programmatically

```python
from services.schema_service import SchemaService

schema_service = SchemaService()

# Get schema attributes
attributes = schema_service.get_attributes("modern")

# Get schema actions
actions = schema_service.get_actions("modern")

# Get specific attribute details
attribute = schema_service.get_attribute_by_name("applicant.creditScore")
print(f"Type: {attribute['data_type']}, Range: {attribute['min_value']}-{attribute['max_value']}")
```

---

## Code Generation

### Generated Code Structure

For each rule, the code generator creates:

```
generated-rules/
└── rule-{ruleName}/
    ├── src/
    │   └── main/
    │       └── java/
    │           └── com/
    │               └── rules/
    │                   └── {packageName}/
    │                       └── {RuleName}.java
    ├── pom.xml
    ├── README.md
    └── target/
        └── {ruleName}-1.0.jar  (after build)
```

### Generated Java Class Example

```java
package com.rules.creditapproval;

import java.util.*;

public class CreditApprovalRule {

    public Map<String, Object> execute(Map<String, Object> context) {
        Map<String, Object> result = new HashMap<>();

        // Extract attributes from context
        Map<String, Object> applicant = (Map<String, Object>) context.get("applicant");
        Integer creditScore = (Integer) applicant.get("creditScore");

        // Execute rule logic
        if (creditScore >= 700) {
            result.put("action", "approveApplication");
            result.put("approved", true);
        } else {
            result.put("action", "rejectApplication");
            result.put("approved", false);
        }

        result.put("executionTimeMs", System.currentTimeMillis());
        return result;
    }

    public static void main(String[] args) throws Exception {
        // CLI interface for testing
        String contextJson = args[0];
        Map<String, Object> context = parseJson(contextJson);

        CreditApprovalRule rule = new CreditApprovalRule();
        Map<String, Object> result = rule.execute(context);

        System.out.println(toJson(result));
    }
}
```

### Code Generation Service

```python
from services.java_bridge import JavaBridge

java_bridge = JavaBridge()

# Generate production code
result = java_bridge.generate_production_code(
    rule_id="rule-12345",
    rule_name="creditApprovalRule",
    rule_content=rule_content,
    package_name="com.rules.creditapproval",
    item_type="rule"
)

print(f"Generated {result['artifactCount']} files")
print(f"Output directory: {result['outputDirectory']}")

# Files generated: Java class, pom.xml, README.md
for filename, content in result['files'].items():
    print(f"  - {filename}")
```

---

## Integration Patterns

### Pattern 1: Synchronous Rule Execution

```python
from flask import Flask, request, jsonify
from services.rule_service import RuleService

app = Flask(__name__)
rule_service = RuleService()

@app.route('/execute-rule', methods=['POST'])
def execute_rule():
    data = request.json
    rule_id = data['rule_id']
    context = data['context']

    # Execute rule synchronously
    result = rule_service.execute_rule(rule_id, context)

    return jsonify(result), 200
```

### Pattern 2: Batch Rule Processing

```python
from services.rule_service import RuleService
import concurrent.futures

rule_service = RuleService()

def process_batch(rule_ids, contexts):
    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        futures = []

        for rule_id, context in zip(rule_ids, contexts):
            future = executor.submit(rule_service.execute_rule, rule_id, context)
            futures.append(future)

        results = [f.result() for f in concurrent.futures.as_completed(futures)]

    return results
```

### Pattern 3: Rule Change Notification

```python
from services.rule_service import RuleService
from flask_socketio import SocketIO, emit

socketio = SocketIO()
rule_service = RuleService()

@socketio.on('subscribe_rule')
def handle_rule_subscription(rule_id):
    # Client subscribes to rule changes
    join_room(f'rule-{rule_id}')

def notify_rule_change(rule_id, change_type):
    # Notify all subscribed clients
    socketio.emit('rule_changed', {
        'rule_id': rule_id,
        'change_type': change_type,
        'timestamp': datetime.now().isoformat()
    }, room=f'rule-{rule_id}')
```

### Pattern 4: Caching Compiled Rules

```python
from functools import lru_cache
from services.java_bridge import JavaBridge

java_bridge = JavaBridge()

@lru_cache(maxsize=100)
def get_compiled_rule(rule_id, rule_hash):
    """Cache compiled rules by ID and content hash"""
    rule = rule_service.get_rule(rule_id)

    if rule['compilation_hash'] == rule_hash:
        return rule['compiled_jar_path']

    # Recompile if hash mismatch
    result = java_bridge.compile_rule(
        rule_id=rule_id,
        rule_name=rule['name'],
        rule_content=rule['content']
    )

    return result['output_jar']
```

---

## Performance Optimization

### Metrics

Current performance benchmarks:

| Operation | Average Time | Notes |
|-----------|--------------|-------|
| Rule Validation | 45ms | ANTLR parsing |
| Rule Compilation | 63ms | Java bytecode generation |
| Rule Execution | 0.67ms | Sub-millisecond performance |
| Context Loading | 12ms | JSON file read |
| Schema Loading | 8ms | Cached after first load |

### Optimization Strategies

#### 1. Enable Caching

```python
from functools import lru_cache

class SchemaService:
    @lru_cache(maxsize=10)
    def get_schema(self, schema_name):
        # Cached schema loading
        return self._load_schema_from_file(schema_name)
```

#### 2. Batch Operations

```python
# Instead of multiple individual requests
for rule_id in rule_ids:
    result = api.get_rule(rule_id)  # N requests

# Use batch endpoint
results = api.get_rules_batch(rule_ids)  # 1 request
```

#### 3. Connection Pooling

```python
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
import requests

session = requests.Session()
retry = Retry(total=3, backoff_factor=0.3)
adapter = HTTPAdapter(max_retries=retry, pool_connections=10, pool_maxsize=20)
session.mount('http://', adapter)
session.mount('https://', adapter)

# Use session for all requests
response = session.get('http://localhost:5001/api/rules')
```

#### 4. Async Processing

```python
import asyncio
import aiohttp

async def validate_rules_async(rule_contents):
    async with aiohttp.ClientSession() as session:
        tasks = []
        for content in rule_contents:
            task = session.post(
                'http://localhost:5001/api/rules/validate',
                json={'content': content}
            )
            tasks.append(task)

        results = await asyncio.gather(*tasks)
        return [await r.json() for r in results]

# Usage
results = asyncio.run(validate_rules_async(rule_list))
```

---

## Security Considerations

### Input Validation

```python
from marshmallow import Schema, fields, validate

class RuleSchema(Schema):
    name = fields.Str(required=True, validate=validate.Length(min=1, max=255))
    content = fields.Str(required=True, validate=validate.Length(min=10, max=100000))
    description = fields.Str(validate=validate.Length(max=1000))
    status = fields.Str(validate=validate.OneOf(['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD']))

# Use schema for validation
schema = RuleSchema()
try:
    validated_data = schema.load(request_data)
except ValidationError as err:
    return jsonify({'error': err.messages}), 400
```

### SQL Injection Prevention

The system uses file-based storage, eliminating SQL injection risks. For legacy SQLAlchemy code:

```python
# Safe - Using ORM
Rule.query.filter_by(name=rule_name).first()

# Unsafe - String interpolation
# NEVER DO THIS
db.execute(f"SELECT * FROM rules WHERE name = '{rule_name}'")
```

### Path Traversal Prevention

```python
import os

def safe_file_path(base_dir, filename):
    # Resolve to absolute path
    requested_path = os.path.abspath(os.path.join(base_dir, filename))

    # Ensure path is within base directory
    if not requested_path.startswith(os.path.abspath(base_dir)):
        raise ValueError("Path traversal attempt detected")

    return requested_path

# Usage
safe_path = safe_file_path(RULES_DIR, user_provided_filename)
```

### Code Injection Prevention

```python
import subprocess
import shlex

# Safe command execution
def run_java_command(jar_path, args):
    # Validate JAR path
    if not os.path.exists(jar_path):
        raise ValueError("JAR file not found")

    # Use list arguments (not shell=True)
    cmd = ['java', '-jar', jar_path] + [shlex.quote(arg) for arg in args]

    result = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        timeout=10,
        shell=False  # Never use shell=True with user input
    )

    return result.stdout
```

### Rate Limiting

```python
from flask_limiter import Limiter
from flask_limiter.util import get_remote_address

limiter = Limiter(
    app,
    key_func=get_remote_address,
    default_limits=["200 per day", "50 per hour"]
)

@app.route('/api/rules', methods=['POST'])
@limiter.limit("10 per minute")
def create_rule():
    # Rule creation logic
    pass
```

---

## Deployment

### Production Configuration

```python
class ProductionConfig(Config):
    DEBUG = False
    API_HOST = '0.0.0.0'
    API_PORT = 5001

    # Use production-grade server (not Flask dev server)
    # Use gunicorn or uWSGI

    # Enable logging
    LOG_LEVEL = 'INFO'
    LOG_FILE = '/var/log/rules-engine/api.log'

    # Security
    SECRET_KEY = os.environ.get('SECRET_KEY')
    JWT_SECRET = os.environ.get('JWT_SECRET')

    # Timeouts
    VALIDATION_TIMEOUT_MS = 3000
    COMPILATION_TIMEOUT_MS = 10000
```

### Using Gunicorn

```bash
# Install gunicorn
pip install gunicorn

# Run with gunicorn
gunicorn -w 4 -b 0.0.0.0:5001 app:app

# With config file
gunicorn -c gunicorn_config.py app:app
```

**gunicorn_config.py**:
```python
bind = "0.0.0.0:5001"
workers = 4
worker_class = "sync"
timeout = 120
keepalive = 5
accesslog = "/var/log/rules-engine/access.log"
errorlog = "/var/log/rules-engine/error.log"
loglevel = "info"
```

### Docker Deployment

```dockerfile
FROM python:3.9-slim

# Install Java
RUN apt-get update && apt-get install -y openjdk-17-jdk maven

WORKDIR /app

# Copy backend files
COPY backend/ /app/backend/
COPY java-bridge/ /app/java-bridge/

# Install Python dependencies
RUN pip install -r backend/requirements.txt

# Build Java bridge
RUN cd java-bridge && mvn clean package

# Expose port
EXPOSE 5001

# Run with gunicorn
CMD ["gunicorn", "-c", "backend/gunicorn_config.py", "backend.app:app"]
```

### Health Checks

```python
@app.route('/health', methods=['GET'])
def health_check():
    health = {
        'status': 'healthy',
        'timestamp': datetime.now().isoformat(),
        'checks': {
            'api': 'ok',
            'file_storage': check_file_storage(),
            'java_bridge': check_java_bridge()
        }
    }

    if all(v == 'ok' for v in health['checks'].values()):
        return jsonify(health), 200
    else:
        return jsonify(health), 503

def check_file_storage():
    try:
        return 'ok' if os.path.exists(RULES_DIR) else 'error'
    except:
        return 'error'

def check_java_bridge():
    try:
        result = subprocess.run(
            ['java', '-version'],
            capture_output=True,
            timeout=5
        )
        return 'ok' if result.returncode == 0 else 'error'
    except:
        return 'error'
```

---

## Troubleshooting

### Common Issues

#### Issue 1: Java Bridge Not Found

**Symptom**: `FileNotFoundError: JAR file not found`

**Solution**:
```bash
# Rebuild Java bridge
cd java-bridge
mvn clean package

# Verify JAR exists
ls -lh target/rules-engine-*.jar

# Update config.py with correct path
JAVA_BRIDGE_JAR = '/absolute/path/to/jar'
```

#### Issue 2: Validation Timeout

**Symptom**: `TimeoutError: Rule validation exceeded 5000ms`

**Solution**:
- Increase timeout in config: `VALIDATION_TIMEOUT_MS = 10000`
- Check for infinite loops in rule logic
- Verify Java process isn't hanging

#### Issue 3: File Permission Errors

**Symptom**: `PermissionError: Cannot write to /data/rules`

**Solution**:
```bash
# Set correct permissions
chmod 755 backend/data
chmod 644 backend/data/rules/*.json

# Or run with correct user
sudo chown -R appuser:appgroup backend/data
```

#### Issue 4: Memory Issues

**Symptom**: `Java heap space error during compilation`

**Solution**:
```python
# Increase Java heap size
java_cmd = [
    'java',
    '-Xmx2G',  # Increase heap to 2GB
    '-jar', jar_path
]
```

#### Issue 5: Rule Name Conflicts

**Symptom**: `409 Conflict: Rule with name 'myRule' already exists`

**Solution**:
- Use unique rule names
- Check existing rules: `GET /api/rules`
- Delete or rename conflicting rule

### Debug Mode

Enable detailed logging:

```python
import logging

logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('debug.log'),
        logging.StreamHandler()
    ]
)

logger = logging.getLogger(__name__)

# In your code
logger.debug(f"Validating rule: {rule_content}")
logger.info(f"Rule {rule_id} validated successfully")
logger.error(f"Validation failed: {error}")
```

### API Testing with curl

```bash
# Health check
curl http://localhost:5001/health

# List rules
curl http://localhost:5001/api/rules

# Create rule
curl -X POST http://localhost:5001/api/rules \
  -H "Content-Type: application/json" \
  -d '{
    "name": "testRule",
    "content": "rule testRule:\n    if condition then action",
    "process_area_id": "TEST_AREA"
  }'

# Validate rule
curl -X POST http://localhost:5001/api/rules/validate \
  -H "Content-Type: application/json" \
  -d '{
    "content": "rule test:\n    if applicant.creditScore >= 700 then approve"
  }'
```

---

## Next Steps

- **DSL Language**: See [DSL Language Guide](./DSL_Language_Guide.md) for rule syntax
- **UI Guide**: See [UI User Guide](./UI_User_Guide.md) for frontend usage
- **API Examples**: Check `backend/tests/` for integration test examples
- **Performance Tuning**: See `backend/docs/performance.md` for advanced optimization

---

**End of Backend Integration Guide**
