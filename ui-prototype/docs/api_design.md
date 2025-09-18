# Rules Authoring UI - API Design

## REST API Endpoints

### Rules Management

#### GET /api/rules
Get all rules with pagination and filtering
```json
{
  "page": 1,
  "limit": 10,
  "status": "active",
  "search": "credit"
}
```

Response:
```json
{
  "rules": [
    {
      "id": 1,
      "name": "creditScoreCheck",
      "description": "Basic credit score validation",
      "content": "rule creditScoreCheck:\n    if applicant.creditScore >= 700 then approveApplication",
      "status": "active",
      "validation_status": "valid",
      "created_at": "2025-08-26T10:00:00Z",
      "updated_at": "2025-08-26T10:00:00Z",
      "version": 1
    }
  ],
  "total": 25,
  "page": 1,
  "pages": 3
}
```

#### GET /api/rules/{id}
Get a specific rule by ID

#### POST /api/rules
Create a new rule
```json
{
  "name": "newRule",
  "description": "Description",
  "content": "rule newRule:\n    if condition then action"
}
```

#### PUT /api/rules/{id}
Update an existing rule

#### DELETE /api/rules/{id}
Delete a rule (soft delete, move to history)

### Rule Validation

#### POST /api/rules/validate
Validate rule syntax without saving
```json
{
  "content": "rule test:\n    if applicant.age >= 18 then approveApplication"
}
```

Response:
```json
{
  "valid": true,
  "message": "Rule syntax is valid",
  "suggestions": [
    {
      "type": "info",
      "message": "Consider adding error handling for missing age field"
    }
  ]
}
```

#### POST /api/rules/{id}/test
Test rule execution with sample data
```json
{
  "rule_content": "rule test:\n    if applicant.age >= 18 then approveApplication",
  "test_data": {
    "applicant": {
      "age": 25,
      "creditScore": 750
    }
  }
}
```

### Autocomplete & IntelliSense

#### GET /api/rules/autocomplete
Get autocomplete suggestions
```json
{
  "context": "if applicant.",
  "position": 13
}
```

Response:
```json
{
  "suggestions": [
    {
      "label": "creditScore",
      "kind": "property",
      "detail": "number",
      "documentation": "Applicant's credit score (300-850)"
    },
    {
      "label": "age",
      "kind": "property", 
      "detail": "number",
      "documentation": "Applicant's age in years"
    }
  ]
}
```

### Rule History

#### GET /api/rules/{id}/history
Get rule change history

#### POST /api/rules/{id}/revert/{version}
Revert rule to a previous version