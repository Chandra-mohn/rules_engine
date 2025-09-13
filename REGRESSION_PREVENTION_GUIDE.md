# Regression Prevention Techniques - Rules Engine

## 🚨 **The Problem We Just Fixed**

We identified and resolved multiple regression issues:
1. **Status field confusion**: `active`/`draft` vs `DRAFT`/`VALID`/`PEND`/`SCHD`/`PROD`
2. **Effective date loss**: All rules had `null` effective_date values
3. **Field redundancy**: `status` and `validation_status` causing confusion

## 🛡️ **Prevention Strategies**

### 1. **Database Schema Validation**

#### **A. Add Database Constraints**
```sql
-- Prevent invalid status values
ALTER TABLE rules ADD CONSTRAINT chk_status
CHECK (status IN ('DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD'));

-- Ensure effective_date is not null for non-draft rules
ALTER TABLE rules ADD CONSTRAINT chk_effective_date
CHECK (status = 'DRAFT' OR effective_date IS NOT NULL);
```

#### **B. Database Migration Scripts**
```python
# Always include validation in migrations
def validate_migration():
    # Check data integrity
    invalid_count = Rule.query.filter(~Rule.status.in_(['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD'])).count()
    if invalid_count > 0:
        raise ValueError(f"Found {invalid_count} rules with invalid status values")
```

### 2. **Application-Level Validation**

#### **A. Model-Level Validation**
```python
# In models.py
from sqlalchemy.orm import validates

class Rule(db.Model):
    VALID_STATUSES = ['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD']

    @validates('status')
    def validate_status(self, key, status):
        if status not in self.VALID_STATUSES:
            raise ValueError(f"Invalid status: {status}. Must be one of {self.VALID_STATUSES}")
        return status

    @validates('effective_date')
    def validate_effective_date(self, key, effective_date):
        if self.status != 'DRAFT' and effective_date is None:
            raise ValueError("Non-draft rules must have an effective_date")
        return effective_date
```

#### **B. Service-Level Validation**
```python
# In rule_service.py
def create_rule(self, data, created_by):
    # Always validate data integrity
    self._validate_rule_data(data)
    # ... rest of creation logic

def _validate_rule_data(self, data):
    if 'status' in data and data['status'] not in Rule.VALID_STATUSES:
        raise ValueError(f"Invalid status: {data['status']}")
```

### 3. **API Contract Testing**

#### **A. Response Schema Validation**
```python
# Create JSON schema for API responses
RULE_RESPONSE_SCHEMA = {
    "type": "object",
    "properties": {
        "id": {"type": "integer"},
        "name": {"type": "string"},
        "status": {"enum": ["DRAFT", "VALID", "PEND", "SCHD", "PROD"]},
        "effective_date": {"type": ["string", "null"]},
        # Explicitly exclude validation_status
    },
    "required": ["id", "name", "status"],
    "additionalProperties": False  # Catch unexpected fields
}
```

#### **B. Integration Tests**
```python
def test_rule_status_values():
    """Ensure only valid status values are returned"""
    response = client.get('/api/rules')
    for rule in response.json['rules']:
        assert rule['status'] in ['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD']

def test_effective_date_not_null():
    """Ensure all rules have effective dates"""
    response = client.get('/api/rules')
    for rule in response.json['rules']:
        assert rule['effective_date'] is not None
```

### 4. **Development Practices**

#### **A. Code Review Checklist**
```markdown
## Database Changes Review
- [ ] Are new enum values properly constrained?
- [ ] Are migrations backward compatible?
- [ ] Are default values consistent with business logic?
- [ ] Are nullable fields intentional?

## API Changes Review
- [ ] Are response fields consistent with documentation?
- [ ] Are new fields added to schemas and tests?
- [ ] Are old fields properly deprecated before removal?

## Data Migration Review
- [ ] Does migration preserve existing data?
- [ ] Is there a rollback plan?
- [ ] Are edge cases handled?
```

#### **B. Pre-Commit Hooks**
```python
# .pre-commit-config.yaml
repos:
  - repo: local
    hooks:
      - id: validate-rule-status
        name: Validate Rule Status Values
        entry: python scripts/validate_status_values.py
        language: python
        files: '(models|services)\.py$'
```

### 5. **Monitoring & Alerting**

#### **A. Data Quality Checks**
```python
# Daily data quality job
def check_data_quality():
    # Check for invalid status values
    invalid_status = Rule.query.filter(~Rule.status.in_(Rule.VALID_STATUSES)).count()
    if invalid_status > 0:
        alert(f"Found {invalid_status} rules with invalid status")

    # Check for missing effective dates
    missing_dates = Rule.query.filter(Rule.effective_date.is_(None), Rule.status != 'DRAFT').count()
    if missing_dates > 0:
        alert(f"Found {missing_dates} non-draft rules without effective dates")
```

#### **B. API Health Checks**
```python
# Include in /api/health
def health_check():
    return {
        "status": "healthy",
        "data_quality": {
            "total_rules": Rule.query.count(),
            "valid_statuses": all(r.status in Rule.VALID_STATUSES for r in Rule.query.all()),
            "effective_dates": Rule.query.filter(Rule.effective_date.is_(None)).count()
        }
    }
```

### 6. **Documentation Standards**

#### **A. API Documentation**
```yaml
# OpenAPI specification
components:
  schemas:
    Rule:
      properties:
        status:
          type: string
          enum: ['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD']
          description: "Rule lifecycle status. DRAFT=editing, VALID=ready, PEND=pending deployment, SCHD=scheduled, PROD=production"
        effective_date:
          type: string
          format: date
          description: "Date when rule becomes effective. Required for non-DRAFT rules"
      required: [id, name, status, effective_date]
```

#### **B. Change Log**
```markdown
## Schema Changes
### 2025-01-13 - Status Field Consolidation
- **BREAKING**: Removed `validation_status` field
- **CHANGE**: Consolidated validation logic into `status` field
- **MIGRATION**: All existing rules preserved, status values normalized
- **IMPACT**: Frontend no longer receives validation_status in API responses
```

### 7. **Testing Strategies**

#### **A. Property-Based Testing**
```python
from hypothesis import given, strategies as st

@given(st.text())
def test_rule_creation_always_has_valid_status(rule_content):
    rule_data = {"content": rule_content, "process_area_id": 1}
    result = rule_service.create_rule(rule_data, "test_user")
    if result[0]:  # If rule was created successfully
        assert result[0].status in Rule.VALID_STATUSES
```

#### **B. Contract Testing**
```python
# Test that API contract matches frontend expectations
def test_api_contract_stability():
    """Ensure API responses match expected structure"""
    response = client.get('/api/rules/1')
    rule = response.json

    # Test presence of required fields
    required_fields = ['id', 'name', 'status', 'effective_date', 'content']
    for field in required_fields:
        assert field in rule, f"Missing required field: {field}"

    # Test absence of deprecated fields
    deprecated_fields = ['validation_status']
    for field in deprecated_fields:
        assert field not in rule, f"Deprecated field still present: {field}"
```

### 8. **Automation Tools**

#### **A. Database Schema Drift Detection**
```python
# Compare production schema vs expected schema
def detect_schema_drift():
    expected_columns = {
        'rules': ['id', 'name', 'content', 'status', 'effective_date', 'created_at']
    }

    actual_columns = get_table_columns('rules')

    missing = set(expected_columns['rules']) - set(actual_columns)
    extra = set(actual_columns) - set(expected_columns['rules'])

    if missing or extra:
        raise SchemaValidationError(f"Schema drift detected. Missing: {missing}, Extra: {extra}")
```

## 🎯 **Implementation Priority**

1. **High Priority**: Model validation, API schema validation, basic integration tests
2. **Medium Priority**: Database constraints, monitoring, documentation
3. **Low Priority**: Property-based testing, advanced automation

## 📋 **Quick Wins**

These can be implemented immediately:

1. Add `@validates` decorators to the Rule model
2. Create basic integration tests for status values
3. Add schema validation to API responses
4. Implement health check endpoints with data quality metrics
5. Document API changes in CHANGELOG.md

## 🚀 **Long-term Strategy**

- **Phase 1**: Reactive (catch regressions when they occur)
- **Phase 2**: Proactive (prevent regressions from happening)
- **Phase 3**: Predictive (anticipate potential issues)

The investment in prevention is always less than the cost of fixing regressions in production.