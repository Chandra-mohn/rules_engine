# Comprehensive Regression Prevention Guide

## Overview
This guide provides a systematic approach to prevent the regression issues we've encountered, including data loss, field corruption, and schema inconsistencies.

## Root Causes of Previous Regressions

### 1. Data Loss Issues
- **effective_date field nulls**: Occurred during data migrations without proper defaults
- **Rule deletion**: Rules were marked as deleted instead of being properly archived

### 2. Field Corruption Issues
- **Status field regression**: Values changed from uppercase (VALID/DRAFT) to lowercase (active/draft)
- **Schema inconsistency**: validation_status field existed in some layers but not others

### 3. Schema Drift
- **Model/DB misalignment**: Database schema didn't match SQLAlchemy models
- **API contract changes**: Response structures changed without versioning

## Prevention Framework

### Phase 1: Database Level Protection

#### 1.1 Data Constraints
```sql
-- Status field constraints (implemented in data_validator.py)
status VARCHAR(10) CHECK (status IN ('DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD', 'deleted'))

-- Schema version constraints
schema_version VARCHAR(20) CHECK (schema_version IN ('modern', 'legacy'))

-- Required field constraints
name VARCHAR(100) NOT NULL UNIQUE
process_area_id INTEGER NOT NULL
```

#### 1.2 Default Values
```sql
status DEFAULT 'DRAFT'
schema_version DEFAULT 'modern'
created_at DEFAULT CURRENT_TIMESTAMP
version DEFAULT 1
```

### Phase 2: Application Level Validation

#### 2.1 Model Validation (models.py)
```python
# Add validation in SQLAlchemy models
@validates('status')
def validate_status(self, key, status):
    valid_statuses = ['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD', 'deleted']
    if status not in valid_statuses:
        raise ValueError(f"Invalid status: {status}")
    return status

@validates('effective_date')
def validate_effective_date(self, key, date):
    if self.status in ['VALID', 'PEND', 'SCHD', 'PROD'] and not date:
        raise ValueError("Active rules must have effective_date")
    return date
```

#### 2.2 Service Layer Validation (rule_service.py)
```python
def create_rule(self, rule_data):
    # Validate required fields
    required_fields = ['name', 'content', 'process_area_id']
    for field in required_fields:
        if not rule_data.get(field):
            raise ValueError(f"Missing required field: {field}")

    # Set defaults for new rules
    rule_data.setdefault('status', 'DRAFT')
    rule_data.setdefault('schema_version', 'modern')
    rule_data.setdefault('version', 1)
```

### Phase 3: Automated Testing

#### 3.1 Regression Test Suite
**File**: `test_regression_suite.py`

- **Data Integrity Tests**: Verify no data loss, check field values
- **Schema Consistency Tests**: Ensure DB schema matches models
- **API Contract Tests**: Validate response structures

**Usage**:
```bash
python test_regression_suite.py
```

#### 3.2 Pre-commit Checks
**File**: `pre_commit_checks.py`

Runs before any code changes:
- Backend health check
- Data backup verification
- Full regression test suite

**Usage**:
```bash
python pre_commit_checks.py
```

#### 3.3 Data Validation
**File**: `data_validator.py`

- Adds database constraints
- Validates existing data
- Creates data snapshots

**Usage**:
```bash
python data_validator.py
```

### Phase 4: Development Workflow

#### 4.1 Before Making Changes
```bash
# 1. Run pre-commit checks
python pre_commit_checks.py

# 2. Create data backup manually if needed
cp database/rules.db database/rules.db.$(date +%Y%m%d_%H%M%S)

# 3. Document the change purpose
echo "Purpose: [describe change]" >> change_log.txt
```

#### 4.2 After Making Changes
```bash
# 1. Run regression tests
python test_regression_suite.py

# 2. Validate data integrity
python data_validator.py

# 3. Test API contracts manually
curl -s "http://localhost:5001/api/rules?limit=3" | python -m json.tool
```

#### 4.3 Model/Schema Changes
```bash
# 1. Update model first
# 2. Create migration script
# 3. Test migration on backup
# 4. Run data validation
# 5. Update API if needed
# 6. Update UI if needed
# 7. Run full test suite
```

### Phase 5: Monitoring and Alerts

#### 5.1 Data Health Monitoring
Create scheduled job to run regression tests:
```bash
# Add to crontab for regular monitoring
0 */6 * * * cd /path/to/backend && python test_regression_suite.py
```

#### 5.2 Schema Drift Detection
```python
# Add to application startup
def check_schema_consistency():
    # Compare DB schema with model definitions
    # Alert if misaligned
```

#### 5.3 Data Backup Strategy
```bash
# Automated daily backups
0 2 * * * cp database/rules.db database/backups/rules_$(date +%Y%m%d).db

# Keep 7 days of backups
0 3 * * * find database/backups/ -name "rules_*.db" -mtime +7 -delete
```

## Quick Reference Commands

### Emergency Recovery
```bash
# Restore from backup
cp database/rules.db.backup database/rules.db

# Restart services
pkill -f "python app.py"
cd backend && python app.py &
```

### Health Check
```bash
# Quick system health check
python test_regression_suite.py
```

### Data Integrity Check
```bash
# Comprehensive data validation
python data_validator.py
```

## File Inventory

| File | Purpose | Usage |
|------|---------|-------|
| `test_regression_suite.py` | Complete regression testing | `python test_regression_suite.py` |
| `pre_commit_checks.py` | Pre-change validation | `python pre_commit_checks.py` |
| `data_validator.py` | Data validation & constraints | `python data_validator.py` |
| `data_snapshot.json` | Current data state snapshot | Auto-generated |
| `regression_test_results.json` | Test results history | Auto-generated |

## Success Metrics

- **Zero data loss**: No rules lost during schema changes
- **Zero field corruption**: Status values remain uppercase and valid
- **Zero schema drift**: DB and models stay synchronized
- **Fast recovery**: < 2 minutes to restore from backup
- **Early detection**: Issues caught before deployment

## Next Steps

1. **Implement monitoring dashboard**: Visual display of system health
2. **Add integration tests**: End-to-end workflow testing
3. **Create deployment checklist**: Standardized release process
4. **Set up alerting**: Automated notifications for failures

---

*Last Updated: 2025-09-13*
*Version: 1.0*