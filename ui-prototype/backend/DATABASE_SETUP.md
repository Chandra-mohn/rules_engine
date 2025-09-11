# Database Setup Guide

This guide provides multiple ways to set up the Rules Engine database with sample data for testing.

## Quick Setup Options

### Option 1: Python Script (Recommended)
```bash
# Navigate to backend directory
cd backend

# Run the setup script
python setup_database.py

# Or specify a custom database location
python setup_database.py --db-path /path/to/your/database.db

# Force overwrite existing database
python setup_database.py --force
```

### Option 2: Direct SQL Script
```bash
# Using SQLite command line
sqlite3 rules_engine.db < create_database.sql

# Or using any SQLite client/tool
# Import and execute create_database.sql
```

## What Gets Created

### Database Schema
- **clients** - Client organizations (AMEX, VISA, Mastercard)
- **process_groups** - Business process groups within clients
- **process_areas** - Specific process areas within groups  
- **rules** - Individual business rules
- **rule_history** - Version history of rule changes
- **rule_lists** - Predefined lists for autocomplete/validation

### Sample Data Includes

#### Clients (3 records)
- **AMEX** - American Express
- **VISA** - Visa Inc.
- **MC** - Mastercard

#### Hierarchy Structure
```
AMEX
├── Credit Card Processing (CC_PROC)
│   ├── Application Processing (APP) - 8 rules
│   ├── Limit Management (LIMIT)
│   └── Account Management (ACCOUNT)
├── Fraud Detection (FRAUD)
│   ├── Real-time Fraud (REAL_TIME) - 2 rules
│   ├── Batch Fraud (BATCH)
│   └── Behavioral Analysis (BEHAVIORAL)
└── Risk Management (RISK)
    ├── Credit Scoring (SCORING)
    └── Underwriting (UNDERWRITING)

VISA
├── Authorization (AUTH)
│   ├── Online Authorization (ONLINE) - 1 rule
│   ├── Offline Authorization (OFFLINE)
│   └── Decline Management (DECLINE)
├── Settlement (SETTLE)
│   ├── Clearing (CLEARING)
│   └── Reconciliation (RECONCILE)
└── Network Processing (NETWORK)
    ├── Transaction Routing (ROUTING)
    └── Network Switching (SWITCHING) - 1 rule

MASTERCARD
├── Acquiring (ACQUIRE)
│   ├── Merchant Management (MERCHANT)
│   └── Point of Sale (POS)
└── Issuing (ISSUE)
    ├── Card Issuance (CARD_ISSUE)
    └── Card Activation (ACTIVATION)
```

#### Rules (12 total)
- **Status Distribution:**
  - PROD: 7 rules (production ready)
  - DRAFT: 2 rules (work in progress)
  - VALID: 1 rule (validated, ready for approval)
  - PEND: 1 rule (pending approval)
  - SCHD: 1 rule (scheduled for deployment)

- **Schema Types:**
  - Modern: 8 rules (new DSL syntax)
  - Legacy: 4 rules (old syntax)

#### Predefined Lists (7 lists)
For autocomplete and validation:
- Application statuses
- Credit score ranges  
- Merchant categories
- Transaction types
- Risk levels
- Fraud indicators
- Business dates

## Verification

After setup, verify the database:

```sql
-- Count records
SELECT 'Clients' as table_name, COUNT(*) as count FROM clients
UNION ALL SELECT 'Process Groups', COUNT(*) FROM process_groups  
UNION ALL SELECT 'Process Areas', COUNT(*) FROM process_areas
UNION ALL SELECT 'Rules', COUNT(*) FROM rules;

-- Check hierarchy
SELECT 
    c.name as client,
    pg.name as process_group,
    pa.name as process_area,
    COUNT(r.id) as rule_count
FROM clients c
JOIN process_groups pg ON c.id = pg.client_id
JOIN process_areas pa ON pg.id = pa.process_group_id
LEFT JOIN rules r ON pa.id = r.process_area_id
GROUP BY c.name, pg.name, pa.name
ORDER BY c.name, pg.name, pa.name;
```

## Configuration

Update your `app.py` or configuration to point to the database:

```python
# For SQLite
SQLALCHEMY_DATABASE_URI = 'sqlite:///rules_engine.db'

# For custom path
SQLALCHEMY_DATABASE_URI = 'sqlite:////absolute/path/to/rules_engine.db'
```

## Troubleshooting

### Permission Issues
```bash
# Make sure the script is executable
chmod +x setup_database.py

# Ensure write permissions for database directory
chmod 755 /path/to/database/directory
```

### SQLite Not Found
```bash
# Install SQLite (if needed)
# macOS: brew install sqlite
# Ubuntu: sudo apt install sqlite3
# Windows: Download from sqlite.org
```

### Python Dependencies
```bash
# Install required packages
pip install sqlite3  # Usually included with Python
```

## Next Steps

1. **Start Backend**: `python app.py`
2. **Start Frontend**: `cd ../frontend && npm start`
3. **Access Application**: http://localhost:3000

The application will automatically connect to the database and you'll see the complete hierarchy with sample rules ready for testing!