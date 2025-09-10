-- Migration script for Rules Hierarchy Enhancement
-- Execute this script to add the new table structure

-- 1. Create Clients table
CREATE TABLE IF NOT EXISTS clients (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    code VARCHAR(10) NOT NULL UNIQUE,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    is_active BOOLEAN DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- 2. Create Process Groups table
CREATE TABLE IF NOT EXISTS process_groups (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    client_id INTEGER NOT NULL,
    code VARCHAR(20) NOT NULL,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    is_active BOOLEAN DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (client_id) REFERENCES clients (id) ON DELETE CASCADE,
    UNIQUE(client_id, code)
);

-- 3. Create Process Areas table
CREATE TABLE IF NOT EXISTS process_areas (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    process_group_id INTEGER NOT NULL,
    code VARCHAR(20) NOT NULL,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    is_active BOOLEAN DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (process_group_id) REFERENCES process_groups (id) ON DELETE CASCADE,
    UNIQUE(process_group_id, code)
);

-- 4. Create backup of existing rules table
CREATE TABLE IF NOT EXISTS rules_backup AS SELECT * FROM rules;

-- 5. Create new rules table with enhanced structure
DROP TABLE IF EXISTS rules_new;
CREATE TABLE rules_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    process_area_id INTEGER NOT NULL,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    content TEXT NOT NULL,
    status VARCHAR(10) DEFAULT 'DRAFT',  -- DRAFT, VALID, PEND, SCHD, PROD
    effective_date DATE,
    expiry_date DATE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(50) DEFAULT 'system',
    updated_by VARCHAR(50) DEFAULT 'system',
    validation_status VARCHAR(20) DEFAULT 'pending',
    validation_message TEXT,
    version INTEGER DEFAULT 1,
    schema_version VARCHAR(20) DEFAULT 'modern',
    FOREIGN KEY (process_area_id) REFERENCES process_areas (id) ON DELETE CASCADE,
    UNIQUE(process_area_id, name, effective_date)  -- Composite unique constraint
);

-- 6. Sample data for the new hierarchy

-- Insert sample clients
INSERT INTO clients (code, name, description) VALUES 
('AMEX', 'American Express', 'American Express Card Services'),
('VISA', 'Visa Inc.', 'Visa Payment Processing'),
('MC', 'Mastercard', 'Mastercard Payment Solutions');

-- Insert sample process groups
INSERT INTO process_groups (client_id, code, name, description) VALUES 
(1, 'CC_PROC', 'Credit Card Processing', 'Core credit card processing operations'),
(1, 'FRAUD', 'Fraud Detection', 'Fraud prevention and detection systems'),
(1, 'RISK', 'Risk Management', 'Credit risk assessment and management'),
(2, 'AUTH', 'Authorization', 'Transaction authorization services'),
(2, 'SETTLE', 'Settlement', 'Transaction settlement processing'),
(2, 'NETWORK', 'Network Processing', 'Network transaction processing'),
(3, 'ACQUIRE', 'Acquiring', 'Merchant acquiring services'),
(3, 'ISSUE', 'Issuing', 'Card issuing services');

-- Insert sample process areas
INSERT INTO process_areas (process_group_id, code, name, description) VALUES 
-- AMEX Credit Card Processing
(1, 'APP', 'Application Processing', 'Credit card application evaluation'),
(1, 'LIMIT', 'Limit Management', 'Credit limit setting and management'),
(1, 'ACCOUNT', 'Account Management', 'Account lifecycle management'),
-- AMEX Fraud Detection  
(2, 'REAL_TIME', 'Real-time Fraud', 'Real-time fraud detection during transactions'),
(2, 'BATCH', 'Batch Fraud', 'Batch fraud analysis and reporting'),
(2, 'BEHAVIORAL', 'Behavioral Analysis', 'Customer behavior pattern analysis'),
-- AMEX Risk Management
(3, 'SCORING', 'Credit Scoring', 'Credit score calculation and validation'),
(3, 'UNDERWRITING', 'Underwriting', 'Credit underwriting rules'),
-- VISA Authorization
(4, 'ONLINE', 'Online Authorization', 'Online transaction authorization'),
(4, 'OFFLINE', 'Offline Authorization', 'Offline transaction processing'),
(4, 'DECLINE', 'Decline Management', 'Transaction decline handling'),
-- VISA Settlement
(5, 'CLEARING', 'Clearing', 'Transaction clearing processes'),
(5, 'RECONCILE', 'Reconciliation', 'Settlement reconciliation'),
-- VISA Network Processing
(6, 'ROUTING', 'Transaction Routing', 'Transaction routing logic'),
(6, 'SWITCHING', 'Network Switching', 'Network switching rules'),
-- MC Acquiring  
(7, 'MERCHANT', 'Merchant Management', 'Merchant onboarding and management'),
(7, 'POS', 'Point of Sale', 'POS transaction processing'),
-- MC Issuing
(8, 'CARD_ISSUE', 'Card Issuance', 'Credit card issuance rules'),
(8, 'ACTIVATION', 'Card Activation', 'Card activation processes');

-- 7. Migrate existing rules to new structure
-- For this demo, we'll assign all existing rules to AMEX -> CC_PROC -> APP
-- In a real migration, you'd need to map rules to appropriate hierarchy levels

INSERT INTO rules_new (
    process_area_id, 
    name, 
    description, 
    content, 
    status,
    effective_date,
    created_at,
    updated_at,
    created_by,
    updated_by,
    validation_status,
    validation_message,
    version,
    schema_version
)
SELECT 
    1 as process_area_id,  -- Default to AMEX -> CC_PROC -> APP
    name,
    description,
    content,
    CASE 
        WHEN status = 'active' THEN 'PROD'
        WHEN status = 'draft' THEN 'DRAFT' 
        WHEN status = 'inactive' THEN 'VALID'
        ELSE 'DRAFT'
    END as status,
    DATE('2024-01-01') as effective_date,  -- Default effective date
    created_at,
    updated_at,
    created_by,
    updated_by,
    validation_status,
    validation_message,
    version,
    schema_version
FROM rules_backup;

-- 8. Add some sample rules across different hierarchy levels
INSERT INTO rules_new (process_area_id, name, description, content, status, effective_date) VALUES 

-- AMEX Application Processing Rules
(1, 'creditScoreThreshold', 'Credit score validation for applications', 
'rule creditScoreThreshold:
    if applicant.creditScore >= 750 then approveApplication
    if applicant.creditScore < 600 then rejectApplication', 'PROD', '2024-01-01'),

(1, 'incomeVerification', 'Annual income verification rules',
'rule incomeVerification:
    if applicant.annualIncome >= 75000 then instantApproval
    if applicant.annualIncome < 30000 then rejectApplication', 'VALID', '2024-02-01'),

-- AMEX Real-time Fraud Rules  
(4, 'velocityCheck', 'Transaction velocity monitoring',
'rule velocityCheck:
    if transaction.dailyCount > 15 then flagForReview
    if transaction.amount > 5000 then manualReview', 'PROD', '2024-01-01'),

(4, 'geographicAnomaly', 'Geographic anomaly detection',
'rule geographicAnomaly:
    if transaction.location != account.homeLocation then sendAlert', 'SCHD', '2024-01-15'),

-- VISA Online Authorization Rules
(9, 'merchantValidation', 'Merchant validation rules',
'rule merchantValidation:
    if merchant.riskLevel = "high" then declineTransaction
    if merchant.category = "gambling" then requireManualReview', 'PEND', '2024-02-15'),

-- MC Card Issuance Rules
(15, 'cardTypeEligibility', 'Card type eligibility assessment',
'rule cardTypeEligibility:
    if applicant.creditScore >= 800 then setLimit(25000)
    if applicant.age < 21 then conditionalApproval', 'DRAFT', '2024-03-01');

-- 9. Replace the old rules table with the new one
-- (This would normally be done with careful planning in production)
DROP TABLE rules;
ALTER TABLE rules_new RENAME TO rules;

-- 10. Update rule_history table to maintain referential integrity
-- Add process_area_id for future use, but keep existing structure for now
ALTER TABLE rule_history ADD COLUMN process_area_id INTEGER;

-- Output success message
SELECT 'Migration completed successfully!' as result;
SELECT 'Total clients: ' || COUNT(*) as client_count FROM clients;
SELECT 'Total process groups: ' || COUNT(*) as process_group_count FROM process_groups;  
SELECT 'Total process areas: ' || COUNT(*) as process_area_count FROM process_areas;
SELECT 'Total migrated rules: ' || COUNT(*) as rule_count FROM rules;