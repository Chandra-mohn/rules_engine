-- Rules Engine Database Creation and Seed Data Script
-- This script creates the complete database schema and populates it with sample data
-- Run this script independently to set up the database for testing

-- Clean up existing tables (in reverse dependency order)
DROP TABLE IF EXISTS rule_history;
DROP TABLE IF EXISTS rule_lists;
DROP TABLE IF EXISTS rules;
DROP TABLE IF EXISTS process_areas;
DROP TABLE IF EXISTS process_groups;
DROP TABLE IF EXISTS clients;

-- Create Clients table
CREATE TABLE clients (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    code VARCHAR(10) NOT NULL UNIQUE,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    is_active BOOLEAN DEFAULT 1,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Create Process Groups table
CREATE TABLE process_groups (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    client_id INTEGER NOT NULL,
    code VARCHAR(20) NOT NULL,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    is_active BOOLEAN DEFAULT 1,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (client_id) REFERENCES clients(id),
    UNIQUE(client_id, code)
);

-- Create Process Areas table
CREATE TABLE process_areas (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    process_group_id INTEGER NOT NULL,
    code VARCHAR(20) NOT NULL,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    is_active BOOLEAN DEFAULT 1,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (process_group_id) REFERENCES process_groups(id),
    UNIQUE(process_group_id, code)
);

-- Create Rules table
CREATE TABLE rules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    process_area_id INTEGER NOT NULL,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    content TEXT NOT NULL,
    status VARCHAR(10) DEFAULT 'DRAFT',  -- DRAFT, VALID, PEND, SCHD, PROD
    effective_date DATE,
    expiry_date DATE,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(50) DEFAULT 'system',
    updated_by VARCHAR(50) DEFAULT 'system',
    validation_status VARCHAR(20) DEFAULT 'pending',  -- pending, valid, invalid
    validation_message TEXT,
    version INTEGER DEFAULT 1,
    schema_version VARCHAR(20) DEFAULT 'modern',  -- modern, legacy
    FOREIGN KEY (process_area_id) REFERENCES process_areas(id),
    UNIQUE(process_area_id, name, effective_date)
);

-- Create Rule History table
CREATE TABLE rule_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    rule_id INTEGER NOT NULL,
    process_area_id INTEGER,
    name VARCHAR(100) NOT NULL,
    content TEXT NOT NULL,
    version INTEGER NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(50) DEFAULT 'system',
    change_reason TEXT,
    FOREIGN KEY (rule_id) REFERENCES rules(id),
    FOREIGN KEY (process_area_id) REFERENCES process_areas(id)
);

-- Create Rule Lists table (for autocomplete and validation)
CREATE TABLE rule_lists (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR(100) NOT NULL UNIQUE,
    description TEXT,
    data_type VARCHAR(20) NOT NULL,  -- string, number, boolean
    list_values TEXT NOT NULL,  -- JSON array
    schema_version VARCHAR(20) DEFAULT 'both',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(50) DEFAULT 'system',
    updated_by VARCHAR(50) DEFAULT 'system'
);

-- Insert sample Clients
INSERT INTO clients (code, name, description, created_at, updated_at) VALUES
('AMEX', 'American Express', 'American Express financial services', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
('VISA', 'Visa Inc.', 'Visa payment network', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
('MC', 'Mastercard', 'Mastercard payment network', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Groups for AMEX
INSERT INTO process_groups (client_id, code, name, description, created_at, updated_at) VALUES
(1, 'CC_PROC', 'Credit Card Processing', 'Credit card application and processing workflows', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(1, 'FRAUD', 'Fraud Detection', 'Fraud detection and prevention systems', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(1, 'RISK', 'Risk Management', 'Risk assessment and credit scoring', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Groups for VISA
INSERT INTO process_groups (client_id, code, name, description, created_at, updated_at) VALUES
(2, 'AUTH', 'Authorization', 'Transaction authorization processes', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(2, 'SETTLE', 'Settlement', 'Payment settlement and clearing', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(2, 'NETWORK', 'Network Processing', 'Network routing and switching', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Groups for Mastercard
INSERT INTO process_groups (client_id, code, name, description, created_at, updated_at) VALUES
(3, 'ACQUIRE', 'Acquiring', 'Merchant acquiring services', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(3, 'ISSUE', 'Issuing', 'Card issuing services', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Areas for AMEX Credit Card Processing
INSERT INTO process_areas (process_group_id, code, name, description, created_at, updated_at) VALUES
(1, 'APP', 'Application Processing', 'Credit card application workflows', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(1, 'LIMIT', 'Limit Management', 'Credit limit management and adjustments', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(1, 'ACCOUNT', 'Account Management', 'Account lifecycle management', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Areas for AMEX Fraud Detection
INSERT INTO process_areas (process_group_id, code, name, description, created_at, updated_at) VALUES
(2, 'REAL_TIME', 'Real-time Fraud', 'Real-time fraud detection during transactions', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(2, 'BATCH', 'Batch Fraud', 'Batch fraud analysis and reporting', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(2, 'BEHAVIORAL', 'Behavioral Analysis', 'Customer behavior pattern analysis', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Areas for AMEX Risk Management
INSERT INTO process_areas (process_group_id, code, name, description, created_at, updated_at) VALUES
(3, 'SCORING', 'Credit Scoring', 'Credit score calculation and validation', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(3, 'UNDERWRITING', 'Underwriting', 'Credit underwriting processes', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Areas for VISA Authorization
INSERT INTO process_areas (process_group_id, code, name, description, created_at, updated_at) VALUES
(4, 'ONLINE', 'Online Authorization', 'Online transaction authorization', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(4, 'OFFLINE', 'Offline Authorization', 'Offline transaction processing', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(4, 'DECLINE', 'Decline Management', 'Transaction decline handling', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Areas for VISA Settlement
INSERT INTO process_areas (process_group_id, code, name, description, created_at, updated_at) VALUES
(5, 'CLEARING', 'Clearing', 'Transaction clearing processes', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(5, 'RECONCILE', 'Reconciliation', 'Payment reconciliation', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Areas for VISA Network Processing
INSERT INTO process_areas (process_group_id, code, name, description, created_at, updated_at) VALUES
(6, 'ROUTING', 'Transaction Routing', 'Transaction routing logic', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(6, 'SWITCHING', 'Network Switching', 'Network switching protocols', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Process Areas for Mastercard
INSERT INTO process_areas (process_group_id, code, name, description, created_at, updated_at) VALUES
(7, 'MERCHANT', 'Merchant Management', 'Merchant onboarding and management', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(7, 'POS', 'Point of Sale', 'POS terminal management', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(8, 'CARD_ISSUE', 'Card Issuance', 'Physical and virtual card issuance', '2025-01-01 00:00:00', '2025-01-01 00:00:00'),
(8, 'ACTIVATION', 'Card Activation', 'Card activation processes', '2025-01-01 00:00:00', '2025-01-01 00:00:00');

-- Insert sample Rules for AMEX Application Processing
INSERT INTO rules (process_area_id, name, description, content, status, effective_date, created_at, updated_at, created_by, updated_by, validation_status, version, schema_version) VALUES
(1, 'creditScoreCheck', 'Basic credit score validation', 'rule creditScoreCheck:
    if applicant.creditScore >= 700 then approveApplication
    if applicant.creditScore < 600 then rejectApplication', 'PROD', '2024-01-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'valid', 1, 'modern'),

(1, 'ageVerification', 'Minimum age requirement', 'rule ageVerification:
    if applicant.age < 18 then rejectApplication
    if applicant.age >= 18 then approveApplication', 'PROD', '2024-01-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'valid', 1, 'modern'),

(1, 'businessDateCheck', 'Business date validation example', 'rule businessDateCheck:
    if applicant.applicationDate after business_date then conditionalApproval
    if applicant.applicationDate before business_date then rejectApplication', 'DRAFT', '2024-01-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'pending', 1, 'modern'),

(1, 'legacyCreditCheck', 'Legacy style credit check', 'rule legacyCreditCheck:
    if CREDIT_SCORE >= 750 then APPROVE
    if CREDIT_SCORE < 600 then REJECT', 'PROD', '2024-01-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'valid', 1, 'legacy'),

(1, 'legacyAgeCheck', 'Legacy age verification', 'rule legacyAgeCheck:
    if AGE < 18 then REJECT
    if AGE >= 65 then MANUAL_REVIEW', 'PROD', '2024-01-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'valid', 1, 'legacy'),

(1, 'legacyTransactionCheck', 'Legacy transaction check', 'rule legacyTransactionCheck:
    if TRANSACTION_AMOUNT > 10000 then MANUAL_REVIEW
    if MERCHANT_CATEGORY = "HIGH_RISK" then DECLINE', 'PROD', '2024-01-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'valid', 1, 'legacy');

-- Insert additional modern rules for AMEX
INSERT INTO rules (process_area_id, name, description, content, status, effective_date, created_at, updated_at, created_by, updated_by, validation_status, version, schema_version) VALUES
(1, 'creditScoreThreshold', 'Credit score validation for applications', 'rule creditScoreThreshold:
    if applicant.creditScore >= 750 then approveApplication
    if applicant.creditScore < 600 then rejectApplication', 'PROD', '2024-01-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'pending', 1, 'modern'),

(1, 'incomeVerification', 'Annual income verification rules', 'rule incomeVerification:
    if applicant.annualIncome >= 75000 then instantApproval
    if applicant.annualIncome < 30000 then rejectApplication', 'VALID', '2024-02-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'pending', 1, 'modern');

-- Insert sample Rules for AMEX Fraud Detection
INSERT INTO rules (process_area_id, name, description, content, status, effective_date, created_at, updated_at, created_by, updated_by, validation_status, version, schema_version) VALUES
(4, 'velocityCheck', 'Transaction velocity monitoring', 'rule velocityCheck:
    if transaction.dailyCount > 15 then flagForReview
    if transaction.amount > 5000 then manualReview', 'PROD', '2024-01-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'pending', 1, 'modern'),

(4, 'geographicAnomaly', 'Geographic anomaly detection', 'rule geographicAnomaly:
    if transaction.location != account.homeLocation then sendAlert', 'SCHD', '2024-01-15', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'pending', 1, 'modern');

-- Insert sample Rules for VISA Authorization
INSERT INTO rules (process_area_id, name, description, content, status, effective_date, created_at, updated_at, created_by, updated_by, validation_status, version, schema_version) VALUES
(9, 'merchantValidation', 'Merchant validation rules', 'rule merchantValidation:
    if merchant.riskLevel = "high" then declineTransaction
    if merchant.category = "gambling" then requireManualReview', 'PEND', '2024-02-15', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'pending', 1, 'modern');

-- Insert sample Rules for VISA Network Processing
INSERT INTO rules (process_area_id, name, description, content, status, effective_date, created_at, updated_at, created_by, updated_by, validation_status, version, schema_version) VALUES
(15, 'cardTypeEligibility', 'Card type eligibility assessment', 'rule cardTypeEligibility:
    if applicant.creditScore >= 800 then setLimit(25000)
    if applicant.age < 21 then conditionalApproval', 'DRAFT', '2024-03-01', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system', 'pending', 1, 'modern');

-- Insert sample Rule Lists for autocomplete and validation
INSERT INTO rule_lists (name, description, data_type, list_values, schema_version, created_at, updated_at, created_by, updated_by) VALUES
('applicationStatuses', 'Valid application status values', 'string', '["pending", "approved", "rejected", "conditional", "review"]', 'both', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system'),
('creditScoreRanges', 'Credit score range categories', 'string', '["excellent", "good", "fair", "poor"]', 'modern', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system'),
('merchantCategories', 'Merchant category codes', 'string', '["retail", "restaurant", "gas_station", "grocery", "online", "gambling", "cash_advance"]', 'both', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system'),
('transactionTypes', 'Valid transaction types', 'string', '["purchase", "cash_advance", "balance_transfer", "fee", "payment", "refund"]', 'both', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system'),
('riskLevels', 'Risk assessment levels', 'string', '["low", "medium", "high", "critical"]', 'both', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system'),
('fraudIndicators', 'Fraud detection indicators', 'string', '["velocity", "geographic", "behavioral", "device", "merchant", "amount"]', 'modern', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system'),
('businessDates', 'Business date validation values', 'string', '["business_date", "previous_business_date", "next_business_date", "weekend", "holiday"]', 'modern', '2025-01-01 00:00:00', '2025-01-01 00:00:00', 'system', 'system');

-- Create indexes for better performance
CREATE INDEX idx_rules_process_area ON rules(process_area_id);
CREATE INDEX idx_rules_status ON rules(status);
CREATE INDEX idx_rules_effective_date ON rules(effective_date);
CREATE INDEX idx_rules_schema_version ON rules(schema_version);
CREATE INDEX idx_process_areas_process_group ON process_areas(process_group_id);
CREATE INDEX idx_process_groups_client ON process_groups(client_id);
CREATE INDEX idx_rule_history_rule ON rule_history(rule_id);

-- Final data verification queries (optional - comment out for production)
-- SELECT 'Clients' as table_name, COUNT(*) as record_count FROM clients
-- UNION ALL
-- SELECT 'Process Groups', COUNT(*) FROM process_groups  
-- UNION ALL
-- SELECT 'Process Areas', COUNT(*) FROM process_areas
-- UNION ALL
-- SELECT 'Rules', COUNT(*) FROM rules
-- UNION ALL
-- SELECT 'Rule Lists', COUNT(*) FROM rule_lists;