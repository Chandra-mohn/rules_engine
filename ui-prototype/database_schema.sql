-- Rules Authoring UI Database Schema

CREATE TABLE rules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR(100) NOT NULL UNIQUE,
    description TEXT,
    content TEXT NOT NULL,
    status VARCHAR(20) DEFAULT 'draft', -- draft, active, inactive, error
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(50) DEFAULT 'system',
    updated_by VARCHAR(50) DEFAULT 'system',
    validation_status VARCHAR(20) DEFAULT 'pending', -- pending, valid, invalid
    validation_message TEXT,
    version INTEGER DEFAULT 1
);

CREATE TABLE rule_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    rule_id INTEGER NOT NULL,
    name VARCHAR(100) NOT NULL,
    content TEXT NOT NULL,
    version INTEGER NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(50) DEFAULT 'system',
    change_reason TEXT,
    FOREIGN KEY (rule_id) REFERENCES rules (id) ON DELETE CASCADE
);

-- Schema versioning tables for dual schema support
CREATE TABLE schema_versions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    version_name VARCHAR(20) NOT NULL UNIQUE, -- 'legacy' or 'modern'
    display_name VARCHAR(50) NOT NULL,        -- 'Legacy Rules' or 'Modern Rules'
    description TEXT,
    is_default BOOLEAN DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE schema_attributes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    schema_version VARCHAR(20) NOT NULL,
    entity_name VARCHAR(50),           -- 'applicant', 'transaction', 'account' (NULL for legacy)
    attribute_name VARCHAR(100) NOT NULL,
    data_type VARCHAR(20) NOT NULL,    -- 'number', 'string', 'date', 'boolean'
    description TEXT,
    examples TEXT,                     -- JSON array of example usage
    FOREIGN KEY (schema_version) REFERENCES schema_versions(version_name)
);

CREATE TABLE schema_actions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    schema_version VARCHAR(20) NOT NULL,
    action_name VARCHAR(100) NOT NULL,
    description TEXT,
    category VARCHAR(50),              -- 'approval', 'rejection', 'review', etc.
    examples TEXT,                     -- JSON array of example usage
    FOREIGN KEY (schema_version) REFERENCES schema_versions(version_name)
);

-- Add schema_version column to rules table
ALTER TABLE rules ADD COLUMN schema_version VARCHAR(20) DEFAULT 'modern';

-- Named lists/sets for IN clause support
CREATE TABLE rule_lists (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR(100) NOT NULL UNIQUE,
    description TEXT,
    data_type VARCHAR(20) NOT NULL,    -- 'string', 'number', 'boolean'
    list_values TEXT NOT NULL,         -- JSON array: ["value1", "value2"]
    schema_version VARCHAR(20) DEFAULT 'both',  -- 'modern', 'legacy', 'both'
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(50) DEFAULT 'system',
    updated_by VARCHAR(50) DEFAULT 'system'
);

-- Indexes for performance
CREATE INDEX idx_rules_name ON rules(name);
CREATE INDEX idx_rules_status ON rules(status);
CREATE INDEX idx_rules_updated_at ON rules(updated_at);
CREATE INDEX idx_rules_schema_version ON rules(schema_version);
CREATE INDEX idx_rule_history_rule_id ON rule_history(rule_id);
CREATE INDEX idx_schema_attributes_version ON schema_attributes(schema_version);
CREATE INDEX idx_schema_actions_version ON schema_actions(schema_version);
CREATE INDEX idx_rule_lists_name ON rule_lists(name);
CREATE INDEX idx_rule_lists_schema_version ON rule_lists(schema_version);

-- Schema versions data
INSERT INTO schema_versions (version_name, display_name, description, is_default) VALUES 
    ('modern', 'Modern Rules', 'Entity.property notation with structured attributes', 1),
    ('legacy', 'Legacy Rules', 'UPPERCASE attribute names for backward compatibility', 0);

-- Modern schema attributes (entity.property format)
INSERT INTO schema_attributes (schema_version, entity_name, attribute_name, data_type, description, examples) VALUES 
    ('modern', 'applicant', 'creditScore', 'number', 'Applicant credit score (300-850)', '["applicant.creditScore >= 750", "applicant.creditScore < 600"]'),
    ('modern', 'applicant', 'age', 'number', 'Applicant age in years', '["applicant.age >= 21", "applicant.age < 18"]'),
    ('modern', 'applicant', 'annualIncome', 'number', 'Annual income in dollars', '["applicant.annualIncome >= 50000", "applicant.annualIncome > 100000"]'),
    ('modern', 'applicant', 'employmentStatus', 'string', 'Current employment status', '["applicant.employmentStatus == \"EMPLOYED\"", "applicant.employmentStatus == \"SELF_EMPLOYED\""]'),
    ('modern', 'applicant', 'applicationDate', 'date', 'Application submission date', '["applicant.applicationDate after business_date", "applicant.applicationDate before business_date"]'),
    ('modern', 'transaction', 'amount', 'number', 'Transaction amount in dollars', '["transaction.amount > 1000", "transaction.amount <= 500"]'),
    ('modern', 'transaction', 'timestamp', 'date', 'Transaction timestamp', '["transaction.timestamp after business_date"]'),
    ('modern', 'account', 'currentBalance', 'number', 'Current account balance', '["account.currentBalance >= 1000", "account.currentBalance < 0"]'),
    ('modern', 'account', 'creditLimit', 'number', 'Account credit limit', '["account.creditLimit > 5000"]');

-- Legacy schema attributes (UPPERCASE format)
INSERT INTO schema_attributes (schema_version, entity_name, attribute_name, data_type, description, examples) VALUES 
    ('legacy', NULL, 'CREDIT_SCORE', 'number', 'Applicant credit score (300-850)', '["CREDIT_SCORE >= 750", "CREDIT_SCORE < 600"]'),
    ('legacy', NULL, 'APPLICANT_AGE', 'number', 'Applicant age in years', '["APPLICANT_AGE >= 21", "APPLICANT_AGE < 18"]'),
    ('legacy', NULL, 'ANNUAL_INCOME', 'number', 'Annual income in dollars', '["ANNUAL_INCOME >= 50000", "ANNUAL_INCOME > 100000"]'),
    ('legacy', NULL, 'EMPLOYMENT_STATUS', 'string', 'Current employment status', '["EMPLOYMENT_STATUS == \"EMPLOYED\"", "EMPLOYMENT_STATUS == \"SELF_EMPLOYED\""]'),
    ('legacy', NULL, 'APPLICATION_DATE', 'date', 'Application submission date', '["APPLICATION_DATE after business_date", "APPLICATION_DATE before business_date"]'),
    ('legacy', NULL, 'TRANSACTION_AMOUNT', 'number', 'Transaction amount in dollars', '["TRANSACTION_AMOUNT > 1000", "TRANSACTION_AMOUNT <= 500"]'),
    ('legacy', NULL, 'TRANSACTION_TIME', 'date', 'Transaction timestamp', '["TRANSACTION_TIME after business_date"]'),
    ('legacy', NULL, 'ACCOUNT_BALANCE', 'number', 'Current account balance', '["ACCOUNT_BALANCE >= 1000", "ACCOUNT_BALANCE < 0"]'),
    ('legacy', NULL, 'CREDIT_LIMIT', 'number', 'Account credit limit', '["CREDIT_LIMIT > 5000"]');

-- Modern schema actions
INSERT INTO schema_actions (schema_version, action_name, description, category, examples) VALUES 
    ('modern', 'approveApplication', 'Approve the application', 'approval', '["then approveApplication"]'),
    ('modern', 'rejectApplication', 'Reject the application', 'rejection', '["then rejectApplication"]'),
    ('modern', 'manualReview', 'Send for manual review', 'review', '["then manualReview"]'),
    ('modern', 'conditionalApproval', 'Approve with conditions', 'approval', '["then conditionalApproval"]'),
    ('modern', 'approveTransaction', 'Approve the transaction', 'approval', '["then approveTransaction"]'),
    ('modern', 'declineTransaction', 'Decline the transaction', 'rejection', '["then declineTransaction"]'),
    ('modern', 'flagForReview', 'Flag transaction for review', 'review', '["then flagForReview"]'),
    ('modern', 'sendAlert', 'Send alert notification', 'alert', '["then sendAlert"]');

-- Legacy schema actions (UPPERCASE format)
INSERT INTO schema_actions (schema_version, action_name, description, category, examples) VALUES 
    ('legacy', 'APPROVE', 'Approve the application/transaction', 'approval', '["then APPROVE"]'),
    ('legacy', 'REJECT', 'Reject the application/transaction', 'rejection', '["then REJECT"]'),
    ('legacy', 'REVIEW', 'Send for manual review', 'review', '["then REVIEW"]'),
    ('legacy', 'CONDITIONAL', 'Approve with conditions', 'approval', '["then CONDITIONAL"]'),
    ('legacy', 'ALLOW', 'Allow the transaction', 'approval', '["then ALLOW"]'),
    ('legacy', 'DECLINE', 'Decline the transaction', 'rejection', '["then DECLINE"]'),
    ('legacy', 'FLAG', 'Flag for review', 'review', '["then FLAG"]'),
    ('legacy', 'ALERT', 'Send alert notification', 'alert', '["then ALERT"]');

-- Sample data
INSERT INTO rules (name, description, content, status, validation_status, schema_version) VALUES 
('creditScoreCheck', 'Basic credit score validation', 'rule creditScoreCheck:
    if applicant.creditScore >= 700 then approveApplication
    if applicant.creditScore < 600 then rejectApplication', 'active', 'valid', 'modern'),
('ageVerification', 'Minimum age requirement', 'rule ageVerification:
    if applicant.age < 18 then rejectApplication
    if applicant.age >= 18 then approveApplication', 'active', 'valid', 'modern'),
('businessDateCheck', 'Business date validation example', 'rule businessDateCheck:
    if applicant.applicationDate after business_date then conditionalApproval
    if applicant.applicationDate before business_date then rejectApplication', 'draft', 'pending', 'modern'),
('legacyCreditCheck', 'Legacy style credit check', 'rule legacyCreditCheck:
    if CREDIT_SCORE >= 750 then APPROVE
    if CREDIT_SCORE < 600 then REJECT', 'active', 'valid', 'legacy'),
('legacyAgeCheck', 'Legacy style age verification', 'rule legacyAgeCheck:
    if APPLICANT_AGE < 18 then REJECT
    if APPLICANT_AGE >= 21 then APPROVE', 'active', 'valid', 'legacy'),
('legacyTransactionCheck', 'Legacy style transaction validation', 'rule legacyTransactionCheck:
    if TRANSACTION_AMOUNT > 10000 and ACCOUNT_BALANCE < TRANSACTION_AMOUNT then DECLINE
    if TRANSACTION_AMOUNT <= 1000 then ALLOW
    if TRANSACTION_AMOUNT > 5000 then REVIEW', 'active', 'valid', 'legacy');

-- Sample named lists data
INSERT INTO rule_lists (name, description, data_type, list_values, schema_version) VALUES 
    ('VALID_STATUSES', 'Allowed application statuses', 'string', '["ACTIVE", "PENDING", "REVIEW", "APPROVED"]', 'both'),
    ('PREMIUM_TIERS', 'Premium customer tiers', 'string', '["GOLD", "PLATINUM", "DIAMOND"]', 'modern'),
    ('APPROVED_SCORES', 'Pre-approved credit scores', 'number', '[750, 800, 850]', 'legacy'),
    ('RISK_COUNTRIES', 'High risk countries', 'string', '["XX", "YY", "ZZ"]', 'both'),
    ('WEEKEND_DAYS', 'Weekend day numbers', 'number', '[6, 7]', 'both'),
    ('BLOCKED_MERCHANTS', 'Blocked merchant categories', 'string', '["CASINO", "GAMBLING", "CRYPTO"]', 'both');