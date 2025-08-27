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

-- Indexes for performance
CREATE INDEX idx_rules_name ON rules(name);
CREATE INDEX idx_rules_status ON rules(status);
CREATE INDEX idx_rules_updated_at ON rules(updated_at);
CREATE INDEX idx_rule_history_rule_id ON rule_history(rule_id);

-- Sample data
INSERT INTO rules (name, description, content, status, validation_status) VALUES 
('creditScoreCheck', 'Basic credit score validation', 'rule creditScoreCheck:\n    if applicant.creditScore >= 700 then approveApplication\n    if applicant.creditScore < 600 then rejectApplication', 'active', 'valid'),
('ageVerification', 'Minimum age requirement', 'rule ageVerification:\n    if applicant.age < 18 then rejectApplication\n    if applicant.age >= 18 then approveApplication', 'active', 'valid'),
('businessDateCheck', 'Business date validation example', 'rule businessDateCheck:\n    if applicant.applicationDate after business_date then conditionalApproval\n    if applicant.applicationDate before business_date then rejectApplication', 'draft', 'pending');