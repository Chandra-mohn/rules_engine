-- Migration: Add rule_context table and update rule table
-- Purpose: Replace schema_entity/schema_attribute with unified context system
-- Date: 2025-10-09

-- Create rule_context table
CREATE TABLE IF NOT EXISTS rule_context (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR(200) NOT NULL UNIQUE,
    description TEXT,
    context_data JSON NOT NULL,

    -- Schema template flag: TRUE for master schemas, FALSE for test contexts
    is_schema_template BOOLEAN DEFAULT 0,

    -- Version tracking
    version VARCHAR(50),

    -- Optional scoping to specific client
    client_id INTEGER,

    -- Timestamps
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    -- Foreign keys
    FOREIGN KEY (client_id) REFERENCES client(id) ON DELETE SET NULL
);

-- Create index for faster lookups
CREATE INDEX IF NOT EXISTS idx_rule_context_is_schema_template
    ON rule_context(is_schema_template);
CREATE INDEX IF NOT EXISTS idx_rule_context_client_id
    ON rule_context(client_id);
CREATE INDEX IF NOT EXISTS idx_rule_context_version
    ON rule_context(version);

-- Add context_id to rule table
ALTER TABLE rule ADD COLUMN context_id INTEGER
    REFERENCES rule_context(id) ON DELETE SET NULL;

-- Create index for rule-context relationship
CREATE INDEX IF NOT EXISTS idx_rule_context_id
    ON rule(context_id);

-- Create trigger to update updated_at timestamp
CREATE TRIGGER IF NOT EXISTS rule_context_updated_at
    AFTER UPDATE ON rule_context
BEGIN
    UPDATE rule_context SET updated_at = CURRENT_TIMESTAMP
    WHERE id = NEW.id;
END;
