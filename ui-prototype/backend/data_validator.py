#!/usr/bin/env python3
"""
Database Data Validator
Adds constraints and validation to prevent field corruption
"""

import sqlite3
import json
from datetime import datetime

class DataValidator:
    def __init__(self, db_path="database/rules.db"):
        self.db_path = db_path

    def add_data_constraints(self):
        """Add database constraints to prevent bad data"""
        print("Adding database constraints...")

        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        try:
            # Add check constraint for status values
            cursor.execute("""
                CREATE TABLE IF NOT EXISTS rules_new (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    process_area_id INTEGER NOT NULL,
                    name VARCHAR(100) NOT NULL UNIQUE,
                    description TEXT,
                    content TEXT NOT NULL,
                    status VARCHAR(10) DEFAULT 'DRAFT' CHECK (status IN ('DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD', 'deleted')),
                    effective_date DATE,
                    expiry_date DATE,
                    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                    created_by VARCHAR(50) DEFAULT 'system',
                    updated_by VARCHAR(50) DEFAULT 'system',
                    validation_message TEXT,
                    version INTEGER DEFAULT 1,
                    schema_version VARCHAR(20) DEFAULT 'modern' CHECK (schema_version IN ('modern', 'legacy'))
                );
            """)

            # Copy existing data
            cursor.execute("""
                INSERT INTO rules_new
                SELECT * FROM rules;
            """)

            # Rename tables
            cursor.execute("DROP TABLE rules;")
            cursor.execute("ALTER TABLE rules_new RENAME TO rules;")

            conn.commit()
            print("✅ Database constraints added successfully")

        except sqlite3.Error as e:
            print(f"⚠️  Constraints may already exist or error occurred: {str(e)}")
        finally:
            conn.close()

    def validate_existing_data(self):
        """Validate existing data for corruption"""
        print("Validating existing data...")

        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        issues = []

        try:
            # Check for invalid status values
            cursor.execute("""
                SELECT id, name, status
                FROM rules
                WHERE status NOT IN ('DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD', 'deleted')
            """)
            invalid_statuses = cursor.fetchall()
            for rule_id, name, status in invalid_statuses:
                issues.append(f"Rule {name} (ID: {rule_id}) has invalid status: {status}")

            # Check for null required fields
            cursor.execute("SELECT id, name FROM rules WHERE name IS NULL OR name = ''")
            null_names = cursor.fetchall()
            for rule_id, name in null_names:
                issues.append(f"Rule ID {rule_id} has null or empty name")

            # Check for missing process_area_id
            cursor.execute("SELECT id, name FROM rules WHERE process_area_id IS NULL")
            null_areas = cursor.fetchall()
            for rule_id, name in null_areas:
                issues.append(f"Rule {name} (ID: {rule_id}) has null process_area_id")

            # Check for lowercase status regression
            cursor.execute("SELECT id, name, status FROM rules WHERE status IN ('active', 'draft', 'inactive')")
            lowercase_statuses = cursor.fetchall()
            for rule_id, name, status in lowercase_statuses:
                issues.append(f"REGRESSION: Rule {name} (ID: {rule_id}) has lowercase status: {status}")

        except sqlite3.Error as e:
            issues.append(f"Database validation error: {str(e)}")
        finally:
            conn.close()

        if issues:
            print(f"❌ Found {len(issues)} data validation issues:")
            for issue in issues:
                print(f"  - {issue}")
            return False
        else:
            print("✅ All data validation checks passed")
            return True

    def create_data_snapshot(self):
        """Create a snapshot of current data state"""
        print("Creating data snapshot...")

        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()

        try:
            # Get current rules data
            cursor.execute("""
                SELECT id, name, status, effective_date, schema_version, process_area_id
                FROM rules
                WHERE status NOT IN ('deleted')
                ORDER BY id
            """)
            rules = cursor.fetchall()

            snapshot = {
                'timestamp': datetime.now().isoformat(),
                'active_rules_count': len(rules),
                'rules': [
                    {
                        'id': r[0], 'name': r[1], 'status': r[2],
                        'effective_date': r[3], 'schema_version': r[4],
                        'process_area_id': r[5]
                    }
                    for r in rules
                ]
            }

            # Save snapshot
            with open('data_snapshot.json', 'w') as f:
                json.dump(snapshot, f, indent=2)

            print(f"✅ Data snapshot created: {len(rules)} active rules")
            return True

        except Exception as e:
            print(f"❌ Failed to create snapshot: {str(e)}")
            return False
        finally:
            conn.close()

def main():
    validator = DataValidator()

    print("=" * 50)
    print("DATA VALIDATION AND CONSTRAINT SETUP")
    print("=" * 50)

    # Create snapshot first
    validator.create_data_snapshot()

    # Add constraints
    validator.add_data_constraints()

    # Validate existing data
    if validator.validate_existing_data():
        print("\n🎉 Data validation completed successfully")
    else:
        print("\n🚨 Data validation found issues - please review")

if __name__ == "__main__":
    main()