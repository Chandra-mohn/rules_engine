#!/usr/bin/env python3
"""
Migrate lists from SQLite to JSON files
"""

import sqlite3
import json
from pathlib import Path
from datetime import datetime

def migrate_lists():
    """Migrate lists from SQLite to JSON files"""
    backend_path = Path(__file__).parent.parent
    db_path = backend_path / 'database' / 'rules.db'
    lists_path = backend_path / 'lists'
    lists_path.mkdir(exist_ok=True)

    print("🔄 Starting lists migration...")
    print(f"Database: {db_path}")
    print(f"Target: {lists_path}\n")

    if not db_path.exists():
        print("❌ Database not found - nothing to migrate")
        return 0

    # Connect to SQLite
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()

    # Check if table exists
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='rule_lists'")
    if not cursor.fetchone():
        print("ℹ️  No rule_lists table found - nothing to migrate")
        conn.close()
        return 0

    # Fetch all lists
    cursor.execute("SELECT * FROM rule_lists")
    lists = cursor.fetchall()

    if not lists:
        print("ℹ️  No lists found in database")
        conn.close()
        return 0

    migrated = 0
    for row in lists:
        try:
            # Parse values JSON
            values_data = row['values']
            if isinstance(values_data, str):
                try:
                    values = json.loads(values_data)
                except json.JSONDecodeError:
                    print(f"⚠️  Warning: Could not parse values for list '{row['name']}'")
                    values = []
            else:
                values = values_data or []

            list_data = {
                'list_name': row['name'],
                'display_name': row.get('display_name') or row['name'],
                'description': row.get('description') or '',
                'schema_version': row.get('schema_version') or 'v2',
                'values': values,
                'metadata': {
                    'created_at': row.get('created_at') or datetime.utcnow().isoformat(),
                    'updated_at': row.get('updated_at') or datetime.utcnow().isoformat(),
                    'version': 1
                }
            }

            # Write to file
            file_path = lists_path / f"{row['name']}.json"
            with open(file_path, 'w') as f:
                json.dump(list_data, f, indent=2)

            migrated += 1
            print(f"✅ Migrated list: {row['name']} ({len(values)} values)")

        except Exception as e:
            print(f"❌ Error migrating list '{row['name']}': {e}")

    conn.close()
    print(f"\n✅ Migration complete: {migrated}/{len(lists)} lists migrated")
    return migrated

if __name__ == '__main__':
    try:
        count = migrate_lists()
        exit(0 if count >= 0 else 1)
    except Exception as e:
        print(f"\n❌ Migration failed: {e}")
        exit(1)
