#!/usr/bin/env python3
"""
Migration script to consolidate schema_actions into rules table
Moves actions from schema_actions table into rules table with item_type='action'
"""

import sqlite3
import sys
import os
from datetime import datetime

def migrate_schema_actions():
    """Migrate actions from schema_actions to rules table with item_type='action'"""

    # Database path
    db_path = os.path.join(os.path.dirname(__file__), 'database', 'rules.db')

    if not os.path.exists(db_path):
        print(f"❌ Database not found at {db_path}")
        return False

    # Create backup
    backup_path = f"{db_path}.backup_actions_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    print(f"📦 Creating backup: {backup_path}")

    try:
        with open(db_path, 'rb') as src, open(backup_path, 'wb') as dst:
            dst.write(src.read())
        print("✅ Backup created successfully")
    except Exception as e:
        print(f"❌ Backup failed: {e}")
        return False

    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    try:
        print("🔍 Analyzing current data...")

        # Check if schema_actions table exists
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='schema_actions'")
        if not cursor.fetchone():
            print("⚠️ schema_actions table not found - migration not needed")
            return True

        # Get all actions from schema_actions
        cursor.execute("""
            SELECT schema_version, action_name, description, category, examples
            FROM schema_actions
        """)
        actions = cursor.fetchall()

        if not actions:
            print("⚠️ No actions found in schema_actions table")
            return True

        print(f"📊 Found {len(actions)} actions to migrate")

        # Check if we need process_area_id (rules table requires it)
        cursor.execute("SELECT id FROM process_areas LIMIT 1")
        default_process_area = cursor.fetchone()

        if not default_process_area:
            print("❌ No process_areas found - cannot migrate actions (rules table requires process_area_id)")
            return False

        default_process_area_id = default_process_area[0]
        print(f"📍 Using process_area_id: {default_process_area_id}")

        # Insert actions into rules table
        migrated_count = 0
        skipped_count = 0

        for schema_version, action_name, description, category, examples in actions:
            # Check if action already exists in rules table
            cursor.execute("""
                SELECT id FROM rules
                WHERE name = ? AND item_type = 'action'
            """, (action_name,))

            if cursor.fetchone():
                print(f"⏭️ Skipping {action_name} - already exists in rules table")
                skipped_count += 1
                continue

            # Create content for the action (simplified format)
            content = f"action {action_name}: {description or 'No description'}"

            # Insert into rules table
            cursor.execute("""
                INSERT INTO rules (
                    process_area_id, name, description, content, status,
                    item_type, schema_version, created_by, updated_by
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (
                default_process_area_id, action_name, description, content,
                'VALID', 'action', schema_version, 'migration', 'migration'
            ))

            print(f"✅ Migrated: {action_name} ({schema_version})")
            migrated_count += 1

        print(f"\n📈 Migration Summary:")
        print(f"   ✅ Migrated: {migrated_count} actions")
        print(f"   ⏭️ Skipped: {skipped_count} actions (already existed)")

        # Verify migration
        cursor.execute("SELECT COUNT(*) FROM rules WHERE item_type = 'action'")
        total_actions = cursor.fetchone()[0]
        print(f"   📊 Total actions in rules table: {total_actions}")

        conn.commit()

        # Drop schema_actions table (commented out for safety)
        print(f"\n⚠️ Ready to drop schema_actions table")
        print(f"   Run manually: DROP TABLE schema_actions;")
        print(f"   After verifying migration was successful")

        return True

    except Exception as e:
        print(f"❌ Migration failed: {e}")
        conn.rollback()
        return False

    finally:
        conn.close()

def verify_migration():
    """Verify the migration was successful"""
    db_path = os.path.join(os.path.dirname(__file__), 'database', 'rules.db')

    if not os.path.exists(db_path):
        print(f"❌ Database not found at {db_path}")
        return False

    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    try:
        # Check actions in rules table
        cursor.execute("""
            SELECT item_type, COUNT(*)
            FROM rules
            GROUP BY item_type
        """)

        print("📊 Rules table breakdown:")
        for item_type, count in cursor.fetchall():
            print(f"   {item_type}: {count}")

        # Show some sample actions
        cursor.execute("""
            SELECT name, description, schema_version
            FROM rules
            WHERE item_type = 'action'
            LIMIT 5
        """)

        print("\n📝 Sample migrated actions:")
        for name, description, schema_version in cursor.fetchall():
            print(f"   {name} ({schema_version}): {description}")

        return True

    except Exception as e:
        print(f"❌ Verification failed: {e}")
        return False

    finally:
        conn.close()

if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--verify":
        verify_migration()
    else:
        print("🚀 Starting schema_actions migration...")
        success = migrate_schema_actions()

        if success:
            print("\n✅ Migration completed successfully!")
            print("🔍 Run with --verify to check results")
        else:
            print("\n❌ Migration failed!")
            sys.exit(1)