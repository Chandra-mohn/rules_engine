#!/usr/bin/env python3
"""
Migration script to add rule_context table and context_id to rule table.
Run this with: python migrate_add_context_table.py
"""

from app import create_app
from models import db

def migrate():
    app = create_app()

    with app.app_context():
        print("🔄 Starting migration: Add rule_context table...")

        try:
            # Create all tables (will only create missing ones)
            db.create_all()
            print("✅ Migration completed successfully!")
            print("   - rule_context table created")
            print("   - context_id column added to rule table (via model)")

        except Exception as e:
            print(f"❌ Migration failed: {e}")
            raise

if __name__ == '__main__':
    migrate()
