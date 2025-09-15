#!/usr/bin/env python3
"""
ActionSets to Rules Migration Script

This script migrates ActionSets data from the 'actionsets' table
to the 'rules' table with item_type='actionset'.

Zero regression approach:
- Preserves all existing Rules data
- Migrates ActionSets to Rules table
- Preserves ActionSet history
"""

import os
import sys
from datetime import datetime

# Add backend directory to path
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from app import create_app
from models import db, Rule, RuleHistory

app = create_app()

# Note: This migration script is no longer needed since ActionSets are now
# created directly as Rules with item_type='actionset' in app.py

def migrate_actionsets_to_rules():
    """Migrate ActionSets to Rules table with item_type='actionset'"""

    with app.app_context():
        try:
            print("Starting ActionSets migration...")

            # Check if ActionSet table exists and has data
            try:
                actionsets = ActionSet.query.all()
                print(f"Found {len(actionsets)} ActionSets to migrate")
            except Exception as e:
                print(f"No ActionSets table found or no data: {e}")
                return

            # Check current Rules count
            rules_count_before = Rule.query.count()
            print(f"Current Rules count: {rules_count_before}")

            migrated_count = 0

            for actionset in actionsets:
                try:
                    # Check if this ActionSet was already migrated
                    existing_rule = Rule.query.filter_by(
                        process_area_id=actionset.process_area_id,
                        name=actionset.name,
                        item_type='actionset'
                    ).first()

                    if existing_rule:
                        print(f"ActionSet '{actionset.name}' already migrated, skipping...")
                        continue

                    # Create new Rule from ActionSet data
                    new_rule = Rule(
                        process_area_id=actionset.process_area_id,
                        name=actionset.name,
                        description=actionset.description,
                        content=actionset.content,
                        status=actionset.status,
                        effective_date=actionset.effective_date,
                        expiry_date=actionset.expiry_date,
                        created_at=actionset.created_at,
                        updated_at=actionset.updated_at,
                        created_by=actionset.created_by,
                        updated_by=actionset.updated_by,
                        validation_message=actionset.validation_message,
                        version=actionset.version,
                        schema_version=actionset.schema_version,
                        item_type='actionset'  # This is the key field
                    )

                    db.session.add(new_rule)
                    db.session.flush()  # Get the new rule ID

                    # Migrate ActionSet history to Rule history
                    try:
                        actionset_history = actionset.history
                        for history_item in actionset_history:
                            new_history = RuleHistory(
                                rule_id=new_rule.id,
                                version=history_item.version,
                                content=history_item.content,
                                status=history_item.status,
                                validation_message=history_item.validation_message,
                                created_at=history_item.created_at,
                                created_by=history_item.created_by,
                                schema_version=history_item.schema_version
                            )
                            db.session.add(new_history)
                    except Exception as e:
                        print(f"Warning: Could not migrate history for ActionSet '{actionset.name}': {e}")

                    migrated_count += 1
                    print(f"Migrated ActionSet '{actionset.name}' (ID: {actionset.id} -> {new_rule.id})")

                except Exception as e:
                    print(f"Error migrating ActionSet '{actionset.name}': {e}")
                    db.session.rollback()
                    continue

            # Commit all changes
            db.session.commit()

            # Verify migration
            rules_count_after = Rule.query.count()
            actionsets_as_rules = Rule.query.filter_by(item_type='actionset').count()

            print(f"\nMigration completed successfully!")
            print(f"Rules before migration: {rules_count_before}")
            print(f"Rules after migration: {rules_count_after}")
            print(f"ActionSets migrated: {migrated_count}")
            print(f"ActionSets as Rules: {actionsets_as_rules}")

            # Display sample migrated data
            print(f"\nSample migrated ActionSets:")
            sample_actionsets = Rule.query.filter_by(item_type='actionset').limit(3).all()
            for rule in sample_actionsets:
                print(f"  - {rule.name} (ID: {rule.id}, Status: {rule.status})")

        except Exception as e:
            print(f"Migration failed: {e}")
            db.session.rollback()
            raise

if __name__ == "__main__":
    print("ActionSets to Rules Migration Script")
    print("===================================")

    # Run migration
    migrate_actionsets_to_rules()