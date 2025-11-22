#!/usr/bin/env python3
"""
Migrate schemas from SQLite to JSON files
"""

import sqlite3
import json
from pathlib import Path
from datetime import datetime

def migrate_schemas():
    """Migrate schemas from SQLite to JSON files"""
    backend_path = Path(__file__).parent.parent
    db_path = backend_path / 'database' / 'rules.db'
    schemas_path = backend_path / 'schemas'
    entities_path = schemas_path / 'entities'
    versions_path = schemas_path / 'versions'

    entities_path.mkdir(parents=True, exist_ok=True)
    versions_path.mkdir(parents=True, exist_ok=True)

    print("🔄 Starting schemas migration...")
    print(f"Database: {db_path}")
    print(f"Target: {schemas_path}\n")

    if not db_path.exists():
        print("❌ Database not found - nothing to migrate")
        return 0

    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()

    # Check if tables exist
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='schema_entities'")
    if not cursor.fetchone():
        print("ℹ️  No schema_entities table - nothing to migrate")
        conn.close()
        return 0

    # Migrate entities
    cursor.execute("SELECT * FROM schema_entities")
    entities = cursor.fetchall()

    entity_count = 0
    for entity_row in entities:
        try:
            # Get attributes for this entity
            cursor.execute("""
                SELECT * FROM schema_attributes
                WHERE entity_id = ?
            """, (entity_row['id'],))
            attributes = cursor.fetchall()

            entity_data = {
                'entity_name': entity_row['name'],
                'display_name': entity_row['display_name'] if entity_row['display_name'] else entity_row['name'],
                'description': entity_row['description'] if entity_row['description'] else '',
                'schema_version': entity_row['schema_version'] if entity_row['schema_version'] else 'v2',
                'attributes': [
                    {
                        'name': attr['name'],
                        'type': attr['type'],
                        'description': attr['description'] if attr['description'] else '',
                        'required': bool(attr['required']) if attr['required'] else False,
                        'default': attr['default_value'] if 'default_value' in attr.keys() else None
                    }
                    for attr in attributes
                ],
                'metadata': {
                    'created_at': entity_row['created_at'] if entity_row['created_at'] else datetime.utcnow().isoformat(),
                    'updated_at': entity_row['updated_at'] if entity_row['updated_at'] else datetime.utcnow().isoformat()
                }
            }

            file_path = entities_path / f"{entity_row['name']}.json"
            with open(file_path, 'w') as f:
                json.dump(entity_data, f, indent=2)

            entity_count += 1
            print(f"✅ Migrated entity: {entity_row['name']} ({len(attributes)} attributes)")

        except Exception as e:
            print(f"❌ Error migrating entity '{entity_row['name']}': {e}")

    # Migrate versions
    version_count = 0
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='schema_versions'")
    if cursor.fetchone():
        cursor.execute("SELECT * FROM schema_versions")
        versions = cursor.fetchall()

        for ver_row in versions:
            try:
                # Parse JSON fields if they exist
                entities_list = []
                actions_list = []

                if ver_row['entities']:
                    try:
                        entities_list = json.loads(ver_row['entities']) if isinstance(ver_row['entities'], str) else ver_row['entities']
                    except:
                        pass

                if ver_row['actions']:
                    try:
                        actions_list = json.loads(ver_row['actions']) if isinstance(ver_row['actions'], str) else ver_row['actions']
                    except:
                        pass

                version_data = {
                    'version_id': ver_row['version_id'],
                    'display_name': ver_row['display_name'] if ver_row['display_name'] else ver_row['version_id'],
                    'description': ver_row['description'] if ver_row['description'] else '',
                    'is_active': bool(ver_row['is_active']) if ver_row['is_active'] else True,
                    'entities': entities_list,
                    'actions': actions_list,
                    'created_at': ver_row['created_at'] if ver_row['created_at'] else datetime.utcnow().isoformat()
                }

                file_path = versions_path / f"{ver_row['version_id']}.json"
                with open(file_path, 'w') as f:
                    json.dump(version_data, f, indent=2)

                version_count += 1
                print(f"✅ Migrated version: {ver_row['version_id']}")

            except Exception as e:
                print(f"❌ Error migrating version '{ver_row['version_id']}': {e}")

    conn.close()
    print(f"\n✅ Schema migration complete: {entity_count} entities, {version_count} versions")
    return entity_count + version_count

if __name__ == '__main__':
    try:
        count = migrate_schemas()
        exit(0 if count >= 0 else 1)
    except Exception as e:
        print(f"\n❌ Migration failed: {e}")
        exit(1)
