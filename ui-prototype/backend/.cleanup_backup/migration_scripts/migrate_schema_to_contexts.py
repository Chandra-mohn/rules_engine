#!/usr/bin/env python3
"""
Migration script to convert schema_entities and schema_attributes to schema template contexts.
This creates RuleContext records with is_schema_template=True based on existing schema tables.

Run this with: python migrate_schema_to_contexts.py
"""

from app import create_app
from models import db, RuleContext
import json


def migrate_schema_to_contexts():
    """Convert existing schema tables to schema template contexts."""
    app = create_app()

    with app.app_context():
        print("🔄 Starting schema-to-context migration...")

        try:
            # Query existing schema entities and attributes
            entities_query = """
                SELECT id, name, description, client_id, version
                FROM schema_entities
                WHERE is_active = 1
            """
            entities = db.session.execute(db.text(entities_query)).fetchall()

            if not entities:
                print("ℹ️  No active schema entities found. Nothing to migrate.")
                return

            # For each entity group, create a schema template context
            for entity in entities:
                entity_id, entity_name, entity_desc, client_id, version = entity

                # Get all attributes for this entity
                attributes_query = """
                    SELECT name, data_type, java_type, min_value, max_value,
                           allowed_values, is_required, description
                    FROM schema_attributes
                    WHERE entity_id = :entity_id
                    ORDER BY name
                """
                attributes = db.session.execute(
                    db.text(attributes_query),
                    {"entity_id": entity_id}
                ).fetchall()

                # Build the context_data structure
                context_data = {entity_name: {}}

                # Add sample values and metadata for each attribute
                for attr in attributes:
                    attr_name, data_type, java_type, min_val, max_val, allowed_values, is_required, desc = attr

                    # Determine sample value based on data_type
                    sample_value = get_sample_value(data_type, min_val, max_val, allowed_values)

                    # Add sample value
                    context_data[entity_name][attr_name] = sample_value

                # Add _metadata field with schema information
                context_data[entity_name]["_metadata"] = {}
                for attr in attributes:
                    attr_name, data_type, java_type, min_val, max_val, allowed_values, is_required, desc = attr

                    metadata = {
                        "type": map_data_type(data_type),
                        "description": desc or f"{attr_name} field"
                    }

                    # Add range if min/max values exist
                    if min_val is not None or max_val is not None:
                        metadata["range"] = [min_val, max_val]

                    # Add enum if allowed_values exist
                    if allowed_values:
                        try:
                            metadata["enum"] = json.loads(allowed_values)
                        except:
                            pass

                    # Add required flag
                    if is_required:
                        metadata["required"] = True

                    # Add Java type mapping
                    if java_type:
                        metadata["javaType"] = java_type

                    context_data[entity_name]["_metadata"][attr_name] = metadata

                # Create schema template context
                schema_name = f"Schema Template - {entity_name.title()}"
                if version:
                    schema_name += f" v{version}"

                # Check if context already exists
                existing = RuleContext.query.filter_by(name=schema_name).first()
                if existing:
                    print(f"   ⚠️  Schema template '{schema_name}' already exists (ID: {existing.id}). Skipping.")
                    continue

                # Create new context
                new_context = RuleContext(
                    name=schema_name,
                    description=f"Schema template migrated from {entity_name} entity: {entity_desc or 'No description'}",
                    context_data=context_data,
                    is_schema_template=True,
                    version=str(version) if version else "1.0",
                    client_id=client_id
                )

                db.session.add(new_context)
                db.session.flush()  # Get the ID

                print(f"   ✅ Created schema template: '{schema_name}' (ID: {new_context.id})")
                print(f"      - Entity: {entity_name}")
                print(f"      - Attributes: {len(attributes)}")

            db.session.commit()
            print("\n✅ Migration completed successfully!")
            print("\n📊 Summary:")
            print(f"   - Schema entities processed: {len(entities)}")
            print("\n💡 Note: Original schema_entities and schema_attributes tables are preserved.")
            print("   You can safely delete them after verifying the migration.")

        except Exception as e:
            db.session.rollback()
            print(f"❌ Migration failed: {e}")
            raise


def get_sample_value(data_type, min_val, max_val, allowed_values):
    """Generate a reasonable sample value based on data type and constraints."""
    if allowed_values:
        try:
            values = json.loads(allowed_values)
            return values[0] if values else None
        except:
            pass

    if data_type == "number":
        if min_val is not None:
            return int(min_val) if min_val == int(min_val) else float(min_val)
        return 0

    if data_type == "string":
        return "sample_value"

    if data_type == "date":
        return "2025-01-01"

    if data_type == "boolean":
        return False

    return None


def map_data_type(data_type):
    """Map schema data types to JSON Schema types."""
    type_mapping = {
        "number": "number",
        "string": "string",
        "date": "string",  # ISO date string
        "boolean": "boolean",
    }
    return type_mapping.get(data_type, "string")


if __name__ == '__main__':
    migrate_schema_to_contexts()
