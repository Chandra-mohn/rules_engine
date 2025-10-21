"""
Schema File Service

File-based operations for schema entities and attributes.
Replaces SchemaEntity and SchemaAttribute database tables.
"""

from pathlib import Path
import json
from typing import Dict, List, Optional
import logging

logger = logging.getLogger(__name__)


class SchemaFileService:
    """Service for managing schema entities stored as JSON files."""

    def __init__(self, base_path: str = None):
        """
        Initialize the schema file service.

        Args:
            base_path: Base directory for schema files (default: test_data/schemas)
        """
        if base_path is None:
            current_dir = Path(__file__).parent.parent
            base_path = current_dir / 'test_data' / 'schemas'

        self.base_path = Path(base_path)
        self.base_path.mkdir(parents=True, exist_ok=True)
        logger.info(f"SchemaFileService initialized with base_path: {self.base_path}")

    def get_schema(self, name: str) -> Optional[Dict]:
        """
        Get a schema entity by name.

        Args:
            name: Schema entity name (e.g., 'applicant', 'transaction')

        Returns:
            Schema dictionary or None if not found
        """
        schema_file = self.base_path / f'{name}.json'

        if not schema_file.exists():
            logger.warning(f"Schema not found: {name}")
            return None

        try:
            with open(schema_file, 'r') as f:
                schema = json.load(f)
            logger.info(f"Loaded schema: {name}")
            return schema
        except Exception as e:
            logger.error(f"Error loading schema {name}: {e}")
            raise

    def list_schemas(self) -> List[Dict]:
        """
        List all schema entities.

        Returns:
            List of schema dictionaries
        """
        schemas = []

        if not self.base_path.exists():
            return schemas

        for schema_file in sorted(self.base_path.glob('*.json')):
            try:
                with open(schema_file, 'r') as f:
                    schema = json.load(f)
                schemas.append(schema)
            except Exception as e:
                logger.error(f"Error loading schema from {schema_file}: {e}")

        logger.info(f"Listed {len(schemas)} schemas")
        return schemas

    def save_schema(self, schema_data: Dict) -> Dict:
        """
        Save or update a schema entity.

        Args:
            schema_data: Schema dictionary with name and attributes

        Returns:
            Saved schema dictionary
        """
        name = schema_data.get('name')
        if not name:
            raise ValueError("Schema name is required")

        schema_file = self.base_path / f'{name}.json'

        try:
            with open(schema_file, 'w') as f:
                json.dump(schema_data, f, indent=2)
            logger.info(f"Saved schema: {name}")
            return schema_data
        except Exception as e:
            logger.error(f"Error saving schema {name}: {e}")
            raise

    def delete_schema(self, name: str) -> bool:
        """
        Delete a schema entity.

        Args:
            name: Schema entity name

        Returns:
            True if deleted, False if not found
        """
        schema_file = self.base_path / f'{name}.json'

        if not schema_file.exists():
            logger.warning(f"Schema not found for deletion: {name}")
            return False

        try:
            schema_file.unlink()
            logger.info(f"Deleted schema: {name}")
            return True
        except Exception as e:
            logger.error(f"Error deleting schema {name}: {e}")
            raise

    def get_attribute(self, schema_name: str, attribute_name: str) -> Optional[Dict]:
        """
        Get a specific attribute from a schema.

        Args:
            schema_name: Schema entity name
            attribute_name: Attribute name

        Returns:
            Attribute dictionary or None if not found
        """
        schema = self.get_schema(schema_name)
        if not schema:
            return None

        attributes = schema.get('attributes', [])
        for attr in attributes:
            if attr.get('name') == attribute_name:
                return attr

        return None

    def get_attributes(self, schema_name: str) -> List[Dict]:
        """
        Get all attributes for a schema.

        Args:
            schema_name: Schema entity name

        Returns:
            List of attribute dictionaries
        """
        schema = self.get_schema(schema_name)
        if not schema:
            return []

        return schema.get('attributes', [])

    def get_schema_count(self) -> int:
        """
        Get total number of schemas.

        Returns:
            Number of schema files
        """
        if not self.base_path.exists():
            return 0

        return len(list(self.base_path.glob('*.json')))
