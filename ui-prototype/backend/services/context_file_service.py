"""
Context File Service

File-based operations for rule contexts (test data and schema templates).
Replaces RuleContext database table.
"""

from pathlib import Path
import json
from typing import Dict, List, Optional
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


class ContextFileService:
    """Service for managing rule contexts stored as JSON files."""

    def __init__(self, base_path: str = None):
        """
        Initialize the context file service.

        Args:
            base_path: Base directory for context files (default: test_data/contexts)
        """
        if base_path is None:
            current_dir = Path(__file__).parent.parent
            base_path = current_dir / 'test_data' / 'contexts'

        self.base_path = Path(base_path)
        self.base_path.mkdir(parents=True, exist_ok=True)

        # Schema templates subdirectory
        self.templates_path = self.base_path / 'schema-templates'
        self.templates_path.mkdir(parents=True, exist_ok=True)

        logger.info(f"ContextFileService initialized with base_path: {self.base_path}")

    def _get_context_path(self, name: str, is_template: bool = False) -> Path:
        """Get file path for a context."""
        # Sanitize name for filename
        filename = name.lower().replace(' ', '-').replace('_', '-') + '.json'

        if is_template:
            return self.templates_path / filename
        return self.base_path / filename

    def get_context(self, name: str) -> Optional[Dict]:
        """
        Get a context by name.

        Args:
            name: Context name

        Returns:
            Context dictionary or None if not found
        """
        # Try regular contexts first
        context_file = self._get_context_path(name, is_template=False)
        if context_file.exists():
            try:
                with open(context_file, 'r') as f:
                    context = json.load(f)
                logger.info(f"Loaded context: {name}")
                return context
            except Exception as e:
                logger.error(f"Error loading context {name}: {e}")
                raise

        # Try templates
        template_file = self._get_context_path(name, is_template=True)
        if template_file.exists():
            try:
                with open(template_file, 'r') as f:
                    context = json.load(f)
                logger.info(f"Loaded context template: {name}")
                return context
            except Exception as e:
                logger.error(f"Error loading context template {name}: {e}")
                raise

        logger.warning(f"Context not found: {name}")
        return None

    def list_contexts(self, is_schema_template: Optional[bool] = None,
                     client_code: Optional[str] = None,
                     search: Optional[str] = None) -> List[Dict]:
        """
        List contexts with optional filtering.

        Args:
            is_schema_template: Filter by template status
            client_code: Filter by client code
            search: Search in name and description

        Returns:
            List of context dictionaries
        """
        contexts = []

        # Scan regular contexts
        if is_schema_template is None or not is_schema_template:
            for context_file in sorted(self.base_path.glob('*.json')):
                try:
                    with open(context_file, 'r') as f:
                        context = json.load(f)

                    # Apply filters
                    if client_code and context.get('client_code') != client_code:
                        continue

                    if search:
                        search_lower = search.lower()
                        name_match = search_lower in context.get('name', '').lower()
                        desc_match = search_lower in context.get('description', '').lower()
                        if not (name_match or desc_match):
                            continue

                    contexts.append(context)
                except Exception as e:
                    logger.error(f"Error loading context from {context_file}: {e}")

        # Scan templates
        if is_schema_template is None or is_schema_template:
            for template_file in sorted(self.templates_path.glob('*.json')):
                try:
                    with open(template_file, 'r') as f:
                        context = json.load(f)

                    # Apply filters (templates don't have client_code usually)
                    if search:
                        search_lower = search.lower()
                        name_match = search_lower in context.get('name', '').lower()
                        desc_match = search_lower in context.get('description', '').lower()
                        if not (name_match or desc_match):
                            continue

                    contexts.append(context)
                except Exception as e:
                    logger.error(f"Error loading template from {template_file}: {e}")

        logger.info(f"Listed {len(contexts)} contexts")
        return contexts

    def save_context(self, context_data: Dict) -> Dict:
        """
        Save or update a context.

        Args:
            context_data: Context dictionary

        Returns:
            Saved context dictionary
        """
        name = context_data.get('name')
        if not name:
            raise ValueError("Context name is required")

        if 'context_data' not in context_data:
            raise ValueError("context_data field is required")

        # Add timestamps
        now = datetime.utcnow().isoformat()
        if 'created_at' not in context_data:
            context_data['created_at'] = now
        context_data['updated_at'] = now

        # Determine if template
        is_template = context_data.get('is_schema_template', False)
        context_file = self._get_context_path(name, is_template=is_template)

        try:
            with open(context_file, 'w') as f:
                json.dump(context_data, f, indent=2)
            logger.info(f"Saved context: {name}")
            return context_data
        except Exception as e:
            logger.error(f"Error saving context {name}: {e}")
            raise

    def delete_context(self, name: str) -> bool:
        """
        Delete a context.

        Args:
            name: Context name

        Returns:
            True if deleted, False if not found
        """
        # Try both locations
        regular_path = self._get_context_path(name, is_template=False)
        template_path = self._get_context_path(name, is_template=True)

        deleted = False

        if regular_path.exists():
            try:
                regular_path.unlink()
                logger.info(f"Deleted context: {name}")
                deleted = True
            except Exception as e:
                logger.error(f"Error deleting context {name}: {e}")
                raise

        if template_path.exists():
            try:
                template_path.unlink()
                logger.info(f"Deleted context template: {name}")
                deleted = True
            except Exception as e:
                logger.error(f"Error deleting context template {name}: {e}")
                raise

        if not deleted:
            logger.warning(f"Context not found for deletion: {name}")

        return deleted

    def clone_context(self, source_name: str, new_name: str, new_description: Optional[str] = None) -> Dict:
        """
        Clone an existing context with a new name.

        Args:
            source_name: Source context name
            new_name: New context name
            new_description: New description (optional)

        Returns:
            Cloned context dictionary
        """
        source_context = self.get_context(source_name)
        if not source_context:
            raise ValueError(f"Source context not found: {source_name}")

        # Create cloned context
        cloned_context = source_context.copy()
        cloned_context['name'] = new_name
        if new_description:
            cloned_context['description'] = new_description
        else:
            cloned_context['description'] = f"Cloned from {source_name}"

        # Remove timestamps (will be regenerated)
        cloned_context.pop('created_at', None)
        cloned_context.pop('updated_at', None)

        return self.save_context(cloned_context)

    def get_context_count(self, is_schema_template: Optional[bool] = None) -> int:
        """
        Get total number of contexts.

        Args:
            is_schema_template: Filter by template status

        Returns:
            Number of context files
        """
        count = 0

        if is_schema_template is None or not is_schema_template:
            count += len(list(self.base_path.glob('*.json')))

        if is_schema_template is None or is_schema_template:
            count += len(list(self.templates_path.glob('*.json')))

        return count
