"""
File-based CRUD service for business rules.
Replaces SQLite with JSON files organized by client/process_group/process_area hierarchy.
"""

import json
import os
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime
import jsonschema
import logging

logger = logging.getLogger(__name__)


class RuleFileService:
    """Service for managing rules as JSON files in hierarchical folder structure."""

    def __init__(self, base_path: str = None):
        """
        Initialize the file service.

        Args:
            base_path: Base directory for rules storage. Defaults to backend/rules/
        """
        if base_path is None:
            current_dir = Path(__file__).parent.parent
            base_path = current_dir / "rules"

        self.base_path = Path(base_path)
        self.schema = self._load_schema()
        logger.info(f"RuleFileService initialized with base_path: {self.base_path}")

    def _load_schema(self) -> Dict:
        """Load JSON schema for validation."""
        schema_path = self.base_path / "schema.json"

        if not schema_path.exists():
            logger.warning(f"Schema file not found at {schema_path}, validation disabled")
            return None

        with open(schema_path, 'r') as f:
            return json.load(f)

    def _derive_hierarchy_from_path(self, file_path: Path) -> Dict:
        """
        Derive hierarchy information from file path.

        Args:
            file_path: Path to rule file

        Returns:
            Dictionary with hierarchy codes derived from path
        """
        try:
            parts = file_path.relative_to(self.base_path).parts
            if len(parts) >= 4:
                return {
                    'client_code': parts[0],
                    'process_group_code': parts[1],
                    'process_area_code': parts[2]
                }
            return {}
        except (ValueError, IndexError):
            return {}

    def _get_rule_path(self, client_code: str, process_group_code: str,
                       process_area_code: str, rule_id: int) -> Path:
        """
        Construct file path for a rule.

        Args:
            client_code: Client code
            process_group_code: Process group code
            process_area_code: Process area code
            rule_id: Rule ID

        Returns:
            Path object for the rule file
        """
        return self.base_path / client_code / process_group_code / process_area_code / f"rule-{rule_id}.json"

    def _ensure_directory(self, client_code: str, process_group_code: str, process_area_code: str):
        """Create directory structure if it doesn't exist."""
        dir_path = self.base_path / client_code / process_group_code / process_area_code
        dir_path.mkdir(parents=True, exist_ok=True)

    def get_rule(self, client_code: str, process_group_code: str,
                 process_area_code: str, rule_id: int) -> Optional[Dict]:
        """
        Get a single rule by hierarchy and ID.

        Args:
            client_code: Client code
            process_group_code: Process group code
            process_area_code: Process area code
            rule_id: Rule ID

        Returns:
            Rule data as dictionary with derived hierarchy, or None if not found
        """
        file_path = self._get_rule_path(client_code, process_group_code, process_area_code, rule_id)

        if not file_path.exists():
            logger.warning(f"Rule not found: {file_path}")
            return None

        try:
            with open(file_path, 'r') as f:
                rule_data = json.load(f)

            rule_data['hierarchy'] = self._derive_hierarchy_from_path(file_path)
            logger.info(f"Successfully read rule from {file_path}")
            return rule_data
        except Exception as e:
            logger.error(f"Error reading rule from {file_path}: {e}")
            raise

    def save_rule(self, rule_data: Dict) -> Dict:
        """
        Save or update a rule.

        Args:
            rule_data: Complete rule data with hierarchy codes (derived from path)

        Returns:
            Result dictionary with success status and file path
        """
        # Extract hierarchy info (may come from derived path or request)
        hierarchy = rule_data.get('hierarchy', {})
        client_code = hierarchy.get('client_code')
        process_group_code = hierarchy.get('process_group_code')
        process_area_code = hierarchy.get('process_area_code')
        rule_id = rule_data.get('id')

        if not all([client_code, process_group_code, process_area_code, rule_id]):
            raise ValueError("Missing required hierarchy information (client_code, process_group_code, process_area_code, id)")

        # Ensure directory exists
        self._ensure_directory(client_code, process_group_code, process_area_code)

        # Update timestamp
        rule_data['updated_at'] = datetime.utcnow().isoformat()

        # Remove hierarchy from data before saving (derived at runtime)
        save_data = {k: v for k, v in rule_data.items() if k != 'hierarchy'}

        # Write file with pretty formatting (git-friendly)
        file_path = self._get_rule_path(client_code, process_group_code, process_area_code, rule_id)

        try:
            with open(file_path, 'w') as f:
                json.dump(save_data, f, indent=2, ensure_ascii=False)

            logger.info(f"Successfully saved rule to {file_path}")
            return {
                "success": True,
                "path": str(file_path),
                "id": rule_id
            }
        except Exception as e:
            logger.error(f"Error saving rule to {file_path}: {e}")
            raise

    def delete_rule(self, client_code: str, process_group_code: str,
                    process_area_code: str, rule_id: int) -> Dict:
        """
        Delete a rule file.

        Args:
            client_code: Client code
            process_group_code: Process group code
            process_area_code: Process area code
            rule_id: Rule ID

        Returns:
            Result dictionary with success status
        """
        file_path = self._get_rule_path(client_code, process_group_code, process_area_code, rule_id)

        if not file_path.exists():
            logger.warning(f"Rule not found for deletion: {file_path}")
            raise FileNotFoundError(f"Rule not found: {client_code}/{process_group_code}/{process_area_code}/rule-{rule_id}")

        try:
            file_path.unlink()
            logger.info(f"Successfully deleted rule: {file_path}")
            return {
                "success": True,
                "path": str(file_path),
                "id": rule_id
            }
        except Exception as e:
            logger.error(f"Error deleting rule {file_path}: {e}")
            raise

    def list_rules(self, client_code: str = None, process_group_code: str = None,
                   process_area_code: str = None) -> List[Dict]:
        """
        List rules with optional filtering by hierarchy level.

        Args:
            client_code: Optional client code filter
            process_group_code: Optional process group code filter
            process_area_code: Optional process area code filter

        Returns:
            List of rule metadata (without full content)
        """
        rules = []

        # Determine search path based on filters
        if client_code and process_group_code and process_area_code:
            search_path = self.base_path / client_code / process_group_code / process_area_code
            pattern = "rule-*.json"
        elif client_code and process_group_code:
            search_path = self.base_path / client_code / process_group_code
            pattern = "**/rule-*.json"
        elif client_code:
            search_path = self.base_path / client_code
            pattern = "**/rule-*.json"
        else:
            search_path = self.base_path
            pattern = "**/rule-*.json"

        if not search_path.exists():
            logger.warning(f"Search path does not exist: {search_path}")
            return []

        # Find all matching rule files
        for rule_file in search_path.glob(pattern):
            if rule_file.name == "schema.json":
                continue

            try:
                with open(rule_file, 'r') as f:
                    rule_data = json.load(f)

                # Return metadata only (exclude large content field)
                rules.append({
                    'id': rule_data.get('id'),
                    'name': rule_data.get('name'),
                    'description': rule_data.get('description'),
                    'status': rule_data.get('status'),
                    'item_type': rule_data.get('item_type'),
                    'version': rule_data.get('version'),
                    'updated_at': rule_data.get('updated_at'),
                    'hierarchy': self._derive_hierarchy_from_path(rule_file)
                })
            except Exception as e:
                logger.error(f"Error reading rule file {rule_file}: {e}")
                continue

        logger.info(f"Found {len(rules)} rules matching criteria")
        return rules

    def get_rule_count(self) -> int:
        """Get total count of rules."""
        count = len(list(self.base_path.glob("**/rule-*.json")))
        return count if count > 0 and (self.base_path / "schema.json").exists() else count - 1 if count > 0 else 0
