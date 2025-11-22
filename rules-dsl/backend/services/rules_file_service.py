"""
Clean .rules-only file service for VS Code plugin backend.
Optimized for .rules format - NO JSON support.
"""

import yaml
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


class RulesFileService:
    """Service for managing .rules files in hierarchical folder structure."""

    def __init__(self, base_path: str = None):
        """
        Initialize the rules file service.

        Args:
            base_path: Base directory for rules storage. Defaults to ../rules/
        """
        if base_path is None:
            current_dir = Path(__file__).parent.parent.parent
            base_path = current_dir / "rules"

        self.base_path = Path(base_path)
        logger.info(f"RulesFileService initialized with base_path: {self.base_path}")

    def load_rule(self, file_path: Path) -> Dict:
        """
        Load a .rules file and parse its content.

        Args:
            file_path: Path to .rules file

        Returns:
            Dict with parsed rule data

        Raises:
            ValueError: If file format is invalid
            FileNotFoundError: If file doesn't exist
        """
        if not file_path.exists():
            raise FileNotFoundError(f"Rules file not found: {file_path}")

        if not file_path.suffix == '.rules':
            raise ValueError(f"Invalid file format: expected .rules, got {file_path.suffix}")

        with open(file_path, 'r') as f:
            content = f.read()

        # Parse frontmatter and DSL content
        frontmatter, dsl_content = self._parse_frontmatter(content)

        # Extract rule name from filename
        rule_name = file_path.stem

        # Derive hierarchy from file path
        hierarchy = self._derive_hierarchy_from_path(file_path)

        # Build rule object
        rule = {
            'name': rule_name,
            'content': dsl_content,
            'context_id': frontmatter.get('context'),
            'effective_date': frontmatter.get('effective'),
            'expiry_date': frontmatter.get('expires'),
            'file_path': str(file_path),
            'source_format': 'rules',
            **hierarchy  # Add client_code, process_group, process_area
        }

        return rule

    def save_rule(self, rule_data: Dict, file_path: Path = None) -> Path:
        """
        Save a rule to .rules format.

        Args:
            rule_data: Rule data dictionary
            file_path: Optional file path (if None, auto-generate from hierarchy)

        Returns:
            Path to saved file
        """
        if file_path is None:
            file_path = self._build_file_path(rule_data)

        # Build .rules content
        content = self._build_rules_content(rule_data)

        # Create directory if needed
        file_path.parent.mkdir(parents=True, exist_ok=True)

        # Write file
        with open(file_path, 'w') as f:
            f.write(content)

        logger.info(f"Saved rule to: {file_path}")
        return file_path

    def list_rules(self, rule_type: str = None) -> List[Dict]:
        """
        List all .rules files, optionally filtered by type.

        Args:
            rule_type: Optional filter ('mon', 'non-mon', 'actionsets')

        Returns:
            List of rule dictionaries
        """
        rules = []

        if rule_type:
            search_path = self.base_path / rule_type
        else:
            search_path = self.base_path

        # Find all .rules files
        for rules_file in search_path.glob('**/*.rules'):
            try:
                rule = self.load_rule(rules_file)
                rules.append(rule)
            except Exception as e:
                logger.warning(f"Failed to load {rules_file}: {e}")

        return rules

    def delete_rule(self, file_path: Path) -> bool:
        """
        Delete a .rules file.

        Args:
            file_path: Path to file to delete

        Returns:
            True if deleted successfully
        """
        if file_path.exists():
            file_path.unlink()
            logger.info(f"Deleted rule: {file_path}")
            return True
        return False

    # Private helper methods

    def _parse_frontmatter(self, content: str) -> Tuple[Dict, str]:
        """
        Parse YAML frontmatter from .rules file.

        Returns:
            Tuple of (frontmatter_dict, dsl_content)
        """
        if not content.startswith('---'):
            return {}, content

        parts = content.split('---', 2)
        if len(parts) < 3:
            return {}, content

        frontmatter_yaml = parts[1]
        dsl_content = parts[2].strip()

        try:
            metadata = yaml.safe_load(frontmatter_yaml) or {}
        except yaml.YAMLError as e:
            logger.warning(f"Invalid YAML frontmatter: {e}")
            return {}, content

        # Validate allowed fields
        allowed_fields = {'context', 'effective', 'expires'}
        unknown_fields = set(metadata.keys()) - allowed_fields
        if unknown_fields:
            logger.warning(f"Unknown frontmatter fields (ignored): {unknown_fields}")
            # Remove unknown fields
            metadata = {k: v for k, v in metadata.items() if k in allowed_fields}

        # Validate dates
        if metadata.get('effective') and metadata.get('expires'):
            self._validate_dates(metadata['effective'], metadata['expires'])

        return metadata, dsl_content

    def _validate_dates(self, effective: str, expires: str) -> None:
        """
        Validate that expiry date is after effective date.

        Raises:
            ValueError: If dates are invalid
        """
        try:
            effective_date = datetime.strptime(effective, '%Y-%m-%d').date()
            expires_date = datetime.strptime(expires, '%Y-%m-%d').date()

            if expires_date <= effective_date:
                raise ValueError(
                    f"Expiry date ({expires}) must be after effective date ({effective})"
                )
        except ValueError as e:
            if "does not match format" in str(e):
                raise ValueError(f"Invalid date format (expected YYYY-MM-DD): {e}")
            raise

    def _build_rules_content(self, rule_data: Dict) -> str:
        """
        Build .rules file content from rule data.

        Args:
            rule_data: Rule dictionary

        Returns:
            Complete .rules file content
        """
        lines = []

        # Build frontmatter if any metadata present
        frontmatter = {}
        if rule_data.get('context_id'):
            frontmatter['context'] = rule_data['context_id']
        if rule_data.get('effective_date'):
            frontmatter['effective'] = rule_data['effective_date']
        if rule_data.get('expiry_date'):
            frontmatter['expires'] = rule_data['expiry_date']

        if frontmatter:
            lines.append('---')
            lines.append(yaml.dump(frontmatter, default_flow_style=False).strip())
            lines.append('---')
            lines.append('')

        # Add description as comment if present
        if rule_data.get('description'):
            lines.append(f"# {rule_data['description']}")
            lines.append('')

        # Add DSL content
        lines.append(rule_data['content'])

        return '\n'.join(lines)

    def _derive_hierarchy_from_path(self, file_path: Path) -> Dict:
        """
        Extract hierarchy information from file path.

        Args:
            file_path: Path to .rules file

        Returns:
            Dict with client_code, process_group, process_area, rule_type
        """
        try:
            relative_path = file_path.relative_to(self.base_path)
            parts = relative_path.parts

            # Structure: {rule_type}/{client}/{process_group}/{process_area}/{rule_name}.rules
            rule_type = parts[0] if len(parts) > 0 else None
            client_code = parts[1] if len(parts) > 1 else None
            process_group = parts[2] if len(parts) > 2 else None
            process_area = parts[3] if len(parts) > 3 else None

            return {
                'rule_type': rule_type,
                'client_code': client_code,
                'process_group_code': process_group,
                'process_area_code': process_area
            }
        except ValueError:
            # File not under base_path
            return {
                'rule_type': None,
                'client_code': None,
                'process_group_code': None,
                'process_area_code': None
            }

    def _build_file_path(self, rule_data: Dict) -> Path:
        """
        Build file path from rule data.

        Args:
            rule_data: Rule dictionary with hierarchy info

        Returns:
            Path object for .rules file
        """
        rule_type = rule_data.get('rule_type', 'mon')
        client_code = rule_data.get('client_code', 'UNKNOWN')
        process_group = rule_data.get('process_group_code', 'UNKNOWN')
        process_area = rule_data.get('process_area_code', 'UNKNOWN')
        rule_name = rule_data.get('name', 'unnamed_rule')

        path = (
            self.base_path
            / rule_type
            / client_code
            / process_group
            / process_area
            / f"{rule_name}.rules"
        )

        return path
