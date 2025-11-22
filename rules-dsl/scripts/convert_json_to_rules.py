#!/usr/bin/env python3
"""
Convert JSON rules to .rules format (big bang migration)
Converts all 34 JSON rules from ui-prototype to rules-dsl .rules format
"""

import json
import yaml
from pathlib import Path
from typing import Dict, Tuple
import sys

# Source and destination paths
SOURCE_BASE = Path("/Users/chandramohn/workspace/rules_engine/ui-prototype/backend/rules")
DEST_BASE = Path("/Users/chandramohn/workspace/rules_engine/rules-dsl/rules")


def convert_json_to_rules(json_file: Path) -> Tuple[str, Path]:
    """
    Convert a single JSON rule file to .rules format

    Returns:
        Tuple of (rules_content, destination_path)
    """
    with open(json_file, 'r') as f:
        data = json.load(f)

    # Build minimal frontmatter (only if fields present)
    frontmatter = {}
    if data.get('context_id'):
        frontmatter['context'] = data['context_id']
    if data.get('effective_date'):
        frontmatter['effective'] = data['effective_date']
    if data.get('expiry_date'):
        frontmatter['expires'] = data['expiry_date']

    # Build .rules file content
    lines = []

    # Add frontmatter if any fields present
    if frontmatter:
        lines.append('---')
        lines.append(yaml.dump(frontmatter, default_flow_style=False).strip())
        lines.append('---')
        lines.append('')

    # Add description as comment if present
    if data.get('description'):
        lines.append(f"# {data['description']}")
        lines.append('')

    # Add DSL content
    lines.append(data['content'])

    rules_content = '\n'.join(lines)

    # Determine destination path
    item_type = data.get('item_type', 'rule')

    # Map item types to folder structure
    type_folder_map = {
        'rule': 'mon',  # Default to monetary
        'mon_rule': 'mon',
        'non_mon_rule': 'non-mon',
        'actionset': 'actionsets'
    }

    type_folder = type_folder_map.get(item_type, 'mon')
    rule_name = data.get('name', f"rule-{data['id']}")

    # Extract hierarchy from current path (relative to SOURCE_BASE)
    relative_path = json_file.relative_to(SOURCE_BASE)
    hierarchy_parts = relative_path.parent.parts  # e.g., ('DEMO', 'CC_STD', 'APPROVAL')

    # Build destination path: rules-dsl/rules/{type_folder}/{hierarchy}/{rule_name}.rules
    dest_path = DEST_BASE / type_folder / Path(*hierarchy_parts) / f"{rule_name}.rules"

    return rules_content, dest_path


def migrate_all_rules():
    """Migrate all JSON rules to .rules format"""

    print("🚀 Starting big bang conversion: JSON → .rules")
    print(f"📂 Source: {SOURCE_BASE}")
    print(f"📂 Destination: {DEST_BASE}")
    print()

    # Find all JSON rule files
    json_files = list(SOURCE_BASE.glob('**/rule-*.json'))

    print(f"📋 Found {len(json_files)} JSON rule files")
    print()

    converted_count = 0
    error_count = 0

    for json_file in sorted(json_files):
        try:
            print(f"Converting: {json_file.name}...", end=' ')

            rules_content, dest_path = convert_json_to_rules(json_file)

            # Create directory if needed
            dest_path.parent.mkdir(parents=True, exist_ok=True)

            # Write .rules file
            with open(dest_path, 'w') as f:
                f.write(rules_content)

            # Show relative path for readability
            relative_dest = dest_path.relative_to(DEST_BASE.parent)
            print(f"✅ → {relative_dest}")

            converted_count += 1

        except Exception as e:
            print(f"❌ ERROR: {e}")
            error_count += 1

    print()
    print("=" * 60)
    print(f"✅ Successfully converted: {converted_count} rules")
    if error_count > 0:
        print(f"❌ Errors: {error_count} rules")
    print("=" * 60)

    # Show summary of created directories
    print()
    print("📁 Created directory structure:")
    for type_folder in ['mon', 'non-mon', 'actionsets']:
        folder_path = DEST_BASE / type_folder
        if folder_path.exists():
            rule_count = len(list(folder_path.glob('**/*.rules')))
            print(f"  {type_folder}/: {rule_count} rules")

    return converted_count, error_count


if __name__ == '__main__':
    try:
        converted, errors = migrate_all_rules()
        sys.exit(0 if errors == 0 else 1)
    except Exception as e:
        print(f"❌ Fatal error: {e}", file=sys.stderr)
        sys.exit(1)
