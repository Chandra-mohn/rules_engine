#!/usr/bin/env python3
"""
Migration script to remove hierarchy metadata from rule JSON files.

This script removes the 'hierarchy' section from all rule JSON files,
as the hierarchy information is now derived from the file path at runtime.
"""

import json
from pathlib import Path
import sys


def remove_hierarchy_from_rules(base_path: Path) -> dict:
    """
    Remove hierarchy metadata from all rule JSON files.

    Args:
        base_path: Base directory containing rule files

    Returns:
        Dictionary with statistics about the migration
    """
    stats = {
        'total_files': 0,
        'modified_files': 0,
        'skipped_files': 0,
        'error_files': 0,
        'errors': []
    }

    if not base_path.exists():
        print(f"❌ Error: {base_path} does not exist")
        return stats

    for rule_file in base_path.rglob('rule-*.json'):
        stats['total_files'] += 1

        try:
            with open(rule_file, 'r') as f:
                rule_data = json.load(f)

            if 'hierarchy' in rule_data:
                del rule_data['hierarchy']

                with open(rule_file, 'w') as f:
                    json.dump(rule_data, f, indent=2, ensure_ascii=False)

                stats['modified_files'] += 1
                print(f"✅ Cleaned: {rule_file.relative_to(base_path)}")
            else:
                stats['skipped_files'] += 1
                print(f"⏭️  Skipped (no hierarchy): {rule_file.relative_to(base_path)}")

        except Exception as e:
            stats['error_files'] += 1
            error_msg = f"{rule_file.relative_to(base_path)}: {str(e)}"
            stats['errors'].append(error_msg)
            print(f"❌ Error: {error_msg}")

    return stats


def main():
    script_dir = Path(__file__).parent
    rules_dir = script_dir.parent / 'rules'

    print("🧹 Starting hierarchy metadata removal...")
    print(f"📂 Scanning: {rules_dir}\n")

    stats = remove_hierarchy_from_rules(rules_dir)

    print("\n" + "=" * 60)
    print("📊 Migration Complete")
    print("=" * 60)
    print(f"Total files scanned: {stats['total_files']}")
    print(f"Files modified: {stats['modified_files']}")
    print(f"Files skipped: {stats['skipped_files']}")
    print(f"Files with errors: {stats['error_files']}")

    if stats['errors']:
        print("\n⚠️  Errors encountered:")
        for error in stats['errors']:
            print(f"  - {error}")
        sys.exit(1)
    else:
        print("\n✅ All files processed successfully!")
        sys.exit(0)


if __name__ == '__main__':
    main()
