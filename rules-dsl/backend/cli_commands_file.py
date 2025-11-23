"""
CLI Commands for File-Based Rules Engine

Flask CLI commands for file-based data management and seeding.
Replaces database-based cli_commands.py
"""

import click
import shutil
from flask.cli import with_appcontext
from pathlib import Path


@click.command('seed-demo')
@with_appcontext
def seed_demo_command():
    """Seed the file system with demo data by copying from test_data/demo_rules/."""
    click.echo('🌱 Seeding file system with demo data...')

    try:
        # Define paths
        source_dir = Path('test_data/demo_rules')
        target_dir = Path('rules')

        # Check if demo data exists
        if not source_dir.exists():
            click.echo('❌ Demo data not found in test_data/demo_rules/')
            click.echo('   Run: python scripts/export_demo_data.py')
            return

        # Check if target already has data
        if target_dir.exists() and any(target_dir.iterdir()):
            if not click.confirm('⚠️  rules/ already contains files. Clear and re-seed?', default=False):
                click.echo('❌ Seeding cancelled')
                return

            # Clear existing data
            click.echo('🗑️  Clearing existing data...')
            for item in target_dir.iterdir():
                if item.name.startswith('.'):
                    continue
                if item.is_dir():
                    shutil.rmtree(item)
                else:
                    item.unlink()

        # Create target directory if needed
        target_dir.mkdir(parents=True, exist_ok=True)

        # Copy demo data
        click.echo('📂 Copying demo rules...')
        copied_count = 0

        for client_dir in source_dir.iterdir():
            if not client_dir.is_dir() or client_dir.name.startswith('.'):
                continue

            # Copy entire client directory tree
            target_client_dir = target_dir / client_dir.name
            shutil.copytree(client_dir, target_client_dir, dirs_exist_ok=True)

            # Count files
            for rule_file in target_client_dir.rglob('rule-*.json'):
                copied_count += 1

        click.echo(f'\n✅ Demo data seeded successfully!')
        click.echo(f'   📊 Copied {copied_count} rules to rules/')

    except Exception as e:
        click.echo(f'\n❌ Error seeding demo data: {str(e)}', err=True)
        import traceback
        traceback.print_exc()


@click.command('clear-data')
@with_appcontext
def clear_data_command():
    """Clear all data from rules/. USE WITH CAUTION!"""
    if not click.confirm('⚠️  This will delete ALL files in rules/. Are you sure?', default=False):
        click.echo('❌ Operation cancelled')
        return

    try:
        target_dir = Path('rules')

        if not target_dir.exists():
            click.echo('✅ rules/ is already empty')
            return

        # Delete all contents
        deleted_count = 0
        for item in target_dir.iterdir():
            if item.name.startswith('.'):
                continue

            if item.is_dir():
                shutil.rmtree(item)
            else:
                item.unlink()

            deleted_count += 1

        click.echo(f'✅ Cleared {deleted_count} items from rules/')

    except Exception as e:
        click.echo(f'❌ Error clearing data: {str(e)}', err=True)


@click.command('db-info')
@with_appcontext
def db_info_command():
    """Display file system statistics."""
    click.echo('\n📊 File System Statistics')
    click.echo('=' * 50)

    try:
        rules_dir = Path('rules')
        test_dir = Path('test_data')

        # Count rules by type
        rules_count = 0
        actionsets_count = 0
        actions_count = 0
        mon_rules_count = 0
        non_mon_rules_count = 0

        if rules_dir.exists():
            import json
            for rule_file in rules_dir.rglob('rule-*.json'):
                with open(rule_file, 'r') as f:
                    rule_data = json.load(f)

                item_type = rule_data.get('item_type', 'rule')
                if item_type == 'actionset':
                    actionsets_count += 1
                elif item_type == 'action':
                    actions_count += 1
                elif item_type == 'mon_rule':
                    mon_rules_count += 1
                elif item_type == 'non_mon_rule':
                    non_mon_rules_count += 1
                else:
                    rules_count += 1

        # Count clients, process groups, process areas
        clients = set()
        process_groups = set()
        process_areas = set()

        if rules_dir.exists():
            for client_dir in rules_dir.iterdir():
                if not client_dir.is_dir() or client_dir.name.startswith('.'):
                    continue
                clients.add(client_dir.name)

                for pg_dir in client_dir.iterdir():
                    if not pg_dir.is_dir() or pg_dir.name.startswith('.'):
                        continue
                    process_groups.add(f"{client_dir.name}/{pg_dir.name}")

                    for pa_dir in pg_dir.iterdir():
                        if not pa_dir.is_dir() or pa_dir.name.startswith('.'):
                            continue
                        process_areas.add(f"{client_dir.name}/{pg_dir.name}/{pa_dir.name}")

        # Count schemas and contexts
        schemas_count = 0
        contexts_count = 0
        templates_count = 0

        if test_dir.exists():
            schemas_dir = test_dir / 'schemas'
            if schemas_dir.exists():
                schemas_count = len(list(schemas_dir.glob('*.json')))

            contexts_dir = test_dir / 'contexts'
            if contexts_dir.exists():
                contexts_count = len(list(contexts_dir.glob('*.json')))

                templates_dir = contexts_dir / 'schema-templates'
                if templates_dir.exists():
                    templates_count = len(list(templates_dir.glob('*.json')))

        # Display statistics
        total_rules = rules_count + actionsets_count + actions_count + mon_rules_count + non_mon_rules_count

        stats = [
            ('Clients', len(clients)),
            ('Process Groups', len(process_groups)),
            ('Process Areas', len(process_areas)),
            ('Rules (total)', total_rules),
            ('  - Regular Rules', rules_count),
            ('  - ActionSets', actionsets_count),
            ('  - Actions', actions_count),
            ('  - Monetary Rules', mon_rules_count),
            ('  - Non-Monetary Rules', non_mon_rules_count),
            ('Schema Entities', schemas_count),
            ('Test Contexts', contexts_count),
            ('Schema Templates', templates_count),
        ]

        for label, count in stats:
            click.echo(f'{label:.<40} {count:>8}')

        click.echo('=' * 50)

    except Exception as e:
        click.echo(f'❌ Error gathering statistics: {str(e)}', err=True)
        import traceback
        traceback.print_exc()


def register_commands(app):
    """Register all CLI commands with the Flask app."""
    app.cli.add_command(seed_demo_command)
    app.cli.add_command(clear_data_command)
    app.cli.add_command(db_info_command)
