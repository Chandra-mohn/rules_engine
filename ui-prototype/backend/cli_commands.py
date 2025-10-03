"""
CLI Commands for Rules Engine

Flask CLI commands for database management and seeding.
"""

import click
from flask import current_app
from flask.cli import with_appcontext
from models import db
from fixtures import create_demo_data, clear_all_data


@click.command('seed-demo')
@with_appcontext
def seed_demo_command():
    """Seed the database with demo data."""
    click.echo('🌱 Seeding database with demo data...')

    try:
        # Check if data already exists
        from models import Rule
        if Rule.query.count() > 0:
            if not click.confirm('⚠️  Database already contains rules. Clear and re-seed?', default=False):
                click.echo('❌ Seeding cancelled')
                return

            # Clear existing data
            click.echo('🗑️  Clearing existing data...')
            clear_all_data()

        # Create demo data
        result = create_demo_data()

        click.echo('\n✅ Demo data seeded successfully!')
        click.echo(f'\n📊 Created:')
        for key, value in result.items():
            click.echo(f'   • {key.replace("_", " ").title()}: {value}')

    except Exception as e:
        click.echo(f'\n❌ Error seeding demo data: {str(e)}', err=True)
        import traceback
        traceback.print_exc()
        db.session.rollback()


@click.command('clear-data')
@with_appcontext
def clear_data_command():
    """Clear all data from the database. USE WITH CAUTION!"""
    if not click.confirm('⚠️  This will delete ALL data from the database. Are you sure?', default=False):
        click.echo('❌ Operation cancelled')
        return

    try:
        clear_all_data()
        click.echo('✅ All data cleared successfully')
    except Exception as e:
        click.echo(f'❌ Error clearing data: {str(e)}', err=True)
        db.session.rollback()


@click.command('db-info')
@with_appcontext
def db_info_command():
    """Display database statistics."""
    from models import Client, ProcessGroup, ProcessArea, Rule, SchemaEntity, SchemaAttribute

    click.echo('\n📊 Database Statistics')
    click.echo('=' * 50)

    stats = [
        ('Clients', Client.query.count()),
        ('Process Groups', ProcessGroup.query.count()),
        ('Process Areas', ProcessArea.query.count()),
        ('Rules (total)', Rule.query.count()),
        ('  - Regular Rules', Rule.query.filter_by(item_type='rule').count()),
        ('  - ActionSets', Rule.query.filter_by(item_type='actionset').count()),
        ('  - Monetary Rules', Rule.query.filter_by(item_type='mon_rule').count()),
        ('  - Non-Monetary Rules', Rule.query.filter_by(item_type='non_mon_rule').count()),
        ('Schema Entities', SchemaEntity.query.count()),
        ('Schema Attributes', SchemaAttribute.query.count()),
    ]

    for label, count in stats:
        click.echo(f'{label:.<40} {count:>8}')

    click.echo('=' * 50)


def register_commands(app):
    """Register all CLI commands with the Flask app."""
    app.cli.add_command(seed_demo_command)
    app.cli.add_command(clear_data_command)
    app.cli.add_command(db_info_command)
