#!/usr/bin/env python3
"""
Database Setup Script for Rules Engine
This script creates and populates the database with sample data.
Can be run independently to set up the database for testing.

Usage:
    python setup_database.py [--db-path path/to/database.db]
"""

import argparse
import sqlite3
import sys
from pathlib import Path

def create_database(db_path):
    """Create the database and populate it with sample data."""
    
    # Read the SQL script
    sql_script_path = Path(__file__).parent / 'create_database.sql'
    
    if not sql_script_path.exists():
        print(f"Error: SQL script not found at {sql_script_path}")
        return False
    
    try:
        # Read the SQL script
        with open(sql_script_path, 'r') as f:
            sql_script = f.read()
        
        # Connect to database (creates file if it doesn't exist)
        print(f"Creating database at: {db_path}")
        conn = sqlite3.connect(db_path)
        
        # Enable foreign key constraints
        conn.execute("PRAGMA foreign_keys = ON")
        
        # Execute the script
        print("Executing database creation script...")
        conn.executescript(sql_script)
        
        # Verify the setup
        print("Verifying database setup...")
        cursor = conn.cursor()
        
        # Count records in each table
        tables = ['clients', 'process_groups', 'process_areas', 'rules', 'rule_lists']
        for table in tables:
            cursor.execute(f"SELECT COUNT(*) FROM {table}")
            count = cursor.fetchone()[0]
            print(f"  {table}: {count} records")
        
        # Verify hierarchy integrity
        cursor.execute("""
            SELECT 
                c.name as client_name,
                COUNT(DISTINCT pg.id) as process_groups,
                COUNT(DISTINCT pa.id) as process_areas,
                COUNT(DISTINCT r.id) as rules
            FROM clients c
            LEFT JOIN process_groups pg ON c.id = pg.client_id
            LEFT JOIN process_areas pa ON pg.id = pa.process_group_id
            LEFT JOIN rules r ON pa.id = r.process_area_id
            GROUP BY c.id, c.name
            ORDER BY c.name
        """)
        
        hierarchy_data = cursor.fetchall()
        print("\nHierarchy Summary:")
        for row in hierarchy_data:
            print(f"  {row[0]}: {row[1]} process groups, {row[2]} process areas, {row[3]} rules")
        
        # Show status distribution
        cursor.execute("""
            SELECT status, COUNT(*) as count
            FROM rules
            GROUP BY status
            ORDER BY status
        """)
        
        status_data = cursor.fetchall()
        print("\nRule Status Distribution:")
        for status, count in status_data:
            print(f"  {status}: {count} rules")
        
        conn.close()
        print(f"\n✅ Database setup completed successfully!")
        print(f"Database location: {db_path}")
        
        return True
        
    except Exception as e:
        print(f"❌ Error setting up database: {e}")
        return False

def main():
    parser = argparse.ArgumentParser(description='Set up Rules Engine database with sample data')
    parser.add_argument('--db-path', 
                       default='rules_engine.db',
                       help='Path to SQLite database file (default: rules_engine.db)')
    parser.add_argument('--force', 
                       action='store_true',
                       help='Force overwrite existing database')
    
    args = parser.parse_args()
    
    db_path = Path(args.db_path)
    
    # Check if database already exists
    if db_path.exists() and not args.force:
        response = input(f"Database {db_path} already exists. Overwrite? [y/N]: ")
        if response.lower() != 'y':
            print("Setup cancelled.")
            return 1
    
    # Create database
    success = create_database(db_path)
    
    if success:
        print("\n🚀 You can now start the application!")
        print("   Backend: python app.py")
        print("   Frontend: npm start (in frontend directory)")
        return 0
    else:
        return 1

if __name__ == '__main__':
    sys.exit(main())