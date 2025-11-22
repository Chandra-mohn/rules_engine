#!/usr/bin/env python3
"""
Data Integrity Monitoring System
Advanced data validation and integrity monitoring for the rules engine database.

This module provides:
- Real-time data consistency checks
- Automated data healing capabilities
- Data quality metrics
- Constraint validation
- Backup verification
- Data migration safety checks
"""

import sqlite3
import json
import os
import shutil
import hashlib
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional, Any
from pathlib import Path
from dataclasses import dataclass
import logging

@dataclass
class DataIntegrityIssue:
    """Represents a data integrity issue"""
    issue_id: str
    severity: str  # CRITICAL, HIGH, MEDIUM, LOW
    category: str  # constraint, consistency, corruption, orphan, duplicate
    table: str
    column: Optional[str]
    row_id: Optional[int]
    message: str
    suggestion: str
    detected_at: str
    auto_fixable: bool = False

class DataIntegrityMonitor:
    """Comprehensive data integrity monitoring and validation system"""

    def __init__(self, db_path: str = "database/rules.db"):
        self.db_path = db_path
        self.backup_dir = Path("database/backups")
        self.backup_dir.mkdir(exist_ok=True)
        self.issues: List[DataIntegrityIssue] = []
        self.metrics: Dict[str, Any] = {}
        self.constraints = self._load_data_constraints()

        # Setup logging
        logging.basicConfig(level=logging.INFO)
        self.logger = logging.getLogger(__name__)

    def _load_data_constraints(self) -> Dict:
        """Load data validation constraints and rules"""
        return {
            'rules': {
                'required_fields': ['id', 'name', 'status', 'content', 'process_area_id'],
                'status_values': ['DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD', 'deleted'],
                'item_type_values': ['rule', 'actionset'],
                'max_lengths': {
                    'name': 100,
                    'description': 500,
                    'validation_message': 1000
                },
                'constraints': [
                    {
                        'name': 'unique_name_per_area',
                        'sql': '''
                            SELECT process_area_id, name, COUNT(*) as count
                            FROM rules
                            WHERE status != 'deleted'
                            GROUP BY process_area_id, name
                            HAVING COUNT(*) > 1
                        ''',
                        'severity': 'HIGH',
                        'message': 'Duplicate rule names in same process area'
                    },
                    {
                        'name': 'effective_date_for_active',
                        'sql': '''
                            SELECT id, name, status
                            FROM rules
                            WHERE status IN ('VALID', 'PEND', 'SCHD', 'PROD')
                            AND effective_date IS NULL
                        ''',
                        'severity': 'MEDIUM',
                        'message': 'Active rules should have effective_date'
                    },
                    {
                        'name': 'valid_process_area_reference',
                        'sql': '''
                            SELECT r.id, r.name, r.process_area_id
                            FROM rules r
                            LEFT JOIN process_areas pa ON r.process_area_id = pa.id
                            WHERE r.process_area_id IS NOT NULL AND pa.id IS NULL
                        ''',
                        'severity': 'CRITICAL',
                        'message': 'Invalid process_area_id reference'
                    },
                    {
                        'name': 'content_not_empty',
                        'sql': '''
                            SELECT id, name
                            FROM rules
                            WHERE (content IS NULL OR content = '' OR LENGTH(TRIM(content)) = 0)
                            AND status != 'deleted'
                        ''',
                        'severity': 'HIGH',
                        'message': 'Rule content cannot be empty for active rules'
                    }
                ]
            },
            'process_areas': {
                'required_fields': ['id', 'name', 'code', 'process_group_id'],
                'constraints': [
                    {
                        'name': 'valid_process_group_reference',
                        'sql': '''
                            SELECT pa.id, pa.name, pa.process_group_id
                            FROM process_areas pa
                            LEFT JOIN process_groups pg ON pa.process_group_id = pg.id
                            WHERE pa.process_group_id IS NOT NULL AND pg.id IS NULL
                        ''',
                        'severity': 'CRITICAL',
                        'message': 'Invalid process_group_id reference'
                    },
                    {
                        'name': 'unique_code_per_group',
                        'sql': '''
                            SELECT process_group_id, code, COUNT(*) as count
                            FROM process_areas
                            GROUP BY process_group_id, code
                            HAVING COUNT(*) > 1
                        ''',
                        'severity': 'HIGH',
                        'message': 'Duplicate process area codes in same group'
                    }
                ]
            },
            'schema_entities': {
                'required_fields': ['id', 'name', 'is_active'],
                'constraints': [
                    {
                        'name': 'active_entities_have_attributes',
                        'sql': '''
                            SELECT se.id, se.name
                            FROM schema_entities se
                            LEFT JOIN schema_attributes sa ON se.id = sa.entity_id
                            WHERE se.is_active = 1
                            GROUP BY se.id, se.name
                            HAVING COUNT(sa.id) = 0
                        ''',
                        'severity': 'MEDIUM',
                        'message': 'Active schema entities should have attributes'
                    }
                ]
            },
            'schema_attributes': {
                'required_fields': ['id', 'entity_id', 'name', 'data_type', 'java_type'],
                'data_type_values': ['string', 'number', 'date', 'boolean'],
                'constraints': [
                    {
                        'name': 'valid_entity_reference',
                        'sql': '''
                            SELECT sa.id, sa.name, sa.entity_id
                            FROM schema_attributes sa
                            LEFT JOIN schema_entities se ON sa.entity_id = se.id
                            WHERE sa.entity_id IS NOT NULL AND se.id IS NULL
                        ''',
                        'severity': 'CRITICAL',
                        'message': 'Invalid entity_id reference'
                    },
                    {
                        'name': 'consistent_type_mapping',
                        'sql': '''
                            SELECT id, name, data_type, java_type
                            FROM schema_attributes
                            WHERE (data_type = 'string' AND java_type != 'String')
                            OR (data_type = 'number' AND java_type NOT IN ('int', 'Integer', 'BigDecimal', 'Double'))
                            OR (data_type = 'date' AND java_type NOT IN ('LocalDate', 'Date'))
                            OR (data_type = 'boolean' AND java_type NOT IN ('boolean', 'Boolean'))
                        ''',
                        'severity': 'MEDIUM',
                        'message': 'Inconsistent data_type to java_type mapping'
                    }
                ]
            }
        }

    def log_issue(self, severity: str, category: str, table: str, message: str,
                  suggestion: str, column: Optional[str] = None, row_id: Optional[int] = None,
                  auto_fixable: bool = False):
        """Log a data integrity issue"""
        issue_id = hashlib.md5(f"{table}_{column}_{row_id}_{message}".encode()).hexdigest()[:8]

        issue = DataIntegrityIssue(
            issue_id=issue_id,
            severity=severity,
            category=category,
            table=table,
            column=column,
            row_id=row_id,
            message=message,
            suggestion=suggestion,
            detected_at=datetime.now().isoformat(),
            auto_fixable=auto_fixable
        )

        self.issues.append(issue)
        self.logger.warning(f"[{severity}] {table}: {message}")

    def create_backup(self, backup_name: Optional[str] = None) -> str:
        """Create a database backup"""
        if not os.path.exists(self.db_path):
            raise FileNotFoundError(f"Database file not found: {self.db_path}")

        if backup_name is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            backup_name = f"rules_backup_{timestamp}.db"

        backup_path = self.backup_dir / backup_name
        shutil.copy2(self.db_path, backup_path)

        # Verify backup integrity
        if self.verify_backup_integrity(str(backup_path)):
            self.logger.info(f"Database backup created: {backup_path}")
            return str(backup_path)
        else:
            os.remove(backup_path)
            raise RuntimeError("Backup verification failed")

    def verify_backup_integrity(self, backup_path: str) -> bool:
        """Verify backup file integrity"""
        try:
            conn = sqlite3.connect(backup_path)
            cursor = conn.cursor()

            # Check if we can read basic tables
            cursor.execute("SELECT COUNT(*) FROM rules")
            rules_count = cursor.fetchone()[0]

            cursor.execute("SELECT COUNT(*) FROM process_areas")
            areas_count = cursor.fetchone()[0]

            conn.close()

            return rules_count >= 0 and areas_count >= 0
        except Exception as e:
            self.logger.error(f"Backup verification failed: {e}")
            return False

    def validate_table_constraints(self, table_name: str) -> bool:
        """Validate constraints for a specific table"""
        if table_name not in self.constraints:
            self.logger.warning(f"No constraints defined for table: {table_name}")
            return True

        table_config = self.constraints[table_name]
        all_valid = True

        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            # Check required fields exist in table schema
            cursor.execute(f"PRAGMA table_info({table_name})")
            columns = [col[1] for col in cursor.fetchall()]

            required_fields = table_config.get('required_fields', [])
            for field in required_fields:
                if field not in columns:
                    self.log_issue(
                        'CRITICAL', 'constraint', table_name,
                        f'Required field missing: {field}',
                        f'Add {field} column to {table_name} table',
                        column=field
                    )
                    all_valid = False

            # Check custom constraints
            constraints = table_config.get('constraints', [])
            for constraint in constraints:
                cursor.execute(constraint['sql'])
                violations = cursor.fetchall()

                if violations:
                    for violation in violations:
                        row_id = violation[0] if violation else None
                        self.log_issue(
                            constraint['severity'], 'constraint', table_name,
                            f"{constraint['message']}: {violation}",
                            f"Review and fix data in {table_name}",
                            row_id=row_id
                        )
                    all_valid = False

            # Check enum field values
            if 'status_values' in table_config:
                cursor.execute(f"""
                    SELECT id, status FROM {table_name}
                    WHERE status NOT IN ({','.join('?' * len(table_config['status_values']))})
                """, table_config['status_values'])

                invalid_statuses = cursor.fetchall()
                for row_id, status in invalid_statuses:
                    self.log_issue(
                        'HIGH', 'constraint', table_name,
                        f'Invalid status value: {status}',
                        f'Update status to one of: {table_config["status_values"]}',
                        column='status', row_id=row_id, auto_fixable=True
                    )
                    all_valid = False

            conn.close()

        except Exception as e:
            self.log_issue(
                'CRITICAL', 'system', table_name,
                f'Error validating table constraints: {str(e)}',
                'Check database connectivity and table structure'
            )
            all_valid = False

        return all_valid

    def detect_data_corruption(self) -> bool:
        """Detect various forms of data corruption"""
        all_valid = True

        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            # Check for NULL values in NOT NULL columns
            cursor.execute("PRAGMA table_info(rules)")
            rules_columns = cursor.fetchall()

            for col_info in rules_columns:
                col_name = col_info[1]
                not_null = col_info[3]

                if not_null:  # Column is NOT NULL
                    cursor.execute(f"SELECT COUNT(*) FROM rules WHERE {col_name} IS NULL")
                    null_count = cursor.fetchone()[0]

                    if null_count > 0:
                        self.log_issue(
                            'CRITICAL', 'corruption', 'rules',
                            f'NULL values found in NOT NULL column: {col_name}',
                            f'Fix NULL values in {col_name} column',
                            column=col_name
                        )
                        all_valid = False

            # Check for character encoding issues
            cursor.execute("SELECT id, name, content FROM rules WHERE name != '' AND content != ''")
            for row_id, name, content in cursor.fetchall():
                try:
                    # Try to encode/decode to detect encoding issues
                    name.encode('utf-8').decode('utf-8')
                    content.encode('utf-8').decode('utf-8')
                except UnicodeError:
                    self.log_issue(
                        'MEDIUM', 'corruption', 'rules',
                        f'Character encoding issue in rule {name}',
                        'Fix character encoding in rule content',
                        row_id=row_id
                    )
                    all_valid = False

            # Check for extremely long content that might indicate corruption
            cursor.execute("SELECT id, name, LENGTH(content) as content_length FROM rules")
            for row_id, name, length in cursor.fetchall():
                if length > 100000:  # 100KB content limit
                    self.log_issue(
                        'MEDIUM', 'corruption', 'rules',
                        f'Unusually large content in rule {name}: {length} characters',
                        'Review rule content for corruption',
                        row_id=row_id
                    )

            conn.close()

        except Exception as e:
            self.log_issue(
                'CRITICAL', 'system', 'database',
                f'Error detecting data corruption: {str(e)}',
                'Check database file integrity'
            )
            all_valid = False

        return all_valid

    def check_orphaned_data(self) -> bool:
        """Check for orphaned records and broken relationships"""
        all_valid = True

        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            # Check for orphaned rules (invalid process_area_id)
            cursor.execute("""
                SELECT r.id, r.name, r.process_area_id
                FROM rules r
                LEFT JOIN process_areas pa ON r.process_area_id = pa.id
                WHERE r.process_area_id IS NOT NULL AND pa.id IS NULL
            """)

            orphaned_rules = cursor.fetchall()
            for rule_id, name, process_area_id in orphaned_rules:
                self.log_issue(
                    'HIGH', 'orphan', 'rules',
                    f'Rule {name} references non-existent process_area_id: {process_area_id}',
                    'Update process_area_id to valid value or create missing process area',
                    row_id=rule_id
                )
                all_valid = False

            # Check for orphaned process areas (invalid process_group_id)
            cursor.execute("""
                SELECT pa.id, pa.name, pa.process_group_id
                FROM process_areas pa
                LEFT JOIN process_groups pg ON pa.process_group_id = pg.id
                WHERE pa.process_group_id IS NOT NULL AND pg.id IS NULL
            """)

            orphaned_areas = cursor.fetchall()
            for area_id, name, group_id in orphaned_areas:
                self.log_issue(
                    'HIGH', 'orphan', 'process_areas',
                    f'Process area {name} references non-existent process_group_id: {group_id}',
                    'Update process_group_id to valid value or create missing process group',
                    row_id=area_id
                )
                all_valid = False

            # Check for orphaned schema attributes (invalid entity_id)
            cursor.execute("""
                SELECT sa.id, sa.name, sa.entity_id
                FROM schema_attributes sa
                LEFT JOIN schema_entities se ON sa.entity_id = se.id
                WHERE sa.entity_id IS NOT NULL AND se.id IS NULL
            """)

            orphaned_attributes = cursor.fetchall()
            for attr_id, name, entity_id in orphaned_attributes:
                self.log_issue(
                    'HIGH', 'orphan', 'schema_attributes',
                    f'Schema attribute {name} references non-existent entity_id: {entity_id}',
                    'Update entity_id to valid value or create missing entity',
                    row_id=attr_id
                )
                all_valid = False

            conn.close()

        except Exception as e:
            self.log_issue(
                'CRITICAL', 'system', 'database',
                f'Error checking orphaned data: {str(e)}',
                'Check database connectivity'
            )
            all_valid = False

        return all_valid

    def detect_duplicate_data(self) -> bool:
        """Detect inappropriate duplicate data"""
        all_valid = True

        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            # Check for duplicate rule names within same process area
            cursor.execute("""
                SELECT process_area_id, name, COUNT(*) as count, GROUP_CONCAT(id) as rule_ids
                FROM rules
                WHERE status != 'deleted'
                GROUP BY process_area_id, name
                HAVING COUNT(*) > 1
            """)

            duplicate_rules = cursor.fetchall()
            for area_id, name, count, rule_ids in duplicate_rules:
                self.log_issue(
                    'HIGH', 'duplicate', 'rules',
                    f'Duplicate rule name "{name}" in process area {area_id}: {count} instances',
                    f'Rename or consolidate duplicate rules (IDs: {rule_ids})',
                    column='name'
                )
                all_valid = False

            # Check for duplicate process area codes within same group
            cursor.execute("""
                SELECT process_group_id, code, COUNT(*) as count, GROUP_CONCAT(id) as area_ids
                FROM process_areas
                GROUP BY process_group_id, code
                HAVING COUNT(*) > 1
            """)

            duplicate_areas = cursor.fetchall()
            for group_id, code, count, area_ids in duplicate_areas:
                self.log_issue(
                    'HIGH', 'duplicate', 'process_areas',
                    f'Duplicate process area code "{code}" in group {group_id}: {count} instances',
                    f'Update codes to be unique (IDs: {area_ids})',
                    column='code'
                )
                all_valid = False

            # Check for duplicate schema entity names
            cursor.execute("""
                SELECT name, COUNT(*) as count, GROUP_CONCAT(id) as entity_ids
                FROM schema_entities
                GROUP BY name
                HAVING COUNT(*) > 1
            """)

            duplicate_entities = cursor.fetchall()
            for name, count, entity_ids in duplicate_entities:
                self.log_issue(
                    'MEDIUM', 'duplicate', 'schema_entities',
                    f'Duplicate schema entity name "{name}": {count} instances',
                    f'Rename or consolidate duplicate entities (IDs: {entity_ids})',
                    column='name'
                )
                all_valid = False

            conn.close()

        except Exception as e:
            self.log_issue(
                'CRITICAL', 'system', 'database',
                f'Error detecting duplicate data: {str(e)}',
                'Check database connectivity'
            )
            all_valid = False

        return all_valid

    def calculate_data_quality_metrics(self) -> Dict[str, Any]:
        """Calculate data quality metrics"""
        metrics = {
            'timestamp': datetime.now().isoformat(),
            'database_size_mb': 0,
            'table_metrics': {},
            'integrity_score': 0,
            'completeness_score': 0,
            'consistency_score': 0
        }

        try:
            # Get database file size
            if os.path.exists(self.db_path):
                size_bytes = os.path.getsize(self.db_path)
                metrics['database_size_mb'] = round(size_bytes / (1024 * 1024), 2)

            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            # Get table metrics
            tables = ['rules', 'process_areas', 'process_groups', 'clients', 'schema_entities', 'schema_attributes']

            for table in tables:
                try:
                    cursor.execute(f"SELECT COUNT(*) FROM {table}")
                    total_count = cursor.fetchone()[0]

                    # Calculate completeness for rules table
                    if table == 'rules':
                        cursor.execute("SELECT COUNT(*) FROM rules WHERE content IS NOT NULL AND content != ''")
                        complete_content = cursor.fetchone()[0]

                        cursor.execute("SELECT COUNT(*) FROM rules WHERE effective_date IS NOT NULL AND status IN ('VALID', 'PROD')")
                        complete_dates = cursor.fetchone()[0]

                        cursor.execute("SELECT COUNT(*) FROM rules WHERE status IN ('VALID', 'PROD')")
                        active_rules = cursor.fetchone()[0]

                        completeness = (complete_content / total_count * 100) if total_count > 0 else 0
                        date_completeness = (complete_dates / active_rules * 100) if active_rules > 0 else 100

                        metrics['table_metrics'][table] = {
                            'total_rows': total_count,
                            'completeness_pct': round(completeness, 2),
                            'date_completeness_pct': round(date_completeness, 2),
                            'active_rules': active_rules
                        }
                    else:
                        metrics['table_metrics'][table] = {
                            'total_rows': total_count
                        }

                except Exception as e:
                    self.logger.error(f"Error calculating metrics for {table}: {e}")

            # Calculate overall scores
            total_issues = len(self.issues)
            critical_issues = len([i for i in self.issues if i.severity == 'CRITICAL'])
            high_issues = len([i for i in self.issues if i.severity == 'HIGH'])

            # Integrity score (100 - penalty for issues)
            integrity_penalty = critical_issues * 20 + high_issues * 10 + (total_issues - critical_issues - high_issues) * 2
            metrics['integrity_score'] = max(0, 100 - integrity_penalty)

            # Completeness score based on rules table
            if 'rules' in metrics['table_metrics']:
                rules_metrics = metrics['table_metrics']['rules']
                metrics['completeness_score'] = (rules_metrics['completeness_pct'] + rules_metrics['date_completeness_pct']) / 2

            # Consistency score based on constraint violations
            constraint_violations = len([i for i in self.issues if i.category == 'constraint'])
            metrics['consistency_score'] = max(0, 100 - constraint_violations * 5)

            conn.close()

        except Exception as e:
            self.logger.error(f"Error calculating data quality metrics: {e}")

        self.metrics = metrics
        return metrics

    def auto_fix_issues(self, issue_types: Optional[List[str]] = None) -> int:
        """Automatically fix auto-fixable issues"""
        if not issue_types:
            issue_types = ['constraint']

        fixed_count = 0
        fixable_issues = [i for i in self.issues if i.auto_fixable and i.category in issue_types]

        if not fixable_issues:
            self.logger.info("No auto-fixable issues found")
            return 0

        # Create backup before making changes
        backup_path = self.create_backup(f"auto_fix_backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}.db")
        self.logger.info(f"Created backup before auto-fix: {backup_path}")

        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()

            for issue in fixable_issues:
                try:
                    if issue.category == 'constraint' and issue.column == 'status':
                        # Fix invalid status values
                        if issue.row_id:
                            cursor.execute(
                                "UPDATE rules SET status = 'DRAFT' WHERE id = ? AND status NOT IN (?, ?, ?, ?, ?, ?)",
                                (issue.row_id, 'DRAFT', 'VALID', 'PEND', 'SCHD', 'PROD', 'deleted')
                            )
                            if cursor.rowcount > 0:
                                fixed_count += 1
                                self.logger.info(f"Fixed status for rule ID {issue.row_id}")

                except Exception as e:
                    self.logger.error(f"Error auto-fixing issue {issue.issue_id}: {e}")

            conn.commit()
            conn.close()

        except Exception as e:
            self.logger.error(f"Error during auto-fix process: {e}")
            # Restore from backup if something went wrong
            shutil.copy2(backup_path, self.db_path)
            self.logger.info("Restored database from backup due to auto-fix error")

        return fixed_count

    def run_comprehensive_validation(self) -> Dict[str, Any]:
        """Run comprehensive data integrity validation"""
        print("=" * 60)
        print("DATA INTEGRITY MONITORING - STARTING")
        print("=" * 60)

        start_time = datetime.now()

        # Create backup before validation
        try:
            backup_path = self.create_backup()
            print(f"✅ Database backup created: {backup_path}")
        except Exception as e:
            print(f"⚠️  Could not create backup: {e}")

        # Run validation checks
        validation_results = {}

        checks = [
            ("Table Constraints", lambda: all(self.validate_table_constraints(table) for table in self.constraints.keys())),
            ("Data Corruption", self.detect_data_corruption),
            ("Orphaned Data", self.check_orphaned_data),
            ("Duplicate Data", self.detect_duplicate_data)
        ]

        overall_success = True
        for check_name, check_func in checks:
            print(f"\n--- {check_name} ---")
            try:
                result = check_func()
                validation_results[check_name.lower().replace(' ', '_')] = result
                if result:
                    print(f"✅ {check_name}: PASSED")
                else:
                    print(f"❌ {check_name}: FAILED")
                    overall_success = False
            except Exception as e:
                print(f"❌ {check_name}: ERROR - {e}")
                validation_results[check_name.lower().replace(' ', '_')] = False
                overall_success = False

        # Calculate metrics
        print("\n--- Data Quality Metrics ---")
        metrics = self.calculate_data_quality_metrics()
        print(f"Database Size: {metrics['database_size_mb']} MB")
        print(f"Integrity Score: {metrics['integrity_score']}/100")
        print(f"Completeness Score: {metrics['completeness_score']}/100")
        print(f"Consistency Score: {metrics['consistency_score']}/100")

        # Auto-fix if requested
        auto_fixed = 0
        if os.getenv('AUTO_FIX_ISSUES', 'false').lower() == 'true':
            print("\n--- Auto-Fixing Issues ---")
            auto_fixed = self.auto_fix_issues()
            print(f"Auto-fixed {auto_fixed} issues")

        # Generate report
        end_time = datetime.now()
        duration = (end_time - start_time).total_seconds()

        report = {
            'timestamp': start_time.isoformat(),
            'duration_seconds': duration,
            'overall_success': overall_success,
            'validation_results': validation_results,
            'metrics': metrics,
            'issues': [
                {
                    'issue_id': issue.issue_id,
                    'severity': issue.severity,
                    'category': issue.category,
                    'table': issue.table,
                    'column': issue.column,
                    'row_id': issue.row_id,
                    'message': issue.message,
                    'suggestion': issue.suggestion,
                    'detected_at': issue.detected_at,
                    'auto_fixable': issue.auto_fixable
                }
                for issue in self.issues
            ],
            'issue_summary': {
                'total': len(self.issues),
                'by_severity': {
                    'critical': len([i for i in self.issues if i.severity == 'CRITICAL']),
                    'high': len([i for i in self.issues if i.severity == 'HIGH']),
                    'medium': len([i for i in self.issues if i.severity == 'MEDIUM']),
                    'low': len([i for i in self.issues if i.severity == 'LOW'])
                },
                'by_category': {
                    'constraint': len([i for i in self.issues if i.category == 'constraint']),
                    'corruption': len([i for i in self.issues if i.category == 'corruption']),
                    'orphan': len([i for i in self.issues if i.category == 'orphan']),
                    'duplicate': len([i for i in self.issues if i.category == 'duplicate']),
                    'consistency': len([i for i in self.issues if i.category == 'consistency'])
                },
                'auto_fixable': len([i for i in self.issues if i.auto_fixable]),
                'auto_fixed': auto_fixed
            }
        }

        # Save report
        report_file = f"data_integrity_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)

        print(f"\n{'='*60}")
        print("DATA INTEGRITY MONITORING - SUMMARY")
        print(f"{'='*60}")
        print(f"Overall Status: {'PASSED' if overall_success else 'FAILED'}")
        print(f"Duration: {duration:.1f}s")
        print(f"Total Issues: {len(self.issues)}")
        print(f"Critical: {report['issue_summary']['by_severity']['critical']}")
        print(f"High: {report['issue_summary']['by_severity']['high']}")
        print(f"Medium: {report['issue_summary']['by_severity']['medium']}")
        print(f"Low: {report['issue_summary']['by_severity']['low']}")
        if auto_fixed > 0:
            print(f"Auto-fixed: {auto_fixed}")
        print(f"Report saved to: {report_file}")

        return report

def main():
    """Main entry point"""
    monitor = DataIntegrityMonitor()

    # Change to backend directory
    os.chdir('/Users/chandramohn/workspace/rules_engine/ui-prototype/backend')

    report = monitor.run_comprehensive_validation()

    # Exit with appropriate code
    if report['overall_success']:
        return 0
    else:
        return 1

if __name__ == "__main__":
    exit(main())