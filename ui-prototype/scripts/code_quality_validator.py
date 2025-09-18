#!/usr/bin/env python3
"""
Code Quality Validation Module
Ensures adherence to established patterns, conventions, and best practices.

This module provides:
- Pattern compliance validation
- Code convention enforcement
- Security best practices checks
- Performance anti-pattern detection
- Architecture pattern validation
"""

import os
import re
import json
import subprocess
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Set
from datetime import datetime
import ast

class CodeQualityIssue:
    """Represents a code quality issue"""

    def __init__(self, file_path: str, line_number: int, issue_type: str,
                 severity: str, message: str, suggestion: Optional[str] = None):
        self.file_path = file_path
        self.line_number = line_number
        self.issue_type = issue_type
        self.severity = severity  # CRITICAL, HIGH, MEDIUM, LOW
        self.message = message
        self.suggestion = suggestion
        self.timestamp = datetime.now().isoformat()

class CodeQualityValidator:
    """Comprehensive code quality validation system"""

    def __init__(self, project_root: Optional[str] = None):
        self.project_root = Path(project_root or "/Users/chandramohn/workspace/rules_engine/ui-prototype")
        self.issues: List[CodeQualityIssue] = []
        self.patterns = self._load_quality_patterns()
        self.conventions = self._load_conventions()

    def _load_quality_patterns(self) -> Dict:
        """Load established patterns and anti-patterns"""
        return {
            'backend_patterns': {
                'required_imports': {
                    'flask_apps': ['from flask import Flask'],
                    'api_modules': ['from flask import Blueprint'],
                    'model_files': ['from sqlalchemy'],
                },
                'anti_patterns': [
                    {
                        'pattern': r'except\s*:',
                        'message': 'Bare except clause - should specify exception type',
                        'severity': 'MEDIUM',
                        'suggestion': 'Use except Exception as e: or specific exception types'
                    },
                    {
                        'pattern': r'print\s*\(',
                        'message': 'Print statement in production code',
                        'severity': 'LOW',
                        'suggestion': 'Use logging instead of print statements'
                    },
                    {
                        'pattern': r'validation_status',
                        'message': 'Reference to deprecated validation_status field',
                        'severity': 'HIGH',
                        'suggestion': 'Use consolidated status field instead'
                    },
                    {
                        'pattern': r'\.execute\s*\(\s*["\'].*\%.*["\']',
                        'message': 'Potential SQL injection vulnerability',
                        'severity': 'CRITICAL',
                        'suggestion': 'Use parameterized queries instead of string formatting'
                    },
                    {
                        'pattern': r'password.*=.*["\'].*["\']',
                        'message': 'Hardcoded password or secret',
                        'severity': 'HIGH',
                        'suggestion': 'Use environment variables for secrets'
                    }
                ],
                'required_patterns': [
                    {
                        'file_pattern': r'api/.*\.py$',
                        'required': r'@.*\.route',
                        'message': 'API files should define routes',
                        'severity': 'MEDIUM'
                    },
                    {
                        'file_pattern': r'models\.py$',
                        'required': r'class.*\(.*db\.Model\)',
                        'message': 'Model files should define SQLAlchemy models',
                        'severity': 'MEDIUM'
                    }
                ]
            },
            'frontend_patterns': {
                'anti_patterns': [
                    {
                        'pattern': r'console\.log\s*\(',
                        'message': 'Console.log statement in production code',
                        'severity': 'LOW',
                        'suggestion': 'Remove debug console.log statements'
                    },
                    {
                        'pattern': r'value=["\'](?:draft|active|inactive)["\']',
                        'message': 'Deprecated lowercase status values',
                        'severity': 'HIGH',
                        'suggestion': 'Use uppercase status values (DRAFT, VALID, PROD)'
                    },
                    {
                        'pattern': r'dangerouslySetInnerHTML',
                        'message': 'Potentially unsafe HTML injection',
                        'severity': 'HIGH',
                        'suggestion': 'Sanitize HTML content before rendering'
                    },
                    {
                        'pattern': r'eval\s*\(',
                        'message': 'Use of eval() function is dangerous',
                        'severity': 'CRITICAL',
                        'suggestion': 'Avoid eval() - use safer alternatives'
                    }
                ],
                'required_patterns': [
                    {
                        'file_pattern': r'components/.*\.jsx?$',
                        'required': r'(export\s+default|export\s+{)',
                        'message': 'React components should have exports',
                        'severity': 'MEDIUM'
                    }
                ]
            },
            'java_patterns': {
                'anti_patterns': [
                    {
                        'pattern': r'System\.out\.print',
                        'message': 'System.out.print in production code',
                        'severity': 'LOW',
                        'suggestion': 'Use proper logging framework'
                    },
                    {
                        'pattern': r'\.printStackTrace\(\)',
                        'message': 'printStackTrace() without proper error handling',
                        'severity': 'MEDIUM',
                        'suggestion': 'Use proper logging and error handling'
                    },
                    {
                        'pattern': r'catch\s*\(\s*Exception\s+\w+\s*\)\s*{\s*}',
                        'message': 'Empty catch block',
                        'severity': 'HIGH',
                        'suggestion': 'Handle exceptions properly or log them'
                    }
                ]
            }
        }

    def _load_conventions(self) -> Dict:
        """Load coding conventions and standards"""
        return {
            'naming_conventions': {
                'python': {
                    'function_pattern': r'^[a-z_][a-z0-9_]*$',
                    'class_pattern': r'^[A-Z][a-zA-Z0-9]*$',
                    'constant_pattern': r'^[A-Z_][A-Z0-9_]*$',
                    'variable_pattern': r'^[a-z_][a-z0-9_]*$'
                },
                'javascript': {
                    'function_pattern': r'^[a-z][a-zA-Z0-9]*$',
                    'class_pattern': r'^[A-Z][a-zA-Z0-9]*$',
                    'constant_pattern': r'^[A-Z_][A-Z0-9_]*$',
                    'variable_pattern': r'^[a-z][a-zA-Z0-9]*$'
                },
                'java': {
                    'method_pattern': r'^[a-z][a-zA-Z0-9]*$',
                    'class_pattern': r'^[A-Z][a-zA-Z0-9]*$',
                    'constant_pattern': r'^[A-Z_][A-Z0-9_]*$',
                    'variable_pattern': r'^[a-z][a-zA-Z0-9]*$'
                }
            },
            'file_conventions': {
                'max_line_length': 120,
                'max_function_length': 50,
                'max_class_length': 500,
                'required_headers': [
                    '#!/usr/bin/env python3',  # For Python scripts
                    '"""',  # Docstring start
                ]
            }
        }

    def log_issue(self, file_path: str, line_number: int, issue_type: str,
                  severity: str, message: str, suggestion: Optional[str] = None):
        """Log a code quality issue"""
        issue = CodeQualityIssue(file_path, line_number, issue_type, severity, message, suggestion)
        self.issues.append(issue)

    def validate_python_files(self) -> bool:
        """Validate Python files for quality issues"""
        print("=== Validating Python Files ===")

        python_files = list(self.project_root.rglob("*.py"))
        # Exclude virtual environment and cache files
        python_files = [f for f in python_files if '.venv' not in str(f) and '__pycache__' not in str(f)]

        all_valid = True

        for py_file in python_files:
            try:
                with open(py_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                    lines = content.split('\n')

                # Check anti-patterns
                for pattern_config in self.patterns['backend_patterns']['anti_patterns']:
                    pattern = pattern_config['pattern']
                    message = pattern_config['message']
                    severity = pattern_config['severity']
                    suggestion = pattern_config.get('suggestion')

                    for line_num, line in enumerate(lines, 1):
                        if re.search(pattern, line):
                            self.log_issue(
                                str(py_file.relative_to(self.project_root)),
                                line_num, 'anti_pattern', severity, message, suggestion
                            )
                            if severity in ['CRITICAL', 'HIGH']:
                                all_valid = False

                # Check required patterns for specific file types
                for req_pattern in self.patterns['backend_patterns']['required_patterns']:
                    file_pattern = req_pattern['file_pattern']
                    required = req_pattern['required']
                    message = req_pattern['message']
                    severity = req_pattern['severity']

                    if re.search(file_pattern, str(py_file)):
                        if not re.search(required, content):
                            self.log_issue(
                                str(py_file.relative_to(self.project_root)),
                                1, 'missing_pattern', severity, message
                            )

                # Check Python syntax and AST
                try:
                    ast.parse(content)
                except SyntaxError as e:
                    self.log_issue(
                        str(py_file.relative_to(self.project_root)),
                        e.lineno or 1, 'syntax_error', 'CRITICAL',
                        f'Python syntax error: {e.msg}'
                    )
                    all_valid = False

                # Check line length convention
                max_length = self.conventions['file_conventions']['max_line_length']
                for line_num, line in enumerate(lines, 1):
                    if len(line) > max_length:
                        self.log_issue(
                            str(py_file.relative_to(self.project_root)),
                            line_num, 'line_length', 'LOW',
                            f'Line exceeds {max_length} characters ({len(line)} chars)'
                        )

                # Check for proper docstrings in classes and functions
                if self._should_have_docstring(py_file):
                    functions_without_docs = self._find_undocumented_functions(content)
                    for func_name, line_num in functions_without_docs:
                        self.log_issue(
                            str(py_file.relative_to(self.project_root)),
                            line_num, 'missing_docstring', 'LOW',
                            f'Function {func_name} missing docstring'
                        )

            except Exception as e:
                self.log_issue(
                    str(py_file.relative_to(self.project_root)),
                    1, 'file_error', 'MEDIUM',
                    f'Error reading file: {str(e)}'
                )

        return all_valid

    def validate_javascript_files(self) -> bool:
        """Validate JavaScript/JSX files for quality issues"""
        print("=== Validating JavaScript/JSX Files ===")

        js_files = list(self.project_root.rglob("*.js")) + list(self.project_root.rglob("*.jsx"))
        # Exclude node_modules
        js_files = [f for f in js_files if 'node_modules' not in str(f)]

        all_valid = True

        for js_file in js_files:
            try:
                with open(js_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                    lines = content.split('\n')

                # Check anti-patterns
                for pattern_config in self.patterns['frontend_patterns']['anti_patterns']:
                    pattern = pattern_config['pattern']
                    message = pattern_config['message']
                    severity = pattern_config['severity']
                    suggestion = pattern_config.get('suggestion')

                    for line_num, line in enumerate(lines, 1):
                        if re.search(pattern, line):
                            self.log_issue(
                                str(js_file.relative_to(self.project_root)),
                                line_num, 'anti_pattern', severity, message, suggestion
                            )
                            if severity in ['CRITICAL', 'HIGH']:
                                all_valid = False

                # Check required patterns
                for req_pattern in self.patterns['frontend_patterns']['required_patterns']:
                    file_pattern = req_pattern['file_pattern']
                    required = req_pattern['required']
                    message = req_pattern['message']
                    severity = req_pattern['severity']

                    if re.search(file_pattern, str(js_file)):
                        if not re.search(required, content):
                            self.log_issue(
                                str(js_file.relative_to(self.project_root)),
                                1, 'missing_pattern', severity, message
                            )

                # Check for React-specific issues
                if js_file.suffix == '.jsx':
                    self._validate_react_component(js_file, content, lines)

            except Exception as e:
                self.log_issue(
                    str(js_file.relative_to(self.project_root)),
                    1, 'file_error', 'MEDIUM',
                    f'Error reading file: {str(e)}'
                )

        return all_valid

    def validate_java_files(self) -> bool:
        """Validate Java files for quality issues"""
        print("=== Validating Java Files ===")

        java_files = list(self.project_root.rglob("*.java"))

        all_valid = True

        for java_file in java_files:
            try:
                with open(java_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                    lines = content.split('\n')

                # Check anti-patterns
                for pattern_config in self.patterns['java_patterns']['anti_patterns']:
                    pattern = pattern_config['pattern']
                    message = pattern_config['message']
                    severity = pattern_config['severity']
                    suggestion = pattern_config.get('suggestion')

                    for line_num, line in enumerate(lines, 1):
                        if re.search(pattern, line):
                            self.log_issue(
                                str(java_file.relative_to(self.project_root)),
                                line_num, 'anti_pattern', severity, message, suggestion
                            )
                            if severity in ['CRITICAL', 'HIGH']:
                                all_valid = False

                # Check Java-specific conventions
                self._validate_java_conventions(java_file, content, lines)

            except Exception as e:
                self.log_issue(
                    str(java_file.relative_to(self.project_root)),
                    1, 'file_error', 'MEDIUM',
                    f'Error reading file: {str(e)}'
                )

        return all_valid

    def validate_architecture_patterns(self) -> bool:
        """Validate adherence to architectural patterns"""
        print("=== Validating Architecture Patterns ===")

        all_valid = True

        # Check three-tier architecture
        required_dirs = ['frontend', 'backend', 'java-bridge']
        for dir_name in required_dirs:
            dir_path = self.project_root / dir_name
            if not dir_path.exists():
                self.log_issue(
                    dir_name, 1, 'architecture', 'CRITICAL',
                    f'Missing required tier: {dir_name}',
                    f'Create {dir_name} directory to maintain three-tier architecture'
                )
                all_valid = False

        # Check separation of concerns
        backend_has_frontend_deps = self._check_cross_tier_dependencies()
        if backend_has_frontend_deps:
            all_valid = False

        # Check API layer structure
        api_dir = self.project_root / 'backend' / 'api'
        if api_dir.exists():
            api_files = list(api_dir.glob('*.py'))
            if len(api_files) < 3:
                self.log_issue(
                    'backend/api', 1, 'architecture', 'MEDIUM',
                    f'API layer has only {len(api_files)} modules, expected at least 3',
                    'Consider organizing API endpoints into logical modules'
                )

        # Check model organization
        models_file = self.project_root / 'backend' / 'models.py'
        if models_file.exists():
            with open(models_file, 'r') as f:
                content = f.read()
                model_count = len(re.findall(r'class\s+\w+\s*\([^)]*db\.Model', content))
                if model_count > 10:
                    self.log_issue(
                        'backend/models.py', 1, 'architecture', 'MEDIUM',
                        f'Models file has {model_count} models, consider splitting',
                        'Consider organizing models into separate modules'
                    )

        return all_valid

    def validate_security_patterns(self) -> bool:
        """Validate security best practices"""
        print("=== Validating Security Patterns ===")

        all_valid = True

        # Check for hardcoded secrets
        sensitive_patterns = [
            (r'password\s*=\s*["\'][^"\']{3,}["\']', 'Hardcoded password'),
            (r'api_?key\s*=\s*["\'][^"\']{10,}["\']', 'Hardcoded API key'),
            (r'secret\s*=\s*["\'][^"\']{8,}["\']', 'Hardcoded secret'),
            (r'token\s*=\s*["\'][^"\']{10,}["\']', 'Hardcoded token'),
        ]

        for file_path in self.project_root.rglob('*'):
            if file_path.is_file() and file_path.suffix in ['.py', '.js', '.jsx', '.java']:
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                        lines = content.split('\n')

                    for pattern, description in sensitive_patterns:
                        for line_num, line in enumerate(lines, 1):
                            if re.search(pattern, line, re.IGNORECASE):
                                self.log_issue(
                                    str(file_path.relative_to(self.project_root)),
                                    line_num, 'security', 'HIGH',
                                    description,
                                    'Use environment variables for sensitive data'
                                )
                                all_valid = False

                except Exception as e:
                    continue  # Skip files that can't be read

        # Check for SQL injection vulnerabilities
        sql_files = list(self.project_root.rglob('*.py'))
        for py_file in sql_files:
            if '.venv' in str(py_file):
                continue

            try:
                with open(py_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                    lines = content.split('\n')

                # Look for string formatting in SQL queries
                sql_injection_patterns = [
                    r'\.execute\s*\(\s*["\'].*%.*["\']',
                    r'\.execute\s*\(\s*f["\'].*{.*}.*["\']',
                    r'cursor\.execute\s*\(\s*["\'].*\+.*["\']'
                ]

                for pattern in sql_injection_patterns:
                    for line_num, line in enumerate(lines, 1):
                        if re.search(pattern, line):
                            self.log_issue(
                                str(py_file.relative_to(self.project_root)),
                                line_num, 'security', 'CRITICAL',
                                'Potential SQL injection vulnerability',
                                'Use parameterized queries instead of string formatting'
                            )
                            all_valid = False

            except Exception as e:
                continue

        return all_valid

    def _should_have_docstring(self, file_path: Path) -> bool:
        """Check if file should have docstrings"""
        # API files, service files, and main modules should have docstrings
        important_files = ['api/', 'services/', 'models.py', 'app.py']
        return any(pattern in str(file_path) for pattern in important_files)

    def _find_undocumented_functions(self, content: str) -> List[Tuple[str, int]]:
        """Find functions without docstrings"""
        lines = content.split('\n')
        undocumented = []

        for i, line in enumerate(lines):
            # Find function definitions
            func_match = re.match(r'^\s*def\s+(\w+)\s*\(', line)
            if func_match:
                func_name = func_match.group(1)
                # Skip private methods and __init__
                if func_name.startswith('_'):
                    continue

                # Check if next non-empty line is a docstring
                has_docstring = False
                for j in range(i + 1, min(i + 5, len(lines))):
                    next_line = lines[j].strip()
                    if not next_line:
                        continue
                    if next_line.startswith('"""') or next_line.startswith("'''"):
                        has_docstring = True
                        break
                    else:
                        break

                if not has_docstring:
                    undocumented.append((func_name, i + 1))

        return undocumented

    def _validate_react_component(self, file_path: Path, content: str, lines: List[str]):
        """Validate React component specific patterns"""
        # Check for proper component export
        if not re.search(r'export\s+default\s+\w+', content):
            self.log_issue(
                str(file_path.relative_to(self.project_root)),
                1, 'react_pattern', 'MEDIUM',
                'React component should have default export'
            )

        # Check for proper hooks usage
        hook_pattern = r'use[A-Z]\w*\s*\('
        state_hook_pattern = r'useState\s*\('

        hooks_found = re.findall(hook_pattern, content)
        if hooks_found:
            # Check if hooks are called at top level
            for line_num, line in enumerate(lines, 1):
                if re.search(hook_pattern, line):
                    # Simple check: hooks shouldn't be inside if statements or loops
                    if re.search(r'^\s*(if|for|while|function)\s', line):
                        self.log_issue(
                            str(file_path.relative_to(self.project_root)),
                            line_num, 'react_pattern', 'HIGH',
                            'Hooks should not be called inside loops, conditions, or nested functions'
                        )

    def _validate_java_conventions(self, file_path: Path, content: str, lines: List[str]):
        """Validate Java naming conventions and patterns"""
        # Check class naming
        class_matches = re.findall(r'class\s+(\w+)', content)
        for class_name in class_matches:
            if not re.match(self.conventions['naming_conventions']['java']['class_pattern'], class_name):
                self.log_issue(
                    str(file_path.relative_to(self.project_root)),
                    1, 'naming_convention', 'LOW',
                    f'Class name {class_name} does not follow Java naming conventions'
                )

        # Check method naming
        method_matches = re.findall(r'public\s+\w+\s+(\w+)\s*\(', content)
        for method_name in method_matches:
            if not re.match(self.conventions['naming_conventions']['java']['method_pattern'], method_name):
                self.log_issue(
                    str(file_path.relative_to(self.project_root)),
                    1, 'naming_convention', 'LOW',
                    f'Method name {method_name} does not follow Java naming conventions'
                )

    def _check_cross_tier_dependencies(self) -> bool:
        """Check for inappropriate cross-tier dependencies"""
        has_violations = False

        # Check if backend has frontend dependencies
        requirements_file = self.project_root / 'backend' / 'requirements.txt'
        if requirements_file.exists():
            try:
                with open(requirements_file, 'r') as f:
                    requirements = f.read().lower()

                frontend_deps = ['react', 'vue', 'angular', 'webpack', 'babel']
                for dep in frontend_deps:
                    if dep in requirements:
                        self.log_issue(
                            'backend/requirements.txt', 1, 'architecture', 'HIGH',
                            f'Backend has frontend dependency: {dep}',
                            'Remove frontend dependencies from backend'
                        )
                        has_violations = True
            except Exception as e:
                pass

        return has_violations

    def generate_quality_report(self, output_file: Optional[str] = None) -> Dict:
        """Generate comprehensive code quality report"""
        # Categorize issues
        by_severity = {}
        by_type = {}
        by_file = {}

        for issue in self.issues:
            # By severity
            if issue.severity not in by_severity:
                by_severity[issue.severity] = []
            by_severity[issue.severity].append(issue)

            # By type
            if issue.issue_type not in by_type:
                by_type[issue.issue_type] = []
            by_type[issue.issue_type].append(issue)

            # By file
            if issue.file_path not in by_file:
                by_file[issue.file_path] = []
            by_file[issue.file_path].append(issue)

        report = {
            'timestamp': datetime.now().isoformat(),
            'total_issues': len(self.issues),
            'summary': {
                'by_severity': {severity: len(issues) for severity, issues in by_severity.items()},
                'by_type': {issue_type: len(issues) for issue_type, issues in by_type.items()},
                'files_with_issues': len(by_file)
            },
            'issues': [
                {
                    'file_path': issue.file_path,
                    'line_number': issue.line_number,
                    'issue_type': issue.issue_type,
                    'severity': issue.severity,
                    'message': issue.message,
                    'suggestion': issue.suggestion,
                    'timestamp': issue.timestamp
                }
                for issue in self.issues
            ],
            'files_analyzed': {
                'python': len(list(self.project_root.rglob("*.py"))),
                'javascript': len(list(self.project_root.rglob("*.js")) + list(self.project_root.rglob("*.jsx"))),
                'java': len(list(self.project_root.rglob("*.java")))
            }
        }

        if output_file:
            with open(output_file, 'w') as f:
                json.dump(report, f, indent=2)

        return report

    def run_full_validation(self) -> bool:
        """Run complete code quality validation"""
        print("=" * 60)
        print("CODE QUALITY VALIDATION - STARTING")
        print("=" * 60)

        all_valid = True

        validators = [
            ("Python Files", self.validate_python_files),
            ("JavaScript Files", self.validate_javascript_files),
            ("Java Files", self.validate_java_files),
            ("Architecture Patterns", self.validate_architecture_patterns),
            ("Security Patterns", self.validate_security_patterns)
        ]

        for validator_name, validator_func in validators:
            print(f"\n--- {validator_name} ---")
            try:
                if not validator_func():
                    all_valid = False
            except Exception as e:
                print(f"Error in {validator_name}: {e}")
                all_valid = False

        # Generate report
        report_file = f"code_quality_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        report = self.generate_quality_report(report_file)

        print("\n" + "=" * 60)
        print("CODE QUALITY VALIDATION - SUMMARY")
        print("=" * 60)
        print(f"Total Issues: {report['total_issues']}")
        print(f"Files Analyzed: {sum(report['files_analyzed'].values())}")
        print(f"Files with Issues: {report['summary']['files_with_issues']}")

        for severity, count in report['summary']['by_severity'].items():
            print(f"{severity}: {count}")

        print(f"\nDetailed report saved to: {report_file}")

        return all_valid and len([i for i in self.issues if i.severity in ['CRITICAL', 'HIGH']]) == 0

def main():
    """Main entry point"""
    validator = CodeQualityValidator()
    success = validator.run_full_validation()

    if success:
        print("\n✅ Code quality validation passed!")
        return 0
    else:
        print("\n❌ Code quality validation failed!")
        return 1

if __name__ == "__main__":
    exit(main())