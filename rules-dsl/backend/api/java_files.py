"""
Java Files API

Provides endpoints to browse and read Java action files.
"""

from flask import Blueprint, request, jsonify
from pathlib import Path
import os

java_files_bp = Blueprint('java_files', __name__)

# Base directory for Java actions - adjust path as needed
# This points to the java-bridge source directory
BASE_DIR = Path(__file__).parent.parent.parent / 'java-bridge' / 'src' / 'main' / 'java'


@java_files_bp.route('/api/java-files/list', methods=['GET'])
def list_java_files():
    """
    List all .java files in the Java source directory.

    Returns:
        JSON with list of files including path, name, and size
    """
    try:
        if not BASE_DIR.exists():
            return jsonify({
                'error': f'Java source directory not found: {BASE_DIR}',
                'files': []
            }), 404

        files = []
        for root, dirs, filenames in os.walk(BASE_DIR):
            for filename in filenames:
                if filename.endswith('.java'):
                    full_path = Path(root) / filename
                    try:
                        # Get path relative to java-bridge/src/main/java
                        rel_path = full_path.relative_to(BASE_DIR)
                        files.append({
                            'path': str(rel_path).replace('\\', '/'),  # Normalize to forward slashes
                            'name': filename,
                            'size': full_path.stat().st_size,
                            'directory': str(rel_path.parent).replace('\\', '/')
                        })
                    except Exception as e:
                        # Skip files we can't read
                        continue

        # Sort by path for consistent ordering
        files.sort(key=lambda x: x['path'])

        return jsonify({'files': files, 'count': len(files)})

    except Exception as e:
        return jsonify({'error': str(e), 'files': []}), 500


@java_files_bp.route('/api/java-files/content', methods=['GET'])
def get_java_file_content():
    """
    Get content of a specific Java file.

    Query params:
        path: Relative path from java/main/src directory

    Returns:
        JSON with file content, path, size, and exists flag
    """
    file_path = request.args.get('path')

    if not file_path:
        return jsonify({'error': 'path parameter required'}), 400

    try:
        # Construct full path
        full_path = BASE_DIR / file_path

        # Security: Ensure path is within allowed directory
        try:
            full_path = full_path.resolve()
            BASE_DIR.resolve()
            if not str(full_path).startswith(str(BASE_DIR.resolve())):
                return jsonify({'error': 'Invalid path - outside allowed directory'}), 403
        except Exception:
            return jsonify({'error': 'Invalid path'}), 400

        # Check if file exists
        if not full_path.exists():
            return jsonify({
                'path': file_path,
                'exists': False,
                'error': 'File not found'
            }), 404

        # Read file content
        try:
            with open(full_path, 'r', encoding='utf-8') as f:
                content = f.read()
        except UnicodeDecodeError:
            # Try with different encoding if UTF-8 fails
            with open(full_path, 'r', encoding='latin-1') as f:
                content = f.read()

        return jsonify({
            'path': file_path,
            'content': content,
            'size': len(content),
            'exists': True,
            'lines': len(content.split('\n'))
        })

    except Exception as e:
        return jsonify({
            'error': str(e),
            'path': file_path,
            'exists': False
        }), 500


@java_files_bp.route('/api/java-files/validate', methods=['POST'])
def validate_java_file():
    """
    Validate that a Java file path exists and is readable.

    Request body:
        {"path": "com/rules/actions/ApprovalAction.java"}

    Returns:
        JSON with validation result
    """
    data = request.get_json()

    if not data or 'path' not in data:
        return jsonify({'valid': False, 'error': 'path required in request body'}), 400

    file_path = data['path']

    try:
        full_path = BASE_DIR / file_path

        # Security check
        if not str(full_path.resolve()).startswith(str(BASE_DIR.resolve())):
            return jsonify({'valid': False, 'error': 'Invalid path'}), 403

        if full_path.exists() and full_path.is_file() and file_path.endswith('.java'):
            return jsonify({
                'valid': True,
                'exists': True,
                'path': file_path,
                'size': full_path.stat().st_size
            })
        else:
            return jsonify({
                'valid': False,
                'exists': False,
                'error': 'File not found or not a Java file'
            })

    except Exception as e:
        return jsonify({
            'valid': False,
            'error': str(e)
        }), 500
