from flask import Blueprint, request, jsonify
from pathlib import Path
import json
from config import Config

hierarchy_bp = Blueprint('hierarchy', __name__)
RULES_PATH = Path(__file__).parent.parent / 'rules'

def scan_directory_recursive(path, depth=0, max_depth=10, parent_codes=None):
    """Recursively scan directory structure with arbitrary depth."""
    if depth > max_depth or not path.is_dir():
        return None

    if parent_codes is None:
        parent_codes = []

    folder_code = path.name
    folder_name = folder_code

    sample_rule = next(path.glob('rule-*.json'), None)
    if sample_rule:
        try:
            with open(sample_rule, 'r') as f:
                rule_data = json.load(f)
                hierarchy = rule_data.get('hierarchy', {})
                if depth == 0:
                    folder_name = hierarchy.get('client_name', folder_code)
                elif depth == 1:
                    folder_name = hierarchy.get('process_group_name', folder_code)
                elif depth == 2:
                    folder_name = hierarchy.get('process_area_name', folder_code)
        except:
            pass

    node_types = ['client', 'process_group', 'process_area', 'subarea']
    node_type = node_types[depth] if depth < len(node_types) else f'level{depth}'

    all_codes = parent_codes + [folder_code]
    key = f"{node_type}-{'-'.join(all_codes)}"

    node = {
        'key': key,
        'title': f'{folder_code} - {folder_name}',
        'type': node_type,
        'code': folder_code,
        'name': folder_name,
        'depth': depth,
        'children': [],
        'rule_count': 0
    }

    if depth > 0 and parent_codes:
        node['parent_code'] = parent_codes[-1]
    if depth > 1 and len(parent_codes) > 1:
        node['client_code'] = parent_codes[0]

    for subdir in sorted(path.iterdir()):
        if not subdir.is_dir() or subdir.name.startswith('.'):
            continue

        child_node = scan_directory_recursive(subdir, depth + 1, max_depth, all_codes)
        if child_node:
            node['children'].append(child_node)
            node['rule_count'] += child_node['rule_count']

    rule_files = list(path.glob('rule-*.json'))
    node['rule_count'] += len(rule_files)

    return node

@hierarchy_bp.route('/hierarchy/tree', methods=['GET'])
def get_hierarchy_tree():
    """Get complete hierarchy tree structure with arbitrary depth support."""
    try:
        tree = []

        if not RULES_PATH.exists():
            return jsonify({'tree': []})

        for top_dir in sorted(RULES_PATH.iterdir()):
            if not top_dir.is_dir() or top_dir.name.startswith('.'):
                continue

            node = scan_directory_recursive(top_dir)
            if node:
                tree.append(node)

        return jsonify({
            'tree': tree,
            'total_nodes': len(tree),
            'supports_arbitrary_depth': True
        })

    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/clients', methods=['GET'])
def get_clients():
    """Get all clients by scanning folder structure."""
    try:
        clients = []

        if not RULES_PATH.exists():
            return jsonify({'clients': []})

        for client_dir in sorted(RULES_PATH.iterdir()):
            if not client_dir.is_dir() or client_dir.name.startswith('.'):
                continue

            client_code = client_dir.name
            client_name = client_code

            sample_rule = next(client_dir.rglob('rule-*.json'), None)
            if sample_rule:
                with open(sample_rule, 'r') as f:
                    rule_data = json.load(f)
                    client_name = rule_data.get('hierarchy', {}).get('client_name', client_code)

            clients.append({
                'code': client_code,
                'name': client_name,
                'is_active': True
            })

        return jsonify({'clients': clients})
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/clients/<string:client_code>/process-groups', methods=['GET'])
def get_process_groups_by_client(client_code):
    """Get process groups for a specific client by scanning folder structure."""
    try:
        process_groups = []
        client_dir = RULES_PATH / client_code

        if not client_dir.exists():
            return jsonify({'process_groups': []})

        for pg_dir in sorted(client_dir.iterdir()):
            if not pg_dir.is_dir() or pg_dir.name.startswith('.'):
                continue

            pg_code = pg_dir.name
            pg_name = pg_code

            sample_rule = next(pg_dir.rglob('rule-*.json'), None)
            if sample_rule:
                with open(sample_rule, 'r') as f:
                    rule_data = json.load(f)
                    pg_name = rule_data.get('hierarchy', {}).get('process_group_name', pg_code)

            process_groups.append({
                'code': pg_code,
                'name': pg_name,
                'client_code': client_code,
                'is_active': True
            })

        return jsonify({'process_groups': process_groups})
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/process-groups/<string:client_code>/<string:process_group_code>/process-areas', methods=['GET'])
def get_process_areas_by_process_group(client_code, process_group_code):
    """Get process areas for a specific process group by scanning folder structure."""
    try:
        process_areas = []
        pg_dir = RULES_PATH / client_code / process_group_code

        if not pg_dir.exists():
            return jsonify({'process_areas': []})

        for pa_dir in sorted(pg_dir.iterdir()):
            if not pa_dir.is_dir() or pa_dir.name.startswith('.'):
                continue

            pa_code = pa_dir.name
            pa_name = pa_code

            sample_rule = next(pa_dir.glob('rule-*.json'), None)
            if sample_rule:
                with open(sample_rule, 'r') as f:
                    rule_data = json.load(f)
                    pa_name = rule_data.get('hierarchy', {}).get('process_area_name', pa_code)

            process_areas.append({
                'code': pa_code,
                'name': pa_name,
                'process_group_code': process_group_code,
                'client_code': client_code,
                'is_active': True
            })

        return jsonify({'process_areas': process_areas})
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/process-areas', methods=['GET'])
def get_all_process_areas():
    """Get all process areas by scanning folder structure."""
    try:
        process_areas = []

        if not RULES_PATH.exists():
            return jsonify({'process_areas': []})

        for client_dir in sorted(RULES_PATH.iterdir()):
            if not client_dir.is_dir() or client_dir.name.startswith('.'):
                continue

            client_code = client_dir.name

            for pg_dir in sorted(client_dir.iterdir()):
                if not pg_dir.is_dir() or pg_dir.name.startswith('.'):
                    continue

                pg_code = pg_dir.name

                for pa_dir in sorted(pg_dir.iterdir()):
                    if not pa_dir.is_dir() or pa_dir.name.startswith('.'):
                        continue

                    pa_code = pa_dir.name
                    pa_name = pa_code
                    pg_name = pg_code
                    client_name = client_code

                    sample_rule = next(pa_dir.glob('rule-*.json'), None)
                    if sample_rule:
                        with open(sample_rule, 'r') as f:
                            rule_data = json.load(f)
                            h = rule_data.get('hierarchy', {})
                            pa_name = h.get('process_area_name', pa_code)
                            pg_name = h.get('process_group_name', pg_code)
                            client_name = h.get('client_name', client_code)

                    process_areas.append({
                        'code': pa_code,
                        'name': pa_name,
                        'process_group_code': pg_code,
                        'process_group_name': pg_name,
                        'client_code': client_code,
                        'client_name': client_name,
                        'is_active': True
                    })

        return jsonify({'process_areas': process_areas})
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/breadcrumb/<path:node_type>/<int:node_id>', methods=['GET'])
def get_breadcrumb(node_type, node_id):
    """Get breadcrumb trail for navigation from file system."""
    try:
        breadcrumb = []

        if node_type == 'rule':
            rule_found = None
            for rule_file in RULES_PATH.rglob(f'rule-{node_id}.json'):
                with open(rule_file, 'r') as f:
                    rule_found = json.load(f)
                break

            if not rule_found:
                return jsonify({'error': 'Rule not found'}), 404

            h = rule_found.get('hierarchy', {})
            breadcrumb = [
                {'type': 'client', 'name': h.get('client_name'), 'code': h.get('client_code')},
                {'type': 'process_group', 'name': h.get('process_group_name'), 'code': h.get('process_group_code')},
                {'type': 'process_area', 'name': h.get('process_area_name'), 'code': h.get('process_area_code')},
                {'type': 'rule', 'id': rule_found['id'], 'name': rule_found['name']}
            ]
        elif node_type in ['process_area', 'process_group', 'client']:
            return jsonify({'breadcrumb': [], 'message': 'Breadcrumb by ID not fully supported'})

        return jsonify({'breadcrumb': breadcrumb})

    except Exception as e:
        return jsonify({'error': str(e)}), 500