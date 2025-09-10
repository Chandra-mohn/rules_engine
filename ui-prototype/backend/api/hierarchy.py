from flask import Blueprint, request, jsonify
from models import Client, ProcessGroup, ProcessArea, ClientSchema, ProcessGroupSchema, ProcessAreaSchema, db
from config import Config

hierarchy_bp = Blueprint('hierarchy', __name__)
client_schema = ClientSchema()
clients_schema = ClientSchema(many=True)
process_group_schema = ProcessGroupSchema()
process_groups_schema = ProcessGroupSchema(many=True)
process_area_schema = ProcessAreaSchema()
process_areas_schema = ProcessAreaSchema(many=True)

@hierarchy_bp.route('/hierarchy/tree', methods=['GET'])
def get_hierarchy_tree():
    """Get complete hierarchy tree structure for navigation."""
    try:
        # Get all clients with their process groups and process areas
        clients = Client.query.filter_by(is_active=True).order_by(Client.name).all()
        
        tree = []
        for client in clients:
            client_node = {
                'key': f'client-{client.id}',
                'title': f'{client.code} - {client.name}',
                'type': 'client',
                'id': client.id,
                'code': client.code,
                'name': client.name,
                'children': []
            }
            
            # Get process groups for this client
            for pg in client.process_groups:
                if pg.is_active:
                    pg_node = {
                        'key': f'process-group-{pg.id}',
                        'title': f'{pg.code} - {pg.name}',
                        'type': 'process_group',
                        'id': pg.id,
                        'code': pg.code,
                        'name': pg.name,
                        'client_id': client.id,
                        'children': []
                    }
                    
                    # Get process areas for this process group
                    for pa in pg.process_areas:
                        if pa.is_active:
                            pa_node = {
                                'key': f'process-area-{pa.id}',
                                'title': f'{pa.code} - {pa.name}',
                                'type': 'process_area',
                                'id': pa.id,
                                'code': pa.code,
                                'name': pa.name,
                                'process_group_id': pg.id,
                                'children': []
                            }
                            
                            # Get rules for this process area (just names for tree)
                            for rule in pa.rules:
                                rule_node = {
                                    'key': f'rule-{rule.id}',
                                    'title': rule.name,
                                    'type': 'rule',
                                    'id': rule.id,
                                    'name': rule.name,
                                    'status': rule.status,
                                    'process_area_id': pa.id,
                                    'isLeaf': True
                                }
                                pa_node['children'].append(rule_node)
                            
                            pg_node['children'].append(pa_node)
                    
                    client_node['children'].append(pg_node)
            
            tree.append(client_node)
        
        return jsonify({'tree': tree})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/clients', methods=['GET'])
def get_clients():
    """Get all clients."""
    try:
        clients = Client.query.filter_by(is_active=True).order_by(Client.name).all()
        return jsonify({'clients': [client.to_dict() for client in clients]})
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/clients/<int:client_id>/process-groups', methods=['GET'])
def get_process_groups_by_client(client_id):
    """Get process groups for a specific client."""
    try:
        process_groups = ProcessGroup.query.filter_by(
            client_id=client_id, 
            is_active=True
        ).order_by(ProcessGroup.name).all()
        
        return jsonify({'process_groups': [pg.to_dict() for pg in process_groups]})
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/process-groups/<int:process_group_id>/process-areas', methods=['GET'])
def get_process_areas_by_process_group(process_group_id):
    """Get process areas for a specific process group."""
    try:
        process_areas = ProcessArea.query.filter_by(
            process_group_id=process_group_id,
            is_active=True
        ).order_by(ProcessArea.name).all()
        
        return jsonify({'process_areas': [pa.to_dict() for pa in process_areas]})
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/process-areas', methods=['GET'])
def get_all_process_areas():
    """Get all process areas with hierarchy information."""
    try:
        process_areas = db.session.query(ProcessArea)\
            .join(ProcessGroup)\
            .join(Client)\
            .filter(ProcessArea.is_active == True)\
            .filter(ProcessGroup.is_active == True)\
            .filter(Client.is_active == True)\
            .order_by(Client.name, ProcessGroup.name, ProcessArea.name)\
            .all()
        
        return jsonify({'process_areas': [pa.to_dict() for pa in process_areas]})
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@hierarchy_bp.route('/hierarchy/breadcrumb/<path:node_type>/<int:node_id>', methods=['GET'])
def get_breadcrumb(node_type, node_id):
    """Get breadcrumb trail for navigation."""
    try:
        breadcrumb = []
        
        if node_type == 'rule':
            from models import Rule
            rule = Rule.query.get_or_404(node_id)
            breadcrumb = [
                {'type': 'client', 'id': rule.process_area.process_group.client.id, 
                 'name': rule.process_area.process_group.client.name, 
                 'code': rule.process_area.process_group.client.code},
                {'type': 'process_group', 'id': rule.process_area.process_group.id,
                 'name': rule.process_area.process_group.name, 
                 'code': rule.process_area.process_group.code},
                {'type': 'process_area', 'id': rule.process_area.id,
                 'name': rule.process_area.name, 
                 'code': rule.process_area.code},
                {'type': 'rule', 'id': rule.id, 'name': rule.name}
            ]
        elif node_type == 'process_area':
            pa = ProcessArea.query.get_or_404(node_id)
            breadcrumb = [
                {'type': 'client', 'id': pa.process_group.client.id, 
                 'name': pa.process_group.client.name, 
                 'code': pa.process_group.client.code},
                {'type': 'process_group', 'id': pa.process_group.id,
                 'name': pa.process_group.name, 
                 'code': pa.process_group.code},
                {'type': 'process_area', 'id': pa.id, 'name': pa.name, 'code': pa.code}
            ]
        elif node_type == 'process_group':
            pg = ProcessGroup.query.get_or_404(node_id)
            breadcrumb = [
                {'type': 'client', 'id': pg.client.id, 'name': pg.client.name, 'code': pg.client.code},
                {'type': 'process_group', 'id': pg.id, 'name': pg.name, 'code': pg.code}
            ]
        elif node_type == 'client':
            client = Client.query.get_or_404(node_id)
            breadcrumb = [
                {'type': 'client', 'id': client.id, 'name': client.name, 'code': client.code}
            ]
        
        return jsonify({'breadcrumb': breadcrumb})
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500