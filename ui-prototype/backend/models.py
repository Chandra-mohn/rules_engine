from flask_sqlalchemy import SQLAlchemy
from datetime import datetime
from marshmallow_sqlalchemy import SQLAlchemyAutoSchema
from marshmallow import fields

db = SQLAlchemy()

class Client(db.Model):
    __tablename__ = 'clients'
    
    id = db.Column(db.Integer, primary_key=True)
    code = db.Column(db.String(10), nullable=False, unique=True)
    name = db.Column(db.String(100), nullable=False)
    description = db.Column(db.Text)
    is_active = db.Column(db.Boolean, default=True)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    process_groups = db.relationship('ProcessGroup', backref='client', lazy=True, cascade='all, delete-orphan')
    
    def to_dict(self):
        return {
            'id': self.id,
            'code': self.code,
            'name': self.name,
            'description': self.description,
            'is_active': self.is_active,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None
        }

class ProcessGroup(db.Model):
    __tablename__ = 'process_groups'
    
    id = db.Column(db.Integer, primary_key=True)
    client_id = db.Column(db.Integer, db.ForeignKey('clients.id'), nullable=False)
    code = db.Column(db.String(20), nullable=False)
    name = db.Column(db.String(100), nullable=False)
    description = db.Column(db.Text)
    is_active = db.Column(db.Boolean, default=True)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    process_areas = db.relationship('ProcessArea', backref='process_group', lazy=True, cascade='all, delete-orphan')
    
    # Unique constraint
    __table_args__ = (db.UniqueConstraint('client_id', 'code', name='uq_client_process_group'),)
    
    def to_dict(self):
        return {
            'id': self.id,
            'client_id': self.client_id,
            'code': self.code,
            'name': self.name,
            'description': self.description,
            'is_active': self.is_active,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None,
            'client_code': self.client.code if self.client else None,
            'client_name': self.client.name if self.client else None
        }

class ProcessArea(db.Model):
    __tablename__ = 'process_areas'
    
    id = db.Column(db.Integer, primary_key=True)
    process_group_id = db.Column(db.Integer, db.ForeignKey('process_groups.id'), nullable=False)
    code = db.Column(db.String(20), nullable=False)
    name = db.Column(db.String(100), nullable=False)
    description = db.Column(db.Text)
    is_active = db.Column(db.Boolean, default=True)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    rules = db.relationship('Rule', backref='process_area', lazy=True, cascade='all, delete-orphan')
    
    # Unique constraint
    __table_args__ = (db.UniqueConstraint('process_group_id', 'code', name='uq_process_group_area'),)
    
    def to_dict(self):
        return {
            'id': self.id,
            'process_group_id': self.process_group_id,
            'code': self.code,
            'name': self.name,
            'description': self.description,
            'is_active': self.is_active,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None,
            'process_group_code': self.process_group.code if self.process_group else None,
            'process_group_name': self.process_group.name if self.process_group else None,
            'client_id': self.process_group.client_id if self.process_group else None,
            'client_code': self.process_group.client.code if self.process_group and self.process_group.client else None,
            'client_name': self.process_group.client.name if self.process_group and self.process_group.client else None
        }

    @property
    def actionsets(self):
        return [rule for rule in self.rules if rule.item_type == 'actionset']

    @property
    def actual_rules(self):
        return [rule for rule in self.rules if rule.item_type == 'rule']

class RuleContext(db.Model):
    """
    Rule Context - Persistent test/execution data for rules.
    Replaces schema_entity/schema_attribute tables.

    Two types:
    1. Schema Templates (is_schema_template=True) - Define available structure
    2. Test Contexts (is_schema_template=False) - Actual test data
    """
    __tablename__ = 'rule_context'

    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(200), nullable=False, unique=True)
    description = db.Column(db.Text)
    context_data = db.Column(db.JSON, nullable=False)

    # Schema template flag
    is_schema_template = db.Column(db.Boolean, default=False)

    # Version tracking
    version = db.Column(db.String(50))

    # Optional client scoping
    client_id = db.Column(db.Integer, db.ForeignKey('clients.id'))

    # Timestamps
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Relationships
    client = db.relationship('Client', backref='contexts')

    def to_dict(self):
        return {
            'id': self.id,
            'name': self.name,
            'description': self.description,
            'context_data': self.context_data,
            'is_schema_template': self.is_schema_template,
            'version': self.version,
            'client_id': self.client_id,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None
        }

    def __repr__(self):
        return f'<RuleContext {self.name} ({"Schema" if self.is_schema_template else "Test"})>'

class Rule(db.Model):
    __tablename__ = 'rules'

    id = db.Column(db.Integer, primary_key=True)
    process_area_id = db.Column(db.Integer, db.ForeignKey('process_areas.id'), nullable=False)
    name = db.Column(db.String(100), nullable=False)
    description = db.Column(db.Text)
    content = db.Column(db.Text, nullable=False)
    status = db.Column(db.String(10), default='DRAFT')  # DRAFT, VALID, PEND, SCHD, PROD (consolidated validation + lifecycle)
    effective_date = db.Column(db.Date)
    expiry_date = db.Column(db.Date)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    created_by = db.Column(db.String(50), default='system')
    updated_by = db.Column(db.String(50), default='system')
    # validation_status column exists in DB but consolidated into status field
    validation_message = db.Column(db.Text)  # Keep for validation error messages
    version = db.Column(db.Integer, default=1)
    schema_version = db.Column(db.String(20), default='modern')  # modern, legacy
    item_type = db.Column(db.String(15), default='rule')  # 'rule', 'actionset', 'mon_rule', 'non_mon_rule'
    java_file_path = db.Column(db.String(255))  # Path to Java implementation file (for actions only)

    # Context reference (NEW)
    context_id = db.Column(db.Integer, db.ForeignKey('rule_context.id'))

    # Relationships
    history = db.relationship('RuleHistory', backref='rule', lazy=True, cascade='all, delete-orphan')
    context = db.relationship('RuleContext', backref='rules')
    
    # Unique constraint: combination of process_area_id, name, and effective_date must be unique
    __table_args__ = (db.UniqueConstraint('process_area_id', 'name', 'effective_date', name='uq_rule_composite'),)
    
    def to_dict(self):
        return {
            'id': self.id,
            'process_area_id': self.process_area_id,
            'name': self.name,
            'description': self.description,
            'content': self.content,
            'status': self.status,
            'effective_date': self.effective_date.isoformat() if self.effective_date else None,
            'expiry_date': self.expiry_date.isoformat() if self.expiry_date else None,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None,
            'created_by': self.created_by,
            'updated_by': self.updated_by,
            'validation_message': self.validation_message,
            'version': self.version,
            'schema_version': self.schema_version,
            'item_type': self.item_type,
            'java_file_path': self.java_file_path,
            'context_id': self.context_id,
            # Include hierarchy information
            'process_area_code': self.process_area.code if self.process_area else None,
            'process_area_name': self.process_area.name if self.process_area else None,
            'process_group_id': self.process_area.process_group_id if self.process_area else None,
            'process_group_code': self.process_area.process_group.code if self.process_area and self.process_area.process_group else None,
            'process_group_name': self.process_area.process_group.name if self.process_area and self.process_area.process_group else None,
            'client_id': self.process_area.process_group.client_id if self.process_area and self.process_area.process_group else None,
            'client_code': self.process_area.process_group.client.code if self.process_area and self.process_area.process_group and self.process_area.process_group.client else None,
            'client_name': self.process_area.process_group.client.name if self.process_area and self.process_area.process_group and self.process_area.process_group.client else None
        }

    @property
    def is_actionset(self):
        return self.item_type == 'actionset'

    @property
    def is_rule(self):
        return self.item_type == 'rule'

    @property
    def is_monetary_rule(self):
        return self.item_type == 'mon_rule'

    @property
    def is_non_monetary_rule(self):
        return self.item_type == 'non_mon_rule'


class RuleHistory(db.Model):
    __tablename__ = 'rule_history'
    
    id = db.Column(db.Integer, primary_key=True)
    rule_id = db.Column(db.Integer, db.ForeignKey('rules.id'), nullable=False)
    process_area_id = db.Column(db.Integer, db.ForeignKey('process_areas.id'))
    name = db.Column(db.String(100), nullable=False)
    content = db.Column(db.Text, nullable=False)
    version = db.Column(db.Integer, nullable=False)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    created_by = db.Column(db.String(50), default='system')
    change_reason = db.Column(db.Text)


class RuleList(db.Model):
    __tablename__ = 'rule_lists'
    
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100), nullable=False, unique=True)
    description = db.Column(db.Text)
    data_type = db.Column(db.String(20), nullable=False)  # string, number, boolean
    list_values = db.Column(db.Text, nullable=False)  # JSON array
    schema_version = db.Column(db.String(20), default='both')
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    created_by = db.Column(db.String(50), default='system')
    updated_by = db.Column(db.String(50), default='system')
    
    def to_dict(self):
        import json
        return {
            'id': self.id,
            'name': self.name,
            'description': self.description,
            'data_type': self.data_type,
            'values': json.loads(self.list_values),
            'schema_version': self.schema_version,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None,
            'created_by': self.created_by,
            'updated_by': self.updated_by
        }
    
    def get_values_as_set(self):
        """Get values as a Python set for fast membership testing."""
        import json
        return set(json.loads(self.list_values))

# Marshmallow schemas for serialization
class ClientSchema(SQLAlchemyAutoSchema):
    class Meta:
        model = Client
        load_instance = True
        
    created_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')
    updated_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')

class ProcessGroupSchema(SQLAlchemyAutoSchema):
    class Meta:
        model = ProcessGroup
        load_instance = True
        
    created_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')
    updated_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')

class ProcessAreaSchema(SQLAlchemyAutoSchema):
    class Meta:
        model = ProcessArea
        load_instance = True
        
    created_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')
    updated_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')

class RuleSchema(SQLAlchemyAutoSchema):
    class Meta:
        model = Rule
        load_instance = True
        
    created_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')
    updated_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')

class RuleHistorySchema(SQLAlchemyAutoSchema):
    class Meta:
        model = RuleHistory
        load_instance = True

    created_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')


class RuleContextSchema(SQLAlchemyAutoSchema):
    """Marshmallow schema for RuleContext model serialization."""
    class Meta:
        model = RuleContext
        load_instance = True
        include_fk = True

    created_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')
    updated_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')


# Schema Management Tables
class SchemaEntity(db.Model):
    __tablename__ = 'schema_entities'
    
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(50), nullable=False)
    description = db.Column(db.Text)
    client_id = db.Column(db.Integer, db.ForeignKey('clients.id'))
    is_active = db.Column(db.Boolean, default=True)
    version = db.Column(db.Integer, default=1)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Relationships
    attributes = db.relationship('SchemaAttribute', backref='entity', lazy=True, cascade='all, delete-orphan')
    client = db.relationship('Client', backref='schema_entities')
    
    # Unique constraint
    __table_args__ = (db.UniqueConstraint('client_id', 'name', name='uq_client_schema_entity'),)
    
    def to_dict(self):
        return {
            'id': self.id,
            'name': self.name,
            'description': self.description,
            'client_id': self.client_id,
            'is_active': self.is_active,
            'version': self.version,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None,
            'attributes': [attr.to_dict() for attr in self.attributes] if self.attributes else []
        }

class SchemaAttribute(db.Model):
    __tablename__ = 'schema_attributes'
    
    id = db.Column(db.Integer, primary_key=True)
    entity_id = db.Column(db.Integer, db.ForeignKey('schema_entities.id'), nullable=False)
    name = db.Column(db.String(50), nullable=False)
    data_type = db.Column(db.String(20), nullable=False)  # 'number', 'string', 'boolean', 'date', 'datetime'
    java_type = db.Column(db.String(50))  # 'int', 'String', 'boolean', 'LocalDate', 'LocalDateTime'
    min_value = db.Column(db.Numeric(precision=20, scale=6))
    max_value = db.Column(db.Numeric(precision=20, scale=6))
    allowed_values = db.Column(db.Text)  # JSON array for enums
    is_required = db.Column(db.Boolean, default=False)
    description = db.Column(db.Text)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    
    # Unique constraint
    __table_args__ = (db.UniqueConstraint('entity_id', 'name', name='uq_entity_attribute'),)
    
    def to_dict(self):
        return {
            'id': self.id,
            'entity_id': self.entity_id,
            'name': self.name,
            'data_type': self.data_type,
            'java_type': self.java_type,
            'min_value': float(self.min_value) if self.min_value is not None else None,
            'max_value': float(self.max_value) if self.max_value is not None else None,
            'allowed_values': self.allowed_values,
            'is_required': self.is_required,
            'description': self.description,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None
        }

class SchemaVersion(db.Model):
    __tablename__ = 'schema_versions'
    
    id = db.Column(db.Integer, primary_key=True)
    client_id = db.Column(db.Integer, db.ForeignKey('clients.id'))
    version = db.Column(db.Integer, nullable=False)
    changes = db.Column(db.Text)  # JSON of changes made
    created_by = db.Column(db.String(50), nullable=False)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    
    # Relationships
    client = db.relationship('Client', backref='schema_versions')
    
    def to_dict(self):
        return {
            'id': self.id,
            'client_id': self.client_id,
            'version': self.version,
            'changes': self.changes,
            'created_by': self.created_by,
            'created_at': self.created_at.isoformat() if self.created_at else None
        }

# Schema Marshmallow Classes
class SchemaEntitySchema(SQLAlchemyAutoSchema):
    class Meta:
        model = SchemaEntity
        load_instance = True
        
    created_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')
    updated_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')
    attributes = fields.Nested('SchemaAttributeSchema', many=True, exclude=('entity',))

class SchemaAttributeSchema(SQLAlchemyAutoSchema):
    class Meta:
        model = SchemaAttribute
        load_instance = True
        
    created_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')
    updated_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')

class SchemaVersionSchema(SQLAlchemyAutoSchema):
    class Meta:
        model = SchemaVersion
        load_instance = True
        
    created_at = fields.DateTime(format='%Y-%m-%dT%H:%M:%S')