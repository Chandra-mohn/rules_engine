from flask_sqlalchemy import SQLAlchemy
from datetime import datetime
from marshmallow_sqlalchemy import SQLAlchemyAutoSchema
from marshmallow import fields

db = SQLAlchemy()

class Rule(db.Model):
    __tablename__ = 'rules'
    
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100), nullable=False, unique=True)
    description = db.Column(db.Text)
    content = db.Column(db.Text, nullable=False)
    status = db.Column(db.String(20), default='draft')  # draft, active, inactive, error
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    created_by = db.Column(db.String(50), default='system')
    updated_by = db.Column(db.String(50), default='system')
    validation_status = db.Column(db.String(20), default='pending')  # pending, valid, invalid
    validation_message = db.Column(db.Text)
    version = db.Column(db.Integer, default=1)
    schema_version = db.Column(db.String(20), default='modern')  # modern, legacy
    
    # Relationship to history
    history = db.relationship('RuleHistory', backref='rule', lazy=True, cascade='all, delete-orphan')
    
    def to_dict(self):
        return {
            'id': self.id,
            'name': self.name,
            'description': self.description,
            'content': self.content,
            'status': self.status,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'updated_at': self.updated_at.isoformat() if self.updated_at else None,
            'created_by': self.created_by,
            'updated_by': self.updated_by,
            'validation_status': self.validation_status,
            'validation_message': self.validation_message,
            'version': self.version,
            'schema_version': self.schema_version
        }

class RuleHistory(db.Model):
    __tablename__ = 'rule_history'
    
    id = db.Column(db.Integer, primary_key=True)
    rule_id = db.Column(db.Integer, db.ForeignKey('rules.id'), nullable=False)
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
    
    def to_dict(self):
        return {
            'id': self.id,
            'rule_id': self.rule_id,
            'name': self.name,
            'content': self.content,
            'version': self.version,
            'created_at': self.created_at.isoformat() if self.created_at else None,
            'created_by': self.created_by,
            'change_reason': self.change_reason
        }

# Marshmallow schemas for serialization
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