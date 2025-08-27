from flask import Flask
from flask_cors import CORS
from models import db
from api.rules import rules_bp
from api.schema import schema_bp
from config import Config
import os

def create_app():
    """Application factory pattern."""
    app = Flask(__name__)
    app.config.from_object(Config)
    
    # Initialize extensions
    db.init_app(app)
    CORS(app, origins=Config.CORS_ORIGINS)
    
    # Register blueprints
    app.register_blueprint(rules_bp, url_prefix='/api')
    app.register_blueprint(schema_bp, url_prefix='/api')
    
    # Create database tables
    with app.app_context():
        # Ensure database directory exists
        os.makedirs(Config.DATABASE_PATH.parent, exist_ok=True)
        
        # Create tables
        db.create_all()
        
        # Insert sample data if database is empty
        from models import Rule
        if Rule.query.count() == 0:
            sample_rules = [
                Rule(
                    name='creditScoreCheck',
                    description='Basic credit score validation',
                    content='rule creditScoreCheck:\n    if applicant.creditScore >= 700 then approveApplication\n    if applicant.creditScore < 600 then rejectApplication',
                    status='active',
                    validation_status='valid'
                ),
                Rule(
                    name='ageVerification',
                    description='Minimum age requirement',
                    content='rule ageVerification:\n    if applicant.age < 18 then rejectApplication\n    if applicant.age >= 18 then approveApplication',
                    status='active',
                    validation_status='valid'
                ),
                Rule(
                    name='businessDateCheck',
                    description='Business date validation example',
                    content='rule businessDateCheck:\n    if applicant.applicationDate after business_date then conditionalApproval\n    if applicant.applicationDate before business_date then rejectApplication',
                    status='draft',
                    validation_status='pending'
                )
            ]
            
            for rule in sample_rules:
                db.session.add(rule)
            
            db.session.commit()
    
    @app.route('/api/health')
    def health_check():
        """Health check endpoint."""
        return {'status': 'healthy', 'message': 'Rules authoring API is running'}
    
    return app

if __name__ == '__main__':
    app = create_app()
    app.run(debug=True, host='0.0.0.0', port=5001)
