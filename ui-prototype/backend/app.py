from flask import Flask
from flask_cors import CORS
from models import db
from api.rules import rules_bp
from api.schema import schema_bp
from api.lists import lists_bp
from api.hierarchy import hierarchy_bp
from api.context import bp as context_bp
from config import Config
from cli_commands import register_commands
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
    app.register_blueprint(lists_bp, url_prefix='/api')
    app.register_blueprint(hierarchy_bp, url_prefix='/api')
    app.register_blueprint(context_bp)

    # Register CLI commands
    register_commands(app)

    # Create database tables
    with app.app_context():
        # Ensure database directory exists
        os.makedirs(Config.DATABASE_PATH.parent, exist_ok=True)

        # Create tables
        db.create_all()

        # Auto-seed demo data only if AUTO_SEED_DEMO_DATA env var is set
        if os.getenv('AUTO_SEED_DEMO_DATA', 'false').lower() == 'true':
            from models import Rule
            if Rule.query.count() == 0:
                print("🌱 AUTO_SEED_DEMO_DATA enabled - seeding demo data...")
                from fixtures import create_demo_data
                try:
                    create_demo_data()
                except Exception as e:
                    print(f"⚠️  Warning: Failed to auto-seed demo data: {e}")
                    print("   You can manually seed with: flask seed-demo")
            else:
                print("ℹ️  Database already contains data - skipping auto-seed")
        else:
            from models import Rule
            if Rule.query.count() == 0:
                print("\nℹ️  Database is empty. To seed with demo data, run:")
                print("   flask seed-demo")
                print("   OR set AUTO_SEED_DEMO_DATA=true environment variable\n")

    @app.route('/api/health')
    def health_check():
        """Health check endpoint."""
        return {'status': 'healthy', 'message': 'Rules authoring API is running'}

    return app


if __name__ == '__main__':
    app = create_app()
    app.run(debug=True, host='0.0.0.0', port=5001)
