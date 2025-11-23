import sys
from pathlib import Path

# Add backend directory to Python path
sys.path.insert(0, str(Path(__file__).parent))

from flask import Flask
from flask_cors import CORS
from api.rules_file import rules_file_bp as rules_bp
from api.schema import schema_bp
from api.lists import lists_bp
from api.hierarchy import hierarchy_bp
from api.contexts_file import contexts_file_bp as contexts_bp
from api.java_files import java_files_bp
from config import Config
from cli_commands_file import register_commands
import os

def create_app():
    """Application factory pattern."""
    app = Flask(__name__)
    app.config.from_object(Config)

    CORS(app, origins=Config.CORS_ORIGINS)

    app.register_blueprint(rules_bp, url_prefix='/api')
    app.register_blueprint(schema_bp, url_prefix='/api')
    app.register_blueprint(lists_bp, url_prefix='/api')
    app.register_blueprint(hierarchy_bp, url_prefix='/api')
    app.register_blueprint(contexts_bp, url_prefix='/api')
    app.register_blueprint(java_files_bp)

    register_commands(app)

    with app.app_context():
        for dir_path in [Config.RULES_DIR, Config.LISTS_DIR, Config.SCHEMAS_DIR, Config.CONTEXTS_DIR]:
            os.makedirs(dir_path, exist_ok=True)
        print("✅ Storage initialized")

    @app.route('/api/health')
    def health_check():
        """Health check endpoint."""
        return {'status': 'healthy', 'message': 'Rules authoring API is running'}

    return app


if __name__ == '__main__':
    app = create_app()
    app.run(debug=True, host='0.0.0.0', port=5001)
