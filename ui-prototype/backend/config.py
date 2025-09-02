import os
from pathlib import Path

class Config:
    # Database configuration
    BASE_DIR = Path(__file__).parent
    DATABASE_PATH = BASE_DIR / 'database' / 'rules.db'
    SQLALCHEMY_DATABASE_URI = f'sqlite:///{DATABASE_PATH}'
    SQLALCHEMY_TRACK_MODIFICATIONS = False
    
    # Java integration
    JAVA_RULES_ENGINE_PATH = BASE_DIR.parent / 'java-bridge'
    JAVA_CLASSPATH = JAVA_RULES_ENGINE_PATH / 'classes'
    
    # API configuration
    SECRET_KEY = os.environ.get('SECRET_KEY') or 'dev-secret-key-change-in-production'
    
    # CORS configuration
    CORS_ORIGINS = ['http://localhost:3000']  # React dev server
    
    # Pagination
    DEFAULT_PAGE_SIZE = 10
    MAX_PAGE_SIZE = 100