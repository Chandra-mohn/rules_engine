import os
from pathlib import Path

class Config:
    BASE_DIR = Path(__file__).parent

    # Point to top-level rules/ directory
    RULES_DIR = BASE_DIR.parent / 'rules'
    LISTS_DIR = BASE_DIR.parent / 'rules' / 'lists'
    SCHEMAS_DIR = BASE_DIR.parent / 'rules' / 'schemas'
    CONTEXTS_DIR = BASE_DIR.parent / 'rules' / 'contexts'

    ANTLR_GRAMMAR_PATH = BASE_DIR.parent / 'java-bridge' / 'src' / 'main' / 'antlr4'

    SECRET_KEY = os.environ.get('SECRET_KEY') or 'dev-secret-key-change-in-production'
    CORS_ORIGINS = ['http://localhost:3000']

    DEFAULT_PAGE_SIZE = 10
    MAX_PAGE_SIZE = 100