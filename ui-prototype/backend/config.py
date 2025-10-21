import os
from pathlib import Path

class Config:
    BASE_DIR = Path(__file__).parent

    RULES_DIR = BASE_DIR / 'rules'
    LISTS_DIR = BASE_DIR / 'lists'
    SCHEMAS_DIR = BASE_DIR / 'schemas'
    CONTEXTS_DIR = BASE_DIR / 'contexts'

    ANTLR_GRAMMAR_PATH = BASE_DIR.parent / 'java-bridge' / 'src' / 'main' / 'antlr4'

    SECRET_KEY = os.environ.get('SECRET_KEY') or 'dev-secret-key-change-in-production'
    CORS_ORIGINS = ['http://localhost:3000']

    DEFAULT_PAGE_SIZE = 10
    MAX_PAGE_SIZE = 100