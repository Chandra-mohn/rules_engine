# Rules Authoring UI - Project Structure

```
ui-prototype/
├── backend/                          # Flask backend
│   ├── app.py                        # Main Flask application
│   ├── models.py                     # SQLAlchemy models
│   ├── api/                          # API endpoints
│   │   ├── __init__.py
│   │   ├── rules.py                  # Rules CRUD endpoints
│   │   └── validation.py             # Rule validation endpoints
│   ├── services/                     # Business logic
│   │   ├── __init__.py
│   │   ├── rule_service.py           # Rule management service
│   │   └── java_bridge.py            # Java integration service
│   ├── database/                     # Database files
│   │   ├── __init__.py
│   │   ├── schema.sql                # Database schema
│   │   └── rules.db                  # SQLite database file
│   ├── config.py                     # Configuration settings
│   └── requirements.txt              # Python dependencies
│
├── frontend/                         # React frontend
│   ├── public/
│   │   └── index.html
│   ├── src/
│   │   ├── components/               # React components
│   │   │   ├── RulesList.jsx         # Rules table view
│   │   │   ├── RuleEditor.jsx        # Rule editor with Monaco
│   │   │   ├── RuleForm.jsx          # Rule metadata form
│   │   │   └── Layout.jsx            # App layout
│   │   ├── services/                 # API services
│   │   │   └── api.js                # Axios API client
│   │   ├── utils/                    # Utilities
│   │   │   ├── rulesSyntax.js        # Rules language definition
│   │   │   └── validation.js         # Client-side validation
│   │   ├── App.jsx                   # Main App component
│   │   ├── index.js                  # React entry point
│   │   └── App.css                   # Styles
│   ├── package.json                  # Node dependencies
│   └── package-lock.json
│
├── java-bridge/                      # Java integration
│   ├── RuleValidator.java            # Command-line rule validator
│   ├── ValidationResult.java         # Validation result model
│   └── build.sh                      # Build script
│
├── docs/                             # Documentation
│   ├── API.md                        # API documentation
│   ├── SETUP.md                      # Setup instructions
│   └── ARCHITECTURE.md               # Architecture overview
│
└── scripts/                          # Utility scripts
    ├── setup.sh                      # Environment setup
    ├── start-dev.sh                  # Development server startup
    └── build.sh                      # Production build
```