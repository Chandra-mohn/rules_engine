# Rules Authoring UI - Prototype

A web-based interface for authoring and managing credit card processing rules with syntax highlighting, validation, and testing capabilities.

## 🏗️ Architecture

```
┌─────────────────┐    HTTP/REST    ┌─────────────────┐    subprocess    ┌─────────────────┐
│   React UI      │ ◄──────────────► │  Flask Server   │ ◄───────────────► │   Java Engine   │
│                 │                  │                 │                   │                 │
│ • Table View    │                  │ • REST APIs     │                   │ • Rule Parser   │
│ • Rule Editor   │                  │ • SQLite DB     │                   │ • Validation    │
│ • Syntax HL     │                  │ • Java Bridge   │                   │ • Code Gen      │
└─────────────────┘                  └─────────────────┘                   └─────────────────┘
```

## 🚀 Features

### ✅ Implemented
- **Rules Table View**: List, search, filter, and manage rules
- **Rule Editor**: Full-page editor with Monaco (VS Code editor)
- **Syntax Highlighting**: Custom language definition for Rules DSL
- **Auto-completion**: Context-aware suggestions for keywords, attributes, actions
- **Real-time Validation**: Syntax validation with error reporting
- **CRUD Operations**: Create, read, update, delete rules
- **Version History**: Track rule changes over time
- **SQLite Database**: Persistent storage for rules and history
- **REST API**: Complete API for rule management

### 🔄 Planned Enhancements
- **Rule Testing**: Execute rules against sample data
- **Advanced Validation**: Integration with full Java rules engine
- **User Authentication**: Multi-user support with permissions
- **Rule Templates**: Pre-built rule templates
- **Export/Import**: Rule backup and sharing
- **Advanced Search**: Full-text search across rule content

## 🛠️ Technology Stack

### Frontend
- **React 18** with functional components and hooks
- **Monaco Editor** for syntax highlighting and IntelliSense
- **Ant Design** for UI components
- **Axios** for HTTP requests

### Backend
- **Flask** with SQLAlchemy ORM
- **SQLite** database
- **CORS** support for React communication

### Integration
- **Java Bridge** for rule validation
- **Command-line interface** for Java communication
- **JSON** data exchange format

## 📋 Prerequisites

- **Python 3.8+**
- **Node.js 16+**
- **Java 11+**
- **npm** or **yarn**

## 🚀 Quick Start

### 1. Setup
```bash
# Clone and navigate to the project
cd ui-prototype

# Run setup script
chmod +x scripts/*.sh
./scripts/setup.sh
```

### 2. Start Development Servers
```bash
# Start both backend and frontend
./scripts/start-dev.sh
```

### 3. Access the Application
- **Frontend**: http://localhost:3000
- **Backend API**: http://localhost:5001
- **Health Check**: http://localhost:5001/api/health

## 📁 Project Structure

```
ui-prototype/
├── backend/                    # Flask backend
│   ├── app.py                 # Main Flask application
│   ├── models.py              # SQLAlchemy models
│   ├── api/                   # REST API endpoints
│   ├── services/              # Business logic
│   └── database/              # SQLite database
├── frontend/                  # React frontend
│   ├── src/
│   │   ├── components/        # React components
│   │   ├── services/          # API client
│   │   └── utils/             # Utilities
│   └── public/
├── java-bridge/               # Java integration
└── scripts/                   # Setup and utility scripts
```

## 🎯 Usage

### Creating Rules
1. Click "Create Rule" button
2. Fill in rule metadata (name, description, status)
3. Write rule content in the Monaco editor
4. Use auto-completion (Ctrl+Space) for suggestions
5. Validate syntax with the "Validate" button
6. Save the rule

### Editing Rules
1. Click the edit icon in the rules table
2. Modify rule content or metadata
3. Real-time validation shows syntax errors
4. Save changes to create a new version

### Rule Syntax Examples
```
rule creditScoreCheck:
    if applicant.creditScore >= 700 then approveApplication
    if applicant.creditScore < 600 then rejectApplication

rule businessDateValidation:
    if applicant.applicationDate after business_date then conditionalApproval
    if business_date + 30 days > today then manualReview

rule weekendCheck:
    if day_of_week(transaction.timestamp) >= 6 then manualReview
```

## 🔧 API Endpoints

### Rules Management
- `GET /api/rules` - List rules with pagination/filtering
- `GET /api/rules/{id}` - Get specific rule
- `POST /api/rules` - Create new rule
- `PUT /api/rules/{id}` - Update rule
- `DELETE /api/rules/{id}` - Delete rule

### Validation & Testing
- `POST /api/rules/validate` - Validate rule syntax
- `POST /api/rules/{id}/test` - Test rule execution
- `POST /api/rules/autocomplete` - Get completion suggestions

### History & Versioning
- `GET /api/rules/{id}/history` - Get rule history
- `POST /api/rules/{id}/revert/{version}` - Revert to version

## 🗄️ Database Schema

### Rules Table
- `id` - Primary key
- `name` - Unique rule name
- `description` - Rule description
- `content` - Rule DSL content
- `status` - draft/active/inactive/error
- `validation_status` - valid/invalid/pending
- `version` - Version number
- `created_at`, `updated_at` - Timestamps

### Rule History Table
- `id` - Primary key
- `rule_id` - Foreign key to rules
- `content` - Historical content
- `version` - Version number
- `change_reason` - Reason for change

## 🧪 Development

### Backend Development
```bash
cd backend
source venv/bin/activate
python app.py  # Runs on port 5001
```

### Frontend Development
```bash
cd frontend
npm start
```

### Adding New Features
1. **Backend**: Add API endpoints in `api/` directory
2. **Frontend**: Create components in `src/components/`
3. **Integration**: Update `services/api.js` for new endpoints

## 🚀 Production Deployment

### Backend
```bash
# Use production WSGI server
pip install gunicorn
gunicorn -w 4 -b 0.0.0.0:5001 app:app
```

### Frontend
```bash
# Build for production
npm run build
# Serve with nginx or similar
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make changes with tests
4. Submit a pull request

## 📝 License

This is a prototype for demonstration purposes.

---

## 🎯 Next Steps for Full Implementation

1. **Enhanced Java Integration**: Full rules engine integration
2. **User Authentication**: Login/logout with role-based access
3. **Rule Testing Interface**: Interactive test data input
4. **Performance Optimization**: Caching, pagination improvements
5. **Monitoring & Logging**: Application monitoring and audit logs
6. **Deployment**: Docker containers and CI/CD pipeline