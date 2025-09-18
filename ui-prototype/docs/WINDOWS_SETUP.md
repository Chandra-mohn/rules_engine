# Windows Setup Guide for Rules Engine UI

This guide will help you set up and run the Rules Engine UI on Windows using Git Bash.

## Prerequisites

Before starting, ensure you have these installed:

### 1. Git Bash
- Download from: https://git-scm.com/download/win
- Choose "Git for Windows" installer

### 2. Java 17.x
- **Confirm installed**: Open Git Bash and run `java -version` and `javac -version`
- Should show Java 17.x (you mentioned you already have this)

### 3. Maven 3.9.x
- **Confirm installed**: Open Git Bash and run `mvn --version`  
- Should show Maven 3.9.x (you mentioned you already have this)

### 4. Node.js (16.x or higher)
- Download from: https://nodejs.org/
- Choose the LTS version
- **Verify**: `node --version` and `npm --version`

### 5. Python 3.8+
- Download from: https://www.python.org/downloads/
- **Important**: During installation, check "Add Python to PATH"
- **Verify**: `python --version` (should show 3.8+)

## Setup Instructions

### Step 1: Clone and Navigate
```bash
git clone <repository-url>
cd rules_engine/ui-prototype
```

### Step 2: Build Java Components
```bash
cd java-bridge
mvn clean package -q
cd ..
```

### Step 3: Setup Python Backend
```bash
cd backend
python -m venv venv
source venv/Scripts/activate  # Git Bash syntax
pip install -r requirements.txt
cd ..
```

### Step 4: Setup React Frontend
```bash
cd frontend
npm install
cd ..
```

### Step 5: Setup Database (SQLite)
```bash
cd backend
mkdir -p database
cd ..
```

## Running the Application

### Option 1: Using Git Bash Scripts (Recommended)

**Terminal 1 - Backend:**
```bash
./scripts/start-backend.sh
```

**Terminal 2 - Frontend:**
```bash
./scripts/start-frontend.sh
```

### Option 2: Manual Start

**Terminal 1 - Backend:**
```bash
cd backend
source venv/Scripts/activate
python app.py
```

**Terminal 2 - Frontend:**
```bash
cd frontend
npm start
```

## Access URLs

- **Frontend**: http://localhost:3000
- **Backend API**: http://localhost:5001
- **Health Check**: http://localhost:5001/api/health

## Windows-Specific Considerations

### File Paths
The application automatically handles Windows path conversions. The Java bridge service detects Windows and converts paths appropriately.

### Git Bash vs Command Prompt
- **Use Git Bash** for the shell scripts (`.sh` files)
- Scripts are designed to work in Git Bash environment
- Command Prompt can be used for individual commands but Git Bash is recommended

### Virtual Environment Activation
In Git Bash, use:
```bash
source venv/Scripts/activate
```

NOT:
```bash
venv\Scripts\activate.bat  # This is for Command Prompt only
```

### Maven and Java
Since you already have Java 17.x and Maven 3.9.x working in Git Bash, the build process should work seamlessly.

## Troubleshooting

### Java Bridge Issues
If you see Java validation errors:
```bash
cd java-bridge
mvn clean package -X  # Verbose build to see errors
```

### Python Virtual Environment
If activation fails:
```bash
cd backend
rm -rf venv  # Remove and recreate
python -m venv venv
source venv/Scripts/activate
```

### Port Conflicts
If ports 3000 or 5001 are in use:
- Backend: Edit `backend/config.py` to change port
- Frontend: Use `PORT=3001 npm start` to change frontend port

### Path Issues
If you encounter Windows path issues:
- Ensure you're using forward slashes `/` in Git Bash
- The application automatically converts paths for Java bridge

## Testing the Setup

1. **Health Check**: 
   ```bash
   curl http://localhost:5001/api/health
   ```

2. **Java Validation Test**:
   ```bash
   cd java-bridge
   java -jar target/rules-engine-java-1.0.0.jar help
   ```

3. **Frontend Access**: Open http://localhost:3000 in browser

## Development Tips

- Use Git Bash as your primary terminal for this project
- Keep both backend and frontend running in separate Git Bash windows
- The application supports hot reload for both frontend and backend changes
- Java components need to be rebuilt after grammar changes: `mvn clean package`

## File Structure for Windows
```
C:\your-path\rules_engine\ui-prototype\
├── backend\               # Python Flask API
├── frontend\              # React application  
├── java-bridge\           # Java validation engine
├── scripts\               # Shell scripts (use in Git Bash)
└── WINDOWS_SETUP.md       # This file
```

All scripts and paths have been tested to work correctly in the Git Bash environment on Windows.