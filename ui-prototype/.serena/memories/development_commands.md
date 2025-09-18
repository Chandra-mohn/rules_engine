# Development Commands

## Setup Commands
```bash
chmod +x scripts/*.sh
./scripts/setup.sh          # Full project setup
./scripts/start-dev.sh       # Start both servers
```

## Backend (Flask)
```bash
cd backend
source venv/bin/activate     # or venv\Scripts\activate on Windows
pip install -r requirements.txt
python app.py               # Run on port 5001
```

## Frontend (React)
```bash
cd frontend
npm install
npm start                   # Run on port 3000
npm run build              # Production build
npm test                   # Run tests
```

## Java Bridge
```bash
cd java-bridge
mvn clean compile          # Compile
mvn package               # Build JAR
mvn test                  # Run tests
```

## System Commands (Darwin)
- `ls`, `cd`, `grep`, `find` - Standard Unix commands
- `git` - Version control
- Java 17+ and Maven required for Java bridge