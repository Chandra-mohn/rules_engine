@echo off
REM Rules Authoring UI - Windows Setup Script for Git Bash

echo 🚀 Setting up Rules Authoring UI for Windows...

REM Check if Python 3 is installed
python --version >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo ❌ Python 3 is required but not installed.
    echo    Download from: https://www.python.org/downloads/
    exit /b 1
)

REM Check if Node.js is installed
node --version >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo ❌ Node.js is required but not installed.
    echo    Download from: https://nodejs.org/
    exit /b 1
)

REM Check if Java is installed
java -version >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo ❌ Java is required but not installed.
    echo    Download from: https://adoptium.net/
    exit /b 1
)

REM Check if Maven is installed
mvn --version >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo ❌ Maven is required but not installed.
    echo    Download from: https://maven.apache.org/download.cgi
    exit /b 1
)

echo ✅ Prerequisites check passed

REM Set up Python backend
echo 📦 Setting up Python backend...
cd backend

REM Create virtual environment if it doesn't exist
if not exist "venv" (
    python -m venv venv
)

REM Activate virtual environment and install dependencies
call venv\Scripts\activate.bat
pip install -r requirements.txt

echo ✅ Python backend setup complete

REM Set up React frontend
echo 📦 Setting up React frontend...
cd ..\frontend

REM Install Node.js dependencies
call npm install

echo ✅ React frontend setup complete

REM Compile Java bridge using Maven
echo 📦 Setting up Java bridge...
cd ..\java-bridge

REM Build using Maven
call mvn clean package -q

if %ERRORLEVEL% EQU 0 (
    echo ✅ Java bridge setup complete
) else (
    echo ❌ Java bridge build failed
    exit /b 1
)

REM Create database directory
echo 📦 Setting up database...
cd ..\backend
if not exist "database" mkdir database

echo ✅ Database directory created

echo.
echo 🎉 Setup complete!
echo.
echo To start the development servers in Git Bash:
echo   1. Backend (port 5001):  ./scripts/start-backend.sh
echo   2. Frontend (port 3000): ./scripts/start-frontend.sh
echo.
echo Or use the combined script:
echo   ./scripts/start-dev.sh
echo.
echo Access URLs:
echo   🌐 Frontend: http://localhost:3000
echo   📡 Backend:  http://localhost:5001
echo   📊 Health:   http://localhost:5001/api/health

pause