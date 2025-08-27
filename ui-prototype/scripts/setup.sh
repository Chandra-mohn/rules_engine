#!/bin/bash

# Rules Authoring UI - Setup Script

echo "🚀 Setting up Rules Authoring UI..."

# Check if Python 3 is installed
if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 is required but not installed."
    exit 1
fi

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is required but not installed."
    exit 1
fi

# Check if Java is installed
if ! command -v java &> /dev/null; then
    echo "❌ Java is required but not installed."
    exit 1
fi

echo "✅ Prerequisites check passed"

# Set up Python backend
echo "📦 Setting up Python backend..."
cd backend

# Create virtual environment if it doesn't exist
if [ ! -d "venv" ]; then
    python3 -m venv venv
fi

# Activate virtual environment
source venv/bin/activate

# Install Python dependencies
pip install -r requirements.txt

echo "✅ Python backend setup complete"

# Set up React frontend
echo "📦 Setting up React frontend..."
cd ../frontend

# Install Node.js dependencies
npm install

echo "✅ React frontend setup complete"

# Compile Java bridge
echo "📦 Setting up Java bridge..."
cd ../java-bridge

# Create classes directory
mkdir -p classes

# Compile Java validator
javac -d classes RuleValidator.java

echo "✅ Java bridge setup complete"

# Create database directory
echo "📦 Setting up database..."
cd ../backend
mkdir -p database

echo "✅ Database directory created"

echo ""
echo "🎉 Setup complete!"
echo ""
echo "To start the development servers:"
echo "  1. Backend (port 5001):  ./scripts/start-backend.sh"
echo "  2. Frontend (port 3000): ./scripts/start-frontend.sh"
echo ""
echo "Or use the combined script:"
echo "  ./scripts/start-dev.sh"
echo ""
echo "Access URLs:"
echo "  🌐 Frontend: http://localhost:3000"
echo "  📡 Backend:  http://localhost:5001"
echo "  📊 Health:   http://localhost:5001/api/health"