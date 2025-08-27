#!/bin/bash

# Rules Authoring UI - Backend Server Startup

echo "📡 Starting Flask backend server on port 5001..."

cd backend

# Check if virtual environment exists
if [ ! -d "venv" ]; then
    echo "❌ Virtual environment not found. Please run ./scripts/setup.sh first."
    exit 1
fi

# Activate virtual environment
source venv/bin/activate

# Start Flask server
echo "🚀 Backend server starting..."
python app.py

echo "✅ Backend server stopped."