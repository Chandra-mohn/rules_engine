#!/bin/bash

# Rules Authoring UI - Frontend Server Startup

echo "🌐 Starting React frontend server on port 3000..."

cd frontend

# Check if node_modules exists
if [ ! -d "node_modules" ]; then
    echo "❌ Node modules not found. Please run ./scripts/setup.sh first."
    exit 1
fi

# Start React development server
echo "🚀 Frontend server starting..."
npm start

echo "✅ Frontend server stopped."