#!/bin/bash

# Rules Authoring UI - Development Server Startup

echo "🚀 Starting Rules Authoring UI Development Servers..."

# Function to cleanup background processes
cleanup() {
    echo ""
    echo "🛑 Shutting down servers..."
    kill $BACKEND_PID $FRONTEND_PID 2>/dev/null
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

# Start backend server
echo "📡 Starting Flask backend server..."
cd backend
source venv/bin/activate
python app.py &
BACKEND_PID=$!

# Wait a moment for backend to start
sleep 3

# Start frontend server
echo "🌐 Starting React frontend server..."
cd ../frontend
npm start &
FRONTEND_PID=$!

echo ""
echo "✅ Development servers started!"
echo ""
echo "🌐 Frontend: http://localhost:3000"
echo "📡 Backend:  http://localhost:5001"
echo "📊 API Docs: http://localhost:5001/api/health"
echo ""
echo "Press Ctrl+C to stop all servers"

# Wait for background processes
wait