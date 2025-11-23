#!/bin/bash

# Start Flask backend for VS Code extension on port 5002
# The main UI backend runs on 5001

echo "Starting Rules DSL Extension Backend..."
echo "Port: 5002"
echo "URL: http://localhost:5002"
echo ""
echo "Press Ctrl+C to stop"
echo ""

python app.py --extension
