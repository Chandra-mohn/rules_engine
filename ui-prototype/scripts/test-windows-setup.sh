#!/bin/bash

# Test script to verify Windows setup for Rules Engine UI

echo "🧪 Testing Rules Engine UI Setup on Windows..."
echo

# Test Java installation
echo "Testing Java..."
if java -version 2>&1 | head -1 | grep -q "17"; then
    echo "✅ Java 17.x detected"
else
    echo "❌ Java 17.x not found or wrong version"
    java -version
fi

# Test Maven installation  
echo "Testing Maven..."
if mvn --version 2>&1 | head -1 | grep -q "3.9"; then
    echo "✅ Maven 3.9.x detected"
else
    echo "❌ Maven 3.9.x not found or wrong version"
    mvn --version
fi

# Test Node.js installation
echo "Testing Node.js..."
if command -v node &> /dev/null; then
    NODE_VERSION=$(node --version)
    echo "✅ Node.js detected: $NODE_VERSION"
else
    echo "❌ Node.js not found"
fi

# Test Python installation
echo "Testing Python..."
if command -v python &> /dev/null; then
    PYTHON_VERSION=$(python --version)
    echo "✅ Python detected: $PYTHON_VERSION"
else
    echo "❌ Python not found"
fi

echo

# Test Java bridge build
echo "Testing Java bridge build..."
cd java-bridge
if [ -f "target/rules-engine-java-1.0.0.jar" ]; then
    echo "✅ Java bridge JAR found"
    
    # Test JAR execution
    if java -jar target/rules-engine-java-1.0.0.jar help &> /dev/null; then
        echo "✅ Java bridge JAR executable"
    else
        echo "❌ Java bridge JAR not executable"
    fi
else
    echo "⚠️  Java bridge JAR not found - trying to build..."
    mvn clean package -q
    if [ -f "target/rules-engine-java-1.0.0.jar" ]; then
        echo "✅ Java bridge built successfully"
    else
        echo "❌ Java bridge build failed"
    fi
fi
cd ..

# Test Python backend dependencies
echo "Testing Python backend..."
cd backend
if [ -d "venv" ]; then
    echo "✅ Python virtual environment found"
    source venv/Scripts/activate 2>/dev/null || source venv/bin/activate
    
    if python -c "import flask; import psycopg2" 2>/dev/null; then
        echo "✅ Python dependencies available"
    else
        echo "❌ Python dependencies missing"
    fi
else
    echo "⚠️  Python virtual environment not found"
fi
cd ..

# Test React frontend dependencies  
echo "Testing React frontend..."
cd frontend
if [ -d "node_modules" ]; then
    echo "✅ Node.js dependencies installed"
else
    echo "⚠️  Node.js dependencies not found - run: npm install"
fi
cd ..

echo
echo "🏁 Setup test completed!"
echo
echo "If all tests passed, you can start the application with:"
echo "  Terminal 1: ./scripts/start-backend.sh"
echo "  Terminal 2: ./scripts/start-frontend.sh"