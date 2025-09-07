#!/bin/bash

# Clean Maven build script that suppresses JVM warnings
# Usage: ./build.sh [clean|package|test]

COMMAND=${1:-package}

echo "🔨 Building Rules Engine Java Components..."

# Suppress JVM warnings for cleaner output
export MAVEN_OPTS="$MAVEN_OPTS -Djava.awt.headless=true"
export MAVEN_OPTS="$MAVEN_OPTS -XX:+TieredCompilation"
export MAVEN_OPTS="$MAVEN_OPTS -XX:TieredStopAtLevel=1"

# Run Maven with minimal output
mvn $COMMAND -q --batch-mode 2>&1 | \
    grep -v "sun.misc.Unsafe" | \
    grep -v "HiddenClassDefiner" | \
    grep -v "overlapping" || true

# Check if build was successful by looking for JAR file
if mvn $COMMAND -q --batch-mode > /dev/null 2>&1; then
    echo "✅ Build successful!"
    if [ -f "target/rules-engine-java-1.0.0.jar" ]; then
        echo "📦 JAR: $(ls -lh target/rules-engine-java-1.0.0.jar | awk '{print $5}')"
        echo "🚀 Test: java -jar target/rules-engine-java-1.0.0.jar help"
    fi
else
    echo "❌ Build failed!"
    exit 1
fi