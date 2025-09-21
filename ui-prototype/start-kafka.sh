#!/bin/bash

# Kafka Environment Startup Script for Rules Engine Dual-Mode Testing

set -e

echo "🚀 Starting Kafka environment for Rules Engine testing..."

# Check if Docker is running
if ! docker info &>/dev/null; then
    echo "❌ Docker is not running. Please start Docker Desktop."
    exit 1
fi

# Start Kafka environment
echo "📦 Starting Kafka containers..."
docker-compose -f docker-compose.kafka.yml up -d

# Wait for services to be healthy
echo "⏳ Waiting for services to start..."
sleep 30

# Check if Kafka is ready
echo "🔍 Checking Kafka health..."
KAFKA_READY=false
for i in {1..30}; do
    if docker exec rules-engine-kafka kafka-topics --bootstrap-server localhost:9092 --list &>/dev/null; then
        KAFKA_READY=true
        break
    fi
    echo "   Waiting for Kafka... ($i/30)"
    sleep 2
done

if [ "$KAFKA_READY" = false ]; then
    echo "❌ Kafka failed to start properly"
    docker-compose -f docker-compose.kafka.yml logs kafka
    exit 1
fi

echo "✅ Kafka environment is ready!"
echo ""
echo "📊 Environment Details:"
echo "   • Kafka Broker: localhost:9092"
echo "   • Zookeeper: localhost:2181"
echo "   • Kafka UI: http://localhost:8080"
echo ""
echo "📋 Created Topics:"
docker exec rules-engine-kafka kafka-topics --bootstrap-server localhost:9092 --list

echo ""
echo "🎯 Ready for dual-mode testing:"
echo "   • Batch mode: Process files → rules execution"
echo "   • Streaming mode: Kafka messages → rules execution"
echo ""
echo "🛑 To stop: ./stop-kafka.sh"