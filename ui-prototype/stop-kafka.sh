#!/bin/bash

# Kafka Environment Shutdown Script

echo "🛑 Stopping Kafka environment..."

docker-compose -f docker-compose.kafka.yml down

echo "🧹 Cleaning up containers..."
docker container prune -f

echo "✅ Kafka environment stopped"