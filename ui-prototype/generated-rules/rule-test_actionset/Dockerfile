# Dockerfile for Rules Engine Service
# This runs the main rules engine that loads library JARs, not individual rule microservices

FROM openjdk:17-jdk-slim

WORKDIR /app

# Copy the main rules engine service JAR
COPY target/rules-engine-service.jar app.jar

# Create directory for rule library JARs
RUN mkdir -p /app/rules

# Copy all rule library JARs
COPY rules/*.jar /app/rules/

# Expose application port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Run the rules engine service
ENTRYPOINT ["java", "-Drules.library.path=/app/rules", "-jar", "app.jar"]
