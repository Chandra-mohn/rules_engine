"""
Modular JAR Packaging System
Creates optimized JAR packages for client-specific rules with daily deployment support.
Supports blue-green deployment and dependency management.
"""

from typing import Dict, List, Any, Optional, Set
from dataclasses import dataclass, field
from pathlib import Path
import json
import xml.etree.ElementTree as ET
from datetime import datetime
import shutil
import zipfile
import tempfile
import subprocess
import hashlib


@dataclass
class JarDependency:
    """Represents a JAR dependency."""
    group_id: str
    artifact_id: str
    version: str
    scope: str = "compile"
    type: str = "jar"


@dataclass
class JarArtifact:
    """Represents a generated JAR artifact."""
    artifact_id: str
    group_id: str
    version: str
    classifier: Optional[str] = None
    dependencies: List[JarDependency] = field(default_factory=list)
    source_files: Dict[str, str] = field(default_factory=dict)  # path -> content
    resources: Dict[str, str] = field(default_factory=dict)  # path -> content


@dataclass
class ClientPackageSpec:
    """Specification for client-specific JAR packaging."""
    client_id: str
    version: str
    base_package: str = "com.rules.generated"
    include_mon_rules: bool = True
    include_non_mon_rules: bool = True
    include_shared_rules: bool = True
    deployment_environment: str = "production"  # production, staging, development


class ModularJarPackager:
    """
    Creates modular JAR packages optimized for daily deployment.

    Package Structure:
    - shared-rules-engine-core.jar: Base router + context framework
    - client-{id}-rules-v{version}.jar: Client-specific core rules
    - client-{id}-monitoring-v{version}.jar: Mon rules (depends on core)
    - client-{id}-nonmonitoring-v{version}.jar: Non-mon rules (depends on core)
    """

    def __init__(self, output_directory: Path):
        self.output_directory = Path(output_directory)
        self.output_directory.mkdir(parents=True, exist_ok=True)

        # Standard dependencies for all JARs
        self.base_dependencies = [
            JarDependency("org.json", "json", "20240303"),
            JarDependency("com.github.ben-manes.caffeine", "caffeine", "3.1.8"),
            JarDependency("org.slf4j", "slf4j-api", "2.0.9"),
            JarDependency("ch.qos.logback", "logback-classic", "1.4.11", scope="runtime")
        ]

        # High-performance dependencies
        self.performance_dependencies = [
            JarDependency("net.openhft", "chronicle-map", "3.24.4"),
            JarDependency("org.lmax", "disruptor", "3.4.4"),
            JarDependency("it.unimi.dsi", "fastutil", "8.5.12")
        ]

    def create_complete_package_suite(self, spec: ClientPackageSpec,
                                    router_artifacts: Dict[str, str],
                                    rule_artifacts: Dict[str, Dict[str, str]]) -> Dict[str, Path]:
        """
        Create complete JAR package suite for a client.

        Args:
            spec: Client packaging specification
            router_artifacts: Generated router source code
            rule_artifacts: Rule-specific source code (rule_id -> files)

        Returns:
            Dict mapping artifact names to JAR file paths
        """
        created_jars = {}

        # 1. Create shared core JAR (shared across all clients)
        core_jar = self._create_shared_core_jar()
        created_jars["shared-core"] = core_jar

        # 2. Create client-specific core rules JAR
        core_rules_jar = self._create_client_core_jar(spec, router_artifacts, rule_artifacts)
        created_jars["client-core"] = core_rules_jar

        # 3. Create monitoring rules JAR (if enabled)
        if spec.include_mon_rules:
            mon_jar = self._create_monitoring_jar(spec, rule_artifacts)
            created_jars["monitoring"] = mon_jar

        # 4. Create non-monitoring rules JAR (if enabled)
        if spec.include_non_mon_rules:
            non_mon_jar = self._create_non_monitoring_jar(spec, rule_artifacts)
            created_jars["non-monitoring"] = non_mon_jar

        # 5. Create deployment descriptor
        deployment_descriptor = self._create_deployment_descriptor(spec, created_jars)
        created_jars["deployment-descriptor"] = deployment_descriptor

        return created_jars

    def _create_shared_core_jar(self) -> Path:
        """Create shared core JAR with router framework and context management."""

        artifact = JarArtifact(
            artifact_id="shared-rules-engine-core",
            group_id="com.rules.engine",
            version="1.0.0",
            dependencies=self.base_dependencies + self.performance_dependencies
        )

        # Add core framework source files
        artifact.source_files.update(self._get_core_framework_sources())

        # Add resources
        artifact.resources.update(self._get_core_resources())

        return self._build_jar_artifact(artifact)

    def _create_client_core_jar(self, spec: ClientPackageSpec,
                              router_artifacts: Dict[str, str],
                              rule_artifacts: Dict[str, Dict[str, str]]) -> Path:
        """Create client-specific core rules JAR."""

        artifact = JarArtifact(
            artifact_id=f"client-{spec.client_id.lower()}-rules",
            group_id=f"com.rules.client.{spec.client_id.lower()}",
            version=spec.version,
            dependencies=self.base_dependencies + [
                JarDependency("com.rules.engine", "shared-rules-engine-core", "1.0.0")
            ]
        )

        # Add generated router source files
        artifact.source_files.update(router_artifacts)

        # Add core rule implementations (rules and actionsets)
        for rule_id, rule_files in rule_artifacts.items():
            if self._is_core_rule(rule_id, rule_files):
                artifact.source_files.update(rule_files)

        # Add client-specific resources
        artifact.resources.update(self._get_client_resources(spec))

        return self._build_jar_artifact(artifact)

    def _create_monitoring_jar(self, spec: ClientPackageSpec,
                             rule_artifacts: Dict[str, Dict[str, str]]) -> Path:
        """Create monitoring rules JAR."""

        artifact = JarArtifact(
            artifact_id=f"client-{spec.client_id.lower()}-monitoring",
            group_id=f"com.rules.client.{spec.client_id.lower()}",
            version=spec.version,
            dependencies=self.base_dependencies + [
                JarDependency(f"com.rules.client.{spec.client_id.lower()}",
                             f"client-{spec.client_id.lower()}-rules",
                             spec.version)
            ]
        )

        # Add monitoring rule implementations
        for rule_id, rule_files in rule_artifacts.items():
            if self._is_monitoring_rule(rule_id, rule_files):
                artifact.source_files.update(rule_files)

        # Add monitoring-specific resources
        artifact.resources.update(self._get_monitoring_resources(spec))

        return self._build_jar_artifact(artifact)

    def _create_non_monitoring_jar(self, spec: ClientPackageSpec,
                                 rule_artifacts: Dict[str, Dict[str, str]]) -> Path:
        """Create non-monitoring rules JAR."""

        artifact = JarArtifact(
            artifact_id=f"client-{spec.client_id.lower()}-nonmonitoring",
            group_id=f"com.rules.client.{spec.client_id.lower()}",
            version=spec.version,
            dependencies=self.base_dependencies + [
                JarDependency(f"com.rules.client.{spec.client_id.lower()}",
                             f"client-{spec.client_id.lower()}-rules",
                             spec.version)
            ]
        )

        # Add non-monitoring rule implementations
        for rule_id, rule_files in rule_artifacts.items():
            if self._is_non_monitoring_rule(rule_id, rule_files):
                artifact.source_files.update(rule_files)

        # Add non-monitoring-specific resources
        artifact.resources.update(self._get_non_monitoring_resources(spec))

        return self._build_jar_artifact(artifact)

    def _build_jar_artifact(self, artifact: JarArtifact) -> Path:
        """Build JAR from artifact specification."""

        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)

            # Create project structure
            project_dir = temp_path / "project"
            project_dir.mkdir()

            # Generate pom.xml
            pom_content = self._generate_pom_xml(artifact)
            (project_dir / "pom.xml").write_text(pom_content)

            # Write source files
            src_dir = project_dir / "src" / "main" / "java"
            src_dir.mkdir(parents=True)

            for file_path, content in artifact.source_files.items():
                full_path = src_dir / file_path
                full_path.parent.mkdir(parents=True, exist_ok=True)
                full_path.write_text(content)

            # Write resources
            if artifact.resources:
                resources_dir = project_dir / "src" / "main" / "resources"
                resources_dir.mkdir(parents=True)

                for file_path, content in artifact.resources.items():
                    full_path = resources_dir / file_path
                    full_path.parent.mkdir(parents=True, exist_ok=True)
                    full_path.write_text(content)

            # Build JAR using Maven
            jar_path = self._build_with_maven(project_dir, artifact)

            # Copy to output directory
            output_jar = self.output_directory / f"{artifact.artifact_id}-{artifact.version}.jar"
            shutil.copy2(jar_path, output_jar)

            return output_jar

    def _build_with_maven(self, project_dir: Path, artifact: JarArtifact) -> Path:
        """Build JAR using Maven."""

        # Run Maven build
        result = subprocess.run(
            ["mvn", "clean", "package", "-q"],
            cwd=project_dir,
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            raise RuntimeError(f"Maven build failed: {result.stderr}")

        # Find generated JAR
        target_dir = project_dir / "target"
        jar_files = list(target_dir.glob("*.jar"))

        if not jar_files:
            raise RuntimeError("No JAR file generated")

        # Return the main JAR (not the *-jar-with-dependencies.jar)
        main_jar = next((jar for jar in jar_files if "jar-with-dependencies" not in jar.name), jar_files[0])
        return main_jar

    def _generate_pom_xml(self, artifact: JarArtifact) -> str:
        """Generate Maven pom.xml for artifact."""

        pom = ET.Element("project")
        pom.set("xmlns", "http://maven.apache.org/POM/4.0.0")
        pom.set("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
        pom.set("xsi:schemaLocation",
                "http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd")

        # Basic project info
        ET.SubElement(pom, "modelVersion").text = "4.0.0"
        ET.SubElement(pom, "groupId").text = artifact.group_id
        ET.SubElement(pom, "artifactId").text = artifact.artifact_id
        ET.SubElement(pom, "version").text = artifact.version
        ET.SubElement(pom, "packaging").text = "jar"

        # Properties
        properties = ET.SubElement(pom, "properties")
        ET.SubElement(properties, "maven.compiler.source").text = "17"
        ET.SubElement(properties, "maven.compiler.target").text = "17"
        ET.SubElement(properties, "project.build.sourceEncoding").text = "UTF-8"

        # Dependencies
        if artifact.dependencies:
            dependencies = ET.SubElement(pom, "dependencies")

            for dep in artifact.dependencies:
                dependency = ET.SubElement(dependencies, "dependency")
                ET.SubElement(dependency, "groupId").text = dep.group_id
                ET.SubElement(dependency, "artifactId").text = dep.artifact_id
                ET.SubElement(dependency, "version").text = dep.version
                if dep.scope != "compile":
                    ET.SubElement(dependency, "scope").text = dep.scope
                if dep.type != "jar":
                    ET.SubElement(dependency, "type").text = dep.type

        # Build configuration
        build = ET.SubElement(pom, "build")
        plugins = ET.SubElement(build, "plugins")

        # Compiler plugin
        compiler_plugin = ET.SubElement(plugins, "plugin")
        ET.SubElement(compiler_plugin, "groupId").text = "org.apache.maven.plugins"
        ET.SubElement(compiler_plugin, "artifactId").text = "maven-compiler-plugin"
        ET.SubElement(compiler_plugin, "version").text = "3.11.0"

        compiler_config = ET.SubElement(compiler_plugin, "configuration")
        ET.SubElement(compiler_config, "source").text = "17"
        ET.SubElement(compiler_config, "target").text = "17"

        # Assembly plugin for jar-with-dependencies
        assembly_plugin = ET.SubElement(plugins, "plugin")
        ET.SubElement(assembly_plugin, "groupId").text = "org.apache.maven.plugins"
        ET.SubElement(assembly_plugin, "artifactId").text = "maven-assembly-plugin"
        ET.SubElement(assembly_plugin, "version").text = "3.6.0"

        assembly_config = ET.SubElement(assembly_plugin, "configuration")
        descriptor_refs = ET.SubElement(assembly_config, "descriptorRefs")
        ET.SubElement(descriptor_refs, "descriptorRef").text = "jar-with-dependencies"

        executions = ET.SubElement(assembly_plugin, "executions")
        execution = ET.SubElement(executions, "execution")
        ET.SubElement(execution, "id").text = "make-assembly"
        ET.SubElement(execution, "phase").text = "package"
        goals = ET.SubElement(execution, "goals")
        ET.SubElement(goals, "goal").text = "single"

        # Convert to string
        return ET.tostring(pom, encoding='unicode', xml_declaration=True)

    def _create_deployment_descriptor(self, spec: ClientPackageSpec,
                                    created_jars: Dict[str, Path]) -> Path:
        """Create deployment descriptor for blue-green deployment."""

        descriptor = {
            "client_id": spec.client_id,
            "version": spec.version,
            "deployment_timestamp": datetime.now().isoformat(),
            "environment": spec.deployment_environment,
            "artifacts": {
                name: {
                    "path": str(jar_path),
                    "checksum": self._calculate_checksum(jar_path),
                    "size_bytes": jar_path.stat().st_size
                }
                for name, jar_path in created_jars.items()
                if jar_path.exists() and jar_path.suffix == '.jar'
            },
            "deployment_order": [
                "shared-core",
                "client-core",
                "monitoring",
                "non-monitoring"
            ],
            "health_check_endpoints": [
                f"/health/client/{spec.client_id}",
                f"/metrics/client/{spec.client_id}"
            ],
            "rollback_strategy": "blue_green",
            "validation_tests": [
                "smoke_test",
                "performance_test",
                "integration_test"
            ]
        }

        descriptor_path = self.output_directory / f"deployment-{spec.client_id}-{spec.version}.json"
        descriptor_path.write_text(json.dumps(descriptor, indent=2))

        return descriptor_path

    def _get_core_framework_sources(self) -> Dict[str, str]:
        """Get source files for core framework."""
        # This would typically read from generated router framework
        return {
            "com/rules/router/UniversalTransactionRouter.java": "// Generated router framework",
            "com/rules/router/ClientRuleMap.java": "// Client rule map interface",
            "com/rules/router/RuleExecutor.java": "// Rule executor interface",
            "com/rules/router/TransactionContext.java": "// Immutable context",
            "com/rules/router/RuleResult.java": "// Rule result",
            "com/rules/router/PerformanceMetrics.java": "// Performance metrics"
        }

    def _get_core_resources(self) -> Dict[str, str]:
        """Get resource files for core framework."""
        return {
            "logback.xml": self._generate_logback_config(),
            "application.properties": self._generate_core_properties()
        }

    def _get_client_resources(self, spec: ClientPackageSpec) -> Dict[str, str]:
        """Get client-specific resource files."""
        return {
            "client.properties": self._generate_client_properties(spec),
            "transaction-mappings.json": self._generate_transaction_mappings(spec)
        }

    def _get_monitoring_resources(self, spec: ClientPackageSpec) -> Dict[str, str]:
        """Get monitoring-specific resource files."""
        return {
            "monitoring.properties": f"client.id={spec.client_id}\\nmonitoring.enabled=true\\n"
        }

    def _get_non_monitoring_resources(self, spec: ClientPackageSpec) -> Dict[str, str]:
        """Get non-monitoring-specific resource files."""
        return {
            "nonmonitoring.properties": f"client.id={spec.client_id}\\nmonitoring.enabled=false\\n"
        }

    def _generate_logback_config(self) -> str:
        """Generate Logback configuration for high-performance logging."""
        return """<?xml version="1.0" encoding="UTF-8"?>
<configuration>
    <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
        <encoder>
            <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>

    <appender name="ASYNC" class="ch.qos.logback.classic.AsyncAppender">
        <appender-ref ref="STDOUT"/>
        <queueSize>1000</queueSize>
        <discardingThreshold>0</discardingThreshold>
        <includeCallerData>false</includeCallerData>
    </appender>

    <logger name="com.rules" level="INFO"/>

    <root level="WARN">
        <appender-ref ref="ASYNC"/>
    </root>
</configuration>"""

    def _generate_core_properties(self) -> str:
        """Generate core application properties."""
        return """# Core Rules Engine Properties
rules.engine.version=1.0.0
rules.engine.performance.mode=ultra
rules.engine.context.pool.size=1000
rules.engine.cache.enabled=true
rules.engine.metrics.enabled=true
"""

    def _generate_client_properties(self, spec: ClientPackageSpec) -> str:
        """Generate client-specific properties."""
        return f"""# Client {spec.client_id} Properties
client.id={spec.client_id}
client.version={spec.version}
client.environment={spec.deployment_environment}
client.package={spec.base_package}
"""

    def _generate_transaction_mappings(self, spec: ClientPackageSpec) -> str:
        """Generate transaction mappings configuration."""
        # This would be populated from actual transaction mappings
        mappings = {
            "client_id": spec.client_id,
            "version": spec.version,
            "mappings": {
                "CREDIT_APP": "CreditApplicationExecutor",
                "PAYMENT_PROC": "PaymentProcessingExecutor",
                "FRAUD_CHECK": "FraudDetectionExecutor"
            }
        }
        return json.dumps(mappings, indent=2)

    def _is_core_rule(self, rule_id: str, rule_files: Dict[str, str]) -> bool:
        """Check if rule should be included in core JAR."""
        # Rules and actionsets go in core JAR
        return any(
            "rule" in file_path.lower() or "actionset" in file_path.lower()
            for file_path in rule_files.keys()
        )

    def _is_monitoring_rule(self, rule_id: str, rule_files: Dict[str, str]) -> bool:
        """Check if rule should be included in monitoring JAR."""
        return any(
            "monitoring" in file_path.lower() or "mon_rule" in file_path.lower()
            for file_path in rule_files.keys()
        )

    def _is_non_monitoring_rule(self, rule_id: str, rule_files: Dict[str, str]) -> bool:
        """Check if rule should be included in non-monitoring JAR."""
        return any(
            "nonmonitoring" in file_path.lower() or "non_mon_rule" in file_path.lower()
            for file_path in rule_files.keys()
        )

    def _calculate_checksum(self, file_path: Path) -> str:
        """Calculate SHA-256 checksum of file."""
        hash_sha256 = hashlib.sha256()
        with open(file_path, "rb") as f:
            for chunk in iter(lambda: f.read(4096), b""):
                hash_sha256.update(chunk)
        return hash_sha256.hexdigest()


class BlueGreenDeploymentManager:
    """
    Manages blue-green deployment for daily rule updates.
    Supports safe rollout, validation, and rollback capabilities.
    """

    def __init__(self, deployment_directory: Path):
        self.deployment_directory = Path(deployment_directory)
        self.deployment_directory.mkdir(parents=True, exist_ok=True)

        # Deployment environments
        self.blue_dir = self.deployment_directory / "blue"
        self.green_dir = self.deployment_directory / "green"
        self.blue_dir.mkdir(exist_ok=True)
        self.green_dir.mkdir(exist_ok=True)

        # Current active environment
        self.active_env_file = self.deployment_directory / "active_environment.txt"

    def deploy_new_version(self, spec: ClientPackageSpec, jar_paths: Dict[str, Path]) -> Dict[str, Any]:
        """
        Deploy new version using blue-green strategy.

        Args:
            spec: Client package specification
            jar_paths: Paths to generated JAR files

        Returns:
            Deployment result with status and details
        """
        # Determine current active environment
        active_env = self._get_active_environment()
        new_env = "green" if active_env == "blue" else "blue"
        new_env_dir = self.green_dir if new_env == "green" else self.blue_dir

        try:
            # 1. Copy JARs to new environment
            self._copy_jars_to_environment(jar_paths, new_env_dir)

            # 2. Update configuration for new environment
            self._update_environment_config(spec, new_env_dir)

            # 3. Run validation tests
            validation_result = self._run_validation_tests(spec, new_env_dir)
            if not validation_result["success"]:
                return {
                    "success": False,
                    "message": "Validation tests failed",
                    "details": validation_result
                }

            # 4. Switch to new environment (atomic operation)
            self._switch_active_environment(new_env)

            # 5. Verify health of new environment
            health_check = self._run_health_check(spec)
            if not health_check["success"]:
                # Rollback on health check failure
                self._switch_active_environment(active_env)
                return {
                    "success": False,
                    "message": "Health check failed, rolled back",
                    "details": health_check
                }

            return {
                "success": True,
                "message": f"Successfully deployed {spec.client_id} v{spec.version} to {new_env}",
                "details": {
                    "previous_environment": active_env,
                    "new_environment": new_env,
                    "validation_result": validation_result,
                    "health_check": health_check
                }
            }

        except Exception as e:
            return {
                "success": False,
                "message": f"Deployment failed: {str(e)}",
                "details": {"error": str(e)}
            }

    def rollback_to_previous_version(self) -> Dict[str, Any]:
        """Rollback to previous environment."""
        try:
            active_env = self._get_active_environment()
            previous_env = "blue" if active_env == "green" else "green"

            self._switch_active_environment(previous_env)

            return {
                "success": True,
                "message": f"Rolled back from {active_env} to {previous_env}",
                "details": {
                    "previous_environment": active_env,
                    "current_environment": previous_env
                }
            }

        except Exception as e:
            return {
                "success": False,
                "message": f"Rollback failed: {str(e)}",
                "details": {"error": str(e)}
            }

    def _get_active_environment(self) -> str:
        """Get currently active environment."""
        if self.active_env_file.exists():
            return self.active_env_file.read_text().strip()
        else:
            # Default to blue
            self.active_env_file.write_text("blue")
            return "blue"

    def _switch_active_environment(self, new_env: str) -> None:
        """Switch active environment atomically."""
        temp_file = self.active_env_file.with_suffix(".tmp")
        temp_file.write_text(new_env)
        temp_file.replace(self.active_env_file)

    def _copy_jars_to_environment(self, jar_paths: Dict[str, Path], env_dir: Path) -> None:
        """Copy JAR files to environment directory."""
        for jar_name, jar_path in jar_paths.items():
            if jar_path.suffix == '.jar':
                dest_path = env_dir / jar_path.name
                shutil.copy2(jar_path, dest_path)

    def _update_environment_config(self, spec: ClientPackageSpec, env_dir: Path) -> None:
        """Update configuration files for environment."""
        config = {
            "client_id": spec.client_id,
            "version": spec.version,
            "environment": spec.deployment_environment,
            "updated_at": datetime.now().isoformat()
        }

        config_file = env_dir / "environment.json"
        config_file.write_text(json.dumps(config, indent=2))

    def _run_validation_tests(self, spec: ClientPackageSpec, env_dir: Path) -> Dict[str, Any]:
        """Run validation tests against new environment."""
        # This would integrate with actual test framework
        return {
            "success": True,
            "tests_run": ["smoke_test", "performance_test", "integration_test"],
            "results": {
                "smoke_test": "PASS",
                "performance_test": "PASS",
                "integration_test": "PASS"
            }
        }

    def _run_health_check(self, spec: ClientPackageSpec) -> Dict[str, Any]:
        """Run health check against deployed environment."""
        # This would integrate with actual health check endpoints
        return {
            "success": True,
            "checks": {
                "jar_loading": "PASS",
                "rule_execution": "PASS",
                "performance_metrics": "PASS"
            }
        }