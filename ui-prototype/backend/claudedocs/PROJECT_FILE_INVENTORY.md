# Project File Inventory Report
**Updated**: 2025-10-09

---

## Backend Python Application (Flask API Server)

### Core Application Files
- **app.py** - Flask application factory, database initialization, test data generation, and server startup (port 5001)
- **config.py** - Application configuration settings, database URL, and environment variables
- **models.py** - SQLAlchemy database models for rules, clients, process groups, process areas, and hierarchies
- **cli_commands.py** - Flask CLI commands for database seeding, clearing data, and administrative tasks
- **setup_database.py** - Database initialization script with schema creation and sample data loading

### API Endpoints (RESTful Routes)
- **api/__init__.py** - Blueprint registration and API package initialization
- **api/rules.py** - REST endpoints for CRUD operations on rules and ActionSets
- **api/context.py** - Context management endpoints for rule execution data
- **api/hierarchy.py** - Hierarchical organization endpoints (clients, process groups, process areas)
- **api/lists.py** - List data endpoints for dropdowns and reference data
- **api/schema.py** - Schema information endpoints for entity attributes and validation
- **api/java_files.py** - Generated Java code retrieval endpoints

### Business Logic Services
- **services/rule_service.py** - Core rule business logic, validation orchestration, and production artifact generation
- **services/python_rules_engine.py** - Python ANTLR parser integration, Java compilation, and rule execution engine
- **services/context_manager.py** - Rule execution context preparation and entity data management
- **services/batch_orchestrator.py** - Batch rule processing orchestration for multiple rule evaluation
- **services/parallel_batch_orchestrator.py** - Parallel batch processing with concurrent execution optimization
- **services/kafka_simulator.py** - Kafka message simulation for event-driven rule processing testing
- **services/parallel_kafka_simulator.py** - Parallel Kafka simulator with concurrent message processing
- **services/jar_packaging_system.py** - Java JAR artifact packaging with Maven integration
- **services/gap_analysis_service.py** - Gap analysis between expected and actual rule coverage
- **services/list_cache.py** - Caching layer for list data to reduce database queries

### Grammar Parser and Code Generation
- **grammar_parser/__init__.py** - Grammar parser package initialization
- **grammar_parser/rules_parser.py** - ANTLR parser wrapper for DSL rule parsing with Python integration
- **grammar_parser/template_code_generator.py** - Java code generation from parsed rules using template system
- **grammar_parser/rule_validator.py** - Rule syntax and semantic validation logic
- **grammar_parser/function_registry.py** - Built-in function registry for rule expressions (operators, comparisons)
- **grammar_parser/test_scenario_generator.py** - Automatic test scenario generation from rule definitions

### Java Code Templates
- **templates/java/standard_rule_template.py** - Python f-string templates for standard Java rule code generation (IF/THEN/ELSE, ActionSet, Direct Action)
- **templates/java/test_template.py** - JUnit 5 test class templates for generated rule testing

### Data Schema and Serialization
- **schema/__init__.py** - Schema package initialization
- **schema/rules_schema.py** - Marshmallow schemas for rule data serialization and validation

### Test Data and Fixtures
- **fixtures/__init__.py** - Fixtures package initialization
- **fixtures/demo_data.py** - Demo data generation functions for testing and development

### Validation and Monitoring
- **data_validator.py** - Data integrity validation across database schema
- **data_integrity_monitor.py** - Continuous data integrity monitoring utilities
- **validate_system.py** - System-wide validation and health checks
- **pre_commit_checks.py** - Pre-commit validation checks for code quality

### Utility Scripts
- **migrate_schema_actions.py** - Database migration script for schema and actions updates

### Database Files
- **create_database.sql** - SQL script for initial database creation
- **database_schema.sql** - Complete database schema definition
- **database_migration_hierarchy.sql** - Hierarchical schema migration script

### Configuration and Dependencies
- **requirements.txt** - Python package dependencies (Flask, SQLAlchemy, ANTLR runtime, etc.)

---

## Frontend React Application (Web UI)

### Main Application
- **src/index.js** - React application entry point with DOM rendering
- **src/App.jsx** - Main application component with routing and layout

### React Components
- **src/components/RuleEditor.jsx** - Monaco-based rule editor with syntax highlighting and validation
- **src/components/RulesListEnhanced.jsx** - Enhanced rule list view with filtering, sorting, and bulk operations
- **src/components/RulesTreeNavigation.jsx** - Hierarchical tree navigation for clients/process groups/areas
- **src/components/ActionEditor.jsx** - Specialized editor for ActionSet creation and editing
- **src/components/SampleDataEditor.jsx** - Sample test data editor for rule testing
- **src/components/SchemaViewer.jsx** - Entity schema viewer with attribute details
- **src/components/SchemaSelector.jsx** - Schema selection component for context configuration
- **src/components/GapAnalysis.jsx** - Gap analysis visualization component
- **src/components/CacheDebugger.jsx** - Cache debugging and inspection component

### Services (API Integration)
- **src/services/api.js** - Axios-based API client configuration with base URL and error handling
- **src/services/contextApi.js** - Context management API methods for entity data
- **src/services/suggestionCache.js** - Client-side caching for autocomplete suggestions

### Utilities
- **src/utils/rulesSyntax.js** - Monaco editor syntax highlighting and language definition for DSL

### Configuration
- **package.json** - NPM dependencies, scripts, and proxy configuration (port 3000 → 5001)
- **package-lock.json** - Locked dependency versions for reproducible builds
- **.env** - Environment variables for frontend configuration

---

## Java Bridge (ANTLR Parser and Rules Engine)

### ANTLR Grammar
- **src/main/antlr4/com/rules/grammar/Rules.g4** - ANTLR4 grammar definition for rules DSL syntax

### ANTLR Generated Python Files
- **src/main/antlr4/com/rules/grammar/RulesLexer.py** - ANTLR-generated Python lexer for tokenization (11KB)
- **src/main/antlr4/com/rules/grammar/RulesParser.py** - ANTLR-generated Python parser for parse tree construction (71KB)
- **src/main/antlr4/com/rules/grammar/RulesListener.py** - ANTLR-generated listener interface for parse tree traversal (7KB)
- **src/main/antlr4/com/rules/grammar/RulesVisitor.py** - ANTLR-generated visitor interface for parse tree analysis (4.5KB)

### Build Configuration
- **pom.xml** - Maven build configuration with ANTLR plugin, dependencies, and packaging
- **classpath.txt** - Java classpath for javac compilation (generated or manually created)
- **Rules_v1_backup.g4** - Backup of previous grammar version for reference
- **README.md** - Java bridge documentation and setup instructions

### Documentation
- **FINAL-COMPARISON.md** - Final comparison between grammar versions or implementations
- **json-comparison.md** - JSON output comparison documentation

---

## Maven Projects (Orchestration Libraries)

### Batch Orchestrator
- **batch-orchestrator/pom.xml** - Maven configuration for batch orchestration library
- **batch-orchestrator/dependency-reduced-pom.xml** - Shaded/reduced POM after Maven Shade plugin execution

### Streaming Orchestrator
- **streaming-orchestrator/pom.xml** - Maven configuration for streaming orchestration library
- **streaming-orchestrator/dependency-reduced-pom.xml** - Shaded/reduced POM after Maven Shade plugin execution

### Orchestration Library
- **orchestration-lib/pom.xml** - Maven configuration for core orchestration library

---

## Scripts and Automation

### Development Scripts
- **scripts/setup.sh** - Initial project setup and dependency installation
- **scripts/start-backend.sh** - Backend Flask server startup script
- **scripts/start-frontend.sh** - Frontend React development server startup script
- **scripts/start-dev.sh** - Combined backend + frontend startup for development

### Memory and Session Management
- **scripts/claude_memory_manager.py** - Claude session memory management utilities
- **scripts/memory_consolidation_agent.py** - Memory consolidation and optimization agent
- **scripts/integrated_memory_system.py** - Integrated memory system for session continuity
- **scripts/session_continuity_bridge.py** - Session state preservation between Claude conversations
- **scripts/run_memory_consolidation.py** - Script to execute memory consolidation process
- **scripts/save_context.sh** - Shell script to save current session context
- **scripts/restore_context.sh** - Shell script to restore previous session context

### Validation and Quality
- **scripts/code_quality_validator.py** - Code quality validation and linting automation
- **scripts/validation_agent.py** - Validation agent for system health checks
- **scripts/run_validation_demo.py** - Demonstration script for validation workflows

### Testing and Deployment
- **scripts/test-windows-setup.sh** - Windows environment setup testing script

### Other Utility Scripts
- **send-test-messages.sh** - Script to send test messages for rule execution
- **start-kafka.sh** - Kafka local instance startup script
- **stop-kafka.sh** - Kafka local instance shutdown script
- **cleanup_non_product_files.sh** - Script to remove non-production files (docs, tests, experimental)

---

## Testing Infrastructure

### Test Data
- **tests/data/test-data.json** - Test data samples for integration testing

### Test Frameworks
- **tests/integration_test_framework.py** - Integration testing framework with fixtures and utilities
- **tests/memory_system_tests.py** - Memory system functionality testing
- **tests/test_rule_name_parsing.py** - Rule name parsing validation testing

---

## Project-Level Test Files

- **test_consolidated_architecture.py** - Consolidated architecture integration testing
- **test_dsl_router_generation.py** - DSL router generation testing
- **test_phase3_consolidation.py** - Phase 3 consolidation milestone testing
- **test_python_rules_engine.py** - Python rules engine core functionality testing
- **test_rule_samples.py** - Sample rule validation and execution testing
- **test_unified_router_implementation.py** - Unified router implementation testing
- **debug_advanced_mode.py** - Advanced debugging mode utilities
- **simple_codegen_test.py** - Simple code generation validation script
- **final_codegen_test.py** - Final code generation integration test
- **fixed_codegen_test.py** - Fixed/corrected code generation test
- **performance_validation.py** - Performance validation and benchmarking script

### Test Results (JSON)
- **test_rules.json** - Test rule definitions for validation
- **comprehensive_codegen_test_report.json** - Comprehensive code generation test report
- **comprehensive_codegen_test_results.json** - Comprehensive code generation test results
- **final_codegen_test_results.json** - Final code generation test outcomes

---

## Documentation Files

### Project Documentation (Root)
- **README.md** - Main project documentation with setup instructions and architecture overview
- **CLAUDE.md** - Comprehensive architecture guide for Claude sessions and context preservation

### Architecture and Design
- **docs/project_structure.md** - Detailed project structure documentation
- **docs/ARCHITECTURE_CONSOLIDATION.md** - Architecture consolidation plan and decisions
- **docs/SOLUTION_DESIGN.md** - Solution design documentation with technical approach

### API and Schema Documentation
- **docs/api_design.md** - REST API design documentation with endpoints and contracts
- **docs/SCHEMA_CONFIGURATION.md** - Schema configuration and entity relationship documentation
- **docs/ATTRIBUTES_AND_ACTIONS_REFERENCE.md** - Reference documentation for available attributes and actions

### Implementation Guides
- **docs/ActionSetsSpecs.md** - ActionSet specifications and usage patterns
- **docs/ActionSets_Implementation_Plan.md** - ActionSet implementation roadmap
- **docs/ActionSets_Complex_Examples.md** - Complex ActionSet usage examples
- **docs/Grammar_Test_Suite.md** - Grammar validation test suite documentation
- **docs/QUOTED_IDENTIFIERS.md** - Quoted identifier handling in DSL

### Session and Memory Management
- **docs/CLAUDE_SESSION_CHECKPOINT.md** - Claude session checkpoint documentation
- **docs/CONTEXT_PRESERVATION_FRAMEWORK.md** - Framework for preserving context across sessions
- **docs/CONTEXT_QUICK_REFERENCE.md** - Quick reference guide for context management
- **docs/MEMORY_CONSOLIDATION_SYSTEM.md** - Memory consolidation system architecture
- **docs/VALIDATION_AGENT_GUIDE.md** - Validation agent usage guide
- **docs/USAGE_SUMMARY.md** - System usage summary and common workflows
- **docs/memory_report_20250917_141653.md** - Memory consolidation report from specific date

### Platform-Specific
- **docs/WINDOWS_SETUP.md** - Windows environment setup instructions

### Historical Documentation
- **docs/historical/GENERATOR_MIGRATION_PLAN.md** - Historical code generator migration plan

### Enhancement and Implementation Plans
- **ANTLR_ENHANCEMENT_PLAN.md** - ANTLR grammar enhancement roadmap
- **GRAMMAR_AND_CODE_GENERATION_ENHANCEMENTS.md** - Grammar and code generation improvement plan
- **IMPLEMENTATION_ROADMAP.md** - Overall implementation roadmap and milestones
- **IMPLEMENTATION_COMPLETE.md** - Implementation completion status and summary
- **SIMPLIFIED_GRAMMAR_PROPOSAL.md** - Proposal for simplified grammar syntax

### Code Generation Documentation
- **GENERATED_CODE_EXPLAINED.md** - Explanation of generated Java code structure
- **GENERATED_CODE_STRUCTURE.md** - Generated code organization and patterns
- **CODE_GENERATION_QUALITY_REPORT.md** - Code generation quality assessment report

### Backend-Specific Documentation
- **backend/CODE_GENERATION_ARCHITECTURE.md** - Code generation architecture documentation
- **backend/CODE_GENERATION_EXPLAINED.md** - Detailed code generation process explanation
- **backend/DATABASE_SETUP.md** - Database setup and initialization guide
- **backend/PARALLEL_ARCHITECTURE_GUIDE.md** - Parallel processing architecture guide
- **backend/PYTHON_ANTLR_IMPLEMENTATION.md** - Python ANTLR integration implementation details
- **backend/regression_prevention_guide.md** - Regression prevention best practices

### Build and Deployment
- **DEPLOYMENT_BUILD_GUIDE.md** - Production deployment and build instructions
- **PERFORMANCE_OPTIMIZATION_GUIDE.md** - Performance optimization strategies and techniques

### Features and Specifications
- **FIXTURE_SYSTEM_README.md** - Fixture system documentation for test data management
- **ACTIONSET_GRAMMAR_FIX.md** - ActionSet grammar fix documentation

---

## Reports and Analysis

- **reports/CODE_ANALYSIS_REPORT.md** - Code analysis report with metrics and recommendations
- **reports/code_quality_report_20250918_113347.json** - Code quality assessment JSON report

---

## Claude Configuration and Memory

### Claude Settings
- **.claude/settings.local.json** - Local Claude Code settings and preferences

### Session Snapshots
- **.context_snapshots/pre_compaction_*/*** - Pre-compaction context snapshots with system state, git status, database snapshots

### Memory System
- **.memory_consolidation/consolidated_memory.json** - Consolidated memory across sessions
- **.memory_consolidation/knowledge_gaps.json** - Identified knowledge gaps and missing context
- **.memory_consolidation/patterns_database.json** - Detected patterns and recurring themes
- **.memory_consolidation/session_learnings.json** - Learnings captured during sessions

### Serena MCP Memories
- **.serena/memories/project_overview.md** - High-level project overview memory
- **.serena/memories/development_commands.md** - Common development commands memory
- **.serena/memories/code_generation_enhancement_complete.md** - Code generation enhancement completion status
- **.serena/memories/template_generator_production_ready.md** - Template generator production readiness status

---

## Summary Statistics

**Total Files Documented**: ~180 source/config/documentation files
**Backend Python Files**: ~60 files
**Frontend JavaScript/JSX Files**: ~15 files
**Java/ANTLR Files**: ~5 files
**Test Files**: ~30 files
**Documentation Files**: ~35 files
**Configuration Files**: ~10 files
**Scripts**: ~15 files

**Excluded from Report**:
- node_modules/ (3,000+ library files)
- .venv/ and venv/ (1,500+ Python library files)
- generated-rules/ (dynamically generated Java classes)
- __pycache__/ (Python bytecode)
- build/ and dist/ (build artifacts)
- .git/ (version control internals)

---

**End of Report**
