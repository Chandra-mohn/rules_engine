# Retired Components

This document tracks components that have been retired after consolidating to a single AST architecture.

## 2025-09-11: Retired Original Rules Engine

### What was retired:
- `/rules_engine/backend/` → moved to `/rules_engine/retired_backend/`

### Why it was retired:
The original standalone rules engine has been replaced by the consolidated architecture:

**Old Architecture:**
```
Frontend → Python Backend → Original Java Rules Engine (subprocess)
                         ↘ Java Bridge (separate, simplified)
```

**New Architecture:**
```
Frontend → Python Backend (CRUD/API) → Java Bridge (HTTP server with full AST)
```

### What the retired backend contained:
- Complete ANTLR-based rules engine implementation
- AST nodes for all grammar constructs
- Action registry and implementations
- Code generation framework
- Generated rule classes
- Comprehensive test suite

### Why consolidation was better:
1. **Single Grammar**: No more dual grammar maintenance
2. **Better Performance**: HTTP instead of subprocess calls  
3. **Unified AST**: Full AST support everywhere
4. **Simpler Architecture**: One rule engine, not two
5. **Easier Maintenance**: Single codebase for rule processing

### Current Active Components:
- ✅ `/ui-prototype/backend/` - Python Flask API + database
- ✅ `/ui-prototype/java-bridge/` - Java HTTP server with full AST  
- ✅ `/ui-prototype/frontend/` - React UI

The retired backend's code generation concepts will be reused in Phase 2 of the implementation plan.