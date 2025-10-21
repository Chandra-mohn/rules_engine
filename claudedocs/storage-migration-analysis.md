# Storage Migration Analysis: SQLite to File-Based JSON with DuckDB

**Date**: 2025-10-11
**Status**: Brainstorming & Analysis Phase
**Decision**: Pending Implementation

---

## Executive Summary

Analysis of migrating from SQLite database to file-based JSON storage with DuckDB query engine for business rules management system. The system generates Java code from business rules and requires effective version control and developer collaboration.

**Recommendation**: **Proceed with DuckDB + JSON files hybrid approach**

---

## Current System Context

### Business Rules System
- **Purpose**: Store business rules used to generate Java code for production deployment
- **Scale**: Thousands of rules across multiple groupings
- **Users**: Developer teams with individual rule ownership, leads require access to all rules
- **Workflow**: Rules → Java code generation → Build → Production deployment
- **User Interface**: Power users (not end users) edit rules through UI with assistance features

### Current Pain Points with SQLite
1. **Sharing Challenge**: Distributing new rules between developers is difficult with binary database files
2. **Version Control**: No native git integration for tracking rule changes
3. **Collaboration**: Manual export/import process for rule sharing
4. **Deployment Traceability**: Difficult to correlate rule versions with generated code versions

### Access Patterns
- **Modification Frequency**: Occasional updates, not real-time
- **Query Pattern**: Mostly CRUD operations with minimal filtering
- **Concurrency**: No simultaneous multi-user edits (each developer owns their rules)
- **Performance Requirement**: Filename-based lookup (equivalent to primary key access) acceptable
- **Transaction Needs**: Controlled through git commits, not database transactions

---

## Proposed Architecture: DuckDB + JSON Files Hybrid

### Core Concept
Combine the benefits of git-native file storage with SQL query capabilities:
- **Storage Layer**: Plain JSON files organized in folders
- **CRUD Operations**: Direct file system read/write
- **Complex Queries**: DuckDB for SQL-based filtering and analytics
- **Version Control**: Native git integration for all rule changes

### Architecture Diagram

```
┌─────────────────────────┐
│   UI (React/Web App)    │
│   - Rule editor         │
│   - Validation          │
│   - Preview             │
└───────────┬─────────────┘
            │
┌───────────▼─────────────────────────┐
│     Rule Service Layer              │
│  ┌──────────────┐  ┌──────────────┐│
│  │ CRUD Service │  │Query Service ││
│  │  (Direct     │  │  (DuckDB     ││
│  │   File I/O)  │  │   SQL)       ││
│  └──────┬───────┘  └──────┬───────┘│
│         │                  │        │
│  ┌──────▼──────────────────▼──────┐│
│  │   Git Integration Layer        ││
│  │   - Auto-commit (optional)     ││
│  │   - Conflict detection         ││
│  └────────────────────────────────┘│
└───────────┬─────────────────────────┘
            │
┌───────────▼─────────────────────────┐
│     File System Layer               │
│   rules/                            │
│   ├── {grouping_1}/                 │
│   │   ├── rule-name-1.json          │
│   │   └── rule-name-2.json          │
│   ├── {grouping_2}/                 │
│   │   └── rule-name-3.json          │
│   └── schema.json                   │
└─────────────────────────────────────┘
```

---

## Detailed Design

### 1. Storage Structure

**File Organization**:
```
rules/
├── {existing_grouping_1}/
│   ├── rule-name-1.json
│   ├── rule-name-2.json
│   └── rule-name-3.json
├── {existing_grouping_2}/
│   ├── rule-name-4.json
│   └── rule-name-5.json
├── {existing_grouping_3}/
│   └── rule-name-6.json
└── schema.json  # JSON schema for validation
```

**Design Decisions**:
- **Grouping Strategy**: Use existing business groupings as folder structure, extensible with nested folders
- **Naming Convention**: Rule name becomes filename (ensure uniqueness within grouping)
- **Loading Strategy**: Load on-demand (not all rules at startup)
- **No Metadata Index**: Direct file access is fast enough for thousands of rules

**Example Rule File** (`rules/entities/Order.json`):
```json
{
  "id": "order-rule-001",
  "name": "Order",
  "entity_type": "Order",
  "status": "active",
  "metadata": {
    "author": "developer@company.com",
    "created": "2025-01-15T10:30:00Z",
    "modified": "2025-02-20T14:45:00Z",
    "version": "1.2"
  },
  "validations": [
    {
      "field": "order_total",
      "type": "range",
      "min": 0,
      "max": 1000000
    }
  ],
  "references": [
    "entities/Customer",
    "validations/order-validation-001"
  ],
  "code_generation": {
    "template": "entity",
    "package": "com.company.orders",
    "class_name": "OrderEntity"
  }
}
```

### 2. CRUD Operations (Direct File Access)

**Service Implementation Pattern**:
```javascript
class RuleService {
  constructor(rulesBasePath = 'rules') {
    this.basePath = rulesBasePath;
  }

  // READ: Get rule by grouping and name
  async getRule(grouping, ruleName) {
    const path = `${this.basePath}/${grouping}/${ruleName}.json`;
    try {
      const content = await fs.readFile(path, 'utf8');
      return JSON.parse(content);
    } catch (error) {
      if (error.code === 'ENOENT') {
        throw new Error(`Rule not found: ${grouping}/${ruleName}`);
      }
      throw error;
    }
  }

  // CREATE/UPDATE: Save rule with validation
  async saveRule(grouping, ruleName, data) {
    // Validate against schema
    const valid = this.validateSchema(data);
    if (!valid) {
      throw new Error(`Schema validation failed: ${this.getValidationErrors()}`);
    }

    // Ensure directory exists
    const dir = `${this.basePath}/${grouping}`;
    await fs.mkdir(dir, { recursive: true });

    // Write file with pretty formatting (git-friendly)
    const path = `${dir}/${ruleName}.json`;
    await fs.writeFile(path, JSON.stringify(data, null, 2), 'utf8');

    return { success: true, path };
  }

  // DELETE: Remove rule file
  async deleteRule(grouping, ruleName) {
    const path = `${this.basePath}/${grouping}/${ruleName}.json`;
    try {
      await fs.unlink(path);
      return { success: true, path };
    } catch (error) {
      if (error.code === 'ENOENT') {
        throw new Error(`Rule not found: ${grouping}/${ruleName}`);
      }
      throw error;
    }
  }

  // LIST: Get all rules in a grouping
  async listRules(grouping) {
    const dir = `${this.basePath}/${grouping}`;
    const files = await fs.readdir(dir);
    return files
      .filter(f => f.endsWith('.json'))
      .map(f => f.replace('.json', ''));
  }

  // Schema validation helper
  validateSchema(data) {
    const Ajv = require('ajv');
    const ajv = new Ajv();
    const schema = this.loadSchema();
    const validate = ajv.compile(schema);
    return validate(data);
  }
}
```

**Performance Characteristics**:
- **Read by filename**: O(1) - Direct file access
- **List rules in grouping**: O(n) where n = files in directory (fast for reasonable directory sizes)
- **No connection pooling**: No database connection overhead
- **No query parsing**: Direct file I/O

### 3. Complex Queries (DuckDB)

**Query Service Implementation**:
```javascript
const duckdb = require('duckdb');

class RuleQueryService {
  constructor(rulesBasePath = 'rules') {
    this.basePath = rulesBasePath;
    this.db = new duckdb.Database(':memory:');
  }

  // Find rules with filtering
  async findRulesWithFilter(filters) {
    const { entityType, status, author } = filters;

    const query = `
      SELECT
        json_extract(json, '$.id') as id,
        json_extract(json, '$.name') as name,
        json_extract(json, '$.entity_type') as entity_type,
        json_extract(json, '$.status') as status,
        json_extract(json, '$.metadata.author') as author
      FROM read_json_auto('${this.basePath}/**/*.json')
      WHERE 1=1
        ${entityType ? "AND json_extract(json, '$.entity_type') = ?" : ""}
        ${status ? "AND json_extract(json, '$.status') = ?" : ""}
        ${author ? "AND json_extract(json, '$.metadata.author') = ?" : ""}
    `;

    const params = [entityType, status, author].filter(Boolean);
    return new Promise((resolve, reject) => {
      this.db.all(query, params, (err, rows) => {
        if (err) reject(err);
        else resolve(rows);
      });
    });
  }

  // Find rules referencing a specific entity
  async findRuleReferences(entityId) {
    const query = `
      SELECT
        json_extract(json, '$.id') as id,
        json_extract(json, '$.name') as name,
        json_extract(json, '$.references') as references
      FROM read_json_auto('${this.basePath}/**/*.json')
      WHERE json_extract(json, '$.references') LIKE '%' || ? || '%'
    `;

    return new Promise((resolve, reject) => {
      this.db.all(query, [entityId], (err, rows) => {
        if (err) reject(err);
        else resolve(rows);
      });
    });
  }

  // Lead dashboard: Rules by developer
  async getRulesByDeveloper() {
    const query = `
      SELECT
        json_extract(json, '$.metadata.author') as developer,
        json_extract(json, '$.entity_type') as entity_type,
        COUNT(*) as rule_count
      FROM read_json_auto('${this.basePath}/**/*.json')
      GROUP BY developer, entity_type
      ORDER BY rule_count DESC
    `;

    return new Promise((resolve, reject) => {
      this.db.all(query, [], (err, rows) => {
        if (err) reject(err);
        else resolve(rows);
      });
    });
  }

  // Analytics: Rule distribution by status
  async getRuleStatusDistribution() {
    const query = `
      SELECT
        json_extract(json, '$.status') as status,
        COUNT(*) as count
      FROM read_json_auto('${this.basePath}/**/*.json')
      GROUP BY status
      ORDER BY count DESC
    `;

    return new Promise((resolve, reject) => {
      this.db.all(query, [], (err, rows) => {
        if (err) reject(err);
        else resolve(rows);
      });
    });
  }
}
```

**When to Use DuckDB**:
- ✅ Lead dashboard queries (aggregate views across all developers)
- ✅ Search functionality (find rules by multiple criteria)
- ✅ Analytics and reporting (rule distribution, usage patterns)
- ✅ Referential integrity checks (find all rules referencing X)
- ❌ Individual CRUD operations (use direct file access instead)

**DuckDB Performance**:
- **Zero-copy querying**: Reads JSON files directly from disk
- **Optimized for analytics**: Fast aggregation and filtering
- **No import needed**: No ETL process required
- **In-memory execution**: Very fast for thousands of records

### 4. Git Integration

**Git Workflow Options**:

**Option A: Manual Git Workflow** (Recommended for initial implementation)
```javascript
// UI saves files, developer commits manually
async function saveRuleManual(grouping, ruleName, data) {
  await ruleService.saveRule(grouping, ruleName, data);

  // Developer manually runs:
  // git add rules/{grouping}/{ruleName}.json
  // git commit -m "Update rule: {ruleName}"
  // git push
}
```

**Option B: Auto-Commit on Save**
```javascript
const { exec } = require('child_process');
const util = require('util');
const execPromise = util.promisify(exec);

async function saveRuleWithAutoCommit(grouping, ruleName, data) {
  // Save file
  const { path } = await ruleService.saveRule(grouping, ruleName, data);

  // Git operations
  try {
    await execPromise(`git add ${path}`);
    await execPromise(`git commit -m "Update rule: ${ruleName} by ${data.metadata.author}"`);

    // Optional: auto-push (requires configuration)
    // await execPromise('git push');

    return { success: true, committed: true };
  } catch (error) {
    return { success: true, committed: false, error: error.message };
  }
}
```

**Option C: Batch Commit** (Multiple saves, single commit)
```javascript
class GitBatchService {
  constructor() {
    this.stagedFiles = new Set();
  }

  async stageFile(path) {
    this.stagedFiles.add(path);
  }

  async commitAll(message) {
    if (this.stagedFiles.size === 0) {
      throw new Error('No files staged for commit');
    }

    const files = Array.from(this.stagedFiles).join(' ');
    await execPromise(`git add ${files}`);
    await execPromise(`git commit -m "${message}"`);

    this.stagedFiles.clear();
    return { success: true, filesCommitted: files };
  }
}
```

**Conflict Detection and Resolution**:
```javascript
async function detectConflicts(grouping, ruleName) {
  const path = `rules/${grouping}/${ruleName}.json`;

  try {
    // Check if file has been modified remotely
    const { stdout } = await execPromise('git fetch && git status --porcelain');

    if (stdout.includes(path)) {
      return {
        hasConflict: true,
        message: 'File modified remotely. Pull changes before editing.'
      };
    }

    return { hasConflict: false };
  } catch (error) {
    return { hasConflict: false, error: error.message };
  }
}

// UI workflow with conflict check
async function saveRuleWithConflictCheck(grouping, ruleName, data) {
  // 1. Check for conflicts
  const conflict = await detectConflicts(grouping, ruleName);
  if (conflict.hasConflict) {
    throw new Error(conflict.message);
  }

  // 2. Save file
  await ruleService.saveRule(grouping, ruleName, data);

  // 3. Commit (optional)
  // await gitService.commit(path, message);
}
```

### 5. Referential Integrity

**Challenge**: JSON files don't have foreign key constraints like SQLite.

**Solution: Multi-Layer Validation**:

```javascript
class ReferentialIntegrityService {
  constructor(ruleService, queryService) {
    this.ruleService = ruleService;
    this.queryService = queryService;
  }

  // Validate references exist
  async validateReferences(rule) {
    const errors = [];

    for (const ref of rule.references || []) {
      const [grouping, ruleName] = ref.split('/');

      try {
        await this.ruleService.getRule(grouping, ruleName);
      } catch (error) {
        errors.push(`Referenced rule not found: ${ref}`);
      }
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }

  // Find orphaned references (rules referencing deleted entities)
  async findOrphanedReferences() {
    const query = `
      SELECT
        json_extract(json, '$.id') as id,
        json_extract(json, '$.name') as name,
        json_extract(json, '$.references') as references
      FROM read_json_auto('rules/**/*.json')
    `;

    // Check each reference exists
    // Return list of broken references
  }

  // Check impact before deletion
  async checkDeleteImpact(grouping, ruleName) {
    const ruleId = `${grouping}/${ruleName}`;
    const referencingRules = await this.queryService.findRuleReferences(ruleId);

    if (referencingRules.length > 0) {
      return {
        canDelete: false,
        message: `Cannot delete: ${referencingRules.length} rules reference this rule`,
        referencingRules
      };
    }

    return { canDelete: true };
  }
}
```

**Validation Strategy**:
1. **UI Validation**: Check references before saving
2. **Build-Time Validation**: Script validates all references before code generation
3. **Git Pre-Commit Hook**: Block commits with broken references

**Git Pre-Commit Hook Example** (`.git/hooks/pre-commit`):
```bash
#!/bin/bash

# Validate all rule references before commit
node scripts/validate-rules.js

if [ $? -ne 0 ]; then
  echo "❌ Rule validation failed. Commit blocked."
  exit 1
fi

echo "✅ All rules validated successfully."
exit 0
```

### 6. File Watching for Real-Time Updates

**Use Case**: Multiple developers working simultaneously, need to see changes without restarting UI.

```javascript
const chokidar = require('chokidar');

class RuleWatchService {
  constructor(rulesBasePath = 'rules') {
    this.basePath = rulesBasePath;
    this.listeners = new Map();
    this.watcher = null;
  }

  start() {
    this.watcher = chokidar.watch(`${this.basePath}/**/*.json`, {
      persistent: true,
      ignoreInitial: true
    });

    this.watcher
      .on('add', path => this.notifyChange('add', path))
      .on('change', path => this.notifyChange('change', path))
      .on('unlink', path => this.notifyChange('delete', path));
  }

  stop() {
    if (this.watcher) {
      this.watcher.close();
    }
  }

  // Subscribe to rule changes
  subscribe(grouping, ruleName, callback) {
    const key = `${grouping}/${ruleName}`;
    if (!this.listeners.has(key)) {
      this.listeners.set(key, []);
    }
    this.listeners.get(key).push(callback);
  }

  notifyChange(event, filePath) {
    // Parse grouping and rule name from path
    const match = filePath.match(/rules\/([^/]+)\/([^/]+)\.json/);
    if (!match) return;

    const [, grouping, ruleName] = match;
    const key = `${grouping}/${ruleName}`;

    // Notify all subscribers
    const callbacks = this.listeners.get(key) || [];
    callbacks.forEach(cb => cb({ event, grouping, ruleName, filePath }));
  }
}

// Usage in UI
const watchService = new RuleWatchService();
watchService.start();

// Subscribe to specific rule changes
watchService.subscribe('entities', 'Order', ({ event }) => {
  if (event === 'change') {
    console.log('Order rule changed externally, reloading...');
    // Reload UI or show notification
  }
});
```

---

## Benefits Analysis

### ✅ Advantages

| Benefit | Description | Impact |
|---------|-------------|--------|
| **Git-Native Collaboration** | Rules are plain text files → full git benefits | **HIGH** - Solves primary pain point |
| **Version History** | Every change tracked with author, timestamp, message | **HIGH** - Full audit trail |
| **Human-Readable Diffs** | `git diff` shows exactly what changed | **HIGH** - Easy code review |
| **Conflict Resolution** | Standard git merge tools work perfectly | **MEDIUM** - Familiar workflow |
| **Zero Database Ops** | No backups, migrations, connection pooling | **MEDIUM** - Reduced complexity |
| **Portability** | `git clone` = full rule set ready | **MEDIUM** - Easy onboarding |
| **SQL Query Power** | DuckDB provides SQL when needed | **MEDIUM** - Flexible querying |
| **Developer Autonomy** | Each dev commits their own rules | **MEDIUM** - Clear ownership |
| **Deployment Traceability** | Rules and generated code versions linked | **HIGH** - Production confidence |

### ⚠️ Challenges & Mitigations

| Challenge | Mitigation Strategy | Complexity |
|-----------|-------------------|------------|
| **Concurrent Edits** | Git conflict resolution + UI conflict detection | **LOW** - Standard git workflow |
| **Referential Integrity** | UI validation + build-time checks + pre-commit hooks | **MEDIUM** - Multi-layer validation |
| **Complex Queries** | DuckDB provides SQL layer for analytics | **LOW** - DuckDB handles this |
| **File System Performance** | Filename-based lookup + optional caching | **LOW** - Already fast enough |
| **Schema Evolution** | JSON schema versioning + migration scripts | **MEDIUM** - Standard JSON practice |
| **Large Rule Sets** | DuckDB optimized for thousands of records | **LOW** - Non-issue at this scale |

---

## Comparison: SQLite vs. File-Based vs. Hybrid

| Aspect | SQLite (Current) | Plain JSON Files | DuckDB + JSON (Proposed) |
|--------|------------------|------------------|--------------------------|
| **Git Integration** | ❌ Binary file | ✅ Perfect | ✅ Perfect |
| **Version Control** | ❌ Difficult | ✅ Native | ✅ Native |
| **Human-Readable Diffs** | ❌ No | ✅ Yes | ✅ Yes |
| **SQL Querying** | ✅ Full SQL | ❌ Manual parsing | ✅ Full SQL (DuckDB) |
| **Performance (CRUD)** | ✅ Very Fast | ✅ Fast (direct I/O) | ✅ Fast (direct I/O) |
| **Performance (Analytics)** | ✅ Fast | ❌ Slow (load all) | ✅ Very Fast (DuckDB) |
| **Referential Integrity** | ✅ Foreign keys | ❌ Manual | ⚠️ Validation layer |
| **Transaction Support** | ✅ ACID | ❌ None | ⚠️ Git-based |
| **Concurrent Writes** | ✅ Yes | ❌ File locks | ⚠️ Git conflicts |
| **Operational Overhead** | ⚠️ Medium | ✅ Zero | ✅ Zero |
| **Developer Sharing** | ❌ Difficult | ✅ Easy | ✅ Easy |
| **Deployment Traceability** | ❌ Poor | ✅ Excellent | ✅ Excellent |
| **Schema Validation** | ✅ Built-in | ⚠️ Manual | ⚠️ Manual |

**Winner**: **DuckDB + JSON Hybrid** - Best of both worlds

---

## Migration Strategy

### Phase 1: Proof of Concept (1-2 weeks)

**Goals**:
- Validate file-based storage performance
- Test DuckDB query capabilities
- Verify git workflow with small team

**Tasks**:
1. Export subset of SQLite rules to JSON files (100-200 rules)
2. Implement basic CRUD service with direct file I/O
3. Implement query service with DuckDB
4. Build simple UI integration
5. Test git workflow (commit, push, pull, merge)
6. Performance benchmarking

**Success Criteria**:
- CRUD operations < 100ms for single rule
- DuckDB queries < 500ms for 1000 rules
- Git workflow works seamlessly for 3-5 developers
- No data loss or corruption

### Phase 2: Parallel System (2-3 weeks)

**Goals**:
- Dual-write to both SQLite and files
- Build confidence in file-based system
- Train team on new workflow

**Tasks**:
1. Implement dual-write layer (saves to both SQLite and files)
2. Add validation: compare SQLite vs. files after each operation
3. Migrate all rules from SQLite to files
4. Run both systems in parallel for 2 weeks
5. Train developers on git workflow for rules

**Success Criteria**:
- 100% parity between SQLite and files
- No discrepancies detected
- Team comfortable with git workflow

### Phase 3: Cutover (1 week)

**Goals**:
- Switch to file-based as primary
- Remove SQLite dependency
- Full production readiness

**Tasks**:
1. Disable SQLite writes (read-only fallback)
2. Switch UI to file-based backend
3. Remove dual-write layer after 1 week of stability
4. Remove SQLite dependency completely
5. Document new workflow for team

**Success Criteria**:
- All operations use file-based system
- SQLite completely removed
- Zero production issues

### Phase 4: Optimization (Ongoing)

**Goals**:
- Refine performance
- Add advanced features
- Improve developer experience

**Tasks**:
1. Implement caching layer if needed
2. Add advanced git integration (auto-push, branch management)
3. Build lead dashboard with DuckDB analytics
4. Add file watching for real-time updates
5. Implement pre-commit validation hooks

---

## Technical Specifications

### Technology Stack

| Component | Technology | Version | Purpose |
|-----------|-----------|---------|---------|
| **Storage** | File System | Native | JSON file storage |
| **Query Engine** | DuckDB | 0.9+ | SQL querying of JSON files |
| **Validation** | JSON Schema (Ajv) | 8.0+ | Schema validation |
| **File Watching** | Chokidar | 3.5+ | Real-time file change detection |
| **Git Integration** | child_process (exec) | Native Node.js | Git operations |
| **UI Backend** | Express/Fastify | Current | API layer (unchanged) |

### Dependencies

```json
{
  "dependencies": {
    "duckdb": "^0.9.2",
    "ajv": "^8.12.0",
    "chokidar": "^3.5.3"
  }
}
```

**Size Impact**:
- DuckDB: ~15MB native module
- Ajv: ~500KB
- Chokidar: ~2MB
- **Total**: ~17.5MB additional dependencies

### System Requirements

**Server Requirements**:
- Node.js 16+ (for native module support)
- 512MB RAM minimum (in-memory DuckDB queries)
- File system with good I/O performance (SSD recommended)

**Developer Requirements**:
- Git 2.0+ installed
- Basic git knowledge (commit, push, pull, merge)
- Text editor with JSON support (for manual edits, optional)

---

## Risk Assessment

### High Priority Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **Data Loss During Migration** | Low | Critical | Phase 2 dual-write validation |
| **Performance Degradation** | Low | High | Phase 1 benchmarking validates performance |
| **Git Merge Conflicts** | Medium | Medium | Developer coordination + conflict detection |
| **Broken References After Deletion** | Medium | Medium | Multi-layer validation + pre-commit hooks |

### Medium Priority Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **Schema Drift** | Medium | Medium | JSON schema validation + versioning |
| **File System Limits** | Low | Medium | Folder structure prevents too many files per directory |
| **DuckDB Native Module Issues** | Low | Medium | Fallback to manual JSON parsing if needed |
| **Developer Resistance to Git** | Low | Low | Training + documentation + simple workflow |

### Low Priority Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **File Watching Performance** | Low | Low | Disable if not needed |
| **Git Repository Size Growth** | Low | Low | Git LFS if needed (unlikely for JSON) |

---

## Open Questions for Future Discussion

### 1. Git Workflow Details
- **Question**: Should UI auto-commit on save, or require manual commits?
- **Options**:
  - Auto-commit (convenience, but more git noise)
  - Manual commit (developer control, but extra step)
  - Batch commit (multiple saves, one commit)
- **Decision**: TBD

### 2. Lead Access Pattern
- **Question**: Should leads have read-only or full edit access?
- **Considerations**:
  - Read-only: Safer, but requires developer involvement for changes
  - Full edit: More flexible, but needs coordination to avoid conflicts
- **Decision**: TBD

### 3. Conflict Resolution Strategy
- **Question**: How aggressive should conflict detection be?
- **Options**:
  - Always pull before edit (safe, but slower)
  - Optimistic (fast, but more conflicts)
  - Lock-based (prevents conflicts, but complex)
- **Decision**: TBD

### 4. Backup and Recovery
- **Question**: Do we need additional backup beyond git?
- **Considerations**:
  - Git is backup (distributed, history preserved)
  - Optional: Periodic snapshots to separate location
  - Optional: CI/CD backup of main branch
- **Decision**: TBD

### 5. Schema Evolution Strategy
- **Question**: How do we handle schema changes over time?
- **Options**:
  - Versioned schemas with migration scripts
  - Backward-compatible changes only
  - Automated migration on rule load
- **Decision**: TBD

---

## Next Steps

### Immediate Actions (Before Implementation)
1. **Review this document** with technical team and stakeholders
2. **Answer open questions** listed above
3. **Identify pilot group** for Phase 1 proof of concept (3-5 developers)
4. **Select subset of rules** for initial migration (100-200 rules)
5. **Schedule kickoff meeting** for proof of concept phase

### Phase 1 Preparation (1 week before start)
1. **Set up development environment** with DuckDB and dependencies
2. **Export sample rules** from SQLite to JSON format
3. **Create git repository** for rules storage
4. **Document git workflow** for pilot group
5. **Prepare rollback plan** in case of issues

### Success Metrics to Track
- **Performance**: CRUD operation latency, query response times
- **Reliability**: Data consistency checks, validation pass rate
- **Developer Experience**: Time to share rules, merge conflict frequency
- **System Health**: Error rates, file system usage

---

## References and Resources

### DuckDB Documentation
- Official Docs: https://duckdb.org/docs/
- JSON Functions: https://duckdb.org/docs/extensions/json
- read_json_auto: https://duckdb.org/docs/data/json/overview

### JSON Schema
- JSON Schema Spec: https://json-schema.org/
- Ajv Documentation: https://ajv.js.org/

### Git Best Practices
- Git Branching: https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging
- Resolving Conflicts: https://git-scm.com/book/en/v2/Git-Tools-Advanced-Merging

---

## Document History

| Date | Version | Author | Changes |
|------|---------|--------|---------|
| 2025-10-11 | 1.0 | Brainstorming Session | Initial analysis and architecture design |

---

## Appendix: Example Code Snippets

### Complete CRUD + Query Service Example

```javascript
// rules-service.js
const fs = require('fs').promises;
const path = require('path');
const duckdb = require('duckdb');
const Ajv = require('ajv');

class RulesService {
  constructor(config = {}) {
    this.basePath = config.basePath || 'rules';
    this.schemaPath = config.schemaPath || 'rules/schema.json';
    this.db = new duckdb.Database(':memory:');
    this.ajv = new Ajv();
    this.schema = null;
  }

  async initialize() {
    // Load schema for validation
    const schemaContent = await fs.readFile(this.schemaPath, 'utf8');
    this.schema = JSON.parse(schemaContent);
    this.validate = this.ajv.compile(this.schema);
  }

  // ========== CRUD Operations ==========

  async getRule(grouping, ruleName) {
    const filePath = path.join(this.basePath, grouping, `${ruleName}.json`);
    const content = await fs.readFile(filePath, 'utf8');
    return JSON.parse(content);
  }

  async saveRule(grouping, ruleName, data) {
    // Validate schema
    if (!this.validate(data)) {
      throw new Error(`Validation failed: ${JSON.stringify(this.validate.errors)}`);
    }

    // Ensure directory exists
    const dir = path.join(this.basePath, grouping);
    await fs.mkdir(dir, { recursive: true });

    // Write file
    const filePath = path.join(dir, `${ruleName}.json`);
    await fs.writeFile(filePath, JSON.stringify(data, null, 2), 'utf8');

    return { success: true, path: filePath };
  }

  async deleteRule(grouping, ruleName) {
    const filePath = path.join(this.basePath, grouping, `${ruleName}.json`);
    await fs.unlink(filePath);
    return { success: true, path: filePath };
  }

  async listRules(grouping) {
    const dir = path.join(this.basePath, grouping);
    const files = await fs.readdir(dir);
    return files.filter(f => f.endsWith('.json')).map(f => f.replace('.json', ''));
  }

  // ========== Query Operations (DuckDB) ==========

  async queryRules(filters = {}) {
    const { entityType, status, author } = filters;

    let whereClause = [];
    let params = [];

    if (entityType) {
      whereClause.push("json_extract(json, '$.entity_type') = ?");
      params.push(entityType);
    }
    if (status) {
      whereClause.push("json_extract(json, '$.status') = ?");
      params.push(status);
    }
    if (author) {
      whereClause.push("json_extract(json, '$.metadata.author') = ?");
      params.push(author);
    }

    const where = whereClause.length > 0 ? `WHERE ${whereClause.join(' AND ')}` : '';

    const query = `
      SELECT
        json_extract(json, '$.id') as id,
        json_extract(json, '$.name') as name,
        json_extract(json, '$.entity_type') as entity_type,
        json_extract(json, '$.status') as status
      FROM read_json_auto('${this.basePath}/**/*.json')
      ${where}
    `;

    return new Promise((resolve, reject) => {
      this.db.all(query, params, (err, rows) => {
        if (err) reject(err);
        else resolve(rows);
      });
    });
  }

  async getDashboardStats() {
    const query = `
      SELECT
        json_extract(json, '$.metadata.author') as author,
        json_extract(json, '$.status') as status,
        COUNT(*) as count
      FROM read_json_auto('${this.basePath}/**/*.json')
      GROUP BY author, status
      ORDER BY count DESC
    `;

    return new Promise((resolve, reject) => {
      this.db.all(query, [], (err, rows) => {
        if (err) reject(err);
        else resolve(rows);
      });
    });
  }
}

module.exports = RulesService;
```

### Git Integration Example

```javascript
// git-service.js
const { exec } = require('child_process');
const util = require('util');
const execPromise = util.promisify(exec);

class GitService {
  constructor(repoPath = '.') {
    this.repoPath = repoPath;
  }

  async add(filePath) {
    const { stdout, stderr } = await execPromise(`git add ${filePath}`, {
      cwd: this.repoPath
    });
    return { stdout, stderr };
  }

  async commit(message) {
    const { stdout, stderr } = await execPromise(`git commit -m "${message}"`, {
      cwd: this.repoPath
    });
    return { stdout, stderr };
  }

  async push() {
    const { stdout, stderr } = await execPromise('git push', {
      cwd: this.repoPath
    });
    return { stdout, stderr };
  }

  async pull() {
    const { stdout, stderr } = await execPromise('git pull', {
      cwd: this.repoPath
    });
    return { stdout, stderr };
  }

  async status() {
    const { stdout } = await execPromise('git status --porcelain', {
      cwd: this.repoPath
    });
    return stdout;
  }

  async hasUncommittedChanges() {
    const status = await this.status();
    return status.trim().length > 0;
  }

  async detectConflict(filePath) {
    await this.fetch();
    const status = await this.status();
    return status.includes(filePath);
  }

  async fetch() {
    await execPromise('git fetch', { cwd: this.repoPath });
  }
}

module.exports = GitService;
```

---

**End of Document**
