# Action/ActionSet Conceptual Model Discussion

**Date**: 2025-10-04
**Status**: Pending Implementation
**Context**: Gap analysis showing 0 missing actions/attributes despite actual missing items

---

## Problem Statement

Gap analysis dashboard shows:
- **Missing Actions**: 0 ❌ (incorrect)
- **Missing Attributes**: 0 ❌ (incorrect)

However, individual rule validations correctly show multiple missing actions and attributes.

### Root Cause

In `gap_analysis_service.py` and `rule_service.py`:

```python
def _get_existing_actions(self) -> Set[str]:
    # Queries for item_type='action' which doesn't exist in database
    actions = db.session.query(Rule.name).filter(
        Rule.item_type.in_(['action', 'actionset'])
    ).all()
    # Returns empty/nearly empty set because 'action' type doesn't exist
```

**Database Reality**:
- Only has: `'rule'`, `'actionset'`, `'mon_rule'`, `'non_mon_rule'`
- Does NOT have: `'action'` as item_type

**Result**:
```
available_actions = {} (empty set)
referenced_actions = {approve, reject, notify, ...}
missing = referenced - available = {} (reports 0 missing)
```

---

## Conceptual Clarity Needed

**User's Key Statement**:
> "every actionset is by default an action unless we create a actionset with that name"

### Interpretation

This reveals a **two-tier action model**:

1. **Built-in/System Actions** (NOT in database)
   - Primitive operations: `approve`, `reject`, `notify`, `calculate`, etc.
   - These are the "default" actions
   - Defined in code/configuration, not as database records

2. **User-Defined Actions (ActionSets)** (IN database)
   - Stored with `item_type='actionset'`
   - Composite workflows that can be invoked like built-in actions
   - **Override behavior**: If you create an ActionSet named "approve", it overrides the built-in "approve"

---

## Proposed Conceptual Model

### Action Resolution Logic

When a rule references `executeAction("approvalWorkflow")`:

```
Step 1: Check for ActionSet named "approvalWorkflow" in database
        ├─ Found? → Use user-defined ActionSet (explicit definition wins)
        └─ Not found? → Treat as built-in system action

Step 2: Validate availability
        available_actions = built_in_actions ∪ actionsets_from_db
        is_valid = "approvalWorkflow" ∈ available_actions
```

### Gap Analysis Logic

```python
available_actions = built_in_actions ∪ actionsets_from_db
referenced_actions = extract_from_rules_via_ast()
missing_actions = referenced_actions - available_actions
```

---

## Proposed Implementation Fix

### Option A: Built-in Actions + ActionSets (RECOMMENDED)

```python
def _get_existing_actions(self) -> Set[str]:
    """
    Returns all available actions: built-in system actions + user-defined ActionSets.

    Built-in actions are always available. User-defined ActionSets can override built-ins
    if they share the same name.
    """
    # Built-in system actions (could move to config file)
    built_in_actions = {
        'approve', 'reject', 'notify', 'log', 'calculate',
        'setVariable', 'callAPI', 'sendEmail', 'updateStatus',
        'executeAction',  # meta-action for invoking other actions
        'assignTo', 'escalate', 'archive', 'createTask'
    }

    # User-defined ActionSets from database
    actionsets = db.session.query(Rule.name).filter(
        Rule.item_type == 'actionset',
        Rule.status == 'VALID'
    ).all()

    actionset_names = {action.name for action in actionsets}

    # Union: ActionSets override built-ins if names collide
    return built_in_actions | actionset_names
```

**Rationale**:
- ✅ Matches user's statement: "every actionset is by default an action"
- ✅ Allows user-defined ActionSets to override built-ins
- ✅ Simple, clear semantics
- ✅ No database schema changes needed

### Option B: Configuration File Approach

Create `backend/config/built_in_actions.py`:

```python
# Built-in system actions available in all rules
BUILT_IN_ACTIONS = {
    # Approval/Rejection
    'approve', 'reject', 'conditionalApprove',

    # Notification
    'notify', 'sendEmail', 'sendSMS',

    # Data Operations
    'setVariable', 'calculate', 'log',

    # Workflow
    'executeAction', 'assignTo', 'escalate',

    # State Management
    'updateStatus', 'archive', 'createTask'
}

# Actions that cannot be overridden by ActionSets (security/system critical)
PROTECTED_ACTIONS = {
    'executeAction',  # Core engine operation
    'setVariable',    # Variable management
    'log'            # Audit trail
}
```

Then in gap analysis:

```python
from config.built_in_actions import BUILT_IN_ACTIONS

def _get_existing_actions(self) -> Set[str]:
    actionsets = db.session.query(Rule.name).filter(
        Rule.item_type == 'actionset',
        Rule.status == 'VALID'
    ).all()

    actionset_names = {action.name for action in actionsets}
    return BUILT_IN_ACTIONS | actionset_names
```

**Rationale**:
- ✅ Centralized action management
- ✅ Easy to extend built-in actions
- ✅ Can define protected actions
- ✅ Clear separation of concerns

---

## Open Questions for User

1. **Built-in Action Inventory**: What are ALL the built-in actions the rules engine supports?

2. **Override Semantics**: Should ActionSets be allowed to override built-in actions?
   - Example: If user creates `actionset approve: ...`, does it replace built-in `approve`?
   - Recommendation: Allow override for flexibility

3. **Protected Actions**: Should some actions be protected from override?
   - Example: `executeAction`, `log` might be system-critical
   - Recommendation: Define small set of protected actions if needed

4. **Configuration Location**: Where should built-in actions be defined?
   - Option A: Hardcoded in gap_analysis_service.py (simple, less flexible)
   - Option B: Configuration file (maintainable, extensible)
   - Option C: Database table (dynamic, but complex)
   - Recommendation: Option B (config file)

5. **Action Validation**: Should we validate that referenced built-in actions actually exist in implementation?
   - Example: Rule references `approve` → verify the engine can execute `approve`
   - Recommendation: Yes, but separate from gap analysis (runtime validation)

6. **Attribute Handling**: Should we apply similar logic to attributes?
   - Built-in attributes (e.g., `applicantScore`, `requestAmount`)
   - User-defined schema attributes
   - Recommendation: Yes, consistent model

---

## Validation Checks Before Implementation

- [ ] Confirm list of all built-in actions
- [ ] Decide on override semantics (allow/prevent)
- [ ] Choose configuration approach (hardcoded/config file/database)
- [ ] Verify attribute handling should follow same pattern
- [ ] Decide if protected actions are needed
- [ ] Confirm gap analysis should report missing built-ins

---

## Implementation Plan (Once Approved)

### Phase 1: Define Built-in Actions
- [ ] Create configuration file or constant set
- [ ] Document each built-in action's purpose
- [ ] Define protected actions (if needed)

### Phase 2: Fix Gap Analysis
- [ ] Update `_get_existing_actions()` to include built-ins
- [ ] Update `_get_existing_attributes()` for consistency
- [ ] Test with demo data

### Phase 3: Validation
- [ ] Verify gap analysis dashboard shows correct counts
- [ ] Verify individual rule validations still work
- [ ] Test override scenarios (ActionSet with same name as built-in)

### Phase 4: Documentation
- [ ] Document action resolution logic
- [ ] Update gap analysis documentation
- [ ] Add examples of built-in vs user-defined actions

---

## Expected Outcome

After implementation:

**Gap Analysis Dashboard**:
- Missing Actions: Shows actual count of referenced actions not in (built_ins ∪ actionsets)
- Missing Attributes: Shows actual count of referenced attributes not in (built_ins ∪ schema)

**Consistency**:
- Dashboard summary matches individual rule validations
- Clear distinction between built-in and user-defined actions
- Predictable override behavior

---

## References

- **Current Bug**: `backend/services/gap_analysis_service.py:200-210`
- **Related Code**: `backend/services/rule_service.py:541-558`
- **AST Parser**: `backend/grammar_parser/rules_parser.py:390-487`
- **Fixture System**: `FIXTURE_SYSTEM_README.md`

---

## Implementation Complete ✅

### Final Decisions Made

**Q1: Action Content Field** ✅
- Stores JSON with `javaPath` (relative file path, cross-platform) and `parameters` array
- Example:
```json
{
  "javaPath": "actions/ApprovalAction.java",
  "parameters": [
    {"name": "requestId", "type": "string", "required": true},
    {"name": "userId", "type": "string", "required": true}
  ]
}
```

**Q2: Action Parameters** ✅
- Stored in `content` field as JSON (not separate table)
- Includes parameter name, type, and required flag

**Q3: Fixture Data** ✅
- Created 3 sample Actions: `approve`, `reject`, `sendEmail`
- Stored in `fixtures/demo_data.py`

**Q4: Validation** ✅
- No UI validation of Java file existence
- Allows creating Action records before Java implementation exists

### Schema Implementation

**Actions stored in `Rule` table**:
- `item_type='action'`
- `name`: Action name (e.g., "approve")
- `description`: Human-readable purpose
- `content`: JSON with javaPath and parameters
- `process_area_id`: Required (using first PA as workaround for global actions)
- `status`: VALID/INVALID

### Gap Analysis Fix

**Current State**: Gap analysis now correctly queries for Actions
```python
Rule.query.filter(
    Rule.item_type.in_(['action', 'actionset'])
).all()
```

**Database Contains**:
- 3 Actions: approve, reject, sendEmail
- 7 ActionSets: Various workflows

**Gap Analysis Will Show**:
- Missing Actions: Referenced actions NOT in (db_actions ∪ db_actionsets)
- This resolves the "0 missing actions" bug

### Files Modified

1. **backend/fixtures/demo_data.py**:
   - Added Actions fixture creation
   - JSON content with javaPath and parameters
   - Added to tracking dictionary

2. **backend/cli_commands.py**:
   - Updated db-info to show Actions count

### Testing Results

```bash
flask db-info
# Output:
#   Rules (total)...........................       31
#     - Regular Rules.......................       13
#     - ActionSets..........................        7
#     - Actions.............................        3  ← NEW
#     - Monetary Rules......................        4
#     - Non-Monetary Rules..................        4
```

### Next Steps (Future Work)

1. **UI Component** (deferred for now):
   - Add Actions tab/section
   - Create/Edit/Delete Actions
   - Form for name, description, javaPath, parameters

2. **Schema Enhancement** (consider for future):
   - Make `process_area_id` nullable for global Actions
   - Remove unique constraint dependency on process_area_id for Actions

3. **Gap Analysis Validation**:
   - Test with actual missing actions in rules
   - Verify dashboard shows correct counts

### Final Conceptual Model

```
Execution Hierarchy:
  Rules/ActionSets (DB: item_type='rule'|'actionset'|'mon_rule'|'non_mon_rule')
    ├── Can invoke ActionSets (by name)
    └── Can invoke Actions (by name)
         └── Actions (DB: item_type='action')
              └── Points to Java implementation (via javaPath)
```

**Name Resolution**:
1. Check DB for ActionSet or Action with that name
2. If found → use it
3. If not found → report as missing dependency
