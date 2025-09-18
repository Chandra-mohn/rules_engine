# Context Preservation Quick Reference

## 🚨 BEFORE AUTO-COMPACTION (Essential Commands)

```bash
# Save complete context snapshot
./save_context.sh

# Commit any important untracked files
git add CLAUDE.md CONTEXT_PRESERVATION_FRAMEWORK.md
git commit -m "Context preservation: Update documentation before compaction"

# Verify system health
curl http://localhost:5001/api/health && echo "Backend OK"
curl http://localhost:3000 && echo "Frontend OK"
```

## 🔄 AFTER COMPACTION/NEW SESSION (Recovery Commands)

```bash
# Verify correct directory
pwd
# Should be: /Users/chandramohn/workspace/rules_engine/ui-prototype

# Restore context
./restore_context.sh

# If services not running, start them:
cd backend && python app.py &
cd frontend && npm start &
```

## 📋 CRITICAL CONTEXT TO REMEMBER

### Project Architecture
- **Multi-tier**: React (3000) → Flask (5001) → Java Engine
- **Database**: SQLite with unified Rules/ActionSets table
- **Status Values**: DRAFT, VALID, PEND, SCHD, PROD (uppercase only)

### Current State (Pre-Compaction)
- **Modified Files**: 10 files with ActionSet and UI improvements
- **Database**: 15 rules (13 active, 2 deleted)
- **Recent Work**: ActionSet implementation, status validation fixes
- **Zero Regressions**: All systems working correctly

### Essential Files
```
/Users/chandramohn/workspace/rules_engine/ui-prototype/
├── CLAUDE.md                                    # Complete project documentation
├── backend/models.py                           # Database models (unified table)
├── backend/services/rule_service.py            # Core business logic
├── frontend/src/components/RuleEditor.jsx      # Main UI component
└── java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4  # Grammar definition
```

### Key Patterns
- **File Paths**: Always use absolute paths
- **API Pattern**: API → Service → Model layers
- **Status Auto-promotion**: DRAFT → VALID on successful validation
- **UI Components**: Functional components with hooks pattern

## 🆘 EMERGENCY RECOVERY

If major regression detected:
```bash
# Stop services
pkill -f "python app.py"
pkill -f "npm start"

# Restore database
cp backend/database/rules.db.backup backend/database/rules.db

# Restart services
cd backend && python app.py &
cd frontend && npm start &

# Verify recovery
python backend/test_regression_suite.py
```

## 📞 SESSION CONTINUITY CHECKLIST

- [ ] Working directory: `/Users/chandramohn/workspace/rules_engine/ui-prototype`
- [ ] Git branch: `main`
- [ ] Backend running on port 5001
- [ ] Frontend running on port 3000
- [ ] CLAUDE.md exists and is current
- [ ] No critical files missing
- [ ] Database has ~13 active rules
- [ ] All status values are uppercase

---

**MOST IMPORTANT**: Check `CLAUDE.md` for complete project context and recent changes!