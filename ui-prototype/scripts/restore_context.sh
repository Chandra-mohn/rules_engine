#!/bin/bash
echo "=== CONTEXT RESTORATION STARTED ==="

# Find latest context snapshot
if [[ -d ".context_snapshots" ]]; then
    LATEST_SNAPSHOT=$(ls -t .context_snapshots/pre_compaction_* 2>/dev/null | head -1)
    if [[ -n "$LATEST_SNAPSHOT" ]]; then
        echo "Restoring from: $LATEST_SNAPSHOT"
    else
        echo "⚠️  No pre-compaction snapshots found"
    fi
else
    echo "⚠️  No context snapshots directory found"
fi

# Verify working directory
EXPECTED_DIR="/Users/chandramohn/workspace/rules_engine/ui-prototype"
if [[ "$(pwd)" != "$EXPECTED_DIR" ]]; then
    echo "❌ Wrong directory. Current: $(pwd), Expected: $EXPECTED_DIR"
    echo "Please navigate to the correct directory first"
    exit 1
fi

# Verify critical files
echo "Verifying critical files..."
CRITICAL_FILES=(
    "backend/models.py"
    "backend/services/rule_service.py"
    "frontend/src/components/RuleEditor.jsx"
    "java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4"
    "CLAUDE.md"
    "CONTEXT_PRESERVATION_FRAMEWORK.md"
)

MISSING_FILES=0
for file in "${CRITICAL_FILES[@]}"; do
    if [[ -f "$file" ]]; then
        echo "✅ $file exists"
    else
        echo "❌ $file missing - CRITICAL ERROR"
        MISSING_FILES=$((MISSING_FILES + 1))
    fi
done

if [[ $MISSING_FILES -gt 0 ]]; then
    echo "❌ $MISSING_FILES critical files missing. Restoration cannot continue safely."
    exit 1
fi

# Compare current git status with pre-compaction (if snapshot exists)
if [[ -n "$LATEST_SNAPSHOT" && -f "$LATEST_SNAPSHOT/git_status.txt" ]]; then
    echo "Comparing git status..."
    git status --porcelain > current_git_status.txt
    if cmp -s current_git_status.txt "$LATEST_SNAPSHOT/git_status.txt"; then
        echo "✅ Git status unchanged from pre-compaction"
    else
        echo "⚠️  Git status changed - review differences:"
        echo "--- Pre-compaction ---"
        cat "$LATEST_SNAPSHOT/git_status.txt"
        echo "--- Current ---"
        cat current_git_status.txt
        echo "--- End comparison ---"
    fi
    rm -f current_git_status.txt
fi

# Verify database integrity (if snapshot exists)
if [[ -n "$LATEST_SNAPSHOT" && -f "$LATEST_SNAPSHOT/database_snapshot.json" ]]; then
    echo "Verifying database integrity..."
    python3 -c "
import sys, json, os
sys.path.append('$(pwd)/backend')
try:
    from app import create_app
    from models import Rule
    app = create_app()
    with app.app_context():
        rules = Rule.query.filter_by(is_deleted=False).all()
        current_count = len(rules)

    # Load pre-compaction snapshot
    with open('$LATEST_SNAPSHOT/database_snapshot.json', 'r') as f:
        snapshot_data = json.load(f)
        if 'error' in snapshot_data:
            print(f'⚠️  Pre-compaction snapshot had error: {snapshot_data[\"error\"]}')
        else:
            snapshot_count = len([r for r in snapshot_data if not r.get('is_deleted', False)])
            print(f'Pre-compaction rules: {snapshot_count}')
            print(f'Current rules: {current_count}')
            if current_count >= snapshot_count:
                print('✅ Database integrity maintained or improved')
            else:
                print('❌ Database count decreased - investigate data loss')
except Exception as e:
    print(f'Database verification failed: {e}')
"
fi

# Check if regression test suite exists and run it
if [[ -f "backend/test_regression_suite.py" ]]; then
    echo "Running regression tests..."
    cd backend
    python3 test_regression_suite.py || echo "⚠️  Some regression tests failed"
    cd ..
else
    echo "⚠️  Regression test suite not found"
fi

# Service health check
echo "Checking service health..."
curl -s -f http://localhost:5001/api/health >/dev/null && echo "✅ Backend healthy" || echo "❌ Backend not responding (may need to start)"
curl -s -f http://localhost:3000 >/dev/null && echo "✅ Frontend healthy" || echo "❌ Frontend not responding (may need to start)"

# Display key reminders
echo ""
echo "=== CONTEXT RESTORATION SUMMARY ==="
echo "Working Directory: $(pwd)"
echo "Git Branch: $(git branch --show-current)"
echo "Last Commit: $(git log -1 --oneline)"
echo ""
echo "Key Reminders:"
echo "- Backend should run on port 5001: cd backend && python app.py"
echo "- Frontend should run on port 3000: cd frontend && npm start"
echo "- Use absolute paths only in development"
echo "- Status values must be uppercase: DRAFT, VALID, PEND, SCHD, PROD"
echo "- Check CLAUDE.md for latest project documentation"
echo ""

echo "=== CONTEXT RESTORATION COMPLETED ==="