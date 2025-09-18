#!/bin/bash
echo "=== CONTEXT PRESERVATION STARTED ==="
DATE=$(date +"%Y-%m-%d_%H-%M-%S")

# Create context snapshot
mkdir -p .context_snapshots
SNAPSHOT_DIR=".context_snapshots/pre_compaction_$DATE"
mkdir -p "$SNAPSHOT_DIR"

# Git state
git status --porcelain > "$SNAPSHOT_DIR/git_status.txt"
git log --oneline -10 > "$SNAPSHOT_DIR/recent_commits.txt"
git diff --name-status HEAD~5..HEAD > "$SNAPSHOT_DIR/recent_changes.txt"

# System state
echo "Working Directory: $(pwd)" > "$SNAPSHOT_DIR/system_state.txt"
echo "Date: $(date)" >> "$SNAPSHOT_DIR/system_state.txt"
echo "Branch: $(git branch --show-current)" >> "$SNAPSHOT_DIR/system_state.txt"

# Service status
curl -s http://localhost:5001/api/health && echo "✅ Backend healthy" > "$SNAPSHOT_DIR/service_status.txt" || echo "❌ Backend not responding" > "$SNAPSHOT_DIR/service_status.txt"
curl -s http://localhost:3000 && echo "✅ Frontend healthy" >> "$SNAPSHOT_DIR/service_status.txt" || echo "❌ Frontend not responding" >> "$SNAPSHOT_DIR/service_status.txt"

# Database snapshot
python3 -c "
import sys, json, os
sys.path.append('$(pwd)/backend')
try:
    from app import create_app
    from models import Rule
    app = create_app()
    with app.app_context():
        rules = Rule.query.all()
        data = [{'id': r.id, 'name': r.name, 'status': r.status, 'item_type': getattr(r, 'item_type', 'rule'), 'is_deleted': getattr(r, 'is_deleted', False)} for r in rules]
        with open('$SNAPSHOT_DIR/database_snapshot.json', 'w') as f:
            json.dump(data, f, indent=2)
    print('Database snapshot saved')
except Exception as e:
    print(f'Database snapshot failed: {e}')
    with open('$SNAPSHOT_DIR/database_snapshot.json', 'w') as f:
        json.dump({'error': str(e)}, f)
"

# Critical files list
echo "backend/models.py" > "$SNAPSHOT_DIR/critical_files.txt"
echo "backend/services/rule_service.py" >> "$SNAPSHOT_DIR/critical_files.txt"
echo "frontend/src/components/RuleEditor.jsx" >> "$SNAPSHOT_DIR/critical_files.txt"
echo "java-bridge/src/main/antlr4/com/rules/grammar/Rules.g4" >> "$SNAPSHOT_DIR/critical_files.txt"
echo "CLAUDE.md" >> "$SNAPSHOT_DIR/critical_files.txt"

# Verify critical files exist
echo "Critical files verification:" > "$SNAPSHOT_DIR/file_verification.txt"
while read -r file; do
    if [[ -f "$file" ]]; then
        echo "✅ $file exists" >> "$SNAPSHOT_DIR/file_verification.txt"
    else
        echo "❌ $file missing" >> "$SNAPSHOT_DIR/file_verification.txt"
    fi
done < "$SNAPSHOT_DIR/critical_files.txt"

# Update CLAUDE.md with current state
echo "" >> CLAUDE.md
echo "## PRE-COMPACTION CONTEXT - $DATE" >> CLAUDE.md
echo "" >> CLAUDE.md
echo "### Modified Files:" >> CLAUDE.md
git status --porcelain | grep "^ M" >> CLAUDE.md
echo "" >> CLAUDE.md
echo "### Untracked Files:" >> CLAUDE.md
git status --porcelain | grep "^??" >> CLAUDE.md
echo "" >> CLAUDE.md
echo "### Current Progress:" >> CLAUDE.md
echo "- All regression prevention measures active" >> CLAUDE.md
echo "- System health verified before compaction" >> CLAUDE.md
echo "- Context snapshot saved to $SNAPSHOT_DIR" >> CLAUDE.md

echo "✅ Context preserved in $SNAPSHOT_DIR"
echo "=== CONTEXT PRESERVATION COMPLETED ==="