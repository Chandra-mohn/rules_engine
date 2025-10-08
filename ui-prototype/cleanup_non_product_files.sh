#!/bin/bash
# Cleanup Non-Product Files Script
# Generated: 2025-10-08
# Purpose: Remove documentation, test files, and experimental scripts
# Estimated space savings: 12-17 MB (~180-200 files)

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "🧹 Starting cleanup of non-product files..."
echo "📍 Working directory: $SCRIPT_DIR"
echo ""

# Optional: Create backup before cleanup
read -p "📦 Create backup before cleanup? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    BACKUP_DIR="../cleanup_backup_$(date +%Y%m%d_%H%M%S)"
    echo "📦 Creating backup at: $BACKUP_DIR"
    mkdir -p "$BACKUP_DIR"

    # Backup directories
    [ -d .context_snapshots ] && cp -r .context_snapshots "$BACKUP_DIR/" 2>/dev/null || true
    [ -d .memory_consolidation ] && cp -r .memory_consolidation "$BACKUP_DIR/" 2>/dev/null || true
    [ -d docs ] && cp -r docs "$BACKUP_DIR/" 2>/dev/null || true
    [ -d tests ] && cp -r tests "$BACKUP_DIR/" 2>/dev/null || true

    echo "✅ Backup created successfully"
    echo ""
fi

# Phase 1: Delete entire directories
echo "🗑️  Phase 1: Removing directories..."

if [ -d .context_snapshots ]; then
    rm -rf .context_snapshots/
    echo "  ✓ Removed .context_snapshots/"
fi

if [ -d .memory_consolidation ]; then
    rm -rf .memory_consolidation/
    echo "  ✓ Removed .memory_consolidation/"
fi

if [ -d docs ]; then
    rm -rf docs/
    echo "  ✓ Removed docs/"
fi

if [ -d tests ]; then
    rm -rf tests/
    echo "  ✓ Removed tests/"
fi

if [ -d reports ]; then
    rm -rf reports/
    echo "  ✓ Removed reports/"
fi

echo ""

# Phase 2: Delete root-level documentation files
echo "📄 Phase 2: Removing root-level documentation..."

rm -f ANTLR_ENHANCEMENT_PLAN.md
rm -f CODE_GENERATION_QUALITY_REPORT.md
rm -f DEPLOYMENT_BUILD_GUIDE.md
rm -f FIXTURE_SYSTEM_README.md
rm -f GENERATED_CODE_EXPLAINED.md
rm -f GRAMMAR_AND_CODE_GENERATION_ENHANCEMENTS.md
rm -f IMPLEMENTATION_COMPLETE.md
rm -f IMPLEMENTATION_ROADMAP.md
rm -f PERFORMANCE_OPTIMIZATION_GUIDE.md
rm -f SIMPLIFIED_GRAMMAR_PROPOSAL.md

echo "  ✓ Removed 10 documentation files"
echo ""

# Phase 3: Delete root-level test files
echo "🧪 Phase 3: Removing root-level test files..."

rm -f test_*.py
rm -f *_test.py
rm -f debug_*.py
rm -f simple_codegen_test.py
rm -f performance_validation.py
rm -f final_codegen_test.py
rm -f fixed_codegen_test.py

echo "  ✓ Removed test Python files"
echo ""

# Phase 4: Delete JSON result files
echo "📊 Phase 4: Removing JSON result files..."

rm -f comprehensive_codegen_test_report.json
rm -f comprehensive_codegen_test_results.json
rm -f final_codegen_test_results.json
rm -f test_rules.json

echo "  ✓ Removed JSON result files"
echo ""

# Phase 5: Delete obsolete shell scripts
echo "🔧 Phase 5: Removing obsolete scripts..."

rm -f send-test-messages.sh
rm -f start-kafka.sh
rm -f stop-kafka.sh
rm -f docker-compose.kafka.yml

echo "  ✓ Removed obsolete shell scripts"
echo ""

# Phase 6: Clean up backend directory
echo "🐍 Phase 6: Cleaning backend directory..."

cd backend

# Delete backend documentation
rm -f CODE_GENERATION_ARCHITECTURE.md
rm -f CODE_GENERATION_EXPLAINED.md
rm -f DATABASE_SETUP.md
rm -f PARALLEL_ARCHITECTURE_GUIDE.md
rm -f PYTHON_ANTLR_IMPLEMENTATION.md
rm -f regression_prevention_guide.md

echo "  ✓ Removed backend documentation"

# Delete backend test files
rm -f test_*.py
rm -f enhanced_regression_suite.py

echo "  ✓ Removed backend test files"

# Delete backend JSON result files
rm -f data_snapshot.json
rm -f enhanced_regression_results.json
rm -f parallel_performance_results.json
rm -f parallel_performance_results_detailed.json
rm -f regression_test_results.json
rm -f schema_baseline.json
rm -f validation_baseline.json

echo "  ✓ Removed backend JSON results"

# Delete backend utility files (optional - review these first)
# Uncomment if you want to remove these:
# rm -f show_java_code_locations.py
# rm -f data_validator.py
# rm -f data_integrity_monitor.py
# rm -f pre_commit_checks.py
# rm -f validate_system.py
# rm -f migrate_schema_actions.py

# Remove historical docs
if [ -d docs/historical ]; then
    rm -rf docs/
    echo "  ✓ Removed backend/docs/"
fi

cd ..
echo ""

# Phase 7: Clean up scripts directory
echo "📜 Phase 7: Cleaning scripts directory..."

cd scripts

# Delete experimental Claude memory scripts
rm -f claude_memory_manager.py
rm -f code_quality_validator.py
rm -f integrated_memory_system.py
rm -f memory_consolidation_agent.py
rm -f run_memory_consolidation.py
rm -f run_validation_demo.py
rm -f session_continuity_bridge.py
rm -f validation_agent.py
rm -f restore_context.sh
rm -f save_context.sh

echo "  ✓ Removed experimental scripts"

# Optional: Remove Windows-specific files if not on Windows
if [[ "$OSTYPE" != "msys" && "$OSTYPE" != "win32" ]]; then
    rm -f setup-windows.bat
    rm -f test-windows-setup.sh
    echo "  ✓ Removed Windows-specific scripts"
fi

cd ..
echo ""

# Phase 8: Clean up java-bridge directory
echo "☕ Phase 8: Cleaning java-bridge directory..."

cd java-bridge

rm -f classpath.txt
rm -f FINAL-COMPARISON.md
rm -f json-comparison.md

echo "  ✓ Removed java-bridge output files"

cd ..
echo ""

# Phase 9: Optional - Clean generated rules metadata (regenerates automatically)
read -p "🔄 Remove generated rules metadata? (regenerates on demand) (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "🗂️  Phase 9: Removing generated rules metadata..."

    find generated-rules -type f \( -name "README.md" -o -name "rule-metadata.json" \) -delete 2>/dev/null || true

    echo "  ✓ Removed generated metadata (will regenerate)"
    echo ""
fi

# Summary
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✅ Cleanup Complete!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "📊 Summary:"
echo "  • Removed documentation files"
echo "  • Removed test files"
echo "  • Removed JSON result files"
echo "  • Removed experimental scripts"
echo "  • Removed snapshot/cache directories"
echo ""
echo "💾 Estimated space freed: 12-17 MB"
echo ""
echo "📝 Files preserved:"
echo "  ✓ README.md (main documentation)"
echo "  ✓ CLAUDE.md (active context)"
echo "  ✓ All product source code"
echo "  ✓ Configuration files"
echo "  ✓ Essential scripts (setup, start-*)"
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "💡 Note: Generated rules metadata will regenerate automatically"
    echo ""
fi

echo "🎉 Your codebase is now cleaner!"
echo ""
echo "To verify what was removed, check the backup at:"
echo "  $BACKUP_DIR"
echo ""
