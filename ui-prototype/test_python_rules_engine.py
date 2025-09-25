#!/usr/bin/env python3
"""
Test PythonRulesEngine with the new Phase 3 consolidated architecture.
"""

import sys
from pathlib import Path

# Add backend to path
backend_path = Path(__file__).parent / "backend"
sys.path.insert(0, str(backend_path))

try:
    from services.python_rules_engine import PythonRulesEngine
    print("✅ Successfully imported PythonRulesEngine")
except ImportError as e:
    print(f"❌ Import failed: {e}")
    sys.exit(1)

def test_python_rules_engine():
    """Test that PythonRulesEngine works with consolidated architecture."""
    print("\n🧪 Testing PythonRulesEngine Integration...")

    try:
        # Create engine instance
        engine = PythonRulesEngine()

        # Test rule validation
        test_rule = '''rule "Integration Test":
if applicant.creditScore >= 700 then approveApplication
else rejectApplication'''

        print("  📝 Testing rule validation...")
        validation_result = engine.validate_rule(test_rule)

        assert validation_result is not None, "Validation result should not be None"
        print(f"  ✅ Validation result: {validation_result.get('valid', 'unknown')}")

        # Test that the engine is using UnifiedJavaCodeGenerator
        generator_type = type(engine.code_generator).__name__
        assert generator_type == "UnifiedJavaCodeGenerator", f"Expected UnifiedJavaCodeGenerator, got {generator_type}"
        print(f"  ✅ Using {generator_type}")

        # Test generator mode
        generator_mode = engine.code_generator.mode
        print(f"  ✅ Generator mode: {generator_mode}")

        # Test generator stats
        stats = engine.code_generator.get_generation_stats()
        assert stats['phase_3_complete'] == True, "Phase 3 should be complete"
        assert stats['simple_generator_removed'] == True, "Simple generator should be removed"
        print(f"  ✅ Generator stats: Phase 3 complete, single architecture")

        return True
    except Exception as e:
        print(f"  ❌ PythonRulesEngine test failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def main():
    """Test PythonRulesEngine integration."""
    print("🚀 Testing PythonRulesEngine with Phase 3 Architecture")
    print("=" * 55)

    if test_python_rules_engine():
        print("\n✅ PythonRulesEngine successfully integrated with Phase 3 architecture")
        print("✅ Migration complete - consumers updated")
        return 0
    else:
        print("\n❌ PythonRulesEngine integration failed")
        return 1

if __name__ == "__main__":
    sys.exit(main())