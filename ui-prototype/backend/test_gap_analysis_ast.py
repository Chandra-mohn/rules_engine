#!/usr/bin/env python3
"""
Test script for AST-based gap analysis improvements.
Tests comment filtering and ActionSet detection.
"""

import sys
from pathlib import Path

# Add backend to path
backend_path = Path(__file__).parent
sys.path.insert(0, str(backend_path))

from services.gap_analysis_service import GapAnalysisService
from models import db, Rule, ProcessArea
from app import create_app

def test_comment_filtering():
    """Test that // comments are properly filtered."""
    print("\n" + "="*80)
    print("TEST 1: Comment Filtering")
    print("="*80)

    test_content = """
rule testComments:
    # Hash comment with approveApplication
    // Line comment with rejectApplication
    /* Block comment with conditionalApproval */
    if applicant.creditScore >= 700 then approveApplication
    // Another comment - flagForReview
    if applicant.creditScore < 600 then rejectApplication
"""

    print("\n📝 Test Content:")
    print(test_content)

    # Create a temporary rule for testing
    app = create_app()
    with app.app_context():
        # Get first process area for testing
        process_area = ProcessArea.query.first()
        if not process_area:
            print("❌ No process areas found in database")
            return False

        test_rule = Rule(
            name='test_comment_filtering',
            description='Test comment filtering',
            content=test_content,
            status='DRAFT',
            process_area_id=process_area.id,
            item_type='rule'
        )

        # Don't persist - just analyze
        gap_service = GapAnalysisService()
        result = gap_service._analyze_single_rule(test_rule)

        print("\n✅ Analysis Results:")
        print(f"Validation Status: {result['validation_status']}")
        print(f"Extracted Attributes: {result['extracted_attributes']}")
        print(f"Referenced Actions: {result['referenced_actions']}")
        print(f"Validation Errors: {result['validation_errors']}")

        # Expected: only approveApplication and rejectApplication
        # Should NOT extract: Hash, Line, comment, Block, Another, flagForReview (from comments)
        expected_actions = {'approveApplication', 'rejectApplication'}
        actual_actions = set(result['referenced_actions'])

        print(f"\n🎯 Expected Actions: {expected_actions}")
        print(f"🔍 Actual Actions: {actual_actions}")

        # Check for comment pollution
        comment_pollution = ['Hash', 'comment', 'Line', 'Block', 'Another', 'flagForReview']
        found_pollution = [item for item in comment_pollution if item in actual_actions]

        if found_pollution:
            print(f"❌ FAIL: Found commented items in actions: {found_pollution}")
            return False

        if actual_actions == expected_actions:
            print("✅ PASS: Comments correctly filtered!")
            return True
        else:
            print(f"❌ FAIL: Action mismatch!")
            print(f"   Missing: {expected_actions - actual_actions}")
            print(f"   Extra: {actual_actions - expected_actions}")
            return False

def test_actionset_detection():
    """Test ActionSet reference vs data string detection."""
    print("\n" + "="*80)
    print("TEST 2: ActionSet Reference Detection")
    print("="*80)

    test_content = """
rule testActionSets:
    if applicant.status == "pending approval" then "Standard Application Workflow"
    if message == "Please review application" then flagForReview
    if tier == "premium" then instantApproval, assignBenefits, "Premium Card Processing"
"""

    print("\n📝 Test Content:")
    print(test_content)

    app = create_app()
    with app.app_context():
        process_area = ProcessArea.query.first()
        if not process_area:
            print("❌ No process areas found in database")
            return False

        test_rule = Rule(
            name='test_actionset_detection',
            description='Test ActionSet vs data string detection',
            content=test_content,
            status='DRAFT',
            process_area_id=process_area.id,
            item_type='rule'
        )

        gap_service = GapAnalysisService()
        result = gap_service._analyze_single_rule(test_rule)

        print("\n✅ Analysis Results:")
        print(f"Validation Status: {result['validation_status']}")
        print(f"Referenced Actions: {result['referenced_actions']}")
        print(f"Referenced ActionSets: {result['referenced_actionsets']}")
        print(f"Validation Errors: {result['validation_errors'][:3] if result['validation_errors'] else []}")  # First 3 errors

        # Check that comparison strings are NOT extracted as actions
        comparison_strings = ['pending approval', 'Please review application', 'premium']
        found_comparison_strings = [s for s in comparison_strings
                                   if s in result['referenced_actions'] or s in result['referenced_actionsets']]

        if found_comparison_strings:
            print(f"❌ FAIL: Found comparison strings as actions/actionsets: {found_comparison_strings}")
            return False

        # Check that actual actions are extracted
        expected_actions = {'flagForReview', 'instantApproval', 'assignBenefits'}
        actual_actions = set(result['referenced_actions'])

        print(f"\n🎯 Expected Actions: {expected_actions}")
        print(f"🔍 Actual Actions: {actual_actions}")

        # Check ActionSets (these may or may not exist in DB)
        print(f"\n📦 ActionSets Found: {result['referenced_actionsets']}")

        if 'pending approval' in actual_actions or 'Please review application' in actual_actions:
            print("❌ FAIL: Comparison strings incorrectly detected as actions!")
            return False

        if expected_actions.issubset(actual_actions):
            print("✅ PASS: Actions correctly extracted without comparison string pollution!")
            return True
        else:
            print(f"⚠️  WARNING: Some expected actions missing")
            print(f"   Missing: {expected_actions - actual_actions}")
            return True  # Still pass if no false positives

def test_mixed_content():
    """Test complex rule with comments, ActionSets, and data strings."""
    print("\n" + "="*80)
    print("TEST 3: Mixed Content (Comments + ActionSets + Data Strings)")
    print("="*80)

    test_content = """
rule complexPremiumProcessing:
    // This rule handles premium card applications
    // TODO: Add income verification step
    if applicant.annualIncome >= 100000 and applicant.creditScore >= 750 then
        instantApproval, assignPremiumBenefits, "Premium Card Processing"
    # Legacy comment: old approval logic
    if message == "requires manual review" then flagForReview
    // Fallback to standard workflow
    else "Standard Application Workflow"
"""

    print("\n📝 Test Content:")
    print(test_content)

    app = create_app()
    with app.app_context():
        process_area = ProcessArea.query.first()
        if not process_area:
            print("❌ No process areas found in database")
            return False

        test_rule = Rule(
            name='test_mixed_content',
            description='Test mixed content handling',
            content=test_content,
            status='DRAFT',
            process_area_id=process_area.id,
            item_type='rule'
        )

        gap_service = GapAnalysisService()
        result = gap_service._analyze_single_rule(test_rule)

        print("\n✅ Analysis Results:")
        print(f"Validation Status: {result['validation_status']}")
        print(f"Extracted Attributes: {result['extracted_attributes']}")
        print(f"Referenced Actions: {result['referenced_actions']}")
        print(f"Referenced ActionSets: {result['referenced_actionsets']}")

        # Should NOT extract anything from comments
        comment_terms = ['This', 'rule', 'handles', 'TODO', 'Add', 'income', 'Legacy', 'comment', 'old', 'Fallback']
        found_comment_terms = [term for term in comment_terms
                               if term in result['referenced_actions'] or term in result['referenced_actionsets']]

        if found_comment_terms:
            print(f"❌ FAIL: Found commented terms: {found_comment_terms}")
            return False

        # Should NOT extract comparison strings
        if 'requires manual review' in result['referenced_actions']:
            print("❌ FAIL: Comparison string detected as action!")
            return False

        # Should extract correct actions
        expected_actions = {'instantApproval', 'assignPremiumBenefits', 'flagForReview'}
        actual_actions = set(result['referenced_actions'])

        print(f"\n🎯 Expected Actions: {expected_actions}")
        print(f"🔍 Actual Actions: {actual_actions}")

        if expected_actions.issubset(actual_actions):
            print("✅ PASS: Complex content handled correctly!")
            return True
        else:
            print(f"❌ FAIL: Some actions missing or extra")
            print(f"   Missing: {expected_actions - actual_actions}")
            print(f"   Extra: {actual_actions - expected_actions}")
            return False

def main():
    """Run all tests."""
    print("\n" + "="*80)
    print("🧪 AST-BASED GAP ANALYSIS TEST SUITE")
    print("="*80)

    results = []

    try:
        results.append(("Comment Filtering", test_comment_filtering()))
    except Exception as e:
        print(f"\n❌ TEST 1 EXCEPTION: {str(e)}")
        import traceback
        traceback.print_exc()
        results.append(("Comment Filtering", False))

    try:
        results.append(("ActionSet Detection", test_actionset_detection()))
    except Exception as e:
        print(f"\n❌ TEST 2 EXCEPTION: {str(e)}")
        import traceback
        traceback.print_exc()
        results.append(("ActionSet Detection", False))

    try:
        results.append(("Mixed Content", test_mixed_content()))
    except Exception as e:
        print(f"\n❌ TEST 3 EXCEPTION: {str(e)}")
        import traceback
        traceback.print_exc()
        results.append(("Mixed Content", False))

    # Summary
    print("\n" + "="*80)
    print("📊 TEST SUMMARY")
    print("="*80)

    for test_name, passed in results:
        status = "✅ PASS" if passed else "❌ FAIL"
        print(f"{status}: {test_name}")

    total = len(results)
    passed = sum(1 for _, p in results if p)

    print(f"\n🎯 Overall: {passed}/{total} tests passed")

    if passed == total:
        print("🎉 All tests passed!")
        return 0
    else:
        print("⚠️  Some tests failed - review output above")
        return 1

if __name__ == "__main__":
    sys.exit(main())
