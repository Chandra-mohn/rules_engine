#!/usr/bin/env python3

import sys
import os
import re

# Add backend to path for importing
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'backend'))

def parse_rule_name_from_content(content: str) -> str:
    """
    Parse rule name from rule content.
    Supports both quoted and unquoted identifiers:
    - rule "PROMOTION $5%3 @SEARS":
    - rule regularRuleName:
    
    Args:
        content: Rule content string
        
    Returns:
        Parsed rule name or empty string if not found
    """
    if not content:
        return ''
    
    # Match both quoted and unquoted rule names
    # rule "PROMOTION $5%3 @SEARS":
    # rule regularRuleName:
    pattern = r'^\s*rule\s+(?:"([^"]+)"|([a-zA-Z_][a-zA-Z0-9_]*))\s*:'
    match = re.search(pattern, content, re.MULTILINE)
    
    if match:
        return match.group(1) or match.group(2) or ''
    
    return ''

def test_rule_name_parsing():
    """Test cases for rule name parsing."""
    
    test_cases = [
        # Quoted identifiers with special characters
        ('rule "PROMOTION $5%3 @SEARS":\n    if condition then action', "PROMOTION $5%3 @SEARS"),
        ('rule "Multi Word Rule":\n    if condition then action', "Multi Word Rule"),
        ('rule "TIERED DD+1":\n    if condition then action', "TIERED DD+1"),
        
        # Unquoted identifiers
        ('rule regularRuleName:\n    if condition then action', "regularRuleName"),
        ('rule creditScoreCheck:\n    if condition then action', "creditScoreCheck"),
        
        # Edge cases
        ('   rule   "Spaced Rule"  :\n    if condition then action', "Spaced Rule"),
        ('rule simple_rule:\n    if condition then action', "simple_rule"),
        
        # Invalid cases
        ('if condition then action', ''),  # Missing rule declaration
        ('', ''),  # Empty content
        ('rule :\n    if condition then action', ''),  # Missing name
    ]
    
    print("Testing rule name parsing...")
    print("=" * 50)
    
    all_passed = True
    for i, (content, expected) in enumerate(test_cases, 1):
        result = parse_rule_name_from_content(content)
        status = "✅ PASS" if result == expected else "❌ FAIL"
        
        if result != expected:
            all_passed = False
        
        print(f"Test {i}: {status}")
        print(f"  Content: {repr(content[:50])}...")
        print(f"  Expected: {repr(expected)}")
        print(f"  Got:      {repr(result)}")
        print()
    
    print("=" * 50)
    if all_passed:
        print("🎉 All tests passed!")
        return True
    else:
        print("💥 Some tests failed!")
        return False

if __name__ == "__main__":
    success = test_rule_name_parsing()
    sys.exit(0 if success else 1)