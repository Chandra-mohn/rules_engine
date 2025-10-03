#!/usr/bin/env python3
"""
Test script to demonstrate REDEFINES variant enumeration.
Shows all possible combinations for the small complex copybook.
"""

from copybook_parser import parse_copybook
from copybook_processor import CopybookProcessor, RedefinesEnumerator


def print_separator(char='=', length=70):
    """Print a separator line"""
    print(char * length)


def print_tree(node, indent=0, max_depth=4):
    """Print AST tree with limited depth"""
    if indent > max_depth:
        return

    prefix = "  " * indent
    redef = f" REDEFINES {node.redefines}" if node.redefines else ""
    pic = f" PIC {node.picture.picture_string}" if node.picture else ""
    occurs = f" OCCURS {node.occurs}" if node.occurs else ""
    usage = f" {node.usage.usage_type}" if node.usage else ""

    print(f"{prefix}{node.level:02d} {node.name}{redef}{pic}{occurs}{usage}")

    for child in node.children:
        print_tree(child, indent + 1, max_depth)


def main():
    print_separator()
    print("COBOL REDEFINES Variant Enumeration Test")
    print_separator()
    print()

    # Parse small complex copybook
    print("Parsing: test_small_complex.cpy")
    ast = parse_copybook('test_small_complex.cpy')
    print(f"✓ Parsed successfully: {ast.name}")
    print()

    # Process AST
    print("Processing AST...")
    processor = CopybookProcessor(ast)
    processed = processor.process()

    print(f"✓ Total size: {processed.total_size} bytes")
    print(f"✓ REDEFINES groups: {len(processed.redefines_groups)}")
    print()

    # Show REDEFINES groups
    print_separator('-')
    print("REDEFINES Groups Identified:")
    print_separator('-')

    for i, group in enumerate(processed.redefines_groups, 1):
        print(f"\n{i}. Group for '{group.original_field}':")
        print(f"   Offset: {group.offset} bytes")
        print(f"   Variants:")
        for variant in group.variants:
            print(f"     - {variant.name} (level {variant.level})")

    print()
    print_separator('-')
    print("Enumerating All Variant Combinations:")
    print_separator('-')

    # Enumerate variants
    enumerator = RedefinesEnumerator(processed)
    variants = enumerator.enumerate_variants()

    print(f"\nTotal variants generated: {len(variants)}")
    print()

    # Show each variant structure (limited depth)
    for i, (variant_name, variant_tree) in enumerate(variants, 1):
        print(f"\n{'=' * 70}")
        print(f"Variant {i}: {variant_name}")
        print('=' * 70)
        print_tree(variant_tree, max_depth=3)

    # Group by top-level
    print()
    print_separator('-')
    print("Grouped by Top-Level REDEFINES:")
    print_separator('-')

    grouped = enumerator.group_variants_by_top_level(variants)

    for group_name, group_variants in grouped.items():
        print(f"\n{group_name}: {len(group_variants)} variant(s)")
        for variant_name, _ in group_variants:
            print(f"  - {variant_name}")

    print()
    print_separator()
    print("Test Complete!")
    print_separator()


if __name__ == '__main__':
    main()
