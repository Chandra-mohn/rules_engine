#!/usr/bin/env python3
"""
Demo script showing COBOL data parser usage with REDEFINES disambiguation
"""

import json
from cobol_data_parser import CobolDataParser, RedefinesStrategy, create_sample_data


def demo_basic_parsing():
    """Demo 1: Basic parsing without REDEFINES"""
    print("=" * 70)
    print("DEMO 1: Basic Parsing (Small Complex Copybook)")
    print("=" * 70)
    print()

    # Generate sample data
    schema_file = 'output/TRANSACTION-RECORD_CARD-INFO.json'
    data_file = 'sample_transactions.dat'

    print("Step 1: Generating sample data...")
    create_sample_data(schema_file, data_file, num_records=5)
    print()

    # Parse data
    print("Step 2: Parsing data file...")
    parser = CobolDataParser(schema_file)
    records = parser.parse_file(data_file, max_records=3)

    print(f"Successfully parsed {len(records)} records")
    print()

    # Show first record
    print("Step 3: Sample parsed record:")
    print(json.dumps(records[0], indent=2))
    print()


def demo_redefines_discriminator():
    """Demo 2: REDEFINES with discriminator strategy"""
    print("=" * 70)
    print("DEMO 2: REDEFINES Disambiguation with Discriminator")
    print("=" * 70)
    print()

    schema_file = 'output/TRANSACTION-RECORD_CARD-INFO.json'
    data_file = 'sample_transactions.dat'

    parser = CobolDataParser(schema_file)

    # Strategy 1: Use TX-TYPE field to determine PAYMENT vs REFUND
    print("Strategy: Discriminator Field")
    print("  Field: TX-HEADER.TX-TYPE")
    print("  Logic: '01' → PAYMENT-DATA, '02' → REFUND-DATA")
    print()

    def payment_refund_discriminator(record):
        tx_type = record.get('TX-HEADER', {}).get('TX-TYPE', '').strip()
        if tx_type == '01':
            return 'PAYMENT-DATA'
        elif tx_type == '02':
            return 'REFUND-DATA'
        return 'PAYMENT-DATA'  # default

    parser.set_redefines_strategy(payment_refund_discriminator)

    records = parser.parse_file(data_file, max_records=2)
    print(f"Parsed {len(records)} records")
    for i, record in enumerate(records, 1):
        tx_type = record.get('TX-HEADER', {}).get('TX-TYPE', '')
        print(f"  Record {i}: TX-TYPE = {tx_type}")
    print()


def demo_redefines_options():
    """Demo 3: Three REDEFINES disambiguation options"""
    print("=" * 70)
    print("DEMO 3: Three REDEFINES Disambiguation Options")
    print("=" * 70)
    print()

    print("Option 1: DISCRIMINATOR FIELD")
    print("-" * 70)
    print("""
Use a specific field value to determine which variant to parse.

Example: Transaction Type
  - TX-TYPE = 'PAYMENT' → use PAYMENT-DATA variant
  - TX-TYPE = 'REFUND' → use REFUND-DATA variant

Usage:
    def discriminator(record):
        tx_type = record['TX-HEADER']['TX-TYPE']
        return 'PAYMENT-DATA' if tx_type == 'PAYMENT' else 'REFUND-DATA'

    parser.set_redefines_strategy(discriminator)

Pros:
  ✓ Fast and deterministic
  ✓ Matches business logic
  ✓ No trial-and-error

Cons:
  ✗ Requires discriminator field
  ✗ Manual mapping needed
""")

    print("\nOption 2: DEFAULT VARIANT")
    print("-" * 70)
    print("""
Always use the first variant (simplest approach).

Usage:
    parser.set_redefines_strategy(RedefinesStrategy.default_variant)

Pros:
  ✓ Simple, no logic needed
  ✓ Fast

Cons:
  ✗ May parse wrong variant
  ✗ Data may not make sense
""")

    print("\nOption 3: TRY ALL VARIANTS")
    print("-" * 70)
    print("""
Attempt parsing with each variant, use first that validates successfully.

Usage:
    parser.set_redefines_strategy(RedefinesStrategy.try_all)

Validation checks:
  - Numeric fields contain valid numbers
  - Required fields are non-empty
  - Field lengths match schema
  - Cross-field consistency

Pros:
  ✓ No discriminator needed
  ✓ Automatic detection
  ✓ Robust

Cons:
  ✗ Slower (tries multiple parses)
  ✗ May be ambiguous
  ✗ Requires good validation rules
""")

    print("\nOption 4: HYBRID APPROACH")
    print("-" * 70)
    print("""
Combine strategies for different REDEFINES groups.

Example:
  - TX-DATA: Use discriminator (TX-TYPE field)
  - CUSTOMER-INFO: Try all variants with validation
  - PAYMENT-INFO: Use default (first variant)

Usage:
    def hybrid_strategy(record, redefines_group):
        if redefines_group == 'TX-DATA':
            return discriminator_logic(record)
        elif redefines_group == 'CUSTOMER-INFO':
            return try_all_logic(record)
        else:
            return 'default'

Pros:
  ✓ Flexible
  ✓ Optimized per use case
  ✓ Best of all worlds

Cons:
  ✗ More complex
  ✗ Requires analysis of each group
""")


def demo_complete_workflow():
    """Demo 4: Complete workflow example"""
    print("\n" + "=" * 70)
    print("DEMO 4: Complete Workflow")
    print("=" * 70)
    print()

    # Step 1: Generate sample data with different variants
    print("Step 1: Generate sample data for small complex copybook")
    schema_file = 'output/TRANSACTION-RECORD_CARD-INFO.json'
    data_file = 'transactions.dat'
    create_sample_data(schema_file, data_file, num_records=10)
    print()

    # Step 2: Parse with custom discriminator
    print("Step 2: Parse data with REDEFINES disambiguation")
    parser = CobolDataParser(schema_file)

    # Custom discriminator logic
    def smart_discriminator(record):
        """Determine variants based on business rules"""
        tx_data = record.get('TX-DATA', {})

        # Check if it looks like payment or refund
        # (In real scenario, would use actual discriminator field)
        return 'PAYMENT-DATA'  # Default to payment for demo

    parser.set_redefines_strategy(smart_discriminator)
    records = parser.parse_file(data_file, max_records=5)

    print(f"Parsed {len(records)} records")
    print()

    # Step 3: Process parsed data
    print("Step 3: Extract business information")
    for i, record in enumerate(records, 1):
        tx_header = record.get('TX-HEADER', {})
        print(f"Transaction {i}:")
        print(f"  ID: {tx_header.get('TX-ID')}")
        print(f"  Date: {tx_header.get('TX-DATE')}")
        print(f"  Type: {tx_header.get('TX-TYPE')}")
        print(f"  Status: {tx_header.get('TX-STATUS')}")

    print()

    # Step 4: Show JSON export capability
    print("Step 4: Export to JSON")
    output_file = 'parsed_transactions.json'
    with open(output_file, 'w') as f:
        json.dump(records, f, indent=2)
    print(f"Exported {len(records)} records to {output_file}")


def demo_recommended_approach():
    """Demo 5: Recommended approach for production"""
    print("\n" + "=" * 70)
    print("DEMO 5: Recommended Production Approach")
    print("=" * 70)
    print()

    print("Recommended Strategy: DISCRIMINATOR FIELD")
    print("-" * 70)
    print()

    print("Why Discriminator?")
    print("  ✓ Fast: O(1) lookup, no trial-and-error")
    print("  ✓ Deterministic: Same input → same output")
    print("  ✓ Business-aligned: Matches how COBOL programs work")
    print("  ✓ Maintainable: Clear mapping in code")
    print()

    print("Implementation Steps:")
    print()

    print("1. Identify discriminator fields in your copybook:")
    print("""
   For test_small_complex.cpy:
     - TX-TYPE determines PAYMENT-DATA vs REFUND-DATA
     - CUSTOMER-TYPE determines INDIVIDUAL-INFO vs BUSINESS-INFO
     - PAYMENT-METHOD determines CARD-INFO vs ACH-INFO
     - VERIFICATION-STATUS determines VERIFIED-DATA vs GUEST-DATA
""")

    print("2. Define discriminator function:")
    print("""
    def determine_variant(record, field_group):
        if field_group == 'TX-DATA':
            tx_type = record['TX-HEADER']['TX-TYPE'].strip()
            return 'PAYMENT-DATA' if tx_type in ['01', '02'] else 'REFUND-DATA'

        elif field_group == 'CUSTOMER-INFO':
            cust_type = record['CUSTOMER-TYPE'].strip()
            return 'INDIVIDUAL-INFO' if cust_type == 'I' else 'BUSINESS-INFO'

        # Add more groups as needed
        return 'default'
""")

    print("3. Apply to parser:")
    print("""
    parser = CobolDataParser(schema_file)
    parser.set_redefines_strategy(determine_variant)
    records = parser.parse_file(data_file)
""")

    print("4. Handle edge cases:")
    print("""
    - Missing discriminator: Use default variant
    - Invalid value: Log warning, use default
    - Partial data: Best-effort parsing
""")


if __name__ == '__main__':
    print("\n" + "=" * 70)
    print("COBOL DATA PARSER - DEMONSTRATION")
    print("=" * 70)
    print()

    # Run all demos
    demo_basic_parsing()
    print("\n" + "=" * 70 + "\n")

    demo_redefines_discriminator()
    print("\n" + "=" * 70 + "\n")

    demo_redefines_options()
    print("\n" + "=" * 70 + "\n")

    demo_complete_workflow()
    print("\n" + "=" * 70 + "\n")

    demo_recommended_approach()

    print("\n" + "=" * 70)
    print("DEMO COMPLETE")
    print("=" * 70)
    print()
    print("Next steps:")
    print("  1. Review generated sample_transactions.dat")
    print("  2. Review parsed_transactions.json")
    print("  3. Modify discriminator logic for your data")
    print("  4. Test with real data files")
    print()
