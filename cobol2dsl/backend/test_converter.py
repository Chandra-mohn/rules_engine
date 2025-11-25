#!/usr/bin/env python3
"""
Test script for COBOL to DSL converter.

Tests the conversion pipeline with sample COBOL code.
"""

import sys
from pathlib import Path

# Add modules to path
sys.path.insert(0, str(Path(__file__).parent))

from transpiler.cobol_converter import CobolConverter

# Sample COBOL code (full program structure)
SAMPLE_COBOL = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-APPROVAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-TYPE         PIC X(10).
       01 BALANCE               PIC 9(10)V99.

       PROCEDURE DIVISION.
           IF CUSTOMER-TYPE = 'PREMIUM' AND BALANCE > 10000
              PERFORM APPROVE-PREMIUM
           ELSE
              IF BALANCE > 5000
                 PERFORM APPROVE-STANDARD
              ELSE
                 PERFORM REJECT-APPLICATION
              END-IF
           END-IF
           STOP RUN.

       APPROVE-PREMIUM.
           DISPLAY 'Approved Premium'.

       APPROVE-STANDARD.
           DISPLAY 'Approved Standard'.

       REJECT-APPLICATION.
           DISPLAY 'Rejected'.
"""

def main():
    print("=" * 60)
    print("COBOL to Rules DSL Converter - Test")
    print("=" * 60)
    print()

    print("Input COBOL Code:")
    print("-" * 60)
    print(SAMPLE_COBOL)
    print()

    print("Converting...")
    print("-" * 60)

    try:
        with CobolConverter() as converter:
            dsl_code, metadata = converter.convert(
                SAMPLE_COBOL,
                rule_name="Customer Approval Logic"
            )

            print("\nGenerated Rules DSL:")
            print("=" * 60)
            print(dsl_code)
            print("=" * 60)

            print("\nConversion Metadata:")
            print(f"  - Mappings: {len(metadata.mappings)}")
            for mapping in metadata.mappings:
                print(f"    • {mapping['cobol']} → {mapping['target']} ({mapping['type']}, conf: {mapping['confidence']})")

            if metadata.warnings:
                print(f"\n  - Warnings: {len(metadata.warnings)}")
                for warning in metadata.warnings:
                    print(f"    ⚠️ {warning}")

            print("\n✅ Conversion completed successfully!")

    except Exception as e:
        print(f"\n❌ Conversion failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
