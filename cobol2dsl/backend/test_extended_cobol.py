#!/usr/bin/env python3
"""
Test script for extended COBOL support including MOVE statements.

Tests MOVE, IF, nested conditions, and action calls.
"""

import sys
from pathlib import Path

# Add modules to path
sys.path.insert(0, str(Path(__file__).parent))

from transpiler.cobol_converter import CobolConverter

# Sample COBOL with MOVE statements
SAMPLE_COBOL_WITH_MOVE = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOAN-PROCESSING.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CREDIT-SCORE          PIC 9(3).
       01 LOAN-AMOUNT           PIC 9(10)V99.
       01 APPROVAL-STATUS       PIC X(20).
       01 INTEREST-RATE         PIC 9V99.

       PROCEDURE DIVISION.
           IF CREDIT-SCORE > 750 AND LOAN-AMOUNT < 500000
              MOVE 'APPROVED' TO APPROVAL-STATUS
              MOVE 3.25 TO INTEREST-RATE
              PERFORM SEND-APPROVAL-LETTER
           ELSE
              IF CREDIT-SCORE > 650 AND LOAN-AMOUNT < 250000
                 MOVE 'CONDITIONAL' TO APPROVAL-STATUS
                 MOVE 5.75 TO INTEREST-RATE
                 PERFORM SEND-CONDITIONAL-OFFER
              ELSE
                 MOVE 'REJECTED' TO APPROVAL-STATUS
                 PERFORM SEND-REJECTION-LETTER
              END-IF
           END-IF
           STOP RUN.

       SEND-APPROVAL-LETTER.
           DISPLAY 'Sending approval letter'.

       SEND-CONDITIONAL-OFFER.
           DISPLAY 'Sending conditional offer'.

       SEND-REJECTION-LETTER.
           DISPLAY 'Sending rejection letter'.
"""

def main():
    print("=" * 60)
    print("Extended COBOL to Rules DSL Converter - Test")
    print("=" * 60)
    print()

    print("Input COBOL Code:")
    print("-" * 60)
    print(SAMPLE_COBOL_WITH_MOVE)
    print()

    print("Converting...")
    print("-" * 60)

    try:
        with CobolConverter() as converter:
            dsl_code, metadata = converter.convert(
                SAMPLE_COBOL_WITH_MOVE,
                rule_name="Loan Processing Logic"
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
