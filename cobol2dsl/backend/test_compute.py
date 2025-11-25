#!/usr/bin/env python3
"""
Test script for COMPUTE statement support.

Tests arithmetic expressions based on real CardDemo COBOL patterns.
"""

import sys
from pathlib import Path

# Add modules to path
sys.path.insert(0, str(Path(__file__).parent))

from transpiler.cobol_converter import CobolConverter

# Sample COBOL with COMPUTE statements (based on CardDemo CBACT04C.cbl)
SAMPLE_COBOL_WITH_COMPUTE = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST-CALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TRAN-CAT-BAL            PIC 9(10)V99.
       01  DIS-INT-RATE            PIC 9V99.
       01  WS-MONTHLY-INT          PIC 9(10)V99.
       01  WS-TOTAL-INT            PIC 9(12)V99.
       01  WS-ACCOUNT-BALANCE      PIC 9(10)V99.
       01  WS-FEE                  PIC 9(5)V99.

       PROCEDURE DIVISION.
           IF TRAN-CAT-BAL > 0
              COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200
              COMPUTE WS-TOTAL-INT = WS-TOTAL-INT + WS-MONTHLY-INT
              COMPUTE WS-ACCOUNT-BALANCE = TRAN-CAT-BAL - WS-FEE
              PERFORM WRITE-TRANSACTION
           ELSE
              PERFORM SKIP-TRANSACTION
           END-IF
           STOP RUN.

       WRITE-TRANSACTION.
           DISPLAY 'Writing transaction'.

       SKIP-TRANSACTION.
           DISPLAY 'Skipping transaction'.
"""

def main():
    print("=" * 60)
    print("COMPUTE Statement Support Test")
    print("=" * 60)
    print()

    print("Input COBOL Code:")
    print("-" * 60)
    print(SAMPLE_COBOL_WITH_COMPUTE)
    print()

    print("Converting...")
    print("-" * 60)

    try:
        with CobolConverter() as converter:
            dsl_code, metadata = converter.convert(
                SAMPLE_COBOL_WITH_COMPUTE,
                rule_name="Interest Calculation Logic"
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

            # Validation checks
            print("\n" + "=" * 60)
            print("Validation Checks:")
            print("=" * 60)

            checks = [
                ("COMPUTE with division converted", "/" in dsl_code),
                ("COMPUTE with multiplication converted", "*" in dsl_code),
                ("COMPUTE with addition converted", "+" in dsl_code or "ws.total_int" in dsl_code.lower()),
                ("COMPUTE with subtraction converted", "-" in dsl_code or "ws.account_balance" in dsl_code.lower()),
                ("Parentheses preserved", "(" in dsl_code and ")" in dsl_code),
                ("No DISPLAY statements", "DISPLAY" not in dsl_code),
                ("No STOP RUN", "STOP RUN" not in dsl_code),
                ("No DATA DIVISION elements", "PIC" not in dsl_code),
            ]

            all_passed = True
            for check_name, result in checks:
                status = "✅" if result else "❌"
                print(f"{status} {check_name}")
                if not result:
                    all_passed = False

            if all_passed:
                print("\n🎉 All validation checks passed!")
            else:
                print("\n⚠️ Some validation checks failed - review output above")

    except Exception as e:
        print(f"\n❌ Conversion failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
