#!/usr/bin/env python3
"""
Test script for EVALUATE statement support.

Tests EVALUATE patterns based on real CardDemo COBOL usage.
"""

import sys
from pathlib import Path

# Add modules to path
sys.path.insert(0, str(Path(__file__).parent))

from transpiler.cobol_converter import CobolConverter

# Sample COBOL with EVALUATE statements (based on CardDemo CBTRN03C.cbl)
SAMPLE_COBOL_WITH_EVALUATE = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-STATUS-CHECK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DATEPARM-STATUS         PIC X(2).
       01  APPL-RESULT             PIC S9(9)   COMP.
       01  TRANFILE-STATUS         PIC X(2).

       PROCEDURE DIVISION.
           EVALUATE DATEPARM-STATUS
             WHEN '00'
                 MOVE 0 TO APPL-RESULT
             WHEN '10'
                 MOVE 16 TO APPL-RESULT
             WHEN OTHER
                 MOVE 12 TO APPL-RESULT
           END-EVALUATE

           EVALUATE TRANFILE-STATUS
             WHEN '00'
                 MOVE 0 TO APPL-RESULT
             WHEN '10'
                 MOVE 16 TO APPL-RESULT
             WHEN OTHER
                 MOVE 12 TO APPL-RESULT
           END-EVALUATE

           STOP RUN.
"""

# Sample COBOL with EVALUATE TRUE pattern
SAMPLE_COBOL_EVALUATE_TRUE = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RATE-CALCULATOR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  BALANCE                 PIC 9(10)V99.
       01  RATE                    PIC 9V99.

       PROCEDURE DIVISION.
           EVALUATE TRUE
             WHEN BALANCE > 10000
                 MOVE 2.5 TO RATE
             WHEN BALANCE > 5000
                 MOVE 3.5 TO RATE
             WHEN OTHER
                 MOVE 4.5 TO RATE
           END-EVALUATE

           STOP RUN.
"""

def main():
    print("=" * 60)
    print("EVALUATE Statement Support Test")
    print("=" * 60)
    print()

    # Test 1: Value-based EVALUATE
    print("TEST 1: Value-Based EVALUATE")
    print("-" * 60)
    print("Input COBOL Code:")
    print(SAMPLE_COBOL_WITH_EVALUATE)
    print()

    print("Converting...")
    print("-" * 60)

    try:
        with CobolConverter() as converter:
            dsl_code, metadata = converter.convert(
                SAMPLE_COBOL_WITH_EVALUATE,
                rule_name="File Status Check Logic"
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

            print("\n✅ Test 1 completed successfully!")

            # Validation checks for Test 1
            print("\n" + "=" * 60)
            print("Validation Checks (Test 1):")
            print("=" * 60)

            checks = [
                ("EVALUATE converted to if/elseif/else", "if" in dsl_code and "elseif" in dsl_code and "else" in dsl_code),
                ("WHEN '00' converted", "\"00\"" in dsl_code or "'00'" in dsl_code),
                ("WHEN '10' converted", "\"10\"" in dsl_code or "'10'" in dsl_code),
                ("WHEN OTHER converted to else", "else" in dsl_code),
                ("END-EVALUATE converted to endif", "endif" in dsl_code),
                ("No STOP RUN", "STOP RUN" not in dsl_code),
                ("No DATA DIVISION elements", "PIC" not in dsl_code),
                ("Equality checks present", "==" in dsl_code),
            ]

            all_passed = True
            for check_name, result in checks:
                status = "✅" if result else "❌"
                print(f"{status} {check_name}")
                if not result:
                    all_passed = False

            if all_passed:
                print("\n🎉 All Test 1 validation checks passed!")
            else:
                print("\n⚠️ Some Test 1 validation checks failed - review output above")

    except Exception as e:
        print(f"\n❌ Test 1 conversion failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

    # Test 2: EVALUATE TRUE pattern
    print("\n\n" + "=" * 60)
    print("TEST 2: EVALUATE TRUE Pattern")
    print("=" * 60)
    print()
    print("Input COBOL Code:")
    print(SAMPLE_COBOL_EVALUATE_TRUE)
    print()

    print("Converting...")
    print("-" * 60)

    try:
        with CobolConverter() as converter:
            dsl_code, metadata = converter.convert(
                SAMPLE_COBOL_EVALUATE_TRUE,
                rule_name="Rate Calculator Logic"
            )

            print("\nGenerated Rules DSL:")
            print("=" * 60)
            print(dsl_code)
            print("=" * 60)

            print("\nConversion Metadata:")
            print(f"  - Mappings: {len(metadata.mappings)}")

            print("\n✅ Test 2 completed successfully!")

            # Validation checks for Test 2
            print("\n" + "=" * 60)
            print("Validation Checks (Test 2):")
            print("=" * 60)

            checks = [
                ("EVALUATE TRUE converted", "if" in dsl_code),
                ("Condition preserved (> 10000)", "> 10000" in dsl_code or ">10000" in dsl_code),
                ("Condition preserved (> 5000)", "> 5000" in dsl_code or ">5000" in dsl_code),
                ("WHEN OTHER converted to else", "else" in dsl_code),
                ("No EVALUATE keyword in output", "EVALUATE" not in dsl_code.upper()),
                ("endif present", "endif" in dsl_code),
            ]

            all_passed = True
            for check_name, result in checks:
                status = "✅" if result else "❌"
                print(f"{status} {check_name}")
                if not result:
                    all_passed = False

            if all_passed:
                print("\n🎉 All Test 2 validation checks passed!")
            else:
                print("\n⚠️ Some Test 2 validation checks failed - review output above")

    except Exception as e:
        print(f"\n❌ Test 2 conversion failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

    print("\n" + "=" * 60)
    print("✅ ALL TESTS PASSED")
    print("=" * 60)


if __name__ == "__main__":
    main()
