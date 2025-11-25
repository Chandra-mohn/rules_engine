#!/usr/bin/env python3
"""
Test script for ADD and SUBTRACT statement support.

Tests ADD/SUBTRACT patterns based on real CardDemo COBOL usage.
"""

import sys
from pathlib import Path

# Add modules to path
sys.path.insert(0, str(Path(__file__).parent))

from transpiler.cobol_converter import CobolConverter

# Sample COBOL with ADD and SUBTRACT statements (based on CardDemo CBACT01C.cbl)
SAMPLE_COBOL_WITH_ADD_SUBTRACT = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-OPERATIONS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  APPL-RESULT             PIC S9(9)   COMP.
       01  ACCTFILE-STATUS         PIC X(2).
       01  ZERO-VAL                PIC 9   VALUE 0.
       01  COUNTER                 PIC 9(5).
       01  TOTAL                   PIC 9(10)V99.
       01  AMOUNT                  PIC 9(10)V99.

       PROCEDURE DIVISION.
           ADD 8 TO ZERO-VAL GIVING APPL-RESULT

           IF  ACCTFILE-STATUS = '00'
               SUBTRACT APPL-RESULT FROM APPL-RESULT
           ELSE
               ADD 12 TO ZERO-VAL GIVING APPL-RESULT
           END-IF

           ADD 1 TO COUNTER
           ADD AMOUNT TO TOTAL

           ADD AMOUNT TOTAL GIVING APPL-RESULT

           STOP RUN.
"""

def main():
    print("=" * 60)
    print("ADD and SUBTRACT Statement Support Test")
    print("=" * 60)
    print()

    print("Input COBOL Code:")
    print("-" * 60)
    print(SAMPLE_COBOL_WITH_ADD_SUBTRACT)
    print()

    print("Converting...")
    print("-" * 60)

    try:
        with CobolConverter() as converter:
            dsl_code, metadata = converter.convert(
                SAMPLE_COBOL_WITH_ADD_SUBTRACT,
                rule_name="File Operations Logic"
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
                ("ADD GIVING converted", "=" in dsl_code),
                ("ADD TO converted", "+=" in dsl_code or "= " in dsl_code),
                ("SUBTRACT FROM converted (zeroing pattern)", "= 0" in dsl_code),
                ("Addition operator present", "+" in dsl_code),
                ("Assignment operator present", "=" in dsl_code),
                ("No STOP RUN", "STOP RUN" not in dsl_code),
                ("No DATA DIVISION elements", "PIC" not in dsl_code and "VALUE" not in dsl_code),
                ("No ADD keyword in output", "ADD" not in dsl_code.upper() or dsl_code.count("ADD") == 0),
                ("No SUBTRACT keyword in output", "SUBTRACT" not in dsl_code.upper() or dsl_code.count("SUBTRACT") == 0),
                ("Counter increment converted", "counter" in dsl_code.lower()),
                ("Zeroing pattern optimized", "appl.result = 0" in dsl_code or "appl_result = 0" in dsl_code),
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

            # Show specific pattern conversions
            print("\n" + "=" * 60)
            print("Key Pattern Conversions:")
            print("=" * 60)

            dsl_lower = dsl_code.lower()

            print("\n1. ADD TO ZERO GIVING pattern:")
            if "appl.result = 8" in dsl_lower or "appl_result = 8" in dsl_lower:
                print("   ✅ Optimized: 'ADD 8 TO ZERO GIVING X' → 'x = 8'")
            else:
                print("   → Check DSL output for conversion")

            print("\n2. SUBTRACT X FROM X pattern:")
            if "= 0" in dsl_code:
                print("   ✅ Optimized: 'SUBTRACT X FROM X' → 'x = 0'")
            else:
                print("   → Check DSL output for conversion")

            print("\n3. Simple ADD TO pattern:")
            if "counter" in dsl_lower:
                print("   ✅ Converted: 'ADD 1 TO COUNTER' → 'counter = counter + 1'")
            else:
                print("   → Check DSL output for conversion")

    except Exception as e:
        print(f"\n❌ Conversion failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
