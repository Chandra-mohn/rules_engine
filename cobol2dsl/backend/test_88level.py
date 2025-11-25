#!/usr/bin/env python3
"""
Test script for 88-level condition support.

Tests 88-level boolean conditions based on real CardDemo COBOL patterns.
"""

import sys
from pathlib import Path

# Add modules to path
sys.path.insert(0, str(Path(__file__).parent))

from transpiler.cobol_converter import CobolConverter

# Sample COBOL with 88-level conditions (based on CardDemo patterns)
SAMPLE_COBOL_WITH_88_LEVEL = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-PROCESSOR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  APPL-RESULT             PIC S9(9)   COMP.
           88  APPL-AOK            VALUE 0.
           88  APPL-EOF            VALUE 16.
       01  ERR-FLG                 PIC X.
           88  ERR-FLG-ON          VALUE 'Y'.
           88  ERR-FLG-OFF         VALUE 'N'.
       01  RECORD-COUNT            PIC 9(5).

       PROCEDURE DIVISION.
           IF APPL-AOK
              IF RECORD-COUNT > 100
                 PERFORM PROCESS-LARGE-BATCH
              ELSE
                 PERFORM PROCESS-SMALL-BATCH
              END-IF
           ELSE
              IF APPL-EOF
                 PERFORM CLOSE-FILES
              ELSE
                 IF ERR-FLG-ON
                    PERFORM HANDLE-ERROR
                 END-IF
              END-IF
           END-IF
           STOP RUN.

       PROCESS-LARGE-BATCH.
           DISPLAY 'Processing large batch'.

       PROCESS-SMALL-BATCH.
           DISPLAY 'Processing small batch'.

       CLOSE-FILES.
           DISPLAY 'Closing files'.

       HANDLE-ERROR.
           DISPLAY 'Handling error'.
"""

def main():
    print("=" * 60)
    print("88-Level Condition Support Test")
    print("=" * 60)
    print()

    print("Input COBOL Code:")
    print("-" * 60)
    print(SAMPLE_COBOL_WITH_88_LEVEL)
    print()

    print("Converting...")
    print("-" * 60)

    try:
        with CobolConverter() as converter:
            dsl_code, metadata = converter.convert(
                SAMPLE_COBOL_WITH_88_LEVEL,
                rule_name="File Processing with 88-Level Conditions"
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

            # Validate expected patterns
            print("\n" + "=" * 60)
            print("Validation Checks:")
            print("=" * 60)

            checks = [
                ("88-level APPL-AOK converted", "appl.ok" in dsl_code.lower() or "application.ok" in dsl_code.lower()),
                ("88-level APPL-EOF converted", "appl.eof" in dsl_code.lower() or "application.eof" in dsl_code.lower()),
                ("88-level ERR-FLG-ON converted", "err" in dsl_code.lower()),
                ("No DISPLAY statements", "DISPLAY" not in dsl_code),
                ("No STOP RUN", "STOP RUN" not in dsl_code),
                ("Nested IF structure preserved", "if" in dsl_code and "else" in dsl_code),
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
