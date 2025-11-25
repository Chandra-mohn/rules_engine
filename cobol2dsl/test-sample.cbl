       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER        PIC 9(3) VALUE 0.
       01 TOTAL          PIC 9(5) VALUE 0.
       01 STATUS-CODE    PIC X(2).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 5 TO COUNTER.
           ADD 10 TO COUNTER.

           EVALUATE STATUS-CODE
               WHEN '00'
                   MOVE 100 TO TOTAL
               WHEN '10'
                   MOVE 200 TO TOTAL
               WHEN OTHER
                   MOVE 999 TO TOTAL
           END-EVALUATE.

           STOP RUN.
