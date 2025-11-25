       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTFILE-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ACCTFILE-STATUS    PIC X(2).
       01 APPL-RESULT        PIC 9(2).
       01 ACCOUNT-RECORD     PIC X(100).
       01 ARR-ARRAY-REC      PIC X(100).
       01 VBRC-REC1          PIC X(100).
       01 END-OF-FILE        PIC X(1).
       01 IO-STATUS          PIC X(2).
       88 APPL-AOK           VALUE '00'.
       88 APPL-EOF           VALUE '10'.

       PROCEDURE DIVISION.
       1000-ACCTFILE-GET-NEXT.
           READ ACCTFILE-FILE INTO ACCOUNT-RECORD.
           IF  ACCTFILE-STATUS = '00'
               MOVE 0 TO APPL-RESULT
               INITIALIZE ARR-ARRAY-REC
               PERFORM 1100-DISPLAY-ACCT-RECORD
               PERFORM 1300-POPUL-ACCT-RECORD
               PERFORM 1350-WRITE-ACCT-RECORD
               PERFORM 1400-POPUL-ARRAY-RECORD
               PERFORM 1450-WRITE-ARRY-RECORD
               INITIALIZE VBRC-REC1
               PERFORM 1500-POPUL-VBRC-RECORD
               PERFORM 1550-WRITE-VB1-RECORD
               PERFORM 1575-WRITE-VB2-RECORD
           ELSE
               IF  ACCTFILE-STATUS = '10'
                   MOVE 16 TO APPL-RESULT
               ELSE
                   MOVE 12 TO APPL-RESULT
               END-IF
           END-IF
           IF  APPL-AOK
               CONTINUE
           ELSE
               IF  APPL-EOF
                   MOVE 'Y' TO END-OF-FILE
               ELSE
                   DISPLAY 'ERROR READING ACCOUNT FILE'
                   MOVE ACCTFILE-STATUS TO IO-STATUS
                   PERFORM 9910-DISPLAY-IO-STATUS
                   PERFORM 9999-ABEND-PROGRAM
               END-IF
           END-IF
           EXIT.
           STOP RUN.
