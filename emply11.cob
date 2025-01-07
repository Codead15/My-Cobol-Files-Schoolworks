      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
             IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPREP-TXT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-IN ASSIGN TO "EMPREC.TXT".
           SELECT EMP-OUT ASSIGN TO "EMPREP.TXT".
       DATA DIVISION.
       FILE SECTION.
       FD  EMP-IN
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS EMP-LIST.
       01  EMP-LIST.
           05 EMPNO PIC X(10).
           05 EMPNA PIC X(20).
           05 DC PIC X(5).
       FD  EMP-OUT.
       01  REP-OUT.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.
       01  HEADER1.
           05 FILLER PIC X(21).
           05 FILLER PIC X(24) VALUE 'RIVERDALE INTERNATIONAL'.
           05 FILLER PIC X(14) VALUE 'INDUSTRY, INC.'.
           05 FILLER PIC X(21).
       01  HEADER2.
           05 FILLER PIC X(26).
           05 FILLER PIC X(27) VALUE 'SAN JOSE DEL MONTE BULACAN'.
           05 FILLER PIC X(26).
       01  HEADER3.
           05 FILLER PIC X(32).
           05 FILLER PIC X(16) VALUE 'EMPLOYEE RECORDS'.
           05 FILLER PIC X(32).
       01  MIS-EMP.
           05 MISNO PIC X(10).
           05 FILLER PIC X(15).
           05 MISNA PIC X(20).
       01  PROD-EMP.
           05 PRODNO PIC X(10).
           05 FILLER PIC X(15).
           05 PRODNA PIC X(20).
       01  FIN-EMP.
           05 FINNO PIC X(10).
           05 FILLER PIC X(15).
           05 FINNA PIC X(20).
       01  MKTG-EMP.
           05 MKTGNO PIC X(10).
           05 FILLER PIC X(15).
           05 MKTGNA PIC X(20).
       01  MIS.
           05 FILLER PIC X(3).
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF EMPLOYEE: '.
           05 T-MIS PIC 99.
       01  PROD.
           05 FILLER PIC X(3).
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF EMPLOYEE: '.
           05 T-PROD PIC 99.
       01  FIN.
           05 FILLER PIC X(3).
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF EMPLOYEE: '.
           05 T-FIN PIC 99.
       01  MKTG.
           05 FILLER PIC X(3).
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF EMPLOYEE: '.
           05 T-MKTG PIC 99.
       01  BLNKSCR.
           05 FILLER PIC X(80).
       01  OTHERS.
           05 EOF PIC X(3) VALUE 'NO'.
           05 EOF1 PIC X(3) VALUE 'NO'.
           05 EOF2 PIC X(3) VALUE 'NO'.
           05 EOF3 PIC X(3) VALUE 'NO'.
           05 TOT-MIS PIC 999 VALUE 0.
           05 TOT-PROD PIC 999 VALUE 0.
           05 TOT-FIN PIC 999 VALUE 0.
           05 TOT-MKTG PIC 999 VALUE 0.
           05 GRAN-EMP PIC 9999 VALUE 0.
       01  EMPLOYREC.
           05 EMPNUM PIC X(10).
           05 EMPNAME PIC X(20).
           05 DEPARTMENT PIC A(5).
       SCREEN SECTION.
       01  SCR.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN INPUT EMP-IN
                OUTPUT EMP-OUT.
           DISPLAY DC.
           WRITE REP-OUT FROM HEADER1.
           WRITE REP-OUT FROM HEADER2.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM HEADER3.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM BLNKSCR.
           READ EMP-IN AT END MOVE 'Y' TO EOF.
           MOVE 'INFORMATION MANAGEMENT' TO MIS
           WRITE REP-OUT FROM MIS.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM T-MIS.
           PERFORM MIS-READRECORD-RTN UNTIL EOF IS EQUAL TO 'Y'
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM GRAN-EMP
           WRITE REP-OUT FROM BLNKSCR.
           CLOSE EMP-IN.
           OPEN INPUT EMP-IN.
           READ EMP-IN AT END MOVE 'Y' TO EOF1.
           MOVE 'PRODUCTION & SALES' TO PROD
           WRITE REP-OUT FROM PROD.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM T-PROD.
           PERFORM PROD-READRECORD-RTN UNTIL EOF1 IS EQUAL TO 'Y'
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM GRAN-EMP.
           WRITE REP-OUT FROM BLNKSCR.
           CLOSE EMP-IN.
           OPEN INPUT EMP-IN.
           READ EMP-IN AT END MOVE 'Y' TO EOF2.
           MOVE 'FINANCE' TO FIN
           WRITE REP-OUT FROM FIN.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM T-FIN.
           PERFORM FIN-READRECORD-RTN UNTIL EOF2 IS EQUAL TO 'Y'
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM GRAN-EMP.
           WRITE REP-OUT FROM BLNKSCR.
           CLOSE EMP-IN.
           OPEN INPUT EMP-IN.
           READ EMP-IN AT END MOVE 'Y' TO EOF3.
           MOVE 'MARKETING' TO MKTG
           WRITE REP-OUT FROM MKTG.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM T-MKTG.
           PERFORM MKTG-READRECORD-RTN UNTIL EOF3 IS EQUAL TO 'Y'
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM GRAN-EMP.
           WRITE REP-OUT FROM BLNKSCR.
           PERFORM FINAL-MOVE-RTN
           CLOSE EMP-IN, EMP-OUT.
           STOP RUN.

       MIS-READRECORD-RTN.
           PERFORM DISPLAY-RTN.
           IF DC = 'MIS'
           MOVE EMPNO TO EMPNUM.
           MOVE EMPNA TO EMPNAME.
           WRITE REP-OUT FROM EMPLOYREC
           ADD 1 TO TOT-MIS
           ADD 1 TO GRAN-EMP
           MOVE TOT-MIS TO T-MIS
           READ EMP-IN AT END MOVE 'YES' TO EOF.

       PROD-READRECORD-RTN.
           PERFORM DISPLAY-RTN.
           IF DC = 'PROD'
           MOVE EMPNO TO EMPNUM.
           MOVE EMPNA TO EMPNAME.
           WRITE REP-OUT FROM EMPLOYREC
           ADD 1 TO TOT-PROD
           ADD 1 TO GRAN-EMP
           MOVE TOT-PROD TO T-PROD
           READ EMP-IN AT END MOVE 'YES' TO EOF.

       FIN-READRECORD-RTN.
           PERFORM DISPLAY-RTN.
           IF DC = 'FIN'
           MOVE EMPNO TO EMPNUM.
           MOVE EMPNA TO EMPNAME.
           WRITE REP-OUT FROM EMPLOYREC
           ADD 1 TO TOT-FIN
           ADD 1 TO GRAN-EMP
           MOVE TOT-FIN TO T-FIN
           READ EMP-IN AT END MOVE 'YES' TO EOF.

       MKTG-READRECORD-RTN.
           PERFORM DISPLAY-RTN.
           IF DC = 'MKTG'
           MOVE EMPNO TO EMPNUM.
           MOVE EMPNA TO EMPNAME.
           WRITE REP-OUT FROM EMPLOYREC
           ADD 1 TO TOT-MKTG
           ADD 1 TO GRAN-EMP
           MOVE TOT-MKTG TO T-MKTG
           READ EMP-IN AT END MOVE 'YES' TO EOF.


       DISPLAY-RTN.
           MOVE EMPNO TO EMPNUM.
           MOVE EMPNA TO EMPNAME.
           MOVE DC TO DEPARTMENT.
           WRITE REP-OUT.
           DISPLAY REP-OUT.

       FINAL-MOVE-RTN.
           MOVE TOT-MIS TO T-MIS.
           MOVE TOT-PROD TO T-PROD.
           MOVE TOT-FIN TO T-FIN.
           MOVE TOT-MKTG TO T-MKTG.
           WRITE REP-OUT FROM MIS.
           WRITE REP-OUT FROM PROD.
           WRITE REP-OUT FROM FIN.
           WRITE REP-OUT FROM MKTG.

       FINAL-DISPLAY-RTN.
           DISPLAY REP-OUT.
           READ EMP-OUT AT END MOVE 'Y' TO EOF.
