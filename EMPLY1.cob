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
       FD EMP-IN
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 35 CHARACTERS
           DATA RECORD IS EMPLOY-LIST.
       01 EMPLOY-LIST.
           05 DC PIC X(5).
           05 EMPNO PIC X(10).
           05 EMPNA PIC X(20).
       FD EMP-OUT.
       01 REP-OUT.
           05 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01 HDR1.
           05 FILLER PIC X(21).
           05 FILLER PIC X(24) VALUE 'RIVERDALE INTERNATIONAL '.
           05 FILLER PIC X(14) VALUE 'INDUSTRY, INC.'.
           05 FILLER PIC X(21).
       01 HDR2.
           05 FILLER PIC X(26).
           05 FILLER PIC X(27) VALUE 'SAN JOSE DEL MONTE, BULACAN'.
           05 FILLER PIC X(27).
       01 HDR3.
           05 FILLER PIC X(32).
           05 FILLER PIC X(16) VALUE 'EMPLOYEE RECORDS'.
           05 FILLER PIC X(32).
       01 HDRM.
           05 FILLER PIC X(17) VALUE 'DEPARTMENT NAME: '.
           05 MIS-NA PIC X(22).
           05 FILLER PIC X(41).
       01 HDRP.
           05 FILLER PIC X(17) VALUE 'DEPARTMENT NAME: '.
           05 PROD-NA PIC X(18).
           05 FILLER PIC X(45).
       01 HDRF.
           05 FILLER PIC X(17) VALUE 'DEPARTMENT NAME: '.
           05 FIN-NA PIC X(7).
           05 FILLER PIC X(56).
       01 HDRMK.
           05 FILLER PIC X(17) VALUE 'DEPARTMENT NAME: '.
           05 MKTG-NA PIC X(9).
           05 FILLER PIC X(54).
       01 HDR4.
           05 FILLER PIC X(15) VALUE 'EMPLOYEE NUMBER'.
           05 FILLER PIC X(10).
           05 FILLER PIC X(13) VALUE 'EMPLOYEE NAME'.
           05 FILLER PIC X(42).
       01 MIS-EMP.
           05 MISNO PIC X(10).
           05 FILLER PIC X(15).
           05 MISNA PIC X(20).
       01 PROD-EMP.
           05 PRODNO PIC X(10).
           05 FILLER PIC X(15).
           05 PRODNA PIC X(20).
       01 FIN-EMP.
           05 FINNO PIC X(10).
           05 FILLER PIC X(15).
           05 FINNA PIC X(20).
       01 MKTG-EMP.
           05 MKTGNO PIC X(10).
           05 FILLER PIC X(15).
           05 MKTGNA PIC X(20).
       01 MIS.
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF EMPLOYEE: '.
           05 T-MIS PIC Z99.
       01 PROD.
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF EMPLOYEE: '.
           05 T-PROD PIC Z99.
       01 FIN.
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF EMPLOYEE: '.
           05 T-FIN PIC Z99.
       01 MKTG.
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF EMPLOYEE: '.
           05 T-MKTG PIC Z99.
       01 GRAN-EMP.
           05 FILLER PIC X(32) VALUE 'GRAND TOTAL NUMBER OF EMPLOYEES:'.
           05 FILLER PIC X.
           05 GRAN-NO PIC Z999.
       01 BLNKSCR.
           05 FILLER PIC X(80).
       01 OTHERS.
           05 EOF PIC X(3) VALUE 'N0'.
           05 EOF1 PIC X(3) VALUE 'N0'.
           05 EOF2 PIC X(3) VALUE 'N0'.
           05 EOF3 PIC X(3) VALUE 'N0'.
           05 MIS-NUM PIC 999 VALUE 0.
           05 PROD-NUM PIC 999 VALUE 0.
           05 FIN-NUM PIC 999 VALUE 0.
           05 MKTG-NUM PIC 999 VALUE 0.
           05 GRAN-NUM PIC 9999 VALUE 0.
       SCREEN SECTION.
       01 SCR.
           05 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN INPUT EMP-IN
               OUTPUT EMP-OUT.
           DISPLAY SCR.
           WRITE REP-OUT FROM HDR1.
           WRITE REP-OUT FROM HDR2.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM HDR3.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM BLNKSCR.
           PERFORM MIS-PRCS-RTN.
           CLOSE EMP-IN.
           OPEN INPUT EMP-IN.
           PERFORM PROD-PRCS-RTN.
           CLOSE EMP-IN.
           OPEN INPUT EMP-IN.
           PERFORM FIN-PRCS-RTN.
           CLOSE EMP-IN.
           OPEN INPUT EMP-IN.
           PERFORM MKTG-PRCS-RTN.
           PERFORM FINAL-RTN.
           DISPLAY 'DATA RECORDED SUCCESSFULLY!'.
           CLOSE EMP-IN, EMP-OUT.
           STOP RUN.
       MIS-PRCS-RTN.
           READ EMP-IN AT END MOVE 'YES' TO EOF.
           MOVE 'INFORMATION MANAGEMENT' TO MIS-NA
           WRITE REP-OUT FROM HDRM.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM HDR4.
           PERFORM MIS-BRK-RTN UNTIL EOF IS EQUAL TO 'YES'.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM MIS.
           WRITE REP-OUT FROM BLNKSCR.
       MIS-BRK-RTN.
           IF DC = 'MIS  '
               MOVE EMPNO TO MISNO
               MOVE EMPNA TO MISNA
               ADD 1 TO MIS-NUM
               ADD 1 TO GRAN-NUM
               MOVE MIS-NUM TO T-MIS
               MOVE PROD-NUM TO T-PROD
               MOVE FIN-NUM TO T-FIN
               MOVE MKTG-NUM TO T-MKTG
               MOVE GRAN-NUM TO GRAN-NO
               WRITE REP-OUT FROM MIS-EMP.
           READ EMP-IN AT END MOVE 'YES' TO EOF.
       PROD-PRCS-RTN.
           READ EMP-IN AT END MOVE 'YES' TO EOF1.
           MOVE 'PRODUCTION & SALES' TO PROD-NA
           WRITE REP-OUT FROM HDRP.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM HDR4.
           PERFORM PROD-BRK-RTN UNTIL EOF1 IS EQUAL TO 'YES'.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM PROD.
           WRITE REP-OUT FROM BLNKSCR.
       PROD-BRK-RTN.
           IF DC = 'PROD '
               MOVE EMPNO TO PRODNO
               MOVE EMPNA TO PRODNA
               ADD 1 TO PROD-NUM
               ADD 1 TO GRAN-NUM
               MOVE MIS-NUM TO T-MIS
               MOVE PROD-NUM TO T-PROD
               MOVE FIN-NUM TO T-FIN
               MOVE MKTG-NUM TO T-MKTG
               MOVE GRAN-NUM TO GRAN-NO
               WRITE REP-OUT FROM PROD-EMP.
           READ EMP-IN AT END MOVE 'YES' TO EOF1.
       FIN-PRCS-RTN.
           READ EMP-IN AT END MOVE 'YES' TO EOF2.
           MOVE 'FINANCE' TO FIN-NA
           WRITE REP-OUT FROM HDRF.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM HDR4.
           PERFORM FIN-BRK-RTN UNTIL EOF2 IS EQUAL TO 'YES'.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM FIN.
           WRITE REP-OUT FROM BLNKSCR.
       FIN-BRK-RTN.
           IF DC = 'FIN  '
               MOVE EMPNO TO FINNO
               MOVE EMPNA TO FINNA
               ADD 1 TO FIN-NUM
               ADD 1 TO GRAN-NUM
               MOVE MIS-NUM TO T-MIS
               MOVE PROD-NUM TO T-PROD
               MOVE FIN-NUM TO T-FIN
               MOVE MKTG-NUM TO T-MKTG
               MOVE GRAN-NUM TO GRAN-NO
               WRITE REP-OUT FROM FIN-EMP.
           READ EMP-IN AT END MOVE 'YES' TO EOF2.
       MKTG-PRCS-RTN.
           READ EMP-IN AT END MOVE 'YES' TO EOF3.
           MOVE 'MARKETING' TO MKTG-NA
           WRITE REP-OUT FROM HDRMK.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM HDR4.
           PERFORM MKTG-BRK-RTN UNTIL EOF3 IS EQUAL TO 'YES'.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM MKTG.
           WRITE REP-OUT FROM BLNKSCR.
       MKTG-BRK-RTN.
           IF DC = 'MKTG '
               MOVE EMPNO TO  MKTGNO
               MOVE EMPNA TO MKTGNA
               ADD 1 TO MKTG-NUM
               ADD 1 TO GRAN-NUM
               MOVE MIS-NUM TO T-MIS
               MOVE PROD-NUM TO T-PROD
               MOVE FIN-NUM TO T-FIN
               MOVE MKTG-NUM TO T-MKTG
               MOVE GRAN-NUM TO GRAN-NO
               WRITE REP-OUT FROM MKTG-EMP.
           READ EMP-IN AT END MOVE 'YES' TO EOF3.
       FINAL-RTN.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM BLNKSCR.
           WRITE REP-OUT FROM GRAN-EMP.
