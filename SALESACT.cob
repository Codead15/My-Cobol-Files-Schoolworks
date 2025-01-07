       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTIVITY-3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-IN ASSIGN TO "SALMAN.TXT".
           SELECT SALES-OUT ASSIGN TO "SALCOMM.TXT".
       DATA DIVISION.
       FILE SECTION.
       FD SALES-IN
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 38 CHARACTERS
           DATA RECORD IS SALESMANDETAILS.
       01 SALESMANDETAILS.
           05 ARCODE PIC A.
           05 SALESNUM PIC X(10).
           05 SALESNAME PIC X(20).
           05 SALES PIC 9(5)V99.
       FD SALES-OUT.
       01 REP-OUT.
           05 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01 HEADER1.
           05 FILLER PIC X(26).
           05 FILLER PIC X(28) VALUE 'Destined Sales, Incorporated'.
           05 FILLER PIC X(26).
       01 HEADER2.
           05 FILLER PIC X(28).
           05 FILLER PIC X(23) VALUE 'City of Carmona, Cavite'.
           05 FILLER PIC X(29).
       01 HEADER3.
           05 FILLER PIC X(34).
           05 FILLER PIC X(12) VALUE 'Sales Report'.
           05 FILLER PIC X(34).
       01 H-ALFONSO.
           05 FILLER PIC X(10) VALUE 'Area Name:'.
           05 FILLER PIC X(5).
           05 A-NAME PIC X(12).
           05 FILLER PIC X(53).
       01 H-BACOOR.
           05 FILLER PIC X(10) VALUE 'Area Name:'.
           05 FILLER PIC X(5).
           05 B-NAME PIC X(12).
           05 FILLER PIC X(53).
       01 H-IMUS.
           05 FILLER PIC X(10) VALUE 'Area Name:'.
           05 FILLER PIC X(5).
           05 I-NAME PIC X(12).
           05 FILLER PIC X(53).
       01 H-DASMARINAS.
           05 FILLER PIC X(10) VALUE 'Area Name:'.
           05 FILLER PIC X(5).
           05 D-NAME PIC X(12).
           05 FILLER PIC X(53).
       01 H-KAWIT.
           05 FILLER PIC X(10) VALUE 'Area Name:'.
           05 FILLER PIC X(5).
           05 K-NAME PIC X(12).
           05 FILLER PIC X(53).
       01 HEADER4.
           05 FILLER PIC X(15) VALUE 'Salesman Number'.
           05 FILLER PIC X(8).
           05 FILLER PIC X(13) VALUE 'Salesman Name'.
           05 FILLER PIC X(15).
           05 FILLER PIC X(11) VALUE 'Total Sales'.
           05 FILLER PIC X(8).
           05 FILLER PIC X(10) VALUE 'Commission'.
       01 SALESMAN-A.
           05 SALNUM-A PIC X(10).
           05 FILLER PIC X(13).
           05 SALNAME-A PIC X(20).
           05 FILLER PIC X(8).
           05 TOTSAL-A PIC Z9,999.99.
           05 FILLER PIC X(10).
           05 COMM-A PIC Z9,999.99.
       01 SALESMAN-B.
           05 SALNUM-B PIC X(10).
           05 FILLER PIC X(13).
           05 SALNAME-B PIC X(20).
           05 FILLER PIC X(8).
           05 TOTSAL-B PIC Z9,999.99.
           05 FILLER PIC X(10).
           05 COMM-B PIC Z9,999.99.
       01 SALESMAN-I.
           05 SALNUM-I PIC X(10).
           05 FILLER PIC X(13).
           05 SALNAME-I PIC X(20).
           05 FILLER PIC X(8).
           05 TOTSAL-I PIC Z9,999.99.
           05 FILLER PIC X(10).
           05 COMM-I PIC Z9,999.99.
       01 SALESMAN-D.
           05 SALNUM-D PIC X(10).
           05 FILLER PIC X(13).
           05 SALNAME-D PIC X(20).
           05 FILLER PIC X(8).
           05 TOTSAL-D PIC Z9,999.99.
           05 FILLER PIC X(10).
           05 COMM-D PIC Z9,999.99.
       01 SALESMAN-K.
           05 SALNUM-K PIC X(10).
           05 FILLER PIC X(13).
           05 SALNAME-K PIC X(20).
           05 FILLER PIC X(8).
           05 TOTSAL-K PIC Z9,999.99.
           05 FILLER PIC X(10).
           05 COMM-K PIC Z9,999.99.
       01 H-A-S.
           05 FILLER PIC X(24) VALUE 'Total Accumulated Sales '.
           05 FILLER PIC X(12) VALUE 'in Alfonso:'.
           05 FILLER PIC X(7).
           05 SALES-A PIC ZZZ,999.99.
       01 H-A-C.
           05 FILLER PIC X(29) VALUE 'Total Accumulated Commission '.
           05 FILLER PIC X(12) VALUE 'in Alfonso:'.
           05 FILLER PIC X(3).
           05 COMMS-A PIC Z9,999.99.
       01 H-A-M.
           05 FILLER PIC X(25) VALUE 'Total Number of Salesman '.
           05 FILLER PIC X(12) VALUE 'in Alfonso:'.
           05 FILLER PIC X(8).
           05 MEN-A PIC 99.
       01 H-B-S.
           05 FILLER PIC X(24) VALUE 'Total Accumulated Sales '.
           05 FILLER PIC X(12) VALUE 'in Bacoor:'.
           05 FILLER PIC X(7).
           05 SALES-B PIC ZZZ,999.99.
       01 H-B-C.
           05 FILLER PIC X(29) VALUE 'Total Accumulated Commission '.
           05 FILLER PIC X(12) VALUE 'in Bacoor:'.
           05 FILLER PIC X(3).
           05 COMMS-B PIC Z9,999.99.
       01 H-B-M.
           05 FILLER PIC X(25) VALUE 'Total Number of Salesman '.
           05 FILLER PIC X(12) VALUE 'in Bacoor:'.
           05 FILLER PIC X(8).
           05 MEN-B PIC 99.
       01 H-I-S.
           05 FILLER PIC X(24) VALUE 'Total Accumulated Sales '.
           05 FILLER PIC X(12) VALUE 'in Imus:'.
           05 FILLER PIC X(7).
           05 SALES-I PIC ZZZ,999.99.
       01 H-I-C.
           05 FILLER PIC X(29) VALUE 'Total Accumulated Commission '.
           05 FILLER PIC X(12) VALUE 'in Imus:'.
           05 FILLER PIC X(3).
           05 COMMS-I PIC Z9,999.99.
       01 H-I-M.
           05 FILLER PIC X(25) VALUE 'Total Number of Salesman '.
           05 FILLER PIC X(12) VALUE 'in Imus:'.
           05 FILLER PIC X(8).
           05 MEN-I PIC 99.
       01 H-D-S.
           05 FILLER PIC X(24) VALUE 'Total Accumulated Sales '.
           05 FILLER PIC X(15) VALUE 'in Dasmarinas:'.
           05 FILLER PIC X(7).
           05 SALES-D PIC ZZZ,999.99.
       01 H-D-C.
           05 FILLER PIC X(29) VALUE 'Total Accumulated Commission '.
           05 FILLER PIC X(15) VALUE 'in Dasmarinas:'.
           05 FILLER PIC X(3).
           05 COMMS-D PIC Z9,999.99.
       01 H-D-M.
           05 FILLER PIC X(25) VALUE 'Total Number of Salesman '.
           05 FILLER PIC X(15) VALUE 'in Dasmarinas:'.
           05 FILLER PIC X(8).
           05 MEN-D PIC 99.
       01 H-K-S.
           05 FILLER PIC X(24) VALUE 'Total Accumulated Sales '.
           05 FILLER PIC X(12) VALUE 'in Kawit:'.
           05 FILLER PIC X(7).
           05 SALES-K PIC ZZZ,999.99.
       01 H-K-C.
           05 FILLER PIC X(29) VALUE 'Total Accumulated Commission '.
           05 FILLER PIC X(12) VALUE 'in Kawit:'.
           05 FILLER PIC X(3).
           05 COMMS-K PIC Z9,999.99.
       01 H-K-M.
           05 FILLER PIC X(25) VALUE 'Total Number of Salesman '.
           05 FILLER PIC X(12) VALUE 'in Kawit:'.
           05 FILLER PIC X(8).
           05 MEN-K PIC 99.
       01 H-GRAND-SALE.
           05 FILLER PIC X(18) VALUE 'Grand Total Sales:'.
           05 FILLER PIC X(23).
           05 GRANDSAL PIC Z,Z99,999.99.
           05 FILLER PIC X(27).
       01 H-GRAND-COMM.
           05 FILLER PIC X(23) VALUE 'Grand Total Commission:'.
           05 FILLER PIC X(20).
           05 GRANDCOMM PIC Z9,999.99.
           05 FILLER PIC X(30).
       01 H-GRAND-MAN.
           05 FILLER PIC X(31) VALUE 'Grand Total Number of Salesman:'.
           05 FILLER PIC X(11).
           05 GRANDMAN PIC Z,999.
           05 FILLER PIC X(34).
       01 SPACE-H.
           05 FILLER PIC X(80).
       01 AREA-A.
           05 A-CODE PIC A VALUE 'A'.
           05 TOTSALE-A PIC 9(5)V99.
           05 COMMI-A PIC 9(5)V99.
           05 TOT-SA PIC 9(6)V99 VALUE 0.
           05 TOT-CA PIC 9(5)V99 VALUE 0.
           05 NUM-SMA PIC 99 VALUE 0.
       01 AREA-B.
           05 B-CODE PIC A VALUE 'B'.
           05 TOTSALE-B PIC 9(5)V99.
           05 COMMI-B PIC 9(5)V99.
           05 TOT-SB PIC 9(6)V99 VALUE 0.
           05 TOT-CB PIC 9(5)V99 VALUE 0.
           05 NUM-SMB PIC 99 VALUE 0.
       01 AREA-I.
           05 I-CODE PIC A VALUE 'I'.
           05 TOTSALE-I PIC 9(5)V99.
           05 COMMI-I PIC 9(5)V99.
           05 TOT-SI PIC 9(6)V99 VALUE 0.
           05 TOT-CI PIC 9(5)V99 VALUE 0.
           05 NUM-SMI PIC 99 VALUE 0.
       01 AREA-D.
           05 D-CODE PIC A VALUE 'D'.
           05 TOTSALE-D PIC 9(5)V99.
           05 COMMI-D PIC 9(5)V99.
           05 TOT-SD PIC 9(6)V99 VALUE 0.
           05 TOT-CD PIC 9(5)V99 VALUE 0.
           05 NUM-SMD PIC 99 VALUE 0.
       01 AREA-K.
           05 K-CODE PIC A VALUE 'K'.
           05 TOTSALE-K PIC 9(5)V99.
           05 COMMI-K PIC 9(5)V99.
           05 TOT-SK PIC 9(6)V99 VALUE 0.
           05 TOT-CK PIC 9(5)V99 VALUE 0.
           05 NUM-SMK PIC 99 VALUE 0.
       01 AREA-0.
           05 EOF PIC A VALUE 'N'.
           05 EOF1 PIC A VALUE 'N'.
           05 EOF2 PIC A VALUE 'N'.
           05 EOF3 PIC A VALUE 'N'.
           05 EOF4 PIC A VALUE 'N'.
           05 GRAND-S PIC 9(7)V99 VALUE 0.
           05 GRAND-C PIC 9(5)V99 VALUE 0.
           05 GRAND-M PIC 9(4) VALUE 0.
       SCREEN SECTION.
       01 SCRN.
           05 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN INPUT SALES-IN
               OUTPUT SALES-OUT.
           DISPLAY SCRN.
           WRITE REP-OUT FROM HEADER1.
           WRITE REP-OUT FROM HEADER2.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM HEADER3.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM SPACE-H.
           READ SALES-IN AT END MOVE 'Y' TO EOF.
           MOVE 'Alfonso' TO A-NAME
           WRITE REP-OUT FROM H-ALFONSO.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM HEADER4.
           PERFORM A-BREAK-RTN UNTIL EOF IS EQUAL TO 'Y'.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM H-A-S.
           WRITE REP-OUT FROM H-A-C.
           WRITE REP-OUT FROM H-A-M.
           WRITE REP-OUT FROM SPACE-H.
           CLOSE SALES-IN.
           OPEN INPUT SALES-IN.
           READ SALES-IN AT END MOVE 'Y' TO EOF1.
           MOVE 'Bacoor' TO B-NAME.
           WRITE REP-OUT FROM H-BACOOR.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM HEADER4.
           PERFORM B-BREAK-RTN UNTIL EOF1 IS EQUAL TO 'Y'.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM H-B-S.
           WRITE REP-OUT FROM H-B-C.
           WRITE REP-OUT FROM H-B-M.
           WRITE REP-OUT FROM SPACE-H.
           CLOSE SALES-IN.
           OPEN INPUT SALES-IN.
           READ SALES-IN AT END MOVE 'Y' TO EOF2.
           MOVE 'Imus' TO I-NAME.
           WRITE REP-OUT FROM H-IMUS.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM HEADER4.
           PERFORM I-BREAK-RTN UNTIL EOF2 IS EQUAL TO 'Y'.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM H-I-S.
           WRITE REP-OUT FROM H-I-C.
           WRITE REP-OUT FROM H-I-M.
           WRITE REP-OUT FROM SPACE-H.
           CLOSE SALES-IN.
           OPEN INPUT SALES-IN.
           READ SALES-IN AT END MOVE 'Y' TO EOF3.
           MOVE 'Dasmarinas' TO D-NAME.
           WRITE REP-OUT FROM H-DASMARINAS.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM HEADER4.
           PERFORM D-BREAK-RTN UNTIL EOF3 IS EQUAL TO 'Y'.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM H-D-S.
           WRITE REP-OUT FROM H-D-C.
           WRITE REP-OUT FROM H-D-M.
           WRITE REP-OUT FROM SPACE-H.
           CLOSE SALES-IN.
           OPEN INPUT SALES-IN.
           READ SALES-IN AT END MOVE 'Y' TO EOF4.
           MOVE 'Kawit' TO K-NAME.
           WRITE REP-OUT FROM H-KAWIT.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM HEADER4.
           PERFORM K-BREAK-RTN UNTIL EOF4 IS EQUAL TO 'Y'.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM H-K-S.
           WRITE REP-OUT FROM H-K-C.
           WRITE REP-OUT FROM H-K-M.
           WRITE REP-OUT FROM SPACE-H.
           PERFORM FINAL-RTN.
           DISPLAY 'DATA RECORDED!'.
           CLOSE SALES-IN, SALES-OUT.
           STOP RUN.
       A-BREAK-RTN.
           IF ARCODE = A-CODE
               MOVE SALESNUM TO SALNUM-A
               MOVE SALESNAME TO SALNAME-A
               ADD 1 TO NUM-SMA
               MOVE NUM-SMA TO MEN-A
               ADD 1 TO GRAND-M
               MOVE GRAND-M TO GRANDMAN
               MOVE SALES TO TOTSALE-A
               IF TOTSALE-A <= 5000
                   COMPUTE COMMI-A = (12 / 100) * TOTSALE-A
                   MOVE TOTSALE-A TO TOTSAL-A
                   MOVE COMMI-A TO COMM-A
                   WRITE REP-OUT FROM SALESMAN-A
                   COMPUTE TOT-SA = TOTSALE-A + TOT-SA
                   MOVE TOT-SA TO SALES-A
                   COMPUTE TOT-CA = COMMI-A + TOT-CA
                   MOVE TOT-CA TO COMMS-A
                   COMPUTE GRAND-S = TOTSALE-A + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-A + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-A <= 12000
                   COMPUTE COMMI-A = (18 / 100) * TOTSALE-A
                   MOVE TOTSALE-A TO TOTSAL-A
                   MOVE COMMI-A TO COMM-A
                   WRITE REP-OUT FROM SALESMAN-A
                   COMPUTE TOT-SA = TOTSALE-A + TOT-SA
                   MOVE TOT-SA TO SALES-A
                   COMPUTE TOT-CA = COMMI-A + TOT-CA
                   MOVE TOT-CA TO COMMS-A
                   COMPUTE GRAND-S = TOTSALE-A + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-A + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-A <= 20000
                   COMPUTE COMMI-A = (23 / 100) * TOTSALE-A
                   MOVE TOTSALE-A TO TOTSAL-A
                   MOVE COMMI-A TO COMM-A
                   WRITE REP-OUT FROM SALESMAN-A
                   COMPUTE TOT-SA = TOTSALE-A + TOT-SA
                   MOVE TOT-SA TO SALES-A
                   COMPUTE TOT-CA = COMMI-A + TOT-CA
                   MOVE TOT-CA TO COMMS-A
                   COMPUTE GRAND-S = TOTSALE-A + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-A + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-A <= 27000
                   COMPUTE COMMI-A = (30 / 100) * TOTSALE-A
                   MOVE TOTSALE-A TO TOTSAL-A
                   MOVE COMMI-A TO COMM-A
                   WRITE REP-OUT FROM SALESMAN-A
                   COMPUTE TOT-SA = TOTSALE-A + TOT-SA
                   MOVE TOT-SA TO SALES-A
                   COMPUTE TOT-CA = COMMI-A + TOT-CA
                   MOVE TOT-CA TO COMMS-A
                   COMPUTE GRAND-S = TOTSALE-A + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-A + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-A <= 35000
                   COMPUTE COMMI-A = (35 / 100) * TOTSALE-A
                   MOVE TOTSALE-A TO TOTSAL-A
                   MOVE COMMI-A TO COMM-A
                   WRITE REP-OUT FROM SALESMAN-A
                   COMPUTE TOT-SA = TOTSALE-A + TOT-SA
                   MOVE TOT-SA TO SALES-A
                   COMPUTE TOT-CA = COMMI-A + TOT-CA
                   MOVE TOT-CA TO COMMS-A
                   COMPUTE GRAND-S = TOTSALE-A + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-A + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-A > 35000
                   COMPUTE COMMI-A = (50 / 100) * TOTSALE-A
                   IF COMMI-A > 35000
                       COMPUTE COMMI-A = COMMI-A
                       MOVE TOTSALE-A TO TOTSAL-A
                       MOVE COMMI-A TO COMM-A
                       WRITE REP-OUT FROM SALESMAN-A
                       COMPUTE TOT-SA = TOTSALE-A + TOT-SA
                       MOVE TOT-SA TO SALES-A
                       COMPUTE TOT-CA = COMMI-A + TOT-CA
                       MOVE TOT-CA TO COMMS-A
                       COMPUTE GRAND-S = TOTSALE-A + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-A + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM
                   ELSE
                       COMPUTE COMMI-A = 35000
                       MOVE TOTSALE-A TO TOTSAL-A
                       MOVE COMMI-A TO COMM-A
                       WRITE REP-OUT FROM SALESMAN-A
                       COMPUTE TOT-SA = TOTSALE-A + TOT-SA
                       MOVE TOT-SA TO SALES-A
                       COMPUTE TOT-CA = COMMI-A + TOT-CA
                       MOVE TOT-CA TO COMMS-A
                       COMPUTE GRAND-S = TOTSALE-A + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-A + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM
           ELSE
               WRITE REP-OUT FROM SPACE-H.
           READ SALES-IN AT END MOVE 'Y' TO EOF.
       B-BREAK-RTN.
           IF ARCODE = B-CODE
               MOVE SALESNUM TO SALNUM-B
               MOVE SALESNAME TO SALNAME-B
               ADD 1 TO NUM-SMB
               MOVE NUM-SMB TO MEN-B
               ADD 1 TO GRAND-M
               MOVE GRAND-M TO GRANDMAN
               MOVE SALES TO TOTSALE-B
               IF TOTSALE-B <= 5000
                   COMPUTE COMMI-B = (12 / 100) * TOTSALE-B
                   MOVE TOTSALE-B TO TOTSAL-B
                   MOVE COMMI-B TO COMM-B
                   WRITE REP-OUT FROM SALESMAN-B
                   COMPUTE TOT-SB = TOTSALE-B + TOT-SB
                   MOVE TOT-SB TO SALES-B
                   COMPUTE TOT-CB = COMMI-B + TOT-CB
                   MOVE TOT-CB TO COMMS-B
                   COMPUTE GRAND-S = TOTSALE-B + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-B + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-B <= 12000
                   COMPUTE COMMI-B = (18 / 100) * TOTSALE-B
                   MOVE TOTSALE-B TO TOTSAL-B
                   MOVE COMMI-B TO COMM-B
                   WRITE REP-OUT FROM SALESMAN-B
                   COMPUTE TOT-SB = TOTSALE-B + TOT-SB
                   MOVE TOT-SB TO SALES-B
                   COMPUTE TOT-CB = COMMI-B + TOT-CB
                   MOVE TOT-CB TO COMMS-B
                   COMPUTE GRAND-S = TOTSALE-B + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-B + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-B <= 20000
                   COMPUTE COMMI-B = (23 / 100) * TOTSALE-B
                   MOVE TOTSALE-B TO TOTSAL-B
                   MOVE COMMI-B TO COMM-B
                   WRITE REP-OUT FROM SALESMAN-B
                   COMPUTE TOT-SB = TOTSALE-B + TOT-SB
                   MOVE TOT-SB TO SALES-B
                   COMPUTE TOT-CB = COMMI-B + TOT-CB
                   MOVE TOT-CB TO COMMS-B
                   COMPUTE GRAND-S = TOTSALE-B + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-B + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-B <= 27000
                   COMPUTE COMMI-B = (30 / 100) * TOTSALE-B
                   MOVE TOTSALE-B TO TOTSAL-B
                   MOVE COMMI-B TO COMM-B
                   WRITE REP-OUT FROM SALESMAN-B
                   COMPUTE TOT-SB = TOTSALE-B + TOT-SB
                   MOVE TOT-SB TO SALES-B
                   COMPUTE TOT-CB = COMMI-B + TOT-CB
                   MOVE TOT-CB TO COMMS-B
                   COMPUTE GRAND-S = TOTSALE-B + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-B + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-B <= 35000
                   COMPUTE COMMI-B = (35 / 100) * TOTSALE-B
                   MOVE TOTSALE-B TO TOTSAL-B
                   MOVE COMMI-B TO COMM-B
                   WRITE REP-OUT FROM SALESMAN-B
                   COMPUTE TOT-SB = TOTSALE-B + TOT-SB
                   MOVE TOT-SB TO SALES-B
                   COMPUTE TOT-CB = COMMI-B + TOT-CB
                   MOVE TOT-CB TO COMMS-B
                   COMPUTE GRAND-S = TOTSALE-B + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-B + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-B > 35000
                   COMPUTE COMMI-B = (50 / 100) * TOTSALE-B
                   IF COMMI-B > 35000
                       COMPUTE COMMI-B = COMMI-B
                       MOVE TOTSALE-B TO TOTSAL-B
                       MOVE COMMI-B TO COMM-B
                       WRITE REP-OUT FROM SALESMAN-B
                       COMPUTE TOT-SB = TOTSALE-B + TOT-SB
                       MOVE TOT-SB TO SALES-B
                       COMPUTE TOT-CB = COMMI-B + TOT-CB
                       MOVE TOT-CB TO COMMS-B
                       COMPUTE GRAND-S = TOTSALE-B + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-B + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM
                   ELSE
                       COMPUTE COMMI-B = 35000
                       MOVE TOTSALE-B TO TOTSAL-B
                       MOVE COMMI-B TO COMM-B
                       WRITE REP-OUT FROM SALESMAN-B
                       COMPUTE TOT-SB = TOTSALE-B + TOT-SB
                       MOVE TOT-SB TO SALES-B
                       COMPUTE TOT-CB = COMMI-B + TOT-CB
                       MOVE TOT-CB TO COMMS-B
                       COMPUTE GRAND-S = TOTSALE-B + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-B + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM

           ELSE
               WRITE REP-OUT FROM SPACE-H.
           READ SALES-IN AT END MOVE 'Y' TO EOF1.
       I-BREAK-RTN.
           IF ARCODE = I-CODE
               MOVE SALESNUM TO SALNUM-I
               MOVE SALESNAME TO SALNAME-I
               ADD 1 TO NUM-SMI
               MOVE NUM-SMI TO MEN-I
               ADD 1 TO GRAND-M
               MOVE GRAND-M TO GRANDMAN
               MOVE SALES TO TOTSALE-I
               IF TOTSALE-I <= 5000
                   COMPUTE COMMI-I = (12 / 100) * TOTSALE-I
                   MOVE TOTSALE-I TO TOTSAL-I
                   MOVE COMMI-I TO COMM-I
                   WRITE REP-OUT FROM SALESMAN-I
                   COMPUTE TOT-SI = TOTSALE-I + TOT-SI
                   MOVE TOT-SI TO SALES-I
                   COMPUTE TOT-CI = COMMI-I + TOT-CI
                   MOVE TOT-CI TO COMMS-I
                   COMPUTE GRAND-S = TOTSALE-I + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-I + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-I <= 12000
                   COMPUTE COMMI-I = (18 / 100) * TOTSALE-I
                   MOVE TOTSALE-I TO TOTSAL-I
                   MOVE COMMI-I TO COMM-I
                   WRITE REP-OUT FROM SALESMAN-I
                   COMPUTE TOT-SI = TOTSALE-I + TOT-SI
                   MOVE TOT-SI TO SALES-I
                   COMPUTE TOT-CI = COMMI-I + TOT-CI
                   MOVE TOT-CI TO COMMS-I
                   COMPUTE GRAND-S = TOTSALE-I + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-I + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-I <= 20000
                   COMPUTE COMMI-I = (23 / 100) * TOTSALE-I
                   MOVE TOTSALE-I TO TOTSAL-I
                   MOVE COMMI-I TO COMM-I
                   WRITE REP-OUT FROM SALESMAN-I
                   COMPUTE TOT-SI = TOTSALE-I + TOT-SI
                   MOVE TOT-SI TO SALES-I
                   COMPUTE TOT-CI = COMMI-I + TOT-CI
                   MOVE TOT-CI TO COMMS-I
                   COMPUTE GRAND-S = TOTSALE-I + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-I + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-I <= 27000
                   COMPUTE COMMI-I = (30 / 100) * TOTSALE-I
                   MOVE TOTSALE-I TO TOTSAL-I
                   MOVE COMMI-I TO COMM-I
                   WRITE REP-OUT FROM SALESMAN-I
                   COMPUTE TOT-SI = TOTSALE-I + TOT-SI
                   MOVE TOT-SI TO SALES-I
                   COMPUTE TOT-CI = COMMI-I + TOT-CI
                   MOVE TOT-CI TO COMMS-I
                   COMPUTE GRAND-S = TOTSALE-I + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-I + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-I <= 35000
                   COMPUTE COMMI-I = (35 / 100) * TOTSALE-I
                   MOVE TOTSALE-I TO TOTSAL-I
                   MOVE COMMI-I TO COMM-I
                   WRITE REP-OUT FROM SALESMAN-I
                   COMPUTE TOT-SI = TOTSALE-I + TOT-SI
                   MOVE TOT-SI TO SALES-I
                   COMPUTE TOT-CI = COMMI-I + TOT-CI
                   MOVE TOT-CI TO COMMS-I
                   COMPUTE GRAND-S = TOTSALE-I + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-I + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-I > 35000
                   COMPUTE COMMI-I = (50 / 100) * TOTSALE-I
                   IF COMMI-I > 35000
                       COMPUTE COMMI-I = COMMI-I
                       MOVE TOTSALE-I TO TOTSAL-I
                       MOVE COMMI-I TO COMM-I
                       WRITE REP-OUT FROM SALESMAN-I
                       COMPUTE TOT-SI = TOTSALE-I + TOT-SI
                       MOVE TOT-SI TO SALES-I
                       COMPUTE TOT-CI = COMMI-I + TOT-CI
                       MOVE TOT-CI TO COMMS-I
                       COMPUTE GRAND-S = TOTSALE-I + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-I + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM
                   ELSE
                       COMPUTE COMMI-I = 35000
                       MOVE TOTSALE-I TO TOTSAL-I
                       MOVE COMMI-I TO COMM-I
                       WRITE REP-OUT FROM SALESMAN-I
                       COMPUTE TOT-SI = TOTSALE-I + TOT-SI
                       MOVE TOT-SI TO SALES-I
                       COMPUTE TOT-CI = COMMI-I + TOT-CI
                       MOVE TOT-CI TO COMMS-I
                       COMPUTE GRAND-S = TOTSALE-I + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-I + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM
           ELSE
               WRITE REP-OUT FROM SPACE-H.
           READ SALES-IN AT END MOVE 'Y' TO EOF2.
       D-BREAK-RTN.
           IF ARCODE = D-CODE
               MOVE SALESNUM TO SALNUM-D
               MOVE SALESNAME TO SALNAME-D
               ADD 1 TO NUM-SMD
               MOVE NUM-SMD TO MEN-D
               ADD 1 TO GRAND-M
               MOVE GRAND-M TO GRANDMAN
               MOVE SALES TO TOTSALE-D
               IF TOTSALE-D <= 5000
                   COMPUTE COMMI-D = (12 / 100) * TOTSALE-D
                   MOVE TOTSALE-D TO TOTSAL-D
                   MOVE COMMI-D TO COMM-D
                   WRITE REP-OUT FROM SALESMAN-D
                   COMPUTE TOT-SD = TOTSALE-D + TOT-SD
                   MOVE TOT-SD TO SALES-D
                   COMPUTE TOT-CD = COMMI-D + TOT-CD
                   MOVE TOT-CD TO COMMS-D
                   COMPUTE GRAND-S = TOTSALE-D + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-D + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-D <= 12000
                   COMPUTE COMMI-D = (18 / 100) * TOTSALE-D
                   MOVE TOTSALE-D TO TOTSAL-D
                   MOVE COMMI-D TO COMM-D
                   WRITE REP-OUT FROM SALESMAN-D
                   COMPUTE TOT-SD = TOTSALE-D + TOT-SD
                   MOVE TOT-SD TO SALES-D
                   COMPUTE TOT-CD = COMMI-D + TOT-CD
                   MOVE TOT-CD TO COMMS-D
                   COMPUTE GRAND-S = TOTSALE-D + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-D + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-D <= 20000
                   COMPUTE COMMI-D = (23 / 100) * TOTSALE-D
                   MOVE TOTSALE-D TO TOTSAL-D
                   MOVE COMMI-D TO COMM-D
                   WRITE REP-OUT FROM SALESMAN-D
                   COMPUTE TOT-SD = TOTSALE-D + TOT-SD
                   MOVE TOT-SD TO SALES-D
                   COMPUTE TOT-CD = COMMI-D + TOT-CD
                   MOVE TOT-CD TO COMMS-D
                   COMPUTE GRAND-S = TOTSALE-D + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-D + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-D <= 27000
                   COMPUTE COMMI-D = (30 / 100) * TOTSALE-D
                   MOVE TOTSALE-D TO TOTSAL-D
                   MOVE COMMI-D TO COMM-D
                   WRITE REP-OUT FROM SALESMAN-D
                   COMPUTE TOT-SD = TOTSALE-D + TOT-SD
                   MOVE TOT-SD TO SALES-D
                   COMPUTE TOT-CD = COMMI-D + TOT-CD
                   MOVE TOT-CD TO COMMS-D
                   COMPUTE GRAND-S = TOTSALE-D + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-D + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-D <= 35000
                   COMPUTE COMMI-D = (35 / 100) * TOTSALE-D
                   MOVE TOTSALE-D TO TOTSAL-D
                   MOVE COMMI-D TO COMM-D
                   WRITE REP-OUT FROM SALESMAN-D
                   COMPUTE TOT-SD = TOTSALE-D + TOT-SD
                   MOVE TOT-SD TO SALES-D
                   COMPUTE TOT-CD = COMMI-D + TOT-CD
                   MOVE TOT-CD TO COMMS-D
                   COMPUTE GRAND-S = TOTSALE-D + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-D + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-D > 35000
                   COMPUTE COMMI-D = (50 / 100) * TOTSALE-D
                   IF COMMI-D > 35000
                       COMPUTE COMMI-D = COMMI-D
                       MOVE TOTSALE-D TO TOTSAL-D
                       MOVE COMMI-D TO COMM-D
                       WRITE REP-OUT FROM SALESMAN-D
                       COMPUTE TOT-SD = TOTSALE-D + TOT-SD
                       MOVE TOT-SD TO SALES-D
                       COMPUTE TOT-CD = COMMI-D + TOT-CD
                       MOVE TOT-CD TO COMMS-D
                       COMPUTE GRAND-S = TOTSALE-D + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-D + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM
                   ELSE
                       COMPUTE COMMI-D = 35000
                       MOVE TOTSALE-D TO TOTSAL-D
                       MOVE COMMI-D TO COMM-D
                       WRITE REP-OUT FROM SALESMAN-D
                       COMPUTE TOT-SD = TOTSALE-D + TOT-SD
                       MOVE TOT-SD TO SALES-D
                       COMPUTE TOT-CD = COMMI-D + TOT-CD
                       MOVE TOT-CD TO COMMS-D
                       COMPUTE GRAND-S = TOTSALE-D + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-D + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM
           ELSE
               WRITE REP-OUT FROM SPACE-H.
           READ SALES-IN AT END MOVE 'Y' TO EOF3.
       K-BREAK-RTN.
           IF ARCODE = K-CODE
               MOVE SALESNUM TO SALNUM-K
               MOVE SALESNAME TO SALNAME-K
               ADD 1 TO NUM-SMK
               MOVE NUM-SMK TO MEN-K
               ADD 1 TO GRAND-M
               MOVE GRAND-M TO GRANDMAN
               MOVE SALES TO TOTSALE-K
               IF TOTSALE-K <= 5000
                   COMPUTE COMMI-K = (12 / 100) * TOTSALE-K
                   MOVE TOTSALE-K TO TOTSAL-K
                   MOVE COMMI-K TO COMM-K
                   WRITE REP-OUT FROM SALESMAN-K
                   COMPUTE TOT-SK = TOTSALE-K + TOT-SK
                   MOVE TOT-SK TO SALES-K
                   COMPUTE TOT-CK = COMMI-K + TOT-CK
                   MOVE TOT-CK TO COMMS-K
                   COMPUTE GRAND-S = TOTSALE-K + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-K + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-K <= 12000
                   COMPUTE COMMI-K = (18 / 100) * TOTSALE-K
                   MOVE TOTSALE-K TO TOTSAL-K
                   MOVE COMMI-K TO COMM-K
                   WRITE REP-OUT FROM SALESMAN-K
                   COMPUTE TOT-SK = TOTSALE-K + TOT-SK
                   MOVE TOT-SK TO SALES-K
                   COMPUTE TOT-CK = COMMI-K + TOT-CK
                   MOVE TOT-CK TO COMMS-K
                   COMPUTE GRAND-S = TOTSALE-K + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-K + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-K <= 20000
                   COMPUTE COMMI-K = (23 / 100) * TOTSALE-K
                   MOVE TOTSALE-K TO TOTSAL-K
                   MOVE COMMI-K TO COMM-K
                   WRITE REP-OUT FROM SALESMAN-K
                   COMPUTE TOT-SK = TOTSALE-K + TOT-SK
                   MOVE TOT-SK TO SALES-K
                   COMPUTE TOT-CK = COMMI-K + TOT-CK
                   MOVE TOT-CK TO COMMS-K
                   COMPUTE GRAND-S = TOTSALE-K + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-K + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-K <= 27000
                   COMPUTE COMMI-K = (30 / 100) * TOTSALE-K
                   MOVE TOTSALE-K TO TOTSAL-K
                   MOVE COMMI-K TO COMM-K
                   WRITE REP-OUT FROM SALESMAN-K
                   COMPUTE TOT-SK = TOTSALE-K + TOT-SK
                   MOVE TOT-SK TO SALES-K
                   COMPUTE TOT-CK = COMMI-K + TOT-CK
                   MOVE TOT-CK TO COMMS-K
                   COMPUTE GRAND-S = TOTSALE-K + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-K + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-K <= 35000
                   COMPUTE COMMI-K = (35 / 100) * TOTSALE-K
                   MOVE TOTSALE-K TO TOTSAL-K
                   MOVE COMMI-K TO COMM-K
                   WRITE REP-OUT FROM SALESMAN-K
                   COMPUTE TOT-SK = TOTSALE-K + TOT-SK
                   MOVE TOT-SK TO SALES-K
                   COMPUTE TOT-CK = COMMI-K + TOT-CK
                   MOVE TOT-CK TO COMMS-K
                   COMPUTE GRAND-S = TOTSALE-K + GRAND-S
                   MOVE GRAND-S TO GRANDSAL
                   COMPUTE GRAND-C = COMMI-K + GRAND-C
                   MOVE GRAND-C TO GRANDCOMM
               ELSE IF TOTSALE-K > 35000
                   COMPUTE COMMI-K = (50 / 100) * TOTSALE-K
                   IF COMMI-K > 35000
                       COMPUTE COMMI-D = COMMI-D
                       MOVE TOTSALE-K TO TOTSAL-K
                       MOVE COMMI-K TO COMM-K
                       WRITE REP-OUT FROM SALESMAN-K
                       COMPUTE TOT-SK = TOTSALE-K + TOT-SK
                       MOVE TOT-SK TO SALES-K
                       COMPUTE TOT-CK = COMMI-K + TOT-CK
                       MOVE TOT-CK TO COMMS-K
                       COMPUTE GRAND-S = TOTSALE-K + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-K + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM
                   ELSE
                       COMPUTE COMMI-K = 35000
                       MOVE TOTSALE-K TO TOTSAL-K
                       MOVE COMMI-K TO COMM-K
                       WRITE REP-OUT FROM SALESMAN-K
                       COMPUTE TOT-SK = TOTSALE-K + TOT-SK
                       MOVE TOT-SK TO SALES-K
                       COMPUTE TOT-CK = COMMI-K + TOT-CK
                       MOVE TOT-CK TO COMMS-K
                       COMPUTE GRAND-S = TOTSALE-K + GRAND-S
                       MOVE GRAND-S TO GRANDSAL
                       COMPUTE GRAND-C = COMMI-K + GRAND-C
                       MOVE GRAND-C TO GRANDCOMM
           ELSE
               WRITE REP-OUT FROM SPACE-H.
           READ SALES-IN AT END MOVE 'Y' TO EOF4.
       FINAL-RTN.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM SPACE-H.
           WRITE REP-OUT FROM H-GRAND-SALE.
           WRITE REP-OUT FROM H-GRAND-COMM.
           WRITE REP-OUT FROM H-GRAND-MAN.
