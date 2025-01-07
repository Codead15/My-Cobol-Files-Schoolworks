      * READ THE RECORD FROM THE INPUT FILE AND DISPLAY
      * THEM ON THE SCREEN
      * IT WILL ALSO WRITE THE RECORD ON THE OUTPUT FILE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE4.
       AUTHOR. ELY.
       INSTALLATION. HOUSE.
       DATE-WRITTEN. TODAY.
       DATE-COMPILED. TODAY.
       SECURITY. FOR PRESENTATION ONLY.
       REMARKS. SAMPLE PROGRAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. HP-LAPTOP.
       OBJECT-COMPUTER. HP-LAPTOP.
       SPECIAL-NAMES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDIN ASSIGN TO "STUDREC.TXT".
           SELECT STUDOUT ASSIGN TO "STUDREP.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD  STUDIN
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 42 CHARACTERS
           DATA RECORD IS STUDREC.
       01  STUDREC.
           05 STUDNO PIC X(15).
           05 STUDNA PIC X(20).
           05 CRS PIC X(5).
           05 YR PIC 9.
           05 SEC PIC 9.
       FD  STUDOUT.
       01  REP-OUT.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.
       01  OTHERS.
           05 EOF PIC A VALUE 'N'.
           05 TOT-BSIT PIC 99 VALUE 0.
           05 TOT-BSCS PIC 99 VALUE 0.
           05 TOT-BSIS PIC 99 VALUE 0.
       01  STUDENTS.
           05 FILLER PIC X(5).
           05 STUDNUM PIC X(15).
           05 FILLER PIC X(5).
           05 STUDNAME PIC X(20).
           05 FILLER PIC X(5).
           05 COURSE PIC X(5).
           05 FILLER PIC X(9).
           05 YEAR PIC 9.
           05 FILLER PIC X(10).
           05 SECT PIC 9.
           05 FILLER PIC X(4).
       01  HEADER1.
           05 FILLER PIC X(5).
           05 FILLER PIC X(15) VALUE 'STUDENT NUMBER'.
           05 FILLER PIC X(5).
           05 FILLER PIC X(20) VALUE 'STUDENT NAME'.
           05 FILLER PIC X(5).
           05 FILLER PIC X(6) VALUE 'COURSE'.
           05 FILLER PIC X(8).
           05 FILLER PIC X(4) VALUE 'YEAR'.
           05 FILLER PIC X(10).
           05 FILLER PIC X(7) VALUE 'SECTION'.
       01  HEADER2.
           05 FILLER PIC X(3).
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF BSIT STUDENT: '.
           05 T-IT PIC 99.
       01  HEADER3.
           05 FILLER PIC X(3).
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF BSCS STUDENT: '.
           05 T-CS PIC 99.
       01  HEADER4.
           05 FILLER PIC X(3).
           05 FILLER PIC X(30) VALUE 'TOTAL NUMBER OF BSIS STUDENT: '.
           05 T-IS PIC 99.

       SCREEN SECTION.
       01  SCR.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN INPUT STUDIN
                OUTPUT STUDOUT.
           DISPLAY SCR.
           WRITE REP-OUT FROM HEADER1.
           READ STUDIN AT END MOVE 'Y' TO EOF.
           PERFORM READ-RECORD-RTN UNTIL EOF IS EQUAL TO 'Y'.
           PERFORM FINAL-MOVE-RTN.
           CLOSE STUDIN, STUDOUT.
           OPEN INPUT STUDOUT.
           MOVE 'N' TO EOF.
           READ STUDOUT AT END MOVE 'Y' TO EOF.
           PERFORM FINAL-DISPLAY-RTN UNTIL EOF IS EQUAL TO 'Y'.

           STOP RUN.

       READ-RECORD-RTN.
           PERFORM MOVE-RTN.
           IF CRS IS EQUAL TO 'BSIT'
              ADD 1 TO TOT-BSIT
           ELSE IF CRS IS EQUAL TO 'BSCS'
              ADD 1 TO TOT-BSCS
           ELSE ADD 1 TO TOT-BSIS.
           READ STUDIN AT END MOVE 'Y' TO EOF.

       MOVE-RTN.
           MOVE STUDNO TO STUDNUM.
           MOVE STUDNA TO STUDNAME.
           MOVE CRS TO COURSE.
           MOVE YR TO YEAR.
           MOVE SEC TO SECT.
      * invisible printing header 1
           WRITE REP-OUT FROM STUDENTS.

       FINAL-MOVE-RTN.
           MOVE TOT-BSIT TO T-IT.
           MOVE TOT-BSCS TO T-CS.
           MOVE TOT-BSIS TO T-IS.
           WRITE REP-OUT FROM HEADER2.
           WRITE REP-OUT FROM HEADER3.
           WRITE REP-OUT FROM HEADER4.

       FINAL-DISPLAY-RTN.
           DISPLAY REP-OUT.
           READ STUDOUT AT END MOVE 'Y' TO EOF.
