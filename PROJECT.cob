      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT.
       AUTHOR. GROUP 2.
       REMARKS. FINAL-PROJECT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT1 ASSIGN TO 'SUBJECT.TXT'.
           SELECT ACCT2 ASSIGN TO 'FACULTY.TXT'.
           SELECT ACCT3 ASSIGN TO 'STUDENT.TXT'.
           SELECT ACCT4 ASSIGN TO 'GRADE.TXT'.
           SELECT ACCT5 ASSIGN TO 'ENROLL.TXT'.
           SELECT ACCT6 ASSIGN TO 'GRADES.TXT'.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCT1
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 40 CHARACTERS
           DATA RECORD IS SUBJECT-INFO.
       01  SUBJECT-INFO.
           05 SUBJ-CODE1 PIC X(10).
           05 SUBJ-DESC PIC X(30).

       FD  ACCT2
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 47 CHARACTERS
           DATA RECORD IS FACULTY-INFO.
       01  FACULTY-INFO.
           05 SUBJ-CODE2 PIC X(10).
           05 FAC-ID2 PIC X(12).
           05 FAC-NAME PIC X(25).

       FD  ACCT3
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 59 CHARACTERS
           DATA RECORD IS STUDENT-INFO.
       01  STUDENT-INFO.
           05 SUBJ-CODE3 PIC X(10).
           05 FAC-ID3 PIC X(12).
           05 STUD-NUM3 PIC X(12).
           05 STUD-NAME3 PIC X(25).

       FD  ACCT4
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 40 CHARACTERS
           DATA RECORD IS GRADES-INFO.
       01  GRADES-INFO.
           05 SUBJ-CODE4 PIC X(10).
           05 FAC-ID4 PIC X(12).
           05 STUD-NUM4 PIC X(12).
           05 MID-GRADE PIC 9V99.
           05 FIN-GRADE PIC 9V99.

       FD  ACCT5.
       01  ENROLL-OUTPUT.
           05 FILLER PIC X(80).

       FD  ACCT6.
       01  GRADES-OUTPUT.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.
       01  HEADER.
           05 FILLER PIC X(32).
           05 FILLER PIC X(16) VALUE 'TRI-STAR ACADEMY'.
           05 FILLER PIC X(32).

       01  HEADER2.
           05 FILLER PIC X(28).
           05 FILLER PIC X(25) VALUE 'GREENHILLS, SAN JUAN CITY'.
           05 FILLER PIC X(27).

       01  HEADER3.
           05 FILLER PIC X(28).
           05 FILLER PIC X(25) VALUE 'STUDENT ENROLLMENT REPORT'.
           05 FILLER PIC X(27).

       01  BLNK-HDR.
           05 FILLER PIC X(80).

       01  SUB-HDR.
           05 FILLER PIC X(24) VALUE 'SUBJECT CODE         :  '.
           05 SUBJ-CODE PIC X(10).
           05 FILLER PIC X(46).

       01  SUB-HDR2.
           05 FILLER PIC X(24) VALUE 'SUBJECT DESCRIPTION  :  '.
           05 SUBJ-DESCRIP PIC X(30).
           05 FILLER PIC X(26).

       01  SUB-HDR3.
           05 FILLER PIC X(14) VALUE 'STUDENT NUMBER'.
           05 FILLER PIC X(14).
           05 FILLER PIC X(12) VALUE 'STUDENT NAME'.
           05 FILLER PIC X(40).

       01  ENROLL-INFO.
           05 STUDENT-NO PIC X(12).
           05 FILLER PIC X(16).
           05 STUDENT-NA PIC X(25).
           05 FILLER PIC X(27).

       01  TOTAL-NUM-ENROLL.
           05 FILLER PIC X(24) VALUE 'TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(11) VALUE ' ENROLLED: '.
           05 TOT-NO-ENRL PIC Z99.
           05 FILLER PIC X(42).

       01  GT-NUM-ENROLL.
           05 FILLER PIC X(30) VALUE 'GRAND TOTAL NUMBER OF STUDENTS'.
           05 FILLER PIC X(13) VALUE ' ENROLLED:   '.
           05 GT-NO-ENRL PIC Z,Z99.
           05 FILLER PIC X(32).

       01  OTHERS.
           05 ENRL-EOF PIC X(3) VALUE 'NO'.
           05 ENRL-EOF2 PIC X(3) VALUE 'NO'.
           05 ENRL-EOF3 PIC X(3) VALUE 'NO'.
           05 ENRL-EOF4 PIC X(3) VALUE 'NO'.
           05 ENRL-EOF5 PIC X(3) VALUE 'NO'.
           05 ENRL-EOF6 PIC X(3) VALUE 'NO'.
           05 ENRL-EOF7 PIC X(3) VALUE 'NO'.
           05 TOT-FRST PIC 999 VALUE 0.
           05 TOT-SEC PIC 999 VALUE 0.
           05 TOT-THRD PIC 999 VALUE 0.
           05 TOT-FRTH PIC 999 VALUE 0.
           05 TOT-FFTH PIC 999 VALUE 0.
           05 TOT-SXTH PIC 999 VALUE 0.
           05 TOT-SVNTH PIC 999 VALUE 0.
           05 GT-NUM-ENRL PIC 9999 VALUE 0.

       01  GRD-HDR.
           05 FILLER PIC X(17) VALUE 'SUBJECT CODE  :  '.
           05 SUB-CD PIC X(10).
           05 FILLER PIC X(53).

       01  GRD-HDR2.
           05 FILLER PIC X(17) VALUE 'FACULTY ID    :  '.
           05 FACULTY-ID PIC X(12).
           05 FILLER PIC X(51).

       01  GRD-HDR3.
           05 FILLER PIC X(17) VALUE 'FACULTY NAME  :  '.
           05 FACULTY-NAME PIC X(25).
           05 FILLER PIC X(38).

       01  GRD-HDR4.
           05 FILLER PIC X(14) VALUE 'STUDENT NUMBER'.
           05 FILLER PIC X(11).
           05 FILLER PIC X(12) VALUE 'STUDENT NAME'.
           05 FILLER PIC X(16).
           05 FILLER PIC X(13) VALUE 'AVERAGE GRADE'.
           05 FILLER PIC X(7).
           05 FILLER PIC X(7) VALUE 'REMARKS'.

       01  GRADE-INFO.
           05 STUDENT-NO2 PIC X(12).
           05 FILLER PIC X(13).
           05 STUDENT-NA2 PIC X(25).
           05 FILLER PIC X(3).
           05 AVERAGE-GRD PIC 9.99.
           05 FILLER PIC X(16).
           05 RMRK PIC X(7).

       01  TOT-NUM-PASS.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' PASSED: '.
           05 TOT-NO-PASS PIC Z9.
           05 FILLER PIC X(41).

       01  TOT-NUM-FAIL.
           05 FILLER PIC X(28) VALUE 'TOTAL NUMBER OF STUDENTS WHO'.
           05 FILLER PIC X(9) VALUE ' FAILED: '.
           05 TOT-NO-FAIL PIC Z9.
           05 FILLER PIC X(41).

       01  OTHERS2.
           05 EOF PIC X(3) VALUE 'NO'.
           05 EOF2 PIC X(3) VALUE 'YES'.
           05 EOF3 PIC X(3) VALUE 'NO'.
           05 EOF4 PIC X(3) VALUE 'YES'.
           05 EOF5 PIC X(3) VALUE 'NO'.
           05 EOF6 PIC X(3) VALUE 'YES'.
           05 EOF7 PIC X(3) VALUE 'NO'.
           05 EOF8 PIC X(3) VALUE 'YES'.
           05 EOF9 PIC X(3) VALUE 'NO'.
           05 EOF10 PIC X(3) VALUE 'YES'.
           05 EOF11 PIC X(3) VALUE 'NO'.
           05 EOF12 PIC X(3) VALUE 'YES'.
           05 EOF13 PIC X(3) VALUE 'NO'.
           05 EOF14 PIC X(3) VALUE 'YES'.
           05 EOF15 PIC X(3) VALUE 'NO'.
           05 EOF16 PIC X(3) VALUE 'YES'.
           05 EOF17 PIC X(3) VALUE 'NO'.
           05 EOF18 PIC X(3) VALUE 'YES'.
           05 EOF19 PIC X(3) VALUE 'NO'.
           05 EOF20 PIC X(3) VALUE 'YES'.
           05 EOF21 PIC X(3) VALUE 'NO'.
           05 EOF22 PIC X(3) VALUE 'YES'.

       01  OTHERS3.
           05 AVE-GRD PIC 9V99 VALUE 0.
           05 STUD-PASS PIC 9 VALUE 0.
           05 STUD-FAIL PIC 9 VALUE 0.
           05 STUD-PASS2 PIC 9 VALUE 0.
           05 STUD-FAIL2 PIC 9 VALUE 0.
           05 STUD-PASS3 PIC 9 VALUE 0.
           05 STUD-FAIL3 PIC 9 VALUE 0.
           05 STUD-PASS4 PIC 9 VALUE 0.
           05 STUD-FAIL4 PIC 9 VALUE 0.
           05 STUD-PASS5 PIC 9 VALUE 0.
           05 STUD-FAIL5 PIC 9 VALUE 0.
           05 STUD-PASS6 PIC 9 VALUE 0.
           05 STUD-FAIL6 PIC 9 VALUE 0.
           05 STUD-PASS7 PIC 9 VALUE 0.
           05 STUD-FAIL7 PIC 9 VALUE 0.
           05 STUD-PASS8 PIC 9 VALUE 0.
           05 STUD-FAIL8 PIC 9 VALUE 0.
           05 STUD-PASS9 PIC 9 VALUE 0.
           05 STUD-FAIL9 PIC 9 VALUE 0.
           05 STUD-PASS10 PIC 9 VALUE 0.
           05 STUD-FAIL10 PIC 9 VALUE 0.
           05 STUD-PASS11 PIC 9 VALUE 0.
           05 STUD-FAIL11 PIC 9 VALUE 0.

       SCREEN SECTION.
       01  SCRN.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
           PERFORM ENROLL-PROCED-DIV.
           PERFORM GRADES-PROCED-DIV.
           DISPLAY 'END OF MERGING PROGRAM!'.
           DISPLAY 'CHECK ENROLL.txt FOR THE OUTPUT'.
           DISPLAY 'CHECK GRADES.txt FOR THE OUTPUT'.
           STOP RUN.

       ENROLL-PROCED-DIV.
           OPEN INPUT ACCT1
               INPUT ACCT3
               OUTPUT ACCT5.
           DISPLAY SCRN.
           PERFORM WRITE-HEADER.
           PERFORM FIRST-SUB-PRCS.
           CLOSE ACCT3.
           OPEN INPUT ACCT3.
           PERFORM SCND-SUB-PRCS.
           CLOSE ACCT3.
           OPEN INPUT ACCT3.
           PERFORM THRD-SUB-PRCS.
           CLOSE ACCT3.
           OPEN INPUT ACCT3.
           PERFORM FRTH-SUB-PRCS.
           CLOSE ACCT3.
           OPEN INPUT ACCT3.
           PERFORM FFTH-SUB-PRCS.
           CLOSE ACCT3.
           OPEN INPUT ACCT3.
           PERFORM SXTH-SUB-PRCS.
           CLOSE ACCT3.
           OPEN INPUT ACCT3.
           PERFORM SVNTH-SUB-PRCS.
           PERFORM FIN-RTN.
           CLOSE ACCT1, ACCT3, ACCT5.

       GRADES-PROCED-DIV.
           OPEN INPUT ACCT2
               INPUT ACCT3
               INPUT ACCT4
               OUTPUT ACCT6.
           PERFORM WRITE-HEADER2.
           PERFORM FIRST-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM SCND-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM THRD-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM FRTH-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM FFTH-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM SXTH-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM SVNTH-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM EGHT-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM NNTH-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM TNTH-GRD-PRCS.
           CLOSE ACCT3, ACCT4.
           OPEN INPUT ACCT3
               INPUT ACCT4.
           PERFORM ELVN-GRD-PRCS.
           CLOSE ACCT2, ACCT3, ACCT4, ACCT6.

       WRITE-HEADER.
           WRITE ENROLL-OUTPUT FROM HEADER.
           WRITE ENROLL-OUTPUT FROM HEADER2.
           WRITE ENROLL-OUTPUT FROM BLNK-HDR AFTER 2.
           WRITE ENROLL-OUTPUT FROM HEADER3.
           WRITE ENROLL-OUTPUT FROM BLNK-HDR AFTER 2.

       FIRST-SUB-PRCS.
           READ ACCT1.
           IF SUBJ-CODE1 = 'IT 2001   '
               MOVE SUBJ-CODE1 TO SUBJ-CODE
               MOVE SUBJ-DESC TO SUBJ-DESCRIP
               WRITE ENROLL-OUTPUT FROM SUB-HDR.
               WRITE ENROLL-OUTPUT FROM SUB-HDR2.
               WRITE ENROLL-OUTPUT FROM SUB-HDR3.
               READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF.
               PERFORM FIRST-SUB-COMP UNTIL ENRL-EOF = 'YES'.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
               WRITE ENROLL-OUTPUT FROM TOTAL-NUM-ENROLL.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
       FIRST-SUB-COMP.
           IF SUBJ-CODE3 = SUBJ-CODE1
               MOVE STUD-NUM3 TO STUDENT-NO
               MOVE STUD-NAME3 TO STUDENT-NA
               ADD 1 TO TOT-FRST
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-FRST TO TOT-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL-OUTPUT FROM ENROLL-INFO.
           READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF.

       SCND-SUB-PRCS.
           READ ACCT1.
           IF SUBJ-CODE1 = 'HIST 1000 '
               MOVE SUBJ-CODE1 TO SUBJ-CODE
               MOVE SUBJ-DESC TO SUBJ-DESCRIP
               WRITE ENROLL-OUTPUT FROM SUB-HDR.
               WRITE ENROLL-OUTPUT FROM SUB-HDR2.
               WRITE ENROLL-OUTPUT FROM SUB-HDR3.
               READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF2.
               PERFORM SCND-SUB-COMP UNTIL ENRL-EOF2 = 'YES'.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
               WRITE ENROLL-OUTPUT FROM TOTAL-NUM-ENROLL.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
       SCND-SUB-COMP.
           IF SUBJ-CODE3 = SUBJ-CODE1
               MOVE STUD-NUM3 TO STUDENT-NO
               MOVE STUD-NAME3 TO STUDENT-NA
               ADD 1 TO TOT-SEC
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-SEC TO TOT-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL-OUTPUT FROM ENROLL-INFO.
           READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF2.

       THRD-SUB-PRCS.
           READ ACCT1.
           IF SUBJ-CODE1 = 'NATSCI 100'
               MOVE SUBJ-CODE1 TO SUBJ-CODE
               MOVE SUBJ-DESC TO SUBJ-DESCRIP
               WRITE ENROLL-OUTPUT FROM SUB-HDR.
               WRITE ENROLL-OUTPUT FROM SUB-HDR2.
               WRITE ENROLL-OUTPUT FROM SUB-HDR3.
               READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF3.
               PERFORM THRD-SUB-COMP UNTIL ENRL-EOF3 = 'YES'.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
               WRITE ENROLL-OUTPUT FROM TOTAL-NUM-ENROLL.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
       THRD-SUB-COMP.
           IF SUBJ-CODE3 = SUBJ-CODE1
               MOVE STUD-NUM3 TO STUDENT-NO
               MOVE STUD-NAME3 TO STUDENT-NA
               ADD 1 TO TOT-THRD
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-THRD TO TOT-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL-OUTPUT FROM ENROLL-INFO.
           READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF3.

       FRTH-SUB-PRCS.
           READ ACCT1.
           IF SUBJ-CODE1 = 'NATSCI 105'
               MOVE SUBJ-CODE1 TO SUBJ-CODE
               MOVE SUBJ-DESC TO SUBJ-DESCRIP
               WRITE ENROLL-OUTPUT FROM SUB-HDR.
               WRITE ENROLL-OUTPUT FROM SUB-HDR2.
               WRITE ENROLL-OUTPUT FROM SUB-HDR3.
               READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF4.
               PERFORM FRTH-SUB-COMP UNTIL ENRL-EOF4 = 'YES'.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
               WRITE ENROLL-OUTPUT FROM TOTAL-NUM-ENROLL.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
       FRTH-SUB-COMP.
           IF SUBJ-CODE3 = SUBJ-CODE1
               MOVE STUD-NUM3 TO STUDENT-NO
               MOVE STUD-NAME3 TO STUDENT-NA
               ADD 1 TO TOT-FRTH
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-FRTH TO TOT-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL-OUTPUT FROM ENROLL-INFO.
           READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF4.

       FFTH-SUB-PRCS.
           READ ACCT1.
           IF SUBJ-CODE1 = 'COMP 2000 '
               MOVE SUBJ-CODE1 TO SUBJ-CODE
               MOVE SUBJ-DESC TO SUBJ-DESCRIP
               WRITE ENROLL-OUTPUT FROM SUB-HDR.
               WRITE ENROLL-OUTPUT FROM SUB-HDR2.
               WRITE ENROLL-OUTPUT FROM SUB-HDR3.
               READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF5.
               PERFORM FFTH-SUB-COMP UNTIL ENRL-EOF5 = 'YES'.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
               WRITE ENROLL-OUTPUT FROM TOTAL-NUM-ENROLL.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
       FFTH-SUB-COMP.
           IF SUBJ-CODE3 = SUBJ-CODE1
               MOVE STUD-NUM3 TO STUDENT-NO
               MOVE STUD-NAME3 TO STUDENT-NA
               ADD 1 TO TOT-FFTH
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-FFTH TO TOT-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL-OUTPUT FROM ENROLL-INFO.
           READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF5.

       SXTH-SUB-PRCS.
           READ ACCT1.
           IF SUBJ-CODE1 = 'IT 2003      '
               MOVE SUBJ-CODE1 TO SUBJ-CODE
               MOVE SUBJ-DESC TO SUBJ-DESCRIP
               WRITE ENROLL-OUTPUT FROM SUB-HDR.
               WRITE ENROLL-OUTPUT FROM SUB-HDR2.
               WRITE ENROLL-OUTPUT FROM SUB-HDR3.
               READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF6.
               PERFORM SXTH-SUB-COMP UNTIL ENRL-EOF6 = 'YES'.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
               WRITE ENROLL-OUTPUT FROM TOTAL-NUM-ENROLL.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
       SXTH-SUB-COMP.
           IF SUBJ-CODE3 = SUBJ-CODE1
               MOVE STUD-NUM3 TO STUDENT-NO
               MOVE STUD-NAME3 TO STUDENT-NA
               ADD 1 TO TOT-SXTH
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-SXTH TO TOT-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL-OUTPUT FROM ENROLL-INFO.
           READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF6.

       SVNTH-SUB-PRCS.
           READ ACCT1.
           IF SUBJ-CODE1 = 'MATH 1000 '
               MOVE SUBJ-CODE1 TO SUBJ-CODE
               MOVE SUBJ-DESC TO SUBJ-DESCRIP
               WRITE ENROLL-OUTPUT FROM SUB-HDR.
               WRITE ENROLL-OUTPUT FROM SUB-HDR2.
               WRITE ENROLL-OUTPUT FROM SUB-HDR3.
               READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF7.
               PERFORM SVNTH-SUB-COMP UNTIL ENRL-EOF7 = 'YES'.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
               WRITE ENROLL-OUTPUT FROM TOTAL-NUM-ENROLL.
               WRITE ENROLL-OUTPUT FROM BLNK-HDR.
       SVNTH-SUB-COMP.
           IF SUBJ-CODE3 = SUBJ-CODE1
               MOVE STUD-NUM3 TO STUDENT-NO
               MOVE STUD-NAME3 TO STUDENT-NA
               ADD 1 TO TOT-SVNTH
               ADD 1 TO GT-NUM-ENRL
               MOVE TOT-SVNTH TO TOT-NO-ENRL
               MOVE GT-NUM-ENRL TO GT-NO-ENRL
               WRITE ENROLL-OUTPUT FROM ENROLL-INFO.
           READ ACCT3 AT END MOVE 'YES' TO ENRL-EOF7.

       FIN-RTN.
           WRITE ENROLL-OUTPUT FROM BLNK-HDR.
           WRITE ENROLL-OUTPUT FROM GT-NUM-ENROLL.

       WRITE-HEADER2.
           WRITE GRADES-OUTPUT FROM HEADER.
           WRITE GRADES-OUTPUT FROM HEADER2.
           WRITE GRADES-OUTPUT FROM BLNK-HDR AFTER 2.
           WRITE GRADES-OUTPUT FROM HEADER3.
           WRITE GRADES-OUTPUT FROM BLNK-HDR AFTER 2.

       FIRST-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'COMP 2000 ' AND
               FAC-ID2 = '67890          '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF.
               READ ACCT4.
               PERFORM FIRST-GRD-COMP UNTIL EOF = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       FIRST-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM FIRST-GRD-FIN UNTIL EOF2 = 'NO'
               PERFORM FIRST-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF.
       FIRST-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL.
           READ ACCT4 AT END MOVE 'NO' TO EOF2.
       FIRST-GRD-BRK.
           MOVE STUD-PASS TO TOT-NO-PASS
           MOVE STUD-FAIL TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF2.

       SCND-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'NATSCI 105' AND
               FAC-ID2 = '12345       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF3.
               READ ACCT4.
               PERFORM SCND-GRD-COMP UNTIL EOF3 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       SCND-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM SCND-GRD-FIN UNTIL EOF4 = 'NO'
               PERFORM SCND-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF3.
       SCND-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS2
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL2.
           READ ACCT4 AT END MOVE 'NO' TO EOF4.
       SCND-GRD-BRK.
           MOVE STUD-PASS2 TO TOT-NO-PASS
           MOVE STUD-FAIL2 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF4.

       THRD-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'IT 2001   ' AND
               FAC-ID2 = '34567       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF5.
               READ ACCT4.
               PERFORM THRD-GRD-COMP UNTIL EOF5 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       THRD-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM THRD-GRD-FIN UNTIL EOF6 = 'NO'
               PERFORM THRD-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF5.
       THRD-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS3
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL3.
           READ ACCT4 AT END MOVE 'NO' TO EOF6.
       THRD-GRD-BRK.
           MOVE STUD-PASS3 TO TOT-NO-PASS
           MOVE STUD-FAIL3 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF6.

       FRTH-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'COMP 2000 ' AND
               FAC-ID2 = '23456       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF7.
               READ ACCT4.
               PERFORM FRTH-GRD-COMP UNTIL EOF7 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       FRTH-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM FRTH-GRD-FIN UNTIL EOF8 = 'NO'
               PERFORM FRTH-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF7.
       FRTH-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS4
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL4.
           READ ACCT4 AT END MOVE 'NO' TO EOF8.
       FRTH-GRD-BRK.
           MOVE STUD-PASS4 TO TOT-NO-PASS
           MOVE STUD-FAIL4 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF8.

       FFTH-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'HIST 1000 ' AND
               FAC-ID2 = '45678       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF9.
               READ ACCT4.
               PERFORM FFTH-GRD-COMP UNTIL EOF9 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       FFTH-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM FFTH-GRD-FIN UNTIL EOF10 = 'NO'
               PERFORM FFTH-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF9.
       FFTH-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS5
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL5.
           READ ACCT4 AT END MOVE 'NO' TO EOF10.
       FFTH-GRD-BRK.
           MOVE STUD-PASS5 TO TOT-NO-PASS
           MOVE STUD-FAIL5 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF10.

       SXTH-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'IT 2003   ' AND
               FAC-ID2 = '56789       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF11.
               READ ACCT4.
               PERFORM SXTH-GRD-COMP UNTIL EOF11 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       SXTH-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM SXTH-GRD-FIN UNTIL EOF12 = 'NO'
               PERFORM SXTH-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF11.
       SXTH-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
               ELSE
                   MOVE 'FAILED ' TO RMRK.
           READ ACCT4 AT END MOVE 'NO' TO EOF12.
       SXTH-GRD-BRK.
           IF AVE-GRD <= 3.12
               ADD 1 TO STUD-PASS6
           ELSE
               ADD 1 TO STUD-FAIL6.
           MOVE STUD-PASS6 TO TOT-NO-PASS.
           MOVE STUD-FAIL6 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF12.

       SVNTH-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'MATH 1000 ' AND
               FAC-ID2 = '89012       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF13.
               READ ACCT4.
               PERFORM SVNTH-GRD-COMP UNTIL EOF13 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       SVNTH-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM SVNTH-GRD-FIN UNTIL EOF14 = 'NO'
               PERFORM SVNTH-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF13.
       SVNTH-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS7
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL7.
           READ ACCT4 AT END MOVE 'NO' TO EOF14.
       SVNTH-GRD-BRK.
           MOVE STUD-PASS7 TO TOT-NO-PASS
           MOVE STUD-FAIL7 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF14.

       EGHT-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'NATSCI 100' AND
               FAC-ID2 = '12345       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF15.
               READ ACCT4.
               PERFORM EGHT-GRD-COMP UNTIL EOF15 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       EGHT-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM EGHT-GRD-FIN UNTIL EOF16 = 'NO'
               PERFORM EGHT-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF15.
       EGHT-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS8
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL8.
           READ ACCT4 AT END MOVE 'NO' TO EOF16.
       EGHT-GRD-BRK.
           MOVE STUD-PASS8 TO TOT-NO-PASS
           MOVE STUD-FAIL8 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF16.

       NNTH-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'COMP 2000 ' AND
               FAC-ID2 = '34567       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF17.
               READ ACCT4.
               PERFORM NNTH-GRD-COMP UNTIL EOF17 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       NNTH-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM NNTH-GRD-FIN UNTIL EOF18 = 'NO'
               PERFORM NNTH-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF17.
       NNTH-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS9
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL9.
           READ ACCT4 AT END MOVE 'NO' TO EOF18.
       NNTH-GRD-BRK.
           MOVE STUD-PASS9 TO TOT-NO-PASS
           MOVE STUD-FAIL9 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF18.

       TNTH-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'IT 2001   ' AND
               FAC-ID2 = '56789       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF19.
               READ ACCT4.
               PERFORM TNTH-GRD-COMP UNTIL EOF19 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       TNTH-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM TNTH-GRD-FIN UNTIL EOF20 = 'NO'
               PERFORM TNTH-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF19.
       TNTH-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS10
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL10.
           READ ACCT4 AT END MOVE 'NO' TO EOF20.
       TNTH-GRD-BRK.
           MOVE STUD-PASS10 TO TOT-NO-PASS
           MOVE STUD-FAIL10 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF20.

       ELVN-GRD-PRCS.
           READ ACCT2.
           IF SUBJ-CODE2 = 'MATH 1000 ' AND
               FAC-ID2 = '78901       '
               MOVE SUBJ-CODE2 TO SUB-CD.
               MOVE FAC-ID2 TO FACULTY-ID.
               MOVE FAC-NAME TO FACULTY-NAME.
               WRITE GRADES-OUTPUT FROM GRD-HDR.
               WRITE GRADES-OUTPUT FROM GRD-HDR2.
               WRITE GRADES-OUTPUT FROM GRD-HDR3.
               WRITE GRADES-OUTPUT FROM GRD-HDR4.
               READ ACCT3 AT END MOVE 'YES' TO EOF21.
               READ ACCT4.
               PERFORM ELVN-GRD-COMP UNTIL EOF21 = 'YES'.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
               WRITE GRADES-OUTPUT FROM TOT-NUM-PASS.
               WRITE GRADES-OUTPUT FROM TOT-NUM-FAIL.
               WRITE GRADES-OUTPUT FROM BLNK-HDR.
       ELVN-GRD-COMP.
           IF SUBJ-CODE3 = SUB-CD AND FAC-ID3 = FACULTY-ID
               MOVE STUD-NUM3 TO STUDENT-NO2
               MOVE STUD-NAME3 TO STUDENT-NA2
               PERFORM ELVN-GRD-FIN UNTIL EOF22 = 'NO'
               PERFORM ELVN-GRD-BRK
               OPEN INPUT ACCT4.
           READ ACCT3 AT END MOVE 'YES' TO EOF21.
       ELVN-GRD-FIN.
           IF SUBJ-CODE4 = SUB-CD AND FAC-ID4 = FACULTY-ID AND
               STUD-NUM4 = STUDENT-NO2
               COMPUTE AVE-GRD = (MID-GRADE + FIN-GRADE) / 2
               MOVE AVE-GRD TO AVERAGE-GRD
               IF AVE-GRD <= 3.12
                   MOVE 'PASSED ' TO RMRK
                   ADD 1 TO STUD-PASS11
               ELSE
                   MOVE 'FAILED ' TO RMRK
                   ADD 1 TO STUD-FAIL11.
           READ ACCT4 AT END MOVE 'NO' TO EOF22.
       ELVN-GRD-BRK.
           MOVE STUD-PASS11 TO TOT-NO-PASS
           MOVE STUD-FAIL11 TO TOT-NO-FAIL.
           WRITE GRADES-OUTPUT FROM GRADE-INFO.
           CLOSE ACCT4.
           MOVE 'YES' TO EOF22.
