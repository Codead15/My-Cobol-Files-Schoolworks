       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALESMAN-TXT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALESMANFILE ASSIGN TO "SALESMAN.TXT"
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SALESMANFILE.
         01 SALESMANDETAILS.
         02 SALESNUM PIC X(12).
         02 SALESNAME PIC X(20).
         02 Q1SALE PIC 9(5)V99.
         02 Q2SALE PIC 9(5)V99.
         02 Q3SALE PIC 9(5)V99.
         02 Q4SALE PIC 9(5)V99.
       WORKING-STORAGE SECTION.
         01 ANSWER PIC A VALUE 'Y'.
         01 EOF-SWITCH PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN EXTEND SALESMANFILE.
           PERFORM INPUT-RTN UNTIL ANSWER = 'N'.
           CLOSE SALESMANFILE.
           PERFORM 100-INITIALIZE.
           PERFORM 200-PROCESS-RECORDS
               UNTIL EOF-SWITCH = 'Y'.
           CLOSE SALESMANFILE
           STOP RUN.

        INPUT-RTN.
           DISPLAY "ENTER SALESMAN NUMBER: "
           ACCEPT SALESNUM.
           DISPLAY "ENTER SALESMAN NAME: "
           ACCEPT SALESNAME.
           DISPLAY "Enter 1ST QUARTER SALES: "
           ACCEPT Q1SALE.
           DISPLAY "Enter 2ND QUARTER SALES: "
           ACCEPT Q2SALE.
           DISPLAY "Enter 3RD QUARTER SALES: "
           ACCEPT Q3SALE.
           DISPLAY "Enter 4TH QUARTER SALES: "
           ACCEPT Q4SALE.
           WRITE SALESMANDETAILS.
           DISPLAY " ".
           DISPLAY "Do you want to try again (Y/N)? "
           ACCEPT ANSWER.
           DISPLAY " ".

       100-INITIALIZE.
           OPEN INPUT SALESMANFILE.
           READ SALESMANFILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
           END-READ.

       200-PROCESS-RECORDS.
           DISPLAY "SALESMAN NUMBER: " SALESNUM
           DISPLAY "SALESMAN NAME: " SALESNAME
           DISPLAY "1ST QUARTER SALES: " Q1SALE
           DISPLAY "2ND QUARTER SALES ARE: " Q2SALE
           DISPLAY "3RD QUARTER SALES ARE: " Q3SALE
           DISPLAY "4TH QUARTER SALES ARE: " Q4SALE
           DISPLAY " "
           READ SALESMANFILE
               AT END
                   MOVE "Y" TO EOF-SWITCH
           END-READ.
