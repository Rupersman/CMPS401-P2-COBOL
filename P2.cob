       *>  Your program reads an input file (P2In.dat). It shows the 
       *>  outputs on an output file (P2Out.dat) and on the screen. 
       *>  The following headings appear on your outputs:
       *>  a. Your name
       *>  b. Student W number
       *>  c. COURSE TITLE, GR, EARNED, QPTS
       *>  d. SEMESTER
       *>  e. CUMULATIVE

       *>  Program-ID: P2.cob
       *>  Author: Paogrammers
       *>  OS: Ubuntu 20
       *>  Compiler: OpenCOBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P2.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT myInFile ASSIGN TO "P2In.dat".
       SELECT myOutFile ASSIGN TO "P2Out.dat".
       DATA DIVISION.

       FILE SECTION.

       FD myInFile.
       01 inRecord.
           02 sName PIC X(18). *>Student Name
           02 wNum PIC X(8). *>Student W#
           02 sem PIC X(11). *>Course semester
           02 course PIC X(10). *>Course 
           02 cTitle PIC X(28). *>Course title
           02 grade PIC X. *>Course grade
           02 credits PIC 9. *>Course credits
           02 finalLine   PIC X. *>Final line Needed!!!!
       
       FD myOutFile.
       01 outRecord.
           02 txt PIC X(71).
           02 finalLine2 PIC X.

       WORKING-STORAGE SECTION.

           01 w PIC X(3) VALUE "YES".
           01 currSem PIC X(11).
           *>A=4, B=3, C=2, D=1
           01 gradeFac PIC 9.
           *>qpts = gradeFac * credits
           01 qpts PIC 99.
           01 zQpts PIC ZZ.
           01 semEarned PIC 99.
           01 cumEarned PIC 99.
           01 semQpts PIC 99.
           01 cumQpts PIC 999.
           01 zCumQpts PIC ZZZ.
           01 semGPA PIC 9.99.
           01 cumGPA PIC 9.99.
      
           01 head1 PIC X(30) VALUE "                       SOUTHEA".
           01 head2 PIC X(26) VALUE "STERN LOUISIANA UNIVERSITY".
           01 head3 PIC X(31) VALUE "                               ".
           01 head4 PIC X(17) VALUE "HAMMOND, LA 70402".

       PROCEDURE DIVISION.
           OPEN INPUT myInFile.
           OPEN OUTPUT myOutFile.

           PERFORM subRead *> First subread
           MOVE finalLine to finalLine2
           *>Main heading
           DISPLAY head1, head2 

           STRING  head1
                   head2
                   "                      "
           into txt
           END-STRING
           WRITE outRecord 

           Display head3, head4
           
           MOVE " " to txt
           STRING  head3
                   head4
                   "       "
           into txt
           END-STRING
           WRITE outRecord
           
           DISPLAY sName
           
           MOVE " " to txt
           STRING  sName
                   "             "
           into txt
           END-STRING
           WRITE outRecord 

           DISPLAY wNum

           MOVE " " to txt
           STRING  wNum
                   "                "
                       finalLine2
           into txt
           END-STRING
           WRITE outRecord

           DISPLAY " "

           *>WRITE outRecord from finalLine2

           MOVE sem to currSem
           DISPLAY currSem

           MOVE " " to txt
           STRING  currSem
                   "                "
           into txt
           END-STRING
           WRITE outRecord

           *>Display heading
           DISPLAY "COURSE    TITLE               " WITH NO ADVANCING
           DISPLAY "         GR    EARNED      QPTS"
           
           MOVE " " to txt
           STRING  "COURSE    TITLE               "
                   "         GR    EARNED      QPTS"
           into txt
           END-STRING
           WRITE outRecord

           PERFORM UNTIL w = "NO"
               *> Loop block
               *> Perform functions and display
               
               IF sem IS  NOT EQUAL TO currSem
                   *>If we have read in a new semester display the 
                   *>semester and cumulative credits, qpts, and gpa
                   PERFORM finishSemester
                   MOVE sem to currSem
                   DISPLAY " "
                   DISPLAY sem

                   MOVE " " to txt
                   STRING  finalLine2
                           sem
                           "                "
                   into txt
                   END-STRING
                   WRITE outRecord
               END-IF
           
               PERFORM semesterDisplay    
               
               PERFORM subRead
           END-PERFORM.

           PERFORM finishSemester
           CLOSE myInFile.
           CLOSE myOutFile.
           STOP RUN.
       subRead.
           READ myInFile
           AT END 
               MOVE "NO" TO w
           *>NOT AT END
           END-READ.

       *>Displays the semester class 
       semesterDisplay.
           *>Calculate qpts
           IF grade IS EQUAL TO "A"
               MOVE 4 to gradeFac
           ELSE IF grade IS EQUAL TO "B"
               MOVE 3 to gradeFac
           ELSE IF grade IS EQUAL TO "C"
               MOVE 2 to gradeFac
           Else 
               MOVE 1 to gradeFac
           END-IF.

           COMPUTE qpts = gradeFac * credits.
           MOVE qpts to zQpts.

           DISPLAY course, cTitle, " ", grade, "     " WITH NO ADVANCING
           DISPLAY credits, ".00        " WITH NO ADVANCING
           DISPLAY zQpts, ".00".
           
           MOVE " " to txt
           STRING  course
                   cTitle
                   " "
                   grade
                   "     "
                   credits
                   ".00        "
                   zQpts
                   ".00"
           into txt
           END-STRING
           WRITE outRecord

           COMPUTE semEarned = semEarned + credits.
           COMPUTE semQpts = semQpts + qpts.
           

       *>If the next semester is different from the previous one, then 
       *>display the total semester and cummulative points and 
       *>calculate the semester and cummulative gpa

       finishSemester.
           COMPUTE cumEarned = cumEarned + semEarned.
           COMPUTE cumQpts = cumQpts + semQpts.

           MOVE cumQpts to zCumQpts.

           COMPUTE semGPA = semQpts / semEarned.
           COMPUTE cumGPA = cumQpts / cumEarned.
           DISPLAY "                         SEMESTER" WITH NO ADVANCING
           DISPLAY "           ", semEarned, ".00" WITH NO ADVANCING
           DISPLAY "        ", semQpts, ".00     " WITH NO ADVANCING
           DISPLAY semGPA

           MOVE " " to txt
           STRING  "                         SEMESTER"
                   "           "
                   semEarned
                   ".00        "
                   semQpts
                   ".00     "
           into txt
           END-STRING
           WRITE outRecord

           DISPLAY "                         " WITH NO ADVANCING
           DISPLAY "CUMMULATIVE        " WITH NO ADVANCING
           DISPLAY cumEarned, ".00       " WITH NO ADVANCING
           DISPLAY zCumQpts, ".00     " WITH NO ADVANCING
           DISPLAY cumGPA.

           MOVE " " to txt
           STRING  "                         CUMMULATIVE        "
                   cumEarned
                   ".00       "
                   zCumQpts
                   ".00     "
                   cumGPA
           into txt
           END-STRING
           WRITE outRecord

           MOVE 0 to semEarned.
           MOVE 0 to semQpts.