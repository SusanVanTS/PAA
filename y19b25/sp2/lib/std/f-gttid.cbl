       IDENTIFICATION DIVISION.
        PROGRAM-ID.   F-GTTID.

      * Get temp file name [yymdhmss].
      * Author     	Date     Remarks
      * Van Tze Shan 	5/9/19	 -

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           COPY '/z/y19b25/sp2/lib/fd/fclock'.

       DATA DIVISION.
        FILE SECTION.
           COPY '/z/y19b25/sp2/lib/fd/fdlock'. 

        WORKING-STORAGE SECTION.
           COPY '/z/y19b25/sp2/lib/fd/dblock'. 

        01 WS-TABLE.
           03 WS-CHARS                 PIC X(60) VALUE
              'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz12345
      -       '678'.
           03 WS-CHAR REDEFINES WS-CHARS
                                       PIC X(01) OCCURS 60.
        01 WS-MISC.
           03 WS-YYYYMMDD.
              05 WS-YY1                PIC 9(02).
              05 WS-YY2                PIC 9(02).
              05 WS-MTH                PIC 9(02).
              05 WS-DAY                PIC 9(02).
           03 WS-HHMMSSSS.
              05 WS-HH                 PIC 9(02).
              05 WS-MIN                PIC 9(02).
              05 WS-SS1                PIC 9(02).
              05 WS-SS2                PIC 9(02).

        01 S-RUN                       PIC X(01).
        01 S-STATUS-CHECK              PIC X(01).
              
        01 WS-MISC2.
           03 WS-SYSTEM-PATH           PIC X(20).
              88 EGR-PATH              VALUE '/v/cps', ' '.
           03 WS-COMMAND               PIC X(80).
           
        LINKAGE SECTION.
        01 LINK-DATA-ID                PIC X(08).

      ******************************************************************
       PROCEDURE DIVISION USING LINK-DATA-ID.

        DECLARATIVES.
           
        LOCK-FILE-HANDLING SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON LOCK-FILE.
           IF LOCK-STATUS NOT = 93
              CALL   '/v/cps/lib/std/f-status' USING LOCK-DATA
              CANCEL '/v/cps/lib/std/f-status'.
              IF S-RUN = 'Y' MOVE 'Y' TO S-STATUS-CHECK
              ELSE EXIT PROGRAM STOP RUN.

        END DECLARATIVES.

      ******************************************************************
        BEGIN.
 
           INITIALIZE LINK-DATA-ID, WS-MISC2.
           ACCEPT LINK-DATA-ID FROM ENVIRONMENT 'PA-TEMP-ID'.
           IF LINK-DATA-ID NOT = SPACES
              GO TO TERMINATION.

           MOVE 'N' TO S-RUN.

           ACCEPT WS-SYSTEM-PATH FROM ENVIRONMENT 'PA-SYSTEM-PATH'.
           IF NOT EGR-PATH
              STRING WS-SYSTEM-PATH, '/lib/std/c-gttm' 
                     DELIMITED BY ' ' INTO WS-COMMAND
              CALL   WS-COMMAND USING LINK-DATA-ID
              CANCEL WS-COMMAND
              GO TO TERMINATION.

        BEGIN-SUB.

           MOVE 'N' TO S-STATUS-CHECK.
           OPEN I-O LOCK-FILE WITH LOCK.
           IF S-STATUS-CHECK = 'Y'
              GO TO BEGIN-SUB.

           ACCEPT WS-YYYYMMDD FROM CENTURY-DATE.

        BEGIN-SUB2.

           ACCEPT WS-HHMMSSSS FROM TIME.
           IF WS-SS2 > 59
              GO TO BEGIN-SUB2.
 
           ADD 1 TO WS-HH, WS-MIN, WS-SS1, WS-SS2.

           STRING WS-YY2, WS-CHAR(WS-MTH), WS-CHAR(WS-DAY),
                  WS-CHAR(WS-HH), WS-CHAR(WS-MIN), WS-CHAR(WS-SS1),
                  WS-CHAR(WS-SS2) DELIMITED BY SIZE INTO LINK-DATA-ID.

      * Save Temp-ID onto cblconfig.
           SET ENVIRONMENT 'PA-TEMP-ID' TO LINK-DATA-ID.

           CLOSE LOCK-FILE.

        TERMINATION.
           EXIT PROGRAM.
           STOP RUN.

      * End of Program.
