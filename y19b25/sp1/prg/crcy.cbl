       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CRCY.

      * CREATE COUNTRY DATA FILE
      * AUTHOR		DATE	TYPE	A/C	NOTES
      * VAN TZE SHAN	2/8/19	-	PAA	SP1

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	   COPY '/z/y19b25/sp1/lib/fd/fccy'.
	   
       DATA DIVISION.
	FILE SECTION.
      $XFD FILE = cy
	   COPY '/z/y19b25/sp1/lib/fd/fdcy'.

        WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp1/lib/fd/dbcy'.

	01 WS-COMMAND 		PIC X(80).
	01 S-WINDOW		PIC X(10).
	01 S-WINDOW2 		PIC X(10).
	01 S-WINDOW3		PIC X(10).
	01 S-PAUSE		PIC X(01).
	01 S-STATUS-CHECK	PIC X(01).
	01 WS-STATUS		PIC 9(02) VALUE ZEROS.
	01 S-ANSWER		PIC X(01) VALUE 'N'.
	   88 VALID-ANSWER	VALUE 'Y', 'y', 'N', 'n'.
        01 FKEY			PIC 9(03).
	   88 FKEY-ENTER	VALUE 013.
	   88 FKEY-ESCAPE	VALUE 027.

       SCREEN SECTION.
       01 SELECT-SCR.
	  03 LINE 02 COL 02 'File :'.
	  03 COL + 2 PIC X(40) FROM CY-DATANAME.
	  03 LINE 03 COL 02 'Ok to proceed? [ ] [Y/N]'.
	  03 LINE 03 COL 19 PIC X(01) USING S-ANSWER BELL UPPER.

       01 PAUSE-SCR.
	  03 LINE 02 COL 02 'Press any key to continue...'.
	  03 COL + 2 PIC X(01) USING S-PAUSE AUTO OFF BELL.

      ********************************************************************
       PROCEDURE DIVISION.

	DECLARATIVES.

	CY-FILE-HANDLING SECTION.
	   USE AFTER STANDARD ERROR PROCEDURE ON CY-FILE.
	   CALL	  '/v/cps/lib/std/f-status' USING CY-DATA.
	   CANCEL '/v/cps/lib/std/f-status'.
	   MOVE   'Y' TO S-STATUS-CHECK.

       END DECLARATIVES.

      ********************************************************************
        BEGIN.

	   DISPLAY FLOATING WINDOW LINES 8 SIZE 60
	   CELL SIZE = ENTRY-FIELD FONT SEPARATE

	   TITLE 'Country File Creation'
           POP-UP S-WINDOW.

        ACCEPT-RTN.
	   DISPLAY SELECT-SCR.
	   ACCEPT SELECT-SCR.
	   ACCEPT FKEY FROM ESCAPE KEY.

	   IF FKEY-ESCAPE GO TO TERMINATION.
	   IF NOT (FKEY-ENTER AND VALID-ANSWER) GO TO ACCEPT-RTN.

	   IF S-ANSWER NOT = 'Y'
	      GO TO TERMINATION.

           OPEN OUTPUT CY-FILE.
	   CLOSE CY-FILE.
	   IF S-STATUS-CHECK = 'Y' GO TO TERMINATION.

      * Call to change Mode & Group.
	   DISPLAY WINDOW AT 0101 SIZE 80 LINES 24
	   POP-UP S-WINDOW2
	   DISPLAY OMITTED AT 1516.

	   STRING 'chmod 775', CY-DATANAME
	 	  DELIMITED BY SIZE INTO WS-COMMAND.
           CALL   'SYSTEM' USING WS-COMMAND GIVING WS-STATUS.
	   CANCEL 'SYSTEM'.
	   IF WS-STATUS NOT = ZEROS
	      DISPLAY PAUSE-SCR
	      ACCEPT PAUSE-SCR
	      GO TO BEGIN-SUB.

           STRING 'chgrp cps', CY-DATANAME
		  DELIMITED BY SIZE INTO WS-COMMAND.
	   CALL   'SYSTEM' USING WS-COMMAND GIVING WS-STATUS.
	   CANCEL 'SYSTEM'.
	   IF WS-STATUS NOT = ZEROS
	      DISPLAY PAUSE-SCR
	      ACCEPT  PAUSE-SCR.

        BEGIN-SUB.

	   CLOSE WINDOW S-WINDOW2.

        TERMINATION.

	   CLOSE WINDOW S-WINDOW.
	   EXIT PROGRAM.
	   STOP RUN.

      * End of program.
