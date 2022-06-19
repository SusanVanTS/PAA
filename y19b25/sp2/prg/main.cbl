       IDENTIFICATION DIVISION.
	PROGRAM-ID.   MAIN.

      * MAIN MENU
      * AUTHOR	 	 DATE  	 TYPE	 A/C	 NOTES	
      * VAN TZE SHAN	 1/8/19  -   	 PA 	 SP1

       ENVIRONMENT DIVISION.
       DATA DIVISION.
	WORKING-STORAGE SECTION.
	   COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.
       
       78 T-SIZE               VALUE 08.

       01 WS-REC.
          03 WS-NAME           PIC X(30).
	  03 WS-PATH           PIC X(30).

       01 WS-NAMES.
	  03 FIL               PIC X(60) VALUE 
	     'DB Creation                   /z/y19b25/sp2/prg/crmenu'.
          03 FIL               PIC X(60) VALUE
	     'System Configuration          /z/y19b25/sp2/prg/scmenu'.
          03 FIL               PIC X(60) VALUE
	     'Setup Maintenance             /z/y19b25/sp2/prg/stmenu'.
          03 FIL               PIC X(60) VALUE
	     'Management Report             /z/y19b25/sp2/prg/rpmenu'.
          03 FIL               PIC X(60) VALUE SPACES.

       01 REDEFINES WS-NAMES OCCURS 08.
	  03 WS-RDF-NAMES      PIC X(30).
	  03 WS-RDF-PATH       PIC X(30).

       01 LINK-PROG-KEY        PIC X(30).

       SCREEN SECTION.
       01 MAIN-SCR.
	  03 LIST-1 LIST-BOX USING WS-REC PAGED 3-D
	     LINE 1.5 COL 03 SIZE 62 CELL LINES T-SIZE
	     DATA-COLUMNS      = (1,31)
	     DISPLAY-COLUMNS   = (1)
	     EXCEPTION-VALUE W-DBLCLICK.
          03 PUSH-BUTTON 'Select'
	     LINE 7 COL 18 SIZE 12 LINES 1.2
	     TERMINATION-VALUE 13.
	  03 PUSH-BUTTON 'Exit'
	     LINE 7 COL + 3 SIZE 12 LINES 1.2
	     TERMINATION-VALUE 27.
      ****************************************************************
       PROCEDURE DIVISION.

	MAIN-LOGIC.
	   MOVE 'Y' TO S-RUN.
	   MOVE 'N' TO S-STATUS-CHECK.
	   COPY   '/v/cps/lib/std/gtcoid.prd'.
	   CALL   '/z/y19b25/sp2/lib/std/x-scrcp' USING S-WINDOW.
	   CANCEL '/z/y19b25/sp2/lib/std/x-scrcp'.
           SET ENVIRONMENT 'PA-USER-ID' TO 'y19b25'.

           DISPLAY FLOATING WINDOW LINES 7.6 SIZE 66 COLOR 65793
	   CELL SIZE = ENTRY-FIELD FONT SEPARATE
	   TITLE-BAR MODAL NO SCROLL NO WRAP
	   TITLE 'Student Project 2'
	   POP-UP S-WINDOW2.
	   PERFORM LIST-1-RTN THRU LIST-1-END.

        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   EXIT PROGRAM.
	   STOP RUN.

      ***************************************************************
       LIST-1-RTN.
       
	   DISPLAY MAIN-SCR.
	   MODIFY LIST-1, MASS-UPDATE = 1.
	   PERFORM VARYING S-SUB FROM 1 BY 1 UNTIL S-SUB > 08
		   MOVE WS-RDF-NAMES (S-SUB) TO WS-NAME 
		   MOVE WS-RDF-PATH  (S-SUB) TO WS-PATH
		   MODIFY LIST-1, ITEM-TO-ADD = WS-REC
           END-PERFORM.
	   MODIFY LIST-1, MASS-UPDATE = 0.

	   MODIFY LIST-1, QUERY-INDEX = 1.
	   INQUIRE LIST-1, ITEM-VALUE IN WS-REC.
	   DISPLAY MAIN-SCR.

	   PERFORM WITH TEST AFTER UNTIL K-ESCAPE
		   ACCEPT MAIN-SCR
		   IF K-ENTER OR K-L-DBLCLICK
		      INQUIRE LIST-1, SELECTION-INDEX IN C-SUB,
		      MODIFY  LIST-1, QUERY-INDEX = C-SUB,
		      INQUIRE LIST-1, ITEM-VALUE IN WS-REC
		      CALL WS-PATH USING WS-NAME
		      CANCEL WS-PATH
                   END-IF
           END-PERFORM.

       LIST-1-END. EXIT.

      * End of program.
