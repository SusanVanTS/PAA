       IDENTIFICATION DIVISION.
        PROGRAM-ID.   HPRC.

      * COUNTRY TABLE.
      * AUTHOR 		DATE	TYPE	A/C	NOTES
      * VAN TZE SHAN	2/8/19	-	PAA	CODING

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	   COPY '/z/y19b25/sp2/lib/fd/fcrc'.

       DATA DIVISION.
	FILE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/fdrc'.

       WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/dbrc'.
	   COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.

       78 T-SIZE		VALUE 20.

       01 WS-REC.
	  03 WS-RC-KEY		PIC X(04).
	  03 WS-RC-NAME		PIC X(30).

       01 WS-T-REC.
	  03 WS-T-RC-KEY	PIC X(04).

       LINKAGE SECTION.
       01 LINK-RC-KEY		PIC X(04).
       01 LINK-OK		PIC X(01).

       SCREEN SECTION.
       01 MAIN-SCR.
	  03 LABEL LINE 1.5 COL 03 'Code'.
	  03 LABEL COL 11 'Name'.
	  03 LIST-1 LIST-BOX USING WS-REC PAGED 3-D
	     LINE 2.5 COL 03 SIZE 60 CELL LINES T-SIZE
	     DATA-COLUMNS 	= (1,5,35)
	     DISPLAY-COLUMNS 	= (1,9)
	     SEPARATION 	= (10,10)
	     DIVIDERS		= (1)
	     SORT-ORDER		= (2)
	     EXCEPTION PROCEDURE LIST-1-RTN THRU LIST-1-END
	     EXCEPTION-VALUE W-DBLCLICK.
          COPY '/v/cps/lib/std/hpbtn.scr'.

      ********************************************************************
       PROCEDURE DIVISION USING LINK-RC-KEY, LINK-OK.

	DECLARATIVES.

	  COPY '/z/y19b25/sp2/lib/fd/dcrc'.

        END DECLARATIVES.

      ********************************************************************
        MAIN-LOGIC.
 
	   MOVE 'N' TO S-RUN, LINK-OK.
	   OPEN INPUT RC-FILE.

	   MOVE 'Y'		TO S-RUN.
	   MOVE 'Race Table'	TO S-WINDOW-TITLE.
	   COPY '/v/cps/lib/std/hpwin.prd'.
	   DISPLAY MAIN-SCR.

      * Get initial page by searching the 1st record.
	   INITIALIZE RC-KEY, WS-T-REC.
	   SET K-EVENT TO TRUE.
	   SET E-SEARCH TO TRUE.
	   PERFORM LIST-1-RTN THRU LIST-1-END.
	   PERFORM WITH TEST AFTER
	  	   UNTIL K-ENTER OR K-ESCAPE OR K-L-DBLCLICK
		   ACCEPT MAIN-SCR
           END-PERFORM.

	   IF K-ENTER OR K-L-DBLCLICK
	      INQUIRE LIST-1, SELECTION-INDEX IN C-SUB
	      MODIFY  LIST-1, QUERY-INDEX = C-SUB
	      INQUIRE LIST-1, ITEM-VALUE IN WS-REC
	      IF WS-RC-KEY NOT = SPACES
		 MOVE WS-RC-KEY TO LINK-RC-KEY
	 	 MOVE 'Y' TO LINK-OK.

        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   CLOSE RC-FILE.
	   EXIT PROGRAM.
	   STOP RUN.

      ********************************************************************
        LIST-1-RTN.
	
	   IF NOT K-EVENT GO TO LIST-1-END

	   INQUIRE LIST-1, SELECTION-INDEX IN C-SUB.
	  
	   IF NOT (E-UP OR E-PAGEUP OR E-DOWN OR E-PAGEDOWN OR E-SEARCH)
	      GO TO LIST-1-END.
           IF E-UP OR E-PAGEUP
	      MOVE 1 TO S-SUB 
	   ELSE
	      MOVE T-SIZE TO S-SUB.

      * Get start key.
           INITIALIZE WS-T-REC.
	   IF E-SEARCH
	      INQUIRE LIST-1, SEARCH-TEXT IN WS-T-RC-KEY
           ELSE
	      MODIFY LIST-1, QUERY-INDEX = S-SUB,
	      INQUIRE LIST-1, ITEM-VALUE IN WS-T-REC
	      IF WS-T-RC-KEY = SPACES
		 GO TO LIST-1-END.

      * Start file.
	   MOVE WS-T-RC-KEY TO RC-KEY.
	   IF E-UP OR E-PAGEUP
	      START RC-FILE KEY < RC-KEY INVALID
		    GO TO LIST-1-END

           ELSE
	      IF E-SEARCH
		 START RC-FILE KEY >= RC-KEY INVALID
		       GO TO LIST-1-END
              ELSE
		 START RC-FILE KEY > RC-KEY INVALID
		       GO TO LIST-1-END.

      * Determine # of records to get.
	   IF E-UP OR E-DOWN
	      MOVE 1 TO R-COUNT
           ELSE
	      MOVE T-SIZE TO R-COUNT.

      * Get records onto list.
	   MODIFY LIST-1, MASS-UPDATE = 1.
	   PERFORM GET-REC THRU GET-REC-END
		   VARYING S-SUB FROM 1 BY 1 UNTIL S-SUB > R-COUNT.
           MODIFY LIST-1, MASS-UPDATE = 0.

	   MODIFY LIST-1, QUERY-INDEX = C-SUB.
	   INQUIRE LIST-1, ITEM-VALUE IN WS-REC.
	   DISPLAY MAIN-SCR.

       LIST-1-END. EXIT.
      ********************************************************************
        GET-REC.
	  
	   IF E-UP OR E-PAGEUP
	      READ RC-FILE PREVIOUS END
		   MOVE R-COUNT TO S-SUB GO TO GET-REC-END
           ELSE
	      READ RC-FILE NEXT END
		   MOVE R-COUNT TO S-SUB GO TO GET-REC-END.

      * Clear list if valid search.
           IF  (E-SEARCH OR E-PAGEDOWN) AND S-SUB = 1
	       MODIFY LIST-1, RESET-LIST = 1.
          
	   INITIALIZE WS-REC.
	   MOVE RC-KEY TO WS-RC-KEY.
	   MOVE RC-NAME TO WS-RC-NAME.

      * Insert to top/bottom of the list.
           MOVE 1 TO C-SUB.
	   IF E-PAGEUP OR E-UP
              MODIFY LIST-1, INSERTION-INDEX = 1, ITEM-TO-ADD = WS-REC
           ELSE
	      MODIFY LIST-1, ITEM-TO-ADD = WS-REC
	      IF E-DOWN
	 	 MOVE T-SIZE TO C-SUB.

        GET-REC-END. EXIT.

      **********************************************************************

      * End of program.
      
