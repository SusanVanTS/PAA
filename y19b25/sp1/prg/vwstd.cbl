       IDENTIFICATION DIVISION.
	PROGRAM-ID.    VWSTD.

      * VIEW COUNTRY FILE.
      * AUTHOR 		DATE	TYPE 	A/C	NOTES
      * VAN TZE SHAN	6/8/19	-	PAA	CODING
       
       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	  COPY '/z/y19b25/sp1/lib/fd/fcstd'.

       DATA DIVISION.
	FILE SECTION.
	  COPY '/z/y19b25/sp1/lib/fd/fdstd'.

        WORKING-STORAGE SECTION.
	  COPY '/z/y19b25/sp1/lib/fd/dbstd'.
	  COPY '/v/cps/lib/std/stdvar.def'.
	  COPY '/v/cps/lib/std/fkey.def'.

       78 T-SIZE		VALUE 30.

       01 WS-REC.
	  03 WS-STD-KEY		PIC X(06).
	  03 WS-STD-NAME	PIC X(40).
	  03 WS-STD-GENDER	PIC X(06).
	  03 WS-STD-DOB-DMY	PIC 99/99/9999.
	  03 WS-STD-CY-KEY	PIC X(04).
	  03 WS-STD-RC-KEY	PIC X(04).

       01 WS-TEMP.
	  03 WS-T-STD-KEY	PIC X(06). 

       LINKAGE SECTION.
       01 LINK-PROG-KEY		PIC X(30).

       SCREEN SECTION.
       01 MAIN-SCR.
	  03 LABEL LINE 02 COL 04 'Code'.
	  03 LABEL COL 14 'Name'.
	  03 LABEL COL + 45 'Gender'.
          03 LABEL COL + 3 'DOB'.
	  03 LABEL COL + 8 'Country'.
	  03 LABEL COL + 2 'Race'.
	  03 LIST-1 LIST-BOX USING WS-REC PAGED 3-D
	     LINE 03 COL 04 SIZE 106 CELL LINES T-SIZE
	     DATA-COLUMNS	= (1,7,46,53,63,66,72,86) 
	     DISPLAY-COLUMNS	= (1,10,59,67,78,85) 
	     DIVIDERS		= (1,1,1,1,1)
	     SORT-ORDER		= (2)
	     EXCEPTION PROCEDURE LIST-1-RTN THRU LIST-1-END
	     EXCEPTION-VALUE W-DBLCLICK.

      ********************************************************************
       PROCEDURE DIVISION USING LINK-PROG-KEY.

	DECLARATIVES.

	   COPY '/z/y19b25/sp1/lib/fd/dcstd'.

        END DECLARATIVES.
      ********************************************************************
        MAIN-LOGIC.

	   MOVE 'N' TO S-RUN.
	   OPEN INPUT STD-FILE.

	   MOVE 'Y' 			TO S-RUN
	   MOVE 'View & Print Student'  TO S-WINDOW-TITLE.
	   COPY '/v/cps/lib/std/floatwin.prd'.

	   PERFORM FKEY-RTN THRU FKEY-END.
	   DISPLAY MAIN-SCR.

      * Get initialize page.
	   INITIALIZE STD-REC, WS-TEMP.
	   SET K-EVENT TO TRUE.
	   SET E-SEARCH TO TRUE.
	   PERFORM LIST-1-RTN THRU LIST-1-END.
	   PERFORM WITH TEST AFTER UNTIL K-ESCAPE
		   ACCEPT MAIN-SCR
		   IF K-F6
	    	      CALL   '/z/y19b25/sp1/prg/ptstd' USING
		             LINK-PROG-KEY
                      CANCEL '/z/y19b25/sp1/prg/ptstd'
                   END-IF
           END-PERFORM.
	
        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   CLOSE STD-FILE.
	   EXIT PROGRAM.
	   STOP RUN.
      ********************************************************************
        LIST-1-RTN.

	   IF NOT K-EVENT GO TO LIST-1-END.

	   INQUIRE LIST-1, SELECTION-INDEX IN C-SUB.
 
	   IF NOT (E-UP OR E-PAGEUP OR E-DOWN OR E-PAGEDOWN OR E-SEARCH)
              GO TO LIST-1-END.
 
	   IF E-UP OR E-PAGEUP
              MOVE 1 TO S-SUB
	   ELSE
              MOVE T-SIZE TO S-SUB.

      * Get start key.
	   INITIALIZE WS-TEMP.
	   IF E-SEARCH
	      INQUIRE LIST-1, SEARCH-TEXT IN WS-T-STD-KEY 
	   ELSE
              MODIFY LIST-1, QUERY-INDEX = S-SUB,
	      INQUIRE LIST-1, ITEM-VALUE IN WS-TEMP
	      IF WS-T-STD-KEY = SPACES
		 GO TO LIST-1-END.

      * Start file
	   MOVE WS-T-STD-KEY TO STD-KEY.
	   IF E-UP OR E-PAGEUP
	      START STD-FILE KEY < STD-KEY INVALID
		    GO TO LIST-1-END

           ELSE 
	      START STD-FILE KEY > STD-KEY INVALID
		    GO TO LIST-1-END.

      * Determine # of records to get.
	   IF E-UP OR E-DOWN
	      MOVE 1	TO R-COUNT
           ELSE
	      MOVE T-SIZE TO R-COUNT.

      * Get Records onto list..
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
	      READ STD-FILE PREVIOUS END
		   MOVE R-COUNT TO S-SUB GO TO GET-REC-END
	   ELSE
	      READ STD-FILE NEXT END
		   MOVE R-COUNT TO S-SUB GO TO GET-REC-END.

      * Clear list if valid search.
           IF (E-SEARCH OR E-PAGEDOWN) AND S-SUB = 1
	      MODIFY LIST-1, RESET-LIST = 1.

           INITIALIZE WS-REC.
	   MOVE STD-KEY	TO WS-STD-KEY.
	   MOVE STD-NAME TO WS-STD-NAME.
	   EVALUATE STD-GENDER
	    WHEN 'M' MOVE 'Male  ' TO WS-STD-GENDER
	    WHEN 'F' MOVE 'Female' TO WS-STD-GENDER.
      *    MOVE STD-GENDER TO WS-STD-GENDER.
	   MOVE STD-DOB-DMY TO WS-STD-DOB-DMY.
	   MOVE STD-CY-KEY TO WS-STD-CY-KEY.
	   MOVE STD-RC-KEY TO WS-STD-RC-KEY.

      * Insert to top/bottom of the list.
	   MOVE 1 TO C-SUB.
	   IF E-PAGEUP OR E-UP
	      MODIFY LIST-1, INSERTION-INDEX = 1, ITEM-TO-ADD = WS-REC
	   ELSE
	      MODIFY LIST-1, ITEM-TO-ADD  = WS-REC
	      IF E-DOWN
		 MOVE T-SIZE TO C-SUB.

       GET-REC-END. EXIT.
      ********************************************************************
       FKEY-RTN.

           CALL	'/v/cps/lib/std/x-fkey' USING
		'12345y7890123467890', S-TOOLBAR, S-BUTTON.
          CANCEL'/v/cps/lib/std/x-fkey'.

       FKEY-END. EXIT.

      *********************************************************************

      * End of program.

