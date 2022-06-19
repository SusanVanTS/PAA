       IDENTIFICATION DIVISION.
	PROGRAM-ID.    VWSTD.

      * VIEW COUNTRY FILE.
      * AUTHOR 		DATE	TYPE 	A/C	NOTES
      * VAN TZE SHAN	6/9/19	-	PAA	CODING
       
       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	  COPY '/z/y19b25/sp2/lib/fd/fcstd'.
	  COPY '/z/y19b25/sp2/lib/fd/fccy'.
	  COPY '/z/y19b25/sp2/lib/fd/fcrc'.
	  COPY '/z/y19b25/sp2/lib/fd/fcrg'.

       DATA DIVISION.
	FILE SECTION.
	  COPY '/z/y19b25/sp2/lib/fd/fdstd'.
	  COPY '/z/y19b25/sp2/lib/fd/fdcy'.
	  COPY '/z/y19b25/sp2/lib/fd/fdrc'.
	  COPY '/z/y19b25/sp2/lib/fd/fdrg'.

        WORKING-STORAGE SECTION.
	  COPY '/z/y19b25/sp2/lib/fd/dbstd'.
	  COPY '/z/y19b25/sp2/lib/fd/dbcy'.
	  COPY '/z/y19b25/sp2/lib/fd/dbrc'.
	  COPY '/z/y19b25/sp2/lib/fd/dbrg'.
	  COPY '/v/cps/lib/std/stdvar.def'.
	  COPY '/v/cps/lib/std/fkey.def'.

       78 T-SIZE		VALUE 10.

       01 WS-REC.
	  03 WS-STD-KEY		PIC X(06).
	  03 WS-STD-NAME	PIC X(40).
	  03 WS-STD-GENDER	PIC X(06).
	  03 WS-STD-CY-KEY	PIC X(04).
	  03 WS-STD-RC-KEY	PIC X(04).
	  03 WS-STD-RG-KEY	PIC X(04).
	  03 WS-STD-DOB-DMY	PIC 99/99/9999.
	  03 WS-STD-ADD1	PIC X(50).
	  03 WS-STD-ADD2	PIC X(50).
	  03 WS-STD-ADD3	PIC X(50).
	  03 WS-STD-HEIGHT	PIC 999V99.
	  03 WS-STD-WEIGHT	PIC 999V99.
	  03 WS-STD-EMAIL	PIC X(30).
	  03 WS-STD-MOBILE	PIC X(30).
	  03 WS-STD-CY-NAME	PIC X(40).
	  03 WS-STD-RC-NAME	PIC X(40).
	  03 WS-STD-RG-NAME	PIC X(40).
   
       01 WS-TEMP.
	  03 WS-T-STD-KEY	PIC X(06). 

       LINKAGE SECTION.
       01 LINK-PROG-KEY		PIC X(30).

       SCREEN SECTION.
       01 MAIN-SCR.
	  03 LABEL LINE 02 COL 04 'Code'.
	  03 LABEL COL 13.5 'Name'.
	  03 LABEL COL + 45.5 'Gender'.
	  03 LABEL COL + 3.5 'Country Code'.
	  03 LABEL COL + 3 'Race Code'.
	  03 LABEL COL + 4 'Religion Code'.
	  03 LIST-1 LIST-BOX USING WS-REC PAGED 3-D
	     LINE 03 COL 04 SIZE 106 CELL LINES T-SIZE
	     NOTIFY-SELCHANGE 
	     DATA-COLUMNS	= (1,7,46,53,56,59,63,86) 
	     DISPLAY-COLUMNS	= (1,10,59,68,80,92) 
	     DIVIDERS		= (1,1,1,1,1)
	     SORT-ORDER		= (2)
	     EXCEPTION PROCEDURE LIST-1-RTN THRU LIST-1-END
             EXCEPTION-VALUE W-DBLCLICK.
       
       01 DIVIDE-SCR.
	  03 LABEL LINE 9.5 COL 04 'Address:'.
          03 ENTRY-FIELD 3-D COL 17 FROM WS-STD-ADD1.
          03 ENTRY-FIELD 3-D LINE + 1 COL 17
             FROM WS-STD-ADD2.
          03 ENTRY-FIELD 3-D LINE + 1 COL 17
             FROM WS-STD-ADD3.
          03 LABEL LINE + 1 COL 04 'Gender:'.
          03 ENTRY-FIELD 3-D COL 17 FROM WS-STD-GENDER.
          03 LABEL LINE + 1 COL 04 'Date of Birth:'.
          03 ENTRY-FIELD 3-D COL + 3 FROM WS-STD-DOB-DMY.
          03 LABEL LINE + 1 COL 04 'Height:'.
          03 ENTRY-FIELD 3-D COL 17 FROM WS-STD-HEIGHT.
          03 LABEL COL + 1.5 'cm'.
	  03 LABEL LINE + 1 COL 04 'Weight:'.
          03 ENTRY-FIELD 3-D COL 17 FROM WS-STD-WEIGHT.
          03 LABEL COL + 1.5 'kg'.
	  03 LABEL LINE + 1 COL 04 'Country:'.
          03 ENTRY-FIELD 3-D COL 17 FROM WS-STD-CY-NAME.
          03 LABEL LINE + 1 COL 04 'Race:'.
          03 ENTRY-FIELD 3-D COL 17 FROM WS-STD-RC-NAME.
          03 LABEL LINE + 1 COL 04 'Religion:'.
          03 ENTRY-FIELD 3-D COL 17 FROM WS-STD-RG-NAME.
          03 LABEL LINE + 1 COL 04 'Email#:'.
          03 ENTRY-FIELD 3-D COL 17 FROM WS-STD-EMAIL.
          03 LABEL LINE + 1 COL 04 'Mobile#:'.
          03 ENTRY-FIELD 3-D COL 17 FROM WS-STD-MOBILE.
      
      ********************************************************************
       PROCEDURE DIVISION USING LINK-PROG-KEY.
	
	DECLARATIVES.

	   COPY '/z/y19b25/sp2/lib/fd/dcstd'.
	   COPY '/z/y19b25/sp2/lib/fd/dccy'.
	   COPY '/z/y19b25/sp2/lib/fd/dcrc'.
	   COPY '/z/y19b25/sp2/lib/fd/dcrg'.

        END DECLARATIVES.
      ********************************************************************
        MAIN-LOGIC.

	   MOVE 'N' TO S-RUN.
	   OPEN INPUT STD-FILE, CY-FILE, RC-FILE, RG-FILE.

	   MOVE 'Y' 			TO S-RUN
	   MOVE 'View & Print Student'  TO S-WINDOW-TITLE.
	   COPY '/v/cps/lib/std/floatwin.prd'.

	   PERFORM FKEY-RTN THRU FKEY-END.
	   DISPLAY MAIN-SCR, DIVIDE-SCR.

      * Get initialize page.
	   INITIALIZE STD-REC, WS-TEMP.
	   SET K-EVENT TO TRUE.
	   SET E-SEARCH TO TRUE.
	   PERFORM LIST-1-RTN THRU LIST-1-END.
	   PERFORM WITH TEST AFTER UNTIL K-ESCAPE
		   ACCEPT MAIN-SCR
		   IF K-F6
	    	      CALL   '/z/y19b25/sp2/prg/ptstd' USING
		             LINK-PROG-KEY
                      CANCEL '/z/y19b25/sp2/prg/ptstd'
                   END-IF
           END-PERFORM.
	
        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   CLOSE STD-FILE, CY-FILE, RC-FILE, RG-FILE.
	   EXIT PROGRAM.
	   STOP RUN.
      ********************************************************************
        LIST-1-RTN.

	   IF NOT K-EVENT GO TO LIST-1-END.

	   INQUIRE LIST-1, SELECTION-INDEX IN C-SUB.
 
	   IF NOT (E-UP OR E-PAGEUP OR E-DOWN OR E-PAGEDOWN OR E-SEARCH)
              GO TO LIST-1-SUB.
 
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

       LIST-1-SUB.
           DISPLAY DIVIDE-SCR.

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
      *    MOVE STD-GENDER 	   TO WS-STD-GENDER.
	   MOVE STD-DOB-DMY 	   TO WS-STD-DOB-DMY.
	   MOVE STD-CY-KEY 	   TO WS-STD-CY-KEY.
	   MOVE STD-RC-KEY 	   TO WS-STD-RC-KEY.
	   MOVE STD-RG-KEY 	   TO WS-STD-RG-KEY.
	   MOVE STD-ADD1   	   TO WS-STD-ADD1.
	   MOVE STD-ADD2	   TO WS-STD-ADD2.
	   MOVE STD-ADD3	   TO WS-STD-ADD3.
	   MOVE STD-EMAIL	   TO WS-STD-EMAIL.
	   MOVE STD-MOBILE	   TO WS-STD-MOBILE.
	   MOVE STD-HEIGHT	   TO WS-STD-HEIGHT.
	   MOVE STD-WEIGHT	   TO WS-STD-WEIGHT.

	   INITIALIZE CY-REC.
	   MOVE STD-CY-KEY	TO CY-KEY.
	   READ CY-FILE INVALID
	      INITIALIZE CY-DETAILS.
	   MOVE CY-NAME TO WS-STD-CY-NAME.

	   INITIALIZE RC-REC.
	   MOVE STD-RC-KEY	TO RC-KEY.
	   READ RC-FILE INVALID
	      INITIALIZE RC-DETAILS.
	   MOVE RC-NAME TO WS-STD-RC-NAME.
	   
	   INITIALIZE RG-REC.
	   MOVE STD-RG-KEY	TO RG-KEY.
	   READ RG-FILE INVALID
	      INITIALIZE RG-DETAILS.
	   MOVE RG-NAME TO WS-STD-RG-NAME.
	   
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

