       IDENTIFICATION DIVISION.
	PROGRAM-ID.   X-HPCAL.

      * DATA HELP TABLE
      * AUTHOR 		DATE 	TYPE	A/C	NOTES
      * VAN TZE SHAN	5/8/19	-	PAA	SP1
      
       ENVIRONMENT DIVISION.
	DATA DIVISION.
	WORKING-STORAGE SECTION.
	  COPY '/v/cps/lib/std/gridc.def'.
          COPY '/v/cps/lib/std/stdvar.def'.
	  COPY '/v/cps/lib/std/fkey.def'.
	  COPY RESOURCE '/v/cps/lib/icon/tool.bmp'.
	  COPY RESOURCE '/v/cps/lib/icon/date.bmp'.

       78 G-SIZE			VALUE 007.
       78 G-COL				VALUE 007.
       78 WS-19000101			VALUE 693975.
       78 WS-HEADER-COLOR		VALUE 000490.
       78 WS-SELECT-CELL-COLOR		VALUE 0000386.
       
       01 WS-DAYS.
	  03 FIL			PIC X(36) VALUE
	     '00003105909012015118121223273304334'.
       01 WS-DAY REDEFINES WS-DAYS	PIC 9(03) OCCURS 12.
       
       01 WS-DAY-OF-MONTH.
          03 FIL                        PIC X(36) VALUE
	     '031028031030031030031031030031030031'.
       01 WS-DOM REDEFINES WS-DAY-OF-MONTH
					PIC 9(03) OCCURS 12.
       01 WS-HEADER.
	  03 FIL                        PIC X(21) VALUE
	     'MonTueWedThuFriSatSun'.

       01 WS-HDR REDEFINES WS-HEADER 	PIC X(03) OCCURS G-SIZE.

       01 WS-MONTHS.
	  03 FIL			PIC X(09) VALUE "January".
	  03 FIL                        PIC X(09) VALUE "Febraury".
	  03 FIL                        PIC X(09) VALUE "March".
	  03 FIL                        PIC X(09) VALUE "April".
	  03 FIL                        PIC X(09) VALUE "May".
	  03 FIL                        PIC X(09) VALUE "June".
	  03 FIL                        PIC X(09) VALUE "July".
	  03 FIL                        PIC X(09) VALUE "August".
	  03 FIL                        PIC X(09) VALUE "September".
	  03 FIL                        PIC X(09) VALUE "October".
	  03 FIL                        PIC X(09) VALUE "November".
	  03 FIL                        PIC X(09) VALUE "December".
       01 WS-MTH REDEFINES WS-MONTHS	PIC X(09) OCCURS 12.

       01 WS-GRID.
	  03 WS-DD			PIC Z(03) OCCURS G-SIZE.
	  03 WS-YMD			PIC 9(08) OCCURS G-SIZE.

       01 WS-T-GRID.
	  03 FIL                        PIC X(03) OCCURS G-SIZE.
	  03 WS-T-YMD                   PIC 9(08) OCCURS G-SIZE.

       01 WS-MISC.
	  03 G-SUB			PIC 9(02).
	  03 G-C-SUB                    PIC 9(02).
	  03 G-C-ROW                    PIC 9(02).
	  03 G-C-COL			PIC 9(02).
	  03 WS-PREV-ROW		PIC 9(02).
	  03 WS-PREV-COL		PIC 9(02).
	  03 WS-CURR-YMD.		
	     05 WS-CURR-YYYY		PIC 9(04).
	     05 WS-CURR-MM		PIC 9(02).
	     05 WS-CURR-DD		PIC 9(02).
          03 WS-TITLE			PIC X(40).

       01 WS-MISC2.
	  03 WS-FIRST-YMD.
	     05 WS-FIRST-YYYY		PIC 9(04).
	     05 WS-FIRST-MM		PIC 9(02).
	     05 WS-FIRST-DD		PIC 9(02).
	  03 WS-LAST-YMD.
	     05 WS-LAST-YYYY		PIC 9(04).
	     05 WS-LAST-MM		PIC 9(02).
	     05 WS-LAST-DD		PIC 9(02).
          03 WS-LEAP-YEAR		PIC 9(06).
	  03 WS-DOW			PIC X(03).
	  03 WS-REMAIN			PIC 9(02).
	     88 LEAP-YEAR		VALUE 0.
	  03 WS-TOT-DAY			PIC 9(10).
	  03 WS-TOT-WEEK		PIC 9(10).
	  03 WS-LEAP-COUNT		PIC 9(10).
	  03 WS-IDX			PIC 9(02).

       LINKAGE SECTION.
       01 LINK-DMY			PIC 9(08).
      *01 LINK-DOW			PIC X(03).
       01 LINK-OK			PIC X(01).

       SCREEN SECTION.
       01 MAIN-SCR.
	  03 GRID-1 GRID 3-D ID 101
	     LINE 3 COL 04 SIZE 35 LINES G-SIZE
	     DATA-COLUMNS	= (1,4,7,10,13,16,19,22)
	     DISPLAY-COLUMNS 	= (1,6,11,16,21,26,31)
	     ALIGNMENT		= ('C','C','C','C','C','C','C')
	     COLUMN-DIVIDERS	= (1,1,1,1,1,1,1)
    	     CURSOR-FRAME-WIDTH = -1
      *      VIRTUAL-WIDTH	= 35
	     HEADING-COLOR	= WS-HEADER-COLOR
	     HEADING-DIVIDER-COLOR = G-HEADER-DIV
	     DIVIDER-COLOR	= G-DIVIDER
	     END-COLOR		= G-END
	     CURSOR-COLOR	= G-CURSOR
	     ROW-COLOR-PATTERN	= (G-ROW-PATT1, G-ROW-PATT2)
	     ROW-COLOR		= G-ROW
	     COLUMN-COLOR	= G-COL
	     CELL-COLOR		= G-CELL
      *      ADJUSTABLE-COLUMNS
	     COLUMN-HEADINGS
	     CENTERED-HEADINGS
	     TILED-HEADINGS
	     POP-UP MENU	GRID-MENU
	     EVENT PROCEDURE GRID-1-RTN THRU GRID-1-END.

       01 DATE-SCR.
	  03 LABEL LINE 1.8 COL 04 FROM WS-TITLE.

       01 FKEY-SCR.
	  03 PUSH-BUTTON 'Esc - Exit'
	     COL 2.2 SELF-ACT
	     BITMAP-HANDLE 	= S-BITMAP
	     BITMAP-NUMBER 	= 1
	     TERMINATION-VALUE 	= 027.
          02 LABEL COL + 1 LINES 1.5 ' '.
	  03 BAR-SCR BAR COL + 1 LINES 1.5
	     WIDTH 5 COLOR 8 COLORS = (8,8,1,8,8)
	     SHADING (1,-1).
          03 LABEL COL + 1 LINES 1.5 ' '.
	  03 PUSH-BUTTON 'F1 - Previous Year' SELF-ACT
	     COL + 1 LINES 15 SIZE 16 BITMAP
	     BITMAP-HANDLE      = S-BITMAP2
	     BITMAP-NUMBER      = 1
	     EXCEPTION-VALUE	= 001.
          03 PUSH-BUTTON 'F2 - Previous Month' SELF-ACT
	     COL + 1 LINES 15 SIZE 16 BITMAP
	     BITMAP-HANDLE      = S-BITMAP2
	     BITMAP-NUMBER      = 2
	     EXCEPTION-VALUE    = 002.
          03 PUSH-BUTTON 'F3 - Next Month' SELF-ACT
	     COL + 1 LINES 15 SIZE 16 BITMAP
	     BITMAP-HANDLE      = S-BITMAP2
	     BITMAP-NUMBER      = 3
	     EXCEPTION-VALUE    = 003.
          03 PUSH-BUTTON 'F4 - Next Year' SELF-ACT
	     COL + 1 LINES 15 SIZE 16 OVERLAP-LEFT BITMAP
	     BITMAP-HANDLE      = S-BITMAP2
	     BITMAP-NUMBER      = 4
	     EXCEPTION-VALUE    = 004.
          03 PUSH-BUTTON 'Enter - Select' SELF-ACT
	     COL + 1 BITMAP
	     BITMAP-HANDLE      = S-BITMAP
	     BITMAP-NUMBER      = 12
	     EXCEPTION-VALUE    = 13.

      ********************************************************************
       PROCEDURE DIVISION USING LINK-DMY, LINK-OK.

	BEGIN.

      *     IF LINK-DMY = ZEROS
      *        INITIALIZE LINK-DOW

	   MOVE 'N' TO LINK-OK.

      * Floating Window
	   INITIALIZE WS-MISC.
	   COPY '/v/cps/lib/std/gridc.prd'.

	   DISPLAY FLOATING WINDOW LINES 9 SIZE 41
	   CELL SIZE = ENTRY-FIELD FONT SEPARATE
	   TITLE-BAR MODAL NO SCROLL NO WRAP
	   TITLE 'Date Table'
	   POP-UP S-WINDOW.
	   DISPLAY TOOL-BAR, LINES 2.2 BACKGROUND-LOW HANDLE S-TOOLBAR.
	   DISPLAY FRAME AT 0102 LINES 8.7 CELL SIZE 39 RAISED.

	   CALL 'W$BITMAP' USING WBITMAP-LOAD, 'TOOL.BMP'
	   GIVING S-BITMAP.
	   CALL 'W$BITMAP' USING WBITMAP-LOAD, 'DATE.BMP'
	   GIVING S-BITMAP2.
	   DISPLAY FKEY-SCR UPON S-TOOLBAR.

	   INITIALIZE WS-GRID, WS-T-GRID, KEY-STATUS.
	   DISPLAY MAIN-SCR.
	   PERFORM GET-DATE THRU GET-DATE-END.
	  
	   INITIALIZE KEY-STATUS.
	   PERFORM 0100-MAIN THRU 0199-END UNTIL S-RUN = 'N'.

        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   EXIT PROGRAM.
	   STOP RUN.

      ********************************************************************
        0100-MAIN.
	 
           ACCEPT MAIN-SCR.
	   MOVE 4 TO ACCEPT-CONTROL.

	   IF K-ESCAPE
              MOVE 'N' TO S-RUN GO TO 0199-END.
 
	   IF K-ENTER OR (K-EVENT AND E-BEGIN-ENTRY)
              IF WS-CURR-YMD NOT = ZEROS
		 STRING WS-CURR-DD, WS-CURR-MM, WS-CURR-YYYY
		        DELIMITED BY SIZE INTO LINK-DMY
      *           MOVE WS-DOW 	TO LINK-DOW
		 MOVE 'Y' 	TO LINK-OK
		 MOVE 'N' 	TO S-RUN
		 GO TO 0199-END.

	   IF K-F1 OR K-F2 OR K-F3 OR K-F4
	      PERFORM GET-DATE THRU GET-DATE-END.

        0199-END. EXIT.

      ********************************************************************
        GET-DATE.

	   EVALUATE KEY-STATUS
	    WHEN 1	SUBTRACT 1 FROM WS-CURR-YYYY
	    WHEN 2 	IF WS-CURR-MM = 1
			   MOVE 12 TO WS-CURR-MM
			   SUBTRACT 1 FROM WS-CURR-YYYY
			ELSE
			   SUBTRACT 1 FROM WS-CURR-MM
			END-IF
            WHEN 3	IF WS-CURR-MM = 12
			   MOVE 1 TO WS-CURR-MM
			   ADD 1 TO WS-CURR-YYYY
                        ELSE
			    ADD 1 TO WS-CURR-MM
                        END-IF
            WHEN 4	ADD 1 TO WS-CURR-YYYY
	    WHEN OTHER ACCEPT WS-CURR-YMD FROM CENTURY-DATE.

	    INITIALIZE WS-MISC2.

      * Get first date of the month.
	    MOVE WS-CURR-YMD	TO WS-FIRST-YMD.
	    MOVE 1		TO WS-FIRST-DD.

	    DIVIDE WS-FIRST-YYYY BY 4 GIVING WS-LEAP-YEAR
				      REMAINDER WS-REMAIN.

            IF LEAP-YEAR AND WS-FIRST-MM < 3
	       SUBTRACT 1 FROM WS-LEAP-YEAR.

      * Get total days for the first date of the month.
	    COMPUTE WS-TOT-DAY = WS-FIRST-YYYY * 365 + WS-LEAP-YEAR +
		 		 WS-DAY(WS-FIRST-MM)+ 01.

      * Subtract cutoff date (01/01/1900) from total days
            SUBTRACT WS-19000101 FROM WS-TOT-DAY.

      * Get The Day of Week for the 1st date of the month.
      * 1-Mon 2-Tue 3-Wed 4-Thu 5-Fri 6-Sat 7-Sun.
	   DIVIDE WS-TOT-DAY BY 7 GIVING WS-TOT-WEEK REMAINDER WS-IDX.
	   IF WS-IDX = 0
	      MOVE 7 TO WS-IDX.

      * Get the last date of the month.
	   MOVE WS-FIRST-YMD	TO WS-LAST-YMD.
	   MOVE WS-DOM(WS-LAST-MM) TO WS-LAST-DD.
           IF LEAP-YEAR AND WS-LAST-MM = 2
	      ADD 1 TO WS-LAST-DD.

      * Adjust the current date if > the last date of the month>
	   IF WS-CURR-DD > WS-LAST-DD
	      MOVE WS-LAST-DD TO WS-CURR-DD.

      * Reset Grid.
	   MODIFY GRID-1, MASS-UPDATE = 1.
	   MODIFY GRID-1, RESET-GRID =1.
	   MODIFY GRID-1, INSERTION-INDEX =1,
	   RECORD-TO-ADD = WS-HEADER.

      * Add Calendar onto Grid.
	   MOVE 1 TO G-C-ROW, G-C-COL.
	   PERFORM VARYING S-SUB FROM 2 BY 1 UNTIL S-SUB > G-SIZE
		   INITIALIZE WS-GRID
		   PERFORM VARYING S-SUB2 FROM WS-IDX BY 1

			   UNTIL S-SUB2 > G-COL
			   OR WS-FIRST-YMD > WS-LAST-YMD
			   MOVE WS-FIRST-DD  TO WS-DD(S-SUB2)
			   MOVE WS-FIRST-YMD TO WS-YMD(S-SUB2)
			   IF WS-FIRST-YMD = WS-CURR-YMD
			      MOVE S-SUB  TO G-C-ROW
			      MOVE S-SUB2 TO G-C-COL
		           END-IF
			   ADD 1 TO WS-FIRST-DD
		   END-PERFORM
		   MODIFY GRID-1, RECORD-TO-ADD = WS-GRID
		   MOVE 1 TO WS-IDX
 	   END-PERFORM.

      * Set the color for sunday.
	   MODIFY GRID-1, X = G-COL, Y = 1, CELL-COLOR = 13.
	   PERFORM VARYING S-SUB FROM 2 BY 1 UNTIL S-SUB > G-SIZE
	           MODIFY GRID-1, X= G-COL, Y = S-SUB,
		   CELL-COLOR = 13
           END-PERFORM.
	   MODIFY GRID-1, MASS-UPDATE = 0.

	   PERFORM GRID-1-SUB THRU GRID-1-END.

        GET-DATE-END. EXIT.

      ********************************************************************
        GRID-1-RTN.

      * Terminate ACCEPT if Enter or Double CLick for begin entry.
	   IF E-BEGIN-ENTRY
	      MOVE 1 TO E-ACTION
	      GO TO GRID-1-END.

	   MOVE G-C-ROW TO C-ROW.
	   MOVE G-C-COL TO C-COL.
	   IF E-GOTO-CELL OR E-HEADER-CLICK
	      INQUIRE GRID-1, Y =C-ROW, X = C-COL.
	   
	   INQUIRE GRID-1, RECORD-DATA IN WS-T-GRID.
	   IF WS-T-YMD(C-COL) = ZEROS OR SPACE
	      MODIFY GRID-1, CURSOR-Y = G-C-ROW, CURSOR-X = G-C-COL
	      GO TO GRID-1-END.

      * Skip if click heading
	   IF C-ROW = 1 AND NOT E-SEARCH
	      GO TO GRID-1-END
	   ELSE
	      MOVE C-ROW TO G-C-ROW
	      MOVE C-COL TO G-C-COL.

      * Terminate event if not expected event types>
           MOVE 4 TO E-ACTION.

        GRID-1-SUB.

      * Reverse selected cell.
	   MODIFY GRID-1, START-X = G-C-COL, START-Y = G-C-ROW,
		  X = G-C-COL, Y = G-C-ROW,
		  REGION-COLOR = WS-SELECT-CELL-COLOR.
	   MODIFY GRID-1, CURSOR-Y = G-C-ROW, CURSOR-X = G-C-COL.
	   MODIFY GRID-1, Y = G-C-ROW,
	   INQUIRE GRID-1, RECORD-DATA IN WS-GRID.
	   MOVE WS-YMD(G-C-COL) TO WS-CURR-YMD.
	   MOVE WS-HDR(G-C-COL) TO WS-DOW.

       	   INITIALIZE E-TYPE.

	   IF G-C-COL = WS-PREV-COL AND
	      G-C-ROW = WS-PREV-ROW
	      GO TO GRID-1-END.

           MOVE G-C-COL TO WS-PREV-COL.
	   MOVE G-C-ROW TO WS-PREV-ROW.
	   COMPUTE G-C-SUB = G-C-COL.

	   INITIALIZE WS-TITLE.
	   STRING WS-MTH(WS-CURR-MM) DELIMITED BY ' '
		  ' ', WS-CURR-DD, ',' WS-CURR-YYYY, '-', WS-DOW
		  DELIMITED BY SIZE INTO WS-TITLE.

           DISPLAY DATE-SCR.
	   
      * MODIFY S-WINDOW, TITLW WS-TITLE.

       GRID-1-END. EXIT.

      * End of program. 
