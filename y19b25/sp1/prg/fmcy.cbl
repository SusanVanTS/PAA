       IDENTIFICATION DIVISION.
	PROGRAM-ID.   FMCY.
	
      * MAINTAIN COUNTRY FILE
      * AUTHOR		DATE	 TYPE	A/C	NOTES
      * VAN TZE SHAN 	1/8/2019 -	PAA	SP1

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	   COPY '/z/y19b25/sp1/lib/fd/fccy'.

       DATA DIVISION.
	FILE SECTION.
	   COPY '/z/y19b25/sp1/lib/fd/fdcy'.

        WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp1/lib/fd/dbcy'.
	   COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.
	   COPY RESOURCE '/v/cps/lib/icon/help.jpg'.

        LINKAGE SECTION.
	01 LINK-PROG-KEY	  PIC X(30).

	SCREEN SECTION.
	01 SELECT-SCR.
	   COPY '/v/cps/lib/std/fmmode.scr'.
	   03 LABEL LINE 02 COL 04 'Country:'.
	   03 ENTRY-FIELD 3-D ID 101 COL + 3 PIC X(02) USING CY-KEY.
	   03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	      COL + 1.5 LINES 13
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER 	= 1
	      TERMINATION-VALUE = 101.

        01 PROCESS-SCR.
	   COPY '/v/cps/lib/std/fmmode.scr'.
	   03 LABEL LINE 02 COL 04 'Country:'.
	   03 ENTRY-FIELD 3-D ENABLED 0 COL 14 PIC X(02) USING CY-KEY.
	   03 LABEL LINE 03 COL 04 'Name:'.
	   03 ENTRY-FIELD 3-D ID 102 COL 14 PIC X(20) USING CY-NAME.
	
      *******************************************************************
       PROCEDURE DIVISION USING LINK-PROG-KEY.

	DECLARATIVES.
	 
	   COPY '/z/y19b25/sp1/lib/fd/dccy'.

        END DECLARATIVES.
      *******************************************************************
        BEGIN.

	   MOVE 'N' TO S-RUN.
	   OPEN I-O CY-FILE.

      * Floating Window
	   Move 'Define Country' TO S-WINDOW-TITLE.
	   COPY '/v/cps/lib/std/floatwin.prd'.

      * Bitmap
	   CALL 'W$BITMAP' USING
	         WBITMAP-LOAD, 'help.jpg' GIVING S-BITMAP.

           MOVE 'Y' TO S-RUN.
           INITIALIZE CY-REC.
           PERFORM 0100-MAIN THRU 0199-END UNTIL S-RUN = 'N'.
     I
        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   CLOSE CY-FILE.
	   EXIT PROGRAM.
	   STOP RUN.

      **************************************************************
        0100-MAIN.

	   MOVE 'S' TO S-PRS-MODE.
	   PERFORM FKEY-RTN THRU FKEY-END.

        0110-MAIN.

	   UNLOCK CY-FILE.
	   PERFORM ERROR-RTN THRU ERROR-END.
	   DISPLAY SELECT-SCR.
	   ACCEPT  SELECT-SCR.
	   MOVE 4 TO ACCEPT-CONTROL.

	   IF K-ESCAPE
	      MOVE 'N' TO S-RUN GO TO 0199-END.

           IF K-F2
	      PERFORM GET-NEXT THRU GET-NEXT-END
	      IF S-STATUS-CHECK = 'Y' OR S-ERROR-CODE NOT = ZEROS
		 GO TO 0110-MAIN
              ELSE
		 GO TO 0120-MAIN.

           IF K-F7
	      CALL   '/z/y19b25/sp1/prg/vwcy' USING LINK-PROG-KEY
	      CANCEL '/z/y19b25/sp1/prg/vwcy'
	      GO TO 0110-MAIN.

           IF (K-F10 and S-CONTROL-ID = 101) OR KEY-STATUS = 101
	      CALL   '/z/y19b25/sp1/prg/hpcy' USING CY-KEY,S-OK
	      CANCEL '/z/y19b25/sp1/prg/hpcy'
	      MOVE 101 TO S-CONTROL-ID
	      IF S-OK ='Y'
		 DISPLAY SELECT-SCR
		 GO TO 0120-MAIN.

           IF NOT K-ENTER GO TO 0110-MAIN.

	   IF CY-KEY = SPACES
              MOVE 200005 TO S-ERROR-CODE
	      MOVE 101 	 TO S-CONTROL-ID
	      GO TO 0110-MAIN.

	 0120-MAIN.
	      MOVE 'N' TO S-STATUS-CHECK.
	      MOVE 'R' TO S-PRS-MODE.
	      READ CY-FILE INVALID
		   MOVE 'A' TO S-PRS-MODE
		   INITIALIZE CY-DETAILS
		   MOVE ZEROS TO CY-PADDING.

              IF S-STATUS-CHECK = 'Y' GO TO 0190-MAIN.

              DESTROY SELECT-SCR.
	      PERFORM FKEY-RTN THRU FKEY-END.

        0130-MAIN.

	   PERFORM ERROR-RTN THRU ERROR-END.
	   DISPLAY PROCESS-SCR.
	   ACCEPT  PROCESS-SCR.
	   MOVE 4 TO ACCEPT-CONTROL.

	   IF K-ESCAPE 
	      MOVE 'N' TO S-RUN GO TO 0190-MAIN.

           IF K-F1 GO TO 0190-MAIN.

	   IF K-F2
	      PERFORM GET-NEXT THRU GET-NEXT-END
	      GO TO 0120-MAIN.

           IF K-F3
	      PERFORM GET-PREV THRU GET-PREV-END
              GO TO 0120-MAIN.

           IF K-F4 AND S-PRS-MODE = 'R'
	      PERFORM CONFIRM-RTN THRU CONFIRM-END
	      IF S-CONFIRM = 'Y'
		 DELETE CY-FILE
		 GO TO 0190-MAIN
              ELSE 
		 GO TO 0130-MAIN.

           IF NOT (K-F8 OR K-ENTER) GO TO 0130-MAIN.

	   IF CY-NAME = SPACES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 102    TO S-CONTROL-ID
	      GO TO 0130-MAIN.

           PERFORM CONFIRM-RTN THRU CONFIRM-END.
	   IF S-CONFIRM NOT = 'Y'
	      GO TO 0130-MAIN.

           IF S-PRS-MODE = 'A' WRITE CY-REC.
	   IF S-PRS-MODE = 'R' REWRITE CY-REC.

        0190-MAIN.

	   DESTROY PROCESS-SCR.

        0199-END. EXIT.
      *******************************************************************
        GET-NEXT.

	   START CY-FILE KEY > CY-KEY INVALID
		 MOVE 100010 TO S-ERROR-CODE
		 NOT INVALID
		     READ CY-FILE NEXT END
			  MOVE 100010 TO S-ERROR-CODE
		     END-READ.

        GET-NEXT-END. EXIT.
      ********************************************************************
        GET-PREV.                                                                
           START CY-FILE KEY < CY-KEY INVALID                                   
		 MOVE 100005 TO S-ERROR-CODE                                    
		 NOT INVALID                                                    
		     READ CY-FILE BACKWARD END                     
			  MOVE 100005 TO S-ERROR-CODE                           
	         END-READ.                                                   
        
	GET-PREV-END. EXIT.
      ********************************************************************
        FKEY-RTN.

	   EVALUATE S-PRS-MODE
	   WHEN 'S' MOVE '1yy456y89012y4567890' TO S-ACTIVE-FKEY
	   WHEN 'A' MOVE 'yyy4567y9012y4567890' TO S-ACTIVE-FKEY
	   WHEN 'R' MOVE 'yyyy567y9012y4567890' TO S-ACTIVE-FKEY.

           CALL   '/v/cps/lib/std/x-fkey ' USING
	          S-ACTIVE-FKEY,  S-TOOLBAR, S-BUTTON.
	   CANCEL '/v/cps/lib/std/x-fkey'.
           COPY   '/v/cps/lib/std/fmmode.prd'.

	FKEY-END. EXIT.

      ***************************************************************

	   COPY '/v/cps/lib/std/cfirm.prd'.
	   COPY '/v/cps/lib/std/errmsg.prd'.

      *End of Program.
