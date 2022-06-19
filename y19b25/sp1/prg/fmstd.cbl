       IDENTIFICATION DIVISION.
	PROGRAM-ID.   FMSTD.
	
      * MAINTAIN STUDENT FILE
      * AUTHOR		DATE	 TYPE	A/C	NOTES
      * VAN TZE SHAN 	6/8/2019 -	PAA	SP1

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	   COPY '/z/y19b25/sp1/lib/fd/fcstd'.
	   COPY '/z/y19b25/sp1/lib/fd/fccy'.
	   COPY '/z/y19b25/sp1/lib/fd/fcrc'.

       DATA DIVISION.
	FILE SECTION.
	   COPY '/z/y19b25/sp1/lib/fd/fdstd'.
	   COPY '/z/y19b25/sp1/lib/fd/fdcy'.
	   COPY '/z/y19b25/sp1/lib/fd/fdrc'.

        WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp1/lib/fd/dbstd'.
	   COPY '/z/y19b25/sp1/lib/fd/dbcy'.
	   COPY '/z/y19b25/sp1/lib/fd/dbrc'.
	   COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.
	   COPY RESOURCE '/v/cps/lib/icon/help.jpg'.

	01 WS-OPTION	       	PIC X(01).
	01 WS-CY-RECORD		PIC 9(02).

        LINKAGE SECTION.
	01 LINK-PROG-KEY	  PIC X(30).

	SCREEN SECTION.
	01 SELECT-SCR.
	   COPY '/v/cps/lib/std/fmmode.scr'.
	   03 LABEL LINE 02 COL 04 'Student AC#:'.
	   03 ENTRY-FIELD 3-D ID 101 COL + 3 PIC X(06) USING STD-KEY.
	   03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	      COL + 1.5 LINES 13
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER 	= 1
	      TERMINATION-VALUE = 101.

        01 PROCESS-SCR.
	   COPY '/v/cps/lib/std/fmmode.scr'.
	   03 LABEL LINE 02 COL 04 'Student AC#:   '.
	   03 ENTRY-FIELD 3-D ID 101 ENABLED 0 COL 17 PIC X(06) 
	      USING STD-KEY.
	   03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	      COL + 1.5 
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER     = 1
	      TERMINATION-VALUE = 101.
	   03 LABEL LINE 03 COL 04 'Name:'.
	   03 ENTRY-FIELD 3-D ID 102 COL 17 PIC X(40) USING STD-NAME.
	   03 LABEL LINE 04 COL 04 'Address:'.
	   03 ENTRY-FIELD 3-D ID 103 COL 17 PIC X(40)
	      USING STD-ADD1.
	   03 ENTRY-FIELD 3-D ID 104 LINE 05 COL 17 PIC X(40)
	      USING STD-ADD2.
	   03 ENTRY-FIELD 3-D ID 105 LINE 06 COL 17 PIC X(40) 
	      USING STD-ADD3.
	   03 LABEL LINE 07 COL 04 'Gender:'.
	   03 RADIO-BUTTON COL 17
	      GROUP = 1 GROUP-VALUE = 1 VALUE WS-OPTION.
           03 LABEL COL + 1 'Male'.
	   03 RADIO-BUTTON COL + 3
	      GROUP = 1 GROUP-VALUE = 2 VALUE WS-OPTION.
           03 LABEL COL + 1 'Female'.
 	   03 LABEL LINE 08 COL 04 'DOB:'.
	   03 ENTRY-FIELD 3-D ID 106 COL 17 PIC 99/99/9999
	      USING STD-DOB-DMY.
	   03 PUSH-BUTTON 'F10 - Calender Help Table' NO-TAB
	      COL + 1.5 
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER     = 1
	      TERMINATION-VALUE = 106.
	   03 LABEL LINE 09 COL 04 'Country:'.
	   03 ENTRY-FIELD 3-D ID 107 COL 17 PIC X(04) 
	      USING STD-CY-KEY.
	   03 PUSH-BUTTON 'F10 - Country Help Table' NO-TAB
	      COL + 1.5 
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER     = 1
	      TERMINATION-VALUE = 107.
	   03 LABEL LINE 10 COL 04 'Race:'.
	   03 ENTRY-FIELD 3-D ID 108 COL 17 PIC X(04) 
	      USING STD-RC-KEY.
	   03 PUSH-BUTTON 'F10 - Race Help Table' NO-TAB
	      COL + 1.5 
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER     = 1
	      TERMINATION-VALUE = 108.
	   03 LABEL LINE 11 COL 04 'Email:#'.
	   03 ENTRY-FIELD 3-D ID 109 COL 17 PIC X(30) 
	      USING STD-EMAIL.
	   03 LABEL LINE 12 COL 04 'Mobile:#'.
 	   03 ENTRY-FIELD 3-D ID 110 COL 17 PIC X(30)
	      USING STD-MOBILE.

      *******************************************************************
       PROCEDURE DIVISION USING LINK-PROG-KEY.

	DECLARATIVES.
	 
	   COPY '/z/y19b25/sp1/lib/fd/dcstd'.
	   COPY '/z/y19b25/sp1/lib/fd/dccy'.
	   COPY '/z/y19b25/sp1/lib/fd/dcrc'.

        END DECLARATIVES.
      *******************************************************************
        BEGIN.

	   MOVE 'N' TO S-RUN.
	   OPEN I-O STD-FILE.
	   OPEN INPUT CY-FILE.
	   OPEN INPUT RC-FILE.

      * Floating Window
	   Move 'Define Student' TO S-WINDOW-TITLE.
	   COPY '/v/cps/lib/std/floatwin.prd'.

      * Bitmap
	   CALL 'W$BITMAP' USING
	         WBITMAP-LOAD, 'help.jpg' GIVING S-BITMAP.

           MOVE 'Y' TO S-RUN.
           INITIALIZE STD-REC.
           PERFORM 0100-MAIN THRU 0199-END UNTIL S-RUN = 'N'.
     I
        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   CLOSE STD-FILE.
	   EXIT PROGRAM.
	   STOP RUN.

      **************************************************************
        0100-MAIN.

	   MOVE 'S' TO S-PRS-MODE.
	   PERFORM FKEY-RTN THRU FKEY-END.

        0110-MAIN.

	   UNLOCK STD-FILE.
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
	      CALL   '/z/y19b25/sp1/prg/vwstd' USING LINK-PROG-KEY
	      CANCEL '/z/y19b25/sp1/prg/vwstd'
	      GO TO 0110-MAIN.

           IF (K-F10 and S-CONTROL-ID = 101) OR KEY-STATUS = 101
	      CALL   '/z/y19b25/sp1/prg/hpstd' USING STD-KEY,S-OK
	      CANCEL '/z/y19b25/sp1/prg/hpstd'
	      MOVE 101 TO S-CONTROL-ID
	      IF S-OK ='Y'
		 DISPLAY SELECT-SCR
		 GO TO 0120-MAIN.

           IF NOT K-ENTER GO TO 0110-MAIN.

	   IF STD-KEY = SPACES
              MOVE 200005 TO S-ERROR-CODE
	      MOVE 101 	 TO S-CONTROL-ID
	      GO TO 0110-MAIN.

	 0120-MAIN.
	      MOVE 'N' TO S-STATUS-CHECK.
	      MOVE 'R' TO S-PRS-MODE.
	      READ STD-FILE INVALID
		   MOVE 'A' TO S-PRS-MODE
		   INITIALIZE STD-DETAILS
		   MOVE ZEROS TO STD-PADDING
		   MOVE 01011980 TO STD-DOB-DMY
		   MOVE '01' TO STD-CY-KEY
		   MOVE 'AF' TO STD-RC-KEY.

	      IF S-STATUS-CHECK = 'Y' GO TO 0190-MAIN.
	      
	      EVALUATE STD-GENDER
	        WHEN 'M' MOVE '1' TO WS-OPTION 
	        WHEN 'F' MOVE '2' TO WS-OPTION
		WHEN ' ' MOVE '1' TO WS-OPTION.

              
              DESTROY SELECT-SCR.
	      PERFORM FKEY-RTN THRU FKEY-END.

        0130-MAIN.

	   PERFORM ERROR-RTN THRU ERROR-END.
	   DISPLAY PROCESS-SCR.
	   ACCEPT  PROCESS-SCR.
	   MOVE 4 TO ACCEPT-CONTROL.

	   EVALUATE WS-OPTION
	     WHEN 1 MOVE 'M' TO STD-GENDER
	     WHEN 2 MOVE 'F' TO STD-GENDER.

	   CALL 	'/z/y19b25/sp1/lib/std/f-cdate'
			USING STD-DOB-DMY, S-ERROR-CODE
	   CANCEL	'/z/y19b25/sp1/lib/std/f-cdate'
	   IF S-ERROR-CODE NOT = ZEROS
	      GO TO 0120-MAIN.
           
	   IF K-ESCAPE 
	      MOVE 'N' TO S-RUN GO TO 0190-MAIN.

           IF K-F1
	      GO TO 0190-MAIN.

	   IF K-F2
	      PERFORM GET-NEXT THRU GET-NEXT-END
	      GO TO 0120-MAIN.

           IF K-F3
	      PERFORM GET-PREV THRU GET-PREV-END
              GO TO 0120-MAIN.

           IF K-F4 AND S-PRS-MODE = 'R'
	      PERFORM CONFIRM-RTN THRU CONFIRM-END
	      IF S-CONFIRM = 'Y'
		 DELETE STD-FILE
		 GO TO 0190-MAIN
              ELSE 
		 GO TO 0130-MAIN.

	   IF (K-F10 and S-CONTROL-ID = 106) OR KEY-STATUS = 106 		
	      CALL 	'/z/y19b25/sp1/lib/std/x-hpcal' 
			USING STD-DOB-DMY,S-OK
	      CANCEL	'/z/y19b25/sp1/lib/std/x-hpcal'
	      MOVE 106  TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 106  TO S-CONTROL-ID
	      END-IF	
		 GO TO 0130-MAIN.

           INITIALIZE CY-REC.
	   MOVE STD-CY-KEY TO CY-KEY.
	   READ CY-FILE.
	   IF S-STATUS-CHECK = 'Y'
	      MOVE 107 TO S-CONTROL-ID
	      GO TO 0120-MAIN.

           INITIALIZE RC-REC.
	   MOVE STD-RC-KEY TO RC-KEY.
	   READ RC-FILE.
	   IF S-STATUS-CHECK = 'Y'
	      MOVE 108 TO S-CONTROL-ID
	      GO TO 0120-MAIN. 

	   IF (K-F10 and S-CONTROL-ID = 107) OR KEY-STATUS = 107
	      CALL      '/z/y19b25/sp1/prg/hpcy' 
			USING STD-CY-KEY, S-OK
	      CANCEL    '/z/y19b25/sp1/prg/hpcy'
	      MOVE 107       TO S-CONTROL-ID
	      IF S-OK = 'Y'
	         MOVE 107 	TO S-CONTROL-ID
	      END-IF 	 
	      GO TO 0130-MAIN.

	   IF (K-F10 and S-CONTROL-ID = 108) OR KEY-STATUS = 108
	      CALL      '/z/y19b25/sp1/prg/hprc' 
			USING STD-RC-KEY, S-OK
	      CANCEL    '/z/y19b25/sp1/prg/hprc'
	      MOVE 108 TO S-CONTROL-ID	
	      IF S-OK = 'Y'
	         MOVE 108 TO S-CONTROL-ID
	      END-IF
		 GO TO 0130-MAIN.

	   IF (K-F10 and S-CONTROL-ID = 101) OR KEY-STATUS = 101
	      CALL   '/z/y19b25/sp1/prg/hpstd' USING STD-KEY,S-OK
	      CANCEL '/z/y19b25/sp1/prg/hpstd'
	      MOVE 101 TO S-CONTROL-ID
	      IF S-OK ='Y'
	         DISPLAY SELECT-SCR
	      END-IF
	         GO TO 0120-MAIN.

	   IF NOT (K-F8 OR K-ENTER) GO TO 0130-MAIN.

	   IF STD-NAME = SPACES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 102    TO S-CONTROL-ID
	      GO TO 0130-MAIN.

           PERFORM CONFIRM-RTN THRU CONFIRM-END.
	   IF S-CONFIRM NOT = 'Y'
	      GO TO 0130-MAIN.

           IF S-PRS-MODE = 'A' WRITE STD-REC.
	   IF S-PRS-MODE = 'R' REWRITE STD-REC.

        0190-MAIN.

	   DESTROY PROCESS-SCR.

        0199-END. EXIT.
      *******************************************************************
        GET-NEXT.

	   START STD-FILE KEY > STD-KEY INVALID
		 MOVE 100010 TO S-ERROR-CODE
		 NOT INVALID
		     READ STD-FILE NEXT END
			  MOVE 100010 TO S-ERROR-CODE
		     END-READ.

        GET-NEXT-END. EXIT.
      ********************************************************************
        GET-PREV.
	   START STD-FILE KEY < STD-KEY INVALID
		 MOVE 100005 TO S-ERROR-CODE
		 NOT INVALID
		     READ STD-FILE NEXT END
			  MOVE 100010 TO S-ERROR-CODE
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

      * End of Program.
