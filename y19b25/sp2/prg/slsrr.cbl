       IDENTIFICATION DIVISION.
	PROGRAM-ID. 	SLSRR.

      * PRINT STUDENT FILE
      * AUTHOR		DATE	TYPE	A/C	NOTES
      * VANTZESHAN	10/9/19	-	PAA	SP2

        WORKING-STORAGE SECTION.
	   COPY 	'/v/cps/lib/std/stdvar.def'.
	   COPY		'/v/cps/lib/std/fkey.def'.
	   COPY RESOURCE '/v/cps/lib/icon/help.jpg'.

       01 WS-MISC.
	  03 CY-S-KEY		PIC X(04).
	  03 CY-E-KEY		PIC X(04).
	  03 RC-S-KEY		PIC X(04).
	  03 RC-E-KEY		PIC X(04).
	  03 RG-S-KEY		PIC X(04).
          03 RG-E-KEY		PIC X(04).
	  03 WS-GENDER		PIC 9(02).
	  03 WS-GENDER2 REDEFINES WS-GENDER 
				PIC 9(01) OCCURS 2.

       LINKAGE SECTION.
       01 LINK-PROG-KEY		PIC X(30).

       SCREEN SECTION.
       01 SELECT-SCR.
	  03 LABEL LINE 02 COL 04 'Country:'.
	  03 ENTRY-FIELD 3-D ID 101 COL + 3.5 PIC X(02)
	     USING CY-S-KEY AUTO.
          03 PUSH-BUTTON 'F10 - Help Table' NO-TAB 
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 101.
          03 LABEL COL + 3 'to'.
	  03 ENTRY-FIELD 3-D ID 102 COL + 3 PIC X(02)
	     USING CY-E-KEY AUTO.
          03 PUSH-BUTTON 'F10 - Help Table' NO-TAB 
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 102.
	  03 LABEL LINE 03 COL 04 'Race:'.
	  03 ENTRY-FIELD 3-D ID 103 COL + 5.4 PIC X(02)
	     USING RC-S-KEY AUTO.
          03 PUSH-BUTTON 'F10 - Help Table' NO-TAB 
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 103.
          03 LABEL COL + 3 'to'.
	  03 ENTRY-FIELD 3-D ID 104 COL + 3 PIC X(02)
	     USING RC-E-KEY AUTO.
          03 PUSH-BUTTON 'F10 - Help Table' NO-TAB 
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 104.
	  03 LABEL LINE 04 COL 04 'Religion:'.
	  03 ENTRY-FIELD 3-D ID 105 COL + 3.1 PIC X(02)
	     USING RG-S-KEY AUTO.
          03 PUSH-BUTTON 'F10 - Help Table' NO-TAB 
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 105.
          03 LABEL COL + 3 'to'.
	  03 ENTRY-FIELD 3-D ID 106 COL + 3 PIC X(02)
	     USING RG-E-KEY.
          03 PUSH-BUTTON 'F10 - Help Table' NO-TAB 
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 106.
          03 LABEL LINE 05 COL 04 'Gender:'.
	  03 CHECK-BOX ID 107 COL + 3.7 PIC 9(01) 
                   		USING WS-GENDER2(1). 
	  03 LABEL LINE 05 COL + 1 'Male'.
	  03 CHECK-BOX ID 108 LINE 05 COL + 3 PIC 9(01) 
				USING WS-GENDER2(2). 
	  03 LABEL LINE 05 COL + 1 'Female'.
	  COPY '/v/cps/lib/std/ptbtn.scr'.
      ********************************************************************
       PROCEDURE DIVISION.
      ********************************************************************

	BEGIN.

	   SET ENVIRONMENT 'PA-USER-ID' TO 'y19b25'.
	   MOVE 'N' TO S-RUN.

	   CALL		'/z/y19b25/sp2/lib/std/f-gttid'
			USING S-DATA-ID
	   CANCEL	'/z/y19b25/sp2/lib/std/f-gttid'.

	   COPY 	'/v/cps/lib/std/gtcoid.prd'.
	   MOVE	'Print Student Statistic Report' TO S-WINDOW-TITLE.
	   COPY		'/v/cps/lib/std/ptwin.prd'.

	   CALL	'W$BITMAP' USING WBITMAP-LOAD, 
		'help.jpg' GIVING S-BITMAP.

	   INITIALIZE WS-MISC.
	   MOVE 'Y' TO S-RUN.
	   MOVE 11  TO WS-GENDER.
	   PERFORM 0100-MAIN THRU 0199-END UNTIL S-RUN = 'N'.

	TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   EXIT PROGRAM.
	   STOP RUN.
      ********************************************************************
        0100-MAIN.
	   
           PERFORM ERROR-RTN THRU ERROR-END.
	   DISPLAY SELECT-SCR.
	   ACCEPT SELECT-SCR.
	   MOVE 4 TO ACCEPT-CONTROL.

	   IF K-ESCAPE
	      MOVE 'N' TO S-RUN	GO TO 0199-END.

	   IF (K-F10 AND S-CONTROL-ID = 101) OR KEY-STATUS = 101
	      CALL   '/z/y19b25/sp2/prg/hpcy' 
		      USING CY-S-KEY, S-OK
	      CANCEL '/z/y19b25/sp2/prg/hpcy'
	      MOVE 101 TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 102 TO S-CONTROL-ID
           END-IF
	      GO TO 0100-MAIN.

	   IF (K-F10 AND S-CONTROL-ID = 102) OR KEY-STATUS = 102
	      CALL   '/z/y19b25/sp2/prg/hpcy' 
		      USING CY-E-KEY, S-OK
	      CANCEL '/z/y19b25/sp2/prg/hpcy'
	      MOVE 102 TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 103 TO S-CONTROL-ID
           END-IF
	      GO TO 0100-MAIN.

	   IF (K-F10 AND S-CONTROL-ID = 103) OR KEY-STATUS = 103
	      CALL   '/z/y19b25/sp2/prg/hprc' 
		      USING RC-S-KEY, S-OK
	      CANCEL '/z/y19b25/sp2/prg/hprc'
	      MOVE 103 TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 104 TO S-CONTROL-ID
           END-IF
	      GO TO 0100-MAIN.

	   IF (K-F10 AND S-CONTROL-ID = 104) OR KEY-STATUS = 104
	      CALL   '/z/y19b25/sp2/prg/hprc' 
		      USING RC-E-KEY, S-OK
	      CANCEL '/z/y19b25/sp2/prg/hprc'
	      MOVE 104 TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 105 TO S-CONTROL-ID
           END-IF
	      GO TO 0100-MAIN.

	   IF (K-F10 AND S-CONTROL-ID = 105) OR KEY-STATUS = 105
	      CALL   '/z/y19b25/sp2/prg/hprg' 
		      USING RG-S-KEY, S-OK
	      CANCEL '/z/y19b25/sp2/prg/hprg'
	      MOVE 105 TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 106 TO S-CONTROL-ID
           END-IF
	      GO TO 0100-MAIN.

	   IF (K-F10 AND S-CONTROL-ID = 106) OR KEY-STATUS = 106
	      CALL   '/z/y19b25/sp2/prg/hprg' 
		      USING RG-E-KEY, S-OK
	      CANCEL '/z/y19b25/sp2/prg/hprg'
	      MOVE 106 TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 107 TO S-CONTROL-ID
           END-IF
	      GO TO 0100-MAIN.

           IF NOT K-ENTER GO TO 0100-MAIN.

	   IF CY-E-KEY NOT = SPACES AND 
	      CY-S-KEY > CY-E-KEY 
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 101	  TO S-CONTROL-ID
	      GO TO 0100-MAIN.

	   IF RC-E-KEY NOT = SPACES AND 
	      RC-S-KEY > RC-E-KEY 
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 103	  TO S-CONTROL-ID
	      GO TO 0100-MAIN.

	   IF RG-E-KEY NOT = SPACES AND 
	      RG-S-KEY > RG-E-KEY 
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 105	  TO S-CONTROL-ID
	      GO TO 0100-MAIN.

           IF WS-GENDER = ZEROES
	      MOVE 100040 TO S-ERROR-CODE
	      MOVE 107    TO S-CONTROL-ID
	      GO TO 0100-MAIN.

           IF CY-S-KEY = SPACES
	      MOVE LOW-VALUE TO CY-S-KEY.

           IF RC-S-KEY = SPACES
	      MOVE LOW-VALUE TO RC-S-KEY.

           IF RG-S-KEY = SPACES
	      MOVE LOW-VALUE TO RG-S-KEY.

           IF CY-E-KEY = SPACES
	      MOVE HIGH-VALUE TO CY-E-KEY.

           IF RC-E-KEY = SPACES
	      MOVE HIGH-VALUE TO RC-E-KEY.

           IF RG-E-KEY = SPACES
	      MOVE HIGH-VALUE TO RG-E-KEY.


	   CALL   '/z/y19b25/sp2/prg/psssr'
	   	  USING S-DATA-ID, WS-MISC, S-OK.
	   CANCEL '/z/y19b25/sp2/prg/psssr'.

	   CALL   '/z/y19b25/sp2/prg/ptssr'
  	          USING LINK-PROG-KEY,S-DATA-ID.
	   CANCEL '/z/y19b25/sp2/prg/ptssr'.

	   IF CY-E-KEY = HIGH-VALUE
	      INITIALIZE CY-E-KEY.

 	   IF RC-E-KEY = HIGH-VALUE
	      INITIALIZE RC-E-KEY.

	   IF RG-E-KEY = HIGH-VALUE
	      INITIALIZE RG-E-KEY.

        0199-END. EXIT.

      ********************************************************************
	   COPY '/v/cps/lib/std/errmsg.prd'.

      * End of program.

