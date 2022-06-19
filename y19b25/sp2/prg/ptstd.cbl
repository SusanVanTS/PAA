       IDENTIFICATION DIVISION.
	PROGRAM-ID.    PTSTD.

      * PRINT STUDENT FILE
      * AUTHOR		DATE	TYPE	A/C	NOTES
      * VAN TZE SHAN	4/9/19	-	PAA	SP2

        WORKING-STORAGE SECTION.
	   COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.
	   COPY RESOURCE '/v/cps/lib/icon/help.jpg'.

       01 WS-MISC.
	  03 START-KEY		PIC X(08).
	  03 END-KEY		PIC X(08).
	  03 WS-GENDER		PIC 9(02).
	  03 WS-GENDER2 REDEFINES WS-GENDER 
				PIC 9(01) OCCURS 2.
	  03 AGE-S-KEY		PIC 9(02).
	  03 AGE-E-KEY 		PIC 9(02).
	  03 CY-S-KEY 		PIC X(04).
	  03 CY-E-KEY		PIC X(04).
          03 RC-S-KEY		PIC X(04).
	  03 RC-E-KEY		PIC X(04).
	  03 RG-S-KEY           PIC X(04).
	  03 RG-E-KEY           PIC X(04).
	  03 REPORT-TYPE-KEY 	PIC X(01).
	  03 SORT-MODE-KEY	PIC X(01).

       LINKAGE SECTION.
       01 LINK-PROG-KEY		PIC X(30).

       SCREEN SECTION.
       01 SELECT-SCR.
	  03 LABEL LINE 02 COL 04 'Student Code:'.
	  03 ENTRY-FIELD 3-D ID 101 COL 18 PIC X(06)
		   USING START-KEY AUTO.
          03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
             COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 101.
          03 LABEL COL + 3 'to'.
	  03 ENTRY-FIELD 3-D ID 102 COL + 3 PIC X(06) 
		   USING END-KEY AUTO.
	  03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 102.
          03 LABEL LINE 03 COL 4 'Gender:'. 
	  03 CHECK-BOX ID 103 LINE + 0.1 COL 18 
	     PIC 9(01) USING WS-GENDER2(1).
	  03 LABEL LINE 03 COL + 1.5 'Male'.
          03 CHECK-BOX ID 104 LINE + 0.1 COL + 3 
             PIC 9(01) USING WS-GENDER2(2).
	  03 LABEL LINE 03 COL + 1.5 'Female'.
	  03 LABEL LINE 04 COL 4 'Age:'.
	  03 ENTRY-FIELD 3-D ID 105 COL 18 PIC 9(02)
		   USING AGE-S-KEY AUTO.
	  03 LABEL COL + 3 'to'.
	  03 ENTRY-FIELD 3-D ID 106 COL + 3 PIC 9(02) 
		   USING AGE-E-KEY AUTO.
	  03 LABEL LINE 05 COL 4 'Country:'.
	  03 ENTRY-FIELD 3-D ID 107 COL 18 PIC X(02)
		   USING CY-S-KEY AUTO.
	  03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 107.
	  03 LABEL COL + 3 'to'.
	  03 ENTRY-FIELD 3-D ID 108 COL + 3 PIC X(02) 
		   USING CY-E-KEY AUTO.
	  03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 108.
	  03 LABEL LINE 06 COL 4 'Race:'.
	  03 ENTRY-FIELD 3-D ID 109 COL 18 PIC X(02)
		   USING RC-S-KEY AUTO.
	  03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 109.
	  03 LABEL COL + 3 'to'.
	  03 ENTRY-FIELD 3-D ID 110 COL + 3 PIC X(02) 
		   USING RC-E-KEY AUTO.
	  03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 110.
	  03 LABEL LINE 07 COL 4 'Religion:'.
	  03 ENTRY-FIELD 3-D ID 111 COL 18 PIC X(02)
		   USING RG-S-KEY AUTO.
	  03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 111.
	  03 LABEL COL + 3 'to'.
	  03 ENTRY-FIELD 3-D ID 112 COL + 3 PIC X(02) 
		   USING RG-E-KEY AUTO.
	  03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 112.
          COPY '/v/cps/lib/std/ptbtn.scr'.

       01 SRPT-SCR.
	  03 LABEL LINE 02 COL 04 'Report Type:'.
          03 RADIO-BUTTON LINE + 0.2 COL 17
	     GROUP = 2 GROUP-VALUE = 1 VALUE REPORT-TYPE-KEY.
          03 LABEL LINE - 0.2 COL + 1 'Simple'.
	  03 RADIO-BUTTON LINE + 0.2 COL + 3.7 
	     GROUP = 2 GROUP-VALUE = 2 VALUE REPORT-TYPE-KEY.
          03 LABEL LINE - 0.2 COL + 1 'Detail'.
	  03 LABEL LINE 03 COL 04 'Sort Mode:'.
	  03 RADIO-BUTTON LINE + 0.2 COL + 5
	     GROUP = 3 GROUP-VALUE = 1 VALUE SORT-MODE-KEY.
	  03 LABEL LINE - 0.2 COL + 1 'By AC#'.
	  03 RADIO-BUTTON LINE + 0.2 COL + 3
	     GROUP = 3 GROUP-VALUE = 2 VALUE SORT-MODE-KEY.
	  03 LABEL LINE - 0.2 COL + 1 'By Name'.
	  03 RADIO-BUTTON LINE + 0.2 COL + 3
	     GROUP = 3 GROUP-VALUE = 3 VALUE SORT-MODE-KEY.
	  03 LABEL LINE - 0.2 COL + 1 'By Age + Name'.
	  03 RADIO-BUTTON LINE + 0.2 COL + 3
	     GROUP = 3 GROUP-VALUE = 4 VALUE SORT-MODE-KEY.
	  03 LABEL LINE - 0.2 COL + 1 'By Country + Name'.
          03 PUSH-BUTTON 'OK to Print' SELF-ACT LINE 6
	     COL 72 LINES 1.2 SIZE 12 
	     TERMINATION-VALUE = 113.

      ********************************************************************
       PROCEDURE DIVISION.
      ********************************************************************
        BEGIN. 

	   SET ENVIRONMENT 'PA-USER-ID' TO 'y19b25'.
	   MOVE 'N' TO S-RUN.
	  
	   CALL		'/z/y19b25/sp2/lib/std/f-gttid' 
			USING S-DATA-ID
           CANCEL 	'/z/y19b25/sp2/lib/std/f-gttid'

	   COPY '/v/cps/lib/std/gtcoid.prd'.
	   MOVE 'Print Student Profile' TO S-WINDOW-TITLE.
	   COPY '/v/cps/lib/std/ptwin.prd'.

	   CALL 'W$BITMAP' USING
	        WBITMAP-LOAD, 'help.jpg' GIVING S-BITMAP.

           INITIALIZE WS-MISC.
	   MOVE 'Y' TO S-RUN.
	   MOVE 11 TO WS-GENDER.
	   MOVE 1  TO AGE-S-KEY.
	   MOVE 99 TO AGE-E-KEY.
	   MOVE 1  TO REPORT-TYPE-KEY.
	   MOVE 1  TO SORT-MODE-KEY.
	   PERFORM 0100-MAIN THRU 0199-END UNTIL S-RUN = 'N'.
 
        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   EXIT PROGRAM.
	   STOP RUN.

      ********************************************************************
        0100-MAIN.

	   PERFORM ERROR-RTN THRU ERROR-END.
	   DISPLAY SELECT-SCR.
	   ACCEPT  SELECT-SCR.
	   MOVE 4 TO ACCEPT-CONTROL.
 
	   IF K-ESCAPE
	      MOVE 'N' TO S-RUN GO TO 0199-END.
 
           IF (K-F10 AND S-CONTROL-ID = 101) OR KEY-STATUS = 101
	      CALL	'/z/y19b25/sp2/prg/hpstd' USING START-KEY, S-OK
	      CANCEL 	'/z/y19b25/sp2/prg/hpstd'
	      MOVE 101 TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 102 TO S-CONTROL-ID
              END-IF
	      GO TO 0100-MAIN.

           IF (K-F10 AND S-CONTROL-ID = 102) OR KEY-STATUS = 102
	      CALL 	'/z/y19b25/sp2/prg/hpstd' USING END-KEY, S-OK
	      CANCEL	'/z/y19b25/sp2/prg/hpstd'
	      IF S-OK = 'Y'
		 MOVE 105 TO S-CONTROL-ID
              END-IF
	      GO TO 0100-MAIN.
           
	   IF (K-F10 AND S-CONTROL-ID = 107) OR KEY-STATUS = 107
	      CALL 	'/z/y19b25/sp2/prg/hpcy' USING CY-S-KEY, S-OK
	      CANCEL	'/z/y19b25/sp2/prg/hpcy'
	      IF S-OK = 'Y'
		 MOVE 108 TO S-CONTROL-ID
              END-IF
	      GO TO 0100-MAIN.
 
	   IF (K-F10 AND S-CONTROL-ID = 108) OR KEY-STATUS = 108
	      CALL 	'/z/y19b25/sp2/prg/hpcy' USING CY-E-KEY, S-OK
	      CANCEL	'/z/y19b25/sp2/prg/hpcy'
	      IF S-OK = 'Y'
		 MOVE 109 TO S-CONTROL-ID
              END-IF
	      GO TO 0100-MAIN.
 
           IF (K-F10 AND S-CONTROL-ID = 109) OR KEY-STATUS = 109
	      CALL 	'/z/y19b25/sp2/prg/hprc' USING RC-S-KEY, S-OK
	      CANCEL	'/z/y19b25/sp2/prg/hprc'
	      IF S-OK = 'Y'
		 MOVE 110 TO S-CONTROL-ID
              END-IF
	      GO TO 0100-MAIN.
           
	   IF (K-F10 AND S-CONTROL-ID = 110) OR KEY-STATUS = 110
	      CALL 	'/z/y19b25/sp2/prg/hprc' USING RC-E-KEY, S-OK
	      CANCEL	'/z/y19b25/sp2/prg/hprc'
	      IF S-OK = 'Y'
		 MOVE 111 TO S-CONTROL-ID
              END-IF
	      GO TO 0100-MAIN.

	   IF (K-F10 AND S-CONTROL-ID = 111) OR KEY-STATUS = 111
	      CALL 	'/z/y19b25/sp2/prg/hprg' USING RG-S-KEY, S-OK
	      CANCEL	'/z/y19b25/sp2/prg/hprg'
	      IF S-OK = 'Y'
		 MOVE 112 TO S-CONTROL-ID
              END-IF
	      GO TO 0100-MAIN.

	   IF (K-F10 AND S-CONTROL-ID = 112) OR KEY-STATUS = 112
	      CALL 	'/z/y19b25/sp2/prg/hprg' USING RG-E-KEY, S-OK
	      CANCEL	'/z/y19b25/sp2/prg/hprg'
	      IF S-OK = 'Y'
		 MOVE 112 TO S-CONTROL-ID
              END-IF
	      GO TO 0100-MAIN.

           IF NOT K-ENTER GO TO 0100-MAIN.
 
	   IF END-KEY NOT = SPACES AND
	      START-KEY > END-KEY
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 101    TO S-CONTROL-ID
	      GO TO 0100-MAIN.
 
              IF WS-GENDER = ZEROES
		 MOVE 100040	TO S-ERROR-CODE
		 MOVE 111	TO S-CONTROL-ID
		 GO TO 0100-MAIN.

              IF AGE-E-KEY NOT = SPACES AND 
		 AGE-S-KEY > AGE-E-KEY 
		 MOVE 100035 	TO S-ERROR-CODE
		 MOVE 105	TO S-CONTROL-ID
		 GO TO 0100-MAIN.
              
	      IF CY-E-KEY NOT = SPACES AND
		 CY-S-KEY > CY-E-KEY 
		 MOVE 100035	TO S-ERROR-CODE
		 MOVE 107       TO S-CONTROL-ID
		 GO TO 0100-MAIN.
	      
	      IF RC-E-KEY NOT = SPACES AND
		 RC-S-KEY > RC-E-KEY 
		 MOVE 100035	TO S-ERROR-CODE
		 MOVE 109       TO S-CONTROL-ID
		 GO TO 0100-MAIN.

	      IF RG-E-KEY NOT = SPACES AND
		 RG-S-KEY > RG-E-KEY 
		 MOVE 100035	TO S-ERROR-CODE
		 MOVE 110       TO S-CONTROL-ID
		 GO TO 0100-MAIN.

              IF START-KEY = SPACES
		 MOVE LOW-VALUE TO START-KEY.

              IF END-KEY = SPACES
		 MOVE HIGH-VALUE TO END-KEY.

              IF AGE-S-KEY = SPACES
		 MOVE LOW-VALUE TO AGE-S-KEY. 

              IF AGE-E-KEY = SPACES
		 MOVE HIGH-VALUE TO AGE-E-KEY. 

              IF CY-S-KEY = SPACES
		 MOVE LOW-VALUE TO CY-S-KEY. 

              IF CY-E-KEY = SPACES
		 MOVE HIGH-VALUE TO CY-E-KEY.

              IF RC-S-KEY = SPACES
		 MOVE LOW-VALUE TO RC-S-KEY.

              IF RC-E-KEY = SPACES
		 MOVE HIGH-VALUE TO RC-E-KEY.

              IF RG-S-KEY = SPACES
		 MOVE LOW-VALUE TO RG-S-KEY.

              IF RG-E-KEY = SPACES
		 MOVE HIGH-VALUE TO RG-E-KEY.

              CALL 	'/z/y19b25/sp2/prg/psstd'
			USING S-DATA-ID, WS-MISC, S-OK
              CANCEL	'/z/y19b25/sp2/prg/psstd'.
              IF S-OK NOT = 'Y'
		 GO TO 0190-MAIN.
              
	      PERFORM SRPT-RTN THRU SRPT-RTN-END.

	0190-MAIN.
           IF END-KEY = HIGH-VALUE
	      INITIALIZE END-KEY.

	      IF AGE-E-KEY = 99
		 MOVE 99 TO AGE-E-KEY.

              IF CY-E-KEY = HIGH-VALUE
		 INITIALIZE CY-E-KEY.

	      IF RC-E-KEY = HIGH-VALUE
		 INITIALIZE RC-E-KEY.

	      IF RG-E-KEY = HIGH-VALUE
		 INITIALIZE RG-E-KEY.
        
	0199-END. EXIT.
      ********************************************************************
        SRPT-RTN.
	   
	   MOVE 'Select Report Type'	TO S-WINDOW-TITLE2.
	   DISPLAY FLOATING WINDOW 
	   LINES 7 SIZE 86 CELL SIZE = ENTRY-FIELD FONT SEPARATE
	   TITLE-BAR MODAL NO SCROLL NO WRAP
	   TITLE S-WINDOW-TITLE2
	   POP-UP S-WINDOW2.
        
	SRPT-SUB.

	   PERFORM ERROR-RTN THRU ERROR-END.
	   DISPLAY SRPT-SCR.
	   ACCEPT  SRPT-SCR.
	   MOVE 4 TO ACCEPT-CONTROL.

	   IF K-ESCAPE GO TO SRPT-ESC.

	   IF NOT (K-ENTER OR KEY-STATUS = 113)
		  GO TO SRPT-SUB.

           EVALUATE REPORT-TYPE-KEY
	     WHEN 1 	CALL 	'/z/y19b25/sp2/prg/ptstd1'
	     		USING LINK-PROG-KEY, S-DATA-ID, SORT-MODE-KEY
                  	CANCEL	'/z/y19b25/sp2/prg/ptstd1'
	     WHEN 2 	CALL 	'/z/y19b25/sp2/prg/ptstd2'
	     		USING LINK-PROG-KEY, S-DATA-ID, SORT-MODE-KEY
                  	CANCEL	'/z/y19b25/sp2/prg/ptstd2'.
           GO TO SRPT-SUB.

        SRPT-ESC.
	   DESTROY SRPT-SCR.
	   CLOSE WINDOW S-WINDOW2.
	   MOVE 101 TO S-CONTROL-ID.

        SRPT-RTN-END. EXIT.
      
      
      ********************************************************************
          COPY '/v/cps/lib/std/errmsg.prd'.

      * End of program.
