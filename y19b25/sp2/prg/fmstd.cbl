       IDENTIFICATION DIVISION.
	PROGRAM-ID.   FMSTD.
	
      * MAINTAIN STUDENT FILE
      * AUTHOR		DATE	 TYPE	A/C	NOTES
      * VAN TZE SHAN 	4/9/2019 -	PAA	SP2

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	   COPY '/z/y19b25/sp2/lib/fd/fcstd'.
	   COPY '/z/y19b25/sp2/lib/fd/fccy'.
	   COPY '/z/y19b25/sp2/lib/fd/fcrc'.
	   COPY '/z/y19b25/sp2/lib/fd/fcrg'.
	   COPY '/z/y19b25/sp2/lib/fd/fccfig'.

       DATA DIVISION.
	FILE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/fdstd'.
	   COPY '/z/y19b25/sp2/lib/fd/fdcy'.
	   COPY '/z/y19b25/sp2/lib/fd/fdrc'.
	   COPY '/z/y19b25/sp2/lib/fd/fdrg'.
	   COPY '/z/y19b25/sp2/lib/fd/fdcfig'.

        WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/dbstd'.
	   COPY '/z/y19b25/sp2/lib/fd/dbcy'.
	   COPY '/z/y19b25/sp2/lib/fd/dbrc'.
	   COPY '/z/y19b25/sp2/lib/fd/dbrg'.
	   COPY '/z/y19b25/sp2/lib/fd/dbcfig'.
	   COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.
	   COPY RESOURCE '/v/cps/lib/icon/help.jpg'.

	01 WS-OPTION	       	PIC X(01).
	01 STD-AGE		PIC 9(02). 
	01 WS-DATE		PIC X(01).

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
	   03 ENTRY-FIELD 3-D ID 102 COL 17 PIC X(40) 
	      USING STD-NAME AUTO.
	   03 LABEL LINE 04 COL 04 'Address:'.
	   03 ENTRY-FIELD 3-D ID 103 COL 17 PIC X(40)
	      USING STD-ADD1 AUTO.
	   03 ENTRY-FIELD 3-D ID 104 LINE 05 COL 17 PIC X(40)
	      USING STD-ADD2 AUTO.
	   03 ENTRY-FIELD 3-D ID 105 LINE 06 COL 17 PIC X(40) 
	      USING STD-ADD3 AUTO.
	   03 LABEL LINE 07 COL 04 'Gender:'.
	   03 RADIO-BUTTON LINE + 0.1 COL 17
	      GROUP = 1 GROUP-VALUE = 1 VALUE WS-OPTION.
           03 LABEL LINE - 0.1 COL + 1 'Male'.
	   03 RADIO-BUTTON LINE + 0.1 COL + 3
	      GROUP = 1 GROUP-VALUE = 2 VALUE WS-OPTION.
           03 LABEL LINE - 0.1 COL + 1 'Female'.
 	   03 LABEL LINE 08 COL 04 'DOB:'.
	   03 ENTRY-FIELD 3-D ID 106 COL 17 PIC 99/99/9999
	      USING STD-DOB-DMY AUTO.
	   03 PUSH-BUTTON 'F10 - Calender Help Table' NO-TAB
	      COL + 1.5 
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER     = 1
	      TERMINATION-VALUE = 106.
	   03 LABEL COL + 1 ' ('.
	   03 LABEL COL + 1 PIC Z9 FROM STD-AGE. 
	   03 LABEL COL + 1 ' Years Old)'. 
	   03 LABEL LINE 09 COL 04 'Height:'.
	   03 ENTRY-FIELD 3-D ID 107 COL 17 PIC ZZ9.99 
	      USING STD-HEIGHT AUTO.
	   03 LABEL COL + 1.5 'cm'.
	   03 LABEL LINE 10 COL 04 'Weight:'.
	   03 ENTRY-FIELD 3-D ID 108 COL 17 PIC ZZ9.99 
	      USING STD-WEIGHT AUTO.
           03 LABEL COL + 1.5 'kg'.
	   03 LABEL LINE 11 COL 04 'Country:'.
	   03 ENTRY-FIELD 3-D ID 109 COL 17 PIC X(02) 
	      USING STD-CY-KEY AUTO.
	   03 PUSH-BUTTON 'F10 - Country Help Table' NO-TAB
	      COL + 1.5 
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER     = 1
	      TERMINATION-VALUE = 109.
	   03 LABEL LINE 12 COL 04 'Race:'.
	   03 ENTRY-FIELD 3-D ID 110 COL 17 PIC X(02) 
	      USING STD-RC-KEY AUTO.
	   03 PUSH-BUTTON 'F10 - Race Help Table' NO-TAB
	      COL + 1.5 
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER     = 1
	      TERMINATION-VALUE = 110.
	   03 LABEL LINE 13 COL 04 'Religion:'.
	   03 ENTRY-FIELD 3-D ID 111 COL 17 PIC X(02) 
	      USING STD-RG-KEY AUTO.
	   03 PUSH-BUTTON 'F10 - Religion Help Table' NO-TAB
	      COL + 1.5 
	      BITMAP-HANDLE S-BITMAP
	      BITMAP-NUMBER     = 1
	      TERMINATION-VALUE = 111.
	   03 LABEL LINE 14 COL 04 'Email:#'.
	   03 ENTRY-FIELD 3-D ID 112 COL 17 PIC X(30) 
	      USING STD-EMAIL AUTO.
	   03 LABEL LINE 15 COL 04 'Mobile:#'.
 	   03 ENTRY-FIELD 3-D ID 113 COL 17 PIC X(30)
	      USING STD-MOBILE AUTO.

      *******************************************************************
       PROCEDURE DIVISION USING LINK-PROG-KEY.

	DECLARATIVES.
	 
	   COPY '/z/y19b25/sp2/lib/fd/dcstd'.
	   COPY '/z/y19b25/sp2/lib/fd/dccy'.
	   COPY '/z/y19b25/sp2/lib/fd/dcrc'.
	   COPY '/z/y19b25/sp2/lib/fd/dcrg'.
	   COPY '/z/y19b25/sp2/lib/fd/dccfig'.

        END DECLARATIVES.
      *******************************************************************
        BEGIN.

	   MOVE 'N' TO S-RUN.
	   OPEN I-O STD-FILE.
	   OPEN INPUT CY-FILE.
	   OPEN INPUT RC-FILE.
	   OPEN INPUT RG-FILE.
	   OPEN INPUT CFIG-FILE.

	   INITIALIZE CFIG-REC.
	   MOVE ZEROES TO CFIG-KEY.
	   MOVE ZEROES TO STD-AGE.
	   READ CFIG-FILE.
	   CLOSE CFIG-FILE.

      * Floating Window
	   Move 'Define Student' TO S-WINDOW-TITLE.
	   COPY '/v/cps/lib/std/floatwin.prd'.

      * Bitmap
	   CALL 'W$BITMAP' USING
	         WBITMAP-LOAD, 'help.jpg' GIVING S-BITMAP.

           MOVE 'Y' TO S-RUN.
           INITIALIZE STD-REC.
           PERFORM 0100-MAIN THRU 0199-END UNTIL S-RUN = 'N'.
     
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
	      CALL   '/z/y19b25/sp2/prg/vwstd' USING LINK-PROG-KEY
	      CANCEL '/z/y19b25/sp2/prg/vwstd'
	      GO TO 0110-MAIN.

           IF (K-F10 and S-CONTROL-ID = 101) OR KEY-STATUS = 101
	      CALL   '/z/y19b25/sp2/prg/hpstd' USING STD-KEY,S-OK
	      CANCEL '/z/y19b25/sp2/prg/hpstd'
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
		   MOVE ZEROS TO STD-PADDING.

	      EVALUATE STD-GENDER
	        WHEN 'M' MOVE '1' TO WS-OPTION 
	        WHEN 'F' MOVE '2' TO WS-OPTION
		WHEN ' ' MOVE '1' TO WS-OPTION.

	      IF S-PRS-MODE = 'R'
		 PERFORM GET-AGE THRU GET-AGE-END.

	      IF S-STATUS-CHECK = 'Y' GO TO 0190-MAIN.

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

	   IF (K-ENTER OR K-F8)
	      PERFORM GET-AGE THRU GET-AGE-END
	      IF S-ERROR-CODE > 0
		 GO TO 0130-MAIN.
	   
	   IF STD-NAME = SPACES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 102    TO S-CONTROL-ID
	      GO TO 0130-MAIN.

	   IF (K-F10 and S-CONTROL-ID = 101) OR KEY-STATUS = 101
	      CALL   '/z/y19b25/sp2/prg/hpstd' USING STD-KEY,S-OK
	      CANCEL '/z/y19b25/sp2/prg/hpstd'
	      MOVE 101 TO S-CONTROL-ID
	      IF S-OK ='Y'
		 DISPLAY SELECT-SCR
	      END-IF
	         GO TO 0130-MAIN.   
	   
	   IF STD-ADD1 = SPACES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 103    TO S-CONTROL-ID
	      GO TO 0130-MAIN.

	   IF (K-F10 and S-CONTROL-ID = 106) OR KEY-STATUS = 106 		
	      CALL 	'/z/y19b25/sp2/lib/std/x-hpcal' 
			USING STD-DOB-DMY,S-OK
	      CANCEL	'/z/y19b25/sp2/lib/std/x-hpcal'
	      MOVE 106  TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 106  TO S-CONTROL-ID
	      END-IF	
	         PERFORM GET-AGE THRU GET-AGE-END.
           
	   IF STD-DOB-DMY = SPACES
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 106    TO S-CONTROL-ID
	      GO TO 0130-MAIN
	   ELSE 
	      MOVE 'D' TO WS-DATE
	      CALL	'/z/y19b25/sp2/lib/std/f-ckdate'
	      		USING WS-DATE, STD-DOB-DMY, S-ERROR-CODE
	      CANCEL	'/z/y19b25/sp2/lib/std/f-ckdate'
	      IF S-ERROR-CODE NOT = ZEROS
		 MOVE 106 TO S-CONTROL-ID
		 MOVE 100020 TO S-ERROR-CODE
		 GO TO 0130-MAIN
              END-IF.

	   IF (STD-AGE < CFIG-AGE-MIN) OR
	      (STD-AGE > CFIG-AGE-MAX)
	      MOVE 100035  TO S-ERROR-CODE
	      MOVE 106     TO S-CONTROL-ID
	      GO TO 0130-MAIN
	   END-IF.
	   
	   IF STD-HEIGHT = ZEROES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 107    TO S-CONTROL-ID
	      GO TO 0130-MAIN.

	   IF STD-HEIGHT > CFIG-HEIGHT-MAX OR 
	      STD-HEIGHT < CFIG-HEIGHT-MIN
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 107 TO S-CONTROL-ID
	      GO TO 0130-MAIN. 
	   
	   IF STD-WEIGHT = ZEROES
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 108    TO S-CONTROL-ID
	      GO TO 0130-MAIN.

	   IF STD-WEIGHT > CFIG-WEIGHT-MAX OR 
	      STD-WEIGHT < CFIG-WEIGHT-MIN
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 108 TO S-CONTROL-ID
	      GO TO 0130-MAIN.
	   
	   IF STD-CY-KEY = SPACES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 109    TO S-CONTROL-ID
	      GO TO 0130-MAIN.

	   IF (K-F10 and S-CONTROL-ID = 109) OR KEY-STATUS = 109
	      CALL      '/z/y19b25/sp2/prg/hpcy' 
	      		USING STD-CY-KEY, S-OK
	      CANCEL    '/z/y19b25/sp2/prg/hpcy'
	      MOVE 109       TO S-CONTROL-ID
	      IF S-OK = 'Y'
	   	 MOVE 109 	TO S-CONTROL-ID
	      END-IF 	 
	         GO TO 0130-MAIN.
	   
	   INITIALIZE CY-REC.
	   MOVE STD-CY-KEY TO CY-KEY.
	   READ CY-FILE.
	   IF S-STATUS-CHECK = 'Y'
	      MOVE 109 TO S-CONTROL-ID
	      GO TO 0130-MAIN.
	   
	   IF STD-RC-KEY = SPACES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 110    TO S-CONTROL-ID
	      GO TO 0130-MAIN.

	   IF (K-F10 and S-CONTROL-ID = 110) OR KEY-STATUS = 110
	      CALL      '/z/y19b25/sp2/prg/hprc' 
			USING STD-RC-KEY, S-OK
	      CANCEL    '/z/y19b25/sp2/prg/hprc'
	      MOVE 110 TO S-CONTROL-ID	
	      IF S-OK = 'Y'
		 MOVE 110 TO S-CONTROL-ID
	      END-IF
		 GO TO 0130-MAIN.
	   
	   INITIALIZE RC-REC.
	   MOVE STD-RC-KEY TO RC-KEY.
	   READ RC-FILE.
	   IF S-STATUS-CHECK = 'Y'
	      MOVE 110 TO S-CONTROL-ID
	      GO TO 0130-MAIN.
	   
	   IF STD-RG-KEY = SPACES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 111    TO S-CONTROL-ID
	      GO TO 0130-MAIN.
	   
	   IF (K-F10 and S-CONTROL-ID = 111) OR KEY-STATUS = 111
	      CALL      '/z/y19b25/sp2/prg/hprg' 
			USING STD-RG-KEY, S-OK
	      CANCEL    '/z/y19b25/sp2/prg/hprg'
	      MOVE 111 TO S-CONTROL-ID	
	      IF S-OK = 'Y'
		 MOVE 111 TO S-CONTROL-ID
	      END-IF
		 GO TO 0130-MAIN.
	   
	   INITIALIZE RG-REC.
           MOVE STD-RG-KEY TO RG-KEY.
   	   READ RG-FILE.
   	   IF S-STATUS-CHECK = 'Y'
	      MOVE 111 TO S-CONTROL-ID
	      GO TO 0130-MAIN.
	   
	   IF STD-EMAIL = SPACES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 112    TO S-CONTROL-ID
	      GO TO 0130-MAIN.

	   IF STD-MOBILE = SPACES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 113    TO S-CONTROL-ID
	      GO TO 0130-MAIN.
	   
           IF NOT (K-F8 OR K-ENTER) GO TO 0130-MAIN.
	   
	   PERFORM CONFIRM-RTN THRU CONFIRM-END.
	   IF S-CONFIRM NOT = 'Y'
	      GO TO 0130-MAIN.

           IF S-PRS-MODE = 'A' WRITE STD-REC.
	   IF S-PRS-MODE = 'R' REWRITE STD-REC.

        0190-MAIN.

	   DESTROY PROCESS-SCR.

        0199-END. EXIT.
      *******************************************************************
	GET-AGE.
	   
	   INITIALIZE STD-AGE.

	   IF STD-DOB-DMY NOT = SPACES
	      CALL	'/z/y19b25/sp2/lib/std/f-gtage'
			USING STD-DOB-DMY, STD-AGE
              CANCEL	'/z/y19b25/sp2/lib/std/f-gtage'.

           DISPLAY PROCESS-SCR.
        
	GET-AGE-END. EXIT.

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
