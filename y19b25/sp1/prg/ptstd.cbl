       IDENTIFICATION DIVISION.
	PROGRAM-ID.    PTSTD.

      * PRINT STUDENT FILE
      * AUTHOR		DATE	TYPE	A/C	NOTES
      * VAN TZE SHAN	6/8/19	-	PAA	SP1

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION. 
	 FILE-CONTROL.
	   COPY '/z/y19b25/sp1/lib/fd/fcstd'.
	   COPY '/v/cps/lib/std/fcprint'.

       DATA DIVISION.
	FILE SECTION.
           COPY '/z/y19b25/sp1/lib/fd/fdstd'.
	   COPY '/v/cps/lib/std/fdprint'.

        WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp1/lib/fd/dbstd'.
	   COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.
	   COPY '/v/cps/lib/std/dbprint'.
	   COPY RESOURCE '/v/cps/lib/icon/help.jpg'.

       01 WS-MISC.
	  03 START-KEY		PIC X(06).
	  03 END-KEY		PIC X(06).

       01 PRT-HEADER.
          03 PRT-COMPNAME    	PIC X(50).
	  03 FIL		PIC X(07).	
	  03 FIL		PIC X(07) VALUE 'DATE :'.
          03 PRT-SYS-DMY	PIC 99/99/9999.
	  03 FIL		PIC X(02).
	  03 FIL		PIC X(07) VALUE 'PAGE :'.
	  03 PRT-PAGE-COUNT	PIC 9(04).

       01 PRT-HEADER2.
	  03 FIL 		PIC X(50) VALUE
	     'REPORT TITLE: COUNTRY LISTING'.
	  03 FIL                PIC X(07).
          03 FIL 		PIC X(07) VALUE 'TIME :'.
	  03 PRT-START-HHMM	PIC X(07).

       01 PRT-HEADER3.
	  03 FIL		PIC X(56) VALUE
	     'NO.  CODE     NAME                                      '.
 	  03 FIL                PIC X(31) VALUE
	     ' GENDER DOB        COUNTRY RACE'.

       01 PRT-LINE.
	  03 FIL 		PIC X(55) VALUE
	     '---- -------- -----------------------------------------'.
          03 FIL                PIC X(34) VALUE
      	     '- ------ ---------  ------- ----'.

       01 PRT-NEXT-PAGE.
	  03 FIL 		PIC X(16) VALUE
	     '* CONTINUE PAGE'.
          03 PRT-PAGE-COUNT2 	PIC 9(04).
	  03 FIL		PIC X(02) VALUE '*'.

       01 PRT-DETAIL.
	  03 PRT-REC-COUNT	PIC Z(04).
	  03 FIL		PIC X(01).
          03 PRT-STD-KEY	PIC X(06).
	  03 FIL 		PIC X(03).
	  03 PRT-STD-NAME       PIC X(40).
	  03 FIL                PIC X(03).
	  03 PRT-STD-GENDER     PIC X(06).
	  03 FIL                PIC X(01).
	  03 PRT-STD-DOB-DMY    PIC X(08).
          03 FIL                PIC X(03).
	  03 PRT-STD-CY-KEY     PIC X(04).
	  03 FIL                PIC X(04).
	  03 PRT-STD-RC-KEY     PIC X(04).
	  03 FIL                PIC X(01).

       01 PRT-DETAIL1.
	  03 FIL                PIC X(14).
	  03 PRT-STD-ADD1	PIC X(40).

       01 PRT-DETAIL2.
	  03 FIL                PIC X(14).
	  03 PRT-STD-ADD2	PIC X(40).

       01 PRT-DETAIL3.
	  03 FIL                PIC X(14).
	  03 PRT-STD-ADD3       PIC X(40).

       01 PRT-DETAIL4.
	  03 FIL                PIC X(14).

       01 PRT-END.
	  03 FIL		PIC X(26) VALUE
	     '* END OF REPORT * TIME :'. 
	  03 PRT-END-HHMM	PIC X(07).

       LINKAGE SECTION.
       01 LINK-PROG-KEY		PIC X(30).

       SCREEN SECTION.
       01 SELECT-SCR.
	  03 LABEL LINE 02 COL 04 'Student Code:'.
	  03 ENTRY-FIELD 3-D ID 101 COL + 3 PIC X(06)
		   USING START-KEY AUTO.
          03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
             COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 101.
          03 LABEL COL + 3 'to'.
	  03 ENTRY-FIELD 3-D ID 102 COL + 3 PIC X(06) USING END-KEY.
	  03 PUSH-BUTTON 'F10 - Help Table' NO-TAB
	     COL + 1.5 LINES 13 BITMAP
	     BITMAP-HANDLE S-BITMAP
	     BITMAP-NUMBER = 1
	     TERMINATION-VALUE = 102.
          COPY '/v/cps/lib/std/ptbtn.scr'.

      ********************************************************************
       PROCEDURE DIVISION USING LINK-PROG-KEY.

	DECLARATIVES.

	   COPY '/z/y19b25/sp1/lib/fd/dcstd'.
	   COPY '/v/cps/lib/std/dcprint'.

        END DECLARATIVES.

      ********************************************************************
        BEGIN. 

	   SET ENVIRONMENT 'PA-USER-ID' TO 'y19b25'.
	   MOVE 'N' TO S-RUN.
	   OPEN INPUT STD-FILE.
	  
	   COPY '/v/cps/lib/std/gtcoid.prd'.
	   MOVE 'Print Student Listing' TO S-WINDOW-TITLE.
	   COPY '/v/cps/lib/std/ptwin.prd'.

	   CALL 'W$BITMAP' USING
	        WBITMAP-LOAD, 'help.jpg' GIVING S-BITMAP.

           INITIALIZE WS-MISC.
	   MOVE 'Y' TO S-RUN.
	   PERFORM 0100-MAIN THRU 0199-END UNTIL S-RUN = 'N'.
 
        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   CLOSE STD-FILE.
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
	      CALL	'/z/y19b25/sp1/prg/hpstd' USING START-KEY, S-OK
	      CANCEL 	'/z/y19b25/sp1/prg/hpstd'
	      MOVE 101 TO S-CONTROL-ID
	      IF S-OK = 'Y'
		 MOVE 102 TO S-CONTROL-ID
              END-IF
	      GO TO 0100-MAIN.

           IF (K-F10 AND S-CONTROL-ID = 102) OR KEY-STATUS = 102
	      CALL 	'/z/y19b25/sp1/prg/hpstd' USING END-KEY, S-OK
	      CANCEL	'/z/y19b25/sp1/prg/hpstd'
	      MOVE 102 TO S-CONTROL-ID. 
 
           IF NOT K-ENTER GO TO 0100-MAIN.
 
	   IF END-KEY NOT = SPACES AND
	      START-KEY > END-KEY
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 101    TO S-CONTROL-ID
	      GO TO 0100-MAIN.
 
           MOVE 101 TO S-CONTROL-ID.
	   MOVE 80 TO S-PRT-COL.
	   COPY '/v/cps/lib/std/print.prd'.
	   IF PRINT-DATANAME = SPACE
	      GO TO 0199-END.
 
           OPEN OUTPUT PRINT-FILE.
           IF S-STATUS-CHECK = 'Y'
	      GO TO 0199-END.
 
	   IF END-KEY = SPACES
	      MOVE HIGH-VALUE TO END-KEY.
 
	   INITIALIZE STD-REC.
	   MOVE START-KEY TO STD-KEY.
	   MOVE 'Y' TO S-RUN2.
	   START STD-FILE KEY >= STD-KEY INVALID
		 MOVE 'N' TO S-RUN2.

	   COPY '/v/cps/lib/std/s-thread.prd'.

	   WRITE PRINT-REC FROM S-INIT-STRING AFTER 0.
	   MOVE 'Y' TO S-FIRST-PRINT.
	   PERFORM PRT-CONTROL THRU PRT-CONTROL-END.
 
	   PERFORM 0200-PRT THRU 0299-PRT-END
		   UNTIL S-RUN2 = 'N' OR THREAD-RETURN = 99.

	   IF THREAD-RETURN NOT = 99
	      PERFORM PRT-ENDING THRU PRT-ENDING-END.
	     
	   CLOSE PRINT-FILE.
	   COPY '/v/cps/lib/std/e-thread.prd'.
	  
	   IF END-KEY = HIGH-VALUE
	      INITIALIZE END-KEY.
	     
	0199-END. EXIT.

      ********************************************************************
        0200-PRT.
          
	   READ STD-FILE NEXT END
		 MOVE 'N' TO S-RUN2 GO TO 0299-PRT-END.
 
	   IF STD-KEY > END-KEY
	        MOVE 'N' TO S-RUN2 GO TO 0299-PRT-END.
	        
	   INITIALIZE PRT-DETAIL.
	   ADD 1		TO S-REC-COUNT 
	   MOVE S-REC-COUNT	TO PRT-REC-COUNT
	   MOVE STD-KEY		TO PRT-STD-KEY
      	   MOVE STD-NAME	TO PRT-STD-NAME
      	   MOVE STD-ADD1	TO PRT-STD-ADD1
      	   MOVE STD-ADD2        TO PRT-STD-ADD2
      	   MOVE STD-ADD3        TO PRT-STD-ADD3
	   EVALUATE STD-GENDER
	    WHEN 'M' 	MOVE 'Male  ' TO PRT-STD-GENDER
	    WHEN 'F'	MOVE 'Female' TO PRT_STD_GENDER.
	   MOVE STD-DOB-DMY	TO PRT-STD-DOB-DMY
	   MOVE STD-CY-KEY	TO PRT-STD-CY-KEY
	   MOVE STD-RC-KEY      TO PRT-STD-RC-KEY
 
	   PERFORM PRT-CONTROL THRU PRT-CONTROL-END.
	   WRITE PRINT-REC FROM PRT-DETAIL.
	   WRITE PRINT-REC FROM PRT-DETAIL1.
	   WRITE PRINT-REC FROM PRT-DETAIL2.
	   WRITE PRINT-REC FROM PRT-DETAIL3.
           WRITE PRINT-REC FROM PRT-DETAIL4.

        0299-PRT-END. EXIT.

      ********************************************************************
        PRT-CONTROL.

	   IF S-FIRST-PRINT = 'Y' OR LINAGE-COUNTER > 58
	      IF S-FIRST-PRINT = 'Y'
	      	 MOVE 	'N' TO S-FIRST-PRINT
		 CALL	'/v/cps/lib/std/f-dmyhm' USING
			PRT-SYS-DMY, PRT-START-HHMM
		 CANCEL	'/v/cps/lib/std/f-dmyhm'
		 MOVE 'PRESTIGE ATLANTIC' TO PRT-COMPNAME
		 MOVE 0			 TO S-REC-COUNT
		 MOVE 1			 TO S-PAGE-COUNT
 	      ELSE
	   	 ADD 1  		  TO S-PAGE-COUNT
		 MOVE S-PAGE-COUNT TO PRT-PAGE-COUNT2
		 WRITE PRINT-REC FROM PRT-NEXT-PAGE AFTER 2
		 WRITE PRINT-REC FROM SPACE AFTER PAGE
              END-IF
	      MOVE S-PAGE-COUNT TO PRT-PAGE-COUNT
	      WRITE PRINT-REC FROM PRT-HEADER
	      WRITE PRINT-REC FROM PRT-HEADER2
	      WRITE PRINT-REC FROM PRT-HEADER3 AFTER 2
	      WRITE PRINT-REC FROM PRT-LINE.

	PRT-CONTROL-END.EXIT.
      ********************************************************************
        PRT-ENDING.

	   CALL '/v/cps/lib/std/f-dmyhm' USING
	         PRT-SYS-DMY, PRT-START-HHMM
	 	 CANCEL  '/v/cps/lib/std/f-dmyhm'.

	   COMPUTE S-LINE = 62 - LINAGE-COUNTER.
	   WRITE PRINT-REC FROM PRT-END AFTER S-LINE.

        PRT-ENDING-END.EXIT.
      ********************************************************************
          COPY '/v/cps/lib/std/errmsg.prd'.

      * End of program.
