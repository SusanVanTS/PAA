       IDENTIFICATION DIVISION.
	PROGRAM-ID.   FMCFIG.
	
      * MAINTAIN COUNTRY FILE
      * AUTHOR		DATE	 TYPE	A/C	NOTES
      * VAN TZE SHAN 	1/8/2019 -	PAA	SP2

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	   COPY '/z/y19b25/sp2/lib/fd/fccfig'.

       DATA DIVISION.
	FILE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/fdcfig'.

        WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/dbcfig'.
	   COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.
	   COPY RESOURCE '/v/cps/lib/icon/help.jpg'.

        LINKAGE SECTION.
	01 LINK-PROG-KEY	  PIC X(30).

	SCREEN SECTION.
	01 PROCESS-SCR.
	   03 FRAME LINE 02 COL 04 'System Parameters'
		    LINES 4.5 CELL SIZE 106 ENGRAVED HIGH. 
	   03 LABEL LINE + 1 COL 06 'Age Allowable:'.
	   03 ENTRY-FIELD 3-D ID 101 COL 23 PIC 9(02) USING
		    CFIG-AGE-MIN AUTO.
           03 LABEL COL + 1.5 'years old'.
	   03 ENTRY-FIELD 3-D ID 102 COL + 3 PIC 9(02) USING 
		    CFIG-AGE-MAX AUTO.
           03 LABEL COL + 1.5 'years old'.
	   03 LABEL LINE + 1 COL 06 'Height Allowable:'.
	   03 ENTRY-FIELD 3-D ID 103 COL 23 PIC ZZ9.99 USING
		    CFIG-HEIGHT-MIN AUTO.
           03 LABEL COL + 1.5 'cm'.
	   03 LABEL COL + 2.7 'to'.
	   03 ENTRY-FIELD 3-D ID 104 COL + 3 PIC ZZ9.99 USING 
		    CFIG-HEIGHT-MAX AUTO.
           03 LABEL COL + 1.5 'cm'.
	   03 LABEL LINE + 1 COL 06 'Weight Allowable:'.
	   03 ENTRY-FIELD 3-D ID 105 COL 23 PIC ZZ9.99 USING
		    CFIG-WEIGHT-MIN AUTO.
           03 LABEL COL + 1.5 'kg'.
	   03 LABEL COL + 3 'to'.
	   03 ENTRY-FIELD 3-D ID 106 COL + 3 PIC ZZ9.99 USING 
		    CFIG-WEIGHT-MAX AUTO.
           03 LABEL COL + 1.5 'kg'.
      *******************************************************************
       PROCEDURE DIVISION USING LINK-PROG-KEY.

	DECLARATIVES.
	 
	   COPY '/z/y19b25/sp2/lib/fd/dccfig'.

        END DECLARATIVES.
      *******************************************************************
        BEGIN.

	   MOVE 'N' TO S-RUN.
	   OPEN I-O CFIG-FILE.

      * Floating Window
	   Move 'System Configuration' TO S-WINDOW-TITLE.
	   COPY '/v/cps/lib/std/floatwin.prd'.
           PERFORM FKEY-RTN THRU FKEY-END.

           INITIALIZE CFIG-REC.
	   MOVE ZEROES TO CFIG-KEY.
	   READ CFIG-FILE.
	   IF S-STATUS-CHECK = 'Y'
	      GO TO TERMINATION.
           MOVE 'Y' TO S-RUN.
	   MOVE 101 TO S-CONTROL-ID.
	   PERFORM 0100-MAIN THRU 0199-END.
        
	TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   CLOSE CFIG-FILE.
	   EXIT PROGRAM.
	   STOP RUN.

      **************************************************************
        0100-MAIN.

	   PERFORM ERROR-RTN THRU ERROR-END.
	   DISPLAY PROCESS-SCR.
	   ACCEPT  PROCESS-SCR.
	   MOVE 4 TO ACCEPT-CONTROL.

	   IF K-ESCAPE 
	      MOVE 'N' TO S-RUN GO TO 0190-MAIN.

           IF NOT (K-F8 OR K-ENTER) GO TO 0100-MAIN.

	   IF CFIG-AGE-MIN = ZEROES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 101    TO S-CONTROL-ID
	      GO TO 0100-MAIN.

	   IF CFIG-AGE-MAX = ZEROES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 102    TO S-CONTROL-ID
	      GO TO 0100-MAIN.
           
	   IF CFIG-AGE-MAX NOT = ZEROES AND
	      CFIG-AGE-MAX < CFIG-AGE-MIN
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 102    TO S-CONTROL-ID
	      GO TO 0100-MAIN.

	   IF CFIG-HEIGHT-MIN = ZEROES
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 103    TO S-CONTROL-ID
	      GO TO 0100-MAIN.

	   IF CFIG-HEIGHT-MAX = ZEROES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 104    TO S-CONTROL-ID
	      GO TO 0100-MAIN.

	   IF (CFIG-HEIGHT-MAX NOT = ZEROES AND
	       CFIG-HEIGHT-MAX < CFIG-HEIGHT-MIN)
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 104    TO S-CONTROL-ID
	      GO TO 0100-MAIN.

	   IF CFIG-WEIGHT-MIN = ZEROES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 105    TO S-CONTROL-ID
	      GO TO 0100-MAIN.

	   IF CFIG-WEIGHT-MAX = ZEROES 
	      MOVE 200015 TO S-ERROR-CODE
	      MOVE 106    TO S-CONTROL-ID
	      GO TO 0100-MAIN.

	   IF (CFIG-WEIGHT-MAX NOT = ZEROES AND
	       CFIG-WEIGHT-MAX < CFIG-WEIGHT-MIN)
	      MOVE 100035 TO S-ERROR-CODE
	      MOVE 106    TO S-CONTROL-ID
	      GO TO 0100-MAIN.

           PERFORM CONFIRM-RTN THRU CONFIRM-END.
	   IF S-CONFIRM NOT = 'Y'
	      GO TO 0100-MAIN.

           REWRITE CFIG-REC.

        0190-MAIN.

	   DESTROY PROCESS-SCR.

        0199-END. EXIT.
      *******************************************************************
        FKEY-RTN.

	   MOVE '1234567y9012y4567890' TO S-ACTIVE-FKEY.

           CALL   '/v/cps/lib/std/x-fkey ' USING
	          S-ACTIVE-FKEY,  S-TOOLBAR, S-BUTTON.
	   CANCEL '/v/cps/lib/std/x-fkey'.
           COPY '/v/cps/lib/std/fmmode.prd'.

	FKEY-END. EXIT.

      ***************************************************************

	   COPY '/v/cps/lib/std/cfirm.prd'.
	   COPY '/v/cps/lib/std/errmsg.prd'.

      *End of Program.
