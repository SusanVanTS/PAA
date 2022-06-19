       IDENTIFICATION DIVISION.
	PROGRAM-ID.   PSSSR.

      * Process selected student record onto temp.
      * Author  	DATE    TYPE  A/C  REMARKS
      * VANTZESHAN    	10/9/19	-     PAA  SP2

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	   COPY '/z/y19b25/sp2/lib/fd/fcstd'.
	   COPY '/z/y19b25/sp2/lib/fd/fcssr.t'.

       DATA DIVISION.
        FILE SECTION.
  	   COPY '/z/y19b25/sp2/lib/fd/fdstd'.
 	   COPY '/z/y19b25/sp2/lib/fd/fdssr.t'.

        WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/dbstd'.
	   COPY '/z/y19b25/sp2/lib/fd/dbssr.t'.
	   COPY '/v/cps/lib/std/stdvar.def'.

       01 WS-MISC.
          03 WS-STD-AGE           	PIC 9(02).

       LINKAGE SECTION.
       01 LINK-DATA-ID            	PIC X(08).
       01 LINK-MISC.
          03 LINK-CY-S-KEY    		PIC X(04).
   	  03 LINK-CY-E-KEY      	PIC X(04).
	  03 LINK-RC-S-KEY    		PIC X(04).
	  03 LINK-RC-E-KEY      	PIC X(04).
	  03 LINK-RG-S-KEY    		PIC X(04).
	  03 LINK-RG-E-KEY      	PIC X(04). 
	  03 LINK-WS-GENDER       	PIC 9(02).
	  03 LINK-WS-GENDER2 REDEFINES LINK-WS-GENDER
			  		PIC 9(01) OCCURS 2.
       01 LINK-OK			PIC X(01).
      ********************************************************************

       PROCEDURE DIVISION USING LINK-DATA-ID, LINK-MISC, LINK-OK.

	DECLARATIVES.

	   COPY '/z/y19b25/sp2/lib/fd/dcstd'.
	   COPY '/z/y19b25/sp2/lib/fd/dcssr.t'.

        END DECLARATIVES.
      ********************************************************************
	BEGIN.

	   MOVE 'N' TO S-RUN, LINK-OK.
	   MOVE LINK-DATA-ID TO SSR-T-DATA-ID.
	   OPEN OUTPUT SSR-T-FILE.
	   CLOSE SSR-T-FILE.

	   OPEN I-O SSR-T-FILE.
	   OPEN INPUT STD-FILE.

	   MOVE 'Y' TO S-RUN.
	   MOVE 'N' TO S-STATUS-CHECK.
	   
	   MOVE 30 TO S-SIZE.
	   MOVE 03 TO S-LINES.
	   MOVE'PS' TO S-TYPE.

	   INITIALIZE STD-REC, WS-MISC.
	   START STD-FILE KEY >= STD-KEY INVALID
		 MOVE 'N' TO S-RUN.

           PERFORM 0100-MAIN THRU 0199-END
		UNTIL S-RUN = 'N' OR THREAD-RETURN = 99.

           IF THREAD-RETURN = 99
	      MOVE 'Y' TO LINK-OK.

        TERMINATION.
	   CLOSE WINDOW S-WINDOW.
	   CLOSE SSR-T-FILE, STD-FILE.
	   EXIT PROGRAM.
	   STOP RUN.

      ********************************************************************
	0100-MAIN.

	   READ STD-FILE NEXT END
		MOVE 'N' TO S-RUN GO TO 0199-END.

           IF NOT ((STD-GENDER = 'M' AND LINK-WS-GENDER2(1) = 1) OR
		  (STD-GENDER = 'F' AND LINK-WS-GENDER2(2) = 1))
		  GO TO 0199-END.

           IF (STD-CY-KEY < LINK-CY-S-KEY) OR 
	      (STD-CY-KEY > LINK-CY-E-KEY)
	      GO TO 0199-END.

           IF (STD-RC-KEY < LINK-RC-S-KEY) OR
	      (STD-RC-KEY > LINK-RC-E-KEY)
              GO TO 0199-END.

	   IF (STD-RG-KEY < LINK-RG-S-KEY) OR
	      (STD-RG-KEY > LINK-RG-E-KEY)
              GO TO 0199-END.

	   PERFORM CY-RTN THRU CY-RTN-END.
	   PERFORM RC-RTN THRU RC-RTN-END.
	   PERFORM RG-RTN THRU RG-RTN-END.

	0199-END. EXIT.
      ********************************************************************
	CY-RTN.

	   INITIALIZE SSR-T-REC.
	   MOVE 1 		TO SSR-T-KEY1.
	   MOVE STD-CY-KEY	TO SSR-T-KEY2.
           READ SSR-T-FILE INVALID
	      INITIALIZE SSR-T-DETAILS
           WRITE SSR-T-REC.

	   PERFORM REC-RTN THRU REC-RTN-END.
	   REWRITE SSR-T-REC.

        CY-RTN-END.
      ********************************************************************
	RC-RTN.

	   INITIALIZE SSR-T-REC.
	   MOVE 2 		TO SSR-T-KEY1.
	   MOVE STD-RC-KEY	TO SSR-T-KEY2.
           READ SSR-T-FILE INVALID
	      INITIALIZE SSR-T-DETAILS
           WRITE SSR-T-REC.

	   PERFORM REC-RTN THRU REC-RTN-END.
	   REWRITE SSR-T-REC.

        RC-RTN-END.
      ********************************************************************
	RG-RTN.

	   INITIALIZE SSR-T-REC.
	   MOVE 3 		TO SSR-T-KEY1.
	   MOVE STD-RG-KEY	TO SSR-T-KEY2.
           READ SSR-T-FILE INVALID
	      INITIALIZE SSR-T-DETAILS
           WRITE SSR-T-REC.

	   PERFORM REC-RTN THRU REC-RTN-END.
	   REWRITE SSR-T-REC.

        RG-RTN-END.
      **********************************************************************
        REC-RTN.

	   EVALUATE STD-GENDER
	    WHEN 'M'	ADD 1 TO SSR-T-GENDER(1)
	    WHEN 'F' 	ADD 1 TO SSR-T-GENDER(2)
	    WHEN OTHER 	ADD 1 TO SSR-T-ERRORS.

           CALL		'/z/y19b25/sp2/lib/std/f-gtage'
			USING STD-DOB-DMY, WS-STD-AGE.
	   CANCEL	'/z/y19b25/sp2/lib/std/f-gtage'.

	   EVALUATE WS-STD-AGE
	    WHEN 20 THRU 29	ADD 1 TO SSR-T-AGE(1)
	    WHEN 30 THRU 39	ADD 1 TO SSR-T-AGE(2)
	    WHEN 40 THRU 49	ADD 1 TO SSR-T-AGE(3)
	    WHEN 50 THRU 59	ADD 1 TO SSR-T-AGE(4)
	    WHEN 60 THRU 65	ADD 1 TO SSR-T-AGE(5)
	    WHEN OTHER		ADD 1 TO SSR-T-ERRORS.

           EVALUATE STD-HEIGHT
	    WHEN 110 THRU 120	ADD 1 TO SSR-T-HEIGHT(1)
	    WHEN 121 THRU 130	ADD 1 TO SSR-T-HEIGHT(2)
	    WHEN 131 THRU 140	ADD 1 TO SSR-T-HEIGHT(3)
	    WHEN 141 THRU 150	ADD 1 TO SSR-T-HEIGHT(4)
	    WHEN OTHER		ADD 1 TO SSR-T-ERRORS.

           EVALUATE STD-WEIGHT
	    WHEN 50 THRU 59	ADD 1 TO SSR-T-WEIGHT(1)
	    WHEN 60 THRU 69 	ADD 1 TO SSR-T-WEIGHT(2)
	    WHEN 70 THRU 75	ADD 1 TO SSR-T-WEIGHT(3)
	    WHEN OTHER		ADD 1 TO SSR-T-ERRORS.

        REC-RTN-END. 

      * End of program.
