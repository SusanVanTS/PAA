       IDENTIFICATION DIVISION.
	PROGRAM-ID. F-CKSON.

      * CHECK FATHER & SON RELATION
      * AUTHOR 		DATE	TYPE	A/C	NOTES
      * VANTZESHAN	10/9/19	-	PAA	SP2

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	   COPY	'/z/y19b25/sp2/lib/fd/fcstd'.

       DATA DIVISION.
	FILE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/fdstd'.

        WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/dbstd'.
	   COPY '/v/cps/lib/std/stdvar.def'.

       01 WS-TYPE	PIC X(02).

       01 WS-KEY	PIC X(02).

       LINKAGE SECTION.
       01 LINK-TYPE	PIC X(02).
       01 LINK-KEY	PIC X(04).
       01 LINK-OK 	PIC X(01).

      ********************************************************************
       PROCEDURE DIVISION USING LINK-TYPE, LINK-KEY, LINK-OK.
	
	DECLARATIVES.
	 
           COPY '/z/y19b25/sp2/lib/fd/dcstd'.

        END DECLARATIVES.

      ********************************************************************
	BEGIN.

	   INITIALIZE WS-TYPE, LINK-OK.
	   OPEN INPUT STD-FILE.
	   MOVE 'Y' TO LINK-OK.
	   MOVE LINK-TYPE TO WS-TYPE.
	   MOVE LINK-KEY TO WS-KEY.
           
	   EVALUATE LINK-TYPE
	    WHEN 'cy'	PERFORM CY-RTN THRU CY-RTN-END
	    WHEN 'rc'	PERFORM RC-RTN THRU RC-RTN-END
	    WHEN 'rg'	PERFORM RG-RTN THRU RG-RTN-END.

        TERMINATION.
	   CLOSE STD-FILE.
	   EXIT PROGRAM.
	   STOP RUN.

      ********************************************************************
        CY-RTN.

           INITIALIZE STD-REC.
	   MOVE WS-KEY TO STD-CY-KEY.
	   START STD-FILE KEY > STD-ALT-KEY1 INVALID
	     NOT INVALID 
	     READ STD-FILE NEXT END
		  NOT END
		  IF WS-KEY = STD-CY-KEY
		     MOVE 'N' TO LINK-OK
		     GO TO CY-RTN-END
		  END-IF
             END-READ.

        CY-RTN-END. EXIT.
      ********************************************************************
	RC-RTN.

           INITIALIZE STD-REC.
	   MOVE WS-KEY TO STD-RC-KEY.
	   START STD-FILE KEY > STD-ALT-KEY2 INVALID
	     NOT INVALID 
	     READ STD-FILE NEXT END
		  NOT END
		  IF WS-KEY = STD-RC-KEY
		     MOVE 'N' TO LINK-OK
		     GO TO RC-RTN-END
		  END-IF
             END-READ.

        RC-RTN-END. EXIT.
      ********************************************************************
        RG-RTN.

           INITIALIZE STD-REC.
	   MOVE WS-KEY TO STD-RG-KEY.
	   START STD-FILE KEY > STD-ALT-KEY3 INVALID
	     NOT INVALID 
	     READ STD-FILE NEXT END
		  NOT END
		  IF WS-KEY = STD-RG-KEY
		     MOVE 'N' TO LINK-OK
		     GO TO RG-RTN-END
		  END-IF
             END-READ.

        RG-RTN-END. EXIT.

      * End of program


	
