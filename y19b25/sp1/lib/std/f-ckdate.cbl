       IDENTIFICATION DIVISION.
	PROGRAM-ID.   F-CKDATE.

      * VALIDATE DATE.
      * AUTHOR		DATE	TYPE	A/C	NOTES
      * VAN TZE SHAN	5/8/19	-	PAA	SP1

       ENVIRONMENT DIVISION.
	DATA DIVISION.
	WORKING-STORAGE SECTION.
	01 WS-TABLE.
	   03 DAYS			PIC X(24) VALUE
	      '312831303130313130313031'.
           03 DAY-NO REDEFINES DAYS	PIC 9(02) OCCURS 12.

        01 WS-DDMMYYYY.
	   03 WS-DD			PIC 9(02).
	   03 WS-MM			PIC 9(02).
	      88 VALID-MONTH		VALUE 1 THRU 12.  
	   03 WS-YYYY			PIC 9(04).
	      88 LEAP-YEAR		VALUE 0.

        01 WS-YYYYMMDD.
	   03 WS-YYYY2			PIC 9(04).
              88 LEAP-YEAR2             VALUE 0.
	   03 WS-MM2                    PIC 9(02).
	      88 VALID-MONTH2           VALUE 1 THRU 12.
	   03 WS-DD2                    PIC 9(02).

        01 WS-YEAR			PIC 9(04).

	LINKAGE SECTION.
      * LINK TYPE : D/Y
	01 LINK-TYPE			PIC X(01).
	01 LINK-DDMMYYYY		PIC 9(08).
	01 LINK-ERROR-CODE		PIC 9(06).

      *********************************************************************
       PROCEDURE DIVISION USING LINK-TYPE, LINK-DDMMYYYY,
					   LINK-ERROR-CODE.

        BEGIN.

	   INITIALIZE LINK-ERROR-CODE, WS-YEAR.
	   MOVE ZEROS TO LINK-ERROR-CODE.

	   EVALUATE LINK-TYPE
	      WHEN 'D'
		   PERFORM DMY-RTN THRU DMY-END
              WHEN 'Y'
		   PERFORM YMD-RTN THRU YMD-END.

        TERMINATION.

	   EXIT PROGRAM.
	   STOP RUN.

      *********************************************************************
        DMY-RTN.

	   MOVE LINK-DDMMYYYY	TO WS-DDMMYYYY.
	   MOVE WS-YYYY		TO WS-YEAR.

	   DIVIDE WS-YYYY BY 4 GIVING WS-YYYY REMAINDER WS-YYYY.
	   IF LEAP-YEAR MOVE 29 TO DAY-NO(02).

	   IF WS-DD = 0 OR (NOT VALID-MONTH) OR WS-DD > DAY-NO(WS-MM)
	      MOVE 100020 TO LINK-ERROR-CODE.

        DMY-END. EXIT.
      *********************************************************************
        YMD-RTN.
	  
	   MOVE LINK-DDMMYYYY	TO WS-YYYYMMDD.
	   MOVE WS-YYYY2	TO WS-YEAR.

	   DIVIDE WS-YYYY2 BY 4 GIVING WS-YYYY2 REMAINDER WS-YYYY2.
	   IF LEAP-YEAR2 MOVE 29 TO DAY-NO(02).

	   IF WS-DD2 = 0 OR (NOT VALID-MONTH2) OR 
		       WS-DD2 > DAY-NO(WS-MM2)
		       MOVE 100020 TO LINK-ERROR-CODE.

        YMD-END. EXIT.
      *********************************************************************
      * End of program.
