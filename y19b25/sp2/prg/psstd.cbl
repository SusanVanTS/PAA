       IDENTIFICATION DIVISION.
	PROGRAM-ID. PSSTD.

      * Process selected student record onto temp.
      * Author		Date	Remarks
      * Van Tze Shan	5/9/19	Coding for process student

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	  COPY 	'/z/y19b25/sp2/lib/fd/fcstd'.
	  COPY  '/z/y19b25/sp2/lib/fd/fcstd.t'.

       DATA DIVISION.
	FILE SECTION.
	 COPY 	'/z/y19b25/sp2/lib/fd/fdstd'.
	 COPY	'/z/y19b25/sp2/lib/fd/fdstd.t'.

        WORKING-STORAGE SECTION.
	 COPY	'/z/y19b25/sp2/lib/fd/dbstd'.
	 COPY   '/z/y19b25/sp2/lib/fd/dbstd.t'.
	 COPY   '/v/cps/lib/std/stdvar.def'.

       01 WS-MISC.
	  03 WS-STD-AGE 	PIC 9(02).

       LINKAGE SECTION.
       01 LINK-DATA-ID		PIC X(08).
       01 LINK-MISC.
	  03 LINK-START-KEY	PIC X(08).
	  03 LINK-END-KEY	PIC X(08).	
	  03 LINK-WS-GENDER	PIC 9(02).
	  03 LINK-WS-GENDER2 REDEFINES LINK-WS-GENDER
	     			PIC 9(01) OCCURS 2.
          03 LINK-AGE-S-KEY	PIC 9(02).
	  03 LINK-AGE-E-KEY	PIC 9(02).
	  03 LINK-CY-S-KEY	PIC X(04).
	  03 LINK-CY-E-KEY	PIC X(04).
	  03 LINK-RC-S-KEY	PIC X(04).
	  03 LINK-RC-E-KEY	PIC X(04).
	  03 LINK-RG-S-KEY	PIC X(04).
	  03 LINK-RG-E-KEY	PIC X(04).
       01 LINK-OK		PIC X(01).

      ********************************************************************
       PROCEDURE DIVISION USING LINK-DATA-ID, LINK-MISC, LINK-OK.
	
	DECLARATIVES.

	   COPY   '/z/y19b25/sp2/lib/fd/dcstd'.
	   COPY   '/z/y19b25/sp2/lib/fd/dcstd.t'.

        END DECLARATIVES.

      ********************************************************************
        BEGIN.

	   MOVE 'N'		TO S-RUN, LINK-OK.
	   MOVE LINK-DATA-ID	TO STD-T-DATA-ID.
	   OPEN OUTPUT STD-T-FILE.
	   CLOSE STD-T-FILE.

	   OPEN I-O STD-T-FILE.
	   OPEN INPUT STD-FILE.

      * Main logic
	   MOVE 'Y'	TO S-RUN.
	   MOVE 'N' 	TO S-STATUS-CHECK.

	   MOVE 30 TO S-SIZE.
	   MOVE 03 TO S-LINES.
	   MOVE 'PS' TO S-TYPE.

	   COPY   '/v/cps/lib/std/s-thread.prd'.

	   INITIALIZE STD-REC, WS-MISC.
	   MOVE LINK-START-KEY TO STD-KEY.
	   START STD-FILE KEY >= STD-KEY INVALID
	   MOVE 'N' TO S-RUN.

           PERFORM 0100-MAIN THRU 0199-END
	      UNTIL S-RUN = 'N' OR THREAD-RETURN = 99.

           COPY   '/v/cps/lib/std/e-thread.prd'.

	   IF THREAD-RETURN NOT = 99
	      MOVE 'Y' TO LINK-OK.

        TERMINATION.
	   CLOSE STD-T-FILE, STD-FILE.
	   EXIT PROGRAM.
	   STOP RUN.

      ********************************************************************
        0100-MAIN.
	   READ STD-FILE NEXT END
	      MOVE 'N' TO S-RUN	GO TO 0199-END.

           IF STD-KEY > LINK-END-KEY
	      MOVE 'N' TO S-RUN    GO TO 0199-END.

	   IF NOT ((STD-GENDER = 'M' AND LINK-WS-GENDER2(1) = 1) OR
		  (STD-GENDER = 'F' AND LINK-WS-GENDER2(2) =1))
		  GO TO 0199-END.
           
	   CALL		'/z/y19b25/sp2/lib/std/f-gtage'
			USING STD-DOB-DMY, WS-STD-AGE.
           CANCEL	'/z/y19b25/sp2/lib/std/f-gtage'.
           IF (WS-STD-AGE < LINK-AGE-S-KEY) OR 
	      (WS-STD-AGE > LINK-AGE-E-KEY)
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

           INITIALIZE STD-T-REC.
	   MOVE STD-KEY		TO STD-T-KEY.
	   MOVE STD-NAME	TO STD-T-NAME.
	   MOVE STD-ADD1	TO STD-T-ADD1.
	   MOVE STD-ADD2	TO STD-T-ADD2.
	   MOVE STD-ADD3	TO STD-T-ADD3.
	   MOVE STD-GENDER 	TO STD-T-GENDER.
	   MOVE STD-DOB-DMY	TO STD-T-DOB-DMY.
	   MOVE WS-STD-AGE	TO STD-T-AGE.
	   MOVE STD-HEIGHT	TO STD-T-HEIGHT.
	   MOVE STD-WEIGHT      TO STD-T-WEIGHT.
	   MOVE STD-CY-KEY	TO STD-T-CY-KEY.
	   MOVE STD-RC-KEY	TO STD-T-RC-KEY.
	   MOVE STD-RG-KEY      TO STD-T-RG-KEY.
	   MOVE STD-EMAIL	TO STD-T-EMAIL.
	   MOVE STD-MOBILE	TO STD-T-MOBILE.

	   WRITE STD-T-REC.

        0199-END. EXIT.

      * End of program




