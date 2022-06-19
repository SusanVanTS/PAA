       IDENTIFICATION DIVISION.
	PROGRAM-ID.	PTSSR.

      * PRINT STUDENT STATISTICS REPORT
      * AUTHOR		DATE	TYPE	A/C	NOTES
      * VANTZESHAN	10/9/19	-	PAA	SP2

       ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	 FILE-CONTROL.
	   COPY	'/z/y19b25/sp2/lib/fd/fccy'.
	   COPY '/z/y19b25/sp2/lib/fd/fcrc'.
	   COPY '/z/y19b25/sp2/lib/fd/fcrg'.
	   COPY '/z/y19b25/sp2/lib/fd/fcstd'.
	   COPY '/z/y19b25/sp2/lib/fd/fcssr.t'.
	   COPY '/v/cps/lib/std/fcprint'.

       DATA DIVISION.
	FILE SECTION.
	   COPY	'/z/y19b25/sp2/lib/fd/fdcy'.
	   COPY '/z/y19b25/sp2/lib/fd/fdrc'.
	   COPY '/z/y19b25/sp2/lib/fd/fdrg'.
	   COPY '/z/y19b25/sp2/lib/fd/fdstd'.
	   COPY '/z/y19b25/sp2/lib/fd/fdssr.t'.
	   COPY '/v/cps/lib/std/fdprint'.
        
	WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/dbcy'.
	   COPY '/z/y19b25/sp2/lib/fd/dbrc'.
	   COPY '/z/y19b25/sp2/lib/fd/dbrg'.
	   COPY '/z/y19b25/sp2/lib/fd/dbstd'.
	   COPY '/z/y19b25/sp2/lib/fd/dbssr.t'.
           COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.
	   COPY '/v/cps/lib/std/dbprint'.
	   COPY RESOURCE '/v/cps/lib/icon/help.jpg'.

       01 WS-KEY	PIC X(01).

       01 PRT-HEADER.
	  03 PRT-COMPNAME	PIC X(147).
	  03 FIL		PIC X(07) VALUE 'DATE :'.
	  03 PRT-SYS-DMY	PIC 99/99/9999.
	  03 FIL		PIC X(02).
	  03 FIL 		PIC X(07) VALUE 'PAGE :'.
	  03 PRT-PAGE-COUNT	PIC 9(04).

       01 PRT-HEADER2.
	  03 FIL		PIC X(147) VALUE
	     'REPORT TITLE: STUDENT STATISTIC REPORT'.
          03 FIL		PIC X(07) VALUE 'TIME :'.
	  03 PRT-START-HHMM	PIC X(07).

       01 PRT-HEADER3.
	  03 FIL		PIC X(07) VALUE 'NO'.
	  03 FIL 		PIC X(09) VALUE 'TYPE'.
	  03 FIL		PIC X(21) VALUE 'NAME'.
	  03 FIL 		PIC X(08) VALUE 'MALE'.
	  03 FIL		PIC X(08) VALUE 'FEMALE'.
	  03 FIL		PIC X(08) VALUE 'AGE'.
	  03 FIL                PIC X(08) VALUE 'AGE'.
	  03 FIL                PIC X(08) VALUE 'AGE'.
	  03 FIL                PIC X(08) VALUE 'AGE'.
	  03 FIL                PIC X(08) VALUE 'AGE'.
	  03 FIL                PIC X(11) VALUE 'HEIGHT'.
          03 FIL                PIC X(11) VALUE 'HEIGHT'.
	  03 FIL                PIC X(11) VALUE 'HEIGHT'.
	  03 FIL                PIC X(11) VALUE 'HEIGHT'.
	  03 FIL                PIC X(11) VALUE 'WEIGHT'.
	  03 FIL                PIC X(11) VALUE 'WEIGHT'.
	  03 FIL                PIC X(11) VALUE 'WEIGHT'.
	  03 FIL                PIC X(08) VALUE 'ERRORS'.

       01 PRT-HEADER4.
	  03 FIL		PIC X(53).
	  03 FIL		PIC X(08) VALUE '20-29'.
	  03 FIL                PIC X(08) VALUE '30-39'.
	  03 FIL                PIC X(08) VALUE '40-49'.
	  03 FIL                PIC X(08) VALUE '50-59'.
	  03 FIL                PIC X(08) VALUE '60-65'.
	  03 FIL                PIC X(11) VALUE '110-120 cm'.
	  03 FIL                PIC X(11) VALUE '121-130 cm'.
	  03 FIL                PIC X(11) VALUE '131-140 cm'.
	  03 FIL                PIC X(11) VALUE '141-150 cm'.
	  03 FIL 		PIC X(11) VALUE '50-59 kg'.
	  03 FIL                PIC X(11) VALUE '60-69 kg'.
	  03 FIL                PIC X(11) VALUE '70-75 kg'.
          03 FIL		PIC X(08).

       01 PRT-LINE.
	  03 FIL		PIC X(06) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(08) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(20) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(07) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(07) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(07) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(07) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(07) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(07) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(07) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(10) VALUE ALL '-'.
	  03 FIL 		PIC X(01).
	  03 FIL                PIC X(10) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL                PIC X(10) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL                PIC X(10) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL                PIC X(10) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL                PIC X(10) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL                PIC X(10) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL		PIC X(07) VALUE ALL '-'.

       01 PRT-NEXT-PAGE.
	  03 FIL		PIC X(16) VALUE 
	     '* CONTINUE PAGE'.
	  03 PRT-PAGE-COUNT2	PIC 9(04).
	  03 FIL		PIC X(02) VALUE ' *'.

       01 PRT-DETAIL.
	  03 PRT-REC-COUNT	PIC Z(06).
	  03 FIL		PIC X(01).
	  03 PRT-TYPE		PIC X(08).
	  03 FIL 		PIC X(01).
	  03 PRT-NAME		PIC X(20).
	  03 FIL                PIC X(01).
	  03 PRT-MALE		PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-FEMALE		PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-AGE1		PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-AGE2		PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-AGE3		PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-AGE4		PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-AGE5		PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-HEIGHT1	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-HEIGHT2        PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-HEIGHT3	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-HEIGHT4	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-WEIGHT1        PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-WEIGHT2        PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-WEIGHT3        PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-ERRORS		PIC ZZZ,ZZ9.

       01 PRT-SUBTOTAL.
	  03 FIL		PIC X(16).
	  03 PRT-ST		PIC X(08).
	  03 FIL		PIC X(13).
	  03 PRT-ST-MALE	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-ST-FEMALE	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-ST-AGE1	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-ST-AGE2	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-ST-AGE3	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-ST-AGE4	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-ST-AGE5	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-ST-HEIGHT1	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-ST-HEIGHT2     PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-ST-HEIGHT3	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-ST-HEIGHT4	PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-ST-WEIGHT1     PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-ST-WEIGHT2     PIC ZZZ,ZZ9.
	  03 FIL                PIC X(04).
	  03 PRT-ST-WEIGHT3     PIC ZZZ,ZZ9.
	  03 FIL                PIC X(01).
	  03 PRT-ST-ERRORS	PIC ZZZ,ZZ9.
 
       01 PRT-ENTER.
	  03 FIL		PIC X(100).

       01 PRT-END.
	  03 FIL 		PIC X(26) VALUE
	     '* END OF REPORT * TIME :'.
          03 PRT-END-HHMM	PIC X(07).

       01 WS-SUBTOTAL.
	  03 WS-GENDER1		PIC 9(06).
	  03 WS-GENDER2		PIC 9(06).
	  03 WS-AGE1		PIC 9(06).
	  03 WS-AGE2		PIC 9(06).
	  03 WS-AGE3		PIC 9(06).
	  03 WS-AGE4		PIC 9(06).
	  03 WS-AGE5		PIC 9(06).
	  03 WS-HEIGHT1		PIC 9(06).
	  03 WS-HEIGHT2		PIC 9(06).
	  03 WS-HEIGHT3		PIC 9(06).
	  03 WS-HEIGHT4		PIC 9(06).
	  03 WS-WEIGHT1		PIC 9(06).
	  03 WS-WEIGHT2		PIC 9(06).
	  03 WS-WEIGHT3		PIC 9(06).
	  03 WS-ERRORS		PIC 9(06).

       LINKAGE SECTION.
       01 LINK-PROG-KEY		PIC X(30).
       01 LINK-DATA-ID		PIC X(08).

      ********************************************************************
       PROCEDURE DIVISION USING LINK-PROG-KEY, LINK-DATA-ID.
	
	DECLARATIVES.

	   COPY '/z/y19b25/sp2/lib/fd/dccy'.
	   COPY '/z/y19b25/sp2/lib/fd/dcrc'.
	   COPY '/z/y19b25/sp2/lib/fd/dcrg'.
	   COPY '/z/y19b25/sp2/lib/fd/dcstd'.
	   COPY '/z/y19b25/sp2/lib/fd/dcssr.t'.
	   COPY '/v/cps/lib/std/dcprint'.

	END DECLARATIVES.
      ********************************************************************
	BEGIN.
	   
	   MOVE 'N' TO S-RUN.
	   MOVE LINK-DATA-ID TO SSR-T-DATA-ID.
	   OPEN INPUT SSR-T-FILE, CY-FILE, RC-FILE, RG-FILE.

	   COPY '/v/cps/lib/std/gtcoid.prd'.
	   MOVE 'Print Student Statistic Report'
		TO S-WINDOW-TITLE.

	   MOVE 'Y' TO S-RUN.
	   MOVE 204 TO S-PRT-COL.
	   COPY '/v/cps/lib/std/print.prd'.
	   IF PRINT-DATANAME = SPACE
	      GO TO TERMINATION.

     	   OPEN OUTPUT PRINT-FILE.
	   IF S-STATUS-CHECK = 'Y'
	      GO TO TERMINATION.

	   COPY '/v/cps/lib/std/s-thread.prd'.

	   WRITE PRINT-REC FROM S-INIT-STRING AFTER 0.
	   MOVE 'Y' TO S-FIRST-PRINT.
	   PERFORM PRT-CONTROL THRU PRT-CONTROL-END.

	   INITIALIZE SSR-T-REC.
	   MOVE 'Y' TO S-RUN.

	   START SSR-T-FILE KEY >= SSR-T-KEY1 INVALID
		 MOVE 'N' TO S-RUN.

	   PERFORM 0200-PRT THRU 0299-PRT-END
		 UNTIL S-RUN = 'N' OR THREAD-RETURN = 99.

                 IF THREAD-RETURN NOT = 99
	            PERFORM PRT-ENDING THRU PRT-ENDING-END.

	   CLOSE PRINT-FILE.

	   COPY '/v/cps/lib/std/e-thread.prd'.

        TERMINATION.
	   CLOSE SSR-T-FILE, CY-FILE, RC-FILE, RG-FILE.
	   EXIT PROGRAM.
	   STOP RUN.
      ********************************************************************
	0200-PRT.

	   READ SSR-T-FILE NEXT END
		WRITE PRINT-REC FROM PRT-SUBTOTAL
		MOVE 'N' TO S-RUN GO TO 0299-PRT-END.

           IF ((SSR-T-KEY1 NOT = WS-KEY) AND
	      (WS-KEY NOT = SPACES))
	      WRITE PRINT-REC FROM PRT-SUBTOTAL
	      WRITE PRINT-REC FROM PRT-ENTER
	      INITIALIZE S-REC-COUNT, WS-SUBTOTAL.

	      INITIALIZE PRT-DETAIL.

           EVALUATE SSR-T-KEY1
	    WHEN 1	PERFORM CY-RTN THRU CY-RTN-END
	    WHEN 2 	PERFORM RC-RTN THRU RC-RTN-END
	    WHEN 3	PERFORM RG-RTN THRU RG-RTN-END.

           WRITE PRINT-REC FROM PRT-DETAIL.

        0299-PRT-END. EXIT.

      ********************************************************************
        CY-RTN.

           PERFORM PRT-CONTROL THRU PRT-CONTROL-END.
	   MOVE SSR-T-KEY1 TO WS-KEY.
	   INITIALIZE CY-REC.
	   MOVE SSR-T-KEY2 TO CY-KEY.
	   READ CY-FILE INVALID
	      INITIALIZE CY-DETAILS.
           MOVE CY-NAME TO PRT-NAME.
	   MOVE 'COUNTRY' TO PRT-TYPE.

	   ADD 1 		TO S-REC-COUNT.
	   MOVE S-REC-COUNT	TO PRT-REC-COUNT.
	   MOVE SSR-T-GENDER(1)	TO PRT-MALE.
	   MOVE SSR-T-GENDER(2)	TO PRT-FEMALE.
	   MOVE SSR-T-AGE(1)	TO PRT-AGE1.
	   MOVE SSR-T-AGE(2)	TO PRT-AGE2.
	   MOVE SSR-T-AGE(3)	TO PRT-AGE3.
	   MOVE SSR-T-AGE(4)	TO PRT-AGE4.
	   MOVE SSR-T-AGE(5)	TO PRT-AGE5.
	   MOVE SSR-T-HEIGHT(1)	TO PRT-HEIGHT1.
	   MOVE SSR-T-HEIGHT(2)	TO PRT-HEIGHT2.
	   MOVE SSR-T-HEIGHT(3)	TO PRT-HEIGHT3.
	   MOVE SSR-T-HEIGHT(4)	TO PRT-HEIGHT4.
	   MOVE SSR-T-WEIGHT(1)	TO PRT-WEIGHT1.
	   MOVE SSR-T-WEIGHT(2)	TO PRT-WEIGHT2.
	   MOVE SSR-T-WEIGHT(3)	TO PRT-WEIGHT3.
	   MOVE SSR-T-ERRORS	TO PRT-ERRORS.

	   PERFORM SUBTOTAL-RTN THRU SUBTOTAL-RTN-END.
        
	CY-RTN-END. EXIT.
      ****************************************************************
        RC-RTN.

           PERFORM PRT-CONTROL THRU PRT-CONTROL-END.
	   MOVE SSR-T-KEY1 TO WS-KEY.
	   INITIALIZE RC-REC.
	   MOVE SSR-T-KEY2 TO RC-KEY.
	   READ RC-FILE INVALID
	      INITIALIZE RC-DETAILS.
           MOVE RC-NAME TO PRT-NAME.
	   MOVE 'RACE' TO PRT-TYPE.

	   ADD 1 		TO S-REC-COUNT.
	   MOVE S-REC-COUNT	TO PRT-REC-COUNT.
	   MOVE SSR-T-GENDER(1)	TO PRT-MALE.
	   MOVE SSR-T-GENDER(2)	TO PRT-FEMALE.
	   MOVE SSR-T-AGE(1)	TO PRT-AGE1.
	   MOVE SSR-T-AGE(2)	TO PRT-AGE2.
	   MOVE SSR-T-AGE(3)	TO PRT-AGE3.
	   MOVE SSR-T-AGE(4)	TO PRT-AGE4.
	   MOVE SSR-T-AGE(5)	TO PRT-AGE5.
	   MOVE SSR-T-HEIGHT(1)	TO PRT-HEIGHT1.
	   MOVE SSR-T-HEIGHT(2)	TO PRT-HEIGHT2.
	   MOVE SSR-T-HEIGHT(3)	TO PRT-HEIGHT3.
	   MOVE SSR-T-HEIGHT(4)	TO PRT-HEIGHT4.
	   MOVE SSR-T-WEIGHT(1)	TO PRT-WEIGHT1.
	   MOVE SSR-T-WEIGHT(2)	TO PRT-WEIGHT2.
	   MOVE SSR-T-WEIGHT(3)	TO PRT-WEIGHT3.
	   MOVE SSR-T-ERRORS	TO PRT-ERRORS.

	   PERFORM SUBTOTAL-RTN THRU SUBTOTAL-RTN-END.
        
	RC-RTN-END. EXIT.
      ****************************************************************
        RG-RTN.

           PERFORM PRT-CONTROL THRU PRT-CONTROL-END.
	   MOVE SSR-T-KEY1 TO WS-KEY.
	   INITIALIZE RG-REC.
	   MOVE SSR-T-KEY2 TO RG-KEY.
	   READ RG-FILE INVALID
	      INITIALIZE RG-DETAILS.
           MOVE RG-NAME TO PRT-NAME.
	   MOVE 'RELIGION' TO PRT-TYPE.

	   ADD 1 		TO S-REC-COUNT.
	   MOVE S-REC-COUNT	TO PRT-REC-COUNT.
	   MOVE SSR-T-GENDER(1)	TO PRT-MALE.
	   MOVE SSR-T-GENDER(2)	TO PRT-FEMALE.
	   MOVE SSR-T-AGE(1)	TO PRT-AGE1.
	   MOVE SSR-T-AGE(2)	TO PRT-AGE2.
	   MOVE SSR-T-AGE(3)	TO PRT-AGE3.
	   MOVE SSR-T-AGE(4)	TO PRT-AGE4.
	   MOVE SSR-T-AGE(5)	TO PRT-AGE5.
	   MOVE SSR-T-HEIGHT(1)	TO PRT-HEIGHT1.
	   MOVE SSR-T-HEIGHT(2)	TO PRT-HEIGHT2.
	   MOVE SSR-T-HEIGHT(3)	TO PRT-HEIGHT3.
	   MOVE SSR-T-HEIGHT(4)	TO PRT-HEIGHT4.
	   MOVE SSR-T-WEIGHT(1)	TO PRT-WEIGHT1.
	   MOVE SSR-T-WEIGHT(2)	TO PRT-WEIGHT2.
	   MOVE SSR-T-WEIGHT(3)	TO PRT-WEIGHT3.
	   MOVE SSR-T-ERRORS	TO PRT-ERRORS.

	   PERFORM SUBTOTAL-RTN THRU SUBTOTAL-RTN-END.
        
	RG-RTN-END. EXIT.
      ****************************************************************
        SUBTOTAL-RTN.
	   
	   IF ((SSR-T-KEY1 NOT = WS-KEY) AND
	      (SSR-T-KEY NOT = ZEROES))
	      GO TO SUBTOTAL-RTN-END.

           ADD SSR-T-GENDER(1) 	TO WS-GENDER1.
	   ADD SSR-T-GENDER(2) 	TO WS-GENDER2.
	   MOVE WS-GENDER1 	TO PRT-ST-MALE.
	   MOVE WS-GENDER2	TO PRT-ST-FEMALE.

	   ADD SSR-T-AGE(1)	TO WS-AGE1.
	   ADD SSR-T-AGE(2) 	TO WS-AGE2.
	   ADD SSR-T-AGE(3)	TO WS-AGE3.
	   ADD SSR-T-AGE(4)	TO WS-AGE4.
	   ADD SSR-T-AGE(5)	TO WS-AGE5.
	   MOVE WS-AGE1		TO PRT-ST-AGE1.
	   MOVE WS-AGE2		TO PRT-ST-AGE2.
	   MOVE WS-AGE3		TO PRT-ST-AGE3.
	   MOVE WS-AGE4		TO PRT-ST-AGE4.
	   MOVE WS-AGE5		TO PRT-ST-AGE5.

	   ADD SSR-T-HEIGHT(1)	TO WS-HEIGHT1.
	   ADD SSR-T-HEIGHT(2)	TO WS-HEIGHT2.
	   ADD SSR-T-HEIGHT(3)	TO WS-HEIGHT3.
	   ADD SSR-T-HEIGHT(4)	TO WS-HEIGHT4.
	   MOVE WS-HEIGHT1	TO PRT-ST-HEIGHT1.
	   MOVE WS-HEIGHT2	TO PRT-ST-HEIGHT2.
	   MOVE WS-HEIGHT3	TO PRT-ST-HEIGHT3.
	   MOVE WS-HEIGHT4	TO PRT-ST-HEIGHT4.

	   ADD SSR-T-WEIGHT(1)	TO WS-WEIGHT1.
	   ADD SSR-T-WEIGHT(2)	TO WS-WEIGHT2.
	   ADD SSR-T-WEIGHT(3)	TO WS-WEIGHT3.
	   MOVE WS-WEIGHT1	TO PRT-ST-WEIGHT1.
	   MOVE WS-WEIGHT2	TO PRT-ST-WEIGHT2.
	   MOVE WS-WEIGHT3	TO PRT-ST-WEIGHT3.

	   ADD SSR-T-ERRORS	TO WS-ERRORS.
	   MOVE WS-ERRORS	TO PRT-ST-ERRORS.

	   MOVE 'SUBTOTAL' 	TO PRT-ST.

        SUBTOTAL-RTN-END. EXIT.
      ********************************************************************
	PRT-CONTROL.

	   IF S-FIRST-PRINT = 'Y' OR LINAGE-COUNTER > 58
	      IF S-FIRST-PRINT = 'Y'
		 MOVE 'N' TO S-FIRST-PRINT
		 CALL	'/v/cps/lib/std/f-dmyhm' 
		 	USING PRT-SYS-DMY, PRT-START-HHMM
                 CANCEL	'/v/cps/lib/std/f-dmyhm'
		 MOVE 'PRESTIGE ATLANTIC'	TO PRT-COMPNAME
		 MOVE 0				TO S-REC-COUNT
		 MOVE 1 			TO S-PAGE-COUNT
	      ELSE 
		 ADD 1			TO S-PAGE-COUNT
		 MOVE S-PAGE-COUNT 	TO PRT-PAGE-COUNT2
		 WRITE PRINT-REC FROM PRT-NEXT-PAGE AFTER 2
		 WRITE PRINT-REC FROM SPACE AFTER PAGE
	   END-IF
	   MOVE S-PAGE-COUNT TO PRT-PAGE-COUNT
	   WRITE PRINT-REC FROM PRT-HEADER
	   WRITE PRINT-REC FROM PRT-HEADER2
	   WRITE PRINT-REC FROM PRT-HEADER3 AFTER 2
	   WRITE PRINT-REC FROM PRT-HEADER4
	   WRITE PRINT-REC FROM PRT-LINE.

        PRT-CONTROL-END. EXIT.
      ********************************************************************
	PRT-ENDING. 
	   
	   CALL		'/v/cps/lib/std/f-dmyhm'
		  USING PRT-SYS-DMY, PRT-END-HHMM
           CANCEL 	'/v/cps/lib/std/f-dmyhm'.

	   COMPUTE S-LINE = 62 - LINAGE-COUNTER.
	   WRITE PRINT-REC FROM PRT-END AFTER S-LINE.

	PRT-ENDING-END. EXIT.
      ********************************************************************
	   COPY		'/v/cps/lib/std/errmsg.prd'.

      * End of program.
