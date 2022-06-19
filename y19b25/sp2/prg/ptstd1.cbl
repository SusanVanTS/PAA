       IDENTIFICATION DIVISION.
	PROGRAM-ID.    PTSTD1.

      * PRINT STUDENT FILE
      * AUTHOR		DATE	TYPE	A/C	NOTES
      * VAN TZE SHAN	5/9/19	-	PAA	SP2

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION. 
	 FILE-CONTROL.
	   COPY '/z/y19b25/sp2/lib/fd/fcstd.t'.
	   COPY '/z/y19b25/sp2/lib/fd/fccy'.
	   COPY '/z/y19b25/sp2/lib/fd/fcrc'.
	   COPY '/z/y19b25/sp2/lib/fd/fcrg'.
	   COPY '/v/cps/lib/std/fcprint'.

       DATA DIVISION.
	FILE SECTION.
           COPY '/z/y19b25/sp2/lib/fd/fdstd.t'.
	   COPY '/z/y19b25/sp2/lib/fd/fdcy'.
	   COPY '/z/y19b25/sp2/lib/fd/fdrc'.
	   COPY '/z/y19b25/sp2/lib/fd/fdrg'.
	   COPY '/v/cps/lib/std/fdprint'.

        WORKING-STORAGE SECTION.
	   COPY '/z/y19b25/sp2/lib/fd/dbstd.t'.
	   COPY '/z/y19b25/sp2/lib/fd/dbcy'.
	   COPY '/z/y19b25/sp2/lib/fd/dbrc'.
	   COPY '/z/y19b25/sp2/lib/fd/dbrg'.
	   COPY '/v/cps/lib/std/stdvar.def'.
	   COPY '/v/cps/lib/std/fkey.def'.
	   COPY '/v/cps/lib/std/dbprint'.
	   COPY RESOURCE '/v/cps/lib/icon/help.jpg'.

       01 PRT-HEADER.
          03 PRT-COMPNAME    	PIC X(181).
	  03 FIL		PIC X(07) VALUE 'DATE :'.
          03 PRT-SYS-DMY	PIC 99/99/9999.
	  03 FIL		PIC X(02).
	  03 FIL		PIC X(07) VALUE 'PAGE :'.
	  03 PRT-PAGE-COUNT	PIC 9(04).

       01 PRT-HEADER2.
	  03 FIL 		PIC X(181) VALUE
	     'REPORT TITLE: STUDENT PROFILE SIMPLE LISTING'.
          03 FIL 		PIC X(07) VALUE 'TIME :'.
	  03 PRT-START-HHMM	PIC X(07).

       01 PRT-HEADER3.
	  03 FIL		PIC X(03) VALUE 'No.'.
	  03 FIL		PIC X(04).
	  03 FIL		PIC X(03) VALUE 'AC#'.
	  03 FIL		PIC X(04).
          03 FIL		PIC X(04) VALUE 'Name'.
	  03 FIL		PIC X(27).
	  03 FIL                PIC X(07) VALUE 'Gender'.
	  03 FIL 		PIC X(03) VALUE 'DOB'.
	  03 FIL 		PIC X(08).
	  03 FIL		PIC X(03) VALUE 'Age'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(06) VALUE 'Height'.
	  03 FIL		PIC X(04).
	  03 FIL 		PIC X(06) VALUE 'Weight'.
	  03 FIL		PIC X(04).
	  03 FIL                PIC X(07) VALUE 'Country'.
	  03 FIL                PIC X(14).
	  03 FIL                PIC X(04) VALUE 'Race'.
	  03 FIL                PIC X(17).
	  03 FIL                PIC X(08) VALUE 'Religion'.
	  03 FIL                PIC X(13).
	  03 FIL                PIC X(07) VALUE 'Mobile#'.
	  03 FIL                PIC X(24).
	  03 FIL                PIC X(06) VALUE 'Email#'.
	  03 FIL                PIC X(25).

       01 PRT-LINE.
	  03 FIL		PIC X(06) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL		PIC X(06) VALUE ALL '-'.
	  03 FIL		PIC X(01).
          03 FIL		PIC X(30) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL                PIC X(06) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL 		PIC X(10) VALUE ALL '-'.
	  03 FIL 		PIC X(01).
	  03 FIL		PIC X(03) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL		PIC X(09) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL 		PIC X(09) VALUE ALL '-'.
	  03 FIL		PIC X(01).
	  03 FIL                PIC X(20) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL                PIC X(20) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL                PIC X(20) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL                PIC X(30) VALUE ALL '-'.
	  03 FIL                PIC X(01).
	  03 FIL                PIC X(30) VALUE ALL '-'.

       01 PRT-NEXT-PAGE.
	  03 FIL 		PIC X(16) VALUE
	     '* CONTINUE PAGE'.
          03 PRT-PAGE-COUNT2 	PIC 9(04).
	  03 FIL		PIC X(02) VALUE '*'.

       01 PRT-DETAIL.
	  03 PRT-REC-COUNT	PIC Z(06).
	  03 FIL		PIC X(01).
          03 PRT-STD-KEY	PIC X(06).
	  03 FIL 		PIC X(01).
	  03 PRT-STD-NAME       PIC X(30).
	  03 FIL                PIC X(01).
	  03 PRT-STD-GENDER     PIC X(06).
	  03 FIL                PIC X(01).
	  03 PRT-STD-DOB-DMY    PIC 99/99/9999. 
          03 FIL                PIC X(01).
	  03 PRT-STD-AGE	PIC Z(03).
	  03 FIL		PIC X(01).
	  03 PRT-STD-HEIGHT	PIC 999.99.
	  03 FIL 		PIC X(01).
	  03 PRT-CM		PIC X(02).
	  03 FIL		PIC X(01).
	  03 PRT-STD-WEIGHT	PIC Z99.99.
	  03 FIL		PIC X(01).
	  03 PRT-KG		PIC X(02).
          03 FIL                PIC X(01).
	  03 PRT-STD-CY-NAME    PIC X(20).
	  03 FIL                PIC X(01).
	  03 PRT-STD-RC-NAME    PIC X(20).
	  03 FIL                PIC X(01).
	  03 PRT-STD-RG-NAME    PIC X(20).
	  03 FIL                PIC X(01).
	  03 PRT-STD-EMAIL      PIC X(30).
	  03 FIL                PIC X(01).
	  03 PRT-STD-MOBILE     PIC X(30).

       01 PRT-END.
	  03 FIL		PIC X(26) VALUE
	     '* END OF REPORT * TIME :'. 
	  03 PRT-END-HHMM	PIC X(07).

       LINKAGE SECTION.
       01 LINK-PROG-KEY		PIC X(30).
       01 LINK-DATA-ID		PIC X(08).
       01 LINK-SORT-MODE	PIC 9(01).

      ********************************************************************
       PROCEDURE DIVISION USING LINK-PROG-KEY, 
				LINK-DATA-ID, LINK-SORT-MODE.

	DECLARATIVES.

	   COPY '/z/y19b25/sp2/lib/fd/dcstd.t'.
	   COPY '/z/y19b25/sp2/lib/fd/dccy'.
	   COPY '/z/y19b25/sp2/lib/fd/dcrc'.
	   COPY '/z/y19b25/sp2/lib/fd/dcrg'.
	   COPY '/v/cps/lib/std/dcprint'.

        END DECLARATIVES.

      ********************************************************************
        BEGIN. 

	   MOVE 'N' TO S-RUN.
	   MOVE LINK-DATA-ID TO STD-T-DATA-ID.
	   OPEN INPUT STD-T-FILE, CY-FILE, RC-FILE, RG-FILE.
	  
	   COPY '/v/cps/lib/std/gtcoid.prd'.
           MOVE 'Print Student Profile Simple Listing' TO
                S-WINDOW-TITLE.
	   
	   MOVE 230 TO S-PRT-COL.

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

	   INITIALIZE STD-T-REC.
	   MOVE 'Y' TO S-RUN.

	   EVALUATE LINK-SORT-MODE
              WHEN 1	START STD-T-FILE KEY >= STD-T-KEY INVALID
			      MOVE 'N' TO S-RUN
			END-START
	      WHEN 2	START STD-T-FILE KEY >= STD-T-ALT-KEY1 INVALID
			      MOVE 'N' TO S-RUN
			END-START
	      WHEN 3	START STD-T-FILE KEY >= STD-T-ALT-KEY2 INVALID
			      MOVE 'N' TO S-RUN
			END-START
              WHEN 4	START STD-T-FILE KEY >= STD-T-ALT-KEY3 INVALID
			      MOVE 'N' TO S-RUN
			END-START
	   END-EVALUATE.
           
	   PERFORM 0200-PRT THRU 0299-PRT-END
		   UNTIL S-RUN = 'N' OR THREAD-RETURN = 99.

		   IF THREAD-RETURN NOT = 99
			PERFORM PRT-ENDING THRU PRT-ENDING-END.

           CLOSE PRINT-FILE.

	   COPY	'/v/cps/lib/std/e-thread.prd'.

        TERMINATION.
	   CLOSE STD-T-FILE, CY-FILE, RC-FILE, RG-FILE.
           EXIT PROGRAM.
	   STOP RUN.

      ********************************************************************
        0200-PRT.
          
	   READ STD-T-FILE NEXT END
		 MOVE 'N' TO S-RUN GO TO 0299-PRT-END.
 
	   INITIALIZE PRT-DETAIL.
	   ADD 1		TO S-REC-COUNT. 
	   MOVE S-REC-COUNT	TO PRT-REC-COUNT.
	   MOVE STD-T-KEY	TO PRT-STD-KEY.
      	   MOVE STD-T-NAME	TO PRT-STD-NAME.
	   EVALUATE STD-T-GENDER
	    WHEN '0M' 	MOVE 'Male  ' TO PRT-STD-GENDER
	    WHEN '0F'	MOVE 'Female' TO PRT-STD-GENDER.
	   MOVE STD-T-DOB-DMY	TO PRT-STD-DOB-DMY.
	   MOVE STD-T-AGE	TO PRT-STD-AGE.
	   MOVE STD-T-HEIGHT	TO PRT-STD-HEIGHT.
	   MOVE 'cm'		TO PRT-CM.
	   MOVE STD-T-WEIGHT	TO PRT-STD-WEIGHT.
	   MOVE 'kg'		TO PRT-KG.
	   MOVE CY-NAME		TO PRT-STD-CY-NAME.
	   MOVE RC-NAME 	TO PRT-STD-RC-NAME.
	   MOVE RG-NAME		TO PRT-STD-RG-NAME.
	   MOVE STD-T-EMAIL	TO PRT-STD-EMAIL.
	   MOVE STD-T-MOBILE	TO PRT-STD-MOBILE.
 
	   INITIALIZE CY-REC.
	   MOVE STD-T-CY-KEY 	TO CY-KEY.
	   READ CY-FILE INVALID
	      INITIALIZE CY-DETAILS.
	   MOVE CY-NAME TO PRT-STD-CY-NAME.
	   
	   INITIALIZE RC-REC.
	   MOVE STD-T-RC-KEY 	TO RC-KEY.
	   READ RC-FILE INVALID
	      INITIALIZE RC-DETAILS.
	   MOVE RC-NAME TO PRT-STD-RC-NAME.
	   
	   INITIALIZE RG-REC.
	   MOVE STD-T-RG-KEY 	TO RG-KEY.
	   READ RG-FILE INVALID
	      INITIALIZE RG-DETAILS.
	   MOVE RG-NAME TO PRT-STD-RG-NAME.
	   
	   PERFORM PRT-CONTROL THRU PRT-CONTROL-END.
	   WRITE PRINT-REC FROM PRT-DETAIL.

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
