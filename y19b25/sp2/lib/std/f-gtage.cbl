       IDENTIFICATION DIVISION.
        PROGRAM-ID.   F-GTAGE.

      * Subroutine to get # of Days between Dates (exclude starting date).
      * Author   Date   Type A/C    Notes
      * Ong    09/08/11 WO   MPIKJ  W#540 S#EI2011-001: Coding

       ENVIRONMENT DIVISION.
       DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-DAY-TABLE.
           03 WS-NO                   PIC X(36) VALUE 
              '000031059090120151181212243273304334'.
           03 WS-DAY REDEFINES WS-NO  PIC 9(03) OCCURS 12 TIMES.

        01 WS-MISC.
           03 WS-YYYYMMDD1.
	      05 WS-YYYY1	      PIC 9(04).
	      05 WS-MM1		      PIC 9(02).
	      05 WS-DD1		      PIC 9(02).
           03 WS-YYYYMMDD2.
	      05 WS-YYYY2	      PIC 9(04).
	      05 WS-MM2		      PIC 9(02).
	      05 WS-DD2		      PIC 9(02).
           03 WS-SUB                  PIC 9(02).
           03 WS-MTH                  PIC 9(02).
           03 WS-COMP-MTH             PIC S9(07).
           03 WS-LEAP-YEAR-COUNT      PIC 9(04).
           03 WS-REMAIN               PIC 9(01).
              88 LEAP-YEAR            VALUE 0.
           03 OCCURS 2 TIMES.
              05 WS-YYYYMMDD.
                 07 WS-YYYY           PIC 9(04).
                 07 WS-MM             PIC 9(02).
                 07 WS-DD             PIC 9(02).
              05 WS-DAY-COUNT         PIC S9(07).

        LINKAGE SECTION.
        01 LINK-DATE1                 PIC 9(08).
        01 LINK-AGE	              PIC 9(02).
 
      ******************************************************************
       PROCEDURE DIVISION USING LINK-DATE1,LINK-AGE. 

      * LINK-DATE-FORMAT : 1-ddmmyyyy  2-yyyymmdd
      * LINK-AGE-TYPE    : 1-actual days  2-month-to-month

        BEGIN.

           INITIALIZE LINK-AGE.
	   ACCEPT WS-YYYYMMDD2 FROM CENTURY-DATE.
           MOVE LINK-DATE1 TO WS-YYYYMMDD1.
                CALL   '/v/cps/lib/std/f-cvdmy' USING WS-YYYYMMDD1
                CANCEL '/v/cps/lib/std/f-cvdmy'

      *    IF WS-YYYYMMDD1 > WS-YYYYMMDD2
      *       COMPUTE LINK-DAY-COUNT = LINK-DAY-COUNT - 1
      *       GO TO TERMINATION.

           MOVE WS-YYYYMMDD1 TO WS-YYYYMMDD(01).
           MOVE WS-YYYYMMDD2 TO WS-YYYYMMDD(02).
	      
           IF (WS-YYYY1 > (WS-YYYY2 - 100)) AND
	      (WS-YYYY1 < WS-YYYY2)
              PERFORM GET-TOTAL-DAY THRU GET-TOTAL-DAY-END
		   VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 2
              COMPUTE LINK-AGE = 
		   (WS-DAY-COUNT(2) - WS-DAY-COUNT(1))/365.

      *     PERFORM GET-TOTAL-DAY THRU GET-TOTAL-DAY-END
      *          VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 2
      *     COMPUTE LINK-AGE = 
      *             (WS-DAY-COUNT(2) - WS-DAY-COUNT(1))/365.

        TERMINATION.
           EXIT PROGRAM.
           STOP RUN.

      ******************************************************************
        GET-TOTAL-DAY.

           DIVIDE WS-YYYY(WS-SUB) BY 4 GIVING WS-LEAP-YEAR-COUNT 
                                       REMAINDER WS-REMAIN.   
            
           IF LEAP-YEAR AND WS-MM(WS-SUB) < 3
              SUBTRACT 1 FROM WS-LEAP-YEAR-COUNT.

           MOVE WS-MM(WS-SUB) TO WS-MTH.
           COMPUTE WS-DAY-COUNT(WS-SUB) = WS-YYYY(WS-SUB) * 365 + 
                   WS-LEAP-YEAR-COUNT + WS-DAY(WS-MTH) + WS-DD(WS-SUB).

        GET-TOTAL-DAY-END. EXIT.

      * End of Program.
