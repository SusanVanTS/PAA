        FD SSR-T-FILE
	   LABEL RECORD STANDARD.

        01 SSR-T-REC.
	   03 SSR-T-KEY.		
	      05 SSR-T-KEY1		PIC X(01).
	      05 SSR-T-KEY2		PIC X(04).
	   03 SSR-T-DETAILS.
	      05 SSR-T-GENDER OCCURS 2 TIMES	PIC 9(06).
	      05 SSR-T-AGE OCCURS 5 TIMES	PIC 9(06).
	      05 SSR-T-HEIGHT OCCURS 4 TIMES	PIC 9(06).
	      05 SSR-T-WEIGHT OCCURS 3 TIMES	PIC 9(06).
	      05 SSR-T-ERRORS			PIC 9(06).
