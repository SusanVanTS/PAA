        FD STD-T-FILE
	   LABEL RECORD STANDARD.

      * FDSTD - Student File.
      * STD-KEY  : Use X(06).
      * STD-NAME : Use X(40).
      * STD-ADD1 : Use X(40).
      * STD-ADD2 : Use X(40).
      * STD-ADD3 : Use X(40).
      * M-Male, F-Female.

        01 STD-T-REC.
	   03 STD-T-KEY		PIC X(08).
	   03 STD-T-DETAILS.
	      05 STD-T-NAME	PIC X(50).
	      05 STD-T-ADD.
 		 07 STD-T-ADD1	PIC X(50).
		 07 STD-T-ADD2  PIC X(50).
		 07 STD-T-ADD3  PIC X(50).
	      05 STD-T-GENDER   PIC 9(02).
              05 STD-T-GENDER2 REDEFINES STD-T-GENDER
				PIC 9(01) OCCURS 2.
	      05 STD-T-DOB-DMY  PIC 9(08).
	      05 STD-T-AGE	PIC 9(02).
	      05 STD-T-HEIGHT	PIC 999V99.
	      05 STD-T-WEIGHT	PIC 999V99.
	      05 STD-T-CY-KEY   PIC X(04).
	      05 STD-T-RC-KEY   PIC X(04).
	      05 STD-T-RG-KEY	PIC X(04).
	      05 STD-T-EMAIL    PIC X(30).
	      05 STD-T-MOBILE   PIC X(30).
