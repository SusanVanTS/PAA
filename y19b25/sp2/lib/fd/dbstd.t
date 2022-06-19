        01 STD-T-DATA.
	   03 STD-T-FILENAME	PIC X(40) VALUE
	      'Student Profile Temporary Data File'.
	   03 STD-T-DATANAME.	
	      05 FIL		PIC X(19) VALUE
	         '/z/y19b25/sp2/temp/'.
	      05 STD-T-DATA-ID	PIC X(08).
	      05 FIL		PIC X(13) VALUE
		 '.std.t'.
	   03 STD-T-STATUS		PIC X(02).
