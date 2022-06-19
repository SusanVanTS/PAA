        01 SSR-T-DATA.
	   03 SSR-T-FILENAME	PIC X(40) VALUE
	      'Student Statistic Temporary Data File'.
	   03 SSR-T-DATANAME.
	      05 FIL		PIC X(19) VALUE 
				'/z/y19b25/sp2/temp/'.
	      05 SSR-T-DATA-ID	PIC X(08).
	      05 FIL 		PIC X(13) VALUE 'ssr.t'.
	   03 SSR-T-STATUS		PIC X(02).
