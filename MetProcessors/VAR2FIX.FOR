
      SUBROUTINE VAR2FIX(INPPT)
C*--------------------------------------------------------------------
C*    PURPOSE:    THIS ROUTINE WILL READ IN A VARIABLE LENGTH TD3240
C*                FORMAT HOURLY PRECIPITATION FILE AND WRITE OUT A FIXED
C*                LENGTH PRECIPITATION SCRATCH FILE.  
C*
C*    WRITTEN BY: JAYANT A. HARDIKAR
C*                PES, INC
C*
C*    DATE:       2/23/95
C*
C*    INPUT:      Variable-format TD-3240 file
C*
C*    OUTPUT:     Fixed-format temporary file
C*
C*    CALLED BY:  FILES
C----------------------------------------------------------------------

      CHARACTER*27 TEXT1
      CHARACTER*12 TEXT2(100)
      INTEGER NUMTXT
      
      DO 500 I = 1,999999
      
         READ (INPPT,6666,END=999) TEXT1,NUMTXT,(TEXT2(J),J=1,NUMTXT)
6666     FORMAT(A27,I3,100(A12))

         DO 400 II = 1,NUMTXT
            WRITE (INPPT+50,7777) TEXT1,TEXT2(II)
7777        FORMAT(A27,3X,A12)
400      CONTINUE

C*    Note: the unit number is changed when returned to the calling routine

500   CONTINUE

999   RETURN
      END
