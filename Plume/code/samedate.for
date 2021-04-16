      LOGICAL FUNCTION SAMEDATE( yr0, mo0, da0, hr0, min0, 
     &                           yr1, mo1, da1, hr1, min1 )
C-----------------------------------------------------------------------
C
C     SAMEDATE
C
C     Date:    2/17/98
C
C     Description:   Determines if two dates/times are the same - 
C                    Check the year for a two digit year vs four
C                    digit year
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      
      INTEGER*4   yr0, yr1, mo0, mo1, da0, da1, hr0, hr1, min0, min1
      INTEGER*4   chkyr0, chkyr1
      
      SAMEDATE = .TRUE.

c     Set up two digit years to check aganist

      chkyr0 = yr0 - (INT(yr0/100) * 100)
      chkyr1 = yr1 - (INT(yr1/100) * 100)
      
      IF( (yr0 .NE. yr1) .AND. (chkyr0 .NE. yr1) 
     &                   .AND. (chkyr1 .NE. yr0) ) THEN
         SAMEDATE = .FALSE.
      ELSEIF ( mo0 .NE. mo1 ) THEN
         SAMEDATE = .FALSE.
      ELSEIF ( da0 .NE. da1 ) THEN
         SAMEDATE = .FALSE.
      ELSEIF ( hr0 .NE. hr1 ) THEN
         SAMEDATE = .FALSE.
      ELSEIF ( min0 .NE. min1 ) THEN
         SAMEDATE = .FALSE.
      ENDIF
      
      RETURN
      
      END