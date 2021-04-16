      REAL FUNCTION SHINEINT ( nn, dist, x, y )
      
C---------------------------------------------------------------------------
c
c     SHINEINT
c
c     Date:          March 19, 1998
c
c     Description:  This subroutine evaluates the doses as a function of 
c                   distance in TADMOD using logarithmic interpolation.
c           
c     Required modules:  None
c
C---------------------------------------------------------------------------

      IMPLICIT      NONE
      
      REAL          x(20), y(20) 

      REAL          dist, yint

      INTEGER       i, nn

      IF ( dist .GE. x(nn) ) THEN
          SHINEINT = 0.0 
          RETURN
      ELSE IF ( dist .LT. x(2) ) THEN
          SHINEINT = y(1)
          RETURN
      ENDIF

C  Find proper distance bracket

      i = nn - 1
      DO WHILE ( dist .LT. x(i) )
          i = i - 1
      ENDDO     

C  Logarithmic interpolation between computed values if needed

      IF( dist .EQ. x(i) ) THEN
          SHINEINT = y(i)
C error trap added by BAN 22 Oct 2001
	ELSE IF (x(i+1).eq. 0.0 .or. y(i+1) .eq. 0.0) THEN
		SHINEINT = y(i)
      ELSE
          yint = ALOG( y(i) ) + ALOG( y(i+1)/y(i) ) 
     +          * ALOG( dist/x(i) ) / ALOG( x(i+1)/x(i) )
          SHINEINT = EXP( yint ) 
      ENDIF

      RETURN                    
      
      END
