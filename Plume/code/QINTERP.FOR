
      REAL FUNCTION QINTERP( n, u, x, y )
      
C---------------------------------------------------------------------------
c     QINTERP                                        
c     
c     Date:              August 17, 1995
c
c     Description:   This function performs the dose vs distance 
c                    interpolation for R2QUAD volume elements.
c           
c     Required modules:  None
c
C---------------------------------------------------------------------------

      IMPLICIT NONE
      
      REAL      x(20), y(20)
      
      REAL      u, u1, value
 
      INTEGER   n  
      
      INTEGER   i, maxn

      u1 = ALOG ( u )
      maxn = n

C  Try for quick exit 

      IF ( u1 .GT. x(maxn) ) THEN
         QINTERP = 0.0
         RETURN
      ELSE IF( u1 .LT. x(3) ) THEN
         QINTERP = EXP( y(2) )
         RETURN
      ENDIF

C  Search for the proper distance bracket

      i = maxn - 1
      DO WHILE( (u1 .GT. x(i+1)) .OR. (u1 .LT. x(i) ) ) 
         i = i - 1
      ENDDO

C  Interpolate for dose between Y(I) and Y(I+1)

      IF ( u1 .EQ. x(i) ) THEN
         QINTERP = EXP ( y(i) )
      ELSE
         value = y(i) + (y(i+1) - y(i))*((u1 - x(i))/(x(i+1)-x(i)))
         QINTERP = EXP ( value )
      ENDIF

      RETURN
      
      END