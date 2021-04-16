
      REAL FUNCTION QINTERP( maxn, u, x, y )
      
C---------------------------------------------------------------------------
c     QINTERP                                        
c     
c     Date:              November 16, 1998
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
 
c      INTEGER   n(2)  
      
      INTEGER   i, maxn

      u1 = ALOG ( u )

C  Try for quick exit 

      IF ( u1 .GT. x(maxn) ) THEN
         QINTERP = 0.0
         RETURN
c      ELSE IF( u1 .LT. x(3) ) THEN
c         QINTERP = EXP( y(2) )
c         RETURN
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