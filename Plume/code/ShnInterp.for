
      FUNCTION ShnINTERP( n, u, x, y )
      
C---------------------------------------------------------------------------
c     QINTERP                                        
c     
c     Date:              November 9, 1998
c
c     Description:   This function performs the dose vs distance 
c                    interpolation for SAPlShn1 and CLPlShn1 line elements.
c           
c     Required modules:  None
c
C---------------------------------------------------------------------------
C     Revised:
C        6 March 2003   BAN   Lowered interpolation index by 1
C       22 Sept  2004   BAN   Changed EXP to DEXP (underflow problem)
C---------------------------------------------------------------------------

      IMPLICIT NONE

	REAL      ShnInterp
      
      REAL      x(20), y(20)
	real(8) shn1, value      
      REAL      u, u1
 
      INTEGER   n  
      
      INTEGER   i, maxn


      u1 = ALOG ( u )
      maxn = n

C  Try for quick exit 

      IF ( u1 .GT. x(maxn) ) THEN
         ShnINTERP = 0.0
         RETURN
      ELSE IF( u1 .LE. x(1) ) THEN
         ShnINTERP = EXP( y(1) )
         RETURN
      ENDIF

C  Search for the proper distance bracket

      i = 1
      DO WHILE( u1 .GT. x(i) ) 
         i = i + 1
         IF ( i .GT. maxn ) THEN
            ShnInterp = 0.0
            RETURN
         ENDIF    
      ENDDO
c
c     Modification by BAN on 6 March 2003
c     
      I = I-1
c
c
C  Interpolate for dose between Y(I) and Y(I+1)

      IF ( u1 .EQ. x(i) ) THEN
         ShnINTERP = EXP ( y(i) )
      ELSE
         value = y(i) + (y(i+1) - y(i))*((u1 - x(i))/(x(i+1)-x(i)))
         Shn1 = DEXP ( value )
	ShnINTERP = shn1
      ENDIF

      RETURN
      
      END