      REAL FUNCTION U01( dseed )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     U01
C     Uniform (0,1) random number generator
C
C     Based on function of the same name developed by D. Langford, 
C     modified by P. Eslinger.  Obtained from J.C. Simpson 4/20/92.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      
      REAL*8   b, m, dseed
      
      DATA b  /      16807.0D+00  /
      DATA m  / 2147483647.0D+00  /
      
      dseed = DINT( DMOD( b * dseed, m ) )
      U01 = SNGL ( dseed / m )
      
      RETURN
      END     