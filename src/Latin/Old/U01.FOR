      REAL FUNCTION U01( DSEED )
C*....REAL FUNCTION U01( DSEED )
C**********************************************************************
C*
C*  Purpose:
C*
C*    This function generates a real random uniform(0,1) variate
C*    given an initial value of the seed, DSEED.  The input seed
C*    is updated to a new seed using a linear congruental method.
C*
C*    The current usage requires a system with at least a 32 bit
C*    word length for a REAL type variable.  Internal double
C*    precision computations are performed.
C*
C*
C*  Formal Parameters:
C*
C*    DSEED : Double precision seed for random number generator.
C*            On input, it must take a value between 1.0 and
C*            2147483646.0, inclusive.  On output, it has been
C*            updated to a new value in the same range.
C*
C*
C*  Note:
C*
C*    The user must provide an initial value for DSEED.  The value
C*    of DSEED should never be modified after a call to U01.
C*
C*
C*  Reference:
C*
C*    Lewis, Goodman, and Miller.  1969.
C*    "A Pseudo-random Number Generator for the System/360"
C*    IBM Systems Journal, Vol. 8, No. 2, pp. 136-145.
C*
C*
C*  History:
C*
C*    Dave W. Langford :        1985 : Original version
C*    Paul W. Eslinger : 27 Jun 1989 : Change comments
C*
C**********************************************************************
C
      DOUBLE PRECISION B, M, DSEED
C
      DATA B /      16807.0D+00 /
      DATA M / 2147483647.0D+00 /
C
      DSEED = DINT( DMOD(B*DSEED,M) )
      U01   = SNGL( DSEED/M )
C
      RETURN
      END
