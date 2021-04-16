      SUBROUTINE U01S1( N, RVAL, IWORK, DSEED )
C*....SUBROUTINE U01S1( N, RVAL, IWORK, DSEED )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This routine computes a vector of randomly generated numbers on
C*    the interval (0,1) with stratification using equal probable
C*    intervals and exactly one value per strata.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C*
C*  Formal Parameters:
C*
C*    DSEED  : Input - Double precision
C*             Seed for the random number generator. It must lie
C*             between 1.0 and 2147483646.0 inclusive.
C*    N      : Input - Integer
C*             Number of random values to generate, also, the number
C*             of strata to use
C*    RVAL   : Output - Real
C*             Vector of length N of uniform (0,1) values
C*    IWORK  : Input - Integer
C*             Work array of length N
C*
C***********************************************************************
C
      DOUBLE PRECISION DSEED
      DIMENSION RVAL(N), IWORK(N)
C
C *** The stratified samples will be generated sequentially across
C     the interval [0,1] - they must then be randomly permuted.
C     Generate a vector of values from the uniform distribution
C     and use their ranks to permute the generated values.
C
      DO 10 I = 1, N
        RVAL(I) = U01( DSEED )
        IWORK(I)  = I
   10 CONTINUE
C
C *** After the sort, the work vector contains the ranks for doing
C     the permutations
C
      KFLAG = 2
      CALL SSORT( RVAL, IWORK, N, KFLAG )
C
C *** Generate the stratified values sequentially and store them in
C     the permuted locations
C
      RN = N
      DO 20 I = 1, N
        RIM1 = I - 1
        RVAL(IWORK(I)) = (RIM1 + U01(DSEED)) / RN
   20 CONTINUE
C
      RETURN
      END
