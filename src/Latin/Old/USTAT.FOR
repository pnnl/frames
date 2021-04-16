      SUBROUTINE USTAT( X, N, WORK, XMIN, XMAX, XAVG, XMED, XSTD,
     *                  XCOV, XSKW, IERS )
C*....SUBROUTINE USTAT( X, N, WORK, XMIN, XMAX, XAVG, XMED, XSTD,
C*   *                  XCOV, XSKW, IERS )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine will compute some summary statistics for the N
C*    values in the vector X.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C*
C*  Auxiliary Routines Required:
C*
C*    SORT : A subroutine to do a sort on a REAL vector
C*
C*
C*  Variable Descriptios:
C*
C*    Inputs
C*    ----------------------------------------------------
C*    X    : Vector of input values.
C*    N    : Number of values in the vector X.
C*    WORK : A work vector with the same dimension as X.
C*
C*    Outputs
C*    ----------------------------------------------------
C*    XMIN : Minimum value in X.
C*    XMAX : Maximum value in X.
C*    XAVG : Average (mean) value in X.
C*    XMED : Median of the values in X.
C*    XSTD : Standard deviation of the values in X.
C*    XCOV : Coefficient of variation of the values in X.
C*    XSKW : Skewness of the values in X.
C*    IERS : Error flag (Errors are additive).
C*               0 : Normal termination
C*               1 : N < 1, no data values and no calculations
C*              10 : N = 1, no calculations for XSTD, XCOV or XSKW
C*             100 : XSTD <= 0.0, no calculations for XCOV or XSKW
C*            1000 : XAVG = 0, no calculation for XCOV
C*           10000 : N = 2, no calculation for XSKW
C*
C***********************************************************************
C
      INTEGER N
      REAL X, XMIN, XMAX, XAVG, XMED, XSTD, XCOV, XSKW, WORK, SMALL
      DIMENSION X(N), WORK(N)
C
      DOUBLE PRECISION SUM, SUM2
C
C *** The value SMALL is to prevent an underflow in the coefficient
C     of variation calculation
C
      DATA SMALL / 1.0E-30 /
C
C----------------------------------------------------------------------C
C              Initialize and check for error on entry
C----------------------------------------------------------------------C
C
      IERS = 0
      XMIN = 0.0
      XMAX = 0.0
      XAVG = 0.0
      XSTD = 0.0
      XCOV = 0.0
      XSKW = 0.0
      IF( N .LT. 1 ) THEN
        IERS = 1
        RETURN
      ENDIF
C
C----------------------------------------------------------------------C
C            Sort the data to find the median efficiently
C               Use the WORK array for all calculations
C----------------------------------------------------------------------C
C
      DO 10 I = 1, N
        WORK(I) = X(I)
   10 CONTINUE
C
      KFLAG = 1
      CALL SORT( WORK, N, KFLAG )
C
C----------------------------------------------------------------------C
C               Find the minimum, maximum, and median
C----------------------------------------------------------------------C
C
      XMIN = WORK(1)
      XMAX = WORK(N)
      IF( N .EQ. 1 ) THEN
        XMED = WORK(N)
      ELSE
        IF( MOD(N,2) .EQ. 1 ) THEN
          XMED = WORK(N/2+1)
        ELSE
          XMED = (WORK(N/2)+WORK(N/2+1)) / 2.0
        ENDIF
      ENDIF
C
C----------------------------------------------------------------------C
C                          Find the mean
C----------------------------------------------------------------------C
C
      SUM = 0.0
      DO 20 I = 1, N
        SUM = SUM + WORK(I)
   20 CONTINUE
      XAVG =  SUM / FLOAT(N)
C
C----------------------------------------------------------------------C
C                   Find the standard deviation
C              Also sum for the skewness calculation
C----------------------------------------------------------------------C
C
      SUM  = 0.0
      SUM2 = 0.0
      DO 30 I = 1, N
        SUM  = SUM  + (WORK(I)-XAVG)**2
        SUM2 = SUM2 + (WORK(I)-XAVG)**3
   30 CONTINUE
C
      IF( N .EQ. 1 ) THEN
        XSTD = 0.0
        IERS = IERS + 10
        RETURN
      ELSE
        XSTD = SQRT( SUM/FLOAT(N) )
      ENDIF
C
      IF( XSTD .LE. 0.0 ) THEN
        IERS = IERS + 100
        RETURN
      ENDIF
C
C----------------------------------------------------------------------C
C                 Find the coefficient of variation
C----------------------------------------------------------------------C
C
      IF( ABS(XAVG) .GT. SMALL ) THEN
        XCOV = XSTD / ABS(XAVG)
      ELSE
        XCOV = 0.0
        IERS = IERS + 1000
      ENDIF
C
C----------------------------------------------------------------------C
C                         Find the skewness
C----------------------------------------------------------------------C
C
C
      IF( N .EQ. 2 ) THEN
        IERS = IERS + 10000
        RETURN
      ENDIF
C
      XSKW = (SUM2/FLOAT(N)) / XSTD**3
C
C----------------------------------------------------------------------C
C                           Exit Normally
C----------------------------------------------------------------------C
C
      RETURN
      END
