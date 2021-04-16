      REAL FUNCTION PPND( P, IFAULT )
C*....REAL FUNCTION PPND( P, IFAULT )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This function evaluates the normal deviate corresponding to the
C*    lower tail area of P.  In other words, it computes the inverse
C*    of the standard normal cumulative distribution function.
C*
C*
C*  Reference:
C*
C*    Algorithm AS 111
C*    Applied Statistics (1977)
C*    Journal of the Royal Statistical Society, Series C
C*    Vol. 26, No. 1
C*
C*
C*  Note:
C*
C*    The hash sums are the sums of the moduli of the coefficients
C*    they have no inherent meanings, but can be used for checking
C*    transcriptions.
C*
C*    Internal double precision has been added by P.W. Eslinger
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      DOUBLE PRECISION ZERO, SPLIT, HALF, ONE
      DOUBLE PRECISION A0, A1, A2, A3, B1, B2, B3, B4, C0, C1, C2, C3
      DOUBLE PRECISION D1, D2, DP, Q, R
C
      DATA ZERO /0.0D0/, HALF /0.5D0/, ONE /1.0D0/
      DATA SPLIT /0.42D0/
C
      DATA A0 /   2.50662823884D0/
      DATA A1 / -18.61500062529D0/
      DATA A2 /  41.39119773534D0/
      DATA A3 / -25.44106049637D0/
C
      DATA B1 /  -8.47351093090D0/
      DATA B2 /  23.08336743743D0/
      DATA B3 / -21.06224101826D0/
      DATA B4 /   3.13082909833D0/
C
C *** HASH SUM AB 143.70383558076
C
      DATA C0 /  -2.78718931138D0/
      DATA C1 /  -2.29796479134D0/
      DATA C2 /   4.85014127135D0/
      DATA C3 /   2.32121276858D0/
C
      DATA D1 /   3.54388924762D0/
      DATA D2 /   1.63706781897D0/
C
C *** HASH SUM CD  17.43746520924
C
      DP = DBLE( P )
C
      IFAULT = 0
C
      Q = DP - HALF
      IF( DABS(Q) .GT. SPLIT ) GO TO 1
C
      R = Q * Q
      PPND = SNGL(Q * (((A3 * R + A2) * R + A1) * R + A0) /
     *    ((((B4 * R + B3) * R + B2) * R + B1) * R + ONE))
      RETURN
C
    1 R = DP
      IF( Q .GT. ZERO ) R = ONE - DP
      IF( R .LE. ZERO ) GO TO 2
      R = DSQRT( -DLOG(R) )
      PPND = SNGL((((C3 * R + C2) * R + C1) * R + C0) /
     *    ((D2 * R + D1) * R + ONE))
      IF( Q .LT. ZERO ) PPND = -PPND
      RETURN
C
    2 IFAULT = 1
      PPND = SNGL( ZERO )
      RETURN
C
      END
