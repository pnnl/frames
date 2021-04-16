      FUNCTION GAMAIN( X, P, LGAMMA )
C*....FUNCTION GAMAIN( X, P, LGAMMA )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This function computes the incomplete Gamma ratio for positive
C*    values of the arguments X and P.  In other words, this function
C*    evaluates the cumulative distribution function for the Gamma
C*    distribution with a single parameter (i.e. X**(P-1)) in the
C*    density).
C*
C*
C*  Method:
C*
C*     This function uses a series expansion if P.GT.X or X.LE.1,
C*     otherwise a continued fraction approximation is used.
C*
C*
C*  Formal Parameters:
C*
C*    Variable  Definition
C*    --------  --------------------------------------------------------
C*    X         Input - Real: The value X at which to evaluate the
C*              standard Gamma distribution.
C*
C*    P         Input - Real: The power parameter (i.e. X**(P-1)) for
C*              the Gamma density.
C*
C*    LGAMMA    Input - Real: The logarithm of the complete gamma
C*              function with argument P.
C*
C*
C*  Reference:
C*
C*    Algorithm AS 32
C*    Journal of the Royal Statistical Society, Series C
C*    (Applied Statistics). 1970,  Vol. 19,  No. 3
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      REAL LGAMMA
      DIMENSION PN(6)
C
C *** Define accuracy and initialize
C
      ACU  = 1.0E-8
      OFLO = 1.0E+30
      GIN  = 0.0
C
C *** Test for admissibility of arguments
C
      IF( P .LE. 0.0 ) STOP 21
      IF( X .LT. 0.0 ) STOP 22
C
      IF( X .EQ. 0.0 ) GO TO 50
C
C *** Change to pass in the log of the complete Gamma function
C
      G = LGAMMA
      FACTOR = EXP(P * ALOG(X) - X - G)
      IF( X.GT.1.0 .AND. X.GE.P ) GO TO 30
C
C *** Calculation by series expansion
C
      GIN = 1.0
      TERM = 1.0
      RN = P
   20 RN = RN + 1.0
      TERM = TERM * X / RN
      GIN = GIN + TERM
      IF (TERM .GT. ACU) GOTO 20
      GIN = GIN * FACTOR / P
      GOTO 50
C
C *** Calculation by continued fraction
C
   30 A = 1.0 - P
      B = A + X + 1.0
      TERM = 0.0
      PN(1) = 1.0
      PN(2) = X
      PN(3) = X + 1.0
      PN(4) = X * B
      GIN = PN(3) / PN(4)
   32 A = A + 1.0
      B = B + 2.0
      TERM = TERM + 1.0
      AN = A * TERM
      DO 33 I = 1, 2
   33 PN(I+4) = B * PN(I+2) - AN * PN(I)
      IF (PN(6) .EQ. 0.0) GOTO 35
      RN = PN(5) / PN(6)
      DIF = ABS(GIN - RN)
      IF (DIF .GT. ACU) GOTO 34
      IF (DIF .LE. ACU * RN) GOTO 42
   34 GIN = RN
   35 DO 36 I = 1, 4
   36 PN(I) = PN(I+2)
      IF (ABS(PN(5)).LT.OFLO) GOTO 32
      DO 41 I = 1, 4
   41 PN(I) = PN(I) / OFLO
      GOTO 32
   42 GIN = 1.0 - FACTOR * GIN
C
   50 GAMAIN = GIN
C
      RETURN
      END
