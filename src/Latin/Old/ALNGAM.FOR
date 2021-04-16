      REAL FUNCTION ALNGAM( XVALUE, IFAULT )
C*....REAL FUNCTION ALNGAM( XVALUE, IFAULT )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This program calculates the logarithm of the Gamma function for
C*    the argument XVALUE.
C*
C*
C*  Formal Parameters:
C*
C*    Variable  Definition
C*    --------  --------------------------------------------------------
C*    XVALUE    Input - Real: The value at which to evaluate the
C*              logarithm of the Gamma function.  XVALUE must be greater
C*              than zero, yet smaller than XLGST (defined below).
C*
C*    IFAULT    Output - Integer: Error indicator.
C*              IFAULT = 0 : Normal termination
C*              IFAULT = 1 : XVALUE was greater than XLGST
C*              IFAULT = 2 : XVALUE was less than or equal to zero.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C*
C*  Reference:
C*
C*     Macleod, Allan.  " A Robust and Reliable Algorithm for the
C*     Logarithm of the Gamma Function".
C*     Applied Statistics
C*     (Journal of the Royal Statistical Society, Series C.)
C*     Volume 38, No. 2, pp. 397-423.
C*
C***********************************************************************
C
      INTEGER IFAULT
      DOUBLE PRECISION ALR2PI, FOUR, HALF, ONE, ONEP5, R1(9), R2(9),
     &       R3(9), R4(5), TWELVE, X, X1, X2, XLGE, XLGST, Y, ZERO
C
C *** Coefficients of rational functions
C
      DATA R1(1) /-2.66685511495D0/,  R1(2) /-2.44387534237D1/,
     *     R1(3) /-2.19698958928D1/,  R1(4) / 1.11667541262D1/,
     *     R1(5) / 3.13060547623D0/,
     *     R1(6) / 6.07771387771D-1/, R1(7) /1.19400905721D1/,
     *     R1(8) / 3.14690115749D1/,  R1(9) / 1.52346874070D1/
C
      DATA R2(1) /-7.83359299449D1/, R2(2) /-1.42046296688D2/,
     *     R2(3) / 1.37519416416E2/, R2(4) / 7.86994924154D1/,
     *     R2(5) / 4.16438922228D0/,
     *     R2(6) / 4.70668766060D1/, R2(7) / 3.13399215894D2/,
     *     R2(8) / 2.63505074721D2/, R2(9) /4.33400022514D1/
C
      DATA R3(1) /-2.12159572323D5/, R3(2) / 2.30661510616D5/,
     *     R3(3) / 2.74647644705D4/, R3(4) /-4.02621119975D4/,
     *     R3(5) /-2.29660729780D3/,
     *     R3(6) /-1.16328495004D5/, R3(7) /-1.46025937511D5/,
     *     R3(8) /-2.42357409629D4/, R3(9) /-5.70691009324D2/
C
      DATA R4(1) /2.79195317918525D-1/, R4(2) /4.917317610505968D-1/,
     *     R4(3) / 6.92910599291889D-2/,
     *     R4(4) / 3.350343815022304D0/, R4(5) /6.012459259764103D0/
C
C *** Fixed constants
C
      DATA ALR2PI, FOUR, HALF, ONE, ONEP5, TWELVE, ZERO
     * /9.18938533204673D-1, 4.0D0, 5.0D-1, 1.0D0, 1.5D0, 1.2D1, 0.0D0/
C
C *** Machine dependent constants (32 bit IBM computer)
C       See the reference for computation for other hardware
C       configurations
C
      DATA XLGE, XLGST /3.7671D2, 1.32D36/
C
C     -------------------- First Executable Line --------------------
C
      X = XVALUE
      ALNGAM = ZERO
C
C *** Test for valid function argument
C
      IFAULT = 2
      IF(X.GE.XLGST) RETURN
      IFAULT = 1
      IF(X.LE. ZERO) RETURN
      IFAULT = 0
C
C *** Calculation for 0 < X < 0.5 and 0.5 <=X < 1.5 combined
C
      IF (X.LT.ONEP5) THEN
        IF (X.LT.HALF) THEN
          ALNGAM = -LOG(X)
          Y = X + ONE
C
C *** Test whether X < machine epsilon
C
          IF (Y. EQ.ONE) RETURN
        ELSE
          ALNGAM = ZERO
          Y = X
          X = (X - HALF) - HALF
        ENDIF
        ALNGAM = ALNGAM + X * ((((R1(5) * Y + R1(4)) * Y + R1(3))
     &   * Y + R1(2)) * Y + R1(1)) / ((((Y + R1(9)) * Y + R1(8))
     &   * Y + R1(7)) * Y + R1(6))
        RETURN
      ENDIF
C
C *** Calculation for 1.5 <= X < 4.0
C
      IF (X .LT. FOUR) THEN
        Y = (X- ONE) - ONE
        ALNGAM = Y * ((((R2(5) * X + R2(4)) * X + R2(3)) * X + R2(2))
     &    * X + R2(1)) / ((((X+R2(9)) * X + R2(8)) * X + R2(7))
     &    * X + R2(6))
        RETURN
      ENDIF
C
C *** Calculation for 4.0 <= X < 12.0
C
      IF(X .LT. TWELVE) THEN
        ALNGAM = ((((R3(5) * X + R3(4)) * X + R3(3)) * X + R3(2))
     &    * X + R3(1)) / ((((X + R3(9)) * X + R3(8)) * X + R3(7))
     &    * X + R3(6))
        RETURN
      ENDIF
C
C *** Calculation for X > 12.0
C
      Y = LOG(X)
      ALNGAM = X * (Y - ONE) - HALF * Y + ALR2PI
      IF ( X. GT. XLGE) RETURN
      X1 = ONE / X
      X2 = X1 * X1
      ALNGAM = ALNGAM + X1 * ((R4(3) * X2 + R4(2)) * X2 + R4(1))
     &  / ((X2 + R4(5)) * X2 + R4(4))
      RETURN
      END
