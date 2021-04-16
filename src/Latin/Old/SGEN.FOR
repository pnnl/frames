      SUBROUTINE SGEN( IDIST, RVAL, NVAL, UMIN, UMAX, PAR1, PAR2, PAR3,
     *           PAR4, NUSER, UUSER, XUSER )
C*....SUBROUTINE SGEN( IDIST, RVAL, NVAL, UMIN, UMAX, PAR1, PAR2, PAR3,
C*   *           PAR4, NUSER, UUSER, XUSER )
C**********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine will generate NVAL values from a specified
C*    probability distribution and place them in the vector RVAL.
C*    On input the vector RVAL contains values from the Uniform
C*    distribution.  On output it contains values from the desired
C*    statistical distribution.
C*
C*
C*  Notes:
C*
C*    1. Each distribution is generated using the Probability
C*       Integral Transformation method.  I.e., the random value
C*       X is found implicitly from the equation F(X)=U where
C*       U is a generated value from the uniform distribution,
C*       and F(X) is the cumulative distribution function for the
C*       random variable X.
C*
C*    2. Stratified sampling may be used.  The Uniform(0,1) values
C*       in RVAL upon input can be a stratified set of values.
C*
C*    3. Each distribution may be truncated between two specified
C*       limits.  The Uniform(0,1) values input in RVAL will be
C*       modified to implement the range truncation.
C*
C*    4. The user may specify a cumulative distribution function
C*       in the form of a table of values.  Linear interpolation
C*       will be used to generate values from the tabled distribution.
C*
C*    5. Adding or deleting a distribution requires changes in four
C*       places.  This subroutine, subroutine SINV, the vector DLABEL
C*       of distribution labels, and the parameter MXDIST.  The
C*       convention is that a user specified distribution is always
C*       the highest number for the variable IDIST.
C*
C*
C*  Formal Parameters:
C*
C*    Name    Description
C*    ------  --------------------------------------------------------
C*    IDIST   Input - Integer: Flag to determine the distribution
C*            type to generate.  See the table below for the available
C*            distributions.
C*
C*    RVAL    Input - Real vector: Vector of a sample of values from
C*            the Uniform(0,1) distribution.  It can have been
C*            generated using a statified sampling scheme.
C*            Output - Real Vector: Vector of length NVAL containing
C*            the generated random values from distribution IDIST.
C*
C*    NVAL    Input - Integer: Number of values to generate.
C*
C*    UMIN    Input - Real: Minimum value from the uniform distribution
C*            to use for generating the random values.  Set UMIN to 0.0
C*            if the variable range is not truncated.  Otherwise, use
C*            subroutine SINV to set the appropriate value for UMIN.
C*            Note: 0 .LE. UMIN .LT. UMAX .LE. 1
C*
C*    UMAX    Input - Real: Maximum value from the uniform distribution
C*            to use for generating the random values.  Set UMAX to 1.0
C*            if the variable range is not truncated.  Otherwise, use
C*            subroutine SINV to set the appropriate value for UMAX.
C*            Note: 0 .LE. UMIN .LT. UMAX .LE. 1
C*
C*    PAR1    Input - Real: Parameter #1 for the distribution.
C*            See the table below for specific distributions.
C*
C*    PAR2    Input - Real: Parameter #2 for the distribution.
C*            See the table below for specific distributions.
C*
C*    PAR3    Input - Real: Parameter #3 for the distribution.
C*            See the table below for specific distributions.
C*
C*    PAR4    Input - Real: Parameter #4 for the distribution.
C*            See the table below for specific distributions.
C*
C*    NUSER   Input - Integer: Number of values in the defining table
C*            for a user specified distribution function.
C*
C*    UUSER   Input - Real vector: Vector of probabilities associated
C*            with the values in the vector XUSER.
C*
C*    XUSER   Input - Real vector: Vector of values associated with the
C*            probabilities in the vector UUSER.  The probabilities
C*            will be used to generate random values from the user
C*            specified distribution using linear interpolation between
C*            the table entries in XUSER.
C*
C*
C*  Distributions Available:
C*
C*    IDIST Distribution     Parameters
C*    ----- ------------     -----------------------------------------
C*      0   Constant         PAR1 = Constant value
C*      1   Uniform          PAR1 = Lower limit, PAR2 = Upper limit
C*      2   Loguniform       (Base 10) PAR1 = Lower limit,
C*                           PAR2 = Upper limit
C*      3   Loguniform       (Base e) PAR1 = Lower limit,
C*                           PAR2 = Upper limit
C*      4   Normal           PAR1 = Mean, PAR2 = Standard deviation
C*      5   Lognormal        (Base 10) PAR1 = Mean,
C*                           PAR2 = Standard deviation
C*      6   Lognormal        (Base e) PAR1 = Mean,
C*                           PAR2 = Standard deviation
C*      7   Exponential      PAR1 = Mean Value, PAR2 = Shift
C*      8   Triangular       PAR1 = Minimum, PAR2 = Mode,
C*                           PAR3 = Maximum
C*      9   Gamma            PAR1 = Exponent on X,
C*                           PAR2 = Parameter in the exponential.
C*                           The mean is PAR1/PAR2
C*     10   Beta             PAR1 = Left end of range,
C*                           PAR2 = Right end of the range,
C*                           PAR3 = "a" parameter (exponent on (X-PAR1))
C*                           PAR4 = "b" Parameter (exponent on (PAR2-X))
C*                           The mean is a/(a+b)
C*     11   Weibull          PAR1  = Scale, PAR2 = Exponent,
C*                           PAR3 = Shift
C*     12   Logistic         PAR1 = Mean, PAR2 = Scale parameter
C*     13   Cauchy           PAR1 = Median, PAR2 = Scale parameter
C*     14   User             None - User specified Distribution
C*     15   Discrete Uniform PAR1 = Minimum, PAR2 = maximum
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C**********************************************************************
C
      REAL LBETA, LGAMMA
      DIMENSION RVAL(NVAL), UUSER(NUSER), XUSER(NUSER)
C
C *** Check for a constant value
C
      IF( IDIST .EQ. 0 ) THEN
        DO 10 I = 1, NVAL
          RVAL(I) = PAR1
   10   CONTINUE
        RETURN
      ENDIF
C
C-----------------------------------------------------------------------
C
C *** Implement the (possible) range truncation on the uniform values
C     before inversion to data values.
C
      IF( UMIN.GT.0.0 .OR. UMAX.LT.1.0 ) THEN
        UDEL = UMAX - UMIN
        DO 20 I = 1, NVAL
          RVAL(I) = UMIN + UDEL*RVAL(I)
   20   CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C         Choose the distribution type and generate the values
C-----------------------------------------------------------------------
C
C *** Uniform (PAR1,PAR2)
C
      IF( IDIST .EQ. 1 ) THEN
        DELP = PAR2 - PAR1
        DO 25 I = 1, NVAL
          RVAL(I) = PAR1 + DELP*RVAL(I)
   25   CONTINUE
        RETURN
      ENDIF
C
C *** Loguniform - Base 10
C
      IF( IDIST .EQ. 2 ) THEN
        PL1 = ALOG10( PAR1 )
        PL2 = ALOG10( PAR2 )
        DELP = PL2 - PL1
        DO 30 I = 1, NVAL
          RV = PL1 + DELP * RVAL(I)
          RVAL(I) = 10.0**RV
   30   CONTINUE
        RETURN
      ENDIF
C
C *** Loguniform - Base e
C
      IF( IDIST .EQ. 3 ) THEN
        PL1 = ALOG( PAR1 )
        PL2 = ALOG( PAR2 )
        DELP = PL2 - PL1
        DO 40 I = 1, NVAL
          RV = PL1 + DELP * RVAL(I)
          RVAL(I) = EXP( RV )
   40   CONTINUE
        RETURN
      ENDIF
C
C *** Normal distribution
C
      IF( IDIST .EQ. 4 ) THEN
        DO 50 I = 1, NVAL
          RV = RVAL(I)
          RVAL(I) = PAR2*PPND(RV,IFAULT) + PAR1
   50   CONTINUE
        RETURN
      ENDIF
C
C *** Lognormal distribution - base 10
C
      IF( IDIST .EQ. 5 ) THEN
        DO 60 I = 1, NVAL
          RV = RVAL(I)
          RVAL(I) = 10.0**(PAR2*PPND(RV,IFAULT) + PAR1)
   60   CONTINUE
        RETURN
      ENDIF
C
C *** Lognormal distribution - base e
C
      IF( IDIST .EQ. 6 ) THEN
        DO 70 I = 1, NVAL
          RV = RVAL(I)
          RVAL(I) = EXP( PAR2*PPND(RV,IFAULT) + PAR1 )
   70   CONTINUE
        RETURN
      ENDIF
C
C *** Exponential distribution
C
      IF( IDIST .EQ. 7 ) THEN
        DO 80 I = 1, NVAL
          RVAL(I) = PAR2 - PAR1*ALOG(1.0-RVAL(I))
   80   CONTINUE
        RETURN
      ENDIF
C
C *** Triangular distribution
C
      IF( IDIST .EQ. 8 ) THEN
        DELP = (PAR2-PAR1) / (PAR3-PAR1)
        SQP1 = SQRT( (PAR3-PAR1)*(PAR2-PAR1) )
        SQP2 = SQRT( (PAR3-PAR1)*(PAR3-PAR2) )
        DO 90 I = 1, NVAL
          IF( RVAL(I) .LE. DELP ) THEN
            RVAL(I) = PAR1 + SQP1*SQRT( RVAL(I) )
          ELSE
            RVAL(I) = PAR3 - SQP2*SQRT( 1.0-RVAL(I) )
          ENDIF
   90   CONTINUE
        RETURN
      ENDIF
C
C *** Gamma distribution
C
      IF( IDIST .EQ. 9 ) THEN
        LGAMMA = 0.0
        UHGH   = 0.99999
        XHGH   = 0.0
        DO 100 I = 1, NVAL
          CALL GINVER( RVAL(I), X, PAR1, PAR2, LGAMMA, UHGH, XHGH )
          RVAL(I) = X
  100   CONTINUE
        RETURN
      ENDIF
C
C *** Beta distribution
C
      IF( IDIST .EQ. 10 ) THEN
        LBETA = 0.0
        DO 110 I = 1, NVAL
          CALL BINVER( RVAL(I), X, PAR1, PAR2, PAR3, PAR4, LBETA )
          RVAL(I) = X
  110   CONTINUE
        RETURN
      ENDIF
C
C *** Weibull Distribution
C
      IF( IDIST .EQ. 11 ) THEN
        WEXP = 1.0 / PAR2
        DO 120 I = 1, NVAL
          RVAL(I) = (-ALOG(1.0-RVAL(I))/PAR1)**WEXP + PAR3
  120   CONTINUE
        RETURN
      ENDIF
C
C *** Logistic distribution
C
      IF( IDIST .EQ. 12 ) THEN
        DO 130 I = 1, NVAL
          RVAL(I) = PAR1 - PAR2*ALOG( 1.0/RVAL(I) - 1.0 )
  130   CONTINUE
        RETURN
      ENDIF
C
C *** Cauchy distribution
C
      IF( IDIST .EQ. 13 ) THEN
        DATA PI / 3.14159265 /
        DO 140 I = 1, NVAL
          RVAL(I) = PAR2 * TAN(PI*(RVAL(I)-0.5)) + PAR1
  140   CONTINUE
        RETURN
      ENDIF
C
C *** User specified distribution
C
      IF( IDIST .EQ. 14 ) THEN
        DO 150 I = 1, NVAL
          RV = RVAL(I)
          RVAL(I) = USRDST( RV, NUSER, UUSER, XUSER )
  150   CONTINUE
        RETURN
      ENDIF
C
C *** Discrete Uniform (PAR1,PAR2)
C
      IF( IDIST .EQ. 15 ) THEN
        I1 = PAR1
        I2 = PAR2
        IDELP = I2 - I1 + 1
        DO 160 I = 1, NVAL
          IVAL = IDELP * RVAL(I)
          RVAL(I) = I1 + IVAL
  160   CONTINUE
        RETURN
      ENDIF
C
      RETURN
      END
