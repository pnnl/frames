      SUBROUTINE SINV( IDIST, XMIN, XMAX, UMIN, UMAX, PAR1, PAR2, PAR3,
     *           PAR4, ITRUNC, NUSER, UUSER, XUSER, IUNIT, BADDAT )
C*....SUBROUTINE SINV( IDIST, XMIN, XMAX, UMIN, UMAX, PAR1, PAR2, PAR3,
C*   *           PAR4, ITRUNC, NUSER, UUSER, XUSER, IUNIT, BADDAT )
C**********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine is a part of the set of random number
C*    generation routines in the SUMO code.  It has two purposes:
C*
C*      (1) Perform error checking on the definition of probability
C*          distributions.
C*
C*      (2) Perform the inverse mapping for handling truncation limits
C*          for distributions.
C*
C*
C*  Notes:
C*
C*    1. Each distribution is generated (in SGEN) using the Probability
C*       Integral Transformation method.  I.e., the random value X is
C*       found implicitly from the equation F(X)=U where U is a
C*       generated value from the standard (0,1) uniform distribution,
C*       and F(X) is the cumulative distribution function for the
C*       random variable X.
C*
C*    2. Each distribution may be truncated between two specified
C*       limits.
C*
C*    3. The user may specify a cumulative distribution function
C*       in the form of a table of values.  Linear interpolation
C*       will be used to generate values from the tabled distribution.
C*
C*    4. Adding or deleting a distribution requires changes in four
C*       places.  This subroutine, subroutine SGEN, the vector DLABEL
C*       of distribution labels (subroutine INIT), and the parameter
C*       MXDIST (common block DISTC).  The convention is that a user
C*       specified distribution is always the highest number for the
C*       variable IDIST.
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
C*    ITRUNC  Input - Integer: Flag to determine if range truncation
C*            is to be performed.  If ITRUNC = 0 then no truncation
C*            will be done.  If ITRUNC = 1, truncation is done on the
C*            left at the value XMIN.  If ITRUNC = 2, truncation is
C*            done on the right at the value XMAX.  If ITRUNC = 3,
C*            truncation is done at both XMIN and XMAX.
C*
C*    XMIN    Input - Real: Left truncation limit for the distribution.
C*            The value is used only if ITRUNC takes the value 1 or 3.
C*            XMIN must be less than XMAX if ITRUNC = 3.
C*
C*    XMAX    Input - Real: Right truncation limit for the distribution.
C*            The value is used only if ITRUNC takes the value 2 or 3.
C*            XMIN must be less than XMAX if ITRUNC = 3.
C*
C*    UMIN    Output - Real: Minimum value from the uniform distribution
C*            to use for generating the random values.  Corresponds to
C*            the left limit XMIN if truncation is desired, otherwise it
C*            is set to 0.0.
C*
C*    UMAX    Output - Real: Maximum value from the uniform distribution
C*            to use for generating the random values.  Corresponds to
C*            the right limit XMAX if truncation is desired, otherwise i
C*            is set to 1.0.
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
C*    IUNIT   Input - Integer: Unit number for error messages
C*
C*    BADDAT  Logical flag.  Initialized in the calling routine.  It
C*            will be set to .TRUE. if an error is detected.
C*
C*
C*  Common Block Variables:
C*
C*    DLABEL  Input - Character*16 Vector: Vector of labels for the
C*            different distributions which can be generated.
C*
C*    MXDST2  Input - Integer: Maximum number for a distribution
C*            type.
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
      LOGICAL BADDAT
C
      INCLUDE 'DISTC.ins'
C
      DIMENSION UUSER(NUSER), XUSER(NUSER)
C
C *** Set up the uniform limits for the case of no truncation
C
      UMIN = 0.0
      UMAX = 1.0
C
C *** Check for a valid distribution type
C
      IF( IDIST.LT.0 .OR. IDIST.GT.MXDST2 ) THEN
        WRITE(IUNIT,1050) IDIST, MXDST2
 1050   FORMAT(/' Terminal error in Subroutine SINV'/
     *          ' The distribution type number ',I5,' is invalid'/
     *          ' The legal range is 0 to ',I2)
        BADDAT = .TRUE.
        RETURN
      ENDIF
C
C *** Check for a constant value
C
      IF( IDIST .EQ. 0 ) THEN
        RETURN
      ENDIF
C
C *** Uniform
C
      IF( IDIST .EQ. 1 ) THEN
        IF( PAR1 .GE. PAR2 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
 1000     FORMAT(/' Terminal Error in Subroutine SINV for distribution',
     *            ' type ',I2/' Distribution label is ',A16)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
 1010     FORMAT(' The four parameter values are'/1P,4(2X,E14.6))
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
 1020     FORMAT(' Truncation error with truncation flag = ',I2/
     *           ' XMIN = ',1P,E14.6,'   XMAX = ',E14.6)
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 )
     *    UMIN = (XMIN-PAR1) / (PAR2-PAR1)
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 )
     *    UMAX = (XMAX-PAR1) / (PAR2-PAR1)
        RETURN
      ENDIF
C
C *** Loguniform - Base 10
C
      IF( IDIST .EQ. 2 ) THEN
        IF( PAR1.GE.PAR2 .OR. PAR1.LE.0.0 .OR. PAR2.LE.0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        PL1 = ALOG10( PAR1 )
        PL2 = ALOG10( PAR2 )
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 )
     *    UMIN = (ALOG10(XMIN)-PL1) / (PL2-PL1)
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 )
     *    UMAX = (ALOG10(XMAX)-PL1) / (PL2-PL1)
        RETURN
      ENDIF
C
C *** Loguniform - Base e
C
      IF( IDIST .EQ. 3 ) THEN
        IF( PAR1.GE.PAR2 .OR. PAR1.LE.0.0 .OR. PAR2.LE.0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        PL1 = ALOG( PAR1 )
        PL2 = ALOG( PAR2 )
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 )
     *    UMIN = (ALOG(XMIN)-PL1) / (PL2-PL1)
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 )
     *    UMAX = (ALOG(XMAX)-PL1) / (PL2-PL1)
        RETURN
      ENDIF
C
C *** Normal distribution
C
      IF( IDIST .EQ. 4 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMIN-PAR1) / PAR2
          UMIN = ALNORM( X, .FALSE. )
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMAX-PAR1) / PAR2
          UMAX = ALNORM( X, .FALSE. )
        ENDIF
        RETURN
      ENDIF
C
C *** Lognormal distribution - base 10
C
      IF( IDIST .EQ. 5 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. XMIN.LE.0.0) .OR.
     *    (ITRUNC.EQ.2 .AND. XMAX.LE.0.0) .OR.
     *    (ITRUNC.EQ.3 .AND. (XMIN.LE.0.0.OR.XMAX.LE.0.0) ).OR.
     *    (ITRUNC.EQ.3 .AND. XMAX.LE.XMIN) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (ALOG10(XMIN)-PAR1) / PAR2
          UMIN = ALNORM( X, .FALSE. )
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (ALOG10(XMAX)-PAR1) / PAR2
          UMAX = ALNORM( X, .FALSE. )
        ENDIF
        RETURN
      ENDIF
C
C *** Lognormal distribution - base e
C
      IF( IDIST .EQ. 6 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. XMIN.LE.0.0) .OR.
     *    (ITRUNC.EQ.2 .AND. XMAX.LE.0.0) .OR.
     *    (ITRUNC.EQ.3 .AND. (XMIN.LE.0.0.OR.XMAX.LE.0.0) ).OR.
     *    (ITRUNC.EQ.3 .AND. XMAX.LE.XMIN) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (ALOG(XMIN)-PAR1) / PAR2
          UMIN = ALNORM( X, .FALSE. )
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (ALOG(XMAX)-PAR1) / PAR2
          UMAX = ALNORM( X, .FALSE. )
        ENDIF
        RETURN
      ENDIF
C
C *** Exponential distribution
C
      IF( IDIST .EQ. 7 ) THEN
        IF( PAR1 .LE. PAR2 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. XMIN.LT.PAR2) .OR.
     *      (ITRUNC.EQ.2 .AND. XMAX.LT.PAR2) .OR.
     *      (ITRUNC.EQ.3 .AND. XMAX.LE.XMIN) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR2.OR.XMAX.LT.PAR2)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          UMIN = 1.0 - EXP(-(XMIN-PAR2)/PAR1)
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          UMAX = 1.0 - EXP(-(XMAX-PAR2)/PAR1)
        ENDIF
        RETURN
      ENDIF
C
C *** Triangular distribution
C
      IF( IDIST .EQ. 8 ) THEN
        IF( (PAR1.GE.PAR2) .OR. (PAR1.GE.PAR3) .OR.
     *      (PAR2.GE.PAR3) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR3)) .OR.
     *      (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR3)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR3)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          IF( XMIN .LE. PAR2 ) THEN
            UMIN = (XMIN-PAR1)**2/((PAR3-PAR1)*(PAR2-PAR1))
          ELSE
            UMIN = 1.0 - (XMIN-PAR3)**2/((PAR3-PAR1)*(PAR3-PAR2))
          ENDIF
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          IF( XMAX .LE. PAR2 ) THEN
            UMAX = (XMAX-PAR1)**2/((PAR3-PAR1)*(PAR2-PAR1))
          ELSE
            UMAX = 1.0 - (XMAX-PAR3)**2/((PAR3-PAR1)*(PAR3-PAR2))
          ENDIF
        ENDIF
        RETURN
      ENDIF
C
C *** Gamma distribution
C
      IF( IDIST .EQ. 9 ) THEN
        IF( PAR1.LE.0.0 .OR. PAR2.LE.0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.0.0) ) .OR.
     *      (ITRUNC.EQ.2 .AND. (XMAX.LT.0.0) ) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMIN.LT.0.0) ) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.LT.0.0) ) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC .GT. 0 ) LGAMMA = ALNGAM(PAR1,IFT)
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X01 = XMIN*PAR2
          UMIN = GAMAIN( X01, PAR1, LGAMMA )
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X01 = XMAX*PAR2
          UMAX = GAMAIN( X01, PAR1, LGAMMA )
        ENDIF
        RETURN
      ENDIF
C
C *** Beta distribution
C
      IF( IDIST .EQ. 10 ) THEN
        IF( PAR1.GE.PAR2 .OR. PAR3.LE.0.0 .OR. PAR4.LE.0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR2)) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC .GT. 0 ) LBETA = ALNGAM(PAR3,IFT) + ALNGAM(PAR4,IFT)
     *                            - ALNGAM((PAR3+PAR4),IFT)
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X01 = (XMIN-PAR1) / (PAR2 - PAR1)
          UMIN = BETAIN( X01, PAR3, PAR4, LBETA )
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X01 = (XMAX-PAR1) / (PAR2 - PAR1)
          UMAX = BETAIN( X01, PAR3, PAR4, LBETA )
        ENDIF
        RETURN
      ENDIF
C
C *** Weibull Distribution
C
      IF( IDIST .EQ. 11 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. XMIN.LT.PAR3) .OR.
     *      (ITRUNC.EQ.2 .AND. XMAX.LT.PAR3) .OR.
     *      (ITRUNC.EQ.3 .AND. XMAX.LE.XMIN) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR3.OR.XMAX.LT.PAR3)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          UMIN = 1.0 - EXP(-PAR1*(XMIN-PAR3)**PAR2)
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          UMAX = 1.0 - EXP(-PAR1*(XMAX-PAR3)**PAR2)
        ENDIF
        RETURN
      ENDIF
C
C *** Logistic distribution
C
      IF( IDIST .EQ. 12 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMIN-PAR1) / PAR2
          UMIN = 1.0 / ( 1.0 + EXP(-X) )
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMAX-PAR1) / PAR2
          UMAX = 1.0 / ( 1.0 + EXP(-X) )
        ENDIF
        RETURN
      ENDIF
C
C *** Cauchy distribution
C
      IF( IDIST .EQ. 13 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.3 .AND. XMAX.LE.XMIN ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        DATA PI / 3.14159265 /
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMIN-PAR1) / PAR2
          UMIN = 0.5 + ATAN(X) / PI
        ENDIF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMAX-PAR1) / PAR2
          UMAX = 0.5 + ATAN(X) / PI
        ENDIF
        RETURN
      ENDIF
C
C *** User specified distribution
C
      IF( IDIST .EQ. 14 ) THEN
        IF( NUSER .LT. 2 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IBAD = 0
        IF( UUSER(1) .NE. 0.0 ) IBAD = 1
        IF( UUSER(NUSER) .NE. 1.0 ) IBAD = 1
        DO 10 I = 1, NUSER
           IF( UUSER(I).LT.0.0 .OR. UUSER(I).GT.1.0 ) IBAD = 1
           IF( I .GT. 1 ) THEN
             IF( UUSER(I).LE.UUSER(I-1) .OR. XUSER(I).LE.XUSER(I-1) )
     *         IBAD = 1
           ENDIF
   10   CONTINUE
        IF( IBAD .NE. 0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.XUSER(1).OR.
     *          XMIN.GE.XUSER(NUSER))) .OR.
     *      (ITRUNC.EQ.2 .AND. (XMAX.LT.XUSER(1).OR.
     *          XMAX.GE.XUSER(NUSER))) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMIN.LT.XUSER(1))) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.GT.XUSER(NUSER))) .OR.
     *      (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        ENDIF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 )
     *    UMIN = USRDST( XMIN, NUSER, XUSER, UUSER )
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 )
     *    UMAX = USRDST( XMAX, NUSER, XUSER, UUSER )
        RETURN
      ENDIF
C
C *** Discrete Uniform distribution
C     Truncation does not apply
C
      IF( IDIST .EQ. 15 ) THEN
        IF( PAR2 .LT. PAR1 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1040) PAR1, PAR2
 1040     FORMAT(' The two limits parameter values are'/1P,2(2X,E14.6))
          BADDAT = .TRUE.
          RETURN
        ENDIF
      ENDIF
C
      RETURN
      END
