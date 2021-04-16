      SUBROUTINE MARGIN
C*....SUBROUTINE MARGIN
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine schedules generation of the marginal statistical
C*    distribution for each of the "stochastic" variables.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C*
C*  Notes:
C*
C*    1. The matrix of column ranks of the values in X must be
C*    nonsingular, thus, the stratified values cannot be generated
C*    in a sequential fashion across the range of each variable.
C*    Subroutine U01S1 generates the values sequentially and then
C*    randomly permutes them.
C*
C*    2. A column of the X matrix contains the values for a single
C*    variable.
C*
C***********************************************************************
C
      INCLUDE 'FILES.ins'
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
C
C *** Loop over all of the variable distributions
C
      ISTOC = 0
      DO 30 I = 1, PTOT
C
        IF( VTYPE(I) .EQ. 0 ) THEN
C
C ***     Don't need the uniform(0,1) values in SGEN for constant
C         distributions, just need one value
C ***     The fudge on NUSER and NSTRT are to prevent an illegal index
C         error in SINV when a user distribution has NOT been defined
C
          NUSER = 1
          NSTRT = 1
          NNEED = 1
          CALL SGEN( VTYPE(I), RWORK, NNEED,
     *      UMIN(I), UMAX(I),
     *      VPARMS(I,1), VPARMS(I,2), VPARMS(I,3), VPARMS(I,4),
     *      NUSER, FTABLE(NSTRT), XTABLE(NSTRT) )
C
          XOUT(I) = RWORK(1)
C
        ELSE
C
C ***     Stochastic distributions
C
          ISTOC = ISTOC + 1
C
C ***     Generate Uniform(0,1) values with stratification
C
          CALL U01S1( N, RWORK, IWORK, DSEED )
C
C ***     Transform to the desired distribution
C ***     The fudge on NUSER and NSTRT are to prevent an illegal index
C         error in SINV when a user distribution has NOT been defined
C
          IF( VTYPE(I) .EQ. 14 ) THEN
            NUSER = PTABLE(I,2)
            NSTRT = PTABLE(I,1)
          ELSE
            NUSER = 1
            NSTRT = 1
          ENDIF
          CALL SGEN( VTYPE(I), RWORK, N, UMIN(I), UMAX(I),
     *      VPARMS(I,1), VPARMS(I,2), VPARMS(I,3), VPARMS(I,4),
     *      NUSER, FTABLE(NSTRT), XTABLE(NSTRT) )
C
C ***     Store the generated values in the proper place
C
          DO 20 J = 1, N
            X(J,ISTOC) = RWORK(J)
   20     CONTINUE
C
        ENDIF
C
   30 CONTINUE
C
      RETURN
      END
