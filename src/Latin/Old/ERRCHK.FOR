      SUBROUTINE ERRCHK
C*....SUBROUTINE ERRCHK
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine performs error checking on the inputs for the
C*    problem definition.
C*
C*
C*  Notes:
C*
C*    1. Initial error checking is performed in subroutine INPUTS.
C*       This subroutine performs error checks for consistency of
C*       the input data.
C*
C*    2. This subroutine attempts to output error messages for all
C*       errors in the input data before execution is terminated.  In
C*       some cases execution is terminated before all error checks
C*       have been performed.
C*
C*
C*  Calling Sequence:
C*
C*    1. This subroutine must be preceeded with a call to subroutine
C*       INPUTS.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
      INCLUDE 'FILES.ins'
      INCLUDE 'DISTC.ins'
      INCLUDE 'SCREENS.ins'
C
      BADDAT = .FALSE.
C
C ---------------------------------------------------------------------
C        Checks done on definitions of statistical distributions
C ---------------------------------------------------------------------
C
      IRNG = 0
      DO 30 I = 1, PTOT
        IF( VTYPE(I).LT.0 .OR. VTYPE(I).GT.MXDST2 ) THEN
          IRNG = 1
          BADDAT = .TRUE.
        ENDIF
        IF( VNAME(I) .EQ. ' ' ) THEN
          IRNG = 2
          BADDAT = .TRUE.
        ENDIF
   30 CONTINUE
      IF( IRNG .EQ. 1 ) THEN
        WRITE(IERR,1070)
 1070   FORMAT(/
     *    ' *** Terminal error in subroutine ERRCHK'/
     *    '     A variable distribution was not defined'/
     *    '     Check all VARIABLE cards to ensure that the variable'/
     *    '     numbers complete the sequence 1 to K, where a total'/
     *    '     of K variables were defined.  Order of entry of the'/
     *    '     cards is not important.')
        BADDAT = .TRUE.
      ENDIF
      IF( IRNG .EQ. 2 ) THEN
        WRITE(IERR,1090)
 1090   FORMAT(/
     *    ' *** Terminal error in subroutine ERRCHK'/
     *    '     A variable label was not defined'/
     *    '     Check all LABEL cards to ensure that the variable'/
     *    '     numbers complete the sequence 1 to K, where a total'/
     *    '     of K variables were defined.  Order of entry of the'/
     *    '     cards is not important.')
        BADDAT = .TRUE.
      ENDIF
C
C ---------------------------------------------------------------------
C          Check for the minimum set of required files
C ---------------------------------------------------------------------
C
C  The file names are initialized to ' ' in subroutine INPUTS
C  FNRPT : File name for unit IRPT - Output report file
C  FNDAT : File name for unit IDAT - Output data file
C
      IF( FNRPT .EQ. ' ' ) THEN
        WRITE(IERR,1040)
 1040   FORMAT(/
     *   ' *** Terminal error in subroutine ERRCHK'/
     *   '     File name not defined for the report file'/
     *   '     Modify the FILE keyword')
        BADDAT = .TRUE.
      ENDIF
      IF( FNDAT .EQ. ' ' ) THEN
        WRITE(IERR,1050)
 1050   FORMAT(/
     *   ' *** Terminal error in subroutine ERRCHK'/
     *   '     File name not defined for the data file'/
     *   '     Modify the FILE keyword')
        BADDAT = .TRUE.
      ENDIF
C
C ---------------------------------------------------------------------
C            Checks done on the correlation matrix
C ---------------------------------------------------------------------
C
      IF( .NOT.CIDENT ) THEN
C
C ***   Check on the number of values entered
C
        NP = P * (P+1) / 2
        IF( NCOR .NE. NP ) THEN
          WRITE(IERR,1000) NCOR, NP
 1000     FORMAT(/
     *    ' *** Terminal error in subroutine ERRCHK'/
     *    '     The correlation matrix input contained ',I4,' entries'/
     *    '     while ',I4,' entries are required'/
     *    '     Modify the CORRELATION or VARIABLE keywords'/
     *    '     The correlation matrix applies only to ',
     *    'nonconstant variables.')
          BADDAT = .TRUE.
        ENDIF
C
C ***   Save the correlation matrix and check the individual entries
C       Individual values must fall in the range -1 to 1 and the
C       diagonal terms must all be equal to 1
C
        IF( NCOR .GT. 0 ) THEN
          IRNG = 0
          DO 10 I = 1, NP
            IF( COR(I).LT.-1.0 .OR. COR(I).GT.1.0 ) IRNG = 1
   10     CONTINUE
          INDX = 0
          DO 20 I = 1, P
            INDX = INDX + I
            IF( COR(INDX) .NE. 1.0 ) IRNG = 1
   20     CONTINUE
          IF( IRNG .NE. 0 ) THEN
            WRITE(IERR,1010)
 1010       FORMAT(/
     *      ' *** Terminal error in subroutine ERRCHK'/
     *      '     The correlation matrix input contained entries'/
     *      '     either less than -1 or greater than 1'/
     *      '     or the diagonal terms were not all equal to 1'/
     *      '     Modify the CORRELATION keyword')
            BADDAT = .TRUE.
          ENDIF
        ENDIF
C
C ***   Cholesky decomposition of the correlation matrix - stored as
C       a vector.  The matrix is overwritten by the decomposition.
C       This decomposition is needed later, and, it checks to see
C       if the input matrix is positive definite (i.e. the input
C       values form a consistent set of pairwise correlations).
C       Don't attempt the decomposition if the data set was already
C       found to contain errors.
C
        IF( .NOT.BADDAT ) THEN
          CALL CHOLP( COR, P, NP, NULLTY, IFAULT )
          IF( IFAULT .NE. 0 ) THEN
            WRITE(IERR,1020)
 1020       FORMAT(/
     *       ' *** Terminal error in subroutine ERRCHK'/
     *       '     Correlation Matrix NOT Positive Definite'/
     *       '     Modify the CORRELATION keyword')
            BADDAT = .TRUE.
          ENDIF
        ENDIF
C
      ENDIF
C
C ---------------------------------------------------------------------
C            Checks done on the number of iterations
C ---------------------------------------------------------------------
C
      NTMP = 1.25 * FLOAT(P)
      IF( N .LT. P ) THEN
        WRITE(IERR,1060) N, P, NTMP
 1060   FORMAT(/
     *   ' *** Warning error in subroutine ERRCHK ***'/
     *   '     The number of iterations entered was ',I4/
     *   '     It must be as large as the number of stochastic ',
     *   'variables ',I4/
     *   '     A value of at least ',I4,' is recommended ',
     *   'and will be used in this run'/
     *   '     You may wish to modify the ITERATE keyword.')
        IF( NTMP .GT. MAXN ) THEN
          WRITE(IERR,1065) NTMP, MAXN
 1065     FORMAT(/
     *     ' *** Terminal error in subroutine ERRCHK ***'/
     *     '     the attempted modification to the number of ',
     *     'replications to ',I5/
     *     '     exceeded the parameter value MAXN = ',I5/
     *     '     Change the parameter and recompile the program.')
          BADDAT = .TRUE.
        ENDIF
        N = NTMP
      ENDIF
      IF( N .LT. NTMP ) THEN
        WRITE(IERR,1066) N, P, NTMP
 1066   FORMAT(/
     *   ' *** Warning error in subroutine ERRCHK ***'/
     *   '     The number of iterations entered was ',I4/
     *   '     It must be as large as the number of stochastic ',
     *   'variables ',I4/
     *   '     A value of at least ',I4,' is recommended '/
     *   '     You may wish to modify the ITERATE keyword.')
      ENDIF
C
C ---------------------------------------------------------------------
C Checks done on the choice and definition of statistical distributions
C ---------------------------------------------------------------------
C
C *** The fudge on NUSER and NSTRT are to prevent an illegal index
C     error in SINV when a user distribution has NOT been defined
C
      DO 40 I = 1, PTOT
        NUSER = PTABLE(I,2)
        NSTRT = PTABLE(I,1)
        IF( VTYPE(I) .NE. 14 ) THEN
          NUSER = 1
          NSTRT = 1
        ENDIF
        CALL SINV( VTYPE(I), VTLIM(I,1), VTLIM(I,2), UMIN(I), UMAX(I),
     *       VPARMS(I,1), VPARMS(I,2), VPARMS(I,3), VPARMS(I,4),
     *       VTRUN(I), NUSER, FTABLE(NSTRT), XTABLE(NSTRT), IERR,
     *       BADDAT )
   40 CONTINUE
C
C ---------------------------------------------------------------------
C                Stop if any bad data was detected
C ---------------------------------------------------------------------
C
      IF( BADDAT ) THEN
        CERR(1)='One or more errors were encountered in the input'
        CERR(2)='data set for this sensitivity case.  The input file'
        CERR(3)='name is ' // FNKEY
        CERR(4)='The keyword data must be modified before continuing.'
        CERR(5)='Error messages are written to a file.  The file name'
        CERR(6)='is: ' // FNERR
        NERR = 6
        CALL GDERR( NERR, 'ERRCHK' )
        CALL STWO
        STOP
      ENDIF
C
      RETURN
      END
