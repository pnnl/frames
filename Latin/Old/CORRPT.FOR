      SUBROUTINE CORRPT
C*....SUBROUTINE CORRPT
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine performs the following functions for the
C*    correlation matrix associated with a run of the code:
C*
C*      1. Computes the product-moment correlation matrix for the
C*         generated variables that were "stochastic"
C*
C*      2. Outputs the computed matrix to the report file
C*
C*      3. Outputs the desired rank correlation matrix to the report
C*         file.
C*
C*      4. Computes the maximum difference between the requested
C*         rank correlation matrix and the computed product-moment
C*         correlation matrix and outputs it to the report file.
C*
C*
C*  Calling Sequence:
C*
C*    This subroutine must be preceeded by a call to subroutine UNIV
C*    to compute the mean XSTATS(*,3) and standard deviation XSTATS(*,5)
C*    for each of the stochastic variables.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*    Paul W. Eslinger : 13 Jul 1993 : Version 1.11.2
C*       Add check on large SUM
C*
C***********************************************************************
C
      INCLUDE 'QA.ins'
      INCLUDE 'IDENC.ins'
      INCLUDE 'FILES.ins'
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
      INCLUDE 'DISTC.ins'
      INCLUDE 'SCREENS.ins'
C
      DOUBLE PRECISION SUM
C
C *** Page header and problem title
C
      CALL PAGER( IRPT )
      WRITE(IRPT,1000) TITLE
 1000 FORMAT('Title: ',A)
      WRITE(IRPT,1010)
 1010 FORMAT(/'Correlation matix of the generated data.')
C
C *** Compute the correlations
C
      IERC = 0
      IDX = 0
      DO 70 I = 1, P
        DO 60 J = 1, I
          SUM = 0.0D0
          DO 50 K = 1, N
           SUM = SUM + DBLE(X(K,I)-XSTATS(I,3))*DBLE(X(K,J)-XSTATS(J,3))
   50     CONTINUE
          IDX = IDX + 1
          DATA SMALL /1.0E-20/
          DATA RLARGE /1.0E+37/
          IF( XSTATS(I,5).GT.SMALL .AND. XSTATS(J,5).GT.SMALL) THEN
            IF( SUM .GT. RLARGE ) THEN
              CERR(1)='Data range is beyond the capability of'
              CERR(2)='single precision arithemetic.  Change'
              CERR(3)='your variable data definitions.'
              NERR = 3
              CALL GDERR( NERR, 'ERRCHK' )
              CALL STWO
              STOP
            ENDIF
            CORO(IDX) = (SUM/N) / (XSTATS(I,5)*XSTATS(J,5))
          ELSE
            CORO(IDX) = 0.0
            IERC = 1
          ENDIF
   60   CONTINUE
   70 CONTINUE
C
      IF( IERC .EQ. 1 ) THEN
        WRITE(IRPT,1020) SMALL
 1020   FORMAT(/
     *   ' *** Warning error in subroutine CORRPT ***'/
     *   '     One or more variables had a variance less than '/
     *   '     ',1P,E11.4,' the corresponding entries in the'/
     *   '     correlation matrix were set to zero.')
      ENDIF
C
C *** Output the computed correlation matrix
C     Assume that a maximum of 50 variables will be generated
C
      IDX = 1
      IEND = 0
      WRITE(IRPT,1030) (VNAME(MAP(K)),K=1,P)
 1030 FORMAT(/10X,50(2X,A9))
      DO 20 I = 1, P
        IEND = IEND + I
        WRITE(IRPT,1040) VNAME(MAP(I)),(CORO(K),K=IDX,IEND)
 1040   FORMAT(1X,A9,1P,50(1X,E10.3))
        IDX = IDX + I
   20 CONTINUE
C
C *** Output the input correlation matrix
C     Assume that a maximum of 50 variables will be generated
C
      WRITE(IRPT,1015)
 1015 FORMAT(//'Input rank correlation matix.')
C
C *** If a correlation matrix was not entered then build it here
C
      IF( CIDENT ) THEN
        IDX = 0
        DO 24 I = 1, P
          DO 23 J = 1, I
            IDX = IDX + 1
            IF( I .EQ. J ) THEN
              CORSAV(IDX) = 1.0
            ELSE
              CORSAV(IDX) = 0.0
            ENDIF
   23     CONTINUE
   24   CONTINUE
      ENDIF
C
      IDX = 1
      IEND = 0
      WRITE(IRPT,1030) (VNAME(MAP(K)),K=1,P)
      DO 25 I = 1, P
        IEND = IEND + I
        WRITE(IRPT,1040) VNAME(MAP(I)),(CORSAV(K),K=IDX,IEND)
        IDX = IDX + I
   25 CONTINUE
 
C
C *** Determine the maximum difference between the computed product
C     moment correlation matrix and the input rank correlation matrix
C
      CDIF = 0.0
      DO 30 I = 1, MXPP
        CDIF = AMAX1( ABS(CORSAV(I)-CORO(I)), CDIF )
   30 CONTINUE
C
      WRITE(IRPT,1080) CDIF
 1080 FORMAT(/
     * 'The maximum difference between the input rank correlation ',
     * 'matrix and the',/'computed product moment correlation matrix',
     * ' is ',1P,E10.3)
C
      RETURN
      END
