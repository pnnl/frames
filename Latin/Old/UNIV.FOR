      SUBROUTINE UNIV
C*....SUBROUTINE UNIV
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine scheduls computation of some univariate
C*    statistics for each of the "stochastic" variables.  The
C*    computed values are then written to the report file.
C*
C*    The statistics are also stored for later use.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'FILES.ins'
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
      INCLUDE 'IDENC.ins'
C
C *** Don't compute the statistics if all of the variables are
C     defined as constants
C
      IF( PTOT .LE. 0 ) THEN
        RETURN
      ENDIF
C
      CALL PAGER( IRPT )
      WRITE(IRPT,1005) TITLE
 1005 FORMAT('Title: ',A)
      WRITE(IRPT,1000)
 1000 Format(/'Univariate statistics for the "stochastic" variables.')
C
      WRITE(IRPT,1010)
 1010 FORMAT(/'Variable     Minimum    Maximum       Mean    ',
     *        ' Median   St. Dev. C. of Var.')
C
C *** Loop over all of the variables defined for this problem
C
      ISTOC = 0
      DO 30 I = 1, PTOT
        IF( VTYPE(I) .NE. 0 ) THEN
          ISTOC = ISTOC + 1
C
C ***     Pull out one "stochastic" variable at a time
C
          DO 20 J = 1, N
            XTMP(J) = X(J,ISTOC)
   20     CONTINUE
C
C ***     Compute the univariate statistics
C
          CALL USTAT( XTMP, N, RWORK, XMIN, XMAX, XAVG, XMED, XSTD,
     *                XCOV, XSKW, IERS )
C
C ***     Output the univariate statistics
C
          WRITE(IRPT,1050) VNAME(I), XMIN, XMAX, XAVG, XMED, XSTD,
     *                     XCOV
 1050     FORMAT(A9,1P,6(1X,E10.3))
C
C ***     Save the statistics for later use
C
          XSTATS(ISTOC,1) = XMIN
          XSTATS(ISTOC,2) = XMAX
          XSTATS(ISTOC,3) = XAVG
          XSTATS(ISTOC,4) = XMED
          XSTATS(ISTOC,5) = XSTD
          XSTATS(ISTOC,6) = XCOV
C
        ENDIF
C
   30 CONTINUE
C
      RETURN
      END
