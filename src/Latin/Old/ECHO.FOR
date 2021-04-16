      SUBROUTINE ECHO
C*....SUBROUTINE ECHO
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine schedules the writing of a banner page and the
C*    review signature block of the report file.  In addition, it
C*    writes a summary of the files used in this run of the code and
C*    a summary problem definition.
C*
C*
C*  Calling sequence:
C*
C*    1. A call to this subroutine must be preceeded with a call to
C*       subroutines IDENC and QASET.
C*
C*    2. A call to this subroutine must be preceeded with a call to
C*       subroutine INPUTS.
C*
C*    3. This subroutine should be called before anything is written
C*       to the report file.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
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
C
C *** Banner page
C
      CALL BANNER
C
C *** Review signature section
C
      CALL REVIEW
C
C *** Page header and problem title
C
      CALL PAGER( IRPT )
      WRITE(IRPT,1000) TITLE
 1000 FORMAT('Title: ',A)
C
C *** QA Status
C
      IF( QA ) THEN
        WRITE(IRPT,1010) VERN, VDAT
 1010   FORMAT(//
     *  'This program is has been developed and tested according',
     *  ' to project procedures.'/
     *  'The verification report number is ',A,' dated ',A,'.')
      ELSE
        WRITE(IRPT,1020)
 1020   FORMAT(//
     *  'This program is experimental and has not been formally',
     *  ' tested according to'/
     *  'project procedures.  All results are preliminary in nature.')
      ENDIF
C
C *** Give a summary of input and output file names
C
      WRITE(IRPT,1100)
 1100 FORMAT(//'Summary of files used')
C
      WRITE(IRPT,1110) FNERR
 1110 FORMAT(/'Error message file'/'Name: ',A)
C
      WRITE(IRPT,1120) FNKEY
 1120 FORMAT(/'Input keyword control file'/'Name: ',A)
C
      WRITE(IRPT,1130) FNDAT
 1130 FORMAT(/'Output data file'/'Name: ',A)
C
C *** Summary of variables and distributions
C
      WRITE(IRPT,1195) PTOT, P, PCON, N, DSEED
 1195 FORMAT(//
     * 'The run contains ',I3,' total variables.'/
     * 'Of them, ',I3,' are "stochastic" and ',I3,' are "constant".'/
     * 'A total of ',I5,' iterations of data will be generated.'/
     * 'The random number seed is ',1P,D17.10)
C
      WRITE(IRPT,1200)
 1200 FORMAT(/'Summary of variables and distributions for this run.')
      DO 10 I = 1, PTOT
        CALL PRTDST( I )
   10 CONTINUE
C
      RETURN
      END
