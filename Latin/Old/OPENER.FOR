      SUBROUTINE OPENER
C*....SUBROUTINE OPENER
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine opens the files defined in the keyword
C*    control file.  File handling errors are detected and error
C*    messages printed when appropriate.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'FILES.ins'
      INCLUDE 'IDENC.ins'
C
C *** Open the output report file
C
      IRPT = 10
      OPEN(IRPT,FILE=FNRPT,ERR=9010,STATUS='UNKNOWN')
C
C *** Open the output data file
C
      IDAT = 11
      OPEN(IDAT,FILE=FNDAT,ERR=9020,STATUS='UNKNOWN')
C
      RETURN
C
C *** Errors opening the files
C
 9010 CONTINUE
        WRITE(IERR,1010) FNRPT
 1010   FORMAT(' *** Terminal error in Subroutine OPENER ***'/
     *    ' The requested report file cannot be opened'/
     *    ' File = ',A)
        STOP 'OPEN9010'
C
 9020 CONTINUE
        WRITE(IERR,1020) FNDAT
        WRITE(IRPT,1020) FNDAT
 1020   FORMAT(' *** Terminal error in Subroutine OPENER ***'/
     *    ' The requested output data file cannot be opened'/
     *    ' File = ',A)
        STOP 'OPEN9020'
C
      END
