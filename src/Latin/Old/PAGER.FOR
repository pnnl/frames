      SUBROUTINE PAGER( IUNIT )
C*....SUBROUTINE PAGER( IUNIT )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine writes a one line page header to an output file.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C*
C*  Variable definitions:
C*
C*    IUNIT  : Integer unit number for the output file
C*
C***********************************************************************
C
      INCLUDE 'IDENC.ins'
C
      WRITE(IUNIT,7000) PRGNAM, PRGVER, USRNAM, CRUNID
 7000 FORMAT(
     * 'Program: ',A8,' Version: ',A8,' User: ',A16,'  Run ID: ',A12)
C
      RETURN
      END
