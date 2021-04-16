      SUBROUTINE SCREEN
C*....SUBROUTINE SCREEN
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine writes an opening page to the terminal screen.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*    Paul W. Eslinger : 27 Aug 1992 : Make IZERO Integer*2
C*
C***********************************************************************
C
      INCLUDE 'IDENC.ins'
*CPU PC
C
      INTEGER*2 IZERO
C
      CALL CLS
      IZERO = 0
      CALL LOCATEW( IZERO, IZERO, IZERO )
*CPUEND
C
      WRITE(*,1000) CRUNID, PRGNAM, PRGVER, PRGDAT, USRNAM
 1000 FORMAT(
     * 17X,'                      LATIN'/
     * 17X,'     Latin Hypercube Stochastic Data Generation'/
     * 17X,'Multimedia Environmental Pollutant Assessment System'/
     * 17X,'          Pacific Northwest Laboratory'/
     * 17X,'              Richland, Washington'/
     * 17X,'              Run ID: ',A12/
     * 1X,79('-')/
     * 1X,A8,1X,A8,17X,A12,17X,A16/
     * 1X,79('-'))
C
      RETURN
      END
