      SUBROUTINE REVIEW
C*....SUBROUTINE REVIEW
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine prints a signature block for review signatures
C*    to the report file.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'FILES.ins'
C
      WRITE(IRPT,7020)
 7020 FORMAT(//27X,'Review Signature Block')
C
      WRITE(IRPT,7030)
 7030 FORMAT(//1X,'Input Prepared By:',30('_'),9X,'Date:',15('_')/)
C
      WRITE(IRPT,7040)
 7040 FORMAT(/1X,'Input Reviewed By:',30('_'),9X,'Date:',15('_')/)
C
      WRITE(IRPT,7000)
 7000 FORMAT(79(1H*))
C
      RETURN
      END
