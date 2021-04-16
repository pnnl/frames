      SUBROUTINE BANNER
C*....SUBROUTINE BANNER
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine prints a banner page to the report file.  The
C*    banner page contains the program name, programmer credits, and
C*    run identification information.
C*
C*
C*  Calling Sequence:
C*
C*    A call to subroutine IDENC must preceed a call to this
C*    subroutine.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'IDENC.ins'
      INCLUDE 'FILES.ins'
C
      CHARACTER*64 HEAD(7)
C
C *** Define the header
C
      HEAD(1) =
     *'LL               AA       TTTTTTTTTT   IIIIIIIIII   NNN     NN'
      HEAD(2) =
     *'LL              AAAA          TT           II       NN N    NN'
      HEAD(3) =
     *'LL             AA  AA         TT           II       NN  N   NN'
      HEAD(4) =
     *'LL             AA  AA         TT           II       NN   N  NN'
      HEAD(5) =
     *'LL            AAAAAAAA        TT           II       NN    N NN'
      HEAD(6) =
     *'LL           AA      AA       TT           II       NN     NNN'
      HEAD(7) =
     *'LLLLLLLLLL   AA      AA       TT       IIIIIIIIII   NN      NN'
C
      WRITE(IRPT,7010)
 7010 FORMAT(79(1H*))
C
      WRITE(IRPT,7050)
C
      WRITE(IRPT,7015) HEAD(1)
      WRITE(IRPT,7015) HEAD(2)
      WRITE(IRPT,7015) HEAD(3)
      WRITE(IRPT,7015) HEAD(4)
      WRITE(IRPT,7015) HEAD(5)
      WRITE(IRPT,7015) HEAD(6)
      WRITE(IRPT,7015) HEAD(7)
 7015 FORMAT(8X,A64)
C
      WRITE(IRPT,7020) PRGNAM, PRGVER, PRGDAT
 7020 FORMAT(//32X,A8,1X,A8/26X,'Last Modified on ',A12)
C
      WRITE(IRPT,7025)
 7025 FORMAT(///
     * 14X,'         Latin Hypercube Sampling Program '/
     * 14X,'Multimedia Environmental Pollutant Assessment System'/
     * 14X,'           Developer:  Paul W. Eslinger           '/
     * 14X,'Keyword Input Routines Developed by Dave W. Langford')
C
      WRITE(IRPT,7030) CRUNID, USRNAM
 7030 FORMAT(///12X,'Current Run ID = ',A12,'   User Name = ',A16)
C
      WRITE(IRPT,7040) SYSDAT, SYSTIM
 7040 FORMAT(/17X,'System Date = ',A8,'   System Time = ',A8)
C
      WRITE(IRPT,7050)
 7050 FORMAT(///)
C
      WRITE(IRPT,7010)
C
      RETURN
      END
