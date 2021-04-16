C----------------------------------------------------------------------------
C
      SUBROUTINE INITNV
C
C     This module initializes parameter values for each radionuclide
C     chain.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification:  10-Feb-98 BAN
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C-----------------------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'AQUPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
      DO 200 IN = 1, 9
        QAIR(IN) = 0.0
        QSW(IN) = 0.0
        QWAS(IN) = 0.0
        NONAGS(IN) = 0.0
        RESSOL(IN) = 0.0
        RESAVG(IN) = 0.0
        NONAGD(IN) = 0.0
        GWCON(IN) = 0.0
        SWCON(IN) = 0.0
        ARPCON(IN) = 0.0
        SEDCON(IN) = 0.0
        RTAVAL(IN) = 0.0
        PAKADD(IN) = 0.0
        RTENV(IN) = 0.0
c
          SOLCON(IN,1) = 0.0
          SOLAVG(IN,1) = 0.0
          PLTCON(IN,1) = 0.0
          ARFCON(IN,1) = 0.0
c
        DO 202 I = 1, 4
          AQUCON(IN,I) = 0.0
          ANMCON(IN,I) = 0.0
  202   CONTINUE
c
        DO 204 I = 1, 5
          ARF2CN(IN,I) = 0.0
          SL2CON(IN,I) = 0.0
          SL2AVG(IN,I) = 0.0
  204   CONTINUE
c        
        DO 102 IP = 1, 28
          EXPOS(IN,IP) = 0.0
          DO 104 IY = 1, 70
            EXPOST(IY,IN,IP) = 0.0
  104     CONTINUE
  102   CONTINUE
c
  200 CONTINUE
C     Set over-all time index (covers both prior and release periods)--
      ITIMT = 0
c
C     Initialize transport skip flags-- 
c
      DOAIR = .FALSE.
      DOIRR = .FALSE.
      DOBIOT = .FALSE.
      DOWAST = .FALSE.
c
C     Initialize derived concentration input flags--
c
      DERCRP = .FALSE.
      DERANM = .FALSE.
      DERAQU = .FALSE.
c
      RETURN
C----  End of Subroutine INITNV ----------------------------------------
      END
