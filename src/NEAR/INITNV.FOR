C------------------------------------- Version: 12 Feb 98 -------------------
C
c     SUBROUTINE INITNV
C
C     This module initializes parameter values for each radionuclide
C     chain.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     NEAR FIELD MODULE
C     Initial GENII Version: 12-Aug-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification: 12-Feb-98 BAN
C
C---- Call Statement --------------------------------------------------
C
      SUBROUTINE INITNV
C
C---- Include Statements -----------------------------------------------
C
      INCLUDE 'ANMPAR.CMN'
C      INCLUDE 'AQUPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
      DO 200 IN = 1, 9
C       QAIR(IN) = 0.0
C       QSW(IN) = 0.0
        QWAS(IN) = 0.0
        NONAGS(IN) = 0.0
        RESSOL(IN) = 0.0
        RESAVG(IN) = 0.0
        NONAGD(IN) = 0.0
        NONDAV(IN) = 0.0
        DSRES(IN) = 0.0
        DSRAVG(IN) = 0.0
C       GWCON(IN) = 0.0
C       SWCON(IN) = 0.0
C       AIRCON(IN) = 0.0
C       ARPCON(IN) = 0.0
C       SEDCON(IN) = 0.0
C       DWCON(IN) = 0.0
        RTAVAL(IN) = 0.0
        PAKADD(IN) = 0.0
        RTENV(IN) = 0.0
C
        DO 202 I = 1, 4
          SOLCON(IN,I) = 0.0
          SOLAVG(IN,I) = 0.0
          DSCON(IN,I) = 0.0
          DSCAVG(IN,I) = 0.0
          ARFCON(IN,I) = 0.0
C         AQUCON(IN,I) = 0.0
          PLTCON(IN,I) = 0.0
          ANMCON(IN,I) = 0.0
          HARVT(IN,I,1) = 0.0
          HARVT(IN,I,2) = 0.0
  202   CONTINUE
C
        DO 204 I = 1, 6
C         ARF2CN(IN,I) = 0.0
          SL2CON(IN,I) = 0.0
          SL2AVG(IN,I) = 0.0
          DS2CON(IN,I) = 0.0
          DS2AVG(IN,I) = 0.0
          HARVA(IN,I,1) = 0.0
          HARVA(IN,I,2) = 0.0
  204   CONTINUE
C       
        DO 102 IP = 1, NPATH
          EXPOS(IN,IP) = 0.0
          DO 104 IY = 1, NTIME
            EXPOST(IY,IN,IP) = 0.0
  104     CONTINUE
  102   CONTINUE
C
  200 CONTINUE
C
C---- Set over-all time index (covers both prior and release periods) --------
C
      ITIMT = 0
C
C---- Initialize transport skip flags ----------------------------------------
C
C     DOAIR = .FALSE.
C     DOIRR = .FALSE.
      DOBIOT = .FALSE.
      DOWAST = .FALSE.
      EXCAMT = 0.
C
C---- Initialize derived concentration input flags ---------------------------
C
C     DERCRP = .FALSE.
C     DERANM = .FALSE.
C     DERAQU = .FALSE.
C     DERDRK = .FALSE.
C
      RETURN
      END
C
C---- End of Module INITNV ---------------------------------------------------
C



