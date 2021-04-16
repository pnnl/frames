C----------------------------------------------------------------------------
C
C      SUBROUTINE INITNV
C
C     This module initializes parameter values for each radionuclide
C     chain.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial coding:  12-Aug-88  RAP
C     Reviewed and Approved:
C     Last Revision:  23-Mar-98  BAN
C
C------Date-------Who-----Description------------------------------------
C      23-Mar-98  BAN     Clean up for ACUTE use
C
C----- Subroutine Call --------------------------------------------------
C
      SUBROUTINE INITNV
C
C----- Include Statements ----------------------------------------------------
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
c
      DO 200 IN = 1, CHAINLEN
        RESSOL(IN) = 0.0
        GWCON(IN) = 0.0
        SWCON(IN) = 0.0
        ARPCON(IN) = 0.0
        SEDCON(IN) = 0.0
        RTENV(IN) = 0.0
C
        DO 202 I = 1, 4
          SOLCON(IN,I) = 0.0
          ARFCON(IN,I) = 0.0
          AQUCON(IN,I) = 0.0
          PLTCON(IN,I) = 0.0
          ANMCON(IN,I) = 0.0
          HARVT(IN,I) = 0.0
  202   CONTINUE

        DO 204 I = 1, 6
          ARF2CN(IN,I) = 0.0
          SL2CON(IN,I) = 0.0  
          HARVA(IN,I) = 0.0
  204   CONTINUE
        
        DO 102 IP = 1, NPATH
          EXPOS(IN,IP) = 0.0
          DO 104 IY = 1, NTIME
            EXPOST(IY,IN,IP) = 0.0
  104     CONTINUE
  102   CONTINUE

  200 CONTINUE

C
C     Set over-all time index (covers both prior and release periods)--
      ITIMT = 0

C     Initialize transport skip flags-- 
      DOAIR = .FALSE.
      DOIRR = .FALSE.
      DOBIOT = .FALSE.
      DOWAST = .FALSE.


      RETURN
C-----------------------------------------------------------------------
      END

