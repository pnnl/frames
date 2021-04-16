C-------------------------------- Version: 25-Feb-1998 -----------------
C
      SUBROUTINE EDRRES
C
C     This subroutine calculates radionuclide depositon/removal to
C     residential/resuspension soil for a single radionuclide decay chain.  
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 25-Feb-1998
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C-----------------------------------------------------------------------
C 
C     RTATM    - Atmospheric removal rate, UoA/m2-yr
C     RTENV()  - Total environmental removal rate, UoA/m2-yr, for each
C                member of the current radionuclide chain
C     RTIRR    - Irrigation removal rate, UoA/m2-yr
C                chain member, UoA/L
C     WATCON() - Irrigation water concentration, source may be either 
C                ground or surface water, UoA/L
C     ARPCON() - Air concentration, UoA/m3
C     WETDEP() - Wet deposition rate, Bq/m2/yr
C     DRYDEP() - Dry deposition rate, Bq/m2/yr
C-----------------------------------------------------------------------
C    MODIFICATION HISTORY
C 25-Feb-98   BAN  Convert to Bq/kg from Bq/m2
C-----------------------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONC.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN' 
C
      REAL RTIRR, RTATM, WATCON
      REAL RESOUT(LENCHAIN)
C
      DO 500 IN = 1, NONUC
C
C----- Irrigation deposition -------------------------------------------------
C
        RTIRR = 0.0
        IF (RESIRR .AND. DOIRR) THEN 
          WATCON = SWCON(IN)
          RTIRR = WATCON * RIRRR * LM2IN 
        ENDIF
C
C----- Atmospheric deposition ------------------------------------------------
C
        RTATM = 0.0
        IF (DOAIR) RTATM = WETDEP(IN) + DRYDEP(IN)
C
C----- Sum environmental deposition/contribution rates -----------------------
C
        RTENV(IN) = ( RTIRR + RTATM ) / SLDN
C
  500 CONTINUE
C
C---- Integrate over one year--
C
C---- First calculate average concentration during year -----
C
      CALL CHAIN(ONEYR, LEACHR, RTENV, RESOUT, 2)
      DO IN = 1,NONUC
        RESAVG(IN) = RESAVG(IN) + RESOUT(IN)/ONEYR
      END DO
C
C---- Second calculate concentration at end of year for next period ----------
C
      CALL CHAIN (ONEYR, LEACHR, RTENV, RTENV, 1)
C
C----- Add total integrations to residential soils ---------------------------
C      (For start of next year)
C
      DO IN = 1, NONUC
        RESSOL(IN) = RESSOL(IN) + RTENV(IN)
      END DO
C
      RETURN
C----------------------------------------------------------------------
      END   

