C--------------------------------------- VERSION: 24 Feb-98 ----------
C
C      SUBROUTINE EXTCAL(TYR,TPATH)
C
C     This subroutine controls calculation of external dose as well 
C     as dose from inadvertent ingestion of soil.  Exposure from  
C     infinite and finite plumes are calculated.
C
C     For surface water external exposure rates are calculated for
C     contaminated soil at the residential site and for recreational
C     activities.  Internal exposure from inadvertent ingestion of swimming 
C     water and dermal contact with water are also calculated.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     GENII master version: 2-Aug-88 RAPeloquin
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C     Last Modification:   24-Feb-98 BAN
C
C-----------------------------------------------------------------------
C  Parameter Descriptions
C    Name           Description
C  -----------  --------------------------------------------------------------
C     WATCON()  Surface water concentration decayed for time in
C               transit to site, UoA/L 
C  TYR          Current year of analysis
C  TPATH        Transport pathway index: 1-GW, 2-SW, 3-air, 4-soil
C-----------------------------------------------------------------------
C----- Modification History --------------------------------------------------
C    Date    Who  Description of Modification
C  --------- ---  ------------------------------------------------------------
C  20 May 97 DLS  Modified logic so AIREXT selects external pathway and
C                 FINITE selects type of calculation
C  24 Feb 98 BAN  Set all output units to "per kg" from "per m2"
C-----------------------------------------------------------------------------
c
C----- SUBROUTINE CALL STATMENT ----------------------------------------------
C
      SUBROUTINE EXTCAL(TYR,TPATH)
C
C---- INCLUDE FILES ----------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'SWPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
      REAL WATCON(LENCHAIN), TYR
      INTEGER TPATH
C
C---- Calculate exposure from plumes -----------------------------------------
C
        IF (TPATH.EQ.3.AND.AIREXT) THEN
C
          IF (FINITE) THEN
C
C----- Finite plume.  Pass ATO value to output, no calculation. --------------
C
            DO IN = 1, NONUC
              EXPOS(IN,1) = EXTD(IN)
            END DO
C
          ELSE
C
C----- Semi-infinite plume.  Equation 11.13 ----------------------------------
C      
            DO IN = 1, NONUC
              EXPOS(IN,1) = ARPCON(IN) 
            END DO
          ENDIF
        ENDIF
C
C---- Calculate exposure from contaminated soil.  Equation 11.14 -------------
C
      IF (GROUND) THEN
        DO IN = 1, NONUC
          EXPOS(IN,3) = RESAVG(IN) 
        END DO  
C
      ENDIF
C 
C---- Calculate exposure from recreational activities ------------------
C 
      IF (TPATH.EQ.2.AND.RECRE) THEN
           DO IN = 1,NONUC
             WATCON(IN) = SWCON(IN)
           END DO
C
C-------- Calculate sediment concentration.  Eq. 11.21 -----------------------
C 
          IF (TYR .LE. RELEND) THEN
            CALL CHAIN (1.0, DUMMY, WATCON, SEDCON, 1)
            DO IN = 1, NONUC
              SEDCON(IN) = SEDCON(IN) * TCWS / SEDDN
            END DO
          ELSE  ! Release ended, decay previous sed. conc. 1 year.
            CALL CHAIN (TONE, DUMMY, SEDCON, SEDCON, 0)
          ENDIF
          IF (DEBUG) WRITE (NELS,*) 'WATCON, SEDCON: ',WATCON(1),
     .                               SEDCON(1)
C 
C-------- Calculate exposure concentration ---------
C
          DO 110 IN = 1, NONUC
C 
C---------- Swimming exposure pathways --------------------------------------
C
            IF (REC(1)) THEN
              EXPOS(IN,14) = WATCON(IN) ! swimming external. Eq 11.18
            ENDIF
C
C 
C---------- Fishing and shoreline exposure
C
            IF (REC(3)) EXPOS(IN,16) = SEDCON(IN) 
C
  110     CONTINUE
C
      ENDIF
C
      RETURN
c
C---- END OF MODULE EXTCAL --------------------------------------------
c
      END

