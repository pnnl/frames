C------------------------------------ Version: 09-Sep-97 ---------------------
C
C    SUBROUTINE EXPOSR (ISEAS)
C
C     This module controls environmental exposure calculations for a 
C     radionuclide chain.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version:  30-Aug-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:     09-Sept-97  DL Strenge
C-----------------------------------------------------------------------------
C
C     ISEAS    - Season index for acute cases
C
C---- Modification History ---------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  09-Sep-97  DLS  Initial modification of old GENII
C-----------------------------------------------------------------------------
C
      SUBROUTINE EXPOSR ()
C
C-----------------------------------------------------------------------------
C
      INCLUDE 'CONC.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'TIMES.CMN'
C
C----- Type/dimension Statements ---------------------------------------------
C
C
C----- Start of Analysis -----------------------------------------------------
C
C   Calculate uptake rate from terrestrial foods, animal products ------------
C
        IF (C14 .OR. H3) THEN
          CALL CANDH
        ELSE
          IF (TFOOD) CALL CRPCAL
          IF (ANFOOD) CALL ANMCAL
        ENDIF
C
C---- Calculate inhalation rate ----------------------------------------------
C
      IF (SLINH) CALL INHCAL
C
C---- Calculate external exposure rates and inadvertent soil ingestion -------
C
      IF (GROUND .OR. SLING .OR. SLDRML)
C     IF (AIREXT .OR. FINITE .OR. GROUND .OR. SLING .OR. RECRE)
     .  CALL EXTCAL
C
C---- Transfer to exposure rate array ----------------------------------------
C
      DO 201 IP = 1, NPATH
        DO 202 IN = 1, NONUC
          EXPOST(ITIME,IN,IP) = EXPOS(IN,IP)
  202   CONTINUE
  201 CONTINUE
C
      RETURN
      END
C
C---- End of Module EXPOSR ---------------------------------------------------
C
