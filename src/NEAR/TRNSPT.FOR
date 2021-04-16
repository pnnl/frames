C----------------------------------- Version: 20-Feb-98 ----------------------
C
C     SUBROUTINE TRNSPT (RELFLG)
C
C     This module controls calculation of yearly radionuclide transport 
C     through the environment.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII version: 2-Aug-88  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:  20-Feb-98 BA Napier 
C     
C---- Significant parameters -------------------------------------------------
C
C     RELFGL   - Flag set if during exposure period, false during prior
C                build-up period
C
C---- Modification History ---------------------------------------------------
C   Date      Who   Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  08-Sep-97  DLS  Initial modification from old GENII
C  12-Feb-98  BAN  Conversion to use of annual average soil concs.
C  20-Feb-98  BAN  Relocation of PACKAG call
C
C-----------------------------------------------------------------------------
C  Subroutine call statement
C
      SUBROUTINE TRNSPT (RELFLG)
C
C----- Include statements ----------------------------------------------------
C
      INCLUDE 'RAD.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SWPAR.CMN' 
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C----- Type/dimension statements ---------------------------------------------
C
      LOGICAL RELFLG
C
C---- If this is the first year of the exposure period, do manual ------------
C     redistribution
C
      IF (RELFLG .AND. ITIME .EQ. 1) CALL REDIST
C
C---- Decay soils/buried waste for this year, harvest removal ------------
C
      CALL DKHARV
C  
C---- Do all soils except for C14 and H3 -------------------------------------
C
      IF (.NOT. C14 .AND. .NOT. H3) THEN
        IF (NONAG) THEN
C-------  Non-agricultural soil ----------------------------------------------
          CALL EDRNON
        ELSE
C
C-------  Environmental deposition/removal to residential soil ---------------
C
c          IF (AIREXT .OR. FINITE .OR. GROUND .OR. INHAL .OR. SLING) 
          IF (GROUND .OR. INHAL .OR. SLING .OR. SLDRML) 
     .      CALL EDRRES
C
C-------- Environmental deposition/removal to terrestrial food soils ---------
C
          IF (TFOOD) CALL EDRCRP
C
C-------- Environmental deposition/removal to animal food soils --------------
C
          IF (ANFOOD) CALL EDRANM
        ENDIF
      ENDIF
C
      RETURN
      END
C
C----- End of module TRNSPT --------------------------------------------------
C
