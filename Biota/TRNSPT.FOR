C---------------------------------- Version: 24 Feb 1998 -------------
C
      SUBROUTINE TRNSPT (RELFLG,TPATH)
C
C     This module controls calculation of yearly radionuclide transport 
C     through the environment.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 24-Feb-98  BAN
C     Reviewed and Approved:             BA Napier
C     
C-----------------------------------------------------------------------
C   Modification History
C   24 Feb 98   BAN   Move calls to CRPCAL and ANMCAL to end
C-----------------------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'RAD.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'EXPALL.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
      LOGICAL RELFLG
      INTEGER TPATH
c
      CALL DKHARV
C
C---- Do all soils except for C14 and H3 -------------------------------------
C
      IF (.NOT. C14 .AND. .NOT. H3) THEN
C
C----- Environmental deposition/removal to residential soil ------------------
C
          IF (GROUND .OR. SLING) THEN
            CALL EDRRES
          ENDIF
C
C----  Environmental deposition/removal to terrestrial food soils ------------
C
          IF (TFOOD) THEN
            CALL EDRCRP
          ENDIF
C
C----- Environmental deposition/removal to animal food soils -----------------
C
          IF (ANFOOD .or. tfood) THEN
            CALL EDRANM
          ENDIF
C
      ENDIF
C
C----- Deal with Harvest removal in prior buildup period----------------------
C
      IF(TFOOD.AND..NOT.RELFLG) CALL CRPCAL(RELFLG,TPATH)
      IF((ANFOOD .or. tfood).AND..NOT.RELFLG) CALL ANMCAL(RELFLG,TPATH)
C
      RETURN
C
C----- END OF SUBROUTINE TRNSPT ----------------------------------------------
C
      END

