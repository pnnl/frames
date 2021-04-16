C------------------------------------- Version: 08-Sep-97 --------------------
C
C     SUBROUTINE PRIOR
C
C     This subroutine calculates radionuclide decay and buildup prior 
C     to the intake period.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version: 29-Dec-87 RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:  08-Sep-97
C
C-----------------------------------------------------------------------
C
C     IBEFOR   - Integer of BEFORE, yr
C     YRTOGO   - Number of years remaining in inventory decay/buildup
C                period, yr
C
C---- Modification History----------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  08-Sep-97  DLS  Initial modification of old GENII
C---- Subroutine Call Statement ----------------------------------------------
C
      SUBROUTINE PRIOR
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'CONC.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
C
C---- Start of Analysis ------------------------------------------------------
C
C     If non-agricultural scenario, redistribute into food pathway
C     and residential soil compartments, and clear flag--
C
      IF (NONAG) THEN
C
C-----  Residential/resuspension soil ----------------------------------------
C
        DO 124 IN = 1, NONUC
          RESSOL(IN) = NONAGS(IN)
          DSRES(IN) = NONAGD(IN)
  124   CONTINUE
C
C-----  Terrestrial foods soil -----------------------------------------------
C
        IF (TFOOD) THEN
          DO 120 ITF = 1, NTF
            DO 122 IN = 1, NONUC
              IF (TFD(ITF)) THEN
                SOLCON(IN,ITF) = NONAGS(IN)
                DSCON(IN,ITF) = NONAGD(IN)
              ENDIF
  122       CONTINUE
  120     CONTINUE
        ENDIF
C 
C-----  Animal fodder soil ---------------------------------------------------
C
        IF (ANFOOD) THEN
          DO 130 IAN = 1, NAN
            DO 132 IN = 1, NONUC
              IF (ANF(IAN)) THEN
                SL2CON(IN,IAN) = NONAGS(IN)
                DS2CON(IN,IAN) = NONAGD(IN)
	           IF (IAN .EQ. 1) SL2CON(IN,5) = NONAGS(IN)
                 IF (IAN .EQ. 3) SL2CON(IN,6) = NONAGS(IN)
                 IF (IAN .EQ. 1) DS2CON(IN,5) = NONAGD(IN)
                 IF (IAN .EQ. 3) DS2CON(IN,6) = NONAGD(IN)
              ENDIF
  132       CONTINUE
  130     CONTINUE
        ENDIF
C
      ENDIF
C  
      RETURN
      END
C
C---- End of PRIOR Module ----------------------------------------------------
C
