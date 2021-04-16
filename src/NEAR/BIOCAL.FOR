C------------------------------------ Version: 13-Nov-97 ---------------------
C
C     SUBROUTINE BIOCAL
C
C
C     This subroutine pre-calculates biotic transport parameters.
C
C     Module of ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Version of 26-Aug-87  RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C----- Modification History --------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  09-Sep-97  DLS  Initial modification of old GENII version
C  13-Nov-97  DLS  Revised setting of BTDSET to use all options, add IPRE
C-----------------------------------------------------------------------------
C
      SUBROUTINE BIOCAL(IPRE)
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'SOLPAR.CMN' 
      INCLUDE 'OPT.CMN'
C
C---- Set depth index based on overburden thickness --------------------------
C
      IF (OVRBRD .LE. 0.15) THEN
        INDW = 1
      ELSEIF (OVRBRD .LE. 0.5) THEN
        INDW = 2
      ELSEIF (OVRBRD .LE. 1.0) THEN
        INDW = 3
      ELSEIF (OVRBRD .LE. 1.5) THEN
        INDW = 4 
      ELSEIF (OVRBRD .LE. 2.0) THEN
        INDW = 5
      ELSE
        INDW = 6
      ENDIF
C
C---- Set index for biotic transport land use type, BTDSET -------------------
C
      IF(IPRE.LE.0) THEN    ! Pre-intake period
         IF(ARID) THEN      
            BTDSET = 1      ! Arid during pre-intake period
         ELSE
            BTDSET = 2      ! Wet during pre-intake period
         ENDIF
      ELSE            ! Intake period
         BTDSET = 3   ! Agricultural conditions always druing intake period
      ENDIF
C
C---- Calculate quantity of soil brought to the surface by animals -----------
C
      EXCAMT = EXCAV(INDW,BTDSET) * TOTEXC(BTDSET)
C    
      IF (DEBUG) WRITE (*,*) 'Biotic TR (INDW, BTDSET, EXCAMT):',
     .  INDW, BTDSET, EXCAMT
C
      RETURN
      END 
C
C---- End of Module BIOCAL ---------------------------------------------------
C
