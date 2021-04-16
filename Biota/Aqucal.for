C--------------------------------------- VERSION: 28-May-1997 ----------
C
C     SUBROUTINE AQUCAL 
C
C     This subroutine calculates human uptake rate from consumption of
C     aquatic food.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 28-May-97 RAP
C     Reviewed and Approved:             BA Napier
C
C-----------------------------------------------------------------------------
C Parameter Descriptions
C     TYR      - Decay time in years
C
C-----------------------------------------------------------------------------
C
      SUBROUTINE AQUCAL 
C
C--- Include Statements ------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'AQUPAR.CMN' 
      INCLUDE 'OPT.CMN'
      INCLUDE 'TIMES.CMN'
C
C----- Small fix to align order of codes with order of bioaccumulation library
	DIMENSION INDX(4)
	DATA INDX/1,3,2,4/
C
C----- Start of Analysis -----------------------------------------------------
C
      DO 100 IAQ = 1, NAQ
        IF (AQF(IAQ) .OR. (IAQ .EQ. 1)) THEN
            DO IN = 1, NONUC
              AQUCON(IN,IAQ) = SWCON(IN) * BIOACF(INDX(IAQ),IN) 
            END DO  
c
            IF (DEBUG) WRITE (NELS,*) 'IAN, SWCON, AQUCON:',
     .        IAQ, SWCON(1), AQUCON(1,IAQ)
C
C----- Decay for holdup time -------------------------------------------------
C
           T = HLDUP2(IAQ) * YRDA
           CALL CHAIN ( T, DUMMY, AQUCON(1,IAQ), AQUCON(1,IAQ), 0)
C
C----- Exposure from consumption of aquatic foods ----------------------------
C
          DO IN = 1, NONUC
            EXPOS(IN,17+IAQ) = AQUCON(IN,IAQ)
          END DO  
        ENDIF
  100 CONTINUE
      RETURN
C
C----- END OF MODULE AQUCAL --------------------------------------------------
C
      END

