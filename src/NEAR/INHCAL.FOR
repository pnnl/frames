C------------------------------------ Version: 12-Feb-98----------------------
C
C     SUBROUTINE INHCAL 
C
C     This subroutine calculates inhalation exposure rates--
C
C     Module of ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII Version: 6-Apr-88 RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:     12-Feb-98  BAN        
C-----------------------------------------------------------------------
C
C     1.0E-4   - Resuspension factor at time=0, (m-1), Anspaugh model
C     1.0E-9   - Resuspension factor at time=17 yr, (m-1), Anspaugh model
C     INHCON() - Air concentration to use for inhalation calculations,
C                includes resuspension contribution
C     T        - Real ITIME (d)
C
C---- Modification History ---------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  -----------------------------------------------------------
C  09-Sep-97  DLS  Initial modification from old GENII
C  12-Feb-98  BAN  Conversion to general use of annual average soils
C-----------------------------------------------------------------------------
C
      SUBROUTINE INHCAL 
C
C-----------------------------------------------------------------------------
C
      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C-----------------------------------------------------------------------------
C
      REAL INHCON(9), T
C
C---- Calculate resuspension factor ------------------------------------------
C
      IF (IRES .EQ. 1) THEN
C
C-----  Use Mass Loading Model; calculate only once --------------------------
C
        IF (ITIMT .EQ. 1 .OR. ITIME .EQ. 1) THEN
          RESFAC = XMLF / SLDN * KGG
        ENDIF
      ELSEIF (IRES .EQ. 2) THEN
C
C------ Use Anspaugh Model ---------------------------------------------------
C
        IF (ITIMT .LE. 25) THEN
          T = SQRT (FLOAT (ITIMT) * DAYYR)
          RESFAC = (1.0E-4 * EXP (- 0.15 * T) + 1.0E-9) 
     .             * AVALSL / SURCM
        ELSE
          RESFAC = 1.0E-9 * AVALSL / SURCM
        ENDIF
      ELSEIF (IRES.EQ.3) THEN
C
C----- Use input value given by user -----------------------------------------
C
        RESFAC = RESFAC
      ELSE
C
C----- No resuspension -------------------------------------------------------
C
        RESFAC = 0
      ENDIF
C
      IF (DEBUG) WRITE (*,*) 'Resuspension factor:',RESFAC
C     
C---- Chronic exposure -------------------------------------------------
C       Air concentration includes resuspension contribution--
C
        DO 200 IN = 1, NONUC
          INHCON(IN) = RESAVG(IN) * RESFAC * SLDN
          IF (DEBUG) WRITE (*,*) 'Inhalation air:', INHCON(IN)
  200   CONTINUE
        DO 100 IN = 1, NONUC
          EXPOS(IN,28) = INHCON(IN)
  100   CONTINUE
      RETURN
      END
C
C---- End of Module INHCAL ---------------------------------------------------
C
