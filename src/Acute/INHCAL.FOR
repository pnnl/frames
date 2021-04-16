C-------------------------------------- Version: 23-Mar-98 -------------------
C
C     SUBROUTINE INHCAL 
C
C     This subroutine calculates inhalation exposure rates--
C
C     Module of ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Original Coding: 6-Apr-88 RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Latest Revision:  23-Mar-98  BAN
C
C----------------------------------------------------------------------------
C
C     1.0E-4   - Resuspension factor at time=0, (m-1), Anspaugh model
C     1.0E-9   - Resuspension factor at time=17 yr, (m-1), Anspaugh model
C     INHCON() - Air concentration to use for inhalation calculations,
C                includes resuspension contribution
C     T        - Real ITIME (d)
C
C----------------------------------------------------------------------------
C   Modification History
C    Date      Who  Modification Description
C  ---------  ---  -----------------------------------------------------------
C  22-Jul-97  DLS  Revised heading and comment information
C  24-Aug-97  DLS  Expanded resuspension factor model to allow user input of
C                  resuspension factor (IRES = 3)
C  23-Mar-98  BAN  Adjusted units, ADDED EXTPAR.CMN
C----------------------------------------------------------------------------
C
      SUBROUTINE INHCAL 
C
C----- Include Statments -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
C      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
      INCLUDE 'EXTPAR.CMN'
C
C----- Type and dimension statements -----------------------------------------
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
C-----  Use Anspaugh Model ---------------------------------------------------
C
        IF (ITIMT .LE. 25) THEN
          T = SQRT (FLOAT (ITIMT) * DAYYR)
          RESFAC = (1.0E-4 * EXP (- 0.15 * T) + 1.0E-9) 
     .             * AVALSL / SURCM
        ELSE
          RESFAC = 1.0E-9 * AVALSL / SURCM
        ENDIF
      ELSE IF(IRES. EQ. 3) THEN
        RESFAC = RESFAC         ! User input of resuspension factor
      ELSE
        RESFAC = 0   ! IRES not equal to 1,2, or 3.  No resuspension factor
      ENDIF
      IF (DEBUG) WRITE (*,*) 'Resuspension factor:',RESFAC
C
C---- Calculate first year acute inhalation ----------------------------------
C
      IF (ITIMT .EQ. 1) THEN
        DO 402 IN = 1, NONUC
C
C-----    Acute portion of exposure ------------------------------------------
C
          ACUTEP = PWTAIR(IN) * YRSEC / ACUTIM
C
C-----    Chronic portion of exposure ----------------------------------------
C
          CHRONP = RESAVG(IN) * RESFAC * SLDN
          IF (DEBUG) WRITE (*,*) 'Acute inhalation (ACUTEP, CHRONP)',
     .      ACUTEP, CHRONP
          EXPOS(IN,2) = ACUTEP 
CCCCCCC    EXPOS(IN,2) = ACUTEP + CHRONP
          EXPOS(IN,28) = CHRONP
	    IF (SHINDR) EXPOS(IN,22) = EXPOS(IN,22) + ACUTEP
      
  402   CONTINUE
      ELSE
C
C----- Subsequent time periods after acute period ----------------------------
C
C-----  Air concentration includes resuspension contribution only ------------
C
        DO 200 IN = 1, NONUC
          INHCON(IN) = RESAVG(IN) * RESFAC
c	    IF (SHINDR) EXPOS(IN,22) = EXPOS(IN,22) + INHCON(IN)
          EXPOS(IN,28) = INHCON(IN) 
          IF (DEBUG) WRITE (*,*) 'Inhalation air:', INHCON(IN)
  200   CONTINUE
C
      ENDIF
      RETURN
C
C---- End of Module INHCAL ---------------------------------------------------
C
      END   
