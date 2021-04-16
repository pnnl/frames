C--------------------------------------- VERSION: 24-Feb-98 ----------------
C
C     SUBROUTINE INHCAL 
C
C     This subroutine calculates inhalation exposure rates--
C
C     Module of ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     GENII Master Version: RAP
C     Last Modification: 24-Feb-98 BAN
C     Reviewed and Approved: **********  BA Napier
C     
C----------------------------------------------------------------------------
C
C     1.0E-4   - Resuspension factor at time=0, (m-1), Anspaugh model
C     1.0E-9   - Resuspension factor at time=17 yr, (m-1), Anspaugh model
C     ARPCON() - Air concentration to use for inhalation calculations,
C     T        - Real ITIME (d)
C
C----------------------------------------------------------------------------
C     Modification History
C    24-Feb-98  BAN  Units correction on EXPOS
C
      SUBROUTINE INHCAL 
C
C----- Include Statements ---------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C----- Type and dimension statements -----------------------------------------
C
      REAL T
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
C
C------ Use Anspaugh Model ---------------------------------------------------
C
      ELSEIF (IRES .EQ. 2) THEN
        IF (ITIMT .LE. 25) THEN
          T = SQRT (FLOAT (ITIMT) * DAYYR)
          RESFAC = (1.0E-4 * EXP (- 0.15 * T) + 1.0E-9) 
     .             * AVALSL / SURCM
        ELSE
          RESFAC = 1.0E-9 * AVALSL / SURCM
        ENDIF
C
C----- No Resuspension -------------------------------------------------------
C
      ELSE IF(IRES. EQ. 3) THEN
        RESFAC = RESFAC     ! Input of user value for resuspension factor
      ELSE
        RESFAC = 0          ! Value for IRES .NE. 1,2, or 3, no resuspension
      ENDIF
C
      IF (DEBUG) WRITE (NELS,*) 'Resuspension factor:',RESFAC
C
C---- Calculate inhalation from air and resuspension -----------------------
C
        DO IN = 1, NONUC
          EXPOS(IN,2) = ARPCON(IN) 
	    IF (SHINDR) EXPOS(IN,22) = EXPOS(IN,22) + ARPCON(IN)
          EXPOS(IN,28) = RESAVG(IN) * RESFAC * SLDN
        END DO  
C
      RETURN
C
C---- End of Module INHCAL -------------------------------------------
C
      END

