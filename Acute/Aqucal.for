C-------------------------------------- Version: 22-Jul-97 -------------------
C
C     SUBROUTINE AQUCAL 
C
C     This subroutine calculates human uptake rate from consumption of
C     aquatic food.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 9-Sep-1999  BAN
C     Reviewed and Approved:   BA Napier
C
C-----------------------------------------------------------------------
C
C     SWDK()   - Surface water concentration, decayed for transit, 
C                UoA/L
C     TYR      - Decay time in years
C
C-----------------------------------------------------------------------------
C   Modification History
C    Date     Who  Modification Description
C  ---------  ---  -----------------------------------------------------------
C  22-Jul-97  DLS  Revised heading and comment information
C   9-Sep-99  BAN  Adjusted index of mollusc/crustacean to match BIOAC1
C
C-----------------------------------------------------------------------------
C
      SUBROUTINE AQUCAL
C
C---- Include Statments ------------------------------------------------------
C    
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'CONC.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'AQUPAR.CMN' 
      INCLUDE 'OPT.CMN'
C      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C----- Type and dimension statements-----------------------------------------
C
      REAL SWDK(CHAINLEN), TYR
	DIMENSION INDX(4)
C
C----- small fix to rearrange pathways to fit bioaccumulation library
	DATA INDX/1,3,2,4/
C
C----- Start of Analysis -----------------------------------------------------
C
      DO 100 IAQ = 1, NAQ
        IF (AQF(IAQ)) THEN
          IF (.NOT. DERAQU) THEN
C
C-----      Decay surface water for time in transit --------------------------
C
            TYR = AQUTT(IAQ) * YRHR
              CALL CHAIN (TYR, DUMMY, SWACUT, SWDK, 0)
C
C-----      Calculate aquatic foods concentrations ---------------------------
C
            DO 110 IN = 1, NONUC
              AQUCON(IN,IAQ) = SWDK(IN) * BIOACF(INDX(IAQ),IN) 
  110       CONTINUE
            IF (DEBUG) WRITE (*,*) 'IAN, SWDK, AQUCON:',
     .        IAQ, SWDK(1), AQUCON(1,IAQ)
          ENDIF
C
C-----    Decay for holdup time ----------------------------------------------
C
            T = HLDUP2(IAQ) * YRDA
            CALL CHAIN ( T, DUMMY, AQUCON(1,IAQ), AQUCON(1,IAQ), 0)
C
C-----    Exposure from consumption of aquatic foods -------------------------
C
          DO 120 IN = 1, NONUC
            EXPOS(IN,17+IAQ) = AQUCON(IN,IAQ) 
c           EXPOS(IN,17+IAQ) = AQUCON(IN,IAQ) 
c    .                         * USAG(IAQ) * AQUPOP * ADJETQ(IAQ)
  120     CONTINUE
        ENDIF
  100 CONTINUE
      RETURN
C
C---- End of Module AQUCAL ---------------------------------------------------
C
      END
