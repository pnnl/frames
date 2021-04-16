C-------------------------------------- VERSION: 28-May-1997 ---------
C
C     SUBROUTINE DRKCAL 
C
C
C     This subroutine calculates drinking water uptake rates--
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 28-May-97 DLS
C     Reviewed and Approved: 12-Sept-88  BA Napier
C
C-----------------------------------------------------------------------
C
C     WATCON()  - Drinking water concentration, units of activity/L
C     TYR       - Decay time in years
C
C-----------------------------------------------------------------------------
C     Modification History
C    Date     Who  Description of Modification
C  ---------  ---  -----------------------------------------------------------
C  28-May-97  DLS  Pathways added for indoor inhalaton from volatilization
C                  from water, dermal contact while showering, and ingestion
C                  while showering
C-----------------------------------------------------------------------------
C
      SUBROUTINE DRKCAL 
C
C--- Include Statements
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AQUPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
C
C---- Type and dimension statements ------------------------------------------
C
      REAL WATCON(LENCHAIN), TYR, AIRVAP(LENCHAIN)
C
C----- Start of Analysis -----------------------------------------------------
C   Loop on radionuclides in decay chain
C
        DO 100 IN = 1, NONUC
c
C-----    Determine drinking water source--
c
          IF (DWSRC .EQ. 1) THEN
            WATCON(IN) = GWCON(IN)
          ELSEIF (DWSRC .EQ. 2) THEN 
            WATCON(IN) = SWCON(IN)
          ELSE
            WATCON(IN) = 0.0
          ENDIF
C
C-----    Apply drinking water cleanup factor--
C
          IF (DWTRET)
     .      WATCON(IN) = WATCON(IN) * DWCLEN(IN)

  100   CONTINUE
c
C----- Calculate decay and ingrowth during transit time and holdup  ---
c      until consumption
c
        TYR = HOLDDW * YRDA
        CALL CHAIN (TYR, DUMMY, WATCON, WATCON, 0)  
C
C---- Calculate indoor air concentration from release from water -------------
C
        IF(SHINDR) THEN
	    do 150 in=1,nonuc
          AIRVAP(IN) = WATCON(IN) * ANDFAC(IN)
150	    continue
        ENDIF
C
C---- Set Water Concentration Array for domestic water uses ------------------
C
      DO 200 IN = 1, NONUC
        EXPOS(IN,17) = WATCON(IN) 
        EXPOS(IN,23) = WATCON(IN)
        EXPOS(IN,24) = WATCON(IN)
        EXPOS(IN,22) = EXPOS(IN,22) + AIRVAP(IN)
  200 CONTINUE

      RETURN
C----------------------------------------------------------------------
      END   
