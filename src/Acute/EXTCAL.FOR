C-------------------------------------- Version: 24-Mar-98 -------------------
C
C     SUBROUTINE EXTCAL
C
C     This subroutine controls calculation of external dose pathways
C     and inadvertent ingestion of soil.  Exposure from  
C     infinite and finite plumes are calculated.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     Initial Version:   2-Aug-88 RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last modification:  24-Mar-98 BAN
C-----------------------------------------------------------------------
C
C     T         - Length of time sediment is exposed to contaminated
C                 water also transit time to recreational site, yr
C     WATCON()  - Surface water concentration decayed for time in
C                 transit to site, UoA/L 
C     AREAEX    - Source area external dose modification factor, 
C                 dimensionless
C     HECTAR    - Hectare to m2 correction factor, m2.
C     FIRST     - Logical cleared after first pass of logic.  
C
C------------------------------------------------------------------------------
C   Modification History
C    Date     Who  Modification Description 
C  ---------  ---  -----------------------------------------------------------
C  22-Jul-97  DLS  Revised heading and comments information
C  04-Feb-98  DLS  Added conversion of sediment from 1/m2 to 1/kg SEDDN
C  23-Mar-98  BAN  Standardized integral soil nomenclature, units
C  24-Mar-98  BAN  added new pathways
C------------------------------------------------------------------------------
C
      SUBROUTINE EXTCAL
C
C---- Include Statements ------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
C      INCLUDE 'AFPPAR.CMN'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'SWPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C----- Type and Dimension Statements -----------------------------------------
C
      REAL T, WATCON(CHAINLEN), AREAEX, HECTAR, SEDAVG(9)
      LOGICAL FIRST
C
C----- Data Statements -------------------------------------------------------
C
      DATA HECTAR /10000.0/, FIRST /.TRUE./, T/1.0/
C
C---- Calculate exposure from plumes -----------------------------------
C
        IF (ITIMT .EQ. 1) THEN
          DO 404 IN = 1, NONUC
            IF (FINITE) THEN
              EXPOS(IN,1) = PWTXAR(IN) 
c             EXPOS(IN,1) = PWTXAR(IN) * FRCLOD * POPT
            ELSEIF (AIREXT) THEN
              EXPOS(IN,1) = PWTAIR(IN) * YRSEC / ACUTIM ! Divide by acute period
c             EXPOS(IN,1) = PWTAIR(IN) * YRSEC * FRCLOD * POPT
            ENDIF
  404     CONTINUE
        ENDIF
C
C---- Calculate exposure from contaminated soil ------------------------
C
      IF (GROUND) THEN
        IF (FIRST) THEN 
          IF (FRSIZ .GE. 1250.0 .OR. FRSIZ .EQ. 0.0) THEN
            AREAEX = 1.0
          ELSEIF (FRSIZ .GE. 500.0) THEN
            AREAEX = 2.7 * FRSIZ / HECTAR + 0.67
          ELSEIF (FRSIZ .GE. 100.0) THEN
            AREAEX = 6.5 * FRSIZ / HECTAR + 0.48
          ELSEIF (FRSIZ .GE. 25.0) THEN
            AREAEX = 20.0 * FRSIZ / HECTAR + 0.35
          ELSE
            AREAEX = 160.0 * FRSIZ / HECTAR 
          ENDIF
C
          IF (AREAEX .NE. 1.0) THEN
            WRITE (NELS, *) 
     .        'Source area external dose modification factor: ', AREAEX
          ENDIF
         FIRST = .FALSE.
        ENDIF
C
        DO 120 IN = 1, NONUC
          EXPOS(IN,3) = RESAVG(IN) / T    ! Divide by 1 year chronic period
  120   CONTINUE

      ENDIF
C 
C---- Calculate exposure from recreational activities ------------------
C 
      IF (RECRE) THEN
C
C----- Set water concentration for surface water -----------------------------
C
          DO IN = 1,NONUC
            WATCON(IN) = SWACUT(IN)
          END DO
C
C-----    Calculate sediment concentration -----------------------------------
C
          IF (T .LE. RELEND) THEN
            DO 101 IN = 1, NONUC
              SEDCON(IN) = WATCON(IN) * TCWS / SEDDN
  101       CONTINUE
            CALL CHAIN(1.0,DUMMY,SEDCON,SEDAVG,1)
          ELSE
C    Calculate average sediment concentration over the current 1 year period
c    (The time integral is divided by 1 year (not needed in equation) )
            CALL CHAIN (1.0, DUMMY, SEDCON, SEDCON, 0)
            CALL CHAIN(1.0,DUMMY,SEDCON,SEDAVG,1)
          ENDIF
C
C-----    Calculate exposure concentration -----------------------------------
C
             DO 112 IN = 1, NONUC
C
C-----        Swimming exposure Concentration --------------------------------
C
             IF (REC(1)) THEN
                EXPOS(IN,14) = WATCON(IN) / ACUTIM    ! Divide by acute period
C
C------         Inadvertent swimming water ingestion -------------------------
C
                EXPOS(IN,13) = WATCON(IN) / ACUTIM    ! Divide by acute period
C
C------         Swimming dermal-----------------------------------------------
C
                IF (SWDRML) EXPOS(IN,26) = WATCON(IN) / ACUTIM
              ENDIF
C
C-----        Boating exposure -----------------------------------------------
C 
              IF (REC(2)) EXPOS(IN,15) = WATCON(IN)/ ACUTIM  ! Divide by acute period
C
C-----        Fishing and shoreline exposure ---------------------------------
C
              IF (REC(3)) EXPOS(IN,16) = SEDAVG(IN) ! 1 year average chronic period
              IF (SRDRML) EXPOS(IN,27) = SEDAVG(IN)
  112       CONTINUE
      ENDIF
C
C---- Calculate exposure from soil ingestion----------------------------
C
      IF (SLING) THEN
        DO 132 IN = 1, NONUC
          EXPOS(IN,12) = RESAVG(IN)
  132   CONTINUE
      ENDIF
C
C---- Calculate exposure from soil dermal contact ----------------------
C
      IF (SLDRML) THEN
        DO IN = 1, NONUC
          EXPOS(IN,25) = RESAVG(IN)
        END DO
      ENDIF
      RETURN
C
C----- End of Module EXTCAL --------------------------------------------------
C
      END

