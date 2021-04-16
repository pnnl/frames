C------------------------------------ Version: 12-Feb-98 ---------------------
C
C     SUBROUTINE EXTCAL
C
C     This subroutine controls calculation of external dose as well 
C     as dose from inadvertent ingestion of soil.  Exposure from  
C     infinite and finite plumes are calculated.
C
C     The surface water is decayed for time in transit and then
C     external exposure rates are calculated for contaminated soil
C     at the residential site and for recreational activities.
C     Internal exposure from inadvertent ingestion of swimming 
C     water in also calculated.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System
C
C     Initial GENII Version: 2-Aug-88 RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last Modification:     12-Feb-98  BAN      
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
C---- Modification History----------------------------------------------------
C    Date     Who  Modification Summary
C  ---------  ---  ------------------------------------------------------------
C  09-Sep-97  DLS  Initial modification from old GENII
C  03-Feb-98  DLS  Calculate average soil concentration over the current 1-year
C                  period.
C  12-Feb-98  BAN  Take out last change - not needed in conversion to annual
C                  average soil concs.
C-----------------------------------------------------------------------------
C
      SUBROUTINE EXTCAL
C
C----- Include Statements ----------------------------------------------------
C
C     INCLUDE 'AFPPAR.CMN'
C     INCLUDE 'AIRPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
C     INCLUDE 'SWPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Type/Dimension Statements ----------------------------------------------
C
      REAL AREAEX, HECTAR
      LOGICAL FIRST
C
C---- Data Statements --------------------------------------------------------
C
      DATA HECTAR /10000.0/, FIRST /.TRUE./
C
C---- Calculate exposure from contaminated soil ------------------------
C
      IF (GROUND) THEN
C
C----  For first time, calculate effective area parameter --------------------
C
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
C---- If the area parameter is not 1.0, then write value to listing file -----
C
          IF (AREAEX .NE. 1.0) THEN
            WRITE (NELS, *) 
     .        'Source area external dose modification factor: ', AREAEX
            FIRST = .FALSE.
          ENDIF
        ENDIF
C
C---- Calculate concentration for external exposure from all 3 compartments --
C
        DO 120 IN = 1, NONUC
            EXPOS(IN,3) = RESAVG(IN) * AREAEX
C            IF (DEEP) EXPOS(IN,22) = DSRavg(IN) * AREAEX 
C            IF (BURWAS) EXPOS(IN,23) = QWAS(IN) * AREAEX
  120   CONTINUE
C
      ENDIF
C
C---- Calculate exposure from soil ingestion ---------------------------------
C
      IF (SLING) THEN
        DO 132 IN = 1, NONUC
          EXPOS(IN,12) = RESAVG(IN) 
  132   CONTINUE
      ENDIF
      RETURN
      END
C
C---- End of Module EXTCAL ---------------------------------------------------
C
