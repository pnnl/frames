C------------------------------------- Version: 24-mAR-98 -------------------
C
C     SUBROUTINE DRKCAL 
C
C     This subroutine calculates DOMESTIC water-related concentrations
C     for acute exposure
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial Version: 13-Jan-88 RAP
C     Reviewed and Approved: 12-Sept-88  BA Napier
C     Last modificatin:  24 Mar-98  BAN
C
C-----------------------------------------------------------------------
C
C     WATCON()  - Drinking water concentration, units of activity/L
C     TYR       - Decay time in years
C
C-----------------------------------------------------------------------------
C   Modification History
C    Date     Who  Modification Description
C  ---------  ---  -----------------------------------------------------------
C  22-Jul-97  DLS  Revised heading and comment information
C  24-Mar-98  BAN  added 3 new pathways
C-----------------------------------------------------------------------------
C
      SUBROUTINE DRKCAL 
C
C----- Include statments -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AQUPAR.CMN'
      INCLUDE 'CONC.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'TIMES.CMN'
C
C----- Type and Dimension Statments ------------------------------------------
C
      REAL WATCON(9), TYR
C
C----- Start analysis --------------------------------------------------------
C
        DO IN = 1, NONUC
C
C----- Set drinking water source concentration -------------------------------
C
            WATCON(IN) = SWACUT(IN) 
C
C-----  Apply drinking water cleanup factor ----------------------------------
C
          IF (DWTRET) WATCON(IN) = WATCON(IN) * DWCLEN(IN)
        END DO  ! end radionuclide loop for seting water concentration
C
C-----  Transit time and holdup until consumption ----------------------------
C
        TYR = HOLDDW * YRDA      
        CALL CHAIN (TYR, DUMMY, WATCON, WATCON, 0)  
C
C---- Set water concentration for domestic water pathways---------------------
C
      DO IN = 1, NONUC
C---   Drinking water
        IF (DRINK) EXPOS(IN,17) = WATCON(IN) / ACUTIM
C---Indoor inhalation of volatiles
        IF (SHINDR) EXPOS(IN, 22) = WATCON(IN) * ANDFAC(IN) / ACUTIM
C---Showering dermal
        IF (SHDRML) EXPOS(IN, 23) = WATCON(IN) / ACUTIM
C---Showering ingestion
        IF (SHING) EXPOS(IN, 24) = WATCON(IN) / ACUTIM
      END DO
C
      RETURN
C----- End of Module DRKCAL --------------------------------------------------
      END   
