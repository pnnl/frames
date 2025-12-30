C   EPFRCP.FOR EXPOS                Version Date: 09-Jun-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EPFRCP                                *
C                                                                            *
C  Subroutine EPFRCP reads a data set heading line
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    09-Jun-97                                               *
C  Last Modified:    09-Jun-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/INTAKE
C     Called by: INTAKE
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     RCPNAM     U    Char*32  Receptor Location Name
C     NPOINTS    U      Int    Number of exposure points
C     NUMNUC     U      Int    Number of radionuclide parents
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   07-Jan-97      DLS  Initial programming started
C   10-Jan-97      DLS  Added write line for atmospheric and added "points" to
C                       line for groundwater and surface water write.
C   06-May-97      DLS  Deleted "points" and "polar" from data set 1st line.
C   15-May-97      DLS  Revised printing of points for AIR releases
C   27 July 2002   BAN  Replaced .eq. with SEQI in several places
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFRCP(RCPNAM,MEDTYPE,NPOINTS,NUMNUC,FOUNDR)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RCPINFO.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NUMNUC, NPOINTS
      CHARACTER*32 RCPNAM,RCPINP
      CHARACTER*14 AQUIF,SWATR,AIRNM,SOILNM,MEDTYPE
      CHARACTER*12 C
      REAL T
      LOGICAL FOUNDR, SEQI
C
C---- Data Statements --------------------------------------------------------
C
      DATA AQUIF  /'Aquifer       '/
      DATA SWATR  /'Surface Water '/
      DATA AIRNM  /'Air           '/
      DATA SOILNM /'Soil          '/
C      DATA GRID/'grid  '/,POINTS/'points'/
C
C---- Initialize flags -------------------------------------------------------
C
      AIR = .FALSE.
      AQUIFR = .FALSE.
      SURFWAT = .FALSE.
      SOILS = .FALSE.
C
C---- Read first line of data set --------------------------------------------
C
      READ(NEPF,*) RCPINP,MEDTYPE,NPOINTS,NUMNUC
C
C---- Test input receptor name against desired name --------------------------
C
c      IF(RCPINP.EQ.RCPNAM) THEN
      IF(SEQI(RCPINP,RCPNAM,4)) THEN
        FOUNDR = .TRUE.
C
C---- If air, set flag -------------------------------------------------------
C
C        IF(MEDTYPE.EQ.AIRNM) THEN
        IF(SEQI(MEDTYPE,AIRNM,3)) THEN
          AIR = .TRUE.
C
C---- If ground water transport, set flag ------------------------------------
C
C        ELSE IF(MEDTYPE.EQ.AQUIF) THEN
        ELSE IF(SEQI(MEDTYPE,AQUIF,7)) THEN
          AQUIFR = .TRUE.
C
C---- If surface water transport, set flag -----------------------------------
C
C        ELSE IF(MEDTYPE.EQ.SWATR) THEN
        ELSE IF(SEQI(MEDTYPE,SWATR,13)) THEN
          SURFWAT = .TRUE.
C
C---- If soils, set flag -----------------------------------------------------
C
C        ELSE IF(MEDTYPE.EQ.SOILNM) THEN
        ELSE IF(SEQI(MEDTYPE,SOILNM,4)) THEN
          SOILS = .TRUE.
C
        ENDIF
C
      ELSE  ! Read past data set
C
C----- Read coordinate point data 
C
        DO IN = 1,NPOINTS
          READ(NEPF,*) X
        END DO
        X = X
C
C----- For each constituent read data
C
        DO IN = 1,NUMNUC
          READ(NEPF,*) C,C,NPROG,NTPER
            DO IT = 1,NTPER
              READ(NEPF,*) T,C,T,C,NEXP
                DO IX = 1,NEXP
                  READ(NEPF,*) C
                  READ(NEPF,*) T
                END DO
              DO IP = 1,NPROG
                READ(NEPF,*) C,C,NEXP
                DO IX = 1,NEXP
                  READ(NEPF,*) C
                  READ(NEPF,*) T
                END DO
              END DO
            END DO
        END DO
        C = C
        T = T
      ENDIF
C
      IF(FOUNDR) THEN
        DO IX = 1,NPOINTS
          READ(NEPF,*) EXPX(IX),C,EXPY(IX)
        END DO
      ENDIF

      RETURN
C
C----- END OF MODULE EPFMED -------------------------------------------------
C
      END

