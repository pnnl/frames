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
C    26-OCT-98      BAN  MOVE READ OF ACUTE/CHRONIC TO HERE
C    13-Sept-04    BAN  COmmented out check of RCPINP versus RCPNAM
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFRCP(ACUTE,NUMEXP,RCPNAM,MEDTYPE,NPOINTS,NUMNUC,
     .FOUNDR)
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
      CHARACTER*14 AQUIF,SWATR,AIRNM,SOILNM,MEDTYPE,RELTYPE
      CHARACTER*12 C
      REAL T
      LOGICAL FOUNDR, ACUTE, SEQI
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
      READ(NEPF,*) RELTYPE,RCPINP,MEDTYPE,NPOINTS,NUMNUC
C
C---- Determine exposure type ------------------------------------------------
C
      ACUTE = .FALSE.
C      IF(RELTYPE.EQ."acute") THEN
      IF(SEQI(RELTYPE,"acute",5)) THEN
        WRITE(NRLS,100) NUMEXP,RELTYPE
 100    FORMAT(' There is (are)',I3,' data set(s) for type ',a7)
        ACUTE = .TRUE.
C      ELSEIF(RELTYPE.EQ."chronic") THEN
      ELSEIF(SEQI(RELTYPE,"chronic",7)) THEN
        WRITE(NRLS,100) NUMEXP,RELTYPE                          
      ELSE
        WRITE(NERR,1000) RELTYPE
 1000  FORMAT(' Exposure type not recognized for EPF data set:',a14)
        NERROR = NERROR + 1
      ENDIF
C
C---- Test input receptor name against desired name --------------------------
C---This may be redundant (happens in MARKIN) - comment out for now---BAN-----
c      IF(RCPINP.EQ.RCPNAM) THEN

        FOUNDR = .TRUE.
C
C---- If air, set flag -------------------------------------------------------
C
c        IF(MEDTYPE.EQ.AIRNM) THEN
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
c        ELSE IF(MEDTYPE.EQ.SWATR) THEN
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
cc      ELSE  ! Read past data set
C
C----- Read coordinate point data 
C
cc        DO IN = 1,NPOINTS
cc         READ(NEPF,*) X
cc        END DO
cc        X = X
C
C----- For each constituent read data
C
cc        DO IN = 1,NUMNUC
cc          READ(NEPF,*) C,C,NPROG,NTPER
cc            DO IT = 1,NTPER
cc              READ(NEPF,*) T,C,T,C,NEXP
cc                DO IX = 1,NEXP
cc                  READ(NEPF,*) C
cc                  READ(NEPF,*) T
cc                END DO
cc              DO IP = 1,NPROG
cc                READ(NEPF,*) C,C,NEXP
cc                DO IX = 1,NEXP
cc                  READ(NEPF,*) C
cc                  READ(NEPF,*) T
cc                END DO
cc              END DO
cc            END DO
cc        END DO
cc        C = C
cc        T = T
cc      ENDIF
C
      IF(FOUNDR) THEN
        DO IX = 1,NPOINTS
          READ(NEPF,*) EXPX(IX),C,EXPY(IX)
        END DO
      ENDIF

      RETURN
C
C----- END OF MODULE EPFRCP -------------------------------------------------
C
      END

