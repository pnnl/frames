C   EPFDSET.FOR ACUTE                Version Date: 31-Mar-98               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EPFDSET                               *
C                                                                            *
C  Subroutine EPFHEAD writes data set heading information and data           *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    07-Jan-97                                               *
C  Last Modified:    31-Mar-98   BAN                                     *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/ACUTE
C     Called by: ACUTE
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     TPATH      U      Int    Transport pathway type: 1-GW, 2-SW, 3-air, 4-soil                                                                   
C     EXPNAME    U    Char*32  Exposure Location Name
C     NSETS      U      Int    Number of exposure data sets to be printed
C     NPOINTS    U      Int    Number of exposure points
C     NUMNUC     U      Int    Number of radionuclide parents
C==== Modification History ===================================================
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  07-Jan-97  DLS  Initial programming started
C  10-Jan-97  DLS  Added write line for atmospheric and added "points" to
C                  line for groundwater and surface water write.
C  06-May-97  DLS  Deleted "points" and "polar" from data set 1st line.
C  15-May-97  DLS  Revised printing of points for AIR releases
C  29-Jul-97  DLS  Heading and comments revised for use with ACUTE
C  31-Mar-98  BAN  Removed X-Y coordinate printing for air - its in EPFGRID
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFDSET(TPATH,EXPNAM,NPOINTS,NUMNUC)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRINFO.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXINFO.CMN'
      INCLUDE 'FLUX.CMN'
      INCLUDE 'OPT.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NUMNUC, TPATH
      CHARACTER*32 EXPNAM
      CHARACTER*7 AQUIF
      CHARACTER*13 SWATR
      CHARACTER*3 AIRNM
      CHARACTER*4 SOILNM
C
C---- Data Statements --------------------------------------------------------
C
      DATA AQUIF  /'Aquifer'/
      DATA SWATR  /'Surface Water'/
      DATA AIRNM  /'Air'/
      DATA SOILNM /'Soil'/
C
C---- Write first line of data set: number of locations and chronic ----------
C
      IF(TPATH.NE.3) NPOINTS = 1
C
C----- For each exposure location: write name, medium, co-ordinates, Number --
C      of constituents (parents)
C
      NE = LENWORD(EXPNAM,32)
C
C---- Groundwater transport --------------------------------------------------
C
      IF(TPATH.EQ.1) WRITE(NEPF,200) EXPNAM(1:NE),AQUIF,NPOINTS,NUMNUC
 200  FORMAT('"acute","',A,'","',A,'",',I3,',',I2,',')
C
C---- Surface water transport ------------------------------------------------
C
      IF(TPATH.EQ.2) WRITE(NEPF,200) EXPNAM(1:NE),SWATR,NPOINTS,NUMNUC
C
C---- Atmospheric transport --------------------------------------------------
C
      IF(TPATH.EQ.3) THEN
        WRITE(NEPF,200) EXPNAM(1:NE),AIRNM,NPOINTS,NUMNUC
      ENDIF
C
      IF(TPATH.EQ.4) WRITE(NEPF,200) EXPNAM(1:NE),SOILNM,NPOINTS,NUMNUC
C
C---- Write X,Y-coordinate data ----------------------------------------------
C
      IF(TPATH.NE.3) THEN            !Write exposure location coordinates
        WRITE(NEPF,400) EXPX,EXPY    ! for all except air.  One point
 400    FORMAT(F9.3,',"km",',F9.3,',"km",')
C      ELSE        ! For air, loop on number of points
C        DO IX = 1,NPOINTS
C          WRITE(NEPF,400) EXPXA(IX),EXPYA(IX)
C        END DO
      END IF
      RETURN
      END
C
C----- END OF MODULE EPFDSET ------------------------------------------------
C
