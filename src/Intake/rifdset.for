C   RIFDSET.FOR INTAKE               Version Date: 09-Jun-97            
C   Copyright 1997 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE RIFDSET                               *
C                                                                            *
C  Subroutine RIFHEAD writes data set heading information and data           *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    09-Jun-97                                               *
C  Last Modified:    11-Oct-01      BAN                                      *
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
C     RCPNAME    U    Char*32  Receptor Name
C     NAGES      U      Int    Number of age groups to be printed
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
C   11-Oct-01      BAN  Changed to use of LOWAGE and UPAGE
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RIFDSET(EXPNAM,MEDTYPE,NPOINTS,NUMNUC,IAGE)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'AGES.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RCPINFO.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
C     LOGICAL 
      INTEGER NUMNUC
      REAL TAGE(6)
      CHARACTER*32 EXPNAM
      CHARACTER*14 MEDTYPE
C
C---- Data Statements --------------------------------------------------------
C
C
C----- For each exposure location: write name, medium, co-ordinates, Number --
C      of constituents (parents)
C
      IF(IAGE.LE.1) THEN
        NE = LENWORD(EXPNAM,32)
        NM = LENWORD(MEDTYPE,14)
        WRITE(NRIF,100) EXPNAM(1:NE),MEDTYPE(1:NM),NPOINTS,NAGES,NUMNUC
 100    FORMAT('"chronic","',A,'","',A,'",',I3,',',I2,',',I2,',')
C
C----- Write coordinates of all exposure points ------------------------------
C
        DO IX = 1,NPOINTS
          WRITE(NRIF,200) EXPX(IX),EXPY(IX)
 200      FORMAT(F9.1,',"km",',F9.1,',"km",')
        END DO
      ENDIF
C
C---- Calculate and print age group break points -----------------------------
C
C      TAGE(1) = 0.0
C      DO IX = 1,NAGES
C        TAGE(IX+1) = TAGE(IX) + EDAGE(IX)
C      END DO
C      WRITE(NRIF,300) (TAGE(IX),IX=IAGE,IAGE+1)
	WRITE(NRIF,300) LOWAGE(IAGE), UPAGE(IAGE)
 300  FORMAT(F5.0,',',5(F5.0,','))
C
      RETURN
C
C----- END OF MODULE RIFDSET ------------------------------------------------
C
      END

