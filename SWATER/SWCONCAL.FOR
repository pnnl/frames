C   SWCONCAL.FOR                       Version Date: 27-Feb-97               *
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.     *
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SWCONCAL                              *
C                                                                            *
C  Subroutine SWCONCAL controls evaluation of water concentration at a       *
C  location for one decay chain for release to a river or lake               *
C                                                                            *
C  Written by:       Dennis Strenge                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    31-Oct-1996                                             *
C  Last Modified:    27-Feb-1997  DLS                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER
C     Called by: SWATER
C     Calls: NONE
C     Common blocks referenced: SWPAR, SWINFO, SWCON
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C  27-Feb-97  DLS  Added common block DEVICE for error message unit, NERR     
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SWCONCAL(IL,IX)
C
C==== COMMON Block Definitions ===============================================
C
        include 'DEVICE.CMN'
        include 'SWPAR.CMN'
        INCLUDE 'SWINFO.CMN'
        INCLUDE 'SWCON.CMN'
C       INCLUDE 'DECAY.CMN'
C
C==== DIMENSION Statements ===================================================
C
C     NONE
C 
C==== Variable Declarations ==================================================
C
      CHARACTER*15  RIVER,NSLAKE
      CHARACTER*7 ACUTE,CHRONIC
C
C==== DATA Statements ========================================================
C
      DATA ACUTE/'Acute  '/,   CHRONIC/'Chronic'/
      DATA RIVER/'River          '/,  NSLAKE/'Near-shore lake'/
C
C---- Test for acute or chronic surface water model --------------------------
C
      IF(SWTYPE.EQ.ACUTE) THEN
        CALL SWACUTE(IL,IX)
      ELSE IF (SWTYPE.EQ.CHRONIC) THEN
C
C---- Calculate water chronic dispersion factor ------------------------------
C
        IF(SWMODL.EQ.RIVER.OR.SWMODL.EQ.NSLAKE) THEN
          CALL SWCHRON(IL)
        ELSE
C           
C-----  Write error message, water body type is not river or near-shore lake
C
      WRITE(NERR,2001) SWMODL
2001  FORMAT(' Error in SWCHRON.  Water model is not river or lake: ',
     .       A15)

        ENDIF
      ELSE
C
C----- Write error message for release type ----------------------------------
C
        WRITE(NERR,2000) SWTYPE
 2000   FORMAT(' Error in SWCONCAL.  Release type unknown :',A7)
      ENDIF
 99   RETURN
      END
C
C====================== END OF MODULE SWCONCAL ===========================

