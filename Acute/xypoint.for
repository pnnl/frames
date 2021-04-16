C    EXPOS: XYPOINT.FOR              Version Date:  10-Sep-97              
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C      SUBROUTINE XYPOINT                                                    *
C                                                                            *
C  Subroutine XYPOINT finds the nearest point to a set od (X,Y) values       *
C  The subroutine returns the number of the closest point.                   *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    10-Sep-97                                               *
C  Last Modified:    10-Sep-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EXPOS  main program
C     Called by: EXPOS
C     Calls: none    
C     Common blocks referenced: none           
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C  EXPXA(1681)   U     Real   AIRINFO    Array of x-axis points
C  EXPYA(1681)   U     Real   AIRINFO    Array of y-axis points
C  NPOINTS       U     Int     Arg       Number of x,y data pairs to evaluate
c  NXY           S     Int     Arg       Number of the nearest data pair.
C     X          S     Real    Arg.      X-Axis distance of cartesian coord.
C     Y          S     Real    Arg.      Y-Axis distance of cartesian coord.
C==== Modification History ===================================================
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  10-Sep-97  DLS  Initial programming started
C     
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE XYPOINT(NPOINTS)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      (None)
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRINFO.CMN'
      INCLUDE 'EXINFO.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      REAL DIST
      INTEGER NPOINTS
C
C---- Start of Analysis ------------------------------------------------------
C
      X = EXPX 
      Y = EXPY 
c      X = EXPX * 1000.
c      Y = EXPY * 1000.
      ONEPOINT = .TRUE.
      DIST = 1000000.
      NXY = 1  ! Default is first location
      IF(X.EQ.0..AND.Y.EQ.0.) GO TO 100
      IF(NPOINTS.GT.1) THEN
         DO IP = 1,NPOINTS
            XX = (EXPXA(IP)-X)**2
            YY = (EXPYA(IP)-Y)**2
            DD = SQRT(XX + YY)
            IF(DD.LT.DIST) THEN
               DIST = DD
               NXY = IP
            ENDIF
         END DO
      END IF
      RETURN
C
 100  ONEPOINT = .FALSE.
      RETURN
C
C----- END OF MODULE XYPOINTS -----------------------------------------------
C
      END

