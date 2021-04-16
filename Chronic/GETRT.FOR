C    EXPOS: GETRT.FOR                Version Date:  7-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C      SUBROUTINE GETRT                                                      *
C                                                                            *
C  Subroutine GETRT converts cartesian coordinate (X,Y) to polar (R,THETA)   *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:     7-May-97                                               *
C  Last Modified:     7-May-97      DLS                                      *
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
C     X          U     Real    Arg.      X-Axis distance of cartesian coord.
C     Y          U     Real    Arg.      Y-Axis distance of cartesian coord.
C     R          S     Real    Arg.      Radius of polar coordinate
C     THETA      S     Real    Arg.      Angle o polar coordinate
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     7-May-97      DLS  Initial programming started
C     
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE GETRT(X,Y,R,THETA)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C
      INCLUDE 'DEVICE.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      REAL R,THETA,X,Y
C
C---- Data Statements --------------------------------------------------------
C
      DATA PI/3.14159265/
c
C---- Polar axis for Theta = 0 is the positive X-axis -------------------------
C
C---- Check x,y arguments to be sure they are not both zero -------------------
C
      R = 0.0
      THETA = 0.0
      IF(X.EQ.0..AND.Y.EQ.0.) THEN
        WRITE(NERR,100) X,Y
 100    FORMAT(' Error in GETRT, both X and Y arguments are zero: X,Y'
     .   ,1p2e10.2)
        NERROR = NERROR + 1
      ELSE
C    R value is sqrt(x**2 + y**2)
C    THETA value arctan(x,y)
C    Theta in radians is converted to degrees by multiplying by 360 / 2pi  
C
        R = SQRT(X*X + Y*Y)
        T = ATAN2(X,Y)
        THETA = T * 360. / (2 * PI)
        IF(THETA.LT.0) THETA = 360. + THETA
      ENDIF
C
        RETURN
C
C----- END OF MODULE GETXY --------------------------------------------------
C
      END

