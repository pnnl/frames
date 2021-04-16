C    EXPOS: GETXY.FOR                Version Date:  7-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C      SUBROUTINE GETXY                                                      *
C                                                                            *
C  Subroutine GETXY converts polar coordinate (R,THETA) to cartexian (X,Y)   *
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
C     R          U     Real    Arg.      Radius of polar coordinate
C     THETA      U     Real    Arg.      Angle o polar coordinate
C     X          S     Real    Arg.      X-Axis distance of cartesian coord.
C     Y          S     Real    Arg.      Y-Axis distance of cartesian coord.
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     7-May-97      DLS  Initial programming started
C     
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE GETXY(R,THETA,X,Y)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      (None)
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
C    X value is radius * cosine(theta)
C    Y value is radius * sine(theta)
C    Theta in degrees is converted to radians by multiplying by 2 pi / 360 
C
       TH = 450. - THETA
       IF(TH.GE.360.) TH = TH - 360.
       T = TH * 2 * PI / 360.
       X = R*COS(T)
       Y = R*SIN(T)
C
        RETURN
C
C----- END OF MODULE GETXY --------------------------------------------------
C
      END

