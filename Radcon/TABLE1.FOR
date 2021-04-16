C     MEPAS RADCON: TABLE1.FOR            Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE TABLE1                                    *
C                                                                            *
C  Subroutine TABLE1 identifies the proper soil moisture curves              *
C  (tables) for computing soil moisture and for interpolating between        *
C  curves.  The parameter KFLAG identifies the table with the largest        *
C  of the two soil capacities for the interpolation.  Subroutine             *
C  TABLE1 is called from subroutine LEACH1.                                  *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    11/26/91 - JWB                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE LEACH1
C     Calls to: None
C     Common blocks referenced: None
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ----- -------- -------------- --------------------------------
C     AVAILW    USED   REAL    ARG - EXPORT   AVAILABLE WATER, REASSIGNED IF  
C                                             IF GREATER THAN SOIL STORAGE    
C     FRACT     SET    REAL    ARG - EXPORT   INTERPOLATED VALUE OF AVAILW    
C     KFLAG     SET    INTEGER ARG - EXPORT   INDEX FLAG FOR MONTH            
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE TABLE1 (KFLAG,FRACT,SLOP,YINTER,AVAILW)
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION SOILST(11),SLOP(11),YINTER(11)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
      DATA SOILST / 2.5,5.,7.5,10.,12.5,15.,20.,25.,30.,35.,40. /
C
C*****************************************************************************
C     THE TABLES ARE IN UNITS OF CM
C*****************************************************************************
      IF (AVAILW .GT. SOILST(1)) GO TO 50
      AVAILW=SOILST(1)
      KFLAG=2
      FRACT=0.
      GO TO 40
C
   50 IF (AVAILW .LT. SOILST(11)) GO TO 30
      AVAILW=SOILST(11)
      KFLAG=11
      FRACT=1.
      GO TO 40
C
   30 DO 10 I=2,11
      KFLAG=I
      IF (AVAILW .LT. SOILST(I)) GO TO 20
   10 CONTINUE
C*****************************************************************************
C     INTERPOLATION PROCEDURE
C*****************************************************************************
   20 FRACT=SOILST(KFLAG)-SOILST(KFLAG-1)
      FRACT=(AVAILW-SOILST(KFLAG-1))/FRACT
   40 CONTINUE
C
      RETURN
      END
