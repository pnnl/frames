C     MEPAS RADCON: MOUNDING.FOR          Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C  THIS SUBROUTINE CONSIDERS THE EFFECT OF GROUND-WATER MOUNDING ON          *
C  GROUND-WATER VELOCITY AND LATERAL HYDRAULIC SPREADING.  THE CODE          *
C  COMPUTES AN EFFECTIVE HYDRAULIC GRADIENT BETWEEN THE SOURCE AND THE       *
C  SINK.  THE LATERAL SPREADING FACTOR IS BASED ON A STREAMLINE APPROACH AT  *
C  THE 1 SIGMA CONFIDENCE LEVEL.  MATHEMATICAL DEVELOPMENT BASED ON          *
C  OBERLANDER AND NELSON, 1984, GROUNDWATER, V22, NO4.                       *
C                                                                            *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    11/26/91 - JWB                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to: None
C     Common blocks referenced: None
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location  
C     Name      Used   Type    Export/Import  Parameter Name
C     --------- ----- ------   -------------- --------------------------------

C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MOUND (VLEACH,AL1,B1,AN5,X2,U,HC,H1)
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      HFLUX=VLEACH*AL1*B1*AN5
      RADIUS=(AL1+B1)/4
      AA=X2/RADIUS
      ZMOUND=HFLUX*ALOG(AA)/(2*H1*HC*3.14159)
      IF(ZMOUND.GT.H1)ZMOUND=H1
      HG=U*AN5/HC
      GAMA=ZMOUND/(HG*X2*ALOG(AA))
      EFFHG=HG/(1-RADIUS/X2-GAMA*ALOG((1+GAMA)/(RADIUS/X2+GAMA)))
      UZ=HC*EFFHG/AN5
      WRITE (6,9) U
   9  FORMAT (' Regional Groundwater Velocity (U)',T37,'=',E10.2,
     1        ' cm/day')
      IF(UZ.GT.U) U=UZ
      YDIST=X2*2*3.14159*GAMA*0.863/(1+GAMA)
      WRITE (6,3) UZ
      WRITE (6,6) YDIST
   3  FORMAT (' Effective Groundwater Velocity (UZ)',T37,'=',
     1        E10.2,' cm/day')
   6  FORMAT (' Lateral Hydraulic Spreading for a Sigma Interval',
     1        ' (YDIST) = ',E10.2,' cm')
      RETURN
      END
