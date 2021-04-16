C   MEPAS RADCON: AFLUX.FOR             Version Date: 11-24-1991              
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.      
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE AFLUX                                 *
C                                                                            *
C  Subroutine AFLUX calculates the contaminant flux associated with an area  *
C  source.  It uses equation 5.37 of Whelan et al. 1987 to calculate the     *
C  contaminant flux (F).  Before the calculations begin however, the         *
C  subroutine first checks to see if the source should be considered to be a *
C  point source instead of an area source.  AFLUX is called from subroutine  *
C  FUNCTION.                                                                 *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    11/24/91 - JWB                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     Called by: SUBROUTINE FUNCTION
C     Calls: FUNCTION ERFF
C     Common blocks referenced: None
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     A         S/U    REAL    LOCAL     PART A OF ERROR FUNCTION RESULT
C     B         S/U    REAL    LOCAL     PART B OF ERROR FUNCTION RESULT
C     C3        USED   REAL    ARG-EXP   INTERMEDIATE PARAMETER; = SQRT(4*E1)
C     C4        USED   REAL    AGR-EXP   INTERMEDIATE PARAMETER; = X2 + AL2
C     C5        USED   REAL    AGR-EXP   INTERMEDIATE PARAMETER; = X2 - AL2
C     C6        USED   REAL    ARG-EXP   INTERMEDIATE PARAMETER; = U1 * T4
C     C7        USED   REAL    ARG-EXP   INTERMEDIATE PARAMETER; = SQRT(T4)
C     F         SET    REAL    ARG-EXP   TEMPORALLY DISTRIBUTED CONTAMINANT
C     IFLAG     SET    INTEGER ARG-EXP   INDEX ON AVERAGING; = 6, DEFAULTS
C                                        FROM AREA SOURCE TO POINT SOURCE
C     SDT       S/U    REAL    LOCAL     SUM OF C3 AND C7
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     03/24/89     JWB  Modified error function calculations
C     11/24/91     JWB  Added required comments and header for MEPAS QA plan
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE AFLUX (C3,C4,C5,C6,C7,U1,E1,T4,AL2,ARG,IFLAG,F, 
     1 UNITSS,W1) 
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
      SDT=C3*C7
      Z1=(C4-C6)/SDT
      Z2=(C5-C6)/SDT
C*****************************************************************************
C IF THE ARGUMENT OF THE ERROR FUNCTION IS GREATER THAN TWO, USE THE         *
C POINT SOURCE FLUX FUNCTION (IFLAG = 6)                                     *
C*****************************************************************************
      A=ERFF(Z1,ONE1,ERFC1)
      B=ERFF(Z2,ONE2,ERFC2)
      DONE=ONE1-ONE2
      DERFC=ERFC1-ERFC2
      DERF=DONE+DERFC
C*****************************************************************************
C  THESE CHANGES REFLECT A PROBLEM WHEN TWO NUMBER THAT ARE CLOSE TO 1.0      
C  LIKE ERROR FUNCTIONS ARE SUBTRACTED FROM EACH OTHER GIVING AN INACCURATE   
C  ANSWER.  THIS CHANGE AVOIDS THESE INACCURACIES. JWB 3-24-89                
C
C  EQUATION 5.37 FROM WHELAN ET AL. 1987 CALCULATES THE TEMPORALLY DISTRIBUTED
C  CONTAMINANT FLUX IN THE LONGITUDINAL DIRECTION RESULTING FROM AN INSTANT   
C  RELEASE OF AN AREA SOURCE.                                                 
C*****************************************************************************
C     F=(U1*(ERFF(Z1)-ERFF(Z2))+SQRT(E1/(3.1415926*T4))*                      
C    1 (EXPF(-Z2**2)-EXPF(-Z1**2)))/(4*AL2)*EXPF(-ARG)                        
C*****************************************************************************
      F=(U1*DERF+SQRT(E1/(3.1415926*T4))*
     1 (EXPF(-Z2**2)-EXPF(-Z1**2)))/(4*AL2)*EXPF(-ARG)
      F=F*UNITSS*W1
C******************************************************************************
C     CHECK TO SEE IF SOURCE SHOULD BE CONSIDERED A POINT SOURCE INSTEAD 
C******************************************************************************
      IF (F .LT. 0.) IFLAG=6
      RETURN
      END
