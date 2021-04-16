C     MEPAS RADCON: XLINE.FOR             Version Date: 05-07-1996            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE XLINE                                *
C                                                                            *
C  Subroutine XLINE calculates the line source solution in the x-direction   *
C  with flow in the x-direction based on equation 5.18 (Whelan et al. 1987). *
C  It is called from subroutine FUNCTION (function FUN).                     *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    05/07/96 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  SUBROUTINE FUNCTION
C     Calls to: None
C     Common blocks referenced: None
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter  Set/           Location
C     Name       Used   Type    Export/Import  Parameter Description
C     --------- ------ -------  -------------- -------------------------------
C     C3         USED   REAL    ARG - IMPORT   INTERM PARAMETER; = SQRT(4*E1) 
C     C4         USED   REAL    ARG - IMPORT   INTERM PARAMETER; = X2 + AL2   
C     C5         USED   REAL    ARG - IMPORT   INTERM PARAMETER; = X2 - AL2   
C     C6         USED   REAL    ARG - IMPORT   INTERM PARAMETER; = U1 * T4    
C     C7         USED   REAL    ARG - IMPORT   INTERM PARAMETER; = SQRT(T4)   
C     G1         SET    REAL    ARG - EXPORT   LINE SOURCE SOLUTION           
C     W1         USED   REAL    ARG - IMPORT   INTERPOLATED VALUE OF FLUX     
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     03/24/89     JWB  Subtraction of ERROR Functions modified               
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     11/14/92     JWB  Modified error function for X-point source            
C     05/07/96     JPM  Removed IFLAG - not allowing a switch to the x point
C                       source solution (RAD-P062)
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE XLINE (AL2,C3,C4,C5,C6,C7,ARG,G1,UNITSS,W1,ALPHA)
C
C==== Common Block Definitions ===============================================
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
      C1=0.25/AL2
      X5=(C4-C6)/(C3*C7)
      X8=(C5-C6)/(C3*C7)
      A=ERFF(X5,ONE1,ERFC1)
      B=ERFF(X8,ONE2,ERFC2)
      DONE=ONE1-ONE2
      DERFC=ERFC1-ERFC2
      DERF=DONE+DERFC
C*****************************************************************************
C     THESE CHANGES REFLECT A PROBLEM WHEN TWO NUMBER THAT ARE CLOSE TO 1.0
C     LIKE ERROR FUNCTIONS ARE SUBTRACTED FROM EACH OTHER GIVING AN INACCURATE
C     ANSWER.  THIS CHANGE AVOIDS THESE INACCURACIES. JWB 3-24-89
C
C     E5=ERFF(X5)
C     E6=ERFF(X8)
C     G1=C1*(E5-E6)*EXPF(-ARG)
C*****************************************************************************
      G1=C1*DERF*EXPF(-ARG)
      G1=UNITSS*W1*ALPHA*G1
      RETURN
      END
