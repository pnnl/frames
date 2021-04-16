C     MEPAS RADCON: YLINE.FOR             Version Date: 05-07-1996            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE YLINE                                 *
C                                                                            *
C  Subroutine YLINE computes the line source solution of contaminant         *
C  concentration in the y-direction for an aquifer of infinite               *
C  width based on equation 5.20 from Whelan et al. 1987.  It is              *
C  called from subroutine FUNCTION (function FUN).                           *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    05/07/96 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE FUNCTION
C     Calls to: None
C     Common blocks referenced: None
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ----- ------  --------------- --------------------------------
C     B2        USED  REAL     ARG - IMPORT   LENGTH OF SOURCE                
C     C2        USED  REAL     ARG - IMPORT   INTERM PARAMETER; = SQRT (4*E2) 
C     C7        USED  REAL     ARG - IMPORT   INTERM PARAMETER; = SQRT (T4)   
C     G2        SET   REAL     ARG - EXPORT   LINE SOURCE SOLUTION            
C     Y2        USED  REAL     ARG - IMPORT   Y DISTANCE                      
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     05/07/96     JPM  Removed IFLAG - not allowing a switch to the y point
C                       source solution (RAD-P062)
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE YLINE (B2,Y2,C2,C7,G2)
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
      X5=(B2+Y2)/(C2*C7)
      X6=(B2-Y2)/(C2*C7)
      A=ERFF(X5,ONE1,ERFC1)
      B=ERFF(X6,ONE2,ERFC2)
      DONE=ONE1+ONE2
      DERFC=ERFC1+ERFC2
      DERF=DONE+DERFC
C*****************************************************************************
C     LINE SOURCE SOLUTION IN THE Y-DIRECTION USING EQUATION 5.20             
C     FROM WHELAN ET AL. 1987                                                 
C*****************************************************************************
      G2=DERF*0.25/B2
      RETURN
      END
