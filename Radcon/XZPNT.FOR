C     MEPAS RADCON: XZPNT.FOR             Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                         SUBROUTINE XZPNT                                   *
C                                                                            *
C  Subroutine XZPNT computes the point source solution in the y-direction    *
C  for an aquifer of infinite width based on eq. 5.17 (Whelan et al. 1987).  *
C  It also is used to calculate the source solution in the z-direction.      *
C  Subroutine XZPNT is called from subroutine FUNCTION (function FUN).       *
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
C     Called by: SUBROUTINE FUNCTION
C     Calls to: None
C     Common blocks referenced: None
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter  Set/           Location
C     Name       Used   Type    Export/Import  Parameter Description
C     --------- ------ -------- -------------- -------------------------------
C     C6         USED  REAL     ARG - IMPORT   INTERM VARIABLE; = U1 * T4     
C     E          USED  REAL     ARG - IMPORT   DISPERSION; ALSO KNOWN AS E1   
C     G1         SET   REAL     ARG - EXPORT   POINT-SOURCE SOLUTION          
C     G2         SET   REAL     LOCAL          INTERMEDIATE VALUE             
C     T          USED  REAL     ARG - IMPORT   TIME INTERVAL BETWEEN UPPER AND
C                                              LOWER CONVOLUTION INTEGRAL     
C                                              LIMITS; ALSO KNOWN AS T4       
C     W1         USED  REAL     ARG - IMPORT   INTERPOLATED VALUE             
C     X          USED  REAL     ARG - IMPORT   AQUIFER THICKNESS; ALSO = H1   
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE XZPNT (X,T,E,ARG,C6,C7,C9,G1,UNITSS,W1,ALPHA)
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
C*****************************************************************************
C  INTERMEDIATE CALCULATIONS PRIOR TO USING EQ. 5.17 (WHELAN ET AL. 1987)
C*****************************************************************************
      G1=0.
      G2=(X-C6)**2./(4.*E*T)+ARG
      F1=ALOG(C9/C7/SQRT(E))
      F2=ALOG(UNITSS)
      F3=ALOG(W1)
      F4=ALOG(ALPHA)
C*****************************************************************************
C  POINT SOURCE SOLUTION BASED ON EQUATION 5.17 FROM WHELAN ET AL. 1987
C*****************************************************************************
      G1=EXPF(F1+F2+F3+F4-G2)
      RETURN
      END
