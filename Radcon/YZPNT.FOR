C     MEPAS RADCON: YZPNT.FOR             Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE YZPNT                                     *
C                                                                            *
C  Subroutine YZPNT computes the point source solution in the y-direction    *
C  for an aquifer of finite width based on eq. 5.19 (Whelan et al. 1987).    *
C  It is called from subroutine FUNCTION (function FUN).                     *
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
C     C7         USED   REAL    ARG - IMPORT   INTERM PARAMETER; =SQRT(T4)    
C     C9         USED   REAL    ARG - IMPORT   INTERM PARAMETER; =1/SQRT(4*PI)
C     E          USED   REAL    ARG - IMPORT   Y-DIRECTION DISPERSIVITY,      
C                                              ALSO CALLED E2                 
C     G2         SET    REAL    ARG - EXPORT   POINT SOURCE SOLUTION          
C     T          USED   REAL    ARG - IMPORT   TIME, ALSO KNOWN AS T4         
C     X          USED   REAL    ARG - IMPORT   Y DISTANCE, ALSO CALLED Y2     
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE YZPNT (X,T,E,C7,C9,G2)
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
      G2=0.
      G1=X**2./(4.*E*T)
C*****************************************************************************
C  POINT SOURCE SOLUTION IN Y DIRECTION USING EQ. 5.19 (WHELAN ET AL. 1987)
C*****************************************************************************
      G2=C9/C7/SQRT(E)*EXPF(-G1)
      RETURN
      END
