C     MEPAS RADCON: PFLUX.FOR             Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE PFLUX                                *
C                                                                            *
C  Subroutine PFLUX calculates the contaminant flux (F) in the               *
C  longitudinal direction resulting from the release of a point source       *
C  based on equation 5.36 from Whelan et al. 1987.  It is called             *
C  from subroutine FUNCTION.                                                 *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Northwest Laboratory                           *
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
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------ --------------- --------------------------------
C     C3        USED   REAL    ARG - IMPORT   INTERMEDIATE VALUE; SQRT(4*E1)  
C     C6        USED   REAL    ARG - IMPORT   INTERMEDIATE VALUE; U1*T4       
C     C7        USED   REAL    ARG - IMPORT   INTERMEDIATE VALUE; SQRT(T4)    
C     C9        USED   REAL    ARG - IMPORT   INTERMEDIATE VALUE; 1/SQRT(4*PI)
C     F         SET    REAL    ARG - EXPORT   CONTAMINANT FLUX IN X-DIRECTION 
C     W1        USED   REAL    ARG - IMPORT
C     X         USED   REAL    ARG - IMPORT   X-DIRECTION DISTANCE            
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE PFLUX (X,C3,C6,C7,C9,ARG,F,UNITSS,W1)
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
      F=ALOG((X+C6)*C9/C3/C7**3.)
      F1=ALOG(UNITSS)
      F2=ALOG(W1)
      F=F+F1+F2
      F=EXPF(F-(((X-C6)/C3/C7)**2.+ARG))
      RETURN
      END
