C     MEPAS RADCON: VAPORP.FOR            Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                       FUNCTION VAPORP                                      *
C                                                                            *
C  Function VAPORP calculates the saturated vapor pressure at mean           *
C  air temperature based on equation 3.9 from Whelan et al. 1987.            *
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
C     TMEAN     USED  REAL     EXP/IMP        MEAN ANNUAL TEMPERATURE         
C     VAPORP    USED  REAL     LOCAL          SATURATED VAPOR PRESSURE        
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== FUNCTION CALL ==========================================================
C
      FUNCTION VAPORP (TMEAN)
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
C     SATURATED VAPOR PRESSURE USING EQ 3.9 FROM WHELAN ET AL. 1987
C*****************************************************************************
      VAPORP=33.8639*((7.38E-03*TMEAN+0.8072)**8.-1.9E-05*ABS(1.8*
     1       TMEAN+48.)+1.316E-03)
      RETURN
      END
