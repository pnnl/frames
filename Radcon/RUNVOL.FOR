C     MEPAS RADCON: RUNVOL.FOR            Version Date: 04-19-1995
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE RUNVOL                                    *
C                                                                            *
C  Subroutine RUNVOL computes the overland runoff volume adjusted for        *
C  snowmelt.  It is based on equation 4.11 from Whelan et al. 1987.  The     *
C  subroutine is called from the main program.                               *
C                                                                            *
C  Written by:      Gene Whelan                                              *
C                   Battelle Pacific Northwest Laboratories                  *
C                   P.O. Box 999                                             *
C                   Richland, WA  99352                                      *
C                                                                            *
C  Creation Date:   01/19/89 (Converted to PC)                               *
C  Last Modified:   04/19/95 - JPM                                           *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C    Module of: RADCON
C    Called by: RADCONPC
C    Calls to: None
C    Common blocks referenced: None
C
C==== Significant Parameter Designation and Descriptions =====================
C
C    Parameter Set/           Location
C    Name      Used   Type    Export/Import  Parameter Description
C    --------- -----  ------  ------------- ----------------------------------
C    A1        S/U   REAL     LOCAL          INTERMEDIATE VALUE FOR OVERLAND  
C                                            RUNOFF                           
C    A2        S/U   REAL     LOCAL          INTERMEDIATE VALUE FOR OVERLAND  
C                                            RUNOFF                           
C    CN        USED  REAL     ARG - IMPORT   SCS CURVE NUMBER                 
C    PRENUM    USED  REAL     ARG - IMPORT   # OF RAIN EVENTS > 0.01 INCHES   
C    VOL       SET   REAL     ARG - EXPORT   OVERLAND RUNOFF VOLUME           
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     08/22/94     JPM  Corrected mistake in mathematical formulation
C                       (eq 4.11, Whelan et al, 1987).
C     04/19/95     JPM  Removed PRENUM from the denominator of Eq. 4.11.  The
C                       code was correct before the 08/22/94 change. RAD-P061
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RUNVOL (CN,PRENUM,VOL,PREC)
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
      DATA CM / 2.54 /
C
      A1=0.2*CM*PRENUM*(1000.-10*CN)
      A2=PREC*CN
C*****************************************************************************
C     OVERLAND RUNOFF VOLUME CALCULATION USING EQ. 4.11 (WHELAN ET AL. 1987)
C*****************************************************************************
      IF (A2 .GT. A1) VOL=(A2-A1)**2./CN/(A2+4.*A1)
      RETURN
      END
