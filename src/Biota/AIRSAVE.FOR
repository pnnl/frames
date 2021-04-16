C     
C  AIRSAVE.FOR  EXPOS                Version Date: 6-May-98  PWE               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE AIRSAVE                               *
C                                                                            *
C  Subroutine AIRSAVE puts values into arrays for input from the ATO file,   *
C          for interpolation over time
C                                                                            *
C  Written by:       DL Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    24-Jun-97                                               *
C  Last Modified:    6-May-98  PWE                                           *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: ATODAT
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     ARRAY     S/U    REAL    Argument  Array of parameter values for output
C     VTIN       U     REAL    Argument  Array of input values
C     ICM        U     INT     Argument  Position of chain member of interest
C     NX1        U     INT     Argument  Number of positions to be filled
C     IX2        U     INT     Argument  Position of data for second co-ordinate
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   24-Jun-97      DLS  Initial programming started
C   6-May-98       BAN/PWE  Updated to 41x41
C   7-JUN-01       BAN   Added XFAC to allow deposition in /s and /yr
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE AIRSAVE(ARRAY,VALIN,IT,IOP,NX1,IX2,XFAC)
C      
C---- Variable Type Declarations ---------------------------------------------
C
c     LOGICAL 
      INTEGER NX1,IX2
      REAL ARRAY(10,5,41,41),VALIN(41)
C     CHARACTER*
C     CHARACTER*
C
C---- Start of Analysis
C  
      DO IX = 1,NX1
        ARRAY(IT,IOP,IX,IX2) = VALIN(IX) * XFAC
      END DO
      RETURN
C----- END OF MODULE AIRSET -------------------------------------------------
      END

