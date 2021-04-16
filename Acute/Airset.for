C  AIRSET.FOR   ACUTE                Version Date: 11-May-98            
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE AIRSET                                *
C                                                                            *
C  Subroutine AIRSET puts values into arrays for input from the ATO file     *
C                                                                            *
C  Written by:       DL Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    10-Jan-97                                               *
C  Last Modified:    11-May-98    BAN                                     *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/ACUTE
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
C     VALIN      U     REAL    Argument  Array of input values
C     ICM        U     INT     Argument  Position of chain member of interest
C     NX1        U     INT     Argument  Number of positions to be filled
C     IX2        U     INT     Argument  Position of data for second co-ordinate
C==== Modification History ===================================================
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  10-Jan-97  DLS  Initial programming started for EXPOS
C  18-Nov-97  DLS  Converted for use with ACUTE
C  11-May-98  BAN  Update for 41x41 arrays
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE AIRSET(ARRAY,VALIN,ICM,NX1,IX2)
C
C---- Include Statements -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NX1,IX2,ICM
      REAL ARRAY(9       ,41,41),VALIN(41)
C
C---- Start of Analysis
C  
      DO IX = 1,NX1
        ARRAY(ICM,IX,IX2) = ARRAY(ICM,IX,IX2) + VALIN(IX)
      END DO
      RETURN
C----- END OF MODULE AIRSET -------------------------------------------------
      END

