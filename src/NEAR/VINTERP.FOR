C  VINTERP.FOR  EXPOS                Version Date: 24-Jun-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE VINTERP                               *
C                                                                            *
C  Subroutine VINTERP controls interpolation of air concentration data over  *
C             time for each chain member, quantity, and coordinate point     *
C                                                                            *
C  Written by:       DL Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    24-Jun-97                                               *
C  Last Modified:    24-Jun-97      DLS                                      *
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
C     VALIN      U     REAL    Argument  Array of input values
C     ICM        U     INT     Argument  Position of chain member of interest
C     NX1        U     INT     Argument  Number of positions to be filled
C     IX2        U     INT     Argument  Position of data for second co-ordinate
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   24-Jun-97      DLS  Initial programming started
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE VINTERP(VTIN,TIMP,NCM,NTM,OUTYPE,NAX1,NAX2)
C      
C---- Include Statments ------------------------------------------------------
C
      INCLUDE 'TIMES.CMN'
      INCLUDE 'CONIN.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
c     LOGICAL 
      INTEGER NCM,NAX1,NAX2
      REAL VTIN(10,5,10,36),TIMP(10)
      LOGICAL OUTYPE(5)
C
C---- Start of Analysis
C
      T0 = BEFORE - FLOAT(BEFAIR)
      TSTRT1 = TIMP(1)
      TEND1 = TIMP(NTM)
      DO IX1 = 1,NAX1
        DO IX2 = 1,NAX2
          IF(OUTYPE(1)) THEN
            DEPDRY(NCM,IX1,IX2) = AVGCON(T0,TMAX,TSTRT1,TEND1,
     .                            VTIN(1,1,IX1,IX2),TIMP)
          END IF
          IF(OUTYPE(2)) THEN
            DEPWET(NCM,IX1,IX2) = AVGCON(T0,TMAX,TSTRT1,TEND1,
     .                            VTIN(1,2,IX1,IX2),TIMP)
          END IF
          IF(OUTYPE(3)) THEN
            DEPTOT(NCM,IX1,IX2) = AVGCON(T0,TMAX,TSTRT1,TEND1,
     .                            VTIN(1,3,IX1,IX2),TIMP)
          END IF
          IF(OUTYPE(4)) THEN
            AIRCIN(NCM,IX1,IX2) = AVGCON(T0,TMAX,TSTRT1,TEND1,
     .                            VTIN(1,4,IX1,IX2),TIMP)
          END IF
          IF(OUTYPE(5)) THEN
            EXTDOS(NCM,IX1,IX2) = AVGCON(T0,TMAX,TSTRT1,TEND1,
     .                            VTIN(1,5,IX1,IX2),TIMP)
          END IF
        END DO
      END DO
      RETURN
C
C----- END OF MODULE VINTERP ------------------------------------------------
C
      END

