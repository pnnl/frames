C  SETAIR.FOR   EXPOS                Version Date: 10-Jan-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SETAIR                                *
C                                                                            *
C  Subroutine SETAIR sets air/deposition parameter values into single site   *
C     arrays or the current analysis                                         *
C                                                                            *
C  Written by:       DL Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    10-Jan-97                                               *
C  Last Modified:    10-Jan-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/EXPOS
C     Called by: EXPOS
C     Calls: NONE  
C     Common blocks referenced: CONC,CONIN
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     IX1        U      INT    Arg.      1st axis array position (radius/x)
C     IX2        U      INT    Arg.      2nd axis array position (theta/y)
C     NUCTOT     U      INT    Arg.      Number of chain members
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   10-Jan-97      DLS  Initial programming started
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SETAIR(NUCTOT)
C      
C---- INCLUDE statements -----------------------------------------------------
C
      INCLUDE 'CONC.CMN'
      INCLUDE 'CONIN.CMN'
      INCLUDE 'AFLAGS.CMN'
      INCLUDE 'FLUX.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NUCTOT
C      INTEGER IX1, IX2, NUCTOT
C      CHARACTER*6 GRID
C
C---- Data Statements --------------------------------------------------------
C
C      DATA GRID/'grid  '/
C
C---- Start of Analysis
C
        DO IN = 1,NUCTOT
          IF(IN.EQ.CMEM) THEN
            IF(APTYPE.EQ.1) THEN
              AIRCON(IN) = 1.
              DRYDEP(IN) = 0.
              WETDEP(IN) = 0.
              EXTD(IN)   = 1.
            ELSEIF(APTYPE.EQ.2) THEN
              AIRCON(IN) = 0.
              DRYDEP(IN) = 1.
              WETDEP(IN) = 0.
            ELSE
              AIRCON(IN) = 0.
              DRYDEP(IN) = 0.
              WETDEP(IN) = 1.
            ENDIF
          ELSE
            AIRCON(IN) = 0.
            DRYDEP(IN) = 0.
            WETDEP(IN) = 0.
          ENDIF
        END DO
c
C----- For "points" input ------------------------------------------------------
c
C      ELSE
C        DO IN = 1,NUCTOT
C          INX = ICROS(IN)
C          IF(INX.GT.0) THEN
C             AIRCON(IN) = AIRCIN(INX,IX1,IX2)
C             EXTD(IN)   = EXTDOS(INX,IX1,IX2)
C             DRYDEP(IN) = DEPDRY(INX,IX1,IX2)      
C             WETDEP(IN) = DEPWET(INX,IX1,IX2)      
C             TOTDEP(IN) = DEPTOT(INX,IX1,IX2)      
C          ENDIF
C        END DO
C      END IF
      RETURN
C
C----- END OF MODULE SETAIR -------------------------------------------------
C
      END

