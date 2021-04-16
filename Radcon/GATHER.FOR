C     MEPAS RADCON: GATHER.FOR            Version Date: 11-25-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                        SUBROUTINE GATHER                                   *
C                                                                            *
C  Subroutine GATHER collates and prints all contaminants by name.           *
C  It is called by the main program.                                         *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    11/25/91 - JWB                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  RADCONPC and MEASUR
C     Calls to: None
C     Common blocks referenced:  LIST, TIME, INDEX3, INDEX4
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------  -------------- ---------------------------
C     CS        USED   REAL     COMMON - IMP  CONCENTRATION OF CONTAMINANTS
C     II        USED   INTEGER  COMMON - IMP  TIME STEP COUNTER
C     ICNT      USED   INTEGER  ARG - IMPORT  TOTAL NUMBER OF CONSTITUENTS INCLUDING  
C                                             PARENTS AND DAUGHTERS
C     NPRINT    USED   INTEGER  COMMON - IMP  INDEX FOR AMOUNT OF OUTPUT
C     STRT      USED   REAL     COMMON - IMP  STARTING TIME IN NEW MEDIA
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE GATHER (ICNT)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'LIST.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C---- Variable Declarations --------------------------------------------------
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      DO 12 J=1,IT
        DO 10 I=1,ICNT
          IF (CS(J,I) .LT. 0.) then
            CS(J,I)=0.
          endif
          IF (NPRINT .LE. 1) GO TO 10
          WRITE(6,5) J,I,IVS(I),CASN2(I),TIM(J,I),CS(J,I),STRT(I)
5         FORMAT (2X,2I5,2X,A18,2X,A18,5X,3(2X,1PE10.3))
10      CONTINUE
12    CONTINUE
      RETURN
      END
