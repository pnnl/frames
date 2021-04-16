C     MEPAS RADCON: HOLDP.FOR             Version Date: 11-25-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          SUBROUTINE HOLDP                                  *
C                                                                            *
C   This subroutine computes the holding pond calculations for overland      *
C   runoff prior to degradation or decay unless (if there id decay)          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  RADCONPC
C     Calls to: None
C     Common blocks referenced:  INTER, TIME, INDEX3, INDEX4
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------  -------------- ---------------------------
C
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE HOLDP (M1,C,AL,TLIFE,NDS)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'INTER.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION AL(MAXCON,12),NDS(MAXCON),TLIFE(MAXCON)
C
C---- Variable Declarations --------------------------------------------------
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      IF (IT .NE. 1) GO TO 10
      TEND2(M1)=4.6052/AL(M1,1)
      IF (TEND2(M1) .GT. (100.*TLIFE(M1))) TEND2(M1)=100.*TLIFE(M1)
      IF (TEND2(M1) .LT. TLIFE(M1)) TEND2(M1)=TLIFE(M1)
      TSTRT2(M1)=TSTRT1(M1)
      DT=(TEND2(M1)-TSTRT2(M1))/NTIMES
   10 T2(M1)=TSTRT2(M1)+DT*(IT-1)
      C=PNT(1,M1)
      IF (NDS(M1) .EQ. 1) C=C*EXPF(-AL(M1,1)*T2(M1))
      IF (NPRINT .LE. 1) GO TO 101
      WRITE(6,1) IT,M1,T2(M1),PNT(1,M1),C
    1 FORMAT (2I10,1P3E10.3)
  101 CONTINUE
      RETURN
      END
