C     MEPAS RADCON: WLINIT.FOR            Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                     SUBROUTINE WLINIT                                      *
C                                                                            *
C  Subroutine WLINIT computes the initial accumulated potential water loss   *
C  (WLO).  It is called from the subroutine LEACH1.                          *
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
C     Called by: SUBROUTINE LEACH1
C     Calls to: None
C     Common blocks referenced: INDEX3
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------- -------------  --------------------------------
C     FPPOS     USED   REAL    ARG - IMPORT   SUM OF POSITIVE INFILTRATION    
C     KFLAG     USED   INTEGER ARG - IMPORT   INDEX IDENTIFYING APPROPRIATE   
C                                             SOIL MOISTURE RETENTION TABLE   
C     STO       SET    REAL    ARG - EXPORT   INITIAL STORAGE - TABULAR DATA  
C     WLO       SET    REAL    ARG - EXPORT   INITIAL ACCUMULATED WATER LOSS  
C     VALHOLD   SET    REAL    LOCAL          DIFFERENCE BETWEEN OLD & NEW STO
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C

      SUBROUTINE WLINIT (WLO,FPPOS,FPNEG,KFLAG,FRACT,SLOP,YINTER,
     1                   AVAILW)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION SLOP(11),YINTER(11)
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
C     NOTE UNITS IN CM
C*****************************************************************************
      WLO=ABS(WLO)
      J=0
      JJJ=1
C
      CALL STWL (JJJ,KFLAG,STO,WLO,FRACT,SLOP,YINTER,AVAILW)
   10 STSAV=STO
      IF (NPRINT .LE. 1) GO TO 950
      WRITE(6,1) J,JJJ,STO,WLO
  950 CONTINUE
      J=J+1
      STO=STO+FPPOS
      IF (STO .GT. AVAILW) STO=AVAILW
      JJJ=2
C
      CALL STWL (JJJ,KFLAG,STO,WLO,FRACT,SLOP,YINTER,AVAILW)
      WLOSAV=WLO
      IF (NPRINT .LE. 1) GO TO 951
      WRITE(6,1) J,JJJ,STO,WLO
  951 CONTINUE
      WLO=WLO+ABS(FPNEG)
      JJJ=1
      IF (NPRINT .LE. 1) GO TO 952
      WRITE(6,1) J,JJJ,STO,WLO
  952 CONTINUE
C
      CALL STWL (JJJ,KFLAG,STO,WLO,FRACT,SLOP,YINTER,AVAILW)
      IF (NPRINT .LE. 1) GO TO 953
      WRITE(6,1) J,JJJ,STO,WLO
  953 CONTINUE
      IF (J .LE. 1) GO TO 10
      IF (J .EQ. 100) GO TO 20
C
      VALHOLD=ABS(STSAV-STO)
      IF (VALHOLD .GT. 0.1) GO TO 10
C
   20 WLO=-WLOSAV
    1 FORMAT (2I10,1P8E10.3)
      RETURN
      END
