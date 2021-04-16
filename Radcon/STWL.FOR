C     MEPAS RADCON: STWL.FOR              Version Date: 11-26-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE STWL                                      *
C                                                                            *
C  Subroutine STWL computes the soil moisture storage (ST) or the initial    *
C  accumulated water potential loss (WL) for a single month based on soil    *
C  moisture retention tables.  It is called from subroutine STORAG and calls *
C  subroutine FUNCTION (function EXPF).                                      *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    11/26/91 - JWB                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE STORAG
C     Calls to:  SUBROUTINE FUNCTION
C     Common blocks referenced: INDEX3
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------  -------------- --------------------------------
C     KFLAG     USED   INTEGER ARG - IMPORT   INDEX IDICATING WHICH SOIL      
C                                             MOISTURE RETENSION CURVE IS USED
C     ST        SET    REAL    ARG - EXPORT   SOIL MOISTURE DETERMINED FROM   
C                                             APPROPRIATE TABLE               
C     ST1       SET    REAL    LOCAL          INTERM SOIL MOISTURE VARIABLE   
C                                             USED TO INTERPOLATE ST          
C     WL        SET    REAL    ARG - EXPORT   ACCUMULATED WATER POTENTIAL LOSS
C                                             CALCULATED FROM SOIL MOISTURE   
C                                             TABLES                          
C     WL1       SET    REAL    LOCAL          INTERMEDIATE VARIABLE USED TO   
C                                             INTERPOLATE WL FROM TABLES      
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE STWL (JJJ,KFLAG,ST,WL,FRACT,SLOP,YINTER,AVAILW)
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
      IF (JJJ .EQ. 2) GO TO 30
      IF (KFLAG .NE. 1) GO TO 10
      ST=EXPF(WL*SLOP(KFLAG)+YINTER(KFLAG))
      IF (ST .LT. 0.1) ST=0.1
      GO TO 20
C
   10 ST=EXPF(WL*SLOP(KFLAG-1)+YINTER(KFLAG-1))
      ST1=EXPF(WL*SLOP(KFLAG)+YINTER(KFLAG))
      IF (ST .LT. 0.1) ST=0.1
      IF (ST1 .LT. 0.1) ST1=0.1
C*****************************************************************************
C     MORE PRINTOUT INFORMATION
C*****************************************************************************
      IF (NPRINT .LE. 1) GO TO 950
      WRITE(6,1) JJJ,KFLAG,WL,SLOP(KFLAG-1),SLOP(KFLAG),
     C YINTER(KFLAG-1),YINTER(KFLAG),FRACT,ST,ST1
    1 FORMAT (2I10,1P9E10.3)
C
  950 CONTINUE
      ST=EXPF((ALOG(ST1)-ALOG(ST))*FRACT+ALOG(ST))
      IF (ST .LT. 0.1) ST=0.1
      IF (ST .GT. AVAILW) ST=AVAILW
      GO TO 20
C
   30 IF (KFLAG .NE. 1) GO TO 40
      WL=(ALOG(ST)-YINTER(KFLAG))/SLOP(KFLAG)
      GO TO 20
C
   40 WL=(ALOG(ST)-YINTER(KFLAG-1))/SLOP(KFLAG-1)
      WL1=(ALOG(ST)-YINTER(KFLAG))/SLOP(KFLAG)
      WL=(WL1-WL)*FRACT+WL
C
   20 CONTINUE
      RETURN
      END
