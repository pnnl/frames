C     MEPAS RADCON: WLIST.FOR             Version Date: 09-28-2004
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                        SUBROUTINE WLIST                                    *
C                                                                            *
C  Subroutine WLIST stores values for later sorting and printing.            *
C  It is called from subroutine MEASUR and the main program.                 *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    09/28/04 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC and SUBROUTINE MEASUR
C     Calls to: None
C     Common blocks referenced: LIST, TIME, INDEX3, INDEX4
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter  Set/           Location
C     Name       Used   Type    Export/Import  Parameter Descriptions
C     --------- ------ ------  --------------- -------------------------------
C     CDA        SET    REAL    ARG - EXPORT   CONCENTRATION OF DECAY-PRODUCTS
C                                              SET = PARENT CONCENTRATION IF  
C                                              K < NMED; ELSE SET TO DECAY    
C                                              CONCENTRATION
C     CS         USED   REAL    ARG - IMPORT   CONCENTRATION OF CONTAMINANT   
C     ICNT       S/U    INTEGER ARG - EXPORT   TOTAL # OF CONTAMINANTS        
C                                              INCLUDING PARENT AND DAUGHTERS 
C     STRT       USED   REAL    COMMON - EXP   INTERMEDIATE TIME SHIFT        
C     T2         USED   REAL    COMMON - IMP   CURRENT TIME                   
C     TIM        USED   REAL    COMMON - EXP   DECAY TIME, DEPENDENT ON IDECAY
C     TSTRT2     USED   REAL    COMMON - IMP   START TIME OF POLLUTAGRAPH IN  
C                                              CURRENT MEDIA                  
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     04/15/94     JPM  Time Specific Modification - store concentrations
C                       and times in a dummy CS and TIM array.  JPM/GW 
C     07/21/94     JPM  No longer storing the first time and concentration
C                       in the dummy CS and TIM array, in addition to the
C                       specific times and concentrations.
C     06/09/98     JPM  Making IV and CASN1 character variable types
C                       (PH1-E016).
C     09/28/04     JPM  Setting the output flux/concentration equal to zero
C                       over the time period from 0 to TFINAL when the vadose
C                       zone infiltration rate is zero (PH1-P026).
C
C==== SUBROUTINE CALL ========================================================
C

      SUBROUTINE WLIST (IV,CASN1,CDA,ICNT,M1)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'LIST.WTN'
      INCLUDE 'MEDIA.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'COM21.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      CHARACTER*18 IV,CASN1
C
C==== DATA Statements ========================================================
C
C     None
C
      ICNT=ICNT+1
      IVS(ICNT)=IV
      CASN2(ICNT)=CASN1
      CS(IT,ICNT)=CDA
C
C*****************************************************************************
C     Time Specific Modification - Load the dummy CS arrays.
C     CS_SPEC for concentrations, and CS_TIME for the times.  The KOUNT_TS
C     loop is executed once for each specific time, and the NTIMES loop is
C     executed twice (IT = 1 and 2) for each pass thru the KOUNT_TS loop.
C     When IT = 2, store the specific time and concentration.
C*****************************************************************************
C
      IF ((NTIMSPEC.GT.0).AND.(MED(KMED).EQ.3).AND.(IT.EQ.2)) THEN
           CS_SPEC(KOUNT_TS,ICNT)=CDA
           CS_TIME(KOUNT_TS,ICNT)=T2(M1)
      ENDIF
C
      IF (IT .GT. 1) GO TO 10
      STRT(ICNT)=TSTRT2(M1)
   10 TIM(IT,ICNT)=T2(M1)
C*****************************************************************************
C     If NTIMES = 0, then the infiltration rate from the vadose zone is zero.
C     Set the output flux/concentration equal to zero over the time period of
C     zero to TFINAL (total time).  T2(M1) is in medium time, but will not
C     necessarily correlate to zero total time.  To accomplish this, subtract
C     TRAVEL from T2.  PH1-P026
C*****************************************************************************
      IF (NTIMES.EQ.0) THEN
        TIM(IT,ICNT)=T2(M1)-TRAVEL(M1)
        CS(2,ICNT)=CDA
        TIM(2,ICNT)=TFINAL-TRAVEL(M1)
      ENDIF
      RETURN
      END
