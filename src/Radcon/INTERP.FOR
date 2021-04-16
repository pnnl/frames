C     MEPAS RADCON: INTERP.FOR            Version Date: 04-20-1994
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                         SUBROUTINE INTERP                                  *
C                                                                            *
C  Subroutine INTERP linearly interpolates (W) between known points from     *
C  the source (PNT(J,K)) and time (TIM(J,K)) table.  It is called from       *
C  subroutines COUPLE and FUN.                                               *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    04/20/94 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE COUPLE and FUN
C     Calls to:
C     Common blocks referenced: INTER, TIME, INDEX3, INDEX4, INDEX7, MASS
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- -----  ------  -------------- --------------------------------
C     DELTTIM   SET    REAL    LOCAL          DIFFERENCE BETWEEN CURRENT AND  
C                                             PREVIOUS TIME STEPS             
C     DELPNT    SET    REAL    LOCAL          DIFFERENCE BETWEEN CURRENT AND  
C                                             PREVIOUS PNT VALUES             
C     DT        SET    REAL    LOCAL          DIFFERENCE BETWEEN T3 & POINT IN
C                                             TIME OF THE SOURCE FUNCTION     
C     T3        USED   REAL    ARG - IMPORT   LOWER LIMIT ON CONVOLUTION      
C                                             INTEGRAL ALSO INCREMENTED UNTIL 
C                                             EQUAL TO P9                     
C     TEND1     USED   REAL    COMMON - IMP   END OF POLLUTOGRAPH IN PREVIOUS 
C                                             MEDIUM                          
C     TSTRT1    USED   REAL    COMMON - IMP   START OF POLLUTOGRAPH IN        
C                                             PREVIOUS MEDIUM                 
C     W1        SET    REAL    ARG - EXPORT   INTERPOLATED VALUE OF PNT       
C     
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     03/24/89     JWB  NUMI saves the last time (see INPUTM)
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C     04/20/94     JPM  Changed GT to GE in the conditional that checks T3
C                       against the TIME array.  If T3 was equal to the last
C                       time in the TIME array, this module was returning
C                       zero for W1.  Instead, it should be returning the value
C                       in the PNT array corresponding to the last time, which
C                       is not necessarily zero.
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE INTERP (T3,W1,I)
C
C----COMMON Block Definitions ------------------------------------------------
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'INDEX7.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'MASSS.WTN'
      INCLUDE 'CAP.WTn'
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
C     None
C
      IF (T3 .LT. TSTRT1(I)) GO TO 30
      IF (T3 .GT. TEND1(I)) GO TO 30
C*****************************************************************************
C     NUMI IS USED TO SAVE THE LAST TIME WHICH WAS GETTING LOST (SEE INPUTM).
C     JWB 3-24-89
C*****************************************************************************
      NUMI=NUM(I)
      IF(KMED.GT.1)NUMI=NUM(I)+1
      DO 10 J=2,NUMI
      IF ((j.gt.1).and.(TIME(J,I) .GE. T3)) GO TO 20
   10 CONTINUE
   30 W1=0.
      JJ=J
      RETURN
   20 DT=T3-TIME(J-1,I)
      DELTT=TIME(J,I)-TIME(J-1,I)
      DELPNT=PNT(J,I)-PNT(J-1,I)
C      if ((kmed.eq.1) .and. (incr.eq.2)) then
C         write(*,*) dt,deltt,delpnt,pnt(j-1,i)
C      endif
      W1=DT/DELTT*DELPNT+PNT(J-1,I)
      JJ=J
      RETURN
      END
