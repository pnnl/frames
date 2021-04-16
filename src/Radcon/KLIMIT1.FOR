C     MEPAS RADCON: KLIMIT1.FOR            Version Date: 04-16-1997
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          SUBROUTINE KLIMIT1                                *
C                                                                            *
C  Subroutine KLIMIT1 calculates the upper and lower time limits of the      *
C  response curve (P1, P9) based on the peak concentration.  Its purpose is  *
C  to identify the interval of the response curve that is important for      *
C  convolution.  It is called from subroutine LIMITS.                        *
C                                                                            *
C  Written by:       Karl J. Castleton and John P. McDonald                  *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    10/02/96 - KJC/JPM                                      *
C  Last Modified:    04/16/97 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  SUBROUTINE LIMITS
C     Calls to: FUNCTION FUN
C     Common blocks referenced: DUMMY,CTIME,COM21,ERROR,MASSS,MEDIA,INDEX4
C
C==== Significant Parameters Designation and Description =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------- -------------- --------------------------------
C     P1        SET    REAL    ARG - EXPORT   LOWER LIMIT OF RESPONSE CURVE
C     P9        SET    REAL    ARG - EXPORT   UPPER LIMIT OF RESPONSE CURVE   
C     TPEAK1    USED   REAL    COMMON - IMP   ESTIMATED TIME TO PEAK
C                                             CONCENTRATION
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     10/21/96     JPM  Allowing MED = 2 (vadose zone concentrations)
C                       (PH1-E001).
C     04/16/97     JPM  Passing TPEAK1 (estimated time to peak including decay
C                       and dispersion) to subroutine FIND instead of TPEAK
C                       (the advective travel time).  In cases where TPEAK1
C                       is significantly different than TPEAK, both P1 and P9
C                       were being set equal to TPEAK (RAD-P067,PH1-E010).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE KLIMIT1 (P1,P9,hw,iset)
      real p1,p9,hw
      integer iset
      Real    CurRRatio,FPeak,tt2
C
C==== COMMON Block Definitions ==============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
      include 'masss.wtn'
      INCLUDE 'MEDIA.WTN'
      INCLUDE 'INDEX4.WTN'
C
C==== DIMENSION Statements ==================================================
C
C     None
C
C==== Variable Declarations =================================================
C
C     None
C 
C==== DATA Statements =======================================================
C
C     None
C
      if (TotMas(M1).gt.0.0.and.MED(KMED).le.4) then
        CurRRatio=RRATIO
        tt2=t2(m1)
        RRatio=1e-20
        t2(m1)=tpeak1(m1)
C*****************************************************************************
C     The target concentration is seven orders of magnitude less than the peak
C     concentration.  Function FUN (with RRATIO=1E-20) evaluates the response
C     curve at the time t2(m1) to determine the target concentration (FPeak).
C*****************************************************************************
        FPeak=fun(0.0,hw,iset)*1.0E-07
C*****************************************************************************
C     Subroutine FIND returns the times associated with the target
C     concentrations.  'True' means to evaluate the portion of the response
C     curve to the left of the peak, and 'false' means to evaluate to the
C     right.
C*****************************************************************************
        p1=Find(hw,iset,1.0E-5,tpeak1(m1),.True.,FPeak)
        p9=Find(hw,iset,tpeak1(m1),TFinal,.False.,FPeak)
        RRATIO=CurRRatio
        t2(m1)=tt2
      else
        p1 = tstrt1(m1)
        p9 = tend1(m1)
      endif
 9999 RETURN
      END

