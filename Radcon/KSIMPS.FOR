C     MEPAS RADCON: KSIMPS.FOR             Version Date: 04-16-1997
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE KSIMPS                                *
C                                                                            *
C  Subroutine SIMPS controls the numerical integration process.  It          *
C  determines the time of the peak on the curve to be integrated, then       *
C  integrates from the lower integration limit to the peak, then from the    *
C  peak to the upper limit and sums the result.  It is called from the main  *
C  module and calls functions FUN (FUNCTION.FOR), BRUTEFINDPEAK (BRUTE.FOR), *
C  REFINEFINDPEAK (REFINE.FOR), and QTRAP (QTRAP.FOR).                       *
C                                                                            *
C  Written by:       Karl J. Castleton and John P. McDonald                  *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    10/10/96 KJC/JPM                                        *
C  Last Modified:    04/16/97 JPM                                            *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCOND
C     Calls to:  SUBROUTINE FUNCTION (FUN), BRUTE (BRUTEFINDPEAK),
C                REFINE (REFINEFINDPEAK), QTRAP
C     Common blocks referenced: INDEX1, INDEX4, CTIME, MASSS, MEDIA
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ -------- -------------- -------------------------------
C     C         SET    REAL    ARG - EXPORT   SUMMED CONTAMINANT CONCENTRATION
C     DTT       USED   REAL    ARG - IMPORT   UPPER LIMIT FOR THE CONVOLUTION
C                                             INTEGRAL
C     ERRMAX    SET    REAL    ARG - EXPORT   MAXIMUM ERROR
C     P         SET    REAL    LOCAL - EXP    THE TIME OF THE PEAK ON THE
C                                             CONVOLUTION CURVE
C     RATIO     SET    REAL    ARG - EXPORT   TIME TO PEAK DIVIDED BY DURATION
C                                             OF RELEASE
C     T3        USED   REAL    ARG - IMPORT   LOWER LIMIT FOR THE CONVOLUTION
C                                             INTEGRAL
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     10/18/96     JPM  Simplified the condition for the instantaneous
C                       solution by removing the conditions on medium type
C                       (MED).  Changes being made for Phase 1 of the MEPAS
C                       waterborne executables for the framework (PH1-E001).
C     04/16/97     JPM  Making sure that the interval being integrated over
C                       is not larger than the duration of the flux into the
C                       current medium (TLIF).  The curve beyond TLIF is
C                       equal to zero (RAD-P068,PH1-E011).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE KSIMPS (T3,DTT,C,HW,ISET,ERROR,M1,ERRMAX,RATIO)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'MASSS.WTN'
      INCLUDE 'MEDIA.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      real a,b,p
C
C==== DATA Statements ========================================================
C
C     None
C
      IF ((DTT .GT. T3).AND.(T2(M1).GT.0.0)) THEN
C
C*****************************************************************************
C     If RATIO >= RRATIO, then the problem is temporally far field and the
C     instantaneous solution is used in place of the convolution integral
C     for releases to groundwater (ISTYPE <= 2) and PSZ or SZ media
C     (MED(KMED) = 1, 3, or 4).  JPM/GW
C*****************************************************************************
C
        RATIO=TPEAK1(M1)/TLIF(M1)
        IF (RATIO.GE.RRATIO) THEN
          C=FUN(T3,HW,ISET)
        ELSE
          a=t3
          b=dtt
          if (b-a.gt.tlif(m1)) b = a + tlif(m1)
          p=BruteFindPeak(hw,iset,a,b)
          p=RefinePeak(hw,iset,a,b,p)
          ERROR=5.0E-4
          ErrMax=3.0E-3
          if (a.eq.p) then
            c1 = 0.0
          else
            c1=QTrap(hw,iset,a,p,Error)
          endif
          a=t3
          b=dtt
          if (b-a.gt.tlif(m1)) b = a + tlif(m1)
          if (p.eq.b) then
            c2 = 0.0
          else
            c2=QTrap(hw,iset,p,b,Error)
          endif
          c = c1 + c2
        ENDIF
      ELSE
        C = 0.0
      ENDIF
      RETURN
      END
