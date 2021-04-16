C     MEPAS RADCON: REFINE.FOR             Version Date: 04-07-1997
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE RefineFindPeak                        *
C                                                                            *
C  This subroutine cuts the interval to be integrated into 1000 points and   *
C  returns the time of the highest of those 1000 points.                     *
C                                                                            *
C  Written by:       Karl Castleton                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    09/27/96 - KJC                                          *
C  Last Modified:    04/07/97 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: KSIMPS
C     Calls to:  FUNCTION FUN
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ -------- -------------- -------------------------------
C     HW        USED   INTEGER ARG - IMPORT   Flag needed for function fun     
C     ISET      USED   INTEGER ARG - IMPORT   Flag needed for function fun
C     StartT    USED   REAL    ARG - IMPORT   Start Time of interval
C     EndT      USED   REAL    ARG - IMPORT   End Time of interval
C     Peak      USED   REAL    ARG - IMPORT   Guess of were the peak is 
C                                             from BruteFindPeak
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     04/07/97     JPM  Introduced variables xr and xl as the x values passed
C                       to function fun, and making sure that these values
C                       always lie in the interval STARTT to ENDT.  When the
C                       guess for the peak from BruteFindPeak was equal to
C                       STARTT or ENDT, the value passed into function fun
C                       (peak+h or peak-h) was outside the time range
C                       causing REFINE to behave improperly (RAD-P068).
C
C==== SUBROUTINE CALL ========================================================
C
      function RefinePeak(HW,ISET,StartT,EndT,Peak)
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
        real h,a,b,dfx,fr,fl,xr,xl
        integer loop
C
C==== DATA Statements ========================================================
C
C     None
C
        loop=0
C If the 1000.0 is changed below the number 10 should be changed on line 559
C to the log base 2 of the number (e.g. 100.0 below log2 6.64 ~ 7 in line 559)
        h=(endt-startt)/2000.0
        xr=peak+h
        if (xr.gt.endt) xr = endt
        fr=fun(xr,hw,iset)
        xl=peak-h
        if (xl.lt.startt) xl = startt
        fl=fun(xl,hw,iset)
        if (xr-xl.ne.0.0) then
          dfx=(fr-fl)/(xr-xl)
        else
          dfx=0.0
        endif
        if (dfx .gt. 0.) then
          a=peak
          b=endt
        else
          a=startt
          b=peak
        end if
 557      peak=(a+b)/2
 559      if ((loop.ge.10.97).or.((b-a).lt.h)) goto 558
          loop=loop+1
          xr=peak+h
          if (xr.gt.endt) xr = endt
          fr=fun(peak+h,hw,iset)
          xl=peak-h
          if (xl.lt.startt) xl = startt
          fl=fun(peak-h,hw,iset)
          if (xr-xl.ne.0.0) then
            dfx=(fr-fl)/(xr-xl)
          else
            dfx=0.0
          endif
          if (dfx.gt.0) then
            a=peak
          else
            b=peak
          endif
          goto 557
 558    RefinePeak=peak
      return 
      end
