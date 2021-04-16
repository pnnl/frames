C     MEPAS RADCON: FIND.FOR            Version Date: 10-02-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          FUNCTION FIND                                     *
C                                                                            *
C  Function FIND uses Newton's Method to find a target concentration on an   *
C  interval of the response curve, and returns the time at which this        *
C  concentration occurs.  It assumes that the response curve is continuous   *
C  and either increasing or decreasing throughout the interval being         *
C  searched (START to END).  It is called from subroutine KLIMIT1.           *
C                                                                            *
C  Written by:       Karl J. Castleton and John P. McDonald                  *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    10/02/96 - KJC/JPM                                      *
C  Last Modified:                                                            *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  SUBROUTINE KLIMIT1
C     Calls to: FUNCTION FUN
C     Common blocks referenced: DUMMY,CTIME,COM21,ERROR,MASSS
C
C==== Significant Parameters Designation and Description =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------- -------------- --------------------------------
C     end       used   real    import - arg    The end time of the interval
C     left      used   logical import - arg    True if the interval being
C                                              searched is left of the peak,
C                                              false if it's to the right.
C     start     used   real    import - arg    The start time of the interval
C     target    used   real    import - arg    The target concentration to find
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C
C
C==== FUNCTION CALL =========================================================
C
      Function Find(hw,iset,Start,End,Left,Target)
C
C==== COMMON Block Definitions ==============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
      include 'masss.wtn'
C
C==== DIMENSION Statements ==================================================
C
C     None
C
C==== Variable Declarations =================================================
C
      real    start,end,target,hw
      integer iset
      logical Left
      real  h,f,t,smallest,tt2
C 
C==== DATA Statements =======================================================
C
C     None
C
      tt2=t2(m1)
      smallest = 1.0E-07
      t=(start+end)/2
      h=(end-start)/2
 557  t2(m1)=t
      f=fun(0.0,hw,iset)
      h=h/2
      if (Left) then
        if (f.gt.target) then
          t=t-h
        else
          t=t+h
        end if
      else
        if (f.gt.target) then
          t=t+h
        else
          t=t-h
        endif
      endif
      if ((h.gt.smallest).and.(h.gt.0)) goto 557
      Find=t
      t2(m1)=tt2
      return
      end
