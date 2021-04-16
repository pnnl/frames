C     MEPAS RADCON: BRUTE.FOR             Version Date: 09-27-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE BruteFindPeak                         *
C                                                                            *
C  This subroutine selects 9 equally spaced points in the interval to be     *
C  integrated, evaluates function FUN at each point, and returns the time of *
C  the highest function FUN result.                                          *
C                                                                            *
C  Written by:       Karl Castleton                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    09/27/96 - KJC                                          *
C  Last Modified:                                                            *
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
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C
C
C==== SUBROUTINE CALL ========================================================
C
      function BruteFindPeak(HW,ISET,StartT,EndT)
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
        real maxtime,max,test,testtime,h
C
C==== DATA Statements ========================================================
C
C     None
C
        h=(endt-startt)/8.0
        maxtime=startt
        max=fun(startt,hw,iset)
        testtime=startt
        do i = 1,8
          testtime=testtime+h
          test=fun(testtime,hw,iset)
          if (test .gt. max) then
            max=test
            maxtime=testtime
          end if
        enddo
        BruteFindPeak=maxtime
        return
      end
