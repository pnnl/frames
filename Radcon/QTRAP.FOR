C     MEPAS RADCON: QTRAP.FOR            Version Date: 09-27-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE QTrap                                 *
C                                                                            *
C  This is a fortran version of the subroutine qtrap in Numerical Recipes    *
C  in C.  ISBN 0-521-35465-X page 121                                        *
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
C     Called by: SIMPS   
C     Calls to:  FUNCTION FUN
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ -------- -------------- -------------------------------
C     HW        USED   INTEGER ARG - IMPORT   Flag needed for function fun     
C     ISET      USED   INTEGER ARG - IMPORT   Flag needed for function fun
C     A         USED   REAL    ARG - IMPORT   Start Time of interval
C     B         USED   REAL    ARG - IMPORT   End Time of interval
C     Error     USED   REAL    ARG - IMPORT   Error Criteria to stop
C                                             integration (EPS in original)
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C
C==== SUBROUTINE CALL ========================================================
C
      function QTrap(HW,ISET,A,B,Error)
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
        integer hw,iset
        real a,b,error,trapzd
        integer j
        real news,olds
C
C==== DATA Statements ========================================================
C
C     None
C
        olds=trapzd(a,b,hw,iset,1)
        do 100 j=2,9
          news=trapzd(a,b,hw,iset,j)
          if (abs(news-olds) .lt. error*abs(olds)) goto 200
          olds=news
 100    continue
 200  QTrap=news
      return
      end
