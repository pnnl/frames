C     MEPAS RADCON: TrapZD.FOR            Version Date: 10-10-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE TrapZD                                *
C                                                                            *
C  This is a fortran version of the subroutine TrapZD in Numerical Recipes   *
C  in C.  ISBN 0-521-35465-X page 120                                        *
C                                                                            *
C  Written by:       Karl Castleton                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    10/10/96 - KJC                                          *
C  Last Modified:                                                            *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SIMPS   
C     Calls to:  SUBROUTINE FUN     
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
C     N         USED   INTEGER ARG - IMPORT   Stage of Refinement
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C
C
C==== SUBROUTINE CALL ========================================================
C
      function trapzd(A,B,HW,ISET,N)
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
        real a,b,fun
        integer hw,iset,n
        real s,f(0:256),x(0:256)
        integer step,j
C
C==== DATA Statements ========================================================
C
C     None
C
        save f,x,step

        if (n .eq. 1) then
C         Assuming x can never be negative a -1 implies function not evaluated
          do 100 j=1,255
            x(j)=-1.0
 100      continue
          step= 128
          x(0)=a
          f(0)=fun(x(0),hw,iset)
          x(256)=b
          f(256)=fun(x(256),hw,iset)
          trapzd=0.5*(x(256)-x(0))*(f(256)+f(0))
c          write (6,99) 0,x(0),f(0)
c          write (6,99) 256,x(256),f(256)
        else
          s=0.
          j=step
          do 25 while (j .le. 256)
C           if f(x(j)) is needed evaluate it
            if (x(j).lt.0.0) then
              x(j)=a+(b-a)*j/256
              f(j)=fun(x(j),hw,iset)
c              write (6,99) j,x(j),f(j)
99            format (i10,1pe10.3,e10.3)
            endif
            s=s+0.5*(x(j)-x(j-step))*(f(j)+f(j-step))
            j=j+step
 25       continue
          step=step/2
          trapzd=s
        end if
      return
      end
