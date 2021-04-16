C     MEPAS RADCON: AVG70Y.FOR            Version Date: 09-17-1996             
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                         FUNCTION AVG70Y                                    *
C                                                                            *
C  FUNCTION AVG70Y finds the average concentration between times T0 and      *
C  T1 by integrating between these two times and dividing by 70 years.       *
C  The two concentration values C0 and C1 are obtained through linear        *
C  interpolation of the source (PNT(J,K)) and time (TIM(J,K)) table.         *
C  This function is called from the subroutine COUPLE.                       *
C                                                                            *
C  Written by:       James Stroh                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    08/30/93                                                *
C  Last Modified:    11/02/93 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE COUPLE
C     Calls to:
C     Common blocks referenced: INTER, CTIME, INDEX3
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- -----  ------  -------------- --------------------------------
C     T0        USED   REAL    ARG - IMPORT   Start time for the integral
C
C     T1        USED   REAL    LOCAL          End time for the integral
C
C     M1        USED   INT     ARG - IMPORT   Parent number
C
C     CO        USED   REAL    LOCAL          Concentration corresponding to
C                                             T0
C     C1        USED   REAL    LOCAL          Concentration corresponding to
C                                             T1
C     TEND1     USED   REAL    COMMON - IMP   END OF POLLUTOGRAPH IN PREVIOUS 
C                                             MEDIUM                          
C     TSTRT1    USED   REAL    COMMON - IMP   START OF POLLUTOGRAPH IN        
C                                             PREVIOUS MEDIUM                 
C     JSTART    USED   INT     LOCAL          Starting node for conc. and time
C                                             pair.
C     JEND      USED   INT     LOCAL          Ending node for conc. and time
C                                             pair.
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/02/93     JPM  Corrected "subscript out of range" error that occurred
C                       when TEND1 and TSTRT1 were an even multiple of 70.  
C     09/17/96     JPM  Corrected an error in the area computation when both
C                       T0 and T1 occur within a single time step (i.e.,
C                       JSTART = JEND).  Under these conditions, the area
C                       computation was based on T1 - T0 = 70, which is not
C                       always true, because when T1 is greater than or equal
C                       to TEND1, T1 is redefined (RAD-P064).
C
C
C==== FUNCTION CALL ========================================================
C
      FUNCTION AVG70Y (T0,M1)
C
C----COMMON Block Definitions ------------------------------------------------
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'INTER.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      REAL T1,C0, C1, Area
      INTEGER J, JSTART, JEND
C
C==== DATA Statements ========================================================
C
C     None
C
C If the starting time is out of range, return a zero concentration
C
      IF ( (T0 .LT. TSTRT1(M1)) .OR. (T0 .GE. TEND1(M1)) ) THEN
      AVG70Y = 0.0
      RETURN
      END if
C**************************************************************
      T1 = T0 + 70.0
c
c If the end point of the integral is greater than TEND1, reset the end point
c to equal TEND1 (minus some small delta so that the interp routine is ok).
c      
      IF ( T1 .GE. TEND1(M1) )    T1 = TEND1(M1) * 0.9999
c
c Find the data points which surround the start time
c
      J = 2
      DO WHILE ( TIME(J,M1) .LE. T0 )
          J = J+1
      END DO

c Interpolate to find the starting concentration for the integration
      C0 = ( (pnt(j,M1) - pnt(j-1,M1))/(time(j,M1) - time(j-1,M1)) )
     &    * (T0 - time(j-1,M1)) + pnt(j-1,M1)
      JSTART = J
c
c Find the data points which surround the end time
c (Let J start from where JSTART left off)
c
      DO WHILE ( TIME(J,M1) .LE. T1 )
          J = J + 1
      END DO

c Interpolate to find the ending concentration for the integration
      C1 = ( (pnt(j,M1) - pnt(j-1,M1))/(time(j,M1) - time(j-1,M1)) )
     &    * (T1 - time(j-1,M1)) + pnt(j-1,M1)
      JEND = J
c
c  Find the area under the curve for the 70 year interval.
c       Area of a trapazoid:  A = 1/2 * dt * ( F(0) + F(1) )
c
      j = JSTART
      if( JSTART .LT. JEND ) then
          Area = 0.5 * ( time(j,M1) - T0 ) * ( PNT(j,M1) + C0 )
          j = j+1
          Do while ( j .lt. JEND )
              Area = Area + 0.5 * ( time(j,M1) - time(j-1,M1) ) 
     &                          * ( PNT(j,M1)  + PNT(j-1,M1) )
              j = j+1
          end do
          Area = Area + 0.5*( T1 - time(j-1,m1)) * ( C1 + PNT(j-1,M1) )
      else
          Area = 0.5 * (T1 - T0) * (C0 + C1)
      end if
c
c Divide the area by the time interval to get the average concentration
c
      AVG70Y = Area / 70.0
      RETURN
      END
