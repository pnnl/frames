C     MEPAS RADCON: AVGCON.FOR            Version Date: 20-Nov-1995           
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                         FUNCTION AVGCON                                    *
C                                                                            *
C  FUNCTION AVGCON finds the average concentration between times T0 and      *
C  T1 by integrating between these two times and dividing by the input.      *
C  time period, ntimes.                                                      *
C  The two concentration values C0 and C1 are obtained through linear        *
C  interpolation of the source (PNT(J)) and time (TIM(J)) table.             *
C  This function is called from the subroutine HAZ.                          *
C                                                                            *
C  Written by:       James Stroh                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    08/30/93                                                *
C  Last Modified:    20-Nov-1995 DLS (from FUNCTION AVG70Y)                  *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: HAZ   
C     Called by: SUBROUTINE HAZ   
C     Calls to: NONE
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
C (   M1        USED   INT     ARG - IMPORT   Parent number) Use Deleted
C
C     NYR       USED   INT     ARG            Number of years in averaging
C                                             period (years).  20-Nov-1995
C
C    PNT        USED   REAL    ARG            Concentration array 
C
C    TIME       USED   REAL    ARG            Time points corresponding to 
C                                             concentration values (years)
C
C     CO        USED   REAL    LOCAL          Concentration corresponding to
C                                             T0
C     C1        USED   REAL    LOCAL          Concentration corresponding to
C                                             T1
C     TEND1     USED   REAL    COMMON - IMP   END OF concentration data  
C                                                                             
C     TSTRT1    USED   REAL    COMMON - IMP   START OF concentration data     
C                                                                             
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
C
C     20-Nov-95    DLS  Modified program to use variable time period, instead
C                       of the fixed 70 year time period.
C
C==== FUNCTION CALL ========================================================
C
      FUNCTION AVGCON (T0,NYR,TSTRT1,TEND1,PNT,TIME,IERRF)
C
C----COMMON Block Definitions ------------------------------------------------
C    None
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION PNT(1000),TIME(1000)
C
C==== Variable Declarations ==================================================
C
      REAL T1,C0, C1, Area
      INTEGER J, JSTART, JEND, NYR
C
C==== DATA Statements ========================================================
C
C     None
C
C If the starting time is out of range, return a zero concentration
C
C      WRITE(8,500) TO,NYR,TSTRT1,TEND1
C 500  FORMAT(' In AVGCON, TO,NYR,TSTRT1,TEND1',F5.1,I5,2F8.1)
      IERRF = 0
      AVGCON = 0.0
C*****IF (T0 .LT. TSTRT1 .OR. T0 .GE. TEND1 ) THEN
      IF (                    T0 .GE. TEND1 ) THEN
      AVGCON = 0.0            ! Modified 20-Nov-1995 DLS
      RETURN
      END if
C**************************************************************
      IF(NYR.LE.0) GO TO 999
      TYR = FLOAT(NYR)
      T1 = T0 + TYR
      IF(T1.LE.TSTRT1) THEN
         RETURN
      ENDIF
c
c If the end point of the integral is greater than TEND1, reset the end point
c to equal TEND1 (minus some small delta so that the interp routine is ok).
c      
      IF ( T1 .GE. TEND1 )    T1 = TEND1 * 0.9999999
c
c Find the data points which surround the start time
c
      J = 2
      DO WHILE ( TIME(J) .LE. T0 )
          J = J+1
      END DO
c  If start time is less than 1st data time point, then reset start time
c  to first point time
      IF(T0.LT.TSTRT1) THEN
         C0 = PNT(1)
         JSTART = 1
         J = 1
      ELSE
c Interpolate to find the starting concentration for the integration
         C0 = ( (pnt(j) - pnt(j-1))/(time(j) - time(j-1)) )
     &        * (T0 - time(j-1)) + pnt(j-1)
         JSTART = J
      ENDIF
c
c Find the data points which surround the end time
c (Let J start from where JSTART left off)
c
      DO WHILE ( TIME(J) .LE. T1 )
          J = J + 1
      END DO

c Interpolate to find the ending concentration for the integration
      C1 = ( (pnt(j) - pnt(j-1))/(time(j) - time(j-1)) )
     &    * (T1 - time(j-1)) + pnt(j-1)
      JEND = J
c
c  Find the area under the curve for the 70 year interval.
c       Area of a trapazoid:  A = 1/2 * dt * ( F(0) + F(1) )
c
      j = JSTART
      if( JSTART .LT. JEND ) then
          Area = 0.5 * ( time(j) - T0 ) * ( PNT(j) + C0 )
          j = j+1
          Do while ( j .lt. JEND )
              Area = Area + 0.5 * ( time(j) - time(j-1) ) 
     &                          * ( PNT(j)  + PNT(j-1) )
              j = j+1
          end do
          Area = Area + 0.5*( T1 - time(j-1)) * ( C1 + PNT(j-1) )
      else
          Area = TYR/2. * (C0 + C1)
      end if
c
c Divide the area by the time interval to get the average concentration
c
      AVGCON = Area / TYR
      RETURN
 999  CONTINUE
C     WRITE(*,100) NYR, TO
      IERRF = 1
C 100  FORMAT(' AVGCON Error, Bad value for NYR =',I5,' at time ',
C     .       1PE10.3)
      RETURN
      END
