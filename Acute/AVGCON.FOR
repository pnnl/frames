C     GENII: AVGCON.FOR                     Version Date: 04-Nov-1996           
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
C  Written by:       James Stroh/Dennis Strenge                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    08/30/93                                                *
C  Last Modified:    04-Nov-1996 DLS (from FUNCTION AVGCON for AHAZ0         *
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
C     TREL      USED   REAL    ARG            Averaging period (time)
C
C     PNT       USED   REAL    ARG            Concentration array 
C
C     TIME      USED   REAL    ARG            Time points corresponding to 
C                                             concentration values (time)
C
C     CO        USED   REAL    LOCAL          Concentration corresponding to
C                                             T0
C     C1        USED   REAL    LOCAL          Concentration corresponding to
C                                             T1
C     TEND1     USED   REAL    ARG            END OF concentration data  
C                                                                             
C     TSTRT1    USED   REAL    ARG            START OF concentration data     
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
C     04-Nov-96    DLS  Modified for use with GENII SWATER component.  Changed
C                       the averaging period to a real number
C
C==== FUNCTION CALL ========================================================
C
      FUNCTION AVGCON (T0,TREL,TSTRT1,TEND1,PNT,TIME)
C
C----COMMON Block Definitions ------------------------------------------------
C
      INCLUDE 'DEVICE.CMN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION PNT(1000),TIME(1000)
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
      AVGCON = 0.0            ! Modified 20-Nov-1995 DLS
      IF (T0 .GE. TEND1 ) RETURN
C
C---- Check value for release duration.  If zero, error condition ------------
C
      IF(TREL.LE.0.) GO TO 999
      TYR = TREL
      T1 = T0 + TYR
C
C---- Check end time for period against start time for concentration data. ---
C     If end time is before start time, return zero for average concentration.
C
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
      IF(T0.LT.TSTRT1) THEN
        C0 = PNT(1)
        JSTART = 1
        J = 1
      ELSE
c
c Interpolate to find the starting concentration for the integration
        C0 = ( (pnt(j) - pnt(j-1))/(time(j) - time(j-1)) )
     &      * (T0 - time(j-1)) + pnt(j-1)
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
      WRITE(NERR,100) TREL
      NERROR = NERROR + 1
 100  FORMAT(' AVGCON Error, Bad value for TREL =',1P10E10.2)
      RETURN
      END
