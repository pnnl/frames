C     MEPAS RADCON: LIMITS.FOR            Version Date: 09-25-2004
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE LIMITS                                *
C                                                                            *
C  Subroutine LIMITS is the main subroutine for calculating the upper and    *
C  lower limits of integration for the groundwater pathway.  The subroutine  *
C  first calculates the time to peak contaminant concentration (TPEAK(M1))   *
C  using equation 2.60 from Whelan et al. 1996.  This equation is the        *
C  derivative of the analytical Green's function solution assuming a point   *
C  source.  Subroutine KLIMIT1 is then called to numerically determine the   *
C  upper and lower limits of integration (P1, P9) based on the peak          *
C  concentration of the response curve.  Finally, solution time steps are    *
C  calculated based on the integration limits.  The subroutine LIMITS is     *
C  called from the main program and subroutine WETLND and calls subroutine   *
C  KLIMIT1.                                                                  *
C                                                                            *
C  Written by:        Gene Whelan                                            *
C                     Battelle Pacific Northwest Laboratories                *
C                     P.O. Box 999                                           *
C                     Richland, WA  99352                                    *
C                                                                            *
C  Creation Date:     01/19/89 (Converted to PC)                             *
C  Last Modified:     09/25/04 - JPM                                         *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  SUBROUTINES RADCONPC and WETLND
C     Calls to:   SUBROUTINE KLIMIT1
C     Common Blocks Referenced: DUMMY, TIME, WETLND, INDEX1, INDEX2, 
C                               INDEX3, INDEX4, and COM21
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------- ------  -------------  -------------------------------
C     DTT       SET     REAL   ARG - EXPORT   TIME FROM TSTRT2 TO PRESENT TIME
C     EA        SET     REAL   LOCAL          INTERMEDIATE VARIABLE=4*DX*LAMDA
C     GAMMA     USED    REAL   ARG - IMPORT   ARBITRARY CONSTANT USED IN CON- 
C                                             VOLUTION INTEGRAL CALCULATIONS  
C     OUTFLOW   USED    REAL   COMMON - IMP   WATER FLOW RATE LEAVING THE
C                                             WETLAND MEDIUM TO BOTH SURFACE
C                                             WATER AND GROUNDWATER (CM^3/DAY)
C     P1        USED    REAL   LOCAL          LOWER INTEGRATION LIMIT         
C     P9        USED    REAL   LOCAL          UPPER INTEGRATION LIMIT         
C     T3        SET     REAL   ARG - EXPORT   LOWER LIMIT ON CONVOLUTION      
C                                             INTEGRAL                        
C     T33       SET     REAL   ARG - EXPORT   
C     TSTRT2    SET     REAL   COMMON - EXP   STARTING TIME FOR INTEGRATION=P1
C     TEND1     SET     REAL   COMMON - EXP   END TIME FOR SOURCE TERM INPUT  
C                                             INTO PRESENT MEDIA              
C     TEND2     SET     REAL   COMMON - EXP   ENDING TIME FOR INTEGRATION = P9
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     02/23/89     JWB  Added P9 to TEND2 to include offset in release time
C     03/26/89     JWB  Move the response functionalong by DT if T2 > P3      
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C     01/28/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/15/94     JPM  Time Specific Modification - T2(M1) is reset
C                       if the time specific modification is invoked.  JPM/GW
C     05/02/94     JPM  Made changes to the setting of TEND2 - if the user
C                       enters a TFINAL (other than the default value), TEND2
C                       is set to TFINAL for the last medium.  Also, TEND2 is
C                       now truncated to TFINAL only for the last medium.
C                       Don't want to truncate any fluxes, since the tails
C                       still affect concentrations.
C     07/28/95     JPM  Writing error message to the screen and the *.WRN file
C                       in addition to the *.WLS file.
C     09/26/96     JPM  Replaced the call to LIMIT1 with a call to KLIMIT1,
C                       which computes P1 and P9 based on the peak
C                       concentration of the response curve.  Taking the time
C                       shift from the previous medium into account when
C                       setting TEND2, to prevent trailing zero concentrations
C                       in the *.POL file.  Also, pasing HW and ISET through
C                       to KLIMIT1 (RAD-E049).
C     02/27/97     JPM  Replaced variable QOUT with OUTFLOW for wetlands.
C                       QOUT was representing the total outflow to surface
C                       water and groundwater.  Variable OUTFLOW now
C                       represents the total outflow, while QOUT is limited
C                       to just the outflow to surface water (RAD-E053).
C     05/21/97     JPM  Making sure TEND2 is at least as large as TEND1+P9.
C                       TEND2 was less than TEND1+P9 under some conditions
C                       for the first medium, and thus not conserving mass
C                       (RAD-P069).
C     07/21/98     JPM  Made provisions for river media - ending limit was
C                       being set too large (PH1-P015).
C     08/24/98     JPM  Removed the change to TEND2 done for RAD-E049.  Was
C                       causing the curve to be ended early in some cases
C                       (PH1-P017).
C     09/25/04     JPM  Making sure TEND2 never exceeds TFINAL, otherwise the
C                       tail of the response curve is not used in the
C                       convolution causing mass convervation problems.  Also
C                       applying TFINAL to total time, not medium time
C                       (PH1-P025).
C                       
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE LIMITS (GAMMA,T3,DTT,MED,T33,hw,iset)
C
C==== COMMON Block Definitions ===============================================
C
CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
      INCLUDE 'WETLAN.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION P3(MAXCON),MED(15),T33(MAXCON)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      IF (IT .GT. 1) GO TO 359
      IF (MED(KMED) .ne. 7) GO TO 54
C     GAMMA=30.
C*****************************************************************************
C     IF U1 = 0., THEN THE PRECIPITATION GENERATED LEACH RATE IS ZERO.
C*****************************************************************************
      tpeak(m1) = volw / outflow / 365.25
      goto 55
54    IF (U1 .GT. 0.) GO TO 265
      TPEAK(M1)=0.
55    P1=0.
C*****************************************************************************
C     P9 IS SET TO 20 AS TO ENSURE IT IS NOT ASSIGNED AS ZERO ONLY
C*****************************************************************************
      IF ((U1 .EQ. 0.) .AND. (MED(KMED) .NE. 7)) P9=20.
      P9=20./P9
      GO TO 266
  265 CONTINUE
      IF (MED(KMED).EQ.5.OR.MED(KMED).EQ.6) THEN
        P1 = 0.
        P9 = 1.0E-20
        TPEAK1(M1) = X2/U1
        TPEAK(M1) = X2/U1
      ELSE
        EA=4.*E1*ALAM
C*****************************************************************************
C     TIME TO PEAK CONCNETRATION USING EQ. 2.60 (WHELAN ET AL. 1996)
C*****************************************************************************
        TPEAK1(M1)=(SQRT((U1*X2)**2.+EA*X2**2.+E1**2.)-E1)/(EA+U1**2.)
        tpeak(m1) = x2 / u1
C*****************************************************************************
C     KLIMIT1 USES THE PEAK CONCENTRATION OF THE RESPONSE CURVE TO ESTIMATE
C     INTEGRATION LIMITS
C*****************************************************************************
        CALL KLIMIT1 (P1,P9,hw,iset)
CKJC  Added for dll compilation
        if (ERRORSIG) goto 9999
      ENDIF
  266 CONTINUE
C*****************************************************************************
C     TSTRT2(M1) AND TEND2(M1) ARE LIMITS OF INTEGRATION BASED ON X-
C     GREEN'S FUNCTION                                                    
C*****************************************************************************
      IF (P9 .GT. 0.) GO TO 20
      WRITE(6,22) M1,P1,P9
      WRITE(11,22) M1,P1,P9
      CALL MESSAGE ()
   22 FORMAT (
     1 /2x,'ERROR --> M1 = ',I2,' P1 = ',1PE10.2,' P9 = ',E10.2,
     2 /2x,'Error in M1 computation, subroutine LIMITS')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
   20 IF (P1 .LT. 0.) P1=0.
C*****************************************************************************
C     STARTING AND ENDING TIMES OF SUBSEQUENT POLLUTOGRAPHS LIMIT COMPUTATIONS
C     TO 100 LIFETIMES (TFINAL YEARS)
C*****************************************************************************
      TSTRT2(M1)=P1
      IF (P9 .GT. TFINAL) P9=TFINAL
C*****************************************************************************
C     THIS CALCULATION IS EMPIRICAL AND IS USED TO ENSURE THAT THE
C     CALCULATIONS HAVE PROCEEDED LONG ENOUGH.  JWB 3-26-1989
C*****************************************************************************
      TEND2(M1)=P9
      P3(M1)=P9
C*****************************************************************************
C     P9 IS ADDED TO TEND2(M1) TO INCLUDE THE OFFSET FROM THE RELEASE TIME
C                           02/23/89 BY JWB
C*****************************************************************************
      IF( KMED .EQ. 1 ) THEN
        IF (TEND2(M1).LT.TEND1(M1)+P9) TEND2(M1)=TEND1(M1)+P9
      ELSE
        P55=TEND1(M1)+TPEAK(M1)
        IF( P9 .GT. P55 ) THEN
          TEND2(M1)=P9
        ELSE
          TEND2(M1)=P55
        ENDIF
        IF((TEND2(M1).GT.TFINAL-TSTRT2(M1)).AND.(KMED.EQ.NMED)) then
          TEND2(M1)=TFINAL-TSTRT2(M1)
        endif
      ENDIF
C*****************************************************************************
C     MAKE SURE TEND2(M1) NEVER EXCEEDS TFINAL.  IF IT DOES, MASS
C     MAY NOT BE CONSERVED BECAUSE THE TAIL OF THE RESPONSE CURVE WOULD NOT
C     BE USED IN THE CONVOLUTION. THIS IS BECAUSE P9 (THE ENDING TIME ON THE
C     RESPONSE CURVE CANNOT EXCEED TFINAL.  ALSO, MAKE THE TFINAL LIMIT
C     APPLY TO TOTAL TIME, NOT MEDIUM TIME, BUT CONSIDERING TRAVEL(M1).
C     (PH1-P025)
C*****************************************************************************
      IF ((TEND2(M1).GT.(TFINAL-TRAVEL(M1))).OR.(TFINAL.NE.10001)) THEN
        TEND2(M1) = TFINAL - TRAVEL(M1)
      ENDIF
C*****************************************************************************
C     IF THE STARTING TIME FOR THE INTEGRATION EXCEEDS THE ENDING TIME, THEN
C     OUTPUT CONCENTRATIONS STARTING FROM A TOTAL TIME EQUAL TO ZERO.  THIS
C     MAY RESULT IN NEGATIVE MEDIUM TIMES (PH1-P025).
C*****************************************************************************
      IF (TSTRT2(M1).GE.TEND2(M1)) THEN
        IF (TEND2(M1).LE.0.0) THEN
          TSTRT2(M1)= 0.0 - TRAVEL(M1)
        ELSE
          TSTRT2(M1) = 0.0
        ENDIF
      ENDIF
C*****************************************************************************
C     If the response curve does not overlap the flux curve during
C     convolution, zero concentrations result.  This can occur at later times
C     when the trailing edge of the response curve moves out beyond the end
C     of the flux curve.  The following change prevents this from happening
C     by taking into account the time shift (TRAVEL) from the previous
C     medium when setting TEND2 (RAD-E049) JPM.
C*****************************************************************************
C      IF (TEND2(M1)+TRAVEL(M1).GT.P9+TEND1(M1)) THEN
C        TEND2(M1) = TEND2(M1)+TSTRT2(M1)-TRAVEL(M1)
C      ENDIF
C*****************************************************************************
C     CALCULATE THE CORRESPONDING TIME FOR TPEAK FOR ESTIMATING THE MAXIMUM
C     CONVOLUTION INTEGRAND FOR USE IN CALCULATING THE ERROR TERM IN THE
C     SIMPS SUBROUTINE
C*****************************************************************************
      T33(M1)=0.
      IF (TPEAK(M1) .GT. P3(M1)) T33(M1)=TPEAK(M1)-P3(M1)
  359 CONTINUE
      DT=(TEND2(M1)-TSTRT2(M1))/NTIMES
      T2(M1)=TSTRT2(M1)+DT*(IT-1)
C
C*****************************************************************************
C     Time Specific Modification - Reset T2 if the time specific modification
C     is invoked (NTIMSPEC > 0) and the current medium is a saturated zone
C     at a well (MED(KMED)=3).  Variables NTIMSPEC and TIMSPEC are described
C     in INDEX1.WTN and CTIME.WTN respectively.  Variable KOUNT_TS is
C     described in INDEX4.WTN.  JPM  03-22-94
C*****************************************************************************
C
      IF ((NTIMSPEC.GT.0).AND.(MED(KMED).EQ.3).AND.(IT.EQ.NTIMES+1))
     cTHEN
           T2(M1)=TIMSPEC(KOUNT_TS) - TRAVEL(M1)
      ELSE
           IF (IT .EQ. NTIMES+1) T2(M1)=TEND2(M1)
      ENDIF
      IF (T2(M1) .GT. P3(M1)) GO TO 51
      DTT=T2(M1)-TSTRT2(M1)
      T3=0.
      GO TO 53
C*****************************************************************************
C     MOVE THE RESPONSE FUNCTION ALONG BY DT IF T2 IS > P3  JWB 3-26-1989
C*****************************************************************************
   51 T3=T2(M1)-P3(M1)
      DTT=T2(M1)-TSTRT2(M1)
   53 CONTINUE
      IF (NPRINT .LE. 1) GO TO 403
      WRITE(6,362) IT,M1,T2(M1),DT,T3,TSTRT1(M1),TEND1(M1),
     C P3(M1),DTT,TSTRT2(M1),TEND2(M1)
  362 FORMAT ('IT     = ',I3,/,
     1        'M1     = ',I3,/,
     2        'T2     = ',1P,E10.3,/,
     3        'DT     = ',E10.3,/,
     4        'T3     = ',E10.3,/,
     5        'TSTRT1 = ',E10.3,/,
     6        'TEND1  = ',E10.3,/,
     7        'P3     = ',E10.3,/,
     8        'DTT    = ',E10.3,/,
     *        'TSTRT2 = ',E10.3,/,
     1        'TEND2  = ',E10.3//)
  403 CONTINUE
 9999 RETURN
      END
