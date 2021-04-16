C     MEPAS RADCON: SSGW.FOR              Version Date: 10-18-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                        SUBROUTINE SSGW                                     *
C                                                                            *
C  Subroutine SSGW calculates steady-state groundwater fluxes and            *
C  and concentrations for a point source for both the partially              *
C  saturated and saturated zones.  Subroutine SSGW is called from the        *
C  main program and calls subroutines FUNCTION (function BESSKO) and         *
C  INTERP.                                                                   *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    10/18/96 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to:  SUBROUTINES FUNCTION and INTERP
C     Common blocks referenced: DUMMYV, INTER, INIT, TIME, MEDIA, INPUT,
C                               INDEX2, INDEX3, INDEX4, INDEX7, INDEX8
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C    ---------- ------ -------  -------------- -------------------------------
C     ISTEDY    USED   INTEGER COMMON - IMP   INDEX INDICATING SOURCE TERM    
C                                             = 0; STEADY STATE SOURCE        
C                                             = 1; TRANSIENT SOURCE           
C     ISSTAT    USED   INTEGER COMMON - IMP   INDEX TO CHECK IF A STEADY STATE
C                                             BOUNDARY CONDITION HAS BEEN MET 
C     NUM       USED   INTEGER COMMON - IMP   FOR THE FIRST MEDIA IT IS =     
C                                             THE NUMBER OF POINTS IN PNT/TIME
C                                             TABLE.  SECOND TIME THRU IT     
C                                             RESETS TO NTIMES + 1            
C     PNT       USED   REAL    COMMON - IMP   FLUX ENTERING CURRENT MEDIA     
C     PNTT      SET    REAL    LOCAL          INTERPOLATED PNT VALUES FOR     
C                                             SPECIFIC TIMES                  
C     RDIFF     SET    REAL    LOCAL          REPRESENTS THE TIME ASSOCIATED  
C                                             WITH THE RECESSION LIMB         
C     TTIM      SET    REAL    LOCAL          DECAY TIME, VALUE DEPENDENT ON  
C                                             THE VALUE OF IDECAY             
C     ZMIX      SET    REAL    LOCAL          VERTICAL DISTANCE OVER WHICH THE
C                                             CONTAMINANT IS ASSUMED TO BE    
C                                             UNIFORMLY DISTRIBUTED           
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     03/26/89     JWB  Added statement to check for the three cases of IDECAY
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     01/28/92     JWB  Modified maximum flux calculation in saturated zone   
C     03/11/92     JWB  COMMON blocks converted to INCLUDE statements
C     03/12/92     JWB  INCLUDE statement 'INDEX1.WTN' added for ISTYPE value
C                       This change allows DMAX to be computed for ISTYPE=2. 
C     01/28/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     06/01/94     JPM  Removed DMAX.  Code computed initial concentration
C                       can be found in INPUTM.
C     07/28/95     JPM  Writing the error message to the screen and the *.WRN
C                       file in addition to the *.WLS file.
C     10/18/96     JPM  Made changes to allow medium type 2 (MED=2), which now
C                       specifies a concentration computation in the vadose
C                       zone.  Change made for the Phase 1 MEPAS waterborne
C                       executable for the framework (PH1-E001).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SSGW (C,TLIFE)
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
      INCLUDE 'INDEX7.WTN'
      INCLUDE 'INDEX8.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
      INCLUDE 'INIT.WTN'
      INCLUDE 'INPUT.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'MEDIA.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION TLIFE(MAXCON),RDIFF(MAXCON),CMAX(MAXCON)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
      DATA PI / 3.141592654 /
C
      IF( IT .EQ. 1 ) CMAX(m1) = 0.0 
      ISTAT=0 
      PNTMAX=0. 
      TTIM=0. 
C*****************************************************************************
C     CHECKING FOR THE 3 CASES OF DECAY.  JWB 3-26-1989 
C*****************************************************************************
      IF( NDS(M1) .EQ. 1 ) THEN 
        IF( IDECAY.EQ.1 ) THEN 
          TTIM = T2(M1)  
          IF( KMED.GT.1 ) THEN          
            TTIM = X2 / U1 
            IF( TTIM .GT. T2(M1) ) TTIM = T2(M1) 
          END IF 
        ELSE IF( IDECAY.EQ.2 ) THEN 
          TTIM = X2 / U1 
          IF( TTIM .GT. T2(M1) ) TTIM = T2(M1) 
        ELSE IF( IDECAY .EQ. 3 ) THEN 
          TTIM = T2(M1) - X2 / U1 
          IF( TTIM .LT. 0.0 ) TTIM = 0.0 
          IF( KMED .GT. 1 ) TTIM = 0.0 
        END IF 
      END IF
      ATIMP = -ALAM * (TTIM - X2 / U1)
C*****************************************************************************
C     FIND MAXIMUM SOURCE TERM FLUX 
C*****************************************************************************
      DO 30 II=1,NUM(M1) 
        IF( PNTMAX .LT. PNT(II,M1) ) PNTMAX = PNT(II,M1) 
   30 CONTINUE 
C*****************************************************************************
C     COMPUTE PNT FOR CORRESPONDING T2 
C*****************************************************************************
      CALL INTERP (T2(M1),PNTT,M1) 
C*****************************************************************************
C    CALCULATE MAXIMUM STEADY STATE CONCENTRATION AND FLUX BASED ON MAXIMUM 
C    FLUX AT SOURCE 
C*****************************************************************************
      GO TO (60,60,80,80,70,70), MED(KMED) 
C*****************************************************************************
C     MAXIMUM FLUX CALCUALTIONS FOR THE SATURATED ZONE 
C*****************************************************************************
80    ZMIX=SQRT(0.5*E3*X2/U1) 
      H=H1 
      IF (ZMIX .LT. H1) H=ZMIX 
      VALUE1=0.5/PI/H/SQRT(E1*E2)
C*****************************************************************************
C     EVALUATE THE VALUE OF "TERM" FOR THREE CASES:
C       1) E1 >> U1*X2 (TERM < -0.0)
C       2) E1  ÷ U1*X2 (TERM IS BETWEEN 0.0 AND +85)
C       3) E1 << U1*X2 (TERM > 85.0)
C                          01/28/92 - JWB
C*****************************************************************************
      TERM = U1*X2/2./E1
      IF( TERM .LE. 0.0 ) THEN
         VALUE2 = 1.0
      ELSE IF( TERM .GT. 0.0 .AND. TERM .LT. 85.0 ) THEN
         VALUE2 = EXPF(TERM)
      ELSE IF( TERM .GE. 85.0 ) THEN
         VALUE2 = EXPF(85.0)
      END IF
C*****************************************************************************
C     EVALUATE THE VALUE OF "GAMMA" FOR THREE CASES:
C       1) GAMMA > 85.0
C       2) GAMMA BETWEEN 0.0 AND +85
C       3) GAMMA < 0.0
C                          01/28/92 - JWB
C*****************************************************************************
      GAMMA=SQRT((U1/2./E1)**2.+ALAM/E1)*X2 
      IF( GAMMA .GE. 85.0 ) THEN
         BESSEL = BESSK0(85.0) 
      ELSE IF( GAMMA .LT. 85.0 .AND. GAMMA .GE. 0.0 ) THEN
         BESSEL = BESSK0(GAMMA) 
      ELSE IF ( GAMMA .LT. 0.0 ) THEN
         BESSEL = BESSK0(0.0) 
      END IF
      VALUE3=VALUE1*VALUE2*BESSEL
      FLUXMX=PNTMAX*EXPF(-ALAM*TTIM)/UNITS 
      GO TO 40 
C*****************************************************************************
C     MAXIMUM FLUX CALCUALTIONS FOR THE PARTIALLY SATURATED ZONE 
C*****************************************************************************
   60 VALUE1=0. 
      IF (U .GT. 0.) VALUE1=2.*U1/(U1+SQRT(U1**2.+4.*ALAM*E1))/ 
     C AL1/B1/U/AN5 
      VALUE2=H1/2./E1*(U1-SQRT(U1**2.+4*ALAM*E1)) 
      FLUXMX=PNTMAX*EXPF(-ALAM*TTIM)/UNITS 
      GO TO 40 
C*****************************************************************************
C     MED(K) CANNOT EQUAL 5, OR 6 
C*****************************************************************************
   70 WRITE(6,1) MED(KMED)
      WRITE(11,1) MED(KMED)
      CALL MESSAGE ()
    1 FORMAT (
     1 /2x,'ERROR --> MED(KMED) = ',I2,
     2 /2x,'Medium Type cannot equal 2, 5, or 6')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
C*****************************************************************************
C     CALCULATE CONCENTRATION OR FLUX FOR TIME INCREMENT I AT T2(I) 
C*****************************************************************************
   40 GO TO (160,160,180,180,70,70), MED(KMED)
C*****************************************************************************
C     CONCENTRATIONS FOR THE PARTIALLY SATURATED ZONE
C*****************************************************************************
  160 C=VALUE1*EXPF(VALUE2)
C*****************************************************************************
C     FLUXES FOR THE PARTIALLY SATURATED ZONE
C*****************************************************************************
      IF (KMED .NE. NMED) C=C*U*AL1*B1*AN5
      CLOG = 0.0
      cpu = c * pntmax / units
      if( cpu .gt. 1.0 ) clog = alog(cpu)
      ATSUM = ATIMP + CLOG
      IF( ATSUM .GT. 70.0 ) ATIMP = 70.0 - CLOG
      C=C*EXPF(ATIMP)
      GO TO 200
180   IF( KMED .EQ. NMED) THEN
C*****************************************************************************
C     IF LAST MEDIUM, CALCULATE THE CONCENTRATION FOR THE SATURATED ZONE
C*****************************************************************************
        C=VALUE3
        C=C/(A(M1)*AN5)
        CLOG = 0.0
        cpu = c * pntmax / units
        if( cpu .gt. 1.0 ) clog = alog(cpu)
        ATSUM = ATIMP + CLOG
        IF( ATSUM .GT. 70.0 ) ATIMP = 70.0 - CLOG
        C=C*EXPF(ATIMP)
C        write (*,*) 'Steady state Concentration ',c
      ELSE
C*****************************************************************************
C     FLUX CALCUALTIONS FOR THE SATURATED ZONE 
C*****************************************************************************
        C=EXPF(-ALAM*TTIM)
      END IF
  200 CONTINUE
C*****************************************************************************
C     CHECK TO SEE IF TLIFE > TPEAK:  WHERE ARE WE ON THE POLLUTOGRAPH?
C*****************************************************************************
      IF (ISTEDY(M1) .NE. 0) GO TO 210
      IF (ISSTAT(M1) .NE. 0) GO TO 210
C*****************************************************************************
C     RDIFF REPRESENTS THE BEGINNING OF THE RECESSION LIMB
C     TPEAK REPRESENTS THE END OF THE RISING LIMB
C*****************************************************************************
      RDIFF(M1)=TLIFE(M1)-TSTRT3(M1)
      IF (RDIFF(M1) .GT. TPEAK(M1)) GO TO 220
      ISSTAT(M1)=1
      GO TO 210
  220 IF ((T2(M1) .LT. TPEAK(M1)) .OR. (T2(M1) .GT. RDIFF(M1)))
     1  GO TO 210
C*****************************************************************************
C     IN STEADY-STATE PORTION OF POLLUTOGRAPH
C*****************************************************************************
      ISTAT=0
      GO TO 230
  210 ISTAT=1
  230 CONTINUE
      IF (IT .EQ. 1) C=0.
      cmx = c * pntmax / units
      if( cmx .gt. cmax(m1) ) cmax(m1) = cmx
      CONMAX = cmax(m1)
      C = C * PNTT / UNITS
 9999 RETURN
      END
