C     MEPAS RADCON: SSWAP.FOR             Version Date: 10-30-1996
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C        STEADY-STATE SURFACE WATER ANALYSIS PROGRAM (SSWAP)                 *
C                                                                            *
C  Subroutine SSWAP performs steady-state surface water calculations.        *
C  The calculations to determine contaminant concentrations in a river       *
C  from a point or line source located parallel to the flow direction        *
C  are based on equation 6.4 from Whelan et al. 1987.  It then adjusts       *
C  the concentration to take into account the addition of water from         *
C  river tributaries.  For flux computations, the flux coming into the river *
C  is simply decayed for the travel time in the river.  Subroutine SSWAP is  *
C  called from the main program.                                             *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Pacific Northwest Laboratories                          *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    10/30/96 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: RADCONPC
C     Calls to: None
C     Common blocks referenced: INDEX1, INDEX2, INDEX3, INDEX4, INDEX6
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import   Parameter Description
C     --------- ------ ------ --------------- --------------------------------
C     B         SET    REAL    LOCAL          INTERM VARIABLE, SET TO EITHER  
C                                             B1 OR XMIX, WHICHEVER IS SMALLER
C     B1        USED   REAL    ARG - IMPORT   WIDTH OF RIVER                  
C     CON       SET    REAL    ARG - EXPORT   THE DECAYED INITIAL SOURCE CONC
C     E1        SET    REAL    LOCAL          INTERM CALCULATION (REDEFINED)  
C                                             NOT HYDRODYNAMIC DISPERSION     
C     EY        USED   REAL    ARG - IMPORT   LATERAL DISPERSION, (SAME AS E2)
C     H         USED   REAL    ARG - IMPORT   DEPTH OF RIVER (ALSO KNOWN AS   
C                                             VARIABLE H1)                    
C     F1        SET    REAL    LOCAL          SAME AS TPEAK
C     NFLAG     SET    INT     ARG - EXPORT   TURNED ON WHEN THE COMPUTED
C                                             CONCENTRATION EXCEEDS THE DECAYED
C                                             INITIAL SOURCE CONCENTRATION
C     S         SET    REAL    ARG - EXPORT   SURFACE WATER FLUX              
C     TPEAK     USED   REAL    ARG - IMPORT   TIME TO PEAK CONCENTRATION      
C     U         USED   REAL    ARG - IMPORT   FLOW VELOCITY OF RIVER, (ALSO   
C                                             KNOWN AS VARIABLE U1)           
C     XL        USED   REAL    ARG - IMPORT   FRACTION OF SURFACE WATER SOURCE
C                                             TERM UPSTREAM OF RECEPTOR       
C     XMIX      SET    REAL    LOCAL          UNIFORM MIXING DISTANCE IN RIVER
C     XS        USED   REAL    ARG - IMPORT   DISTANCE DOWNSTREAM TO RECEPTOR 
C                                             (ALSO KNOWN AS VARIABLE X2)     
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan
C     08/12/92     JWB  Residual write statement removed (write to screen)
C     01/28/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/17/94     GW   Using B i.s.o. XMIX in calculation of concentration 
C                  AdH
C     05/23/94     JPM  Placed a check on XMIX so it is not set larger than
C                       the width of the river.
C     07/12/94     JPM  Added CON and NFLAG.  CON is the decayed initial source
C                       concentration.  When a computed concentration exceeds
C                       CON, NFLAG is turned on.
C     08/04/94     JPM  Corrected a syntax problem in the conditional that
C                       checks to see if the mixing length is larger than
C                       the river width.
C     08/25/94     JPM  Included INDEX8.WTN - contains MFLG which is set
C                       in this module.  MFLG indicates if the message is
C                       written to the screen when receptor concentrations
C                       exceed the initial source concentration.
C     08/29/94     JPM  Removed the output of the "Uniform Mixing Distance."
C     01/13/95     JPM  Made changes so a concentration of zero is returned
C                       when the integration time is equal to zero.
C     07/28/95     JPM  Writing error message to the screen and the *.WRN file
C                       in addition to the *.WLS file.
C     09/18/96     JPM  Added the condition that KMED = 1 when determining the
C                       appropriate decay condition.  This prevents double
C                       counting of source decay for the wetlands to a river
C                       scenario (RAD-P065).
C     10/30/96     JPM  Computing fluxes when MED = 6 by simply decaying the
C                       flux coming into the river using the travel time in
C                       the river (PH1-E003).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE SSWAP (U,H,XS,B1,XL,GAMMA,S,ALAM,EY,T2,M1,TPEAK,
     1                  UNITS,NDS,CON,NFLAG,CO,MED)
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
      INCLUDE 'INDEX6.WTN'
      INCLUDE 'INDEX8.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION NDS(MAXCON),NFLAG(MAXCON),CON(MAXNTI,MAXCON),CO(MAXCON)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      Y=0
      IF (XS .GT. 0.) GO TO 30
      WRITE(6,2)
      WRITE(11,2)
      CALL MESSAGE ()
    2 FORMAT (
     C /2x,'ERROR --> XS = ',1PE10.3,
     C /2x,'The distance downgradient to the receptor must be',
     C /2x,'greater than zero')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
   30 CONTINUE
      P=3.1415926
      UNITTS=1./UNITS
C*****************************************************************************
C     If the integration time is greater than zero, then compute the river
C     concentration (S)  JPM  01/13/95
C*****************************************************************************
      IF (T2.GT.0) THEN
C*****************************************************************************
C     INTERPOLATE THE VALUE OF CONTAMINANT FLUX AT RIVER SOURCE
C*****************************************************************************
        CALL INTERP (T2,QC,M1)
        B=B1
        F1=TPEAK
C*****************************************************************************
C     DETERMINE THE UNIFORM MIXING LENGTH
C*****************************************************************************
        XMIX=SQRT(0.5*EY*F1)
        IF (XMIX .GT. B1) XMIX = B1
C*****************************************************************************
C     CALCULATE THE RATIO OF DISCHARGE AT RIVER SOURCE TO THAT AT THE DOWN-
C     STREAM RECEPTOR
C*****************************************************************************
        QRATIO=U*B*H/QNEW
        QCBH=0.
C*****************************************************************************
C     CALCULATION OF MASS OF CONTAMINANT
C*****************************************************************************
        GG=XL*QC/B/H/U
        GG=REAL(GG)
        IF (GG .LE. 0.) GO TO 10
        F2=0.
        IF( NDS(M1) .EQ. 1 ) THEN
          F2 = F1 * ALAM
          IF( ISTYPE .GE. 3 .AND. KMED .EQ. 1) THEN
            IF( IDECAY .EQ. 1 ) THEN
              F2 = T2 * ALAM
            ELSE IF( IDECAY .EQ. 3 ) THEN
              F2 = (T2-F1) * ALAM
            END IF
          END IF
        END IF
        IF( F2 .LT. 0.0 ) F2 = 0.0
        FF = ALOG(GG) - F2
        IF( FF .LT. -70.0 ) GO TO 10
C*****************************************************************************
C     ADJUSTMENT OF MASS AND INITIAL SOURCE CONCENTRATION FOR DECAY
C*****************************************************************************
        QCBH = GG * EXPF(-F2)
        CON(IT,M1) = CO(M1) * EXPF(-F2)
C*****************************************************************************
C     A LINE SOURCE CAN BE APPROXIMATED BY A SERIES OF POINT SOURCES
C     USING EQ 6.5 FROM WHELAN ET AL. 1987.  PRELIMINARY CALCULATION
C*****************************************************************************
   10   CONTINUE
        F1=0.
        F=0.
        N=SQRT(U*B**2.*50./P**2./EY/XS)
        NN=N/2
        NN=NN*2
        IF (NN .EQ. N) N=N+1
        IF (N .LT. 101) N=101
  620   N=N-1
        IF(N .LE. 0) GO TO 790
        E1=(N*P/B)**2.*EY/U
        IF ((E1*XS) .GT. 50.) GO TO 620
C*****************************************************************************
C     WHEN Y=0, IT IS ASSUMED THAT THE CONCENTRATION AT THE NEAR-SHORE  
C     BANK REPRESENTS THE UNIFORMLY MIXED CONCENTRATION ACROSS THE     
C    "EFFECTIVE" WIDTH OF THE RIVER; THEREFORE, THE COS TERM DROPS OUT
C*****************************************************************************
        F=EXPF(-E1*XS)*COS(N*P*Y/B)
C*****************************************************************************
C     SUMMATION OTHE TERMS IN THE SERIES
C*****************************************************************************
        F1=F1+F
        GO TO 620
C*****************************************************************************
C     EQUATION 6.5 FROM WHELAN ET AL. 1987                                       
C*****************************************************************************
  790   S=UNITTS*QCBH*(1.+2.*F1)*QRATIO
        IF (S .LT. 1.0E-20) S=0.
        IF (S.GT.CON(IT,M1).AND.ICONC(M1).EQ.3) THEN
          NFLAG(M1) = 1
          MFLG = MFLG + 1
        ENDIF
        IF (NPRINT .LE. 1) GO TO 101
C*****************************************************************************
C     PRINT STATEMENTS FOR ADDITIONAL INFORMATION                                
C*****************************************************************************
        WRITE(6,1) N,QC,U,H,B1,GAMMA,ALAM,TPEAK,
     C   EY,B,XS,XL,QCBH,T2,S,UNITS
    1   FORMAT (I5,1P11E10.2/5X,1P8E10.3)
  101   CONTINUE
C*****************************************************************************
C     SURFACE WATER FLUX CALCULATIONS
C*****************************************************************************
        IF (MED.EQ.6) S = UNITTS*QC*EXPF(-F2)
C*****************************************************************************
C    Else, the integration time (T2) must be less than or equal to zero.  In
C    this case, set the river concentration to zero (S) and leave the initial
C    source concentration undecayed.  JPM  01/13/95
C*****************************************************************************
      ELSE
        S = 0.0
        CON(IT,M1) = CO(M1)
      ENDIF
 9999 RETURN
      END
