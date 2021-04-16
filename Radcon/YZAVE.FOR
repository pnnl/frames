C     MEPAS RADCON: YZAVE.FOR             Version Date: 08-29-1994
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE YZAVE                                *
C                                                                            *
C  Subroutine YZAVE computes the uniform mixing length for lateral direction *
C  and the spacially varying concentration in the vertical direction both    *
C  estimated by using the advection-dispersion equation and a Gaussian       *
C  distribution solution. It is called from subroutine FUNCTION              *
C  (function FUN).                                                           *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Northwest Laboratories                         *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    08/29/94 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE FUNCTION
C     Calls to: None
C     Common blocks referenced: INDEX3, INDEX4
C
C==== Significant Parameter Designation and Descriptions =====================
C     
C     Parameter  Set/           Location
C     Name       Used   Type    Export/Import  Parameter Description
C     --------- ------ -------- -------------- -------------------------------
C     B2         USED   REAL    ARG - IMPORT   WIDTH                          
C     BB         SET    REAL    LOCAL          INTERM VALUE DEPENDENT ON IFLAG
C     G          SET    REAL    ARG - EXPORT   MIXING DISTANCE OR (IN CASE OF
C                                              VERTICAL DIRECTION) VERTICAL
C                                              COMPONENT OF CONCENTRATION BASED
C                                              ON CONCEPT OF SPATIALLY VARYING
C                                              CONCENTRATION IN VERTICAL 
C                                              DIRECTION
C     IFLAG      USED   INTEGER ARG - IMPORT   INDEX ON VERTICAL OR LATERAL   
C                                              AVERAGING;                     
C                                              = 1; VERTICAL AVERAGING IN GW  
C                                                   AT WELL OR RIVER          
C                                              = 2; LATERAL AVERAGING IN SW   
C                                              = 3; LATERAL AVERAGING OF GW   
C                                                   AT RIVER INTERFACE        
C     XMIX       SET    REAL    LOCAL          MIXING DISTANCE                
C     YDIST      USED   REAL    ARG - IMPORT   THICKNESS OF AQUIFER           
C     T4         USED   REAL    ARG - IMPORT   CONVERTED TIME
C     E2         USED   REAL    ARG - IMPORT   VERTICAL/LATERAL DISPERSION
C     Z1         USED   REAL    ARG - COMMON   DEPTH BELOW THE WATERTABLE OF
C                                              CALCULATED CONCENTRATION AT
C                                              RECEPTOR
C     N          SET    INTEGER LOCAL          INDEX ON SERIES EXPANSION
C     N2         SET    INTEGER LOCAL          INDEX ON SERIES EXPANSION
C     A          SET    REAL    LOCAL          EXPONENT IN AT123D FORMULA
C     A1         SET    REAL    LOCAL          SUMMATION-TERM IN AT123D
C                                              FORMULA
C     B          SET    REAL    LOCAL          EXPONENT IN AT123D FORMULA
C     B1         SET    REAL    LOCAL          SUMMATION-TERM IN AT123D
C                                              FORMULA
C     C          SET    REAL    LOCAL          EXPONENT IN AT123D FORMULA
C     C1         SET    REAL    LOCAL          SUMMATION-TERM IN AT123D
C                                              FORMULA
C     F          SET    REAL    LOCAL          EXPONENT IN FORMULA (*.*)
C     F1         SET    REAL    LOCAL          SUMMATION-TERM IN FORMULA (*.*)
C     Q          SET    REAL    LOCAL          TERM IN AT123D FORMULA
C     R          SET    REAL    LOCAL          TERM IN AT123D FORMULA
C     S          SET    REAL    LOCAL          TERM IN AT123D FORMULA
C     DENOM      SET    REAL    LOCAL          DENOMENATOR IN AT123D FORMULA
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/26/91     JWB  Added required comments and header for MEPAS QA plan  
C     04/15/94     GW   Changed vertical averaging of contaminant concentration
C                  AdH  into spatially varying concentration in the vertical 
C                       dimension if at well
C     08/29/94     JPM  Removed the output of the "Unform Mixing Distance."
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE YZAVE (H1,B2,T,T4,E2,IFLAG,G,HW,ISET,YDIST)
C
C==== COMMON Block Definitions ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      BB=0.
      CC=2.0
      IF (IFLAG .EQ. 3) BB=2.0*B2
      IF (IFLAG .LT. 3) CC=0.5
C*****************************************************************************
C     CALCULATE MIXING DISTANCE USING ADVECTION-DISPERSION EQUATION AND
C     A GAUSSIAN DISTRIBUTION
C*****************************************************************************
      XMIX=SQRT(CC*E2*T)+BB+HW
C*****************************************************************************
C     CHECK TO SEE IF AQUIFER DEPTH IS GREATER THAN MIXING LENGTH
C*****************************************************************************
      IF (YDIST.GT.XMIX) XMIX=YDIST
      GO TO (1,2,3),IFLAG
C*****************************************************************************
C     VERTICAL AVERAGING OF CONTAMINANT CONCENTRATION IF AT WELL
C*****************************************************************************
C   1 G=H1
C     IF (XMIX .LT. H1) G=XMIX
C*****************************************************************************
C     SPATIALLY VARYING CONCENTRATION IN THE VERTICAL DIRECTION
C*****************************************************************************
    1 CONTINUE
      P=3.1415926
      T5=(H1**2/P/E2)
CKJC  Check values
C     write(*,*) "Check ",E2,T4,H1,T5
      F1=0.
      F=0.
      A1=0.
      A=0.
      B1=0.
      B=0.
      C1=0.
      C=0.
      DENOM=4.*E2*T4
      N=SQRT(H1**2.*50./P**2./E2/T4)
      N2=SQRT(50.*E2*T4/H1**2.)+Z1/H1
C     WRITE (6,4004) N,N2,Z1,DENOM
C4004 FORMAT (/2X,'N = ',1I10/2X,'N2 = ',1I10/2X,'Z1 = ',1PE10.3/
C    1       2X,'DENOM = ',1PE10.3) 
      IF (N .LE. N2 .OR. N .LE. 500) THEN
C       WRITE (*,*) "FORMULA 1 IS USED"
        IF (N .LT. 101) N=101
 1002   N=N-1
        IF(N .LE. 0) GO TO 1003
        E1=(N*P/H1)**2.*E2*T4
        IF (E1 .GT. 50.) GO TO 1002
C*****************************************************************************
C     WHEN Z1=O, IT IS ASSUMED THAT THE CONCENTRATION AT THE TOP OF THE 
C     SATURATED ZONE REPRESENTS THE UNIFORMILY MIXED CONCENTRATION ACROSS THE
C     "EFFECTIVE" DEPTH OF THE LAYER; THEREFOR THE COS TERM DROPS OUT
C*****************************************************************************
        F=EXPF(-E1)*COS(N*P*Z1/H1)
C     SUMMATION OF THE TERMS IN THE SERIES
C*****************************************************************************
        F1=F1+F
C       WRITE(*,*) "F F1 ",F,F1
        GO TO 1002
C*****************************************************************************
C     EQUATION 6.5 FROM WHELAN ET AL. 1987
C*****************************************************************************
 1003   G=1./H1*(1.+2.*F1)
C***************************************************************************** 
C  PRE-WORK FOR EQUATION AT123D DOCUMENT
C*****************************************************************************
      ELSE
C       WRITE (*,*) "FORMULA 2 IS USED"
        IF (N2 .LT. 101) N2=101
 1005   N2=N2-1    
        IF (N2 .LE. 0) GO TO 1006
        E1=(Z1-2.*N2*H1)**2./4./E2/T4
C       IF (E1 .GT. 50.) GO TO 1005
        Q=(Z1-2.*N2*H1)**2./DENOM
        R=(Z1-2.*(N2-1)*H1)**2./DENOM
        S=(Z1-2.*(N2+1)*H1)**2./DENOM
        A=EXPF(-Q)
        A1=A1+A
        B=EXPF(-R)
        B1=B1+B
        C=EXPF(-S)
        C1=C1+C
C       WRITE (*,*) A1,B1,C1
C7007   FORMAT (/2X,'A1 = '1PE10.3/2X,'A2 = '1PE10.3/2X,'A3 = '1PE10.3)
        GO TO 1005
C***************************************************************************** 
C     EQUATION AT123D DOCUMENT
C*****************************************************************************
 1006   G=(1./SQRT(4.*E2*T4))*(2*A1+B1+C1)
      ENDIF
      GO TO 21
C*****************************************************************************
C     LATERAL AVERAGING OF CONTAMINANT CONCENTRATION FOR SURFACE WATER
C*****************************************************************************
    2 G=2.*B2
      IF (XMIX .LT. (2.*BB)) G=XMIX
      GO TO 20
C*****************************************************************************
C     LATERAL AVERAGING OF CONCENTRATIONS IN GROUNDWATER AT RIVER BOUNDARY
C*****************************************************************************
    3 G=XMIX
   20 IF (ISET .EQ. KMED) GO TO 101
      ISET=KMED
  101 G=1./G
   21 CONTINUE
      RETURN
      END
