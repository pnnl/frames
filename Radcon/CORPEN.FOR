C     MEPAS RADCON: CORPEN.FOR            Version Date: 11-24-1991            
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE CORPEN                                    *
C                                                                            *
C  Subroutine CORPEN computes evapotranspiration (ET3C) using the Penman     *
C  method with a correction factor based on equation 3.7 from Whelan         *
C  et al. 1987.  It corrects the evapotranspiration (ET3) calculated in      *
C  subroutine PENMAN by using a revised wind function.  The corrected        *
C  Penman method is one of three methods used to calculate                   *
C  evapotranspiration.  The most conservative result is used in further      *
C  calculations.  Subroutine CORPEN is called from subroutine FAOPET.        *
C                                                                            *
C  Written by:            Gene Whelan                                        *
C                         Battelle Pacific Northwest Laboratories            *
C                         P.O. Box 999                                       *
C                         Richland, WA  99352                                *
C                                                                            *
C  Creation Date:         01/19/91 (Converted to PC)                         *
C  Last Modified:         11/24/91 - JWB                                     *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCON
C     Called by: SUBROUTINE FAOPET
C     Calls to: None
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter      Set/           Location
C     Name           Used   Type    Export/Import  Parameter Description
C     ------------  ------  ------- -------------- ---------------------------
C     C              SET    REAL    LOCAL          CORRECTION FACTOR FOR THE  
C                                                  PENMAN METHOD              
C     ET3            USED   REAL    ARG - IMPORT   EVAPOTRANSPIRATION CAL-    
C                                                  CULATED USING THE PENMEN   
C                                                  METHOD                     
C     ET3C           SET    REAL    ARG - EXPORT   CORRECTED EVAPOTRANSPIR-   
C                                                  ATION                      
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/24/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE CORPEN (UDAY,U24,URATIO,RHMAX,RS,ET3,ET3C) 
C
C==== COMMON Block Definitions ===============================================
C
C      None
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION CC(4,4,3,4),T1(2,2,2),T2(2,2),T3(2) 
C
C==== Variable Declarations ==================================================
C 
      REAL NRATIO 
C
C==== DATA Statements ========================================================
C
      DATA CC
     1  /  0.86,0.90,1.00,1.00,  0.64,0.71,0.82,0.89,
     2     0.43,0.53,0.68,0.79,  0.27,0.41,0.59,0.70,
     3     0.96,0.98,1.05,1.05,  0.78,0.86,0.94,0.99,
     4     0.62,0.70,0.84,0.93,  0.50,0.60,0.75,0.87,
     5     1.02,1.06,1.10,1.10,  0.85,0.92,1.01,1.05,
     6     0.72,0.82,0.95,1.00,  0.62,0.72,0.87,0.96,
     7     0.86,0.90,1.00,1.00,  0.69,0.76,0.85,0.92,
     8     0.53,0.61,0.74,0.84,  0.37,0.48,0.65,0.76,
     9     0.96,0.98,1.05,1.05,  0.83,0.91,0.99,1.05,
     A     0.70,0.80,0.94,1.02,  0.59,0.70,0.84,0.95,
     B     1.02,1.06,1.10,1.10,  0.89,0.98,1.10,1.14,
     C     0.79,0.92,1.05,1.12,  0.71,0.81,0.96,1.06,
     D     0.86,0.90,1.00,1.00,  0.76,0.81,0.88,0.94,
     E     0.61,0.68,0.81,0.88,  0.46,0.56,0.72,0.82,
     F     0.96,0.98,1.05,1.05,  0.87,0.96,1.06,1.12,
     G     0.77,0.88,1.02,1.10,  0.67,0.79,0.88,1.05,
     H     1.02,1.06,1.10,1.10,  0.94,1.04,1.18,1.28,
     I     0.86,1.01,1.15,1.22,  0.78,0.92,1.06,1.18,
     J     0.86,0.90,1.00,1.00,  0.79,0.84,0.92,0.97,
     K     0.68,0.77,0.87,0.93,  0.55,0.65,0.78,0.90,
     L     0.96,0.98,1.05,1.05,  0.92,1.00,1.11,1.19,
     M     0.85,0.96,1.11,1.19,  0.76,0.88,1.02,1.14,
     N     1.02,1.06,1.10,1.10,  0.99,1.10,1.27,1.32,
     O     0.94,1.10,1.26,1.33,  0.88,1.01,1.16,1.27  /
C
      W=RS
      X=UDAY
      Y=RHMAX
      Z=URATIO
C
      II=INT(W/3)*3
      IF (II .EQ. 0) II=3
      I1=II/3
      I2=I1+1
      IF (I2 .EQ. 5) I2=4
      IF(I1 .GT. 4) I1=4
      IF(I1 .EQ. 0) I1=1
      FAC1=(W-II)/3.
      JJ=INT(X/3)*3
      IF (JJ .GT. 9) JJ=9
      J1=JJ/3+1
      J2=J1+1
      IF (J2 .EQ. 5) J2=4
      FAC2=(X-JJ)/3.
      KK=INT(Y/30) * 30
      IF (KK .EQ. 0) KK=30
      K1=KK/30
      K2=K1+1
      IF (K2 .EQ. 4) K2=3
      FAC3=(Y-KK)/30.
      LL=INT(Z)
      IF (LL .EQ. 0) LL=1
      L1=LL
      L2=L1+1
      IF (L2 .EQ. 5) L2=4
      IF (L1 .GT. 4) L1=4
      FAC4=(Z-LL)
      IF (FAC4 .LT. 0.) FAC4=0.0
C*****************************************************************************
C     DETERMINE THE CORRECTION FACTOR BASD ON A REVISED WIND FUNCTION         
C*****************************************************************************
      T1(1,1,1)=CC(I1,J1,K1,L1)+FAC4*(CC(I1,J1,K1,L2)-CC(I1,J1,K1,L1))
      T1(1,1,2)=CC(I1,J1,K2,L1)+FAC4*(CC(I1,J1,K2,L2)-CC(I1,J1,K2,L1))
      T1(1,2,1)=CC(I1,J2,K1,L1)+FAC4*(CC(I1,J2,K1,L2)-CC(I1,J2,K1,L1))
      T1(1,2,2)=CC(I1,J2,K2,L1)+FAC4*(CC(I1,J2,K2,L2)-CC(I1,J2,K2,L1))
      T1(2,1,1)=CC(I2,J1,K1,L1)+FAC4*(CC(I2,J1,K1,L2)-CC(I2,J1,K1,L1))
      T1(2,1,2)=CC(I2,J1,K2,L1)+FAC4*(CC(I2,J1,K2,L2)-CC(I2,J1,K2,L1))
      T1(2,2,1)=CC(I2,J2,K1,L1)+FAC4*(CC(I2,J2,K1,L2)-CC(I2,J2,K1,L1))
      T1(2,2,2)=CC(I2,J2,K2,L1)+FAC4*(CC(I2,J2,K2,L2)-CC(I2,J2,K2,L1))
      T2(1,1)=T1(1,1,1)+FAC3*(T1(1,1,2)-T1(1,1,1))
      T2(1,2)=T1(1,2,1)+FAC3*(T1(1,2,2)-T1(1,2,1))
      T2(2,1)=T1(2,1,1)+FAC3*(T1(2,1,2)-T1(2,1,1))
      T2(2,2)=T1(2,2,1)+FAC3*(T1(2,2,2)-T1(2,2,1))
      T3(1)=T2(1,1)+FAC2*(T2(1,2)-T2(1,1))
      T3(2)=T2(2,1)+FAC2*(T2(2,2)-T2(2,1))
      T4=T3(1)+FAC1*(T3(2)-T3(1))
      C=T4
C*****************************************************************************
C     CALCULATE THE CORRECTED EVAPOTRANSPIRATION
C*****************************************************************************
      ET3C=ET3*C
      RETURN
      END
