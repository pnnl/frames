C   MEPAS RADCON: BLANEY.FOR            Version Date: 11-22-1991              
C   Copyright 1989 by Battelle Memorial Institute. All rights reserved.       
C*****************************************************************************
C                                                                            *
C                       SUBROUTINE BLANEY                                    *
C                                                                            *
C  Subroutine BLANEY computes evapotranspiration using the Blaney-Criddle    *
C  technique.  It calculates evapotranspiration (ET1) based on equation 3.2  *
C  from Whelan et al. 1987.  It also checks to see if an adjustments need to *
C  be made.  The Blaney-Criddle method is one of three methods used to       *
C  calculate evapotranspiration.  The most conservative value is eventually  *
C  chosen.  Subroutine BLANEY is called from subroutine FAOPET.              *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    11/22/91 - JWB                                          *
C                                                                            *
C*****************************************************************************
C 
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     Called by: SUBROUTINE FAOPET
C     Calls to: None
C     Common blocks referenced: None
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter   Set/           Location
C     Name        Used  Type     Export/Import  Parameter Description
C     ---------  ----- -------- -------------  ---------------------
C     ET1         SET   REAL     ARG - EXPORT   EVAPOTRANSPIRATION USING THE
C                                               BLANEY-CRIDDLE TECHNIQUE
C     F           S/U   REAL     LOCAL
C     RHMIN       USED  REAL     ARG - IMPORT   MINIMUM RELATIVE HUMIDITY
C     TMEAN       USED  REAL     ARG - IMPORT   ADJUSTED AVERAGE MONTHLY TEMP
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/22/91     JWB  Added required comments and header for MEPAS QA plan
C
C==== SUBROUTINE CALL ========================================================
C
       SUBROUTINE BLANEY (LAT,MONTH,TMEAN,RHMIN,NRATIO,UDAY,ET1) 
C
C==== COMMON Block Definitions ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION AA(3,3),BB(6,6,6),C(2,2),D(2),PP(11,12) 
C
C==== Variable Declarations ==================================================
C 
      REAL NRATIO,LAT 
C 
C==== DATA Statements ========================================================
C***********************************************************************
C THIS SECTION INTERPOLATES 'F'.
C***********************************************************************
      DATA PP
     1 / .267,.264,.261,.257,.252,.246,.239,.231,.220,.209,.195,
     2   .269,.268,.266,.264,.261,.257,.253,.248,.243,.236,.228,
     3   .269,.269,.269,.269,.269,.269,.268,.268,.268,.267,.266,
     4   .269,.270,.272,.275,.278,.282,.286,.291,.297,.303,.310,
     5   .271,.273,.276,.281,.287,.294,.303,.312,.322,.334,.346,
     6   .274,.280,.285,.291,.298,.307,.316,.328,.341,.355,.371,
     7   .275,.281,.287,.293,.299,.305,.313,.321,.330,.341,.354,
     8   .274,.278,.282,.287,.291,.295,.300,.304,.309,.315,.322,
     9   .271,.277,.280,.281,.281,.281,.281,.281,.281,.281,.281,
     A   .270,.269,.268,.267,.264,.261,.258,.254,.250,.245,.240,
     B   .269,.267,.264,.260,.254,.247,.240,.231,.222,.211,.200,
     C   .268,.266,.262,.257,.250,.242,.232,.221,.209,.195,.180 /
C***********************************************************************
C THIS SECTION INTERPOLATES ET1 USING A, B, AND F.
C***********************************************************************
      DATA BB / 0.84, 0.80, 0.74, 0.64, 0.52, 0.38,
     1          1.03, 0.95, 0.87, 0.76, 0.63, 0.48,
     2          1.22, 1.10, 1.01, 0.88, 0.74, 0.57,
     3          1.38, 1.24, 1.13, 0.99, 0.85, 0.66,
     4          1.54, 1.37, 1.25, 1.09, 0.94, 0.75,
     5          1.68, 1.50, 1.36, 1.18, 1.04, 0.84,
     6          0.97, 0.90, 0.81, 0.68, 0.54, 0.40,
     7          1.19, 1.08, 0.96, 0.84, 0.66, 0.50,
     8          1.41, 1.26, 1.11, 0.97, 0.77, 0.60,
     9          1.60, 1.42, 1.25, 1.09, 0.89, 0.70,
     A          1.79, 1.59, 1.39, 1.21, 1.01, 0.79,
     B          1.98, 1.74, 1.52, 1.31, 1.11, 0.89,
     C          1.08, 0.98, 0.87, 0.72, 0.56, 0.42,
     D          1.33, 1.18, 1.03, 0.87, 0.69, 0.52,
     E          1.56, 1.38, 1.19, 1.02, 0.82, 0.62,
     F          1.78, 1.56, 1.34, 1.15, 0.94, 0.73,
     G          2.00, 1.74, 1.50, 1.28, 1.05, 0.83,
     H          2.19, 1.90, 1.64, 1.39, 1.16, 0.92,
     I          1.18, 1.06, 0.92, 0.74, 0.58, 0.43,
     J          1.44, 1.27, 1.10, 0.91, 0.72, 0.54,
     K          1.70, 1.48, 1.27, 1.06, 0.85, 0.64,
     L          1.94, 1.67, 1.44, 1.21, 0.97, 0.75,
     M          2.18, 1.86, 1.59, 1.34, 1.09, 0.85,
     N          2.39, 2.03, 1.74, 1.46, 1.20, 0.95,
     O          1.26, 1.11, 0.96, 0.76, 0.60, 0.44,
     P          1.52, 1.34, 1.14, 0.93, 0.74, 0.55,
     Q          1.79, 1.56, 1.32, 1.10, 0.87, 0.66,
     R          2.05, 1.76, 1.49, 1.25, 1.00, 0.77,
     S          2.30, 1.96, 1.66, 1.39, 1.12, 0.87,
     T          2.54, 2.14, 1.82, 1.52, 1.24, 0.98,
     U          1.29, 1.15, 0.98, 0.78, 0.61, 0.45,
     V          1.58, 1.38, 1.17, 0.96, 0.75, 0.56,
     W          1.86, 1.61, 1.36, 1.13, 0.89, 0.68,
     X          2.13, 1.83, 1.54, 1.28, 1.03, 0.79,
     Y          2.39, 2.03, 1.71, 1.43, 1.15, 0.89,
     Z          2.63, 2.22, 1.86, 1.56, 1.27, 1.00/
C
      LL=INT(LAT/5)*5
      IF (LAT .GT. 50) LL=50
      L1=LL/5+1
      L2=L1+1
      IF (L2 .GT. 11) L2=11
      FAC1=(LAT-LL)/5.
      MONTH1=MONTH
      MONTH2=MONTH
C*****************************************************************************
C     INTERMEDIATE CALCULATIONS PRIOR TO USING EQUATION 3.2
C*****************************************************************************
      P1=PP(L1,MONTH1)+FAC1*(PP(L2,MONTH1)-PP(L1,MONTH1))
      P2=PP(L1,MONTH2)+FAC1*(PP(L2,MONTH2)-PP(L1,MONTH2))
      P=P1+0.5*(P2-P1)
      F=P*(0.46*TMEAN+8.13)
C
C*****************************************************************************
C THIS SECTION INTERPOLATES ET1 USING A, B, AND F. 
C*****************************************************************************
C
      X=RHMIN
      Y=NRATIO
      Z=UDAY
C
      I1=INT(X/20.)+1
      I2=I1+1
      IF (I2 .GT. 6) I2=6
      J1=INT(Y/0.2)+1
      J2=J1+1
      IF (J2 .GT. 6) J2=6
      K1=INT(Z/2)+1
      K2=K1+1
      IF(K2 .GT. 6) K2=6
      IF(K1 .GT. 6) K1=6
      X1=(I1-1)*20
      X2=(I2-1)*20
      Y1=(J1-1)*0.2
      Y2=(J2-1)*0.2
      Z1=(K1-1)*2
      Z2=(K2-1)*2
C
      FACX=0.
      FACY=0.
      FACZ=0.
C
      IF (K1 .NE. K2) FACZ=(Z-Z1)/(Z2-Z1)
      C(1,1)=BB(I1,J1,K1)+FACZ*(BB(I1,J1,K2)-BB(I1,J1,K1))
      C(1,2)=BB(I1,J2,K1)+FACZ*(BB(I1,J2,K2)-BB(I1,J2,K1))
      C(2,1)=BB(I2,J1,K1)+FACZ*(BB(I2,J1,K2)-BB(I2,J1,K1))
      C(2,2)=BB(I2,J2,K1)+FACZ*(BB(I2,J2,K2)-BB(I2,J2,K1))
C
      IF (J1 .NE. J2) FACY=(Y-Y1)/(Y2-Y1)
      IF (I1 .NE. I2) FACX=(X-X1)/(X2-X1)
      D(1)=C(1,1)+FACY*(C(1,2)-C(1,1))
      D(2)=C(2,1)+FACY*(C(2,2)-C(2,1))
      BP=D(1)+FACX*(D(2)-D(1))
      AP=0.0043*X-Y-1.41
C*****************************************************************************
C     CALCULATION OF POTENTIAL EVAPOTRANSIRATION, BASED ON EQUATION 
C     3.2 FROM WHELAN ET AL. 1987                                   
C*****************************************************************************
      ET1=AP+BP*F
C*****************************************************************************
C  FAO BLANEY CURVES FOR ADJUSTMENTS ARE FOR F>2.0
C*****************************************************************************
      IF(F .GE. 3.) GO TO 10
      ET1=F
C     WRITE(6,600)
  600 FORMAT('  NO ADJUSTMENT TO BLANEY DATA DONE SINCE "F" WAS <2.')
   10 RETURN
      END
