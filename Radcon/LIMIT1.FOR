C     MEPAS RADCON: LIMIT1.FOR            Version Date: 07-28-1995
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                          SUBROUTINE LIMIT1                                 *
C                                                                            *
C  Subroutine LIMIT1 calculates the upper and lower time limits (P1, P9)     *
C  based on the inflection point of the contaminant concentration/time       *
C  curve.  It is called from subroutine LIMITS.                              *
C                                                                            *
C  Written by:       Gene Whelan                                             *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    07/28/95 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  SUBROUTINE LIMITS
C     Calls to: None
C     Common blocks referenced: DUMMY, TIME, COM21
C
C==== Significant Parameters Designation and Description =====================
C
C     Parameter Set/           Location
C     Name      Used   Type    Export/Import  Parameter Description
C     --------- ------ ------- -------------- --------------------------------
C     D10       USED   REAL    ARG - IMPORT   VALUE USED TO DETERMINE INTEGR- 
C                                             TION LIMITS FROM THE INFLECTION 
C                                             POINTS OF PNT/TIME CURVE        
C     D20       USED   REAL    ARG - IMPORT   SAME AS ABOVE                   
C     P1        SET    REAL    ARG - EXPORT   LOWER LIMIT OF INTEGRATION      
C     P9        SET    REAL    ARG - EXPORT   UPPER LIMIT OF INTEGRATION      
C     TINFL     SET    REAL    LOCAL          TIME TO INFLECTION POINT ON     
C                                             PNT/TIME CURVE                  
C     TPEAK     USED   REAL    COMMON - IMP   TIME TO PEAK CONCENTRATION      
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     01/25/89     JWB  Broke up deriviative for easier evaluation
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C     01/28/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     01/13/95     JPM  Made changes so TFINAL can be less than one year.
C     07/28/95     JPM  Writing the error message to the screen and *.WRN file
C                       in addition to the *.WLS file.
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE LIMIT1 (D10,D20,P1,P9)
C
C==== COMMON Block Definitions ==============================================
C
CKJC  Added for dll compilation
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'ERROR.WTN'
      INCLUDE 'COM21.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
C
C==== DIMENSION Statements ==================================================
C
C     None
C
C==== Variable Declarations =================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      IAA=1
      IBB=1
      C10=-1.
      IF (TPEAK1(M1) .GT. TFINAL-1.) C10=-TPEAK1(M1)/TFINAL-1.
      DO 130 I1=1,2
      IF (I1 .EQ. 1) GO TO 180
      C10=-C10
      IF (TPEAK1(M1) .LT. 1.0) C10=TPEAK1(M1)
  180 IFLAG=0
      TINFL=TPEAK1(M1)
      C20=0.
   10 IFLAG=IFLAG+1
      IF (IFLAG .GT. 1) C20=C10
      TINFL=TINFL+C20
      GO TO (160,170),I1
  160 IF (TINFL .GE. 0.) GO TO 110
      TINFL=0.
      GO TO 150
  170 if( tpeak1(m1) .ge. TFINAL ) then
        tinfl = TFINAL
        p9 = TFINAL
        goto 130
      else
        goto 110
      end if
  150 IBB=-IAA
      GO TO 90
  110 E1T4=E1*TINFL*4.
      U12=U1*2.
      E18=E1*8.
      X2U1T=X2-U1*TINFL
      f1=x2u1t/e1t4
      ALAMP=ALAM-U12*f1-4.*E1*(f1)**2.
C*****************************************************************************
C     THE SECOND DERIVATIVE OF THE CURVE (A) IS BROKEN INTO SMALLER PEICES    
C     FOR EASIER EVALUATION --- 1/25/89 JWB
C*****************************************************************************
      TERM1 = U12*U1/E1T4
      TERM2 = U12/E1T4*E18*F1
      TERM3 = 4.0*E1/E1T4*E18*F1**2.0
      TERM4 = ALAMP**2.0
      TERM5 = ALAMP/TINFL
      TERM6 = 0.75/TINFL**2.0
      A = -(TERM1+TERM2+TERM3)+TERM4+TERM5+TERM6
      IF (IFLAG .GT. 1) GO TO 60
      IF (A) 30,40,50
   30 IAA=-1
      GO TO 10
   40 WRITE(6,200) M1,I1,TPEAK1(M1),TINFL,A
      WRITE(11,200) M1,I1,TPEAK1(M1),TINFL,A
      CALL MESSAGE ()
  200 FORMAT (
     1 /2x,'ERROR --> M1 = ',I2,' I1 = ',I2,' TPEAK1(M1) = ',1PE10.2,
     2 /2x,'          TINFL = ',E10.2,' A = ',E10.2,
     3 /2x,'The peak time cannot be the inflection point and cannot',
     4 /2x,'have a second derivative of zero')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
   50 IAA=1
      GO TO 10
   60 IF (A) 70,140,80
   70 IBB=-1
      GO TO 90
  140 IBB=-IAA
      GO TO 90
   80 IBB=1
   90 CONTINUE
      ICC=IAA/IBB
      IF (ICC) 100,60,10
  100 IF (I1 .EQ. 2) GO TO 120
      P1=TINFL-D10*(TPEAK1(M1)-TINFL)
      IF (P1 .LT. 0.) P1=0.
CJPM  IF (P1 .GT. TFINAL-1.) P1=TFINAL-1.
      IF (P1 .GT. TFINAL) P1=TFINAL
      GO TO 130
  120 P9=TINFL+D20*(TINFL-TPEAK1(M1))
CJPM  IF ((P1 .GE. TFINAL-1.) .OR. (P9 .GT. TFINAL-1.)) P9=TFINAL-1.
      IF ((P1 .GE. TFINAL) .OR. (P9 .GT. TFINAL)) P9=TFINAL
  130 CONTINUE
 9999 RETURN
      END
