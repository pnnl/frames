C     MEPAS RADCON: FUNCTION.FOR          Version Date: 07-12-1998
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                         SUBROUTINE FUNCTION                                *
C                                                                            *
C  Subroutine FUNCTION is a compilation of all FUNCTION routines             *
C  called throughout RADCON.  It contains functions BESSI0, BESSK0,          *
C  ERFF, EXPF, FUN and SQUANK.                                               *
C                                                                            *
C*****************************************************************************
C*****************************************************************************
C                                                                            *
C                              FUNCTION BESSI0                               *
C                                                                            *
C This function represents the modified Bessel function of the second kind,  *
C zero order (I0).                                                           *
C                                                                            *
C*****************************************************************************
C
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     CALLED BY:
C     CALLS FUNCTION:
C     Comment Blocks Referenced: None
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan
C
C==== FUNCTION CALL ==========================================================
C
      FUNCTION BESSI0 (X)
C
C==== COMMENT Block Statements ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      REAL*8 Y,P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
C
C==== DATA Statements ========================================================
C
      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,3.5156229D0,3.0899424D0,
     1     1.2067492D0,0.2659732D0,0.360768D-1,0.45813D-2/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,0.1328592D-1,
     1     0.225319D-2,-0.157565D-2,0.916281D-2,-0.2057706D-1,
     2     0.2635537D-1,-0.1647633D-1,0.392377D-2/
C
      IF (ABS(X) .GE. 3.75) GO TO 10
        Y=(X/3.75)**2
        BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
      GO TO 20
   10   AX=ABS(X)
        Y=3.75/AX
        BESSI0=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4
     *      +Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
   20 CONTINUE
      RETURN
      END
C
C*****************************************************************************
C                                                                            *
C                              FUNCTION BESSK0                               *
C                                                                            *
C   This Function represents the modified Bessel function of the second kind *
C   zero order (K0)                                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     CALLED BY: SSGW
C     CALLS FUNCTION: None
C     Common Blocks Referenced: None
C                                                                             
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== FUNCTION CALL ==========================================================
C
      FUNCTION BESSK0 (X)
C
C==== COMMENT Block Statements ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      REAL*8 Y,P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7
C
C==== DATA Statements ========================================================
C
      DATA P1,P2,P3,P4,P5,P6,P7/-0.57721566D0,0.42278420D0,0.23069756D0,
     1     0.3488590D-1,0.262698D-2,0.10750D-3,0.74D-5/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,-0.7832358D-1,0.2189568D-1,
     1     -0.1062446D-1,0.587872D-2,-0.251540D-2,0.53208D-3/
C
      IF (X .GT. 2.0) GO TO 10
        Y=X*X/4.0
        BESSK0=(-LOG(X/2.0)*BESSI0(X))+(P1+Y*(P2+Y*(P3+
     *        Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      GO TO 20
   10   Y=(2.0/X)
        BESSK0=(EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+
     *        Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
   20 CONTINUE
      RETURN
      END
C*****************************************************************************
C                                                                            *
C                            FUNCTION ERFF(X)                                *
C                                                                            *
C  Function ERFF(X) calculates the error function by series aproximation.    *
C  It is called by subroutines DECAY and FUN and call the subroutine FUNCTION*
C                                                                            *
C   REF: Handbook of Mathematical Functions, U.S. Depart. of Commerce        *
C        National Bureau of Standards Applied Mathematics Series 55          *
C        December 1972, page 299, equation 7.1.26                            *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     CALLED BY: AFLUX, XLINE, YLINE
C     CALLS FUNCTION: None
C     Common Blocks Referenced: None
C                                                                             
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     03/24/89     JWB  Revised for subtraction of two error functioins       
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C
C==== FUNCTION CALL ==========================================================
C
      FUNCTION ERFF(X,ONE,ERFC)
C
C==== COMMENT Block Statements ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C
C     None
C
C==== Variable Declarations ==================================================
C
      DOUBLE PRECISION P, A(5), S, T
C
C==== DATA Statements ========================================================
C
      DATA P,A/ 3.275911D-1, 2.54829592D-1, -2.84496736D-1,
     1          1.421413741D0, -1.453152027D0, 1.061405429D0/
C*****************************************************************************
C  THIS FUNCTION WAS COMPLETELY REVISED ON 03/24/89 BY JWB TO AVOID THE       
C  THE INACCURATE SUBTRACTION OF TWO ERROR FUNCTIONS.  THE PROBLEM ARISES
C  WHEN THE TWO ERROR FUNCTIONS ARE BOTH VERY CLOSE TO 1.0 (i.e., .999999998 &
C  .999999997).  THE RESULTING DIFFERENCE COULD BE ERRONEOUS.  THESE CHANGES  
C  AVOID THIS PROBLEM.                                                        
C*****************************************************************************
      Y=ABS(X)
      T=1/(1+P*Y)
      S=A(5)*T
      DO 1 I=1,4
        S=T*(S+A(5-I))
1     CONTINUE
      B=EXP( -Y**2 )
      ERFC=-S*B
      ONE=1.
C*****************************************************************************
C     NOTE THAT ERFC(-X) = 2 - ERFC(X)
C*****************************************************************************
      S=ONE+ERFC
      IF( X .LE. 0.0 ) THEN
        S = -S
        ONE = -ONE
        ERFC = -ERFC
      ENDIF
      ERFF=S
      RETURN
      END
C*****************************************************************************
C                                                                            *
C                            FUNCTION EXPF(X)                                *
C                                                                            *
C     This function routine calculates the exponential function.             *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     CALLED BY: DECAY, COMPAR, AFLUX, YZPNT, XZPNT, XLINE, WETLND, STWL,     
C                SSGW, SSWAP, PFLUX, HOLDP
C     CALLS FUNCTION:
C     Common Blocks Referenced: None
C                                                                             
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     03/24/89     JWB  The value of X was expanded for the EXP function      
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan  
C     01/28/92     JWB  Modified the code to computehandle EXP (-85 to 85)    
C
C==== FUNCTION CALL ==========================================================
C
      FUNCTION EXPF(X)
C
C==== COMMENT Block Statements ===============================================
C
C     None
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
C*****************************************************************************
C     ADDED IF-THEN-ELSE TO HANDLE EXTREME VALUES OF X WITHOUT MATH OVERFLOWS
C*****************************************************************************
      IF( X .LT. -85.0 ) THEN
         EXPF = EXP(-85.0)
      ELSE IF (X .GE. -85.0 .AND. X .LE. 85.0 ) THEN
         EXPF = EXP(X)
      ELSE IF (X .GT. 85.0 ) THEN
         EXPF = EXP(85.0)
      END IF
      RETURN
      END

C*****************************************************************************
C                                                                            *
C                            FUNCTION FUN                                    *
C                                                                            *
C  This routine calculates the kernal function for the convolution integral. *
C  It is called by the main program  and calls subroutines INTERP and        *
C  FUNCTION (functions ERFF and EXPF).                                       *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     CALLS SUBROUTINE: SIMPS and SQUANK
C     CALLS FUNCTION: ERFF AND EXPF
C     Common Blocks Referenced: DUMMY, INTER, TIME, MEDIA, INDEX1, INDEX2
C                               INDEX3, INDEX4
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C     04/15/94 AdH/JPM  Added variable T4 to the call to YZAVE - Groundwater
C                       spatial distribution in the vertical direction change.
C     04/17/94     JPM  Added the instantaneous solution.
C     05/19/94     JPM  Added a check on W1 when the instantaneous solution
C                       is being utilized.  If W1 is .LE. zero, then return
C                       a zero.  Code crashes in XZPNT when W1 = 0.
C     06/09/94     JPM  Copied the ALEN computation into the IF portion of the
C                       instantaneous/numerical IF-THEN-ELSE block.  This
C                       computation now occurs in both branches of this
C                       conditional, since in needs to be done whether the
C                       instantaneous solution is used or not.  ALEN is the
C                       lateral mixing length in the saturated zone at a river,
C                       which is the width of the plume entering the river.
C     07/22/94     JPM  Changed ALEN from an array to a single variable.  This
C                       parameter is not contaminant specific.
C     08/29/94     JPM  Writing out ALEN to the *.WLS file.
C     09/06/94     JPM  Placed the appropriate conditionals on the writing of
C                       ALEN.
C     07/28/95     JPM  Writing the error messages to the screen and the *.WRN
C                       file in addition to the *.WLS file.
C     11/15/95     JPM  Adding in the line source solution in the z-direction
C                       (subroutine ZLINE).  Used for ISCONF = 5, y-z area
C                       source in the saturated zone.
C     05/06/96     JPM  Multiple SZ's:  moved the computation of ALEN to
C                       subroutine PLUME (RAD-E046).
C     05/07/96     JPM  Removed IFLAG for YLINE and XLINE - not allowing a
C                       switch to the point source solutions for concentration
C                       calculations (RAD-P062).
C     07/15/96     JPM  Allowing the volume source solution (XLINE * YLINE *
C                       ZLINE) for ISCONF = 6.  Also using the line source
C                       flux equation when ISCONF = 6 and MED = 4 (RAD-E048).
C     10/21/96     JPM  Made changes so fluxes or concentrations are computed
C                       for the vadose zone depending on the setting of MED.
C                       When MED=1, fluxes from the vadose zone are computed;
C                       when MED=2, concentrations in the vadose zone are
C                       computed (PH1-E001).
C     07/12/98     JPM  Corrected an error setting the time for decay (TTIM)
C                       when the instantaneous solution is being used.  If
C                       IDECAY = 1 or 2 (source and env. or env. only), TTIM
C                       should be T2, otherwise (source only) TTIM should be
C                       zero (1.0E-20) (PH1-P014).
C
C==== FUNCTION CALL ==========================================================
C
      FUNCTION FUN (T3,HW,ISET)
C
C==== COMMENT Block Statements ===============================================
C
      INCLUDE 'MAXIMUM.WTN'
      INCLUDE 'INDEX1.WTN'
      INCLUDE 'INDEX2.WTN'
      INCLUDE 'INDEX3.WTN'
      INCLUDE 'INDEX4.WTN'
      INCLUDE 'CTIME.WTN'
      INCLUDE 'DUMMY.WTN'
      INCLUDE 'INTER.WTN'
      INCLUDE 'MASSS.WTN'
      INCLUDE 'MEDIA.WTN'
      INCLUDE 'WELLDIST.WTN'
CKJC  Added for dll compilation
      INCLUDE 'ERROR.WTN'
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
C*****************************************************************************
C
C     Compute RATIO as the time to peak divided by the total time over
C     which the contaminant enters the medium.  If RATIO is greater than or
C     equal to RRATIO, and we have a release to groundwater (ISTYPE <= 2),
C     and the current medium is a PSZ or SZ (MED(KMED) =1, 3, or 4), then
C     implement the instantaneous solution.  04-17-94  JPM/GW
C
C*****************************************************************************
C
      RATIO=TPEAK1(M1)/TLIF(M1)
      IF (RATIO.GE.RRATIO) THEN
           UNITSS=1./UNITS
           W1=TOTMAS(M1)
           IF (W1 .LE. 0.) THEN
                FUN = 0.
                RETURN
           ENDIF
           T4=T2(M1)
           C6=U1*T4
           C7=SQRT(T4)
           IFLAG=0
           IF (IDECAY .LT. 3) THEN
             TTIM=T2(M1)
           ELSE
             TTIM = 1.0E-20
           ENDIF
      ELSE
           IF (T3 .GT. (T2(M1)-TSTRT2(M1))) T3=T2(M1)-TSTRT2(M1)
           CALL INTERP (T3,W1,M1)
           IF (W1 .GT. 0.) GO TO 60
           FUN=0.
           RETURN
   60      UNITSS=1./UNITS
           T4=T2(M1)-T3+1.0E-10
           C6=U1*T4
           C7=SQRT(T4)
           IFLAG=0
           TTIM=0.
C*****************************************************************************
C     SET TTIM VALUE, DEPENDENT ON IDECAY
C*****************************************************************************
           IF( IDECAY .EQ. 1 ) THEN
                TTIM = T2(M1) - T3
                IF( KMED .EQ. 1) TTIM= T2(M1)
           ELSE IF( IDECAY .EQ. 2 ) THEN
                TTIM = T2(M1) - T3
           ELSE IF( IDECAY .EQ. 3 ) THEN
                TTIM = 0.0
                IF( KMED .EQ. 1 ) TTIM = T3
           ENDIF
      ENDIF
C
      IF (NDS(M1) .EQ. 1) ARG=ALAM*TTIM
      IF (MED(KMED).EQ.1.OR.MED(KMED).EQ.4) GO TO 200
C*****************************************************************************
C     CONCENTRATION CALCULATIONS BASED ON SOURCE CONFIGURATION
C*****************************************************************************
      IF (ISCONF .GE. 7) ISCONF=7
      IF (MED(KMED).EQ.2) GOTO 11
      GO TO (10,20,30,40,50,50,61),ISCONF
C
   10 GO TO (11,12,13,13,12,12,12),MED(KMED)
C
   11 CALL XZPNT (H1,T4,E1,ARG,C6,C7,C9,G1,UNITSS,W1,ALPHA)
      G2=1./B1
      G3=1./AL2/2.
      GO TO 1000
C
   12 WRITE(6,66) MED(KMED)
      WRITE(11,66) MED(KMED)
      CALL MESSAGE ()
   66 FORMAT (
     1 /2X,'ERROR --> THE MEDIUM INDEX SHOULD NOT EQUAL ',I5,
     2 /2x,'STOP IN FUNCTION FUN')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
C
   13 CALL XZPNT (X2,T4,E1,ARG,C6,C7,C9,G1,UNITSS,W1,ALPHA)
      IF (ISCONF.EQ.5) GOTO 1000
      GO TO (11,12,14,15),MED(KMED)
C
   14 CALL YZPNT (Y2,T4,E2,C7,C9,G2)
      IF (ISCONF.EQ.5) GOTO 13
      IFLAG=1
      CALL YZAVE (H1,B2,TPEAK(M1),T4,E3,IFLAG,G3,HW,ISET,YDIST)
      GO TO 1000
C
   15 IFLAG=3
      CALL YZAVE (H1,B2,TPEAK(M1),T4,E2,IFLAG,G2,HW,ISET,YDIST)
      IFLAG=1
C
      CALL YZAVE (H1,B2,TPEAK(M1),T4,E3,IFLAG,G3,HW,ISET,YDIST)
      GO TO 1000
C
   20 CALL XLINE (AL2,C3,C4,C5,C6,C7,ARG,G1,UNITSS,W1,ALPHA)
      IF (ISCONF.EQ.6) GO TO 1000
      GO TO (11,12,14,15,12,12,12),MED(KMED)
C
   30 CALL XZPNT (X2,T4,E1,ARG,C6,C7,C9,G1,UNITSS,W1,ALPHA)
      GO TO (11,12,17,18,12,12,12),MED(KMED)
17    CALL YLINE (B2,Y2,C2,C7,G2)
      IF (ISCONF.EQ.5) GO TO 13
      IF (ISCONF.EQ.6) GO TO 20
      IFLAG=1
C
      CALL YZAVE (H1,B2,TPEAK(M1),T4,E3,IFLAG,G3,HW,ISET,YDIST)
      GO TO 1000
C
   18 IFLAG=3
      CALL YZAVE (H1,B2,TPEAK(M1),T4,E2,IFLAG,G2,HW,ISET,YDIST)
      IFLAG=1
C
      CALL YZAVE (H1,B2,TPEAK(M1),T4,E3,IFLAG,G3,HW,ISET,YDIST)
      GO TO 1000
C
40    CALL XLINE (AL2,C3,C4,C5,C6,C7,ARG,G1,UNITSS,W1,ALPHA)
      GO TO (11,12,17,18,12,12,12),MED(KMED)
C
50    CALL ZLINE (H1,ZTOP,ZBOT,Z1,E3,T4,G3)
      if (errorsig) goto 9999
      GOTO 17
C
   61 WRITE(6,68)
      WRITE(11,68)
      CALL MESSAGE ()
   68 FORMAT (
     1 /2x,'ERROR --> ISCONF MUST BE < 7: STOP IN FUNCTION FUN')
CKJC  Added for dll compilation
      ERRORSIG=.TRUE.
      goto 9999
CKJC      STOP
C
 1000 CONTINUE
      IF (NPRINT .LE. 0) GO TO 401
c      IF (KMED .GT. 1 .AND. IT .GT. 1)
       WRITE(6,99) W1,G1,G2,G3,ALPHA,UNITSS,MED(KMED),ISCONF,KMED
   99 FORMAT (/2X,'W1 = ',1PE10.3/2X,'G1 = ',1PE10.3/2X,'G2 = ',
     1 1PE10.3/2X,'G3 = ',1PE10.3/2X,'ALPHA = ',1PE10.3/2X,'UNITSS = ',
     1 1PE10.3/2X,'MED = ',I5/2X,'ISCONF = ',I5/2X,'K = ',I5)
  401 CONTINUE
      FUN=G1*G2*G3
      RETURN
C*****************************************************************************
C     FLUX CALCULATIONS
C*****************************************************************************
200   IF (ISCONF .GE. 7) ISCONF=7
      GO TO (110,120,110,120,110,120,61),ISCONF
C
110   GO TO (111,12,12,111,12,12,12,12),MED(KMED)
C
111   CALL PFLUX (X2,C3,C6,C7,C9,ARG,F,UNITSS,W1)
      GO TO 2000
C
120   GO TO (111,12,12,121,12,12,12,12),MED(KMED)
121   CALL AFLUX (C3,C4,C5,C6,C7,U1,E1,T4,AL2,ARG,IFLAG,F,
     1            UNITSS,W1)
      IF (IFLAG .EQ. 6) GO TO 111
C
      GO TO 2000
C
2000  FUN=F
C
      IF (NPRINT .LE. 1) GO TO 31
      IF (M1 .NE. 1) GO TO 31
      IF (IT .NE. 2) GO TO 31
      WRITE(6,32) FUN,UNITSS,W1,F
   32 FORMAT (2X,'FUN =  ',1PE10.3,5X,'UNITSS =  ',1PE10.3,5X,
     1        'W1 =  ',1PE10.3,5X,'F =  ',1PE10.3)
   31 CONTINUE
      RETURN
CKJC  Added for dll compilation
9999  END
C*****************************************************************************
C                                                                            *
C                           FUNCTION SQUANK                                  *
C                                                                            *
C  Function SQUANK (Simpson Quadrature Used Adaptively--Noise Killed) is an  *
C  automatic numerical quadrature routine with a feature of a roundoff error *
C  guard against the effects of excessive roundoff erross in function values.*
C                                                                            *
C  Reference:  ALGORITHM 379, CACM VOL.13,NO.4,APRIL 1970, PP.260-263        *
C                                                                            *
C  Developed by:          J. N. LYNESS                                       *
C                         ARGONNE NATIONAL LABORATORY                        *
C                         ARGONNE, IL  (1970)                                *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: RADCONPC
C     CALLS SUBROUTINE:
C     CALLS FUNCTION:
C     Common Blocks Referenced: None
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     11/25/91     JWB  Added required comments and header for MEPAS QA plan
C     01/27/94     KJC  Added lines so no stop is used ERRORSIG is set instead
C
C==== FUNCTION CALL ==========================================================
C
      FUNCTION SQUANK(A,BIG,ERROR,FIFTH,NO,RUM,HW,ISET)
C
C==== COMMENT Block Statements ===============================================
C
C     None
C
C==== DIMENSION Statements ===================================================
C

CKJC  Added for dll compilation
      INCLUDE 'ERROR.WTN'
      DIMENSION FX3ST(30),X3ST(30),FX5ST(30),ESTST(30)
      DIMENSION X5ST(30),PREDIF(30)
C
C==== Variable Declarations ==================================================
C
C     None
C
C==== DATA Statements ========================================================
C
C     None
C
      EPMACH=7.5E-12
C*****************************************************************************
C     STAGE ONE
C*****************************************************************************
      SUM=0.
      SIM=0.
      CEPSF=180.  *ERROR/(BIG-A)
      CEPS=CEPSF
      ADIFF=0.
      LEVTAG=-1
      FACERR=1.
      XZERO=A
      EFACT=0.
      NIM=1
      LEV=0
      X1=A
      X5=BIG
      X3=0.5  *(A+BIG)
      FX1=FUN(X1,HW,ISET)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
      FX3=FUN(X3,HW,ISET)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
      FX5=FUN(X5,HW,ISET)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
      NO=3
      EST=FX1+FX5+4.  *FX3
C*****************************************************************************
C     STAGE TWO
C*****************************************************************************
      IF(CEPSF) 50,10,50
   10 LEVTAG=0
      FACERR=15.0
      CEPS=EPMACH* ABS(FX1)
      IF(FX1) 50,20,50
   20 CEPS=EPMACH* ABS(FX3)
      LEVTAG=3
      IF(FX3) 50,30,50
   30 CEPS=EPMACH* ABS(FX5)
      IF(FX5) 50,40,50
   40 CEPS=EPMACH
   50 QCEPS=0.25  *CEPS
C*****************************************************************************
C     STAGE THREE
C*****************************************************************************
   60 X2=0.5  *(X1+X3)
      X4=0.5  *(X3+X5)
      FX2=FUN(X2,HW,ISET)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
      FX4=FUN(X4,HW,ISET)
CKJC  Added for dll compilation
      if (ERRORSIG) goto 9999
      NO=NO+2
      EST1=FX1+4.0  *FX2+FX3
      EST2=FX3+4.0  *FX4+FX5
      ADIFF1=ADIFF
      DIFF=EST+EST-EST1-EST2
      IF(LEV-30) 70,320,320
   70 ADIFF= ABS(DIFF)
      CRIT=ADIFF-CEPS
      IF(CRIT) 180,180,80
C*****************************************************************************
C     STAGE FOUR
C*****************************************************************************
   80 IF(ADIFF1-ADIFF) 90,90,170
   90 IF(LEV-5) 170,100,100
  100 EFACT=EFACT+CEPS*(X1-XZERO)*FACERR
      XZERO=X1
      FACERR=15.0
      IF(ADIFF-2.0  *CEPS) 110,110,120
  110 CEPS=ADIFF
      LEVTAG=0
      GO TO 310
  120 IF(ADIFF1-ADIFF) 140,130,140
  130 CEPS=ADIFF
      GO TO 160
  140 CEPS=2.0  *CEPS
      IF(LEVTAG-3) 150,160,160
  150 LEVTAG=2
  160 QCEPS=0.25  *CEPS
C*****************************************************************************
C     STAGE FIVE
C*****************************************************************************
  170 NIM=2*NIM
      LEV=LEV+1
      ESTST(LEV)=EST2
      X3ST(LEV)=X4
      X5ST(LEV)=X5
      FX3ST(LEV)=FX4
      FX5ST(LEV)=FX5
      PREDIF(LEV)=ADIFF
C*****************************************************************************
C     STAGE SIX
C*****************************************************************************
      X5=X3
      X3=X2
      FX5=FX3
      FX3=FX2
      EST=EST1
      GO TO 60
C*****************************************************************************
C     STAGE SEVEN
C*****************************************************************************
  180 IF(LEV) 80,80,190
  190 IF(LEVTAG) 320,200,200
  200 CEPST=15.  *CEPS
      IF(CRIT) 210,320,320
  210 IF(LEVTAG-2) 220,260,270
  220 IF(ADIFF) 320,320,230
  230 IF(ADIFF-QCEPS) 240,320,320
  240 IF(ADIFF-CEPSF) 290,290,250
  250 LEVTAG=0
      CEPS=ADIFF
      EFACT=EFACT+CEPST*(X1-XZERO)
      XZERO=X1
      GO TO 160
  260 LEVTAG=0
      IF(ADIFF) 280,280,230
  270 LEVTAG=0
      IF(ADIFF) 300,300,240
  280 CEPS=ADIFF1
      GO TO 300
  290 LEVTAG=-1
      FACERR=1.
      CEPS=CEPSF
  300 EFACT=EFACT+CEPST*(X1-XZERO)
      XZERO=X1
  310 QCEPS=0.25  *CEPS
C*****************************************************************************
C     STAGE EIGHT
C*****************************************************************************
  320 SUM=SUM+(EST1+EST2)*(X5-X1)
      IF(LEVTAG) 330,350,350
  330 SIM=SIM+DIFF*(X5-X1)
C*****************************************************************************
C     STAGE NINE
C*****************************************************************************
  350 NUM=NIM/2
      NOM=NIM-2*NUM
      IF(NOM) 360,370,360
  360 NIM=NUM
      LEV=LEV-1
      GO TO 350
  370 NIM=NIM+1
      IF(LEV) 390,390,380
C*****************************************************************************
C     STAGE TEN
C*****************************************************************************
  380 X1=X5
      FX1=FX5
      X3=X3ST(LEV)
      X5=X5ST(LEV)
      FX3=FX3ST(LEV)
      FX5=FX5ST(LEV)
      EST=ESTST(LEV)
      ADIFF=PREDIF(LEV)
      GO TO 60
C*****************************************************************************
C     STAGE ELEVEN
C*****************************************************************************
  390 EFACT=EFACT+CEPS*(BIG-XZERO)*FACERR
      RUM=EFACT/180.
      THIRD=SUM/12.
      FIFTH=-SIM/180.
      SQUANK=THIRD+FIFTH
CKJC  Added for dll compilation
 9999 RETURN
      END
