C     MEPAS RADCON: ZLINE.FOR             Version Date: 03-27-1997
C     Copyright 1989 by Battelle Memorial Institute. All rights reserved.     
C*****************************************************************************
C                                                                            *
C                            SUBROUTINE ZLINE                                *
C                                                                            *
C  Subroutine ZLINE calculates the finite width or finite depth line         *
C  source solution in the y- or z-directions with flow in the x-direction    *
C  based on equations 21 in Yeh (1981) and 30a in Yeh and Tsai (1976).       *
C  It is called from subroutine FUNCTION (function FUN).                     *
C                                                                            *
C  References:                                                               *
C                                                                            *
C  Yeh, G. T. and Y. J. Tsai.  1976.  Analytical Three-Dimensional Transient *
C  Modeling of Effluent Discharges.  Water Resources Research, Vol. 12, no.  *
C  3, p 533-40.                                                              *
C                                                                            *
C  Yeh, G. T.  1981.  AT123D:  Analytical Transient One-, Two-, and Three-   *
C  Dimensional Simulation of Waste Transport in the Aquifer System.          *
C  ORNL-5602, Oak Ridge National Laboratory, Oak Ridge, Tennessee.           *
C                                                                            *
C                                                                            *
C  Written by:       John P. McDonald                                        *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA  99352                                     *
C                                                                            *
C  Creation Date:    11/16/95 - JPM                                          *
C  Last Modified:    03/27/97 - JPM                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of:  RADCON
C     Called by:  SUBROUTINE FUNCTION (Function FUN)
C     Calls to: MESSAGE
C     Common blocks referenced: ERROR.WTN
C
C==== Significant Parameter Designation and Descriptions =====================
C
C     Parameter  Set/           Location
C     Name       Used   Type    Export/Import  Parameter Description
C     --------- ------ -------  -------------- -------------------------------
C     AVG        USED   REAL    LOCAL          PREVIOUS 5 MOVING AVERAGES -
C                                              USED TO DETERMINE CONVERGENCE
C     AVGMAX     USED   REAL    LOCAL          MAXIMUM OF THE AVG ARRAY
C     AVGMIN     USED   REAL    LOCAL          MINIMUM OF THE AVG ARRAY
C     B          USED   REAL    ARG - IMPORT   AQUIFER THICKNESS OR WIDTH (CM)
C     B1         USED   REAL    ARG - IMPORT   COORDINATE FOR THE BEGINNING
C                                              OF THE SOURCE (CM)
C     B2         USED   REAL    ARG - IMPORT   COORDINATE FOR THE END OF THE
C                                              SOURCE (CM)
C     CONVER     SET    REAL    LOCAL          CONVERGENCE CRITERION
C     DELTA      SET    REAL    LOCAL          THE ALLOWED DIFFERENCE BETWEEN
C                                              THE SUMMATION VALUE AND SUM_MIN.
C                                              IF THE DIFFERENCE IS GREATER
C                                              THAN DELTA, THE SUMMATION VALUE
C                                              IS USED, OTHERWISE G3 IS SET
C                                              EQUAL TO ZERO.
C     Dz         USED   REAL    ARG - IMPORT   Y- OR Z-DIRECTION DISPERSION
C                                              DIVIDED BY RETARDATION FACTOR
C                                              (CM**2/YR)
C     FACTx      SET    REAL    LOCAL          FACTORS IN THE EXACT SOLUTION
C     G3         SET    REAL    ARG - EXPORT   FINITE WIDTH/DEPTH LINE SOURCE
C                                              SOLUTION IN THE Y OR Z-DIRECTION
C                                              (CM**-3)
C     I          SET    INTEGER LOCAL          INDEX ON THE INFINITE SERIES
C     J          SET    INTEGER LOCAL          ROW INDEX FOR THE RMA ARRAY
C     K          SET    INTEGER LOCAL          COLUMN INDEX FOR THE RMA ARRAY
C     KK         SET    INTEGER LOCAL          INDICATES THE NUMBER OF COLUMNS
C                                              BEING USED IN THE RMA ARRAY
C     MAG        SET    INTEGER LOCAL          MAGNITUDE OF SUM_MIN
C     RMA        SET    REAL    LOCAL          A 2-D ARRAY FOR THE REPEATED
C                                              MOVING AVERAGE TECHNIQUE TO
C                                              ACCELERATE CONVERGENCE
C     ROWS       SET    REAL    LOCAL          NUMBER OF ROWS IN THE RMA ARRAY
C     SUM_MIN    SET    REAL    LOCAL          THE MINIMUM POSSIBLE SUMMATION
C                                              VALUE FOR THE INFINITE SERIES.
C                                              A SUMMATION VALUE EQUAL TO
C                                              SUM_MIN CAUSES G3 = 0, WHILE
C                                              SUMMATION VALUES LESS THAN
C                                              SUM_MIN RESULTS IN A NEGATIVE
C                                              VALUE FOR G3.
C     SUMNEW     SET    REAL    LOCAL          THE I-TH PARTIAL SUM OF THE
C                                              SERIES
C     T          USED   REAL    ARG - IMPORT   TIME (YRS)
C     TOTAL      USED   REAL    LOCAL          RUNNING TOTAL OF COLUMNS IN THE
C                                              RMA ARRAY
C     z          USED   REAL    ARG - IMPORT   Y- OR Z-COORDINATE OF RECEPTOR
C                                              (CM)
C
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     10/11/96     JPM  Made changes so the exact solution is always used.
C                       Using a repeated moving average technique to
C                       accelerate the convergence of the infinite series.
C                       This change was made because the exact solution is
C                       needed so receptor concentrations approach the source
C                       concentration in the very near field.  Also, using the
C                       previous 3 summation estimates to determine
C                       convergence  (RAD-E051).
C     03/27/97     JPM  Using the previous 5 summation estimates to determine
C                       convergence of the infinite series.  Also, if the
C                       summation estimate is within 4 digits in the 6th place
C                       of the minimum summation value (i.e., the summation
C                       value causing G3 to be equal to zero), then assume
C                       G3 is zero.  This avoids problems associated with
C                       rounding errors in the summation estimate (RAD-P066).
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE ZLINE (B,B1,B2,z,Dz,T,G3)
C
C==== Common Block Definitions ===============================================
C
      INCLUDE 'ERROR.WTN'
C
C==== DIMENSION Statements ===================================================
C
      DIMENSION RMA(30,20),TOTAL(20),AVG(5)
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
C     Check for a potential divide by zero error.  Also, make sure that
C     0 <= B1 < B2 <= B
C*****************************************************************************
C
      IF ((Dz.LE.0.0).OR.(B.LE.0.0).OR.(T.LE.0.0)) THEN
        WRITE (6,99) Dz,B,T
        WRITE (11,99) Dz,B,T
        CALL MESSAGE ()
        ERRORSIG = .TRUE.
        GOTO 9999
99      FORMAT (
     1  /2X,'ERROR --> SUBROUTINE ZLINE:',
     2  /2X,'z-Direction Dispersion (Dz) = ',1PE10.3,
     3  /2x,'Aquifer Thickness (B)       = ',1E10.3,
     4  /2x,'Integration Time (T)        = ',1E10.3,
     5  /2x,'All the above parameters must be greater than zero.')
      ENDIF
      IF (0.0.GT.B1.OR.B1.GE.B2.OR.B2.GT.B) THEN
        WRITE (6,999) B1,B2,B
        WRITE (11,999) B1,B2,B
        CALL MESSAGE ()
        ERRORSIG = .TRUE.
        GOTO 9999
999     FORMAT (
     1  /2X,'ERROR --> SUBROUTINE ZLINE:',
     2  /2X,'Starting z-coordinate of source (B1) = ',1PE10.3,
     3  /2x,'Ending z-coordinate of source (B2)   = ',1E10.3,
     4  /2x,'Aquifer Thickness (B)                = ',1E10.3,
     5  /2x,'The following condition must be met: 0 <= B1 < B2 <= B')
      ENDIF
C
C*****************************************************************************
C     Initialize variables
C*****************************************************************************
C
      CONVER = 1.0E-07
      PI = 3.14159265
      SUMNEW = 0.0
      ROWS = 30.0
      COLUMNS = 20.0
      AVGMAX = 5.0
      AVGMIN = 1.0
      DO K = 1,5
        AVG(K) = FLOAT(K)
      ENDDO
      DO K = 1,INT(COLUMNS)
        TOTAL(K) = 0.0
        DO J = 1,INT(ROWS)
          RMA(J,K) = 0.0
        ENDDO
      ENDDO
      I = 0
      J = 0
C
C*****************************************************************************
C     Compute the partial sums (SUMNEW) of the infinite series in equation 21
C     of Yeh (1981).  A repeated moving average technique is being used to
C     accelerate convergence.  If the series has not converged by the 'ROW'th
C     term, begin a moving average in column 2 of the RMA array.  If column 2
C     has not converged by the 'ROW'th*2 term, then begin another moving
C     average in the 3rd column of the RMA array - which is a moving average
C     of the second column.  Continue until all the columns are full.  If the
C     last column does not converge, continue looping until the average of the
C     last column converges.  Convergence occurs when the last 5 values all
C     differ from each other by less than the convergence factor (CONVER).
C*****************************************************************************
C
      DO WHILE ((AVGMAX-AVGMIN).GT.ABS(CONVER*AVGMAX))
        DO K = 4,1,-1
          AVG(K+1) = AVG(K)
        ENDDO
        I = I + 1
        J = J + 1
        IF (J.GT.INT(ROWS)) J = 1
        FACT1 = B/(FLOAT(I)*PI)
        IF (Z.EQ.0.0) THEN
          FACT2 = 1.0
        ELSE
          FACT2 = COS(FLOAT(I)*PI*Z/B)
        ENDIF
        IF (B2.EQ.B) THEN
          TERM1 = 0.0
        ELSE
          TERM1 = SIN(FLOAT(I)*PI*B2/B)
        ENDIF
        IF (B1.EQ.0.0) THEN
          TERM2 = 0.0
        ELSE
          TERM2 = SIN(FLOAT(I)*PI*B1/B)
        ENDIF
        FACT4 = EXP(-(FLOAT(I)*PI/B)**2.*Dz*T)
        SUMNEW = SUMNEW+FACT1*FACT2*(TERM1-TERM2)*FACT4
C
C*****************************************************************************
C     Update the columns of the RMA array as necessary.
C*****************************************************************************
C
        KK = INT(FLOAT(I)/ROWS+1.0)
        IF (KK.GT.INT(COLUMNS)) KK = INT(COLUMNS)
        DO K = 1,KK
          TOTAL(K) = TOTAL(K) - RMA(J,K)
          IF (K.EQ.1) THEN
            RMA(J,K) = SUMNEW
          ELSE
            RMA(J,K) = TOTAL(K-1) / ROWS
          ENDIF
          TOTAL(K) = TOTAL(K) + RMA(J,K)
        ENDDO
C
C*****************************************************************************
C     Take the highest level moving average as the current best sum estimate.
C     If the last column of the RMA array is full, use the average of the
C     last column.
C*****************************************************************************
C
        IF (I.LT.INT(ROWS*COLUMNS)) THEN
          AVG(1) = RMA(J,KK)
        ELSE
          AVG(1) = TOTAL(KK) / ROWS
        ENDIF
        AVGMAX = MAX(AVG(1),AVG(2),AVG(3),AVG(4),AVG(5))
        AVGMIN = MIN(AVG(1),AVG(2),AVG(3),AVG(4),AVG(5))
      ENDDO
C
C*****************************************************************************
C     Compute the minimum allowed summation value (SUM_MIN).  If the summation
C     value equals SUM_MIN, then G3 = 0.  Find the magnitude of SUM_MIN.  Then
C     find the magnitude of the sixth significant digit in SUM_MIN and set
C     DELTA equal to 4 increments of the sixth significant digit.  DELTA is
C     used to determine if the summation value (AVG(1)) is close to SUM_MIN.
C     If so, then G3 is set to zero.  This prevents instabilities as G3
C     approaches zero, because there is some error associated with AVG(1).
C*****************************************************************************
C
      SUM_MIN = (B1-B2)/2.
      IF (ABS(SUM_MIN).NE.0.0) THEN
        MAG = INT(LOG10(ABS(SUM_MIN)))
        MAG = MAG - 6
        DELTA = 4.0*(10.0**REAL(MAG))
      ELSE
        DELTA = 4.0E-06
      ENDIF
C
C*****************************************************************************
C     Final computation of equation 21 in Yeh (1981).  Note that as published,
C     equation 21 is dimensionless.  To incorporate dimensions, the published
C     equation was divided by the length of the source term (B2-B1).  If the
C     infinite series summation is greater than SUM_MIN, then set G3 to zero,
C     also set G3 to zero if the series sum is within DELTA of SUM_MIN.
C     Else compute G3.
C*****************************************************************************
C
      IF (AVG(1).LT.SUM_MIN) THEN
        G3 = 0.0
      ELSE IF ((AVG(1)-SUM_MIN).LT.DELTA) THEN
        G3 = 0.0
      ELSE
        G3 = 1./B+2./(B*(B2-B1))*AVG(1)
      ENDIF
9999  RETURN
      END
