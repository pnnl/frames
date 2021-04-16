      SUBROUTINE S1SORT2( X, Y1, Y2,Y3,Y4, N )
!!**********************************************************************
!!
!!  Purpose:
!!
!!    SSORTI sorts array X and optionally makes the same interchanges
!!    in array Y.  The array X may be sorted in increasing order or
!!    decreasing order.  A modified QUICKSORT algorithm is used.
!!    The vector X is real and the vector Y is integer.
!!
!!  Reference:
!!
!!    Singleton, R. C.
!!    Algorithm 347
!!    "An Efficient Algorithm for Sorting with Minimal Storage"
!!    Comm. Assoc. Comput. Mach.
!!    Vol. 12, No. 3, 1969, pp. 185-187.
!!
!!
!!  Variable Descriptions:
!!
!!    X     : real array of values to be sorted
!!    Yn    : integer arrays to be carried along
!!    N     : number of values in array X to be sorted
!!    KFLAG : control parameter
!!            =  2 means sort X in increasing order and carry Y along.
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!    BAN              : 15 Feb 2008 : Revise for PERCENTILE (add y2,3,4)
!!**********************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: N
      REAL, DIMENSION(*) :: X
      REAL, DIMENSION(*) :: Y1,Y2,Y3,Y4
!
! *** Local variables
      REAL :: TY1, TTY1,TY2,TTY2,TY3,TTY3,TY4,TTY4
      INTEGER, DIMENSION(21) :: IL, IU
      CHARACTER(LEN=6) :: CALLER = 'SSORTI' ! Name of this routine
      INTEGER :: NN, I, M, J, K, KK, IJ, L, kflag
      REAL :: R, T, TT
!
!---- Executable code ---------------------------------------------------
!
      KFLAG = 2
      NN = N
      KK = IABS(KFLAG)
!
! *** SORT X AND CARRY Y'S ALONG
!
  200 CONTINUE
      M = 1
      I = 1
      J = NN
      R = 0.375
  210 IF( I .EQ. J ) GO TO 255
  215 IF( R .GT. 0.5898437 ) GO TO 220
      R = R + 3.90625E-2
      GO TO 225
!
  220 R = R - 0.21875
  225 K = I
!
! *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
!
      IJ = I + IFIX( FLOAT(J-I) * R )
      T  = X(IJ)
      TY1 = Y1(IJ)
      TY2 = Y2(IJ)
      TY3 = Y3(IJ)
      TY4 = Y4(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 230
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      Y1(IJ) = Y1(I)
      Y1(I)  = TY1
      TY1    = Y1(IJ)
      Y2(IJ) = Y2(I)
      Y2(I)  = TY2
      TY2    = Y2(IJ)
      Y3(IJ) = Y3(I)
      Y3(I)  = TY3
      TY3    = Y3(IJ)
      Y4(IJ) = Y4(I)
      Y4(I)  = TY4
      TY4    = Y4(IJ)
  230 L = J
!
! *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
!
      IF (X(J) .GE. T) GO TO 240
      X(IJ) = X(J)
      X(J)  = T
      T     = X(IJ)
      Y1(IJ) = Y1(J)
      Y1(J)  = TY1
      TY1    = Y1(IJ)
      Y2(IJ) = Y2(J)
      Y2(J)  = TY2
      TY2    = Y2(IJ)
      Y3(IJ) = Y3(J)
      Y3(J)  = TY3
      TY3    = Y3(IJ)
      Y4(IJ) = Y4(J)
      Y4(J)  = TY4
      TY4    = Y4(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 240
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      Y1(IJ) = Y1(I)
      Y1(I)  = TY1
      TY1    = Y1(IJ)
      Y2(IJ) = Y2(I)
      Y2(I)  = TY2
      TY2    = Y2(IJ)
      Y3(IJ) = Y3(I)
      Y3(I)  = TY3
      TY3    = Y3(IJ)
      Y4(IJ) = Y4(I)
      Y4(I)  = TY4
      TY4    = Y4(IJ)
      GO TO 240
!
  235 TT=X(L)
      X(L) = X(K)
      X(K) = TT
      TTY1  = Y1(L)
      Y1(L) = Y1(K)
      Y1(K) = TTY1
      TTY2  = Y2(L)
      Y2(L) = Y2(K)
      Y2(K) = TTY2
      TTY3  = Y3(L)
      Y3(L) = Y3(K)
      Y3(K) = TTY3
      TTY4  = Y4(L)
      Y4(L) = Y4(K)
      Y4(K) = TTY4
!
! *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
! *** SMALLER THAN T
!
  240 L = L-1
      IF( X(L) .GT. T ) GO TO 240
!
! *** FIND AN ELEMENT IN THE FIRST HALF OF THE ARRAY WHICH IS
! *** GREATER THAN T
!
  245 K=K+1
      IF (X(K) .LT. T) GO TO 245
!
! *** INTERCHANGE THESE ELEMENTS
!
      IF (K .LE. L) GO TO 235
!
! *** SAVE UPPER AND LOWER SUBSCRIPTS OF THE ARRAY YET TO BE SORTED
!
      IF( L-I .LE. J-K ) GO TO 250
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GO TO 260
!
  250 IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GO TO 260
!
! *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
!
  255 M = M-1
      IF( M .EQ. 0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  260 IF( J-I .GE. 1 ) GO TO 225
      IF( I .EQ. 1 ) GO TO 210
      I = I-1
  265 I = I+1
      IF( I .EQ. J ) GO TO 255
      T  = X(I+1)
      TY1 = Y1(I+1)
      TY2 = Y2(I+1)
      TY3 = Y3(I+1)
      TY4 = Y4(I+1)
      IF( X(I) .LE. T ) GO TO 265
      K = I
  270 X(K+1) = X(K)
      Y1(K+1) = Y1(K)
      Y2(K+1) = Y2(K)
      Y3(K+1) = Y3(K)
      Y4(K+1) = Y4(K)
      K = K-1
      IF( T .LT. X(K) ) GO TO 270
      X(K+1) = T
      Y1(K+1) = TY1
      Y2(K+1) = TY2
      Y3(K+1) = TY3
      Y4(K+1) = TY4
      GO TO 265
!
! *** CLEAN UP
!
  300 CONTINUE
!
      RETURN
      END SUBROUTINE

