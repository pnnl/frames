      SUBROUTINE S2SORT2(D, X, Y2,Y3,Y4, N, ICNT )
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
!!    Yn    : integer array to be (optionally) carried along
!!    N     : number of values in array X to be sorted
!!    KFLAG : control parameter
!!            = 2 means sort X in INcreasing order and carry Y along.
!!    IERR  : Returned error flag
!!            = 0 Means no errors
!!            = 1 Means error on the number of data values
!!            = 2 Means error on the sort indicator
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**********************************************************************
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: N
      REAL, DIMENSION(*) :: X
	REAL, DIMENSION(*) :: D
      REAL, DIMENSION(*) :: Y2
      REAL, DIMENSION(*) :: Y3
      REAL, DIMENSION(*) :: Y4
	INTEGER ICNT(16)
!
! *** Local variables
      REAL :: TY2,TTY2,TY3,TTY3,TY4,TTY4
      INTEGER, DIMENSION(21) :: IL, IU
      CHARACTER(LEN=6) :: CALLER = 'SSORTI' ! Name of this routine
      INTEGER :: NN, I, M, J, K, KK, IJ, L, kflag, ISTART,ISTOP,MM,II
      REAL :: R, T, TT
!
!---- Executable code ---------------------------------------------------
!
      KFLAG = -2
      NN = N
      KK = IABS(KFLAG)
C
C   Add an outer loop dealing with D
C
      istart = 2
	istop = N
	Mm=0
C
 1003 continue
      do ii = istart,n
	if ((d(ii) .ne. d(ii-1) .or. ii .eq. n)) then
	 istop = ii-1
	 if(istop .eq. n-1) istop=n
	 go to 1001
	endif
	enddo
	if (istart .ge. istop) goto 1002
 1001 continue
		mM = Mm+1
	icnt(mM) = istop-istart+2
c!
! *** SORT X AND CARRY Y'S ALONG
!
  200 CONTINUE
      M = 1
      I = ISTART
      J = ISTOP
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
      TY2 = Y2(I+1)
      TY3 = Y3(I+1)
      TY4 = Y4(I+1)
      IF( X(I) .LE. T ) GO TO 265
      K = I
  270 X(K+1) = X(K)
      Y2(K+1) = Y2(K)
      Y3(K+1) = Y3(K)
      Y4(K+1) = Y4(K)
      K = K-1
      IF( T .LT. X(K) ) GO TO 270
      X(K+1) = T
      Y2(K+1) = TY2
      Y3(K+1) = TY3
      Y4(K+1) = TY4
      GO TO 265
!
! *** CLEAN UP
!
  300 CONTINUE
!
      istart = istop+2
      go to 1003
 1002 continue   
!
      RETURN
      END SUBROUTINE

