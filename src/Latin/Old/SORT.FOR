      SUBROUTINE SORT( X, N, KFLAG )
C*....SUBROUTINE SORT( X, N, KFLAG )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine will sort the array X in increasing
C*    or decreasing order.
C*
C*
C*  Reference:
C*
C*    Singleton, R. C.
C*    Algorithm 347
C*    An Efficient Algorithm for Sorting with Minimal Storage
C*    C.A.C.M., 12(3), 1969, Pages 185-187.
C*
C*
C*  Variable Descriptions:
C*
C*    X     - Array of values to be sorted
C*    N     - Number of values in array X to be sorted
C*    KFLAG - Control parameter
C*             =  1 Means sort X in increasing order (ignoring Y)
C*             = -1 Means sort X in decreasing order (ignoring Y)
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      DIMENSION X(N),IL(21),IU(21)
C
      NN = N
C
C *** CHECK FOR SORT IN INCREASING ORDER
C
      IF (KFLAG .EQ. 1 ) GO TO 100
C
C *** CHECK FOR SORT IN DECREASING ORDER
C
      IF (KFLAG .NE. -1 ) STOP 'ERROR IN SORT - KFLAG'
C
C *** ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
C
      DO 20 I = 1, NN
        X(I) = -X(I)
   20 CONTINUE
C
C *** SORT THE VECTOR
C
  100 CONTINUE
C
      M=1
      I=1
      J=NN
      R=.375
  110 IF (I .EQ. J) GO TO 155
  115 IF (R .GT. .5898437) GO TO 120
      R=R+3.90625E-2
      GO TO 125
  120 R=R-.21875
  125 K=I
C
C  *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
C
      IJ = I + IFIX (FLOAT (J-I) * R)
      T=X(IJ)
C
C *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
C
      IF (X(I) .LE. T) GO TO 130
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
  130 L=J
C
C *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
C
      IF (X(J) .GE. T) GO TO 140
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
C
C *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
C
      IF (X(I) .LE. T) GO TO 140
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
      GO TO 140
  135 TT=X(L)
      X(L)=X(K)
      X(K)=TT
C
C *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
C *** SMALLER THAN T
C
  140 L=L-1
      IF (X(L) .GT. T) GO TO 140
C
C *** FIND AN ELEMENT IN THE FIRST HALF OF THE ARRAY WHICH IS
C *** GREATER THAN T
C
  145 K=K+1
      IF (X(K) .LT. T) GO TO 145
C
C *** INTERCHANGE THESE ELEMENTS
C
      IF (K .LE. L) GO TO 135
C
C *** SAVE UPPER AND LOWER SUBSCRIPTS OF THE ARRAY YET TO BE SORTED
C
      IF (L-I .LE. J-K) GO TO 150
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 160
  150 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 160
C
C *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
C
  155 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  160 IF (J-I .GE. 1) GO TO 125
      IF (I .EQ. 1) GO TO 110
      I=I-1
  165 I=I+1
      IF (I .EQ. J) GO TO 155
      T=X(I+1)
      IF (X(I) .LE. T) GO TO 165
      K=I
  170 X(K+1)=X(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 170
      X(K+1)=T
      GO TO 165
C
C *** CLEAN UP FOR SORT IN DECREASING ORDER
C
  300 IF( KFLAG .EQ. 1 ) RETURN
C
      DO 310 I = 1, NN
        X(I) = -X(I)
  310 CONTINUE
C
      RETURN
      END
