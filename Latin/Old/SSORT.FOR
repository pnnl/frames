      SUBROUTINE SSORT( X, Y, N, KFLAG )
C*....SUBROUTINE SSORT( X, Y, N, KFLAG )
C***********************************************************************
C*
C*  Purpose:
C*
C*    SSORT sorts array X and optionally makes the same interchanges
C*    in array Y.  The array X may be sorted in increasing order or
C*    decreasing order.  A modified QUICKSORT algorithm is used.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C*
C*  Reference:
C*
C*    Singleton, R. C., Algorithm 347, An Efficient Algorithm for
C*    Sorting with Minimal Storage, CACM, 12(3), 1969, 185-7.
C*
C*
C*  Variable Descriptions:
C*
C*    X - array of values to be sorted   (usually abscissas)
C*    Y - array to be (optionally) carried along
C*    N - number of values in array X to be sorted
C*    KFLAG - control parameter
C*      =  2 means sort X in increasing order and carry Y along.
C*      =  1 means sort X in increasing order (ignoring Y)
C*      = -1 means sort X in decreasing order (ignoring Y)
C*      = -2 means sort X in decreasing order and carry Y along.
C*
C***********************************************************************
C
      DIMENSION X(N),Y(N),IL(21),IU(21)
C
      NN = N
      KK = IABS(KFLAG)
C
C *** ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
C
   15 IF (KFLAG.GE.1) GO TO 30
C
      DO 20 I = 1, NN
        X(I) = -X(I)
   20 CONTINUE
C
   30 GO TO (100,200),KK
C
C *** SORT X ONLY
C
  100 CONTINUE
      M = 1
      I = 1
      J = NN
      R = 0.375
  110 IF(I .EQ. J) GO TO 155
  115 IF(R .GT. 0.5898437) GO TO 120
      R = R + 3.90625E-2
      GO TO 125
C
  120 R = R - 0.21875
  125 K = I
C
C *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
C
      IJ = I + IFIX( FLOAT(J-I) * R)
      T = X(IJ)
C
C *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
C
      IF (X(I) .LE. T) GO TO 130
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
  130 L = J
C
C *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
C
      IF (X(J) .GE. T) GO TO 140
      X(IJ) = X(J)
      X(J)  = T
      T     = X(IJ)
C
C *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
C
      IF (X(I) .LE. T) GO TO 140
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      GO TO 140
  135 TT = X(L)
      X(L) = X(K)
      X(K) = TT
C
C *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
C *** SMALLER THAN T
C
  140 L = L - 1
      IF( X(L) .GT. T ) GO TO 140
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
      IF( L-I .LE. J-K ) GO TO 150
      IL(M) = I
      IU(M) = L
      I = K
      M = M+1
      GO TO 160
C
  150 IL(M) = K
      IU(M) = J
      J = L
      M = M+1
      GO TO 160
C
C *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
C
  155 M=M-1
      IF( M .EQ. 0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  160 IF( J-I .GE. 1 ) GO TO 125
      IF( I .EQ. 1 ) GO TO 110
      I = I-1
  165 I = I+1
      IF( I .EQ. J ) GO TO 155
      T = X(I+1)
      IF( X(I) .LE. T ) GO TO 165
      K = I
  170 X(K+1) = X(K)
      K = K-1
      IF( T .LT. X(K) ) GO TO 170
      X(K+1) = T
      GO TO 165
C
C *** SORT X AND CARRY Y ALONG
C
  200 CONTINUE
      M = 1
      I = 1
      J = NN
      R = 0.375
  210 IF( I .EQ. J ) GO TO 255
  215 IF( R .GT. 0.5898437 ) GO TO 220
      R = R + 3.90625E-2
      GO TO 225
C
  220 R = R - 0.21875
  225 K = I
C
C *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
C
      IJ = I + IFIX( FLOAT(J-I) * R )
      T  = X(IJ)
      TY = Y(IJ)
C
C *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
C
      IF (X(I) .LE. T) GO TO 230
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      Y(IJ) = Y(I)
      Y(I)  = TY
      TY    = Y(IJ)
  230 L = J
C
C *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
C
      IF (X(J) .GE. T) GO TO 240
      X(IJ) = X(J)
      X(J)  = T
      T     = X(IJ)
      Y(IJ) = Y(J)
      Y(J)  = TY
      TY    = Y(IJ)
C
C *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
C
      IF (X(I) .LE. T) GO TO 240
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      Y(IJ) = Y(I)
      Y(I)  = TY
      TY    = Y(IJ)
      GO TO 240
C
  235 TT=X(L)
      X(L) = X(K)
      X(K) = TT
      TTY  = Y(L)
      Y(L) = Y(K)
      Y(K) = TTY
C
C *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
C *** SMALLER THAN T
C
  240 L = L-1
      IF( X(L) .GT. T ) GO TO 240
C
C *** FIND AN ELEMENT IN THE FIRST HALF OF THE ARRAY WHICH IS
C *** GREATER THAN T
C
  245 K=K+1
      IF (X(K) .LT. T) GO TO 245
C
C *** INTERCHANGE THESE ELEMENTS
C
      IF (K .LE. L) GO TO 235
C
C *** SAVE UPPER AND LOWER SUBSCRIPTS OF THE ARRAY YET TO BE SORTED
C
      IF( L-I .LE. J-K ) GO TO 250
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GO TO 260
C
  250 IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GO TO 260
C
C *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
C
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
      TY = Y(I+1)
      IF( X(I) .LE. T ) GO TO 265
      K = I
  270 X(K+1) = X(K)
      Y(K+1) = Y(K)
      K = K-1
      IF( T .LT. X(K) ) GO TO 270
      X(K+1) = T
      Y(K+1) = TY
      GO TO 265
C
C *** CLEAN UP
C
  300 IF( KFLAG .GE. 1 ) RETURN
C
      DO 310 I = 1, NN
        X(I) = -X(I)
  310 CONTINUE
C
      RETURN
      END
