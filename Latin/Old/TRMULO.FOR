      SUBROUTINE TRMULO( A, B, N, NN, IFAULT )
C*....SUBROUTINE TRMULO( A, B, N, NN, IFAULT )
C***********************************************************************
C*
C*  Purpose:
C*
C*    Given two lower triangular matrices of dimension N stored in the
C*    vectors A(*) and B(*), this subroutine calculates the lower
C*    triangular product A*B and puts the result in the vector B.
C*
C*
C*  Formal Parameters:
C*
C*    Variable       Type        Description
C*    --------  ---------------  --------------------------------------
C*    A         Real Array (NN)  Input matrix stored as a one-
C*                               dimensional array in the sequence
C*                               A(1,1), A(2,1), A(2,2), A(3,1),
C*                               A(3,2), A(3,3), A(4,1) ...
C*
C*    B         Real Array (NN)  Input: Matrix stored as a one-
C*                               dimensional array as in the A vector
C*                               Output: Product matrix stored as a
C*                               one-dimensional array as in the A
C*                               vector.
C*
C*    N         Integer          Input: The order (dimension) of A
C*
C*    NN        Integer          Input: The size of the A and B
C*                               arrays.  It must always be N(N+1)/2.
C*
C*    IFAULT    Integer          Output fault indicator equal to:
C*                                 1 if N is less than 1
C*                                 2 if NN .NE. N(N+1)/2
C*                                 0 otherwise.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      REAL A(NN), B(NN)
      DOUBLE PRECISION SUM
C
C *** In-line (Statement) function for finding indices
C
      IVEC(I,J) = (I-1)*I/2 + J
C
C *** Error checking on the inputs
C
      IFAULT = 1
      IF( N .LE. 0 ) RETURN
      IFAULT = 2
      IF( NN .NE. N*(N+1)/2 ) RETURN
      IFAULT = 0
C
C *** Start of the multiply
C
      DO 30 I = N, 1, -1
        DO 20 J = 1, I
          SUM = 0.0
          DO 10 K = J, I
            IA = IVEC(I,K)
            IB = IVEC(K,J)
            SUM = SUM + A(IA)*B(IB)
   10     CONTINUE
          IB = IVEC(I,J)
          B(IB) = SUM
   20   CONTINUE
   30 CONTINUE
C
      RETURN
      END
