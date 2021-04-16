      SUBROUTINE TRMINO( A, N, NN, IFAULT )
C*....SUBROUTINE TRMINO( A, N, NN, IFAULT )
C***********************************************************************
C*
C*  Purpose:
C*
C*    Given a lower triangular matrix of dimension N stored in the
C*    vector A(*), this subroutine calculates the lower triangular
C*    inverse of A.  The matrix A is overwritten by the inverse.
C*
C*
C*  Formal Parameters:
C*
C*    Variable       Type        Description
C*    --------  ---------------  --------------------------------------
C*    A         Real Array (NN)  The input matrix stored as a one-
C*                               dimensional array in the sequence
C*                               A(1,1), A(2,1), A(2,2), A(3,1),
C*                               A(3,2), A(3,3), A(4,1) ...
C*
C*    N         Integer          Input: The order (dimension) of A
C*
C*    NN        Integer          Input: The size of the A arrays.
C*                               It must always be N(N+1)/2.
C*
C*    IFAULT    Integer          Output fault indicator equal to:
C*                                 1 : if N is less than 1
C*                                 2 : if A is singular
C*                                 3 : if NN .NE. N(N+1)/2
C*                                 0 : otherwise.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      REAL A(NN), SMALL
      DOUBLE PRECISION SUM
C
C *** A small divisor indicates singularity
C
      DATA SMALL / 1.0E-30 /
C
C *** In-line (Statement) function for finding indices
C
      IVEC(I,J) = (I-1)*I/2 + J
C
C *** Error checking on the inputs
C
      IFAULT = 1
      IF( N .LE. 0 ) RETURN
      IFAULT = 3
      IF( NN .NE. N*(N+1)/2 ) RETURN
      IFAULT = 2
C
C *** Start of the inverse
C
      IDXU = NN
      DO 30 K = N, 1, -1
        IF( ABS(A(IDXU)) .LT. SMALL ) RETURN
        A(IDXU) = 1.0 / A(IDXU)
        IDXU = IDXU - 1
        DO 20 I = K-1, 1, -1
          SUM = 0.0D0
          DO 10 J = I+1, K
            IXA = IVEC(J,I)
            IXU = IVEC(K,J)
            SUM = SUM + A(IXA)*A(IXU)
   10     CONTINUE
          IDI = IVEC(I,I)
          IF( ABS(A(IDI)) .LT. SMALL ) RETURN
          A(IDXU) = -SUM / A(IDI)
          IDXU = IDXU - 1
   20   CONTINUE
   30 CONTINUE
C
      IFAULT = 0
C
      RETURN
      END
