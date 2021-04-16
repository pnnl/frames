      SUBROUTINE CHOLP( A, N, NN, NULLTY, IFAULT )
C*....SUBROUTINE CHOLP( A, N, NN, NULLTY, IFAULT )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine computes the Cholesky decomposition of a matrix.
C*
C*    Given a symmetric matrix of dimension N stored as a lower
C*    triangle in the vector A(*), this subroutine calculates the lower
C*    triangular matrix L(*), such that L(transpose)*L=A.  However,
C*    the vector L overwrites the vector A.
C*
C*
C*  Notes:
C*
C*    1. The matrix A must be positive semi-definite.
C*
C*    2. The factor ETA is set for determining an effective zero for
C*       the pivoting operations.
C*           ETA = 1.0E-5 for a 32 bit machine
C*           ETA = 1.0E-9 for a 48 to 64 bit machine
C*
C*    3. An efficient method for inverting submatrices is given in
C*       Freedman, P.R. (1982), Remark AS R44, Applied Statistics,
C*       Vol. 31, pp. 336-339.
C*
C*
C*  Reference:
C*
C*    Algorithm AS 6, Triangular Decomposition of a Symmetric Matrix
C*    Applied Statistics, (Journal of the Royal Statistical Society,
C*    Series C), 1968, Vol. 17, p. 195
C*
C*
C*  Formal Parameters:
C*
C*    Variable       Type        Description
C*    --------  ---------------  --------------------------------------
C*    A         Real Array (NN)  The input matrix stored as a one-
C*                               dimensional array in the sequence
C*                               A(1,1), A(2,1), A(2,2), A(3,1),
C*                               A(3,2), A(3,3), A(4,1) ...  On ouput
C*                               the A matrix is overwritten by the
C*                               decomposition.
C*
C*    N         Integer          Input: The order (dimension) of A
C*
C*    NN        Integer          Input: The size of the A and L
C*                               arrays.  It must always be N(N+1)/2.
C*
C*    NULLTY    Integer          Output: The nullity of A, i.e., the
C*                               number of L(i,i) that have been set
C*                               to zero.  The rank of A is N-NULLTY.
C*
C*    IFAULT    Integer          Output fault indicator equal to:
C*                                 1 if N is less than 1
C*                                 2 if A is not positive semidefinite
C*                                 3 if NN .NE. N(N+1)/2
C*                                 0 otherwise.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
C *** To convert to double precision, change REAL to DOUBLE PRECISION,
C     ABS to DABS, SQRT to DSQRT, change ETA as above and give double
C     precision versions of the constants in the data statement
C
      REAL A(NN), ETA, ETA2, X, W, ZERO, ZABS, ZSQRT
C
      DATA ETA, ZERO / 1.0E-5, 0.0 /
C
C *** Inline function definition
C
      ZABS(X)  = ABS(X)
      ZSQRT(X) = SQRT(X)
C
C *** Error checking on the inputs
C
      IFAULT = 1
      IF( N .LE. 0 ) RETURN
C
      IFAULT = 3
      IF( NN .NE. N*(N+1)/2 ) RETURN
C
      IFAULT = 2
C
C *** Set up constants
C
      ETA2 = ETA * ETA
C
C *** Start of the decomposition
C
      J  = 1
      K  = 0
      II = 0
      NULLTY = 0
C
      DO 80 ICOL = 1, N
        II = II + ICOL
        X  = ETA2 * A(II)
        L  = 0
        KK = 0
        DO 40 IROW = 1, ICOL
          KK = KK + IROW
          K  = K + 1
          W  = A(K)
          M  = J
          DO 10 I = 1, IROW
            L = L + 1
            IF( I .EQ. IROW ) GO TO 20
            W = W - A(L)*A(M)
            M = M + 1
   10     CONTINUE
   20     IF( IROW .EQ. ICOL ) GO TO 50
          IF( A(L) .EQ. ZERO ) GO TO 30
          A(K) = W / A(L)
          GO TO 40
   30     IF( W*W .GT. ZABS(X*A(KK)) ) RETURN
          A(K) = ZERO
   40   CONTINUE
   50   IF( ZABS(W) .LE. ZABS(ETA*A(K)) ) GO TO 60
        IF( W .LT. ZERO ) RETURN
        A(K) = ZSQRT(W)
        GO TO 70
   60   A(K) = ZERO
        NULLTY = NULLTY + 1
   70   J = J + ICOL
   80 CONTINUE
C
      IFAULT = 0
C
      RETURN
      END
