      SUBROUTINE MULABT( A, B, N, NDA, P, NP, IFAULT )
C*....SUBROUTINE MULABT( A, B, N, NDA, P, NP, IFAULT )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine performas a matrix multiply of the full matrix A
C*    and the transpose of the lower triangular matrix B.
C*
C*    The matrix A(N,P) is two-dimensional with leading dimension
C*    NDA in the calling program and is stored in full mode.
C*    The matrix B(P,P) is lower triangular and is stored in a vector,
C*    row by row.
C*
C*
C*  Formal Parameters:
C*
C*    Variable       Type        Description
C*    --------  ----------------  --------------------------------------
C*    A         Real Matrix(N,P)  Input matrix.  This matrix is over-
C*                                written upon output.
C*
C*    B         Real Array (NP)   Input matrix stored as a one-
C*                                dimensional array in the sequence
C*                                B(1,1), B(2,1), B(2,2), B(3,1),
C*                                B(3,2), B(3,3), B(4,1) ...
C*
C*    N         Integer          Input: The leading dimension of A for
C*                               this set of data.
C*
C*    NDA       Integer          Input: The leading dimension of A as
C*                               defined in the calling program.
C*
C*    P         Integer          Input: The (order) dimension of the
C*                               full matrix representation of B.
C*
C*    NP        Integer          Input: The size of the B array.
C*                               It must always be P(P+1)/2.
C*
C*    IFAULT    Integer          Output fault indicator equal to:
C*                                 1 if N is > NDA
C*                                 2 if NP .NE. N(N+1)/2
C*                                 0 otherwise.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INTEGER NDA, N, P, NP, IFAULT
      REAL A(NDA,P), B(NP)
      DOUBLE PRECISION SUM
C
C *** In-line (Statement) function for finding indices
C
      IVEC(I,J) = (I-1)*I/2 + J
C
C *** Error checking on the inputs
C
      IFAULT = 1
      IF( N .GT. NDA ) RETURN
      IFAULT = 2
      IF( NP .NE. P*(P+1)/2 ) RETURN
      IFAULT = 0
C
C *** Start of the multiply
C *** Overwrite A by using the lower triangular property
C *** Explicitly account for B transpose in computing the index IB
C
      I0 = 0
      DO 30 I = 1, N
        DO 20 J = P, 1, -1
          SUM = 0.0
          IB = IVEC(J,I0)
          DO 10 K = 1, J
            IB = IB + 1
            SUM = SUM + A(I,K)*B(IB)
   10     CONTINUE
          A(I,J) = SUM
   20   CONTINUE
   30 CONTINUE
C
      RETURN
      END
