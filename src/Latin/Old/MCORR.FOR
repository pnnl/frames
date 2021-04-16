      SUBROUTINE MCORR( IFAULT )
C*....SUBROUTINE MCORR( IFAULT )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine calculates the correlation matrix between the
C*    P variables which are stored in the columns of the SCORE matrix.
C*    The vector RWORK is a work vector of length at least 2*P.
C*    On output the correlation matrix is stored as a vector in the
C*    vector CORA.  CORA is stored by rows, e.g., the elements (1,1),
C*    (2,1), (2,2), (3,1), ... from a full matrix representation.
C*
C*    The variable IFAULT returns 0 for normal ternmination, or 1 for
C*    a zero or negative variance.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
C
      DOUBLE PRECISION SUM
C
      RN = N
C
C *** Compute the mean and store it temporarily
C     All columns have the same mean
C
      SUM = 0.0
      DO 10 J = 1, N
        SUM = SUM + SCORE(J,1)
   10 CONTINUE
      RWORK(1) = SUM / RN
C
C *** Compute the variance and store it temporarily
C     All columns have the same variance
C
      SUM = 0.0
      DO 30 K = 1, N
        SUM = SUM + (SCORE(K,1)-RWORK(1))*(SCORE(K,1)-RWORK(1))
   30 CONTINUE
      RWORK(2) = SUM / RN
      IF( SUM .LE. 0.0 ) THEN
        IFAULT = 1
      ENDIF
C
C *** Don't compute the correlation matrix for zero or
C     negative variances
C
      IF( IFAULT .GT. 0 ) RETURN
C
C *** Compute the covariances and normalize them to correlations
C *** Include the diagonal elements as a check, and, they are
C     needed to get the proper elements in CORA.
C
      IDX = 0
      DO 70 I = 1, P
        DO 60 J = 1, I
          SUM = 0.0
          DO 50 K = 1, N
            SUM = SUM + (SCORE(K,I)-RWORK(1))*(SCORE(K,J)-RWORK(1))
   50     CONTINUE
          IDX = IDX + 1
          CORA(IDX) = (SUM/RN) / RWORK(2)
   60   CONTINUE
   70 CONTINUE
C
      RETURN
      END
