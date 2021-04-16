      SUBROUTINE REARNG
C*....SUBROUTINE REARNG
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine schedules all of the operations that must be
C*    performed to rearrange values for the Latin Hypercube Sampling
C*    scheme using data that has already been generated with a
C*    stratified sampling scheme.
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
      INCLUDE 'FILES.ins'
C
C-----------------------------------------------------------------------
C                    Compute the score matrix
C        Based on the column rank of a variable in the X matrix
C-----------------------------------------------------------------------
C
C *** Compute the van der Warden scores used to fill the score matrix.
C     Each column of the score matrix contains the same scores, but
C     they appear in different order.  Use the temporary work location
C     XTMP to compute the scores once.
C
      RNP1 = N + 1
      DO 105 J = 1, N
        U = J/RNP1
        XTMP(J) = PPND( U, IFAULT )
  105 CONTINUE
C
      DO 120 I = 1, P
C
C ***   Extract the variable of interest
C       Set up indices for the rank vector in IWORK
C
        DO 100 J = 1, N
          RWORK(J) = X(J,I)
          IWORK(J) = J
  100   CONTINUE
C
C ***   Perform a sort to find the ranks
C ***   NOTE: SSORT sorts RWORK in increasing order and carries
C       along the vector IWORK.  IWORK is decalred type real in SSORT,
C       But since SSORT merely interchanges the values in IWORK and
C       does not modify them, you can pass the integer vector and get
C       away with accessing it as a real.
C
        DATA KFLAG /2/
        CALL SSORT( RWORK, IWORK, N, KFLAG )
C
C ***   Put the sorted X values back into X, thus location gives rank.
C       Also, fill the score matrix.
C
        DO 110 J = 1, N
          X(J,I) = RWORK(J)
          SCORE(J,I) = XTMP(IWORK(J))
  110   CONTINUE
C
  120 CONTINUE
C
C ---------------------------------------------------------------------
C   Compute the correlation between the columns of the score matrix
C ---------------------------------------------------------------------
C *** The subroutine uses the matrix SCORE, the integers N and P,
C     and computes the matrix CORA.
C
      CALL MCORR( IFAULT )
      IF( IFAULT .GT. 0 ) THEN
        WRITE(IRPT,1070)
        WRITE(IERR,1070)
 1070   FORMAT(/
     *   ' *** Terminal error in subroutine REARNG ***'/
     *   '     IFAULT greater than zero after MCORR'/
     *   '     The score matrix correlation matrix is in error'/
     *   '     Check all inputs and/or contact the programmer.')
        STOP 'MCORR 1'
      ENDIF
C
C ---------------------------------------------------------------------
C   Cholesky decomposition of the score matrix correlation matrix
C ---------------------------------------------------------------------
C
      CALL CHOLP( CORA, P, NP, NULLTY, IFAULT )
      IF( IFAULT .NE. 0 ) THEN
        WRITE(IRPT,1080)
        WRITE(IERR,1080)
 1080   FORMAT(/
     *   ' *** Terminal error in subroutine REARNG ***'/
     *   '     The score matrix correlation matrix is not',
     *   ' positive definite'/
     *   '     Check all inputs and/or contact the programmer.')
        STOP 'CORA-1'
      ENDIF
C
C ---------------------------------------------------------------------
C      Compute the inverse of the score matrix correlation matrix
C ---------------------------------------------------------------------
C *** Compute the inverse of CORA and overwrite itself
C
      CALL TRMINO( CORA, P, NP, IFAULT )
C
C ---------------------------------------------------------------------
C      Compute the product of COR(decomposed) and CORA(product,
C       decomposed and inverse)
C ---------------------------------------------------------------------
C     -> To save space, overwrite CORA
C
      CALL TRMULO( COR, CORA, P, NP, IFAULT )
C
C ---------------------------------------------------------------------
C      Compute the modified score matrix and overwrite the
C        original score matrix
C ---------------------------------------------------------------------
C
      CALL MULABT( SCORE, CORA, N, MAXN, P, NP, IFAULT )
C
C-----------------------------------------------------------------------
C                        Data Reshuffling
C-----------------------------------------------------------------------
C
C *** Force the X matrix to have the same column rank structure as
C     the modified score matrix
C
      DO 150 I = 1, P
C
C ***   Extract the column of the score matrix to be shuffled
C       Set up indices for the rank vector in IWORK
C       Also, extract the (sorted) X values to be rearranged
C
        DO 130 J = 1, N
          RWORK(J) = SCORE(J,I)
          XTMP(J)  = X(J,I)
          IWORK(J) = J
  130   CONTINUE
C
C ***   Perform a sort to find the ranks in the score matrix
C ***   NOTE: See note above on SSORT
C
        CALL SSORT( RWORK, IWORK, N, KFLAG )
C
C ***   Put the XTMP values back into X at the locations given
C       by the ranks from the score matrix
C
        DO 140 J = 1, N
          X(IWORK(J),I) = XTMP(J)
  140   CONTINUE
C
  150 CONTINUE
C
      RETURN
      END
