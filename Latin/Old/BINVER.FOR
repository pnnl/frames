      SUBROUTINE BINVER( U, X, A, B, P, Q, LBETA )
C*....SUBROUTINE BINVER( U, X, A, B, P, Q, LBETA )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine computes the inverse of the beta distribution
C*    defined on the interval (A,B) with parameters P and Q. The
C*    method used is a bisection method to solve F(X) = U for X.
C*
C*
C*  Auxiliary Routines:
C*
C*    The function BETAIN is used to compute the cumulative
C*    distribution function for the Beta(P,Q) distribution on the
C*    interval (0,1).
C*
C*
C*  Formal Parameters:
C*
C*    Variable  Definition
C*    --------  --------------------------------------------------------
C*    U         Input - Real: The uniform value to invert to the value
C*              X from a Beta distribution.
C*
C*    X         Output - Real: The value X which satisfies F(X) = U,
C*              where F is the Beta distribution on (A,B) with
C*              parameters P and Q.
C*
C*    A         Input - Real: The left end of the range of the Beta
C*              distribution.
C*
C*    B         Input - Real: The right end of the range of the Beta
C*              distribution.
C*
C*    P         Input - Real: The first parameter (i.e. X**P) for the
C*              Beta density.
C*
C*    Q         Input - Real: The second parameter for the Beta density.
C*
C*    LBETA     Input - Real: The natural log of the complete beta
C*              function for the interval A=0, B=1.  If LBETA is set to
C*              zero on entry, it will be computed internally, otherwise
C*              the value entered will be used.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      REAL LBETA
C
C *** Check for computation of LBETA
C
      IF( LBETA .EQ. 0.0 ) THEN
        IF( P .EQ. Q ) THEN
          LBETA = 2.0*ALNGAM(P,IFT) - ALNGAM((2*P),IFT)
        ELSE
          LBETA = ALNGAM(P,IFT) + ALNGAM(Q,IFT) - ALNGAM((P+Q),IFT)
        ENDIF
      ENDIF
C
C *** Iterate to solve for X given U
C
C *** ITER = 21 yields 2**(-21) = 4.8E-7 Accuracy on X
C     Because X is confined to the interval (0,1)
C
      ITER = 21
C
      XLOW = 0.0
      XHGH = 1.0
      DO 10 I = 1, ITER
        XMID = (XLOW+XHGH)/2.0
        BMID = BETAIN( XMID, P, Q, LBETA )
        IF( BMID .GT. U ) THEN
          XHGH = XMID
        ELSE
          XLOW = XMID
        ENDIF
   10 CONTINUE
C
      X = A + (B-A)*XMID
C
      RETURN
      END
