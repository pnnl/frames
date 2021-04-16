      SUBROUTINE GINVER( U, X, P, A, LGAMMA, UHGH, XHGH )
C*....SUBROUTINE GINVER( U, X, P, A, LGAMMA, UHGH, XHGH )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine computes the inverse of the gamma distribution
C*    with parameters A and P.  The method used is a bisection method
C*    to solve F(X) = U for X.
C*
C*
C*  Auxiliary Routines:
C*
C*    The function GAMAIN is used to compute the cumulative
C*    distribution function for the Gamma(P) distribution.
C*
C*
C*  Formal Parameters:
C*
C*    Variable  Definition
C*    --------  --------------------------------------------------------
C*    U         Input - Real: The uniform value to invert to the value
C*              X from a Gamma distribution.
C*
C*    X         Output - Real: The value X which satisfies F(X) = U,
C*              where F is the Gamma distribution parameters A and P.
C*
C*    P         Input - Real: The power parameter (i.e. X**(P-1)) for th
C*              Gamma density.
C*
C*    A         Input - Real: The multiplier on X in the exponent of
C*              the distribution function.  The standard gamma function
C*              has A = 1.
C*
C*    UHGH      Input - Real: A value in the range (0,1) near 1 where
C*              if U .GE. UHGH, X is assigned to XHGH.  It is suggested
C*              to set UHGH = 0.99999 on a 32-bit machine.
C*
C*    XHGH      Input - Real: A value of X which is greater than any
C*              expected from the gamma distribution being generated.
C*              If XHGH is set to zero on entry, it is calculated in
C*              this subroutine.  It is suggested to set UHGH = 0.99999,
C*              and find the XHGH which maps to this value or a larger
C*              value for UHGH.
C*
C*    LGAMMA    Input - Real: The natural log of the complete gamma
C*              function with A=1.  If LGAMMA is set to zero on entry,
C*              it will be computed internally, otherwise the value
C*              entered will be used.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      REAL LGAMMA
C
C *** Check for computation of LGAMMA
C
      IF( LGAMMA .EQ. 0.0 ) THEN
        LGAMMA = ALNGAM(P,IFT)
      ENDIF
C
C *** XLOW = minimum possible value for X
C *** XHGH = Value corresponding to the uniform level of UHGH
C     or higher.
C
      XLOW = 0.0
C
C *** Start with the mean plus 3 standard deviations when looking
C     for XHGH
C
      XHGH = P + 3.0*P
   10 CONTINUE
      G = GAMAIN( XHGH, P, LGAMMA )
      IF( G .LT. UHGH ) THEN
        XHGH = XHGH + P
        GO TO 10
      ENDIF
C
C *** Check for a large value of X corresponding to a large U
C
      IF( U .GE. UHGH ) THEN
        XMID = XHGH
        GO TO 30
      ENDIF
C
C *** Iterate to solve for X given U
C     ITER = 21 yields 2**(-21) = XHGH * 4.8E-7 Accuracy on X
C
      ITER = 21
      DO 20 I = 1, ITER
        XMID = (XLOW+XHGH)/2.0
        GMID = GAMAIN( XMID, P, LGAMMA )
        IF( GMID .GT. U ) THEN
          XHGH = XMID
        ELSE
          XLOW = XMID
        ENDIF
   20 CONTINUE
C
   30 CONTINUE
C
      X = XMID / A
C
      RETURN
      END
