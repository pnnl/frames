      REAL FUNCTION BETAIN( X, P, Q, LBETA )
C*....REAL FUNCTION BETAIN( X, P, Q, LBETA )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This function computes the ratio of the incomplete Beta function
C*    with the complete Beta function for arguments X between zero and
C*    one, and P and Q positive.
C*
C*    Another way to state the purpose is: This subroutine evaluates
C*    the cumulative distribution function at the location X of the
C*    Beta distribution with parameters P and Q.
C*
C*
C*  Formal Parameters:
C*
C*    Variable  Definition
C*    --------  --------------------------------------------------------
C*    X         Input - Real: The value X at which to evaluate the
C*              Beta distribution on (0,1) with parameters P and Q.
C*
C*    P         Input - Real: The first parameter (i.e. X**P) for the
C*              Beta density.
C*
C*    Q         Input - Real: The second parameter for the Beta density.
C*
C*    LBETA     Input - Real : The natural log of the complete Beta
C*              function using the parameters P and Q on the
C*              interval (0,1).
C*
C*
C*  Reference:
C*
C*    Algorithm AS 63
C*    Applied Statistics (1973),
C*    Vol. 22, No. 3
C*
C*    Modified as per remark ASR 19
C*    Applied Statistics (1977),
C*    Vol. 26, No. 1
C*    ***[FAULT INDICATOR REMOVED]***
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      LOGICAL INDEX
      REAL LBETA
C
C *** Define accuracy and initialize
C
      DATA ACU /0.1E-7/
      BETAIN = X
C
C *** Test for admissibility of arguments
C
      IF( P.LE.0.0 .OR. Q.LE.0.0 ) STOP
      IF( X.LT.0.0 .OR. X.GT.1.0 ) STOP
      IF( X.EQ.0.0 .OR. X.EQ.1.0 ) RETURN
C
C *** Change tail if necessary and determine S
C
      PSQ = P + Q
      CX = 1.0 - X
      IF( P .GE. PSQ*X ) THEN
        XX = X
        PP = P
        QQ = Q
        INDEX = .FALSE.
      ELSE
        XX = CX
        CX = X
        PP = Q
        QQ = P
        INDEX = .TRUE.
      ENDIF
C
      AI = 1.0
      NS = QQ + CX * PSQ
      TERM = 1.0
      BETAIN = 1.0
C
C *** Use Soper's reduction formulae
C
      RX = XX / CX
    3 TEMP = QQ - AI
      IF( NS .EQ. 0 ) RX = XX
    4 TERM = TERM * TEMP * RX / (PP + AI)
      BETAIN = BETAIN + TERM
      TEMP = ABS(TERM)
      IF( TEMP.LE.ACU .AND. TEMP.LE.(ACU*BETAIN) ) GOTO 5
      AI = AI + 1.0
      NS = NS - 1
      IF( NS .GE. 0 ) GOTO 3
      TEMP = PSQ
      PSQ = PSQ + 1.0
      GOTO 4
C
C *** Calculate result
C
    5 CONTINUE
      BETAIN = BETAIN *
     *     EXP(PP * ALOG(XX) + (QQ - 1.0) * ALOG(CX) - LBETA) / PP
      IF( INDEX ) BETAIN = 1.0 - BETAIN
C
      RETURN
      END
