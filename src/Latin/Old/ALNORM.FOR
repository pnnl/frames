      REAL FUNCTION ALNORM( X, UPPER )
C*....REAL FUNCTION ALNORM( X, UPPER )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This function evaluates the area under a standard normal
C*    curve.  The region for integration is from minus infinity to
C*    X if UPPER is .FALSE., or from X to infinity if UPPER is
C*    .TRUE.  A polynomial approximation method is used.
C*
C*
C*  Call List Definitions:
C*
C*    X     : Real input argument
C*    UPPER : Logical input argument
C*
C*
C*  Reference:
C*
C*    Algorithm AS66
C*    Applied Statistics
C*    (Journal of the Royal Statistical Society, Series C)
C*    Vol. 22, No. 3, 1973
C*    Pp. 424-427
C*
C*
C*  Notes:
C*
C*    LTONE =  Argument value at which the lower tail area becomes
C*             unity to the accuracy of the machine.
C*             LTONE = (N+9)/3, Where N is the number of digits
C*             of accuracy, is sufficiently accurate.
C*             CRAY:         N = 14, so LTONE = 7.333
C*             IBM (32 BIT): N = 7,  so LTONE = 5.333
C*
C*    UTZERO = Argument at which the upper tail area becomes
C*             zero to the precision of the machine.
C*             UTZERO = SQRT(-2*ALOG(X)+1)) - 0.3 where X
C*             is the smallest allowable real number
C*             CRAY:         X = 2**(-8191), so UTZERO = 106.25
C*             IBM (32 BIT): X = 1.18E-38,   so UTZERO =  12.84
C*
C*    about nine significant figures (decimal) are correct on a
C*    machine that works to such accuracy.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      REAL LTONE, UTZERO, ZERO, HALF, ONE, CON, X
      DOUBLE PRECISION Z, Y
C
      DOUBLE PRECISION C1, C2, C3, C4, C5, C6, C7,
     *     D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12
C
      LOGICAL UPPER, UP
C
C *** LTONE and UTZERO are machine dependent - see comments above
C
      DATA LTONE, UTZERO / 5.333, 12.84 /
      DATA ZERO, HALF, ONE, CON / 0.0, 0.5, 1.0, 1.28 /
C
      UP = UPPER
      Z  = X
      IF( Z .GE. ZERO ) GO TO 10
      UP = .NOT. UP
      Z = -Z
   10 CONTINUE
C
      IF( Z.LE.LTONE .OR. UP.AND.Z.LE.UTZERO ) GO TO 20
      ALNORM = ZERO
      GO TO 40
C
   20 CONTINUE
      Y = HALF * Z * Z
      IF( Z .GT. CON ) GO TO 30
C
      DATA C1 /   0.398942280444D0 /
      DATA C2 /  -0.399903438504D0 /
      DATA C3 /   5.75885480458D0  /
      DATA C4 / -29.8213557808D0   /
      DATA C5 /   2.62433121679D0  /
      DATA C6 /  48.6959930692D0   /
      DATA C7 /   5.92885724438D0  /
C
      ALNORM = HALF -  Z * ( C1 + C2 * Y /
     1  (Y + C3 + C4 / (Y + C5 + C6 /
     3  (Y + C7 ))))
      GO TO 40
C
      DATA D1  /   0.398942280385D0 /
      DATA D2  /  -3.8052D-8      /
      DATA D3  /   1.00000615302D0  /
      DATA D4  /   3.98064794D-4  /
      DATA D5  /   1.98615381364D0  /
      DATA D6  /  -0.151679116635D0 /
      DATA D7  /   5.29330324926D0  /
      DATA D8  /   4.8385912808D0   /
      DATA D9  / -15.1508972451D0   /
      DATA D10 /   0.742380924027D0 /
      DATA D11 /  30.789933034D0    /
      DATA D12 /   3.99019417011D0  /
C
   30 CONTINUE
      ALNORM = D1 * DEXP(-Y) /
     1 (Z + D2 + D3 /
     2 (Z + D4 + D5 /
     3 (Z + D6 + D7 /
     4 (Z + D8 + D9 /
     5 (Z + D10 + D11 /
     6 (Z + D12 ))))))
C
   40 CONTINUE
      IF( .NOT. UP ) ALNORM = ONE - ALNORM
C
      RETURN
      END
