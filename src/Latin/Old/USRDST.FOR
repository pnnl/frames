      REAL FUNCTION USRDST( RV, NUSER, UUSER, XUSER )
C*....REAL FUNCTION USRDST( RV, NUSER, UUSER, XUSER )
C**********************************************************************
C*
C*  Purpose:
C*
C*    This function transforms a uniform value on the interval [0,1]
C*    to a user defined distribution.  The user distribution is defined
C*    by the piecewise linear cummulative confidence function given by
C*    data points (UUSER(I),XUSER(I)).
C*
C*    Note: This function is really a general purpose interpolation
C*    routine.  Given the definition of a function using X,F(X) pairs
C*    of data, USRDEF returns the F(X) value corresponding to a new
C*    X value (passed in RV).
C*
C*
C*  Formal Parameters:
C*
C*    RV     = Input - Real: The uniform value to transform to a value
C*             from the specified distribution.
C*
C*    NUSER  : Input - Integer: The number of data points in the
C*             definition of the cumulative distribution function.
C*
C*    XUSER  : Input - Real Vector: The distribution values
C(             corresponding to the probability levels in UUSER.
C*
C*    UUSER  : Input - Real Vector: The probability levels
C*             corresponding to the values of the distribution given
C*             in XUSER.  No duplicate values are allowed.
C*             The entries in UUSER must be given in strictly
C*             increasing algebraic order, no duplicate entries are
C*             allowed.
C*
C*   Notes:
C*
C*     If RV is less than UUSER(1), then USRDST = XUSER(1)
C*     If RV is greater than UUSER(NUSER) then USRDST = XUSR(NUSER)
C*
C*     That is, extreme values are moved in to the nearest point
C*     of definition for the distribution.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C**********************************************************************
C
      DIMENSION UUSER(NUSER), XUSER(NUSER)
C
C *** Map the small value to the first defined pair
C
      IF( RV .LE. UUSER(1) ) THEN
        USRDST = XUSER(1)
        RETURN
      ENDIF
C
C *** Map the large value to the last defined pair
C
      IF( RV .GE. UUSER(NUSER) ) THEN
        USRDST = XUSER(NUSER)
        RETURN
      ENDIF
C
C *** Find the index for linear interpolation
C
      DO 10 J = 1, (NUSER-1)
        IF( (RV.GE.UUSER(J)) .AND. (RV.LT.UUSER(J+1)) ) THEN
          IDX = J
          GO TO 20
        ENDIF
  10  CONTINUE
C
  20  CONTINUE
C
C *** Compute the interpolated value
C
      DEL    = (RV-UUSER(IDX)) / (UUSER(IDX+1)-UUSER(IDX))
      USRDST = XUSER(IDX) + DEL*(XUSER(IDX+1)-XUSER(IDX))
C
      RETURN
      END
