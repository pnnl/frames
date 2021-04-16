      SUBROUTINE PRTDST( IVAR )
C*....SUBROUTINE PRTDST( IVAR )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine prints the definition of the statistical
C*    distribution for a single variable to the report file.
C*
C*
C*  Call List:
C*
C*    IVAR  : Variable number of the variable information to be
C*            printed.  Do not modify.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*    Paul W. Eslinger : 25 Aug 1992 : Add unique variable label
C*    Paul W. Eslinger : 27 Aug 1992 : Correct format numbers in
C*         the print loop IF( ITRUNC .EQ. 3 ) THEN ...
C*    Paul W. Eslinger :  3 Feb 1993 : Correct index on write
C*         statement for ITRUNC = 2
C*
C***********************************************************************
C
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
      INCLUDE 'FILES.ins'
      INCLUDE 'DISTC.ins'
C
C *** Check for a legal variable number
C
      IF( IVAR.LT.1 .OR. IVAR.GT.PTOT ) THEN
        WRITE(IRPT,1000) IVAR, PTOT
 1000   FORMAT(/
     *   ' *** Terminal error in subroutine PRTDST ***'
     *   '     An illegal varible number was entered'/
     *   '     The value entered was ',I3/
     *   '     It must be in the range 1 to ',I3)
        STOP 'PRTDST-1'
      ENDIF
C
      WRITE(IRPT,1010) IVAR, VNAME(IVAR), UNIQUE(IVAR)
 1010 FORMAT(/'Definition for variable number ',I3,', name ',A,
     *        ', Unique label ',A)
C
C *** Long variable description if entered
C
       IF( VDESC(IVAR) .NE. ' ' ) THEN
        WRITE(IRPT,1015) VDESC(IVAR)
 1015   FORMAT(3X,A)
      ENDIF
C
      IDIST  = VTYPE(IVAR)
      ITRUNC = VTRUN(IVAR)
      WRITE(IRPT,1020) DLABEL(IDIST)
 1020 FORMAT('   The distribution type is ',A)
C
C---- Constant distribution
C
      IF( IDIST .EQ. 0 ) THEN
        WRITE(IRPT,1030) VPARMS(IVAR,1)
 1030   FORMAT('   The constant value is ',1P,E11.4)
        RETURN
      ENDIF
C
C---- Uniform
C
      IF( IDIST .EQ. 1 ) THEN
        WRITE(IRPT,1040) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1040   FORMAT('   The lower limit is ',1P,E11.4/
     *         '   The upper limit is ',E11.4)
      ENDIF
C
C---- Log Uniform, base 10 (2) or base e (3)
C
      IF( IDIST.EQ.2 .OR. IDIST.EQ.3 ) THEN
        WRITE(IRPT,1040) VPARMS(IVAR,1), VPARMS(IVAR,2)
      ENDIF
C
C---- Normal
C
      IF( IDIST .EQ. 4 ) THEN
        WRITE(IRPT,1050) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1050   FORMAT('   The mean is',15X,1P,E11.4/
     *         '   The standard deviation is ',E11.4)
      ENDIF
C
C---- Log normal, base 10 (type 5), or base e (type 6)
C
      IF( IDIST.EQ.5 .OR. IDIST.EQ.6 ) THEN
        WRITE(IRPT,1050) VPARMS(IVAR,1), VPARMS(IVAR,2)
      ENDIF
C
C---- Exponential
C
      IF( IDIST .EQ. 7 ) THEN
        WRITE(IRPT,1060) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1060   FORMAT('   The mean is  ',1P,E11.4/
     *         '   The shift is ',E11.4)
      ENDIF
C
C---- Triangular
C
      IF( IDIST .EQ. 8 ) THEN
        WRITE(IRPT,1070) VPARMS(IVAR,1), VPARMS(IVAR,2), VPARMS(IVAR,3)
 1070   FORMAT('   The minimum is ',1P,E11.4/
     *         '   The mode is    ',E11.4/
     *         '   The maximum is ',E11.4)
      ENDIF
C
C---- Gamma
C
      IF( IDIST .EQ. 9 ) THEN
        WRITE(IRPT,1080) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1080   FORMAT('   The Exponent on x is',13X,1PE11.4/
     *         '   The parameter in the exponent is ',E11.4)
      ENDIF
C
C---- Beta
C
      IF( IDIST .EQ. 10 ) THEN
        WRITE(IRPT,1090) VPARMS(IVAR,1), VPARMS(IVAR,2),
     *                   VPARMS(IVAR,3), VPARMS(IVAR,4)
 1090   FORMAT('   The left end of range is ',1P,E11.4,
     +         ', the right end is ',E11.4/
     +         '   The parameter ''a'' is     ',E11.4,
     +         ', parameter ''b'' is ',E11.4)
      ENDIF
C
C---- Weibull
C
      IF( IDIST .EQ. 11 ) THEN
        WRITE(IRPT,1100) VPARMS(IVAR,1), VPARMS(IVAR,2), VPARMS(IVAR,3)
 1100   FORMAT('   The scale is    ',1P,E11.4/
     *         '   The exponent is ',E11.4/
     *         '   The shift is    ',E11.4)
      ENDIF
C
C---- Logistic
C
      IF( IDIST .EQ. 12 ) THEN
        WRITE(IRPT,1110) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1110   FORMAT('   The mean is',12X,1P,E11.4/
     *         '   The scale parameter is ',E11.4)
      ENDIF
C
C---- Cauchy
C
      IF( IDIST .EQ. 13 ) THEN
        WRITE(IRPT,1120) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1120   FORMAT('   The median is',10X,1P,E11.4/
     *         '   The scale parameter is ',E11.4)
      ENDIF
C
C---- User Defined
C
      IF( IDIST .EQ. 14 ) THEN
        NUSER = PTABLE(IVAR,2)
        NSTRT = PTABLE(IVAR,1)
        NTEND = NSTRT + NUSER - 1
        WRITE(IRPT,1130) NUSER, XTABLE(NSTRT), XTABLE(NTEND)
 1130   FORMAT('   There are ',I5,' entries in the function definition'/
     *         '   The first entry is at X = ',1P,E11.4/
     *         '   The last entry is at X  = ',E11.4)
      ENDIF
C
C---- Discrete Uniform
C
      IF( IDIST .EQ. 15 ) THEN
        I1 = VPARMS(IVAR,1)
        I2 = VPARMS(IVAR,2)
        WRITE(IRPT,1140) I1, I2
 1140   FORMAT('   The lower limit is ',I7/
     *         '   The upper limit is ',I7)
      ENDIF
C
C---- Echo of truncation statistics
C
      IF( ITRUNC .EQ. 1 ) THEN
        WRITE(IRPT,1150) VTLIM(IVAR,1), UMIN(IVAR)
 1150   FORMAT(
     *   '   The lower truncation limit is ',1P,E11.4,'   (',E9.3,')')
      ENDIF
C
      IF( ITRUNC .EQ. 2 ) THEN
CPWE        WRITE(IRPT,1160) VTLIM(IVAR,1), UMAX(IVAR)
        WRITE(IRPT,1160) VTLIM(IVAR,2), UMAX(IVAR)
 1160   FORMAT(
     *   '   The upper truncation limit is ',1P,E11.4,'   (',E9.3,')')
      ENDIF
C
      IF( ITRUNC .EQ. 3 ) THEN
        WRITE(IRPT,1150) VTLIM(IVAR,1), UMIN(IVAR)
        WRITE(IRPT,1160) VTLIM(IVAR,2), UMAX(IVAR)
      ENDIF
C
      RETURN
      END
