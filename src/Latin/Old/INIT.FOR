      SUBROUTINE INIT
C*....SUBROUTINE INIT
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine initializes some variables to allow later output
C*    or error checking for illegal values.
C*
C*
C*  Calling Sequence:
C*
C*    A call to this subroutine can be preceeded by calls to only
C*    the subroutines IDENC, QASET, SCREEN and FIRSTF.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'FILES.ins'
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
      INCLUDE 'DISTC.ins'
      INCLUDE 'SCREENS.ins'
      INCLUDE 'SPINATTR.ins'
C
C *** Initialize the list of variable types to an illegal value
C     to facilitate later error checking
C
      DO 10 I = 1, MAXPTL
        VTYPE(I)  = -1
        VNAME(I)  = ' '
        UNIQUE(I) = ' '
        VDESC(I)  = ' '
   10 CONTINUE
C
C *** Set statistical distribution labels
C
      DLABEL(0)  = 'Constant        '
      DLABEL(1)  = 'Uniform         '
      DLABEL(2)  = 'Loguniform (10) '
      DLABEL(3)  = 'Loguniform (e)  '
      DLABEL(4)  = 'Normal          '
      DLABEL(5)  = 'Lognormal (10)  '
      DLABEL(6)  = 'Lognormal (e)   '
      DLABEL(7)  = 'Exponential     '
      DLABEL(8)  = 'Triangular      '
      DLABEL(9)  = 'Gamma           '
      DLABEL(10) = 'Beta            '
      DLABEL(11) = 'Weibull         '
      DLABEL(12) = 'Logistic        '
      DLABEL(13) = 'Cauchy          '
      DLABEL(14) = 'User Defined    '
      DLABEL(15) = 'Discrete Uniform'
C
C *** Screen colors
C
      ICDEF = BRITE + YELCHR
      ICBLB = BRITE + WHTCHR
      ICOPT = BRITE + REDCHR
      ICTIT = BRITE + GRNCHR
      ICNOR = NORMAL
      ICMNU = WHTCHR + REDBKG + BRITE
C
C *** Maximum length of an error message line
C
      LERR = 78
C
      RETURN
      END
