      SUBROUTINE UPCASE( CH )
C*....SUBROUTINE UPCASE( CH )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine takes the character CH and ensures it is upper
C*    case.  It also assumes that character storage codes are
C*    alphabetically contiguous.
C*
C*
C*  History:
C*
C*    Dave Langford :  1 Jun 1989 : Original Source
C*    Paul Eslinger : 15 Jan 1990 : Add explicit character numbers
C*
C***********************************************************************
C
      CHARACTER*1 CH
C
C *** Determine the offset between 'A' and 'a' to use in the expression
C         CH = CHAR( ICHAR(CH) + ICHAR('A') - ICHAR('a') )
C *** The ASCII Character set uses ICHAR('A')=65 and ICHAR('a')=97
C
      IF( (CH.GE.'a') .AND. (CH.LE.'z') ) CH = CHAR(ICHAR(CH)-32)
C
      RETURN
      END
