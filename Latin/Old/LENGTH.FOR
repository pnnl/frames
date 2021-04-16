      SUBROUTINE LENGTH( CARD, MAXC, LENC )
C*....SUBROUTINE LENGTH( CARD, MAXC, LENC )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine counts then number of characters in the variable
C*    CARD after trailing blanks are removed.  A length of 0 is
C*    returned if no nonblank characters were found.
C*
C*
C*  Variables:
C*
C*    CARD : Input  : CHARACTER*1 (MAXC) card image
C*    LENC : Output : Number of characters remaining after trimming
C*                    all trailing blanks.
C*    MAXC : Input  : Maximum number of characters to examine
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Version 1.0
C*
C***********************************************************************
C
      CHARACTER*1 BLANK
      CHARACTER*1 CARD(MAXC)
C
      DATA BLANK /' '/
C
C *** Check all MAXC characters starting from the right and working
C     to the left
C
      DO 10 I = 1, MAXC
        LENC = MAXC - I + 1
        IF( CARD(LENC) .NE. BLANK ) RETURN
   10 CONTINUE
C
      LENC  = 0
      RETURN
      END
