      LOGICAL FUNCTION COMCHR( IPLACE, CH )
C*....LOGICAL FUNCTION COMCHR( IPLACE, CH )
C***********************************************************************
C*
C*  Purpose:
C*
C*
C*    This subroutine determines if the character CH is a comment
C*    identifying character.  Comment lines are ignored, as are
C*    characters after a comment identifier on one line.
C*
C*    The call argument IPLACE indicates the position of character CH
C*    in the input line.  Note that the '/' and '*' characters indicate
C*    comments only if they are the first characters in a line.
C*
C*    This routine is a part of the set of routines called by RDBLK.
C*
C*
C*  History:
C*
C*    David W. Langford : 01 Jul 1992 : Version 1.0
C*
C***********************************************************************
C
      CHARACTER*1 CH
C
      IF ((CH.EQ.'!').OR.(CH.EQ.'$')) THEN
         COMCHR=.TRUE.
      ELSE
         IF (((CH.EQ.'/').OR.(CH.EQ.'*')).AND.(IPLACE.EQ.1)) THEN
            COMCHR=.TRUE.
         ELSE
            COMCHR=.FALSE.
         ENDIF
      ENDIF
C
      RETURN
      END
