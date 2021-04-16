      LOGICAL FUNCTION SEPCHR( CH )
C*....LOGICAL FUNCTION SEPCHR( CH )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This function determines if the character CH is a legal command
C*    line separator character.
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
      IF( (ICHAR(CH).LT.10) .OR.
     +    (CH.EQ.' ') .OR. (CH.EQ.',')  .OR.
     +    (CH.EQ.'=') .OR. (CH.EQ.':')  .OR.
     +    (CH.EQ.';') .OR. (CH.EQ.'''') .OR.
     +    (CH.EQ.'(') .OR. (CH.EQ.')') ) THEN
         SEPCHR=.TRUE.
      ELSE
         SEPCHR=.FALSE.
      ENDIF
C
      RETURN
      END
