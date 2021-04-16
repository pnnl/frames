      SUBROUTINE XFKEY( INFO, KEYWRD )
C*....SUBROUTINE XFKEY( INFO, KEYWRD )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine examines the first eight characters in array INFO
C*    in order to move that data into variable KEYWRD.  If a separator
C*    is present, the relevant characters are transferred to variable
C*    KEYWRD, and the remainder of KEYWRD is blank filled.
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
      CHARACTER*1 INFO(8), JNFO(8)
      CHARACTER*8 KEYWRD
      EXTERNAL SEPCHR
      LOGICAL  SEPCHR
C
      DO 110 I=1,8
         JNFO(I)=INFO(I)
         IF (SEPCHR(INFO(I))) THEN
            DO 100 J=I,8
               JNFO(J)=' '
  100       CONTINUE
            GO TO 120
         ENDIF
  110 CONTINUE
  120 CONTINUE
C
      DO 130 I=1,8
         CALL UPCASE (JNFO(I))
  130 CONTINUE
      WRITE(KEYWRD,500) (JNFO(I),I=1,8)
  500 FORMAT(8A1)
C
      RETURN
      END
