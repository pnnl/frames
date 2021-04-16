      SUBROUTINE FINSEP( NUM, LOC )
C*....SUBROUTINE FINSEP( NUM, LOC )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine locates all the separators in the current input
C*    line, INFO.  The number of separators is placed in NUM, and the
C*    location of each is placed in array LOC.  A dummy separator is
C*    placed at position 81, if the last character in array INFO if a
C*    relevant character (not a comment or separator).
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
      INCLUDE 'CDATA.ins'
C
      DIMENSION LOC(LENCRD)
      EXTERNAL COMCHR, SEPCHR
      LOGICAL  COMCHR, SEPCHR
C
      NUM=0
      LENGTH=0
C
      DO 100 I=1,LENCRD
         IF (INFO(I).NE.' ') LENGTH=I
  100 CONTINUE
      IF (LENGTH.LT.LENCRD) LENGTH=LENGTH+1
      IF (LENGTH.EQ.0) RETURN
C
C             LOOP ON THE CHARACTERS OF THE INPUT LINE.  CHECK FIRST
C             FOR A COMMENT CHARACTER, THEN FOR A SEPARATOR CHARACTER.
C
      DO 110 I=1,LENGTH
         IF (COMCHR(I,INFO(I))) THEN
            IF ((I.GT.1).AND.(NUM.GT.0)) THEN
               IF (LOC(NUM).LT.(I-1)) THEN
                  NUM=NUM+1
                  LOC(NUM)=I
               ENDIF
            ENDIF
            RETURN
         ENDIF
         IF (SEPCHR(INFO(I))) THEN
            NUM=NUM+1
            LOC(NUM)=I
         ENDIF
  110 CONTINUE
C
      IF ((LENGTH.EQ.LENCRD).AND.(.NOT.SEPCHR(INFO(LENCRD)))) THEN
         NUM=NUM+1
         LOC(NUM)=LENCRD+1
      ENDIF
      RETURN
      END
