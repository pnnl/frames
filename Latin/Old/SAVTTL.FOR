      SUBROUTINE SAVTTL( CARD )
C*....SUBROUTINE SAVTTL( CARD )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine moves the first 120 characters of the input line,
C     following the first separator, into the array CARD.
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
      CHARACTER*120 CARD
      EXTERNAL SEPCHR
      LOGICAL  SEPCHR
C
      IST=0
      DO 100 I = 1, 120
         IF (IST.EQ.0) THEN
            IF (SEPCHR(INFO(I))) IST=I
         ELSE
            IF (.NOT.SEPCHR(INFO(I))) THEN
               IST=I
               GO TO 110
            ENDIF
         ENDIF
  100 CONTINUE
      WRITE(CARD,500) (INFO(I),I=1,120)
      RETURN
C
  110 CONTINUE
      NB=IST-1
      WRITE(CARD,500) (INFO(I),I=IST,120),(' ',I=1,NB)
      RETURN
  500 FORMAT(120A1)
      END
