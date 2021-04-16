c
      program ct
c
c  test case change
c  Test word is NAMEIN, A15
C
      character*15 NAMEIN, NAMEOUT, B
      data b/'               '/
C
C  READ TEST WORD
C
  1    READ(*,*) NAMEIN
      IF(NAMEIN.EQ.B) STOP
C
      DO IC = 1,15
        IF(NAMEIN(IC:IC).GE.'a') then   ! convert to upper case
          NAMEOUT(IC:IC) = char(ichar(NAMEIN(IC:IC)) - 32)
        ELSE
          NAMEOUT(IC:IC) = NAMEIN(IC:IC)
        ENDIF
      END DO
      WRITE(*,*) NAMEIN,NAMEOUT
      GO TO 1
      END
