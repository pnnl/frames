c
      SUBROUTINE UPPERC(NAMEIN,NCAR,NAMEOUT)
c
c  test case change
c  Test word is NAMEIN, A15
C
      character*8  NAMEIN,NAMEOUT
      integer NCAR
C
C
      DO IC = 1,NCAR
        IF(NAMEIN(IC:IC).GE.'a') then   ! convert to upper case
          NAMEOUT(IC:IC) = char(ichar(NAMEIN(IC:IC)) - 32)
        ELSE
          NAMEOUT(IC:IC) = NAMEIN(IC:IC)
        ENDIF
      END DO
C     WRITE(*,*) NAMEIN,NAMEOUT
      RETURN
      END
