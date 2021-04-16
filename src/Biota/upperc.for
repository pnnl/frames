c
      SUBROUTINE UPPERC(NAMEIN,NCAR,NAMEOUT)
c
C     Last change:  PWE   6 May 98    Expanded CHARACTER range
C
c      character*80  NAMEIN,NAMEOUT
      character*(*)  NAMEIN,NAMEOUT
      integer NCAR
C
C
      DO IC = 1,NCAR
        IF(NAMEIN(IC:IC).GE.'a'.AND.NAMEIN(IC:IC).LE.'z') then   ! convert to upper case
          NAMEOUT(IC:IC) = char(ichar(NAMEIN(IC:IC)) - 32)
        ELSE
          NAMEOUT(IC:IC) = NAMEIN(IC:IC)
        ENDIF
      END DO
C     WRITE(*,*) NAMEIN,NAMEOUT
      RETURN
      END
