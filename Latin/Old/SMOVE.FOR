      SUBROUTINE SMOVE( JNDX )
C*....SUBROUTINE SMOVE( JNDX )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine moves the "stochastic" data from storage
C*    locations in the X matrix into storage locations in the
C*    output vector XOUT.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'FILES.ins'
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
C
C *** Loop over all of the variable distributions
C     But only move the "stochastic" variables
C
      ISTOC = 0
      DO 30 I = 1, PTOT
        IF( VTYPE(I) .NE. 0 ) THEN
          ISTOC = ISTOC + 1
          XOUT(I) = X(JNDX,ISTOC)
        ENDIF
   30 CONTINUE
C
      RETURN
      END
