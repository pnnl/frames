      SUBROUTINE WRDATA
C*....SUBROUTINE WRDATA
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine writes a data file of all computed data.
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
      INCLUDE 'QA.ins'
C
C *** Header for the output file
C
      CALL PAGER( IDAT )
      WRITE(IDAT,1000) TITLE
 1000 FORMAT(A)
      WRITE(IDAT,1010) PTOT, N
 1010 FORMAT(2(1X,I5),' Number of variables and number of iterations.')
C
C *** Data output for all realizations and all variables
C
      ISTOC = 0
      DO 30 J = 1, PTOT
C
C ***   Write out the variable name and the distribution index
C
        WRITE(IDAT,1020) VNAME(J), UNIQUE(J), VTYPE(J), VDESC(J)
 1020   FORMAT(A9,1X,A5,1X,I2/A76)
C
        IF( VTYPE(J) .NE. 0 ) THEN
C
C ***     Write out a "stochastic" variable
C
          ISTOC = ISTOC + 1
          DO 10 I = 1, N
            WRITE(IDAT,1030) I, X(I,ISTOC)
 1030       FORMAT(1X,I5,1P,1X,E11.4)
   10     CONTINUE
C
        ELSE
C
C ***     Write out a "constant" variable
C
          DO 20 I = 1, N
            WRITE(IDAT,1030) I, XOUT(J)
   20     CONTINUE
C
        ENDIF
C
   30   CONTINUE
C
      RETURN
      END
