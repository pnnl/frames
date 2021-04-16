      SUBROUTINE DATRPT
C*....SUBROUTINE DATRPT
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine writes the computed data to the report file.
C*    The data is written out in pages to facilitate keeping a copy
C*    of the the report file in a notebook.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'QA.ins'
      INCLUDE 'IDENC.ins'
      INCLUDE 'FILES.ins'
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
      INCLUDE 'DISTC.ins'
C
      DO 20 I = 1, PTOT, 5
        IEND = MIN(PTOT,(I+4))
C       Assume that a page has 50 lines of data
        DO 10 J = 1, N
          IF( MOD(J,50) .EQ. 1 ) THEN
C ***       Page header and problem title
            CALL PAGER( IRPT )
            WRITE(IRPT,1000) TITLE
 1000       FORMAT('Title: ',A)
            WRITE(IRPT,1005)
 1005       FORMAT(/'Echo of all generated data for all variables.')
            WRITE(IRPT,1010) (VNAME(K),K=I,IEND)
 1010       FORMAT(/5X,5(4X,A9))
          ENDIF
C ***     Data output
          JNDX = J
          CALL SMOVE( JNDX )
          WRITE(IRPT,1020) J,(XOUT(K),K=I,IEND)
 1020     FORMAT(1X,I4,1P,5(1X,E12.5))
   10   CONTINUE
C
   20 CONTINUE
C
      RETURN
      END
