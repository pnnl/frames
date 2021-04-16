      LOGICAL FUNCTION CEXIST( KEYWRD )
C*....LOGICAL FUNCTION CEXIST( KEYWRD )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This is a logical function to determine if a eight letter keyword
C*    is one of the secondary keywords (modifiers) of the current data
C*    block.
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
      CHARACTER*8 KEYWRD
C
      CEXIST=.FALSE.
      DO 100 N = 1, NCVALU
         IF( CVALUE(N) .EQ. KEYWRD ) THEN
            CEXIST = .TRUE.
            RETURN
         ENDIF
  100 CONTINUE
C
      RETURN
      END
