      SUBROUTINE FIRSTF
C*....SUBROUTINE FIRSTF
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine handles opening the first three files needed by
C*    the simulation.
C*
C*    Execution is terminated if the file actions are not completed
C*    successfully.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INCLUDE 'SPINATTR.ins'
      INCLUDE 'FILES.ins'
      INCLUDE 'SCREENS.ins'
      INCLUDE 'IDENC.ins'
C
      LOGICAL THERE
      CHARACTER*64 FCASE
C
C *** Obtain the file containing the file "case" names
C
      IMID = 20
      FNMID = 'SENS.ID'
      INQUIRE(FILE=FNMID,EXIST=THERE)
      IF( THERE ) THEN
        OPEN(IMID,FILE=FNMID,STATUS='OLD',ERR=9010)
      ELSE
        CERR(1) =
     *    'The file containing the run "case" names was not found.'
        CERR(2) = 'The run cannot continue without this file.'
        CERR(3) = 'File: ' // FNMID
        NERR = 3
        CALL GDERR( NERR, 'FIRSTF' )
        CALL STWO
        STOP
      ENDIF
C
C *** Get the case names for the other file names
C     Read and discard the case name for the base case
C
      READ(IMID,*,END=9020,ERR=9020) FCASE
      READ(IMID,*,END=9020,ERR=9020) FCASE
C
C *** Check on the case name
C
      L64 = 64
      CALL LENGTH( FCASE, L64, LEN )
C
C *** Look for an embedded period
C
      DO 10 I = 1, LEN
        IF( FCASE(I:I) .EQ. '.' ) THEN
          LEN = I - 1
          GO TO 20
        ENDIF
   10 CONTINUE
   20 CONTINUE
C
C *** Error check on the length of the case name
C
      IF( LEN .LT. 1 ) THEN
        CERR(1) = 'The number of chacarcters in the "case" name is 0.'
        CERR(2) = 'Check the file ' // FNMID
        CERR(3) = 'To see that it contains proper case names.'
        NERR = 3
        CALL GDERR( NERR, 'FIRSTF' )
        CALL STWO
        STOP
      ENDIF
C
C *** Now form the error message file name
C
      IERR = 8
      FNERR = FCASE(1:LEN) // '.LER'
C
C *** Open the output error file
C
      INQUIRE(FILE=FNERR,EXIST=THERE)
      IF( THERE ) THEN
        OPEN(IERR,FILE=FNERR,ERR=9000)
        CLOSE(IERR,STATUS='DELETE')
        OPEN(IERR,FILE=FNERR,ERR=9000)
      ELSE
        OPEN(IERR,FILE=FNERR,ERR=9000)
      ENDIF
      CALL PAGER( IERR )
C
C *** Obtain the input keyword file name
C
      IKEY = 9
      FNKEY = FCASE(1:LEN) // '.KEY'
C
C *** Open the input keyword file
C
      INQUIRE(FILE=FNKEY,EXIST=THERE)
      IF( THERE ) THEN
        OPEN(IKEY,FILE=FNKEY,STATUS='OLD',ERR=9010)
      ELSE
        CERR(1) = 'Error opening the input keyword file to ' // PRGNAM
        CERR(2) = 'The run cannot continue without this file.'
        CERR(3) = 'File: ' // FNKEY
        NERR = 3
        CALL GDERR( NERR, 'FIRSTF' )
        CALL STWO
        STOP
      ENDIF
C
C *** Message for the user
C
      IR1 = 12
      IR2 = 14
      CTITLE = 'Working on Sensitivity Case ' // FCASE(1:LEN)
      L78 = 78
      CALL LENGTH( CTITLE, L78, LENTIT )
      LMAX = LENTIT
C
      IC1 = 40 - LMAX/2 - 1
      IC2 = IC1 + LMAX + 1
C
      IENUM = 9993
      CALL DEFWIND(IENUM, IR1, IC1, IR2, IC2, DBLBDR, ICOPT)
      CALL CLRB(IENUM)
C
      IC3 = 0
      IR3 = 0
      CALL BLDB(IENUM, IR3, IC3, CTITLE(1:LMAX), NORMAL )
C
      RETURN
C
C *** Error opening the output error file
C
 9000 CONTINUE
C
      CERR(1) = 'Error opening the output file that will contain'
     *       // ' error messages.'
      CERR(2) = 'The run cannot continue without this file.'
      CERR(3) = 'File: ' // FNERR
      NERR = 3
      CALL GDERR( NERR, 'FIRSTF' )
      CALL STWO
C
C *** Error opening the input case name file
C
 9010 CONTINUE
C
      CERR(1) = 'Error opening the input file containing "case" names.'
      CERR(2) = 'The run cannot continue without this file.'
      CERR(3) = 'File: ' // FNMID
      NERR = 3
      CALL GDERR( NERR, 'FIRSTF' )
      CALL STWO
      STOP
C
C *** Error reading the input case name file
C
 9020 CONTINUE
C
      CERR(1) = 'Error reading the file containing "case" names.'
      CERR(2) = 'The run cannot continue without this file.'
      CERR(3) = 'File: ' // FNMID
      NERR = 3
      CALL GDERR( NERR, 'FIRSTF' )
      CALL STWO
      STOP
C
      END
