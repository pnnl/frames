      SUBROUTINE RDBLK( IFIL, CARD )
C*....SUBROUTINE RDBLK( IFIL, CARD )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This module reads a block of data from a keyword driven input
C*    data file (logical unit IFIL).  This will be a keyword card and
C*    and all subsequent continuation cards, to the next keyword card.
C*
C*
C*  Notes:
C*
C*    On input to this routine, it is assumed the next line of the
C*    input data file is already present in array INFO, except on the
C*    first call (ILINE=0).  ILINE must be initialized to zero by the
C*    main routine (or block data).  The array INFO must not be
C*    modified between calls to RDBLK.  It saves the previous keyword
C*    line from the input file.
C*
C*    Output from this set of subroutines includes the keyword from
C*    the keyword line (NAME, Char*8), the data values from that card
C*    and subsequent continuation cards (VALUE), and secondary keywords
C*    (modifiers) on the input card block (CVALUE).  All output
C*    keywords are stored in upper case characters.  All numeric values
C*    are stored as real numbers (not integer).
C*
C*    The routines RDBLK and UPCASE requires alphabetic characters to
C*    be 'represented' contiguously in memory (a through z, and A
C*    through Z).
C*
C*    In routine SEPCHR, a TAB character is a valid separator only if
C*    it converts to a character code of less than 10 decimal.
C*    ASCII code is generally expected, but not necessary if these
C*    conditions prevail.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C*       Module RDBLK was programmed by Dave Langford,
C*       BCSR, 28 March 1989, in FORTRAN F77
C*
C*
C*  Variables:
C*
C*    CVALUE = Array of secondary keywords (modifiers) from the input
C*             data block.  Only the first eight characters are saved.
C*             (CHAR*8)
C*    IFIL   = Logical unit number for data read.
C*    ILINE  = Current line number of the input data file.
C*    INFO   = Saves the contents of the next line of the input file.
C*             (Dimension LENCRD, Char*1)
C*    NAME   = Keyword from the current data block.  (CHAR*8)
C*    NCVALU = Number of secondary keywords (modifiers) from data block.
C*    NVALUE = Number of data values from the data block.
C*    CARD   = Returns the first 120 characters of the keyword line,
C*             following the first separator.
C*    VALUE  = Array of data values from the input data block.
C*             These are stored as real values (not integers).
C*
C*  Subroutines:
C*
C*    RDBLK   Controlling routine.
C*    CEXIST  Logical function to determine if an 8 character string
C*            is present among the secondary keywords (modifiers)
C*            contained in CVALUE.
C*    COMCHR  Identifies comment characters.
C*    DCDSEP  Parses the input line into numeric and character data.
C*    FINSEP  Locates separators in the input line.
C*    FNAME   Extracts a file name (character string) from a input
C*            keyword card.
C*    SAVTTL  Moves the entire contents of a keyword line, following
C*            the first set of separators, into an output character
C*            array.
C*    SEPCHR  Determines if a character is a legal separator.
C*    UPCASE  Converts single characters to upper case.  Depends on
C*            contiguous 'ordering' of alphabetic characters.
C*    XFKEY   Transfers a keyword from a character string to a char*8
C*            variable.  Blank fills for strings less than length 8.
C*
C***********************************************************************
C
      INCLUDE 'CDATA.ins'
      INCLUDE 'FILES.ins'
C
      CHARACTER*120 CARD
      DIMENSION LOC(LENCRD)
      EXTERNAL COMCHR, SEPCHR
      LOGICAL  COMCHR, SEPCHR
C
      IERF=0
C
C             READ FIRST LINE(S) OF DATA FILE, TO A KEYWORD CARD
C
      IF (ILINE.LE.0) THEN
  100    CONTINUE
         ILINE=ILINE+1
         READ (IFIL,500,END=470,ERR=480,IOSTAT=IERF)
     +        (INFO(I),I=1,LENCRD)
         IF (((INFO(1).LT.'A').OR.(INFO(1).GT.'Z')).AND.
     +       ((INFO(1).LT.'a').OR.(INFO(1).GT.'z'))) GO TO 100
      ENDIF
C
      NCVALU=0
      NVALUE=0
      DO 110 I=1,NKEYS
         CVALUE(I) = '        '
  110 CONTINUE
      DO 120 I=1,NVALS
         VALUE(I) = 0.0
  120 CONTINUE
C
C             MOVE KEYWORD INTO ARRAY NAME, WITH BLANK FILLING.
C
      CALL XFKEY (INFO(1),NAME)
C
C             MOVE LINE DATA INTO ARRAY CARD
C
      CALL SAVTTL (CARD)
C
C *** Return to the calling program if an END card is encountered
C     This is the graceful exit to signify the end of the input file
C
      IF( NAME .EQ. 'END' ) RETURN
C
C            LOCATE ALL SEPARATORS IN THIS LINE.
C            THIS IS TOP OF LOOP FOR READING CONTINUATION CARDS.
C
  130 CONTINUE
      CALL FINSEP (NUM,LOC)
C
C            MOVE CHARACTER DATA INTO ARRAY CVALUE,
C            AND NUMERIC DATA INTO ARRAY VALUE.
C
      IF (NUM.GT.1) CALL DCDSEP (NUM,LOC)
C
C             READ NEXT LINE AND REFORMAT
C
  140 CONTINUE
      ILINE=ILINE+1
      READ (IFIL,500,END=150,ERR=480,IOSTAT=IERF) (INFO(I),I=1,LENCRD)
C
C              RETURN IF NEXT KEYWORD CARD WAS FOUND
C
      IF (((INFO(1).GE.'A').AND.(INFO(1).LE.'Z')).OR.
     +    ((INFO(1).GE.'a').AND.(INFO(1).LE.'z'))) RETURN
 
C
C             IF COMMENT CARD, READ THE NEXT LINE.
C
      IF (COMCHR(1,INFO(1))) GO TO 140
C
C             CHECK FOR CONTINUATION CARD.  LOOP UP TO DECODE.
C
      IF (.NOT.SEPCHR(INFO(1))) THEN
         WRITE(IRPT,510)
         GO TO 490
      ENDIF
      GO TO 130
C
C             ARTIFICIAL END-FILE CARD GENERATED
C
  150 CONTINUE
      WRITE(IRPT,520)
      INFO(1)='E'
      INFO(2)='N'
      INFO(3)='D'
      INFO(4)='-'
      INFO(5)='F'
      INFO(6)='I'
      INFO(7)='L'
      INFO(8)='E'
      INFO(9)=' '
      RETURN
C
C             ERRORS
C
  470 CONTINUE
      WRITE(IRPT,970)
      GO TO 490
  480 CONTINUE
      WRITE(IRPT,980)
  490 CONTINUE
      WRITE(IRPT,990) ILINE,IERF
      STOP
C
  500 FORMAT(150A1)
  510 FORMAT(/
     *   ' *** Terminal error in subroutine RDBLK ***'/
     *   '     Error, A continuation card is expected, but the ',
     *   '     current line does not begin with a valid ',
     *   '     separator character.')
C
  520 FORMAT(/
     *   ' *** Warning error in subroutine RDBLK ***'/
     *   '     End-of-File encountered on the input file.'
     *   '     An artificial END-FILE card was generated.')
C
  970 FORMAT(/
     *   ' *** Terminal error in subroutine RDBLK ***'/
     *   '     End-of-File encountered in the input file'/
     *   '     prior to reading a keyword card.')
C
  980 FORMAT(/
     *   ' *** Terminal error in subroutine RDBLK ***'/
     *   '     Error reading data from input file.')
C
  990 FORMAT(/
     *   ' *** Terminal error in subroutine RDBLK ***'/
     *   '     Reading line number  ',I5/
     *   '     I/O Errors Status is ',I5/
     *   '     Error stop in routine RDBLK.')
      END
