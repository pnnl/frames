      SUBROUTINE INPUTS
C*....SUBROUTINE INPUTS
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine reads the control keywords from the input file.
C*    It stores the data for later use and performs preliminary
C*    checking on the input data.  Messages concerning error conditions
C*    encountered are written to the "error" file.  Execution is
C*    terminated if errors are found in the input data set.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*    Paul W. Eslinger : 25 Aug 1992 : Change to add PRELATIN index to
C*            the VARIABLE, DESCRIBE, UNIQUE, and LABEL cards.
C*    Paul W. Eslinger : 27 Aug 1992 : Change indexing scheme for the
C*            VARIABLE card.
C*
C*
C*  Keyword Conventions:
C*
C*    The data file contains control information given by keywords.
C*    A maximum of 8 letters are used to define individual keywords.
C*
C*    If the default value is desired, the keyword definition does
C*    not need to be placed in the input file.
C*
C*    Separators between numeric values, keywords and modifiers
C*    can be a space or the characters ",", "=", ":", ";", "'", "(",
C*    or ")".
C*
C*    Keyword   : Description
C*    -----------------------------------------------------------------
C*
C*    $         : Comment designator.  Any text or numeric values
C*                following the $ are ignored.
C*
C*    !         : Comment designator.  Any text or numeric values
C*                following the ! are ignored.
C*
C*    /         : Comment designator if used in column 1 of a input
C*                data line.  It is ignored if embedded within a line.
C*
C*    *         : Comment designator if used in column 1 of a input
C*                data line.  It is treated as a repeat indicator if
C*                it is embedded within a line.  I.e., 6*50. means
C*                the same as entering the number 50.0 six times.
C*
C*    -----------------------------------------------------------------
C*
C*    CORRELAT  This keyword controls specification of the correlation
C*              matrix for all "stochastic" variables.  If the modifier
C*              IDENTITY is present then all other data for this
C*              keyword is ignored and the identity matrix will be
C*              used (i.e. all variables will be stistically
C*              independent).  Otherwise, several numerical entries
C*              must be made.  The first entry is an integer giving
C*              the number of variables for which the correlation
C*              matrix is being defined.  Subsequent entries given
C*              the values in the correlation matrix.  Only the lower
C*              triangular part of the matrix is entered, in a
C*              sequence that is row by row.  For example, for 4
C*              variables the following card could be used:
C*                 CORRELATion for 4 variables
C*                  1.0
C*                  0.995 1.0
C*                  0.99  0.995 1.0
C*                  0.98  0.99  0.995 1.0
C*
C*    DESCRIBE  This keyword is used to input a long description for
C*              every variable.  The description can contain up to 76
C*              characters.  The correct format is
C*                DESCRIBE N1 N2 "String..."
C*              The string is contained between double quote marks.
C*              The number N1 is ignored (used only in PRELATIN).
C*              The number N2 must match with a variable number on
C*              a VARIABLE card.  One DESCRIBE card must be entered for
C*              each VARIABLE card.  All of the entries for this
C*              keyword must be made on the same line.  No
C*              continuation cards are allowed.
C*
C*    END       This keyword signals the end of the entire set of
C*              keyword definitions.  It must come as the last keyword
C*              of the data set.  A fatal execution error occurs if
C*              this keyword is not found.
C*
C*    EXECUTE   This keyword tells the code to execute the problem
C*              being set up.  If this keyword is not entered the
C*              data will be read and checked for errors but the
C*              requested data will not be generated.
C*
C*    ITERATE   This keyword is used to define the number of
C*              iterations of the output data to compute.  A single
C*              numeric value follows the keyword.  It must be at
C*              least as large as the number of "stochastic"
C*              variables in the problem definition.
C*
C*    LABEL     This keyword is used to define an output label for
C*              every variable.  The label can contain up to 9
C*              characters.  The correct format is
C*                LABEL N1 N2 "Label..."
C*              The label is contained between double quote marks.
C*              The number N1 is ignored (used only in PRELATIN).
C*              The number N2 must match with a variable number on
C*              a VARIABLE card.  One LABEL card must be entered for
C*              each VARIABLE card.  All of the entries for this
C*              keyword must be made on the same line.  No
C*              continuation cards are allowed.
C*
C*    FILE      This keyword controls the naming of two files, the
C*              report file and the data file.  The correct syntax
C*              for opening the report file is:
C*                FILE REPORT "file name"
C*              The file name can be up to 64 characters long and
C*              can contain a path in the file name.  It must be
C*              enclosed in double quote marks.  The entries for
C*              this keyword must be entered all on the same line,
C*              no continuation lines can be used.  The correct
C*              syntax for opening the data file is:
C*                FILE DATA "file name"
C*              The same conventions hold for this file name as for
C*              the report file name.  Two separate FILE keywords
C*              are required to open the two files.
C*
C*    OUTPUT    This keyword controls the output of two things to
C*              the report file.  Two modifiers can be entered:
C*                DATA     If this modifier is present then a copy
C*                         of all computed data is written to the
C*                         report file.
C*                CORRELAT If this modifier is present then the
C*                         correlation matrix is computed and
C*                         written to the report file.  In addition
C*                         a copy of the input data matrix is
C*                         written to the report file along with
C*                         a comparison of the maximum difference
C*                         between the two matrices.
C*
C*    SEED      This keyword is used to define the seed for the
C*              random number generator.  A single numerical value
C*              follows the keyword.  It must be in the range 1 to
C*              2147483646.  Even though the seed is stored and
C*              used as a double precision value it is read as a
C*              single precision value.
C*
C*    TITLE     Keyword to define a problem title.  The title can
C*              contain up to 72 characters.  The correct format is
C*                 TITLE "Title string"
C*              The title is contained between double quote marks.
C*              All of the entries for this keyword must be made
C*              on the same line.  No continuation cards are allowed.
C*
C*    UNIQUE    This keyword is used to define a unique name for
C*              every variable.  The name can contain up to 7
C*              characters.  The correct format is
C*                UNIQUE N1 N2 "Name..."
C*              The name is contained between double quote marks.
C*              The number N1 is ignored (used only in PRELATIN).
C*              The number N2 must match with a variable number on
C*              a VARIABLE card.  One UNIQUE card must be entered for
C*              each VARIABLE card.  All of the entries for this
C*              keyword must be made on the same line.  No
C*              continuation cards are allowed.
C*
C*    USER      Keyword to define a user name.  The user name can
C*              contain up to 16 characters.  The correct format is
C*                 USER "user-name"
C*              The name is contained between double quote marks.
C*              All of the entries for this keyword must be made
C*              on the same line.  No continuation cards are allowed.
C*              The default name is defined in subroutine IDENC.
C*
C*    VARIABLE  This keyword controls entering the definition of the
C*              statistical distribution for each variable.  The
C*              distribution can be defined as a constant.  The first
C*              entry, N1, is ignored (used only in PRELATIN).  The
C*              second entry, N2, gives the variable number.  The
C*              variable numbers must complete the set 1 to K, where K
C*              is the total number of variables being defined.
C*              Variables do not have to be defined in sequential order.
C*              The third entry, N3, defines the distribution type for
C*              the variable.  Type 0 is a constant.  In all, MXDIST
C*              (currently 15) distributions are allowed.  See the
C*              header for subroutine SGEN for the specific
C*              distributions.  If the distribution defines a
C*              constant, the fourth, and last entry, N4 is the desired
C*              constant.  If the distribution is of other types,
C*              then N4 is a truncation variable.
C*                N4 = 0 : No truncation of the range of the
C*                         variable.
C*                N4 = 1 : The range of the variable is truncated at
C*                         the left at the value N5.
C*                N4 = 2 : The range of the variable is truncated at
C*                         the right at the value N5.
C*                N4 = 3 : The range of the variable is truncated at
C*                         the left at N5 and on the right at N6.
C*
C*              If types 1 through 13 are desired, then the remaining
C*              (up to 4) numerical entries give the parameters of the
C*              distributions.  See the header for subroutine SGEN for
C*              the specific parameters for each distribution. An
C*              example for a uniform(2,4) distribution is:
C*                VARIABLE 2 TYPE 1 truncation flag 0 Uniform on 2 to 4
C*
C*              If type 14 is desired, then the user must input a table
C*              of values specifying the cumulative distribution
C*              function desired.  In this case, the next numerical
C*              entry specifies the number of (X,F(X)) pairs to be
C*              entered.  The entries must start with F(X)=0 and
C*              conclude with F(X)=1.  Subsequent entries for X and
C*              F(X) must be in strictly increasing order. An
C*              example input is:
C*                VARIABLE 1 type 14 truncation flag 0 with 3 data pairs
C*                  0.0 0.0
C*                  2.0 0.5
C*                  4.0 1.0
C*
C*              A separate VARIABLE card must be used to define each
C*              variable.
C*    -----------------------------------------------------------------
C*
C***********************************************************************
C
      INCLUDE 'PARAMS.ins'
      INCLUDE 'CONTROL.ins'
      INCLUDE 'DATA.ins'
      INCLUDE 'FILES.ins'
      INCLUDE 'CDATA.ins'
      INCLUDE 'IDENC.ins'
      INCLUDE 'DISTC.ins'
      INCLUDE 'SCREENS.ins'
C
      LOGICAL CEXIST
      EXTERNAL CEXIST
C
      INTEGER VINDEX
C
      CHARACTER CARD*120, TMPNAM*16
C
C *** Patch in the keyword file number to use RDBLK without modification
C
      IFIL = IKEY
C
C *** Initialize the line counter (ILINE) for subroutine RDBLK and,
C       LENU: maximum length of a user name for subroutine FNAME
C       LENF: maximum length of a file name for subroutine FNAME
C       LENC: maximum length of a card image for RDBLK
C       LENL: Maximum length of a variable label
C       LENT: maximum length of a title on a keyword card
C
      ILINE =  0
      LENC  = 120
      LENU  = 16
      LENL  =  9
      LENF  = 64
      LENT  = 72
C
C ---------------------------------------------------------------------
C                 Default values for the keywords
C ---------------------------------------------------------------------
C
C *** CORRELAT Keyword
      CIDENT = .FALSE.
      NCOR = 0
C
C *** EXECUTE Keyword
      EXECUT = .FALSE.
C
C *** FILE Keyword
      FNDAT = ' '
      FNRPT = ' '
C
C *** ITERATE Keyword
      N = 0
C
C *** OUTPUT Keyword
      ODATA = .FALSE.
      OCORR = .FALSE.
C
C *** SEED Keyword
      DSEED = 123456.0
C
C *** TITLE Keyword
      TITLE = 'Title Not Entered by the User'
C
C *** USER Keyword
      USRNAM = 'Anonymous User'
C
C *** VARIABLE Keyword
      P = 0
      PCON = 0
      PTOT = 0
      IDXTBL = 1
C
C ---------------------------------------------------------------------
C               Top of the loop on reading input cards
C ---------------------------------------------------------------------
C
      BADKEY = .FALSE.
   10 CONTINUE
C
      CALL RDBLK( IFIL, CARD )
C
C ---------------------------------------------------------------------
C CORRELAT:            Correlation matrix definition
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'CORRELAT' ) THEN
C
C ***   Check first for duplicate entry that could cause processing
C       problems
C
        IF( CIDENT .OR. NCOR.GT.0 ) THEN
          WRITE(IERR,6335)
 6335     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     A duplicate entry of the CORRELATION keyword ',
     *     'was encountered'/
     *     '     Only one entry is allowed.'/
     *     '     Modify the CORRELATION keyword')
          BADKEY = .TRUE.
        ENDIF
C
C ***   Check first for use of the identity matrix
C
        IF( CEXIST('IDENTITY') ) THEN
          CIDENT = .TRUE.
          GO TO 10
        ENDIF
C
C ***   Store the correlation matrix
C ***   Also save a copy of the correlation matrix for later use
C
        NUSE = VALUE(1)
        IF( NUSE.LE.0 .OR. NUSE.GT.MAXP ) THEN
          WRITE(IERR,7019) NUSE, MAXP
 7019     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     The number of variables in the correlation'/
     *     '     keyword is illegal.  Value entered = ',I5/
     *     '     legal range is 1 to ',I5/
     *     '     Modify the CORRELATION keyword or change the '/
     *     '     parameter MAXP (and MAXPTL) and recompile the code')
           STOP 'CORR-NUSE'
        ENDIF
        NCOR = NUSE * (NUSE+1) / 2
        DO 100 I = 1, NCOR
          COR(I) = VALUE(I+1)
          CORSAV(I) = VALUE(I+1)
  100   CONTINUE
C
        GO TO 10
C
      ENDIF
C
C ---------------------------------------------------------------------
C DESCRIBE:     Check for variable description
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'DESCRIBE' ) THEN
C
        LINDEX = VALUE(2)
C
C ***   Error check on dimensions of the VNAME array
C
        IF( LINDEX.LT.1 .OR. LINDEX.GT.MAXP ) THEN
          WRITE(IERR,8111) LINDEX, MAXP, CARD
 8111     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Execution stop due to an illegal index from the '/
     *     '     DESCRIBE keyword.  Value= ',I5,' limits are 1 to ',I3/
     *     '     CARD = ',A)
          STOP 'DESC-1'
        ENDIF
C
        LEN76 = 76
        CALL FNAME( CARD, LENC, VDESC(LINDEX), LEN76, IERF )
        IF( IERF .NE. 0 ) THEN
          WRITE(IERR,7349) CARD
 7349     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Bad description from keyword DESCRIBE'/
     *     '     Card = ',A)
          BADKEY = .TRUE.
        ELSE
        ENDIF
        GO TO 10
      ENDIF
C
C ---------------------------------------------------------------------
C END:     Check for user supplied end to the keyword file
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'END     ' ) GO TO 20
C
C ---------------------------------------------------------------------
C END-FILE:        Check for abnormal end-of-file in the keyword file
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'END-FILE' ) THEN
        WRITE(IERR,7046)
 7046   FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Execution stop due to an unexpected'/
     *     '     end-of-file in the keyword data file.')
        STOP 'END-FILE'
      ENDIF
C
C ---------------------------------------------------------------------
C EXECUT:         Flag for problem execution after setup
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'EXECUTE ' ) THEN
        EXECUT = .TRUE.
        GO TO 10
      ENDIF
C
C ---------------------------------------------------------------------
C FILE:                  File name options
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'FILE    ' ) THEN
C
        IF( CEXIST('DATA    ') ) THEN
          CALL FNAME( CARD, LENC, FNDAT, LENF, IERF )
          IF( IERF .NE. 0 ) THEN
            WRITE(IERR,7149) CARD
 7149       FORMAT(/
     *       ' *** Terminal error in subroutine INPUTS ***'/
     *       '     Bad data file name from keyword FILE'/
     *       '     Card = ',A)
            BADKEY = .TRUE.
          ENDIF
          GO TO 10
        ENDIF
C
        IF( CEXIST('REPORT  ') ) THEN
          CALL FNAME( CARD, LENC, FNRPT, LENF, IERF )
          IF( IERF .NE. 0 ) THEN
            WRITE(IERR,7148) CARD
 7148       FORMAT(/
     *       ' *** Terminal error in subroutine INPUTS ***'/
     *       '     Bad report file name from keyword FILE'/
     *       '     Card = ',A)
            BADKEY = .TRUE.
          ENDIF
          GO TO 10
        ENDIF
C
C ***   Shouldn't reach here
C
        WRITE(IERR,7147) CARD
 7147   FORMAT(/
     *   ' *** Terminal error in subroutine INPUTS ***'/
     *   '     Bad entry for keyword FILE'/
     *   '     Card = ',A)
        BADKEY = .TRUE.
        GO TO 10
C
      ENDIF
C
C ---------------------------------------------------------------------
C ITERATE:                Number of iterations
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'ITERATE ' ) THEN
C
        N = VALUE(1)
C
C ***   Error check on the number of iterations
C
        IF( N.LT.1 .OR. N.GT.MAXN ) THEN
          WRITE(IERR,8010) N, MAXN
 8010     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Illegal number of iterations on the ITERATE keyword'/
     *     '     The value entered was ',I5/
     *     '     The required limits are 1 to ',I5/
     *     '     Change the ITERATE keyword or the parameter MAXN')
          BADKEY = .TRUE.
        ENDIF
        GO TO 10
      ENDIF
C
C ---------------------------------------------------------------------
C LABEL:                Process variable labels
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'LABEL   ' ) THEN
C
        LINDEX = VALUE(2)
C
C ***   Error check on dimensions of the label array
C
        IF( LINDEX.LT.1 .OR. LINDEX.GT.MAXP ) THEN
          WRITE(IERR,8000) LINDEX, MAXP, CARD
 8000     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Execution stop due to an illegal index from the '/
     *     '     LABEL keyword.  Value= ',I5,' limits are 1 to ',I3/
     *     '     CARD = ',A)
          STOP 'LABEL-1'
        ENDIF
C
        CALL FNAME( CARD, LENC, TMPNAM, LENL, IERF )
C
C ***   Error check and processing on the entered label
C
        IF( IERF .EQ. 0 ) THEN
          VNAME(LINDEX) = TMPNAM
        ELSE
          WRITE(IERR,6045) CARD
 6045     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Bad variable label from the LABEL keyword.'/
     *     '     Card = ',A)
          BADKEY = .TRUE.
        ENDIF
C
        GO TO 10
C
      ENDIF
C
C ---------------------------------------------------------------------
C OUTPUT:     Options for output of correlation matrix and data
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'OUTPUT  ' ) THEN
C
        IF( CEXIST('DATA    ') ) THEN
          ODATA = .TRUE.
        ENDIF
C
        IF( CEXIST('CORRELAT') ) THEN
          OCORR = .TRUE.
        ENDIF
C
        GO TO 10
C
      ENDIF
C
C ---------------------------------------------------------------------
C SEED:               Random number generator seed
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'SEED    ' ) THEN
C
        DSEED = VALUE(1)
C
C ***   Error check on the seed
C
        IF( DSEED.LT.1.0D0 .OR. DSEED.GT.2147483646.0D0 ) THEN
          WRITE(IERR,8013) DSEED
 8013     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Illegal seed on the SEED keyword'/
     *     '     Value= ',1PE14.7,' limits are 1 to 2147483646')
          BADKEY = .TRUE.
        ENDIF
C
        GO TO 10
C
      ENDIF
C
C ---------------------------------------------------------------------
C TITLE:                    Problem title
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'TITLE   ' ) THEN
        CALL FNAME( CARD, LENC, TITLE, LENT, IERF )
        IF( IERF .NE. 0 ) THEN
          WRITE(IERR,7049) CARD
 7049     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Bad title from keyword TITLE'/
     *     '     Card = ',A)
          BADKEY = .TRUE.
        ENDIF
        GO TO 10
      ENDIF
C
C ---------------------------------------------------------------------
C UNIQUE                Process unique variable names
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'UNIQUE  ' ) THEN
C
        LINDEX = VALUE(2)
C
C ***   Error check on dimensions of the array
C
        IF( LINDEX.LT.1 .OR. LINDEX.GT.MAXP ) THEN
          WRITE(IERR,8002) LINDEX, MAXP, CARD
 8002     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Execution stop due to an illegal index from the '/
     *     '     UNIQUE keyword.  Value= ',I5,' limits are 1 to ',I3/
     *     '     CARD = ',A)
          STOP 'UNIQUE-1'
        ENDIF
C
        LEN5 = 5
        CALL FNAME( CARD, LENC, TMPNAM, LEN5, IERF )
C
C ***   Error check and processing on the entered label
C
        IF( IERF .EQ. 0 ) THEN
          UNIQUE(LINDEX) = TMPNAM
        ELSE
          WRITE(IERR,6745) CARD
 6745     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Bad variable name from the UNIQUE keyword.'/
     *     '     Card = ',A)
          BADKEY = .TRUE.
        ENDIF
C
        GO TO 10
C
      ENDIF
C
C ---------------------------------------------------------------------
C USER:                    New user name
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'USER    ' ) THEN
        CALL FNAME( CARD, LENC, TMPNAM, LENU, IERF )
        IF( IERF .EQ. 0 ) THEN
          USRNAM = TMPNAM
        ELSE
          WRITE(IERR,7045) CARD
 7045     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Bad user name from keyword USER'/
     *     '     Card = ',A)
          BADKEY = .TRUE.
        ENDIF
        GO TO 10
      ENDIF
C
C ---------------------------------------------------------------------
C VARIABLE:            Stochastic variable definition
C ---------------------------------------------------------------------
C
      IF( NAME .EQ. 'VARIABLE' ) THEN
C
        INDX = 2
        VINDEX = VALUE(INDX)
C
C ***   Error check on dimensions of the variable arrays
C
        IF( VINDEX.LT.1 .OR. VINDEX.GT.MAXP ) THEN
          WRITE(IERR,8001) VINDEX, MAXP, CARD
 8001     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Execution stop for an illegal variable number on the '/
     *     '     VARIABLE keyword. Value= ',I5,' limits are 1 to ',I3/
     *     '     CARD = ',A)
          STOP 'VARIAB-1'
        ENDIF
C
C ***   Determine if this variable is "constant" or "stochastic"
C
        INDX = INDX + 1
        ITYPE = VALUE(INDX)
        IF( ITYPE.LT.0 .OR. ITYPE.GT.MXDST2 ) THEN
          WRITE(IERR,8011) ITYPE, MXDST2, CARD
 8011     FORMAT(/
     *    ' *** Terminal error in subroutine INPUTS ***'/
     *    '     Execution stop for an illegal distribution index on'/
     *    '     the VARIABLE keyword. Value= ',I5,' limits are 0 to ',
     *    I2,/
     *    '     CARD = ',A)
          STOP 'VARIAB-2'
        ENDIF
C
C ***   Increment variable counters
C
        PTOT = PTOT + 1
        IF( PTOT .GT. MAXPTL ) THEN
          WRITE(IERR,7302) MAXPTL, CARD
 7302     FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Execution stop for too many VARIABLE keywords.'/
     *     '     A maximum of ',I3,' variables may be used.'/
     *     '     Change the parameter MAXPTL and recompile.'/
     *     '     CARD = ',A)
          STOP 'VARIAB-3'
        ENDIF
        IF( ITYPE .EQ. 0 ) THEN
          PCON = PCON + 1
        ENDIF
        IF( ITYPE.GE.1 .AND. ITYPE.LE.MXDST2 ) THEN
          P = P + 1
          IF( P .GT. MAXP ) THEN
            WRITE(IERR,7301) MAXP, CARD
 7301       FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     Execution stop for too many VARIABLE keywords.'/
     *     '     A maximum of ',I3,' stochastic variables may be used.'/
     *     '     Change the parameter MAXP (and MAXPTL) and recompile.'/
     *     '     CARD = ',A)
            STOP 'VARIAB-2'
          ENDIF
C
C ***     Save the mapping vector of indices
C         It goes from the set of P stochastic to the set of PTOT
C
          MAP(P) = PTOT
C
        ENDIF
C
C ***   Store the variable type
C
        VTYPE(VINDEX) = ITYPE
C
C ***   Process the "constant" variables
C
        IF( ITYPE .EQ. 0 ) THEN
          INDX = INDX + 1
          VPARMS(VINDEX,1) = VALUE(INDX)
          VPARMS(VINDEX,2) = 0.0
          VPARMS(VINDEX,3) = 0.0
          VPARMS(VINDEX,4) = 0.0
          GO TO 10
        ENDIF
C
C ***   Action for the truncation flag on the variable distribution
C
        INDX = INDX + 1
        VTRUN(VINDEX) = VALUE(INDX)
        IF( VTRUN(VINDEX) .EQ. 1 ) THEN
          INDX = INDX + 1
          VTLIM(VINDEX,1) = VALUE(INDX)
        ENDIF
        IF( VTRUN(VINDEX) .EQ. 2 ) THEN
          INDX = INDX + 1
          VTLIM(VINDEX,2) = VALUE(INDX)
        ENDIF
        IF( VTRUN(VINDEX) .EQ. 3 ) THEN
          INDX = INDX + 1
          VTLIM(VINDEX,1) = VALUE(INDX)
          INDX = INDX + 1
          VTLIM(VINDEX,2) = VALUE(INDX)
        ENDIF
C
C ***   Pick off the parameters for the "classical" distribution types
C       1 through 13
C       They contain up to 4 parameters for the distribution
C
        IF( ITYPE.GE.1 .AND. ITYPE.LE.13 ) THEN
          INDX = INDX + 1
          VPARMS(VINDEX,1) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,2) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,3) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,4) = VALUE(INDX)
          GO TO 10
        ENDIF
C
C ***   Process the "stochastic" variables for type 14
C
        IF( ITYPE .EQ. 14 ) THEN
          INDX = INDX + 1
          NPAIRS = VALUE(INDX)
          PTABLE(VINDEX,1) = IDXTBL
          PTABLE(VINDEX,2) = NPAIRS
          ITMP = IDXTBL + NPAIRS - 1
          IF( ITMP .GT. MAXTBL ) THEN
            WRITE(IERR,7232) VINDEX, ITMP, MAXTBL
 7232       FORMAT(/
     *       ' *** Terminal error in Subroutine INPUTS ***'/
     *       '     The input table for variable ',I4/
     *       '     Would put storage requirements at ',I4,' locations'/
     *       '     Only ',I4,' locations are available'/
     *       '     Change the input table or the parameter MAXTBL'/
     *       '     The parameter change requires a new executable')
            STOP 'INPUTS-T'
          ENDIF
          DO 110 I = 1, NPAIRS
            INDX = INDX + 1
            XTABLE(IDXTBL) = VALUE(INDX)
            INDX = INDX + 1
            FTABLE(IDXTBL) = VALUE(INDX)
            IDXTBL = IDXTBL + 1
  110     CONTINUE
          GO TO 10
        ENDIF
C
C ***   Process the "stochastic" variables for type 15
C       They contain up to 4 parameters for the distribution
C
        IF( ITYPE.GE.15 ) THEN
          INDX = INDX + 1
          VPARMS(VINDEX,1) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,2) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,3) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,4) = VALUE(INDX)
          GO TO 10
        ENDIF
C
        GO TO 10
C
      ENDIF
C
C ---------------------------------------------------------------------
C            *** Error message for undefined keyword ***
C ---------------------------------------------------------------------
C
      WRITE(IERR,7200) NAME
 7200 FORMAT(/
     *     ' *** Terminal error in subroutine INPUTS ***'/
     *     '     The keyword (',A8,') is not defined.')
      BADKEY = .TRUE.
C
C *** Go try for another card
C
      GO TO 10
C
   20 CONTINUE
C
C *** Stop for bad keywords
C
      IF( BADKEY ) THEN
        CERR(1)='One or more errors were encountered in the keyword'
        CERR(2)='data for this sensitivity case.  The input file name'
        CERR(3)='is: ' // FNKEY
        CERR(4)='The keyword data must be modified before continuing.'
        CERR(5)='Error messages are written to a file.  The file name'
        CERR(6)='is: ' // FNERR
        NERR = 6
        CALL GDERR( NERR, 'INPUTS' )
        CALL STWO
        STOP
      ENDIF
C
C *** Normal termination for the input routine
C
      RETURN
      END
