C----------------------------------------------------------------------------
C
      SUBROUTINE OPNFIL (IMODE, IHEAD, LUN)
C
C     This module controls opening of input and output files.  This 
C     module may also read the file containing filenames and optionally
C     writes titling information to intermediate output files.
C
C     General Module of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 4-JAN-99  BAN
C     Reviewed and Approved: 12-Sept-88
C
C-----------------------------------------------------------------------
C
C     IMODE     - Index to specify: 
C                   1 - open an existing file, 
C                   2 - open a new file,
C                   3 - open and read the filename file
C                   4 - open existing file, read and store title
C                   5 - open existing file for append
C                   6 - open an existing unformatted file
C                   7 - open an unformatted file, read and store title
C                   8 - open a new unformatted file
C                   9 - close file
C     IHEAD     - Index to specify heading type:  
C                   0 - no heading,
C                   1 - ENV: output file (includes options)
C                   2 - ENV: Title plus date and time on second line
C                   3 - ENVIN: Title plus date and time on second line  
C                   4 - INTDF: Copy title to TITLS(5)
C                   9 - form feed only
C     ILUN      - Logical unit number of the current file
C     IOS       - Status flag returned by system call to OPEN
C     LEXIST    - Logical set if file exists
C
C-----------------------------------------------------------------------
C  INITIAL INSTALLATION 29 OCT 2009   BAN
C-----------------------------------------------------------------------

      INCLUDE 'FILES.CMN'
      INCLUDE 'TITL.CMN'
      INCLUDE 'DEVICE.CMN'

      LOGICAL LEXIST      
      INTEGER IMODE, IHEAD, LUN, IOS, LUN1
      CHARACTER FF*1
C  for whatever reason, LUN was a sort of uninitialized pointer, so replaced below with LUN1
C  BAN 23 Aug 2012 upon change to INTEL compiler      
      LUN1 = LUN
      IOS = 0
      FF = CHAR(12)

      IF (IMODE .EQ. 3) LUN1 = 1
      IF (LUN1 .LT. 1 .OR. LUN1 .GT. 50) GOTO 91

      IF (IMODE .EQ. 1) THEN
C       Open existing file--

        OPEN (LUN1, FILE=FILN(LUN1), STATUS='OLD', IOSTAT=IOS)

      ELSEIF (IMODE .EQ. 2) THEN
C       Open new file--

        INQUIRE (FILE=FILN(LUN1), EXIST=LEXIST)
        IF (LEXIST) THEN
          OPEN (LUN1, FILE=FILN(LUN1), STATUS='OLD', IOSTAT=IOS)
        ELSE
          OPEN (LUN1, FILE=FILN(LUN1), STATUS='NEW', IOSTAT=IOS)
        ENDIF

      ELSEIF (IMODE .EQ. 3) THEN

C       Open and read filename file, check first in default directory--

        INQUIRE (FILE=FILN(1), EXIST=LEXIST, IOSTAT=IOS)

        IF (LEXIST) THEN
          OPEN (1, FILE=FILN(1), STATUS='OLD', IOSTAT=IOS)
        ELSE
          OPEN (1, FILE='FILENAME.DAT', STATUS='OLD', IOSTAT=IOS)
        ENDIF

        DO 100 I = 2, 50
          READ (1,1000,ERR=98,END=101) FILN(I)
  100   CONTINUE
  101   CONTINUE

      ELSEIF (IMODE .EQ. 4) THEN
C       Open existing file, read and store title--

        OPEN (LUN1, FILE=FILN(LUN1), STATUS='OLD', IOSTAT=IOS)
        IF (IOS .NE. 0) GOTO 97

        READ (LUN1,'(A80)',ERR=99, END=98) TITLS(LUN1)


      ELSEIF (IMODE .EQ. 5) THEN
C       Open existing file for append--

        INQUIRE (FILE=FILN(LUN1), EXIST=LEXIST)
        IF (LEXIST) THEN
          OPEN (LUN1, FILE=FILN(LUN1), STATUS='OLD', ACCESS='APPEND',
     .          IOSTAT=IOS)
        ELSE
          OPEN (LUN1, FILE=FILN(LUN1), STATUS='NEW', IOSTAT=IOS)
        ENDIF

      ELSEIF (IMODE .EQ. 6) THEN
C       Open an existing unformatted file--

        OPEN (LUN1, FILE=FILN(LUN1), STATUS='OLD', FORM='UNFORMATTED',
     .        IOSTAT=IOS)

      ELSEIF (IMODE .EQ. 7) THEN
C       Open an unformatted file, read and store title--

        OPEN (LUN1, FILE=FILN(LUN1), STATUS='OLD', FORM='UNFORMATTED',
     .        IOSTAT=IOS)
        READ (LUN1) TITLS(LUN1)

      ELSEIF (IMODE .EQ. 8) THEN
C       Open a scratch unformatted file--

        OPEN (LUN1, STATUS='SCRATCH', FORM='UNFORMATTED',
     .        IOSTAT=IOS)

      ELSEIF (IMODE .EQ. 9) THEN
        IF (IHEAD .EQ. 9) WRITE (LUN1,'(A)') FF
        CLOSE (LUN1) 
      ELSE
        GO TO 92
      ENDIF  

      IF (IOS .NE. 0) GOTO 97
      

      RETURN

C---- Format Statements ------------------------------------------------

 1000 FORMAT (10X,A30)
 

C---- Error Messages ---------------------------------------------------

   91 WRITE (NERR,*) 'OPNFIL:  Invalid value for LUN: ',LUN1
      NERROR = NERROR + 1
      RETURN
C
   92 WRITE (NERR,*) 'OPNFIL:  Invalid value for IMODE: ', IMODE
      NERROR = NERROR + 1
      RETURN
C
   97 WRITE (NERR,*) 'IOS:',IOS
      CALL FILERR (1, LUN1, 'In OPNFIL           ')
      RETURN
   98 CALL FILERR (2, LUN1, 'Title read in OPNFIL')
      RETURN
   99 CALL FILERR (3, LUN1, 'Title read in OPNFIL')
      RETURN
C
C-----------------------------------------------------------------------
      END

