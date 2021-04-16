C-- ACUTE: OPNFIL.FOR ------------------ Version: 29-Jul-97 -----------------
C
C     SUBROUTINE OPNFIL (IMODE, IHEAD, LUN)
C
C     This module controls opening of input and output files.  This 
C     module may also read the file containing filenames and optionally
C     writes titling information to intermediate output files.
C
C     General Module of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Initial GENII version last modification: 2-Dec-88  RAP
C     Reviewed and Approved: 12-Sept-88
C     Current Version: 29-Jul-97  DLS
C-----------------------------------------------------------------------
C Paremeter descriptions
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
C----- Modification Histore---------------------------------------------------
C   Date      Who    Description of Modification
C  ---------  ---  -----------------------------------------------------------
C  29-Jul-97  DLS  Heading info and comments expanded for GENII/ACUTE component
C  5-JAN 98   BAN  Disabled IMODE=2 capability to remove data call
C
C-----------------------------------------------------------------------------
C
      SUBROUTINE OPNFIL (IMODE, IHEAD, LUN)
C
C----- Include Statements ----------------------------------------------------
C
      INCLUDE 'FILES.CMN'
C      INCLUDE 'DAYPC.CMN'
      INCLUDE 'TITL.CMN'
      INCLUDE 'DEVICE.CMN'
C
C----- Type statements -------------------------------------------------------
C
      LOGICAL LEXIST      
      INTEGER IMODE, IHEAD, IOS, LUN
      CHARACTER FF*1
C
C----- Start of Analysis -----------------------------------------------------
C
      IOS = 0
      FF = CHAR(12)
C
C----- For IMODE = 3, read FILENAME.DAT to get file names for this run -------
C
      IF (IMODE .EQ. 3) LUN = 1
      IF (LUN .LT. 1 .OR. LUN .GT. 50) GOTO 91
C
      IF (IMODE .EQ. 1) THEN
C
C-----  Open existing file ---------------------------------------------------
C
        OPEN (LUN, FILE=FILN(LUN), STATUS='OLD', IOSTAT=IOS)
C
      ELSEIF (IMODE .EQ. 2) THEN
C
C-----  Open new file --------------------------------------------------------
C
        INQUIRE (FILE=FILN(LUN), EXIST=LEXIST)
        IF (LEXIST) THEN
          OPEN (LUN, FILE=FILN(LUN), STATUS='OLD', IOSTAT=IOS)
        ELSE
          OPEN (LUN, FILE=FILN(LUN), STATUS='NEW', IOSTAT=IOS)
        ENDIF
C
      ELSEIF (IMODE .EQ. 3) THEN
C
C-----  Open and read filename file, check first in default directory --------
C
        INQUIRE (FILE=FILN(1), EXIST=LEXIST, IOSTAT=IOS)
        IF (LEXIST) THEN
          OPEN (1, FILE=FILN(1), STATUS='OLD', IOSTAT=IOS)
        ELSE
          OPEN (1, FILE='FILENAME.DAT', STATUS='OLD', IOSTAT=IOS)
        ENDIF
C
C---- Read file names into array FILN ----------------------------------------
C
        DO 100 I = 2, 50
          READ (1,1000,ERR=98,END=101) FILN(I)
  100   CONTINUE
  101   CONTINUE

      ELSEIF (IMODE .EQ. 4) THEN
C
C-----  Open existing file, read and store title -----------------------------
C
        OPEN (LUN, FILE=FILN(LUN), STATUS='OLD', IOSTAT=IOS)
        IF (IOS .NE. 0) GOTO 97
        READ (LUN,'(A80)',ERR=99, END=98) TITLS(LUN)
C
C-----  Check if this is a DOSE, INTDF, or EXTDF call, 
C       store title for reports--
C
        IF (LUN .EQ. 11) THEN
          TITLS(5) = TITLS(LUN)
        ELSEIF (LUN .EQ. 18) THEN
          TITLS(5) = TITLS(LUN)
        ELSEIF (LUN .EQ. 35) THEN
          TITLS(5) = TITLS(LUN)
        ENDIF
C
      ELSEIF (IMODE .EQ. 5) THEN
C
C-----  Open existing file for append ----------------------------------------
C
        INQUIRE (FILE=FILN(LUN), EXIST=LEXIST)
        IF (LEXIST) THEN
          OPEN (LUN, FILE=FILN(LUN), STATUS='OLD', ACCESS='APPEND',
     .          IOSTAT=IOS)
        ELSE
          OPEN (LUN, FILE=FILN(LUN), STATUS='NEW', IOSTAT=IOS)
        ENDIF
      ELSEIF (IMODE .EQ. 6) THEN
C
C-----  Open an existing unformatted file ------------------------------------
C
        OPEN (LUN, FILE=FILN(LUN), STATUS='OLD', FORM='UNFORMATTED',
     .        IOSTAT=IOS)
      ELSEIF (IMODE .EQ. 7) THEN
C
C-----  Open an unformatted file, read and store title -----------------------
C
        OPEN (LUN, FILE=FILN(LUN), STATUS='OLD', FORM='UNFORMATTED',
     .        IOSTAT=IOS)
        READ (LUN) TITLS(LUN)
C
      ELSEIF (IMODE .EQ. 8) THEN
C
C-----  Open a scratch unformatted file --------------------------------------
C
        OPEN (LUN, STATUS='SCRATCH', FORM='UNFORMATTED',
     .        IOSTAT=IOS)
C
      ELSEIF (IMODE .EQ. 9) THEN
        IF (IHEAD .EQ. 9) WRITE (LUN,'(A)') FF
        CLOSE (LUN) 
      ELSE
        GO TO 92
      ENDIF  
C
      IF (IOS .NE. 0) GOTO 97
C     
C      IF (IMODE .EQ. 2) THEN 
C        IF (IHEAD .EQ. 1) THEN      
C          WRITE (LUN,'(A79)') TITLS(11)
C          WRITE (LUN,'(5A)') 'Created on ',TODAY,' at ',CLOCK
C        ELSEIF (IHEAD .EQ. 2) THEN      
C          WRITE (LUN,'(A79)') TITLS(11)
C          WRITE (LUN,'(5A)') 'Created on ',TODAY,' at ',CLOCK
C        ELSEIF (IHEAD .EQ. 3) THEN      
C          WRITE (LUN,'(A79)') TITLS(5)
C          WRITE (LUN,'(5A)') 'Created on ',TODAY,' at ',CLOCK
C        ELSEIF (IHEAD .EQ. 4) THEN
C          TITLS(5) = TITLS(18)
C        ENDIF
C      ENDIF
C
C----- Normal return ---------------------------------------------------------
C
      RETURN
C
C---- Format Statements ------------------------------------------------------
C
 1000 FORMAT (10X,A30)
C
C---- Error Messages ---------------------------------------------------------
C     Bad value supplied to the subroutine for Logical Unit (LUN)
C
   91 WRITE (NERR,*) 'OPNFIL:  Invalid value for LUN: ',LUN
      NERROR = NERROR + 1
      RETURN
C
C----- Bad value for IMODE ---------------------------------------------------
C
   92 WRITE (NERR,*) 'OPNFIL:  Invalid value for IMODE: ', IMODE
      NERROR = NERROR + 1
      RETURN
C
C----- Error opening existing file (IOS not 0) -------------------------------
C
   97 WRITE (NERR,*) 'IOS:',IOS
      CALL FILERR (1, LUN, 'In OPNFIL           ')
      RETURN
C
C----- Error reading from FILENAME.DAT or a title line in another file -------
C
   98 CALL FILERR (2, LUN, 'Title read in OPNFIL')
      RETURN
C
C----- Error reading a title line for IMODE = 4 ------------------------------
C
   99 CALL FILERR (3, LUN, 'Title read in OPNFIL')
      RETURN
      END
C
C-----------------------------------------------------------------------------
C

