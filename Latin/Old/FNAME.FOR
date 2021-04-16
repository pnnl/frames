      SUBROUTINE FNAME( CARD, LENC, FILE, LENF, IERF )
C*....SUBROUTINE FNAME( CARD, LENC, FILE, LENF, IERF )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine extracts a file name from the information
C*    contained in the character vector CARD.  The file name must
C*    be enclosed in double apostrophes and cannot contain more than
C*    LENF characters.  This subroutine only reads CARD until two
C*    apostrophes are found.  Any characters after the second
C*    apostrophe are ignored.
C*
C*    This routine operates with the set of routines called by RDBLK.
C*
C*
C*  Call List Variables:
C*
C*    CARD : Input (character) vector from which to extract the file
C*           name.  CARD is considered as a CHARACTER*1 vector of
C*           length LENC.
C*    LENC : Input (integer) giving the maximum number of characters
C*           in CARD.
C*    FILE : Output (character) array containing the file name which
C*           was extracted from CARD.  FILE is considered as a
C*           CHARACTER*1 vector of length LENF.  FILE is blank filled
C*           before any file name extraction is performed.
C*    LENF : Input (integer) giving the maximum number of characters
C*           in the file name FILE.
C*    IERF : Output (integer) error flag.
C*             0 = Normal termination
C*             1 = There were not two apostrophes in CARD.  A blank
C*                 file name will be returned.
C*             2 = Two adjacent apostrophes were located.  A blank
C*                 file name will be returned.
C*             3 = The file name would contain more than LENF
C*                 characters.  A blank file name is returned.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Original Source
C*
C***********************************************************************
C
      INTEGER LENC, LENF, IERF
      CHARACTER*1 CARD(*), FILE(*), APOS, BLANK
C
      DATA APOS /'"'/, BLANK /' '/
C
C *** Set the error flag and blank fill the file name
C
      IERF = 0
      DO 10 I = 1, LENF
        FILE(I) = BLANK
   10 CONTINUE
C
C *** Locate the apostrophes bracketing the file name
C
      I1 = 0
      I2 = 0
      DO 20 I = 1, LENC
        IF( CARD(I) .EQ. APOS ) THEN
          IF( I1 .EQ. 0 ) THEN
            I1 = I
          ELSE
            I2 = I
            GO TO 30
          ENDIF
        ENDIF
   20 CONTINUE
C
C *** Error checking
C
   30 CONTINUE
C
      IF( I1.EQ.0 .OR. I2.EQ.0 ) THEN
        IERF = 1
        RETURN
      ENDIF
C
      LENN = I2 - I1 - 1
      IF( LENN .LE. 0 ) THEN
        IERF = 2
        RETURN
      ENDIF
C
      IF( LENN .GT. LENF ) THEN
        IERF = 3
        RETURN
      ENDIF
C
C *** Transfer the file name
C
      DO 40 I = 1, LENN
        FILE(I) = CARD(I1+I)
   40 CONTINUE
C
      RETURN
      END
