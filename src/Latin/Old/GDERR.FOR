      SUBROUTINE GDERR( NERR, CALLER )
C*....SUBROUTINE GDERR( NERR, CALLER )
C***********************************************************************
C*
C*  Purpose:
C*
C*    This subroutine handles writing a generalized error message
C*    to the screen.  The error message is placed in the middle of
C*    the screen.  The error message may contain up to 22 lines.
C*    The screen image is captured on entry to this routine and is
C*    restored upon exit from the routine.  The title of the error
C*    message window uses the variable CALLER to identify the
C*    routine that detected the error.
C*
C*
C*  History:
C*
C*    Paul W. Eslinger : 01 Jul 1992 : Version 1.0
C*
C***********************************************************************
C
      INCLUDE 'SPINATTR.ins'
      INCLUDE 'SCREENS.ins'
C
      CHARACTER CALLER*(*)
C
C *** Error check on the number of lines
C
      IF( NERR .GT. 22 ) NERR = 22
      IF( NERR .LT.  1 ) RETURN
C
C *** Find the maximum length of any line in the message
C
      LMAX = 0
      DO 10 I = 1, NERR
        CALL LENGTH( CERR(I), LERR, LEN )
        LMAX = MAX(LEN,LMAX)
   10 CONTINUE
C
      IF( LMAX .EQ. 0 ) RETURN
C
C *** Capture the screen image
C
      CALL CAPTSCN( IMAGE )
C
C *** Open a window to display the message in the middle of the screen
C
      IR1 = 12 - NERR/2 - 1
      IR2 = IR1 + NERR + 1
      CTITLE = 'Error Message From ' // CALLER
      L78 = 78
      CALL LENGTH( CTITLE, L78, LENTIT )
      IF( LMAX .LT. LENTIT ) LMAX = LENTIT
C
      IC1 = 40 - LMAX/2 - 1
      IC2 = IC1 + LMAX + 1
C
      IENUM = 9999
      CALL DEFWIND(IENUM, IR1, IC1, IR2, IC2, DBLBDR, ICOPT)
      CALL TITLE(IENUM,CTITLE(1:LENTIT), TOP, LJUST, ICTIT )
      CALL CLRB(IENUM)
C
C *** Display the message
C
      IC3 = 0
      IR3 = 0
      DO 20 I = 1, NERR
        CALL LENGTH( CERR(I), L78, LEN )
        CALL BLDB(IENUM, IR3, IC3, CERR(I)(1:LEN), (WHTCHR+BRITE) )
        IR3 = IR3 + 1
   20 CONTINUE
C
      CALL CUROFF
      CALL ANYKEY
      CALL CURON
C
C *** Redisplay the screen image
C
      CALL DISPSCN( IMAGE )
C
      RETURN
      END
