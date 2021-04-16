      SUBROUTINE DCDSEP( NUM, LOC )
C*....SUBROUTINE DCDSEP( NUM, LOC )
C***********************************************************************
C*
C*  Purpose:
C*
C*   This subroutine examines data in the input line (INFO) and
C*   transfers character data into the array CVALUE, and numeric data
C*   into the array VALUE.  The vector LOC contains the field
C*   separators in the input line.  NUM indicates the number of
C*   separators present.
C*
C*    This routine is a part of the set of routines called by RDBLK.
C*
C*
C*  Notes:
C*
C*     1. Real numbers may be written in exponential format, but only
C*        an 'E' or an 'e' are recognized exponent indicators.  The
C*        double precision 'D' exponent indicator is not valid.
C*
C*     2. A '*' character interior to a line is taken to indicate a
C*        repetition of the following value or character field.  It
C*        must be preceded by an unsigned integer value.
C*
C*
C*  History:
C*
C*    David W. Langford : 01 Jul 1992 : Version 1.0
C*
C***********************************************************************
C
      INCLUDE 'CDATA.ins'
      INCLUDE 'FILES.ins'
C
      CHARACTER*1 CH, DUMY(LENCRD)
      DIMENSION LOC(LENCRD)
      LOGICAL DECSET, EXPSET, MINUS
C
C             LOOP ON THE NUMBER OF SEPARATORS, MINUS ONE.
C             INITIALIZE CHARACTER COUNT BETWEEN ADJACENT SEPARATORS.
C
      DO 210 I=1,(NUM-1)
         J = I+1
         ISTART = LOC(I)+1
         NCHARS = LOC(J)-ISTART
         IF (NCHARS.LT.1) GO TO 210
C
C             INITIALIZATIONS FOR DECODING LOOP
C
         DECSET = .FALSE.
         EXPSET = .FALSE.
         IEND   = LOC(J)-1
         ISETNO = 0
         MULT   =-1
         NDIGIT = 0
         RDATA  = 0.0
C
         IS   = LOC(I)
         IEND = LOC(J)-1
C
C             CHECK FOR SIGN OF NUMERIC RESULT
C
  100    CONTINUE
         MINUS = .FALSE.
         IF (INFO(IS+1).EQ.'+') THEN
            IS = IS+1
            IF (IS.GE.IEND) GO TO 180
         ENDIF
         IF (INFO(IS+1).EQ.'-') THEN
            MINUS = .TRUE.
            IS    = IS+1
            IF (IS.GE.IEND) GO TO 180
         ENDIF
C
C             PRIMARY LOOP DECODES POSITIVE INTEGER VALUES
C
  110    CONTINUE
         ISETNO = ISETNO+1
         NUMB   = 0
         NDIGIT = 0
  120    CONTINUE
         IS = IS+1
         CH = INFO(IS)
         IF ((CH.GE.'0').AND.(CH.LE.'9')) THEN
            READ (CH,500) J
            NUMB   = NUMB*10+J
            NDIGIT = NDIGIT+1
            IF (IS.GE.IEND) THEN
               IF (MINUS) NUMB = -NUMB
               GO TO 140
            ENDIF
            GO TO 120
         ENDIF
         IF (MINUS) NUMB = -NUMB
C
C             CHECK FOR REPETITION FACTOR
C
         IF (CH.EQ.'*') THEN
            IF ((MULT.GT.0) .OR.(NDIGIT.LT.1)) GO TO 180
            IF ((IS.GE.IEND).OR.(ISETNO.GT.1)) GO TO 180
            IF (MINUS) GO TO 180
            MULT   = NUMB
            ISTART = IS+1
            NCHARS = LOC(I+1)-ISTART
            GO TO 100
         ENDIF
C
C               CHECK FOR DECIMAL POINT.  NOTE THAT 'MINUS' IS SAVED.
C
         IF (CH.EQ.'.') THEN
            IF (DECSET.OR.EXPSET) GO TO 180
            DECSET = .TRUE.
            IF (NDIGIT.GT.0) RDATA = FLOAT(NUMB)
            IF (IS.GE.IEND) GO TO 160
            GO TO 110
         ENDIF
C
C              CHECK FOR EXPONENT MARKER
C
         IF ((CH.EQ.'E').OR.(CH.EQ.'e')) THEN
            IF (EXPSET) GO TO 180
            IF ((ISETNO.LT.2).AND.(NDIGIT.LT.1)) GO TO 180
            EXPSET = .TRUE.
            IF (NDIGIT.GT.0) THEN
               D = FLOAT(NUMB)
               IF (DECSET) THEN
                  DO 130 J=1,NDIGIT
                     D = 0.1*D
  130             CONTINUE
               ENDIF
               RDATA = RDATA+D
            ENDIF
            IF (IS.GE.IEND) GO TO 160
            GO TO 100
         ENDIF
C
C             CANNOT DECIPHER STRING AS A NUMBER
C
         GO TO 180
C
C             FINISHED DECODING NUMERIC VALUE
C
  140    CONTINUE
         IF (EXPSET) THEN
            IF (NUMB.NE.0) RDATA = RDATA*(10.0**NUMB)
         ELSEIF (DECSET) THEN
            D = FLOAT(NUMB)
            DO 150 J=1,NDIGIT
               D = 0.1*D
  150       CONTINUE
            RDATA = RDATA+D
         ELSE
            RDATA = FLOAT(NUMB)
         ENDIF
C
C             FILL IN NUMERIC DATA.  ADD MULTIPLE VALUES, IF REQUESTED.
C
  160    CONTINUE
         NVALUE = NVALUE+1
         IF (NVALUE.GT.NVALS) GO TO 470
         VALUE(NVALUE) = RDATA
C
         IF (MULT.GT.1) THEN
            IF ((NVALUE+MULT-1).GT.NVALS) GO TO 470
            DO 170 J=2,MULT
               NVALUE = NVALUE+1
               VALUE(NVALUE) = RDATA
  170       CONTINUE
         ENDIF
         GO TO 210
C
C ----------------------------------------------------------------------
C
C             TRANSFER AS A CHARACTER VARIABLE.
C
  180    CONTINUE
         NCVALU = NCVALU+1
         IF (NCVALU.GT.NKEYS) GO TO 480
         K=0
         DO 190 J=ISTART,IEND
            K=K+1
            DUMY(K) = INFO(J)
  190    CONTINUE
         DUMY(K+1) = ' '
         CALL XFKEY (DUMY,CVALUE(NCVALU))
C
C             FILL IN MULTIPLE CHARACTER VALUES, IF REQUESTED.
C
         IF (MULT.GT.1) THEN
            IF ((NCVALU+MULT-1).GT.NKEYS) GO TO 480
            K = NCVALU
            DO 200 J=2,MULT
               NCVALU = NCVALU+1
               CVALUE(NCVALU) = CVALUE(K)
  200       CONTINUE
         ENDIF
 
  210 CONTINUE
C
      RETURN
C
C             ERRORS
C
  470 CONTINUE
      WRITE(IRPT,970) NVALS
      GO TO 490
  480 CONTINUE
      WRITE(IRPT,980) NKEYS
  490 CONTINUE
      WRITE(IRPT,990) ILINE
      STOP
C
  500 FORMAT(I1)
  510 FORMAT(120A1)
  970 FORMAT(/
     * ' *** Terminal error in subroutine DCDSEP ***'/
     * '     Error in the input data deck.  Too many numeric values'/
     * '     were found within a single data block.  Limit is ',I5,'.')
C
  980 FORMAT(/
     * ' *** Terminal error in subroutine DCDSEP ***'/
     * '     Error in the input data deck.  Too many character values'/
     * '     were found within a single data block.  Limit is ',I5,'.')
C
  990 FORMAT(/
     * ' *** Terminal error in subroutine DCDSEP ***'/
     * '     Error at line ',I5,' of the input data file.'/
     * '     Error stop.')
      END
