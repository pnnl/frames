C      AGE ALGORITHM INDEX
C        BA NAPIER
C        24 OCTOBER 1998
C
      SUBROUTINE AGEidx(TA1, TA2, INDXAG, INDX2)
	real ta1, ta2
     integer indxag
	DATA ONE/1.0/, FIVE/5.0/,TEN/10.0/, FIFTEEN/15.0/
	DATA TWENTY/20.0/, TWENTY5/25.0/, FIFTY/50.0/
	WRITE (*,'(2F5.0)') TA1,TA2
C
C---- SIMPLE CASES---------------------------------------------
      IF (TA2 .LE. ONE) THEN
        INDXAG=1
	  INDX2=1
        GO TO 88
      ELSE IF ((TA2 .LE. FIVE) .AND. (TA1 .GE. ONE)) THEN
        INDXAG = 2
	  INDX2=1
        GO TO 88
      ELSE IF ((TA2 .LE. TEN) .AND. (TA1 .GE. FIVE)) THEN
        INDXAG = 3
	  INDX2=2
        GO TO 88
      ELSE IF ((TA2 .LE. FIFTEEN) .AND. (TA1 .GE. TEN)) THEN
        INDXAG = 4
	  INDX2=2
        GO TO 88
      ELSE IF ((TA2 .LE. TWENTY) .AND. (TA1 .GE. FIFTEEN)) THEN
        INDXAG = 5
	  INDX2=3
        GO TO 88
      ELSE IF ((TA2 .LE. TWENTY5) .AND. (TA1 .GE. TWENTY)) THEN
        INDXAG = 6
	  INDX2=3
        GO TO 88
	ELSE IF ((TA2 .GT. TWENTY5) .AND. (TA1 .GE. TWENTY5)) THEN
	  INDXAG = 6
	  INDX2 = 4
      END IF  ! SIMPLE CASES
C
C-----HARDER CASES-----------------------------
C
      IF (TA2 .LE. FIVE) THEN
        INDXAG = 2
	  INDX2 = 1
        GO TO 88
      ELSE IF (TA2 .LE. TEN) THEN
        INDXAG = 3
	  INDX2 = 2
        GO TO 88
      ELSE IF (TA2 .LE. FIFTEEN) THEN
        INDXAG = 4
	  INDX2 = 2
        GO TO 88
      ELSE IF (TA2 .LE. TWENTY) THEN
        INDXAG = 5
	  INDX2 = 3
        GO TO 88
      ELSE IF (TA2 .LE. TWENTY5) THEN
	  INDXAG = 6
	  INDX2 = 3
	ELSE
	  INDXAG = 6
	  INDX2 = 4
      END IF
   88 CONTINUE
      IF ((TA2 .GT. FIFTY) .AND. (TA1 .LE. FIVE)) THEN
	  INDX2 = 5
	END IF
      RETURN
      END
