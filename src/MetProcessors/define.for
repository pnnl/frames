      SUBROUTINE DEFINE
C***********************************************************************
C                 DEFINE Module of ISC2 Model
C
C        PURPOSE: Defines Location of Fields on Runstream Input Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:       March 2, 1992
C
C        MODIFIED:   Jayant Hardikar - 2/9/95 : Made it generic
C
C        INPUTS:     One line of variables separated by spaces
C
C        OUTPUTS:    Number of Fields on Card, IFC
C                    Beginning and Ending Columns of Fields, LOCB and LOCE
C
C        CALLED FROM: PARSER
C***********************************************************************
C
C     Variable Declarations
      PARAMETER (IFMAX = 27)
      PARAMETER (ISTRG = 256)
      CHARACTER*1 RUNST
      CHARACTER*40 FIELD
      LOGICAL INFLD      
      COMMON /FIELDS/ LOCB(IFMAX), LOCE(IFMAX), IFC
      COMMON /FLDCHR/ FIELD(IFMAX)
      COMMON /LOGIN1/ INFLD
      COMMON /RUNSTR/ RUNST(ISTRG)

C     Initialize the Blank Line and In-field Status Indicators
      INFLD = .FALSE.

C     Define the Starting Column for the Input Line In Case File Is Shifted.
C     Allow for Shift of Up to 3 Columns

      LOCB(1) = 0
      IF (RUNST(1) .NE. ' ') THEN
         LOCB(1) = 1
      ELSE IF (RUNST(2) .NE. ' ') THEN
         LOCB(1) = 2
      ELSE IF (RUNST(3) .NE. ' ') THEN
         LOCB(1) = 3
      ELSE IF (RUNST(4) .NE. ' ') THEN
         LOCB(1) = 4
      ELSE
         LOCB(1) = 1
      END IF

      IFC = 0

C     Loop through the Data Fields
      DO 20 I = LOCB(1), ISTRG

         IF (.NOT.INFLD .AND. RUNST(I).NE.' ') THEN
C           Location is the Beginning of a Field
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
C           Record the Location of Beginning of the Field
            LOCB(IFC) = I
         ELSE IF (INFLD .AND. RUNST(I).EQ.' ') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Record the Location of Ending of the Field
            LOCE(IFC) = I - 1
         END IF

C        Check for End of Input String
         IF (INFLD .AND. I.EQ.ISTRG) THEN
            LOCE(IFC) = ISTRG
         END IF

 20   CONTINUE

      RETURN
      END
