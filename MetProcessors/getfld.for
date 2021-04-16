      SUBROUTINE GETFLD
C***********************************************************************
C                 GETFLD Module of ISC2 Model
C
C        PURPOSE: Gets Contents of Fields on Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C*       MODIFIED:   Jayant Hardikar - 2/9/95 : Made it generic
C
C        INPUTS:     Fields on a line
C
C        OUTPUTS: Contents of Fields on Card
C
C        CALLED FROM:   PARSER
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
    
      
      DO 25 I = 1, IFC
         IF (LOCE(I)-LOCB(I) .LE. 39) THEN
C           Field Satisfies Limit of 40 Characters
            WRITE(FIELD(I),9004) (RUNST(J),J=LOCB(I),LOCE(I))
         ELSE
            WRITE(FIELD(I),9004) (RUNST(J),J=LOCB(I),LOCB(I)+39)
         END IF
 25   CONTINUE

 9004 FORMAT(40(A1:))

      RETURN
      END
