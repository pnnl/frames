      SUBROUTINE PARSER (RUNSTG,FIELDS,IFCC)
C***********************************************************************
C                 PARSER Module
C
C        PURPOSE: A generic routine for parsing a line of space-delimited
C                 text into individual character variables
C
C        PROGRAMMER: Jayant Hardikar
C
C        DATE:    February 9, 1995
C
C        INPUTS:  Input character string (space delimited)
C                 Number of Characters in String
C
C        OUTPUTS: Array of parsed character Variables
C
C        CALLED FROM: SAMSON
C***********************************************************************
C
C     Variable Declarations
      PARAMETER (IFMAX = 27)
      PARAMETER (ISTRG = 256)

      CHARACTER*256  RUNSTG
      CHARACTER        FIELDS(IFMAX)* 40

      CHARACTER RUNST(ISTRG)*1
      CHARACTER FIELD(IFMAX)*40

      LOGICAL INFLD
      
      COMMON /FIELDS/ LOCB(IFMAX), LOCE(IFMAX), IFC
      COMMON /FLDCHR/ FIELD
      COMMON /LOGIN1/ INFLD
      COMMON /RUNSTR/ RUNST


      DO 20 I = 1,ISTRG
         READ (RUNSTG(I:I),'(A1)') RUNST(I)
20    CONTINUE

      
C*    Convert Lower Case to Upper Case Letters        ---   CALL LWRUPR
      CALL LWRUPR (RUNSTG)
      
C     Define Fields on Card                           ---   CALL DEFINE
      CALL DEFINE

C     Get the Contents of the Fields as Characters    ---   CALL GETFLD
      CALL GETFLD


      DO 10 I = 1,IFMAX
         FIELDS (I) = FIELD(I)
10    CONTINUE

      IFCC = IFC


      RETURN
      END
