      SUBROUTINE LWRUPR (RUNST1)
C***********************************************************************
C*                LWRUPR Module of ISC2 Model
C*
C*       PURPOSE: Transfer All Characters From Lower Case To
C*                Upper Case (Using INDEX IntrinsiC*Function)
C*                Note that the CHAR*80 RUNST1 Variable Includes
C*                the Original Case for Echoing and for Later Use
C*                To Retrieve Filenames.
C*
C*       PROGRAMMER: Roger Brode, Kevin Stroupe
C*
C*       DATE:    March 2, 1992
C*
C*       MODIFIED:   Jayant Hardikar, PES, Inc.
C*                   February 13, 1995
C*                   Adapted for PCRAMMET - Made Generic
C*
C*       INPUTS:  Input Runstream Card Image (80 Character Array)
C*                Number of Characters in String, PARAMETER ISTRG
C*
C*       OUTPUTS: Input Runstream Card Image (Array) in Uppercase
C*
C*       CALLED FROM:   READIN
C***********************************************************************C*
C*    Variable Declarations
      PARAMETER (ISTRG = 80)
      CHARACTER UPCASE*26
      CHARACTER LWCASE*26
      CHARACTER*1 RUNST(ISTRG)
      CHARACTER   RUNST1*80
      
C*    Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/

      DO 10 I = 1, ISTRG
         READ (RUNST1(I:I),'(A1)') RUNST(I)
10    CONTINUE      

      DO 20 I = 1, ISTRG
         IF (RUNST(I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,RUNST(I))
            IF (INDCHK .NE. 0) THEN
               RUNST(I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
 20   CONTINUE
 
      DO 30 I = 1, ISTRG
         WRITE (RUNST1(I:I),'(A1)') RUNST(I)
30    CONTINUE


      RETURN
      END
