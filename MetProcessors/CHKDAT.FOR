      SUBROUTINE CHKDAT(IHR)
C***********************************************************************
C*    CHKDAT      Module of PCRAMMET Meteorological Pre-processor 
C*
C*    PURPOSE:    Checks the Date and Time Stamps Between All Surface
C*                Mixing Height (and Precip Data)
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 16, 1995
C*     
C*    INPUTS:     Various Date amd Time Stamps
C*
C*    OUTPUTS:    none - program terminates on error
C*        
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

C*    Check To See If The Surface Data Time Stamp Matches
C*    The Upper Air Data Time Stamp            
      IF (ISYR(IHR) .NE. IYRMIX(2) .OR.
     &    ISMO(IHR) .NE. IMOMIX(2) .OR.
     &    ISDY(IHR) .NE. IDYMIX(2)) THEN
          GO TO 1040
      ENDIF
      
C*    If Using Precip Data...
C*    A check on dates for precipitation is not necessary because
C*    the year, month, day and hour come from the CD144 or SAMSON data
C*    which is then provided to the routines that fetch the precip for
C*    the requested hour.

      GO TO 999

C*    Error Handling
1040  CALL ERRHDL('E','1040',1)
1041  CALL ERRHDL('E','1041',1)

999   RETURN
      END  
