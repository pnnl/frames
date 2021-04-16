      SUBROUTINE CHKCLM(IHR)
C***********************************************************************
C*    CHKCLM      Module of PCRAMMET Meteorological Pre-processor 
C*
C*    PURPOSE:    This routine checks if the current hour is calm
C*             
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 23, 1995
C*     
C*    INPUTS:     Wind speed
C*
C*    OUTPUTS:    Logical (T or F) indicating calm conditions
C*        
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations 
      INCLUDE 'PCRAM.INC'

C*    If The Wind Speed Is Zero, Then
      IF (WSPEED(IHR) .EQ. 0.0) THEN
         CALM(IHR) = .TRUE.
      ELSE
         CALM(IHR) = .FALSE.
      ENDIF
      
      RETURN
      END      
