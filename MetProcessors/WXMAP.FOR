      SUBROUTINE WXMAP(IWETH,IP5)
C***********************************************************************
C*    WXMAP       Module of PCRAMMET Meteorological Pre-processor 
C*
C*    PURPOSE:    Converts SAMSON present weather codes into
C*                CD144 weather observations (columns 25-29)
C*                as best as possible
C*             
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*                Adapted From DEPMET Version: 1.0 Level: 931108
C*                                           : J. Chang, SRC
C*
C*    DATE:       February 23, 1995
C*     
C*    INPUTS:     
C*            IDATE - integer    - Date for this hour YYJJJHH
C*            IPREC - int. array - Precipitation TYPE data
C*
C*    OUTPUTS:    
C*           IPCODE - integer    - Two-digit precipitation CODE
C*        
C*    REFERENCE: Press W.H. et al., 1986: Numerical Recipes, Cambridge
C*               Univ. Press, 818pp.
C*
C*    CALLED FROM: MAIN program
C***********************************************************************

C*    Variable Declarations
C*    IPREC    = COLUMNS 25-29 OF NOAA WEATHER CODE(IN CHAR FORM)
C*    IWETH    = SAMSON PRESENT WEATHER CODE ARRAY
      INTEGER IWETH(9),IP5(5)

C*    CLASSIFY LIQUID PRECIPITATION
      IF (IWETH(2) .EQ. 0) IP5(1) = 1
      IF (IWETH(2) .EQ. 1) IP5(1) = 2
      IF (IWETH(2) .EQ. 2) IP5(1) = 3
      IF (IWETH(2) .EQ. 3) IP5(1) = 4
      IF (IWETH(2) .EQ. 4) IP5(1) = 5
      IF (IWETH(2) .EQ. 5) IP5(1) = 6
      IF (IWETH(2) .EQ. 6) IP5(1) = 7
      IF (IWETH(2) .EQ. 7) IP5(1) = 8
      IF (IWETH(2) .EQ. 8) IP5(1) = 9
      IF (IWETH(2) .EQ. 9) IP5(1) = 0

      IF (IWETH(3) .EQ. 0) IP5(2) = 1
      IF (IWETH(3) .EQ. 1) IP5(2) = 2
      IF (IWETH(3) .EQ. 3) IP5(2) = 4
      IF (IWETH(3) .EQ. 4) IP5(2) = 5
      IF (IWETH(3) .EQ. 5) IP5(2) = 6
      IF (IWETH(3) .EQ. 6) IP5(2) = 7
      IF (IWETH(3) .EQ. 7) IP5(2) = 8
      IF (IWETH(3) .EQ. 8) IP5(2) = 9
      IF (IWETH(3) .EQ. 9) IP5(2) = 0

C*    CLASSIFY SNOW
      IF (IWETH(4) .EQ. 0) IP5(3) = 1
      IF (IWETH(4) .EQ. 1) IP5(3) = 2
      IF (IWETH(4) .EQ. 2) IP5(3) = 3
      IF (IWETH(4) .EQ. 3) IP5(3) = 4
      IF (IWETH(4) .EQ. 4) IP5(3) = 5
      IF (IWETH(4) .EQ. 5) IP5(3) = 6
      IF (IWETH(4) .EQ. 6) IP5(3) = 7
      IF (IWETH(4) .EQ. 7) IP5(3) = 8
      IF (IWETH(4) .EQ. 8) IP5(3) = 9
      IF (IWETH(4) .EQ. 9) IP5(3) = 0

      IF (IWETH(5) .EQ. 0) IP5(4) = 1
      IF (IWETH(5) .EQ. 1) IP5(4) = 2
      IF (IWETH(5) .EQ. 2) IP5(4) = 3
      IF (IWETH(5) .EQ. 3) IP5(4) = 4
      IF (IWETH(5) .EQ. 4) IP5(4) = 5
      IF (IWETH(5) .EQ. 5) IP5(4) = 6
      IF (IWETH(5) .EQ. 9) IP5(4) = 0

      IF (IWETH(6) .EQ. 0) IP5(5) = 1
      IF (IWETH(6) .EQ. 1) IP5(5) = 2
      IF (IWETH(6) .EQ. 2) IP5(5) = 3
      IF (IWETH(6) .EQ. 4) IP5(5) = 5
      IF (IWETH(6) .EQ. 9) IP5(5) = 0

      RETURN
      END
