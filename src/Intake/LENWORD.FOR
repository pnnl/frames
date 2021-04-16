C   LENWORD.FOR                      Version Date: 24-Jul-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     INTEGER FUNCTION LENWORD                               *
C
C  Subroutine to determine length of word to end of non-blank characters
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    24-Jul-97                                               *
C  Last Modified:    24-Jul-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: MEPAS/AHAZ
C     Called by: 
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     WORD       U      CHR    Argument  Word to be evaluated   
C     NCHAR      U      INT    Argument  Number of characters to be tested
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   24-Jul-97      DLS  Initial programming started
C==== SUBROUTINE CALL ========================================================
C
      INTEGER FUNCTION LENWORD(WORD,NCHAR)
C      
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*128 WORD 
      CHARACTER*1 B
C
C---- Data Statements --------------------------------------------------------
C
      DATA B/' '/
C
C---- Start of Analysis
C  
C
      LAST = 0
      DO IC = NCHAR,1,-1
         IF(WORD(IC:IC).NE.B) THEN
           LAST = IC
           GO TO 100
         END IF
      END DO
 100  LENWORD = LAST
C
      RETURN
C----- END OF MODULE LENWORD ------------------------------------------------
      END
