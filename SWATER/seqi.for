C   SEQI.FOR                         Version Date: 14-Aug-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     LOGICAL FUNCTION SEQI                                  *
C
C  Subroutine to compare two strings in case-insensitive mode          
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    14-Aug-97                                               *
C  Last Modified:    14-Aug-97      DLS                                      *
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
C     A          U      CHAR   Argument  First word to be compared
C     B          U      CHAR   Argument  Second word to be compared
C     NCAR       U      INT    Argument  Number of characters to be tested
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   14-Aug-97      DLS  Initial programming started
C==== SUBROUTINE CALL ========================================================
C
      LOGICAL FUNCTION SEQI(A,B,NCAR)
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*128 A,B  
      CHARACTER*1 AC,BC
      INTEGER NCAR
      LOGICAL SEQ
C
C---- Loop on number of characters to be compared.  Stop checking when -------
C     two characters do not agree (when both are expressed as capitals
C
      SEQ = .FALSE.
      DO IC = 1,NCAR
        AC = A(IC:IC)
        BC = B(IC:IC)
        IF(AC.GE.'a'.AND.AC.LE.'z')
     .     AC = CHAR(ICHAR(AC) - 32)
        IF(BC.GE.'a'.AND.BC.LE.'z')
     .     BC = CHAR(ICHAR(BC) - 32)
        IF(AC.NE.BC) GO TO 100
      END DO
C
C----- If control reaches this point then no differences were detected in ----
C      the two words.  Return a 'true' value for the function.
C
      SEQ = .TRUE.
 100  SEQI = SEQ
      RETURN
C
C----- END OF MODULE SEQI ----------------------------------------------------
C
      END






