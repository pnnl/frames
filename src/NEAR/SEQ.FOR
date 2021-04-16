C   SEQ.FOR                            Version Date: 29-Oct-96               
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE SEQ                                   *
C                                                                            *
C  Subroutine SEQ is used by functions to extract values from the .GID file  *
C  SEQ compares two character names (14 characters) to see if they match.
C  A value of .TRUE. is returned if they match and a value of .FALSE. if they
C  don't match.
C                                                                            *
C  Written by:       Karl Castleton                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    29-Nov-95                                               *
C  Last Modified:    29-Oct-96      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENERAL USE
C     Called by: GETREAL, GETINT, GETSTR          
C     Calls: NONE 
C     Common blocks referenced: NONE 
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C    NAME1       U      STR    Argument  Name of parameter from .PRM file
C    NAME2       U      STR    Argument  Name of parameter searching for
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06-Dec-95    DLS  Added heading information 
C     29-Oct-96    DLS  Modified logic to require all characters to match,
C                       including blanks
C==== SUBROUTINE CALL ========================================================
C
      LOGICAL FUNCTION SEQ(NAME1,NAME2)
C
C---- Variable type declarations ---------------------------------------------
C
      CHARACTER*14 NAME1,NAME2
      LOGICAL TEMP
      INTEGER POS
C
      TEMP=.TRUE.
      POS=1
 320  IF (NAME1(POS:POS) .EQ. NAME2(POS:POS)) THEN
        POS=POS+1
      ELSE
        TEMP=.FALSE.
      END IF
      IF (POS .LE. 14 .AND. TEMP .AND. (NAME1(POS:POS) .GT. ' '
     +.OR. NAME2(POS:POS) .GT. ' ')) THEN
        GOTO 320
      END IF
      SEQ=TEMP
      END
