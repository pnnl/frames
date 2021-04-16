C   SEQ.FOR                            Version Date: 30-May-97               
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
C  Last Modified:    30-May-97      DLS                                      *
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
C    NCHAR       U      INT    Arbument  Number of characters to check
C==== Modification History ===================================================
C
C   Date        Who  Modification Description
C  ---------    ---  ------------------------------------------------------
C  06-Dec-95  DLS  Added heading information 
C  29-Oct-96  DLS  Modified logic to require all characters to match,
C                  including blanks
C  30-May-97  DLS  Added NCHAR to argument list for variable word length
C==== SUBROUTINE CALL ========================================================
C
      LOGICAL FUNCTION SEQ(NAME1,NAME2,NCHAR)
C
C---- Variable type declarations ---------------------------------------------
C
      CHARACTER*32 NAME1,NAME2
      CHARACTER*1 NPT2
      LOGICAL TEMP
      INTEGER POS,NCHAR
C
      TEMP=.TRUE.
      POS=1
C320  IF (NAME1(POS:POS) .EQ. NAME2(POS:POS)) THEN
C       POS=POS+1
C     ELSE
C       TEMP=.FALSE.
C     END IF
 320  IF (NAME1(POS:POS) .EQ. NAME2(POS:POS)) THEN
        POS=POS+1
      ELSE
        NP1 = ICHAR(NAME2(POS:POS))
        IF(NP1.GT.90) NP2 = NP1-32
        IF(NP1.LT.91) NP2 = NP1+32
        NPT2 = CHAR(NP2)
        IF(NAME1(POS:POS).EQ.NPT2) THEN
          POS = POS + 1
        ELSE
          TEMP=.FALSE.
        END IF
      END IF
      IF (POS .LE. NCHAR.AND. TEMP .AND. (NAME1(POS:POS) .GT. ' '
     +.OR. NAME2(POS:POS) .GT. ' ')) THEN
        GOTO 320
      END IF
      SEQ=TEMP
      END
