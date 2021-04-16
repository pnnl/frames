C   GETSTR.FOR                         Version Date: 29-Oct-96               
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETSTR                                *
C                                                                            *
C  Subroutine GETSTR extracts a string parameter value from the .GID file.   *
C                                                                            *
C  Written by:       Karl Castleon                                           *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    16-Nov-95                                               *
C  Last Modified:    29-Oct-96      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENERAL USE
C     Called by: 
C     Calls: MOVETO, SEQ
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C    SETDATA     U      CHAR   Argument  Array of line images to search for an
C                                        integer parameter
C    NAME        U      CHAR   Argument  Name of parameter sought.
C    C1          U      INT    Argument  CONSTITUENT COUNTER
C    C2          U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
C    C3          U      INT    Argument  LOCATION COUNTER
C    C4          U      INT    Argument  FLUX COUNTER
C    C5          U      INT    Argument  MONTH COUNTER
C    C6          U      INT    Argument  MISCELANEOUS COUNTER
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06-Dec-95    DLS  Added heading information                   
C     29-Oct-96    DLS  Converted for use with the Framework program
C==== SUBROUTINE CALL ========================================================
C
      CHARACTER*32 FUNCTION GETSTR(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,
     .                             C6)
C
C---- Variable Type Declarations ---------------------------------------------
C
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES,LINE
      CHARACTER NAME*14
      CHARACTER*80 SETDATA(linemax)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER USUNTS*8,UNITS*8,TNAME*14,VALUE*32
C
C---- Start of analyses ------------------------------------------------------
C
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,
     .                        UNITS,VALUE
        GETSTR=VALUE
      ELSE
        GETSTR='NOT FOUND'
      END IF
      END
