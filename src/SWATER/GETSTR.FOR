C   GETSTR.FOR                         Version Date: 01-May-97               
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
C  Last Modified:    01-May-97  DL Strenge                                   *
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
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  06-Dec-95  DLS  Added heading information                   
C  29-Oct-96  DLS  Converted for use with the Framework program
C  01-May-97  DLS  Added PARMTR.PAR to use LINEMAX as SETDATA dimension
C==== SUBROUTINE CALL ========================================================
C
C      INCLUDE 'DEVICE.CMN'
      CHARACTER*32 FUNCTION GETSTR(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,
     .                             C6)
      INCLUDE 'PARMTR.PAR'
      CHARACTER*80 SETDATA(LINEMAX)
      INTEGER C1,C2,C3,C4,C5,C6,NLINES,LINE
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER NAME*14
      LOGICAL MOVETO
      INTEGER T1
      CHARACTER UNITS*8,TNAME*14,VALUE*32
C
C---- Start of analyses ------------------------------------------------------
C
      value = ' '
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T1,T1,T1,T1,T1,T1,UNITS,
     .                        UNITS,VALUE
        TNAME = TNAME
        T1 = T1
        UNITS = UNITS
        GETSTR=VALUE
      ELSE
        GETSTR='NOT FOUND'
      END IF
      END
