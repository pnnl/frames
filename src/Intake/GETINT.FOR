C   GETINT.FOR                       Version Date: 29-Oct-96               
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETINTAK                              *
C                                                                            *
C  Subroutine GETINT extracts an integer value from the .GID file            *
C                                                                            *
C  Written by:       Karl Castleton                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    16-Nov-93                                               *
C  Last Modified:    29-Oct-96      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: MEPAS/HAZ
C     Called by: SUBROUTINE GETINTAK              
C     Calls: MOVETO
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     SETDATA    U      CHAR   Argument  Array of line images for extraction
C                                        of an integer parameter 
C     ***THE ARRAY MUST ALREADY BE FILED***
C     NAME       U      CHAR   Argument  Name of parameter sought
C     C1         U      INT    Argument  CONSTITUENT COUNTER
C     C2         U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
C     C3         U      INT    Argument  LOCATION COUNTER
C     C4         U      INT    Argument  FLUX COUNTER
C     C5         U      INT    Argument  MONTH COUNTER
C     C6         U      INT    Argument  MISCELANEOUS COUNTER
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06-Dec-95    DLS  Added heading information                 
C==== SUBROUTINE CALL ========================================================
C
      INTEGER FUNCTION GETINT(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
      INCLUDE 'PARMTR.PAR'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER C1,C2,C3,C4,C5,C6,NLINES
      CHARACTER NAME*14
      CHARACTER*80 SETDATA(LINEMAX)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER UNITS*8,USUNTS*8,TNAME*14
      INTEGER VALUE
C
C---- Start of Analysis
C    MOVETO finds the record having the desired parameter value
C
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,
     .                      VALUE
C
        TNAME = TNAME
        T1 = T1
        T2 = T2
        T3 = T3
        T4 = T4
        T5 = T5
        T6 = T6
        REF = REF
        USUNTS = USUNTS
        UNITS = UNITS
        GETINT=VALUE
      ELSE
        GETINT=0
      END IF
      END

