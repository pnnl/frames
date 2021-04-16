C   GETLOG.FOR                       Version Date: 12-Nov-96               
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETLOG                                *
C                                                                            *
C  Subroutine GETLOG extracts a logical  value from the .GID file            *
C                                                                            *
C  Written by:       Karl Castleton/DL Strenge                               *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    16-Nov-93                                               *
C  Last Modified:    12-Nov-96 DLS Generated from GETINT                     *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: general    
C     Called by:                                  
C     Calls: MOVETO
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     SETDATA    U      CHAR   Argument  Array of line images for extraction
C                                        of a logical parameter 
C     ***THE ARRAY MUST ALREADY BE FILED***
C     NAME       U      CHAR   Argument  Name of parameter sought
C     C1         U      INT    Argument  first counter      
C     C2         U      INT    Argument  second counter
C     C3         U      INT    Argument  third counter  
C     C4         U      INT    Argument  fourth counter
C     C5         U      INT    Argument  fifth counter
C     C6         U      INT    Argument  sixth counter       
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06-Dec-95    DLS  Added heading information                 
C     12-Nov-96    DLS  Converted for reading a logical parameter value
c
C==== SUBROUTINE CALL ========================================================
C
      LOGICAL FUNCTION GETLOG(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
C      
C---- Variable Type Declarations ---------------------------------------------
C
C      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES
      CHARACTER NAME*14
      CHARACTER*120 SETDATA(10000)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER UNITS*8,USUNTS*8,TNAME*14
      LOGICAL VALUE
C
C---- Start of Analysis
C    MOVETO finds the record having the desired parameter value
C
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,
     .                      VALUE
C
        GETLOG=VALUE
      ELSE
        GETLOG=.FALSE.
      END IF
      END

