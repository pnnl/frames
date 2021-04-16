C   GETREAL.FOR                        Version Date: 28-Oct-96               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETREAL                               *
C                                                                            *
C  Subroutine GETREAL   extracts a real value from the SETDATA array         *
C   from the .GID file                                                       *
C                                                                            *
C  Written by:       Karl Castleton                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    16-Nov-93                                               *
C  Last Modified:    28-Oct-96 DLS for Framework Application                 *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENERAL USE
C     Called by: CALLING PROGRAM                  
C     Calls: SUBROUTINE MOVETO  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C    SETDATA     U      CHAR   Argument  Input data array from .GID file
C    ***THE DATA SET MUST ALREADY BE READ FROM GID FILE***
C    NLINES      U      INT    Argument  Number of lines used in SETDATA
C    NAME        U      CHAR   Argument  Name of parameter in .GID file
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
C     04-Dec-95    DLS  Added heading information                   
C     28-Oct-96    DLS  Modified to read from SETDATA array instead of GID
C==== SUBROUTINE CALL ========================================================
C
      REAL*4 FUNCTION GETREAL(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
      INCLUDE 'PARMTR.PAR'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER C1,C2,C3,C4,C5,C6,NLINES,LINE
      CHARACTER NAME*14
      CHARACTER*80 SETDATA(LINEMAX)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER UNITS*8,USUNTS*8,TNAME*14
      REAL VALUE
C
C---- Start of analysis
C     Logical function MOVETO finds the location of the record in the 
C     parameter file.  If found, MOVETO = .TRUE., otherwise = .FALSE.
C
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,
     .                        UNITS,VALUE
        TNAME = TNAME
        T1 = T1
        T2 = T2
        T3 = T3 
        T4 = T4
        T5 = T5
        T6 = T6
        USUNTS = USUNTS
        UNITS = UNITS
        REF = REF
        GETREAL=VALUE
      ELSE
        GETREAL=0.0
      END IF
      END

 
