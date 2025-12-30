C   MOVETO.FOR                              Version Date: 28-Oct-96               
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE MOVETO                                *
C                                                                            *
C  Subroutine MOVETO finds the location of the sought parameter in the .GID  *
C                 data set.                                                  *
C THIS ROUTINE MOVES TO THE ARRAY POSITION YOU ARE LOOKING                   *
C A FUNCTION VALUE OF .TRUE. IF THE RECORD IS .FOUND. FALSE IF               *
C IT IS NOT                                                                  *
C                                                                            *
C  Written by:       Karl Castleton                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    16-Nov-93                                               *
C  Last Modified:    28-Oct-96      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENERAL  
C     Called by: SUBROUTINE GETINT, GETSTR, GETREALNC
C     Calls: SEQ
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C    SETDATA     U      CHAR   Argument  Data set array to search       
C    NLINES      U      INT    Argument  Number of lines in data set used
C    NAME        U      CHAR   Argument  Name of the parameter sought
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
C     28-Oct-96    DLS  Modified to read from SETDATA instead of file
C==== SUBROUTINE CALL ========================================================
C
         LOGICAL FUNCTION MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,
     .                    C6,line)
      PARAMETER (LINEMAX = 10000)
C
C---- Variable type declarations ---------------------------------------------
C
      INTEGER C1,C2,C3,C4,C5,C6
      CHARACTER*14 NAME
      CHARACTER*120 SETDATA(LINEMAX)
      LOGICAL SEQi
      INTEGER T1,T2,T3,T4,T5,T6
      CHARACTER TNAME*14
C
C---- Start of analysis ------------------------------------------------------
C
      LINE = 0
      TNAME=' '
      T1=0
      T2=0
      T3=0
      T4=0
      T5=0
      T6=0
C
C---- Initialize output ------------------------------------------------------
C
      ILN = 0
      MOVETO=.FALSE.
C
C---- Extract a line from the SETDATA array (name, index values, and data ----
C
 300  ILN = ILN + 1
      IF(ILN.GT.NLINES) GO TO 303
      READ (SETDATA(ILN),*,ERR=303,END=303) TNAME,T1,T2,T3,T4,T5,T6
C
C---- Test name and index values against values sought -----------------------
c        
      IF ((.NOT. SEQI( TNAME , NAME,14 )) .OR. C1.NE.T1 .OR. C2.NE.T2
     +  .OR. C3.NE.T3 .OR. C4.NE.T4 .OR. C5.NE.T5
     +  .OR. C6.NE.T6) THEN
        GO TO 300
      ELSE !
        LINE = ILN
        MOVETO=.TRUE.
      END IF
 303  END
