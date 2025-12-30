C   MOVESET.FOR                          Version Date: 29-Oct-96               
C   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                 LOGICAL FUNCTION MOVESET                                   *
C                                                                            *
C  Subroutine MOVESET finds the location of the sought set of parameters in  *
C  the .GID file.                                                            *
C                                                                            *
C  Written by:       Karl Castleton (as function MOVETO)                     *
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
C     Module of: GENERAL USAGE
C     Called by: SUBROUTINE GETSET
C     Calls: SEQ
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C    NGID        U      INT    Argument  Logical unit of the .GID file  
C    NAME        U      CHAR   Argument  Name of SET sought
C    NLINES      U      INT    Argument  Number of lines in set sought
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     06-Dec-95    DLS  Added heading information                 
C     28-Oct-96    DLS  Modified from MOVETO to MOVESET for Framework Application
C     29-Oct-96    DLS  Change argument names to NGID and NLINES for consistency
C==== SUBROUTINE CALL ========================================================
C
      LOGICAL FUNCTION MOVESET(NGID,NAME,NLINES)
C    
C---- Variable type declarations ---------------------------------------------
C
      INTEGER NGID,NLINES
      CHARACTER*14 NAME
      LOGICAL SEQI
      CHARACTER TNAME*14
C
C---- Start of analysis ------------------------------------------------------
C
      TNAME='              '
C
C---- Rewind .GID file to top ------------------------------------------------
C
      REWIND(NGID)
      MOVESET=.FALSE.
C
C---- Read a line from the .GID file (name, index values, and data -----------
C
 300  READ (NGID,*,ERR=303,END=303) TNAME,NLINES
C
C---- Test name and index values against values sought -----------------------
C
      IF (.NOT. SEQI( TNAME , NAME,14 ))  THEN
        GO TO 300
      ELSE !
        MOVESET=.TRUE.
      END IF
 303  END
C
C--------- END OF MODULE MOVESET ---------------------------------------------
