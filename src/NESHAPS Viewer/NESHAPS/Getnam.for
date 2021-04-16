C   DOSERISK:     GETNAM.FOR             Version Date: 15-Jan-03               
C   Copyright 1998 by Battelle Memorial Institute.  
C                            All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETNAM                                *
C                                                                            *
C  Subroutine GETNAM This subroutine gets the file names from an .ID file    *
C                                                                            *
C  Written by:       Bonnie Hoopes/Dennis Strenge/Bruce Napier               *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    15-Jan-03    BAN                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/NESHAPS
C     Called by: NESHAPS
C     Calls: NONE
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     
C==== Modification History ===================================================
C
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  15-jAN-03  BAN  Modified for use with NESHAPS
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE GETNAM(GIDFNM,RUNFNM,NSITE,NUMNES,GLFNAM)
C
      USE DFLIB
C==== COMMON Block Definitions ===============================================
C
      include 'NESHAPS.CMN'
C
C
C==== DIMENSION Statements ===================================================
C
C     NONE
C 
C==== Variable Declarations ==================================================
C
      character*128 numnes1, nsite1, gidfnm, runfnm
	character*32 glfnam
C
C   this is the replacement for the FACIL.ID file; reads parameters passed by FRAMES
	CALL GETARG(1, GIDFNM)
	print *, "gid file =", gidfnm
	CALL GETARG(2, RUNFNM)
	print *, "temp file =", runfnm
	CALL GETARG(3, NSITE1)
	READ(NSITE1,*)NSITE
	print *, "number of Site =", nsite
	CALL GETARG(4, NUMnes1)
	READ(NUMnes1,*)NUMNES
	print *, "Health glyph =", numNES
	CALL GETARG(5, GLFNAM)
	print *, "Glyph =", glfname

      RETURN
      END
C====================== END OF MODULE GETNAM =============================
