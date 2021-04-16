C   EXPOS:      GETNAM.FOR             Version Date: 14-May-98               
C   Copyright 1989, 1992, 1996 by Battelle Memorial Institute.  
C                            All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETNAM                                *
C                                                                            *
C  Subroutine GETNAM This subroutine gets the file names from an .ID file    *
C                                                                            *
C  Written by:       Bonnie Hoopes/Dennis Strenge                            *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    14-May-1998  BAN                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/EXPOS 
C     Called by: EXPOS 
C     Calls: NONE
C     Common blocks referenced: DEVICE, FNAMES
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     
C==== Modification History ===================================================
C
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C     29OCT09      BAN  INITIAL DEVELOPMENT
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE GETNAM()
C
	USE DFLIB
C
C==== COMMON Block Definitions ===============================================
C
        include 'DEVICE.CMN'
        include 'FNAMES.CMN'
        INCLUDE 'EXINFO.CMN'
C
C==== DIMENSION Statements ===================================================
C
C     NONE
C 
C==== Variable Declarations ==================================================
C
      character*128 numefx1, nsite1
C
C==== DATA Statements ========================================================
C
	CALL GETARG(1, GIDFNM)
	print *, "gid file =", gidfnm
	CALL GETARG(2, RUNFNM)
	print *, "temp file =", runfnm
	CALL GETARG(3, NSITE1)
	READ(NSITE1,*)NSITE
	print *, "number of Site =", nsite
	CALL GETARG(4, NUMEfx1)
	READ(NUMEfx1,*)NUMEfx
	print *, "Receptor location =", numefx
	CALL GETARG(5, GLFNAME)
	print *, "Glyph =", glfname
C
      RETURN
      END
C====================== END OF MODULE GETNAM =============================
