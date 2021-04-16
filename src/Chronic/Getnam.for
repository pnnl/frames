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
C     18-Dec-96    DLS  Modification for use with EXPOS
C     14-May-97    DLS  Added reading of 5th parameter (GLFNAME) and increased
C                       file name length to 128 characters.
C     14-May-98    BAN  Increase line length to 131 characters
c     7 Feb 2002   BAN  Revise to read from command line instead of FACIL.ID
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
c      CHARACTER*131 RECIN
c      CHARACTER RECTYP
c      CHARACTER*128 RUNIN
      character*128 numexp1, nsite1
C
C==== DATA Statements ========================================================
C
c      DATA N128/128/

C   this is the place to put the replacement for the FACIL.ID file
	CALL GETARG(1, GIDFNM)
	print *, "gid file =", gidfnm
	CALL GETARG(2, RUNFNM)
	print *, "temp file =", runfnm
	CALL GETARG(3, NSITE1)
	READ(NSITE1,*)NSITE
	print *, "number of Site =", nsite
	CALL GETARG(4, NUMEXP1)
	READ(NUMEXP1,*)NUMEXP
	print *, "Receptor location =", numexp
	CALL GETARG(5, GLFNAME)
	print *, "Glyph =", glfname
C
C----- Open .ID file ---------------------------------------------------------
C 
c      OPEN (NIDF, FILE='FACIL.ID', STATUS='OLD')
C
C  Find input record with RECTYP = 1: Name for Input Data files (GID, WCF,
C                                     SCF, and ATO)
C
c    5 READ (NIDF,1003,END=6) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'1') GOTO 5
c      READ (recin,1005) GIDFNM
c    6 REWIND (NIDF)
C
C  Find input record with RECTYP = 2: Name for Case output files (ELS,
C                                     EPF, and ERR)
C   
c   10 READ (NIDF,1003,END=11) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'2') GOTO 10
c      READ (recin,1005) RUNIN
c      CALL UPPERC(RUNIN,N128,RUNFNM)
c   11 REWIND (NIDF)
C
C  Find input record with RECTYP=3: Number of Site to use
C NSITE
c   15 READ (NIDF,1003,END=16) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'3') GO TO 15
c      READ (recin,1006) NSITE
c   16 REWIND(NIDF)
C
C  Find input record with RECTYP=4: Number of exposure location
C  EXPNUM
C
c   20 READ (NIDF,1003,END=21) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'4') GO TO 20
c      READ (recin,1006) NUMEXP
c   21 REWIND(NIDF)
C
C  Find input record with RECTYP = 5: Name for glyph (ExpName)
C   
c   25 READ (NIDF,1003,END=26) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'5') GOTO 25
c      READ (recin,1005) GLFNAME
c   26 REWIND (NIDF)
C
c 1003 FORMAT (A128)
c 1004 FORMAT (1X,A1)
c 1005 FORMAT (3X,A)
c 1006 FORMAT (3X,I3)
C
c      CLOSE (NIDF)
C
      RETURN
      END
C====================== END OF MODULE GETNAM =============================
