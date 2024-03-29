C   SWATER:     GETNAM.FOR             Version Date: 01-May-97               
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
C  Last Modified:    01-May-1997  DLS   
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER
C     Called by: SWATER
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
C     28-JUL-92    DLS  Modification of heading information
C     30-OCT-96    DLS  Modification for use with SWATER                   
C     01-May-97    DLS  Added GLFNAME to input list
c     7 Feb 2002   BAN  Revised to use command line instead of FACIL.ID
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE GETNAM()
C
      USE DFLIB
C==== COMMON Block Definitions ===============================================
C
        include 'DEVICE.CMN'
        include 'FNAMES.CMN'
        INCLUDE 'SWINFO.CMN'
C
C==== DIMENSION Statements ===================================================
C
C     NONE
C 
C==== Variable Declarations ==================================================
C
c      CHARACTER*132 RECIN
c      CHARACTER*1 RECTYP
      character*128 nsite1, nriver1
C
C==== DATA Statements ========================================================
C
C   this is the place to put the replacement for the FACIL.ID file
	CALL GETARG(1, GIDFNM)
	print *, "gid file =", gidfnm
	CALL GETARG(2, RUNFNM)
	print *, "temp file =", runfnm
	CALL GETARG(3, NSITE1)
	READ(NSITE1,*)NSITE
	print *, "number of Site =", nsite
	CALL GETARG(4, NRIVER1)
	READ(NRIVER1,*)NRIVER
	print *, "Receptor location =", nriver
	CALL GETARG(5, GLFNAME)
	print *, "Glyph =", glfname
Cc      OPEN (NIDF, FILE='FACIL.ID', STATUS='OLD')
C
C  Find input record with RECTYP = 1: Name for Global Input Data file (GID)
C
c    5 READ (NIDF,1003,END=6) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'1') GOTO 5
c      READ (RECIN,1005) GIDFNM
c    6 REWIND (NIDF)
cC
cC  Find input record with RECTYP = 2: Name for Case files (WFF, WCF, ERR)
cC   
c   10 READ (NIDF,1003,END=11) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'2') GOTO 10
cc     READ (RECIN,1005) RUNFNM
cc  11 REWIND (NIDF)    
C
C  Find input record with RECTYP=3: Number of Site to use
C NSITE
c   15 READ (NIDF,1003,END=16) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'3') GO TO 15
c      READ (RECIN,1006) NSITE
c   16 REWIND(NIDF)
cC
cC  Find input record with RECTYP=4: Number of river to use
cC  NRIVER
cC
c   20 READ (NIDF,1003,END=21) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'4') GO TO 20
c      READ (RECIN,1006) NRIVER
c   21 REWIND(NIDF)
cc
cC  Find input record with RECTYP=5: Glyf Name GLFNAME
cC
c   25 READ (NIDF,1003,END=26) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'5') GO TO 25
c      READ (RECIN,1005) GLFNAME
c   26 REWIND(NIDF)
cC
c 1003 FORMAT (A80)
c 1004 FORMAT (1X,A1)
c 1005 FORMAT (3X,A)
c 1006 FORMAT (3X,I3)
C
c      CLOSE (NIDF)
C
      RETURN
      END
C====================== END OF MODULE GETNAM =============================
