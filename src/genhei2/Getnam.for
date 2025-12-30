C   DOSERISK:     GETNAM.FOR             Version Date: 05-Jun-98               
C   Copyright 1998 by Battelle Memorial Institute.  
C                            All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETNAM                                *
C                                                                            *
C  Subroutine GETNAM This subroutine gets the file names from an .ID file    *
C                                                                            *
C  Written by:       Bonnie Hoopes/Dennis Strenge/Bruce Napier                            *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    01/19/89 (Converted to PC)                              *
C  Last Modified:    05-Jun-1998  BAN                                        *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/DOSRISK
C     Called by: HEALTH 
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
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  05-Jun-98  BAN  Modified for use with DOSERISK
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE GETNAM()
C
      USE DFLIB
C==== COMMON Block Definitions ===============================================
C
        include 'DEVICE.CMN'
        include 'FNAMES.CMN'
C
C==== DIMENSION Statements ===================================================
C
C     NONE
C 
C==== Variable Declarations ==================================================
C
c      CHARACTER*131 RECIN
      character*128 numHEI1, nsite1
Cc	CHARACTER RECTYP
C
C   this is the place to put the replacement for the FACIL.ID file
	CALL GETARG(1, GIDFNM)
	print *, "gid file =", gidfnm
	CALL GETARG(2, RUNFNM)
	print *, "temp file =", runfnm
	CALL GETARG(3, NSITE1)
	READ(NSITE1,*)NSITE
	print *, "number of Site =", nsite
	CALL GETARG(4, NUMHEI1)
	READ(NUMHEI1,*)NUMHEI
	print *, "Receptor location =", numhei
	CALL GETARG(5, GLFNAME)
	print *, "Glyph =", glfname
CC----- Open .ID file ---------------------------------------------------------
C
c      OPEN (NIDF, FILE='FACIL.ID', STATUS='OLD')
C
C  Find input record with RECTYP = 1: Name for Input Data files (GID, RIF)
C
c    5 READ (NIDF,1003,END=6) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'1') GOTO 5
c      READ (RECIN,1005) GIDFNM
c    6 REWIND (NIDF)
cC
cC  Find input record with RECTYP = 2: Name for Case output files (HLS,
cC                                     HIF, and ERR)
cC   
c   10 READ (NIDF,1003,END=11) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'2') GOTO 10
c c     READ (RECIN,1005) RUNFNM
C      CALL UPPERC(RUNIN,N128,RUNFNM)
c   11 REWIND (NIDF)
cC
cC  Find input record with RECTYP=3: Number of Site (SCENARIO 1) to use
cC NSITE
c   15 READ (NIDF,1003,END=16) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'3') GO TO 15
c      READ (RECIN,1006) NSITE
c   16 REWIND(NIDF)
cC
cCc  Find input record with RECTYP=4: Number of HEI Glyph
cC  NUMHEI
cC
c   20 READ (NIDF,1003,END=21) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'4') GO TO 20
c      READ (RECIN,1006) NUMHEI
c   21 REWIND(NIDF)
cC
cC  Find input record with RECTYP = 5: Name for glyph (HEIName)
cC   
c   25 READ (NIDF,1003,END=26) RECIN
c      READ (RECIN,1004) RECTYP
c      IF (RECTYP.NE.'5') GOTO 25
c      READ (RECIN,1005) GLFNAME
c   26 REWIND (NIDF)
cC
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
