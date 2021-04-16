C   INTAKE:     GETNAM.FOR             Version Date: 05-Jun-97               
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
C  Last Modified:    05-Jun-1997  DLS                                        *
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
C    Date     Who  Modification Description
C  ---------  ---  ------------------------------------------------------
C  18-Dec-96  DLS  Modification for use with EXPOS
C  14-May-97  DLS  Added reading of 5th parameter (GLFNAME) and increased
C                  file name length to 128 characters.
c  05-Jun-97  DLS  Modified for use with INTAKE
cc 7 Feb 2002 BAN  REvised to read from command line instead of FACIL.ID
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
c        INCLUDE 'EXINFO.CMN'
C
C==== DIMENSION Statements ===================================================
C
C     NONE
C 
C==== Variable Declarations ==================================================
C
cc      CHARACTER*131 RECIN
cc      CHARACTER RECTYP
c      CHARACTER*128 RUNIN
      CHARACTER*128 NSITE1, NUMRCP1
C
C==== DATA Statements ========================================================
C
C      DATA N128/128/
C
C----- Open .ID file ---------------------------------------------------------
C
C   this is the place to put the replacement for the FACIL.ID file
	CALL GETARG(1, GIDFNM)
	print *, "gid file =", gidfnm
	CALL GETARG(2, RUNFNM)
	print *, "temp file =", runfnm
	CALL GETARG(3, NSITE1)
	READ(NSITE1,*)NSITE
	print *, "number of Site =", nsite
	CALL GETARG(4, NUMRCP1)
	READ(NUMRCP1,*)NUMRCP
	print *, "Receptor location =", numrcp
	CALL GETARG(5, GLFNAME)
	print *, "Glyph =", glfname
C
cc      OPEN (NIDF, FILE='FACIL.ID', STATUS='OLD')
C
C  Find input record with RECTYP = 1: Name for Input Data files (GID, EPF)
C
cc    5 READ (NIDF,1003,END=6) RECIN
cc      READ (RECIN,1004) RECTYP
cc      IF (RECTYP.NE.'1') GOTO 5
cc      READ (RECIN,1005) GIDFNM
cc    6 REWIND (NIDF)
C
C  Find input record with RECTYP = 2: Name for Case output files (RLS,
C                                     RIF, and ERR)
C   
cc   10 READ (NIDF,1003,END=11) RECIN
cc      READ (RECIN,1004) RECTYP
cc      IF (RECTYP.NE.'2') GOTO 10
cc      READ (RECIN,1005) RUNFNM
C      CALL UPPERC(RUNIN,N128,RUNFNM)
cc   11 REWIND (NIDF)
C
C  Find input record with RECTYP=3: Number of Site to use
C NSITE
cc   15 READ (NIDF,1003,END=16) RECIN
cc      READ (RECIN,1004) RECTYP
cc      IF (RECTYP.NE.'3') GO TO 15
cc      READ (RECIN,1006) NSITE
cc   16 REWIND(NIDF)
C
C  Find input record with RECTYP=4: Number of receptor location
C  NUMRCP
C
cc   20 READ (NIDF,1003,END=21) RECIN
cc      READ (RECIN,1004) RECTYP
cc      IF (RECTYP.NE.'4') GO TO 20
cc      READ (RECIN,1006) NUMRCP
cc   21 REWIND(NIDF)
C
C  Find input record with RECTYP = 5: Name for glyph (RcpName)
C   
cc   25 READ (NIDF,1003,END=26) RECIN
cc      READ (RECIN,1004) RECTYP
cc      IF (RECTYP.NE.'5') GOTO 25
cc      READ (RECIN,1005) GLFNAME
cc   26 REWIND (NIDF)
C
cc 1003 FORMAT (A128)
cc 1004 FORMAT (1X,A1)
cc 1005 FORMAT (3X,A)
cc 1006 FORMAT (3X,I3)
C
cc      CLOSE (NIDF)
C
      RETURN
      END
C====================== END OF MODULE GETNAM =============================
