C    EXPOS:MARKIN.FOR                      Version Date: 15-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     SUBROUTINE MARKIN                                      *
C                                                                            *
C  Program MARKIN controls finding of the correct marker in the WCF, ATO,    *
C  or SCR input file.                                                        *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    20-Dec-96                                               *
C  Last Modified:    15-May-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/EXPOS  
C     Called by: EXPOS 
C     Calls: NONE         
C     Common blocks referenced: EXINFO, DEVICE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   15-May-97      DLS  Initial programming started
C   
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MARKIN(TPATH,NAME,FOUNDM)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'DEVICE.CMN'
C     INCLUDE 'FNAMES.CMN'
C     INCLUDE 'EXINFO.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*1 C
      CHARACTER*32 MARK,NAME
      INTEGER TPATH, UNIT
      LOGICAL FOUNDM
C
C---- Data Statements --------------------------------------------------------
C
C---- Set input unit for reading ---------------------------------------------
C
      FOUNDM = .FALSE.
      UNIT = NATO
      IF(TPATH.LE.2) UNIT = NWCF
      IF(TPATH.EQ.4) UNIT = NSCF
C
C---- Read a marker line ----------------------------------------------------
C
 10   READ(UNIT,*,ERR=999,END=999) MARK,NLTEXT    ! Read a marker and number of lines
      IF(MARK.NE.NAME) THEN
        DO IL = 1,NLTEXT
          READ(UNIT,*) C
        END DO
        C = C
        GO TO 10
      END IF
      FOUNDM = .TRUE.
C
      RETURN
C
C---- Error in file read.  Marker name not found. ---------------------------
C
 999  WRITE(NERR,100) NAME,MARK
 100  FORMAT(' Error in reading marker information '/
     .       ' Sought: ',a20,'  Last found: ',a20)
C
C----- END OF MODULE MARKIN -------------------------------------------------
C
      END

