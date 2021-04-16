C   INTAKE:MARKIN.FOR                      Version Date: 15-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     SUBROUTINE MARKIN                                      *
C                                                                            *
C  Program MARKIN controls finding of the correct marker in the EPF file     *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    20-Dec-96                                               *
C  Last Modified:    05-Jun-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/INTAKE 
C     Called by: INTAKE
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
C     Date     Who  Modification Description
C   ---------  ---  ------------------------------------------------------
C   15-May-97  DLS  Initial programming started
C   05-Jun-97  DLS  Revised for INTAKE (from EXPOS)
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE MARKIN(NAME,FOUNDM)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'DEVICE.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*1 C
      CHARACTER*32 MARK,NAME
      INTEGER UNIT
      LOGICAL FOUNDM
C
C---- Data Statements --------------------------------------------------------
C
C---- Set input unit for reading ---------------------------------------------
C
      FOUNDM = .FALSE.
      UNIT = NEPF
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

