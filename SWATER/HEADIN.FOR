C    HEADIN.FOR                      Version Date: 02-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     SUBROUTINE HEADIN                                      *
C                                                                            *
C  Program HEADIN controls reading of heading information from the water     *
C  flux file (WFF).                                                          *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    30-Oct-96                                               *
C  Last Modified:    02-May-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER 
C     Called by: SWATER
C     Calls: NONE         
C     Common blocks referenced: SWINFO, DEVICE
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
C   30-Oct-96      DLS  Initial programming started
C   02-May-97      DLS  Added reading of marker sets and EOF file error.
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE HEADIN(IRIVSRC,FOUND)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'SWINFO.CMN'
      INCLUDE 'OPT.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*80 TEXT
      CHARACTER*32 MARKER
      LOGICAL FOUND
C
C---- Data Statements --------------------------------------------------------
C
C---- Read heading text ------------------------------------------------------
C
      IF(DEBUG) WRITE(NSLS,500) NSLS
 500  FORMAT(' HEADIN Called, NSLS =',I3)
      REWIND(NWFF)
C
C---- Search for data set with Marker Name equal to RivSrcName ---------------
C
c      IF(DEBUG) WRITE(NSLS,600) GLFNAME
c 600  FORMAT(' In HEADIN, searching for WFF data set: ',a40)
 10   READ(NWFF,*,END=998) MARKER,NSETL       ! read number of lines in data set
      IF(MARKER.NE.RIVSRC(IRIVSRC)) THEN
        DO ISL = 1,NSETL
          READ(NWFF,300,END=999)
  300     FORMAT(A)
        END DO
        GO TO 10
      ENDIF
      FOUND=.TRUE.
      READ(NWFF,200) NTEXT       ! read number of lines of text to read
 200  FORMAT(I3)
      WRITE(NSLS,100) GIDFNM
 100  FORMAT(' Heading information from water flux file ',A8,'.WFF')
      IF(NTEXT.GT.0) THEN
        DO IL = 1,NTEXT
          READ(NWFF,201,END=999) TEXT    ! Read a line of text
 201      FORMAT(A80)
          WRITE(NSLS,101) TEXT   ! Write a line of text to output listing file
 101      FORMAT(1X,A80)
        END DO
      ENDIF
C
C----- Read line with number of media ---------------------------------------
C
      READ(NWFF,200,END=999) NMEDIA
      WRITE(NSLS,102) NMEDIA
 102  FORMAT(' WFF contains information for ',I2,' media locations')
      RETURN
999   WRITE(NERR,1000) MARKER
 1000 FORMAT(' End of file on WFF file in HEADIN for Marker Name:',/
     . a128)
      NERROR = NERROR + 1
998   FOUND=.FALSE.
      RETURN
C----- END OF MODULE HEADIN -------------------------------------------------
      END

