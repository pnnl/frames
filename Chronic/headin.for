C    EXPOS:HEADIN.FOR                      Version Date: 15-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     SUBROUTINE HEADIN                                      *
C                                                                            *
C  Program HEADIN controls reading of heading information from the water     *
C  concentration file (WCF).                                                 *
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
C   30-Oct-96      DLS  Initial programming started
C   15-May-97      DLS  Revised to use current WCF format
C   14 Oct 04      BAN  Revised 200 Format to free field read to match MEPAS
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE HEADIN(TPATH,NMEDIA)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'EXINFO.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      CHARACTER*80 TEXT
      INTEGER TPATH, UNIT
C
C---- Data Statements --------------------------------------------------------
C
C---- Set input unit for reading ---------------------------------------------
C
      UNIT = NATO
      IF(TPATH.LE.2) UNIT = NWCF
C
C---- Read heading text ------------------------------------------------------
C
      IF(TPATH.LE.2) WRITE(NELS,400) RUNFNM
 400  FORMAT(' Heading information from water conc file ',A28,'.WCF')
      IF(TPATH.EQ.3) WRITE(NELS,401) RUNFNM
 401  FORMAT(' Heading information from air conc file ',A28,'.ATO')
      IF(TPATH.EQ.4) WRITE(NELS,402) RUNFNM
 402  FORMAT(' Heading information from soil conc file ',A28,'.SCF')
c      READ(UNIT,200) NTEXT       ! read number of lines of text to read
      READ(UNIT,*) NTEXT       ! read number of lines of text to read
 200  FORMAT(I3)
      IF(NTEXT.GT.0) THEN
        DO IL = 1,NTEXT
          READ(UNIT,201) TEXT    ! Read a line of text
 201      FORMAT(A80)
          WRITE(NELS,101) TEXT   ! Write a line of text to output listing file
 101      FORMAT(1X,A80)
        END DO
      ENDIF
C
C----- Read line with number of media ---------------------------------------
C
c      READ(UNIT,200) NMEDIA
      READ(UNIT,*) NMEDIA
      IF(TPATH.LE.2) WRITE(NELS,403) NMEDIA
 403  FORMAT(' WCF contains information for ',I2,' media locations')
      IF(TPATH.EQ.3) WRITE(NELS,404) NMEDIA
 404  FORMAT(' ATO contains information for ',I2,' media locations')
      IF(TPATH.EQ.4) WRITE(NELS,405) NMEDIA
 405  FORMAT(' SRC contains information for ',I2,' media locations')
      RETURN
C----- END OF MODULE HEADIN -------------------------------------------------
      END

