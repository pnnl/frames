C    EXPOS:HEADINA.FOR                      Version Date: 08-Jan-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     SUBROUTINE HEADINA                                     *
C                                                                            *
C  Program HEADIN controls reading of heading information from the air       *
C  concentration file (ATO).                                                 *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    08-Jan-97                                               *
C  Last Modified:    08-Jan-97      DLS                                      *
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
C     08-Jan-97    DLS  Initial programming from HEADIN
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE HEADINA(NMEDIA)
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
C
C---- Data Statements --------------------------------------------------------
C
C---- Read heading text ------------------------------------------------------
C
      WRITE(NELS,100) RUNFNM
 100  FORMAT(' Heading information from air conc file ',A8,'.ATO')
      READ(NATO,200) NTEXT       ! read number of lines of text to read
 200  FORMAT(I3)
      IF(NTEXT.GT.0) THEN
        DO IL = 1,NTEXT
          READ(NATO,201) TEXT    ! Read a line of text
 201      FORMAT(A80)
          WRITE(NELS,101) TEXT   ! Write a line of text to output listing file
 101      FORMAT(1X,A80)
        END DO
      ENDIF
C
C----- Read line with number of media ---------------------------------------
C
      READ(NATO,200) NMEDIA
      WRITE(NELS,102) NMEDIA
 102  FORMAT(' ATO contains information for ',I2,' media locations')
      RETURN
C
C----- END OF MODULE HEADINA ------------------------------------------------
C
      END

