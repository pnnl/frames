C    EXPOS:HEADIN.FOR                      Version Date: 15-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     SUBROUTINE HEADIN                                      *
C                                                                            *
C  Program HEADIN controls reading of heading information from the soil      *
C  concentration file (SCF).                                                 *
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
C   15-May-97      DLS  Revised to use current SCF format
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE HEADIN(NMEDIA)
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
C     None
C---- Write heading text -----------------------------------------------------
C
      WRITE(NELS,402) RUNFNM
 402  FORMAT(' Heading information from soil conc file ',A28,'.SCF')
C
C---- Read heading text ------------------------------------------------------
C
      READ(NSCF,200) NTEXT       ! read number of lines of text to read
 200  FORMAT(I3)
      IF(NTEXT.GT.0) THEN
        DO IL = 1,NTEXT
          READ(NSCF,201) TEXT    ! Read a line of text
 201      FORMAT(A80)
          WRITE(NELS,101) TEXT   ! Write a line of text to output listing file
 101      FORMAT(1X,A80)
        END DO
      ENDIF
C
C----- Read line with number of media ---------------------------------------
C
      READ(NSCF,200) NMEDIA
      WRITE(NELS,405) NMEDIA
 405  FORMAT(' SCF contains information for ',I2,' media locations')
      RETURN
C
C----- END OF MODULE HEADIN -------------------------------------------------
C
      END

