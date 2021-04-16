C    HEADOUT.FOR                      Version Date: 27-Feb-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     SUBROUTINE HEADOUT                                     *
C                                                                            *
C  Program HEADOUT controls writing of heading information to the water      *
C  concentration file (WCF)                                                  *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    30-Oct-96                                               *
C  Last Modified:    27-Feb-97      DLS                                      *
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
C    Date     Who  Modification Description
C  ---------  ---  -----------------------------------------------------------
C  30-Oct-96  DLS  Initial programming started
C  27-Feb-96  DLS  Added common block OPT
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE HEADOUT()
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
C     CHARACTER*80 TEXT
      CHARACTER*10 TMPDAT, TMPTIM, VERSION

C---- Data Statements --------------------------------------------------------
C
C---- Additional header information
C  
      VERSION ='VER 2.10.1'
      CALL DATE_AND_TIME( TMPDAT, TMPTIM )
      TMPTIM = TMPTIM(1:2)//':'//TMPTIM(3:4)//':'//TMPTIM(5:6)//'  '
      TMPDAT = TMPDAT(5:6)//'-'//TMPDAT(7:8)//'-'//TMPDAT(1:4)
C
C---- Write heading text -----------------------------------------------------
C
      NTEXT = 5
      IF(DEBUG) WRITE(NSLS,500) NWCF
 500  FORMAT(' HEADOUT Called, NWCF =',I3)
      WRITE(NWCF,100) NTEXT
 100  FORMAT(I3)
      WRITE(NWCF,101) SITNAM,RIVNAM
 101  FORMAT(' Water concentration file generated by GENII/SWATER'/
     .       ' Site name:       ',A32/
     .       ' Water body name: ',A32)
C
c----- print version, and date and time run
c
      write (nwcf,301) VERSION, TMPDAT, TMPTIM
  301 FORMAT (' GENII ',A10,/ 'Run on: ', A10,'  at ',A10)
C
C
C----- Write line with number of usage locations (NUMEXP) -------------------
C
      WRITE(NWCF,100) NUMEXP
C
      RETURN
C
C----- END OF MODULE HEADOUT ------------------------------------------------
C
      END