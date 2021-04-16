C   RIFHEAD.FOR INTAKE               Version Date: 09-Jun-97               
C   Copyright 1997 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE RIFHEAD                               *
C                                                                            *
C  Subroutine RIFHEAD writes heading information to the RIF file             *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    09-Jun-97      DL Strenge                               *
C  Last Modified:    09-Jun-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/INTAKE
C     Called by: INTAKE
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     RCPSNUM    U      INT    Argument  Number of receptor sources     
C     RCPSRCNA   U      CHAR   Argument  Names of each receptor source
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   09-Jun-97      DLS  Initial programming started
C   26 July 2002   BAN  Replaced .eq. with SEQI in several spots
C   30 Sept 2004   BAN  Moved write of # of sets from here to INTAKE
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE RIFHEAD(NR,RCPSNUM,RCPSRCNA)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER RCPSNUM,NR
      CHARACTER*32 RCPSRCNA(10)
      CHARACTER*10 TMPDAT, TMPTIM, VERSION
C
C---- Data Statements --------------------------------------------------------
C
C   NONE
C
C---- Start of Analysis
C  
      VERSION ='VER 2.10.1'
      CALL DATE_AND_TIME( TMPDAT, TMPTIM )
      TMPTIM = TMPTIM(1:2)//':'//TMPTIM(3:4)//':'//TMPTIM(5:6)//'  '
      TMPDAT = TMPDAT(5:6)//'-'//TMPDAT(7:8)//'-'//TMPDAT(1:4)

c     Number of output lines in header, NLOUT
C
      NLOUT = 3+ RCPSNUM
      WRITE(NRIF,100) NLOUT
 100  FORMAT(I3,',')
      WRITE(NRIF,200) RCPSNUM
 200  FORMAT(' This receptor has data for ',I2,' exposure sources.')
      WRITE(NRLS,101) 
 101  FORMAT(' This file provides a record of conditions selected',/
     .       '    for the GENII/EDUP intake component.'/
     .       ' The following exposure sources are considered:')
C
C----- Write a line for each exposure source ---------------------------------
C
      DO IR = 1,RCPSNUM
        NRL = LENWORD(RCPSRCNA(IR),32)
        WRITE(NRIF,300) IR,RCPSRCNA(IR)(1:NRL)
 300    FORMAT(' Exposure source',I3,', is ',A)
      END DO
c
c----- print version, and date and time run
c
      write (nrif,301) VERSION, TMPDAT, TMPTIM
  301 FORMAT (' GENII ',A10,/ 'Run on: ', A10,'  at ',A10)
C
C----- Write line with number of data sets ----------------------------------
c
C---- the following write was moved to later in INTAKE 30 Sept 2004
C
c      WRITE(NRIF,400) NR
c 400  FORMAT(I3,',')
C
      RETURN
C
C----- END OF MODULE RIFHEAD ------------------------------------------------
C
      END

