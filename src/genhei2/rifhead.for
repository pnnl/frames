C   HIFHEAD.FOR INTAKE               Version Date: 09-Jun-98               
C   Copyright 1997 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE HIFHEAD                               *
C                                                                            *
C  Subroutine HIFHEAD writes heading information to the HIF file             *
C                                                                            *
C                                                                            *
C  Written by:       Dl Strenge/BA Napier                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    09-Jun-97      DL Strenge                               *
C  Last Modified:    05-Jun-98      BA Napier                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EDUP/DOSERISK
C     Called by: HEALTH
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     HEISNUM    U      INT    Argument  Number of HEALTH IMPACT GLYPHS     
C     HEISRCNA   U      CHAR   Argument  Names of each HEALTH IMPACT GLYPH
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   09-Jun-97      ban  Initial programming started FOR DOSERISK
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE HIFHEAD(NR,HEISNUM,HEISRCNA)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER HEISNUM,NR
      CHARACTER*32 HEISRCNA(10)
      CHARACTER*10 TMPDAT, TMPTIM, VERSION
C
C---- Data Statements --------------------------------------------------------
C
C   NONE
C
C---- Start of Analysis
C
C---- Additional header information
C  
      VERSION ='VER 2.10.1'
      CALL DATE_AND_TIME( TMPDAT, TMPTIM )
      TMPTIM = TMPTIM(1:2)//':'//TMPTIM(3:4)//':'//TMPTIM(5:6)//'  '
      TMPDAT = TMPDAT(5:6)//'-'//TMPDAT(7:8)//'-'//TMPDAT(1:4)
C  
c     Number of output lines in header, NLOUT
C
      NLOUT = 3 + HEISNUM
      WRITE(NHIF,100) NLOUT
 100  FORMAT(I3,',')
      WRITE(NHIF,200) HEISNUM
 200  FORMAT(' This GLYPH has data for ',I2,' RECEPTOR sources.')
      WRITE(NHLS,101) 
 101  FORMAT(' This file provides a record of conditions selected',/
     .       '    for the GENII DOSE & RISK component.'/
     .       ' The following RECEPTOR groups are considered:')
C
c----- print version, and date and time run
c
      write (nhif,301) VERSION, TMPDAT, TMPTIM
  301 FORMAT (' GENII ',A10,/ 'Run on: ', A10,'  at ',A10)
C
C----- Write a line for each receptor ---------------------------------
C
      DO IR = 1,HEISNUM
        NRL = LENWORD(HEISRCNA(IR),32)
        WRITE(NHLS,300) IR,HEISRCNA(IR)(1:NRL)
 300    FORMAT(' RECEPTOR group',I3,', is ',A)
      END DO
C
C----- Write line with number of data sets ----------------------------------
c
      WRITE(NHIF,400) NR
 400  FORMAT(I3,',"chronic",')
C
      RETURN
C
C----- END OF MODULE HIFHEAD ------------------------------------------------
C
      END

