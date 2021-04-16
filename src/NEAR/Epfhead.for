C   EPFHEAD.FOR EXPOS                Version Date: 15-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EPFHEAD                               *
C                                                                            *
C  Subroutine EPFHEAD writes heading information to the EPF and ELS output   *
C   files                                                                    *
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
C     Module of: EDUP/EXPOS
C     Called by: EXPOS
C     Calls: NONE  
C     Common blocks referenced: NONE
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     SETDATA    S      CHR    GID file  Storage array for GID data set 
C
C==== Modification History ===================================================
C     Date         Who  Modification Description
C     --------     ---  ------------------------------------------------------
C   20-Dec-96      DLS  Initial programming started
C   27-Dec-96      DLS  Modified to write to ELS file also
C   15-May-97      DLS  Revised use of transport type parameters and counts.
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFHEAD(NSOIL,SCRNAM)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
C     LOGICAL 
      CHARACTER*32 SCRNAM(10)
      CHARACTER*14 SOILNM
      CHARACTER*10 TMPDAT, TMPTIM, VERSION
C
C---- Data Statements --------------------------------------------------------
C
      DATA SOILNM /'Soil          '/
C
C---- Start of Analysis
C
      nsl = nsoil + 2
C
C---- Additional header information
C  
      VERSION ='VER 2.10.1'
      CALL DATE_AND_TIME( TMPDAT, TMPTIM )
      TMPTIM = TMPTIM(1:2)//':'//TMPTIM(3:4)//':'//TMPTIM(5:6)//'  '
      TMPDAT = TMPDAT(5:6)//'-'//TMPDAT(7:8)//'-'//TMPDAT(1:4) 
c     Number of output lines in header, NLOUT
C
      WRITE(NEPF,100) NSL
 100  FORMAT(I3,',')
      WRITE(NELS,101) 
 101  FORMAT(' This file provides a record of conditions selected',/
     .       '    for the GENII Near-field exposure component.'/
     .       ' The following transport media are considered:')
C
c----- print version, and date and time run
c
      write (nepf,301) VERSION, TMPDAT, TMPTIM
  301 FORMAT (' GENII ',A10,/ 'Run on: ', A10,'  at ',A10)
C
c
C-----  Write line for each soil source name --------------------------------

      DO IO = 1,NSOIL
        WRITE(NEPF,200) SOILNM,SCRNAM(IO)
 200    FORMAT('    Medium type: ',A14,' Medium Name: ',A32)
      END DO
C
C----- Write line with number of data sets (always = 1) ---------------------
c
      NSETS = 1
      WRITE(NEPF,300) NSETS
 300  FORMAT(I3,',')
C
      RETURN
C----- END OF MODULE EPFHEAD ------------------------------------------------
      END