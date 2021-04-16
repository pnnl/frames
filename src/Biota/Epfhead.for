C   BTFHEAD.FOR BIOTA               Version Date: 15-May-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE BTFHEAD                               *
C                                                                            *
C  Subroutine BTFHEAD writes heading information to the BTF and BTS output   *
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
C     Module of: EDUP/BIOTA
C     Called by: BIOTA
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
C   30-Oct-09      BAN  Initial programming revised
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE BTFHEAD(NSW,NAIR,RIVNAM,AIRNAM)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NGW,NSW,NAIR
      CHARACTER*32 RIVNAM(10),AIRNAM(10)
      CHARACTER*14 SWATR,AIRNM
      CHARACTER*10 TMPDAT, TMPTIM, VERSION
C
C---- Data Statements --------------------------------------------------------
C
      DATA SWATR/'Surface Water '/
      DATA AIRNM  /'Air           '/
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
      NLOUT = 2
      if(doswx) nlout = nlout + NSW
      if(doairx) nlout = nlout + NAIR
      WRITE(NBTF,100) NLOUT
 100  FORMAT(I3,',')
      WRITE(NELS,101) 
 101  FORMAT(' This file provides a record of conditions selected',/
     .       '    for the GENII biota component.'/
     .       ' The following transport media are considered:')
c
c----- print version, and date and time run
c
      write (nBTF,301) VERSION, TMPDAT, TMPTIM
 301  FORMAT (' GENII ',A10,/ 'Run on: ', A10,'  at ',A10)
C
C-----  Write line for each surface water source name -----------------------
c
      IF(DOSWX) THEN
        DO IO = 1,NSW
          WRITE(NBTF,200) SWATR,RIVNAM(IO)
          WRITE(NELS,200) SWATR,RIVNAM(IO)
 200      FORMAT('    Medium type: ',A14,' Medium Name: ',A32)
        END DO
      ENDIF
c
C-----  Write line for each air source name ---------------------------------
c
      IF(DOAIRX) THEN
        DO IO = 1,NAIR
          WRITE(NBTF,200) AIRNM,AIRNAM(IO)
          WRITE(NELS,200) AIRNM,AIRNAM(IO)
        END DO
      ENDIF
c
C----- Write line with number of data sets (always = 1) ---------------------
c
      NSETS = 0
      IF(DOSWX)   NSETS = NSETS + 1
      IF(DOAIRX)  NSETS = NSETS + 1
      WRITE(NBTF,300) NSETS
 300  FORMAT(I3,',')
C
      RETURN
C----- END OF MODULE BTFHEAD ------------------------------------------------
      END

