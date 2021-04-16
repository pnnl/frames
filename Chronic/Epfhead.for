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
      SUBROUTINE EPFHEAD(NGW,NSW,NAIR,AQUNAM,RIVNAM,AIRNAM)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
C     LOGICAL 
c      INTEGER NGW,NSW,NAIR,NSOIL
      INTEGER NGW,NSW,NAIR
      CHARACTER*32 AQUNAM(10),RIVNAM(10),AIRNAM(10)
c      CHARACTER*32 AQUNAM(10),RIVNAM(10),AIRNAM(10),SRCNAM(10)
c      CHARACTER*14 AQUIF,SWATR,AIRNM,SOILNM
      CHARACTER*14 AQUIF,SWATR,AIRNM
      CHARACTER*10 TMPDAT, TMPTIM, VERSION
C
C---- Data Statements --------------------------------------------------------
C
      DATA AQUIF/'Aquifer       '/
      DATA SWATR/'Surface Water '/
      DATA AIRNM  /'Air           '/
c      DATA SOILNM /'Soil          '/
C
C---- Start of Analysis
C
C---- Additional header information
C  
      VERSION ='V 2.10.1  '
      CALL DATE_AND_TIME( TMPDAT, TMPTIM )
      TMPTIM = TMPTIM(1:2)//':'//TMPTIM(3:4)//':'//TMPTIM(5:6)//'  '
      TMPDAT = TMPDAT(5:6)//'-'//TMPDAT(7:8)//'-'//TMPDAT(1:4)
C
c     Number of output lines in header, NLOUT
C
      NLOUT = 2
      if(dogwx) nlout = nlout + NGW
      if(doswx) nlout = nlout + NSW
      if(doairx) nlout = nlout + NAIR
c      if(dosoilx) nlout = nlout + NSOIL
      WRITE(NEPF,100) NLOUT
 100  FORMAT(I3,',')
      WRITE(NELS,101) 
 101  FORMAT(' This file provides a record of conditions selected',/
     .       '    for the GENII chronic exposure component.'/
     .       ' The following transport media are considered:')
c
c----- print version, and date and time run
c
      write (nepf,301) VERSION, TMPDAT, TMPTIM
 301  FORMAT (' GENII ',A10,/ 'Run on: ', A10,'  at ',A10)
C
c----- Write line for each groundwater aquifer name -------------------------
c
      IF(DOGWX) THEN
        DO IO = 1,NGW
          WRITE(NEPF,200) AQUIF,AQUNAM(IO)
 200      FORMAT('    Medium type: ',A14,' Medium Name: ',A32)
          WRITE(NELS,200) AQUIF,AQUNAM(IO)
        END DO
      ENDIF
c
C-----  Write line for each surface water source name -----------------------
c
      IF(DOSWX) THEN
        DO IO = 1,NSW
          WRITE(NEPF,200) SWATR,RIVNAM(IO)
          WRITE(NELS,200) SWATR,RIVNAM(IO)
        END DO
      ENDIF
c
C-----  Write line for each air source name ---------------------------------
c
      IF(DOAIRX) THEN
        DO IO = 1,NAIR
          WRITE(NEPF,200) AIRNM,AIRNAM(IO)
          WRITE(NELS,200) AIRNM,AIRNAM(IO)
        END DO
      ENDIF
c
C-----  Write line for each soil source name --------------------------------
c
c      IF(DOSOILX) THEN
c        DO IO = 1,NSOIL
c          WRITE(NEPF,200) SOILNM,SRCNAM(IO)
c        END DO
c      ENDIF
C
C----- Write line with number of data sets (always = 1) ---------------------
c
      NSETS = 0
      IF(DOGWX)   NSETS = NSETS + 1
      IF(DOSWX)   NSETS = NSETS + 1
      IF(DOAIRX)  NSETS = NSETS + 1
c      IF(DOSOILX) NSETS = NSETS + 1
      WRITE(NEPF,300) NSETS
 300  FORMAT(I3,',')
C
      RETURN
C----- END OF MODULE EPFHEAD ------------------------------------------------
      END

