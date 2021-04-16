C   EPFHEADA.FOR ACUTE                Version Date: 24-Aug-97             
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE EPFHEADA                              *
C                                                                            *
C  Subroutine EPFHEAD writes heading information to the EPF and ELS output   *
C   files                                                                    *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle PacifiC Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    20-Dec-96                                               *
C  Last Modified:    24-Aug-97      DLS                                      *
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
C  ----------- -----  ------  --------- -------------------------------------
C  NLOUT        S/U   INT     Local     Number of output lines in header
C  NSETS        S/U   INT     Local     Number of data sets to be evaluated and
C                                       written to the EPF output file
C==== Modification History ===================================================
C     Date     Who  Modification Description
C  ---------  ---  -----------------------------------------------------------
C  20-Dec-96  DLS  Initial programming started
C  11-Jun-97  DLS  Revised use with ACUTE module                      
C  29-Jul-97  DLS  Updated comments for ACUTE module
C  24-Aug-97  DLS  Changed name to EPFHEADA and eliminated GW information
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE EPFHEADA(NSW,NAIR,RIVNAM,AIRNAM)
C
C---- Include statements -----------------------------------------------------
c
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
C     LOGICAL 
      INTEGER NSW,NAIR
      CHARACTER*32 RIVNAM(10),AIRNAM(10)
C      INTEGER NGW,NSW,NAIR,NSOIL
C      CHARACTER*32 AQUNAM(10),RIVNAM(10),AIRNAM(10),SRCNAM(10)
      CHARACTER*14 SWATR,AIRNM
C      CHARACTER*14 AQUIF,SWATR,AIRNM
      CHARACTER*10 TMPDAT, TMPTIM, VERSION

C---- Data Statements --------------------------------------------------------
C
C      DATA AQUIF/'Aquifer       '/
      DATA SWATR/'Surface Water '/
      DATA AIRNM  /'Air           '/
C      DATA SOILNM /'Soil          '/
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
      NLOUT = 2
C     if(dogwx) nlout = nlout + NGW
      if(doswx) nlout = nlout + NSW
      if(doairx) nlout = nlout + NAIR
C      if(dosoilx) nlout = nlout + NSOIL
      WRITE(NEPF,100) NLOUT
 100  FORMAT(I3,',')
      WRITE(NELS,101) 
 101  FORMAT(' This file provides a record of conditions selected',/
     .       '    for the GENII/EDUP acute exposure component.'/
     .       ' The following transport media are considered:')
C
C----- print version, and date and time run
C
      write (nepf,301) VERSION, TMPDAT, TMPTIM
 301  FORMAT (' GENII ',A10,/ 'Run on: ', A10,'  at ',A10)
C
C----- Write line for each groundwater aquifer name -------------------------
c
C     IF(DOGWX) THEN
C       DO IO = 1,NGW
C         WRITE(NEPF,200) AQUIF,AQUNAM(IO)
c200      FORMAT('    Medium type: ',A14,' Medium Name: ',A32)
C         WRITE(NELS,200) AQUIF,AQUNAM(IO)
C       END DO
C     ENDIF
C
C-----  Write line for each surface water source name -----------------------
C
      IF(DOSWX) THEN
        DO IO = 1,NSW
          WRITE(NEPF,200) SWATR,RIVNAM(IO)
          WRITE(NELS,200) SWATR,RIVNAM(IO)
 200      FORMAT('    Medium type: ',A14,' Medium Name: ',A32)
        END DO
      ENDIF
C
C-----  Write line for each air source name ---------------------------------
C
      IF(DOAIRX) THEN
        DO IO = 1,NAIR
          WRITE(NEPF,200) AIRNM,AIRNAM(IO)
          WRITE(NELS,200) AIRNM,AIRNAM(IO)
        END DO
      ENDIF
C
C-----  Write line for each soil source name --------------------------------
C
C      IF(DOSOILX) THEN
C        DO IO = 1,NSOIL
C          WRITE(NEPF,200) SOILNM,SRCNAM(IO)
C        END DO
C      ENDIF
C
C----- Write line with number of data sets (always = 1) ---------------------
C
      NSETS = 0
C     IF(DOGWX)   NSETS = NSETS + 1
      IF(DOSWX)   NSETS = NSETS + 1
      IF(DOAIRX)  NSETS = NSETS + 1
      IF(DOSOILX) NSETS = NSETS + 1
      WRITE(NEPF,300) NSETS
 300  FORMAT(I3,',')
C
C----- Normal Return ---------------------------------------------------------
C
      RETURN
      END
C
C----- END OF MODULE EPFHEADA ------------------------------------------------
C
