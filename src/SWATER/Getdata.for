C   GETDATA.FOR                        Version Date: 08-Jan-98               
C   Copyright 1989, 1992, 1996 by Battelle Memorial Institute.  
C                            All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE GETDATA                               *
C                                                                            *
C  Subroutine GETDATA reads a set of data from the GID file for a location   *
C             and decay chain                                                *
C  Written by:       Dennis Strenge                                          *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    31-Oct-1996                                             *
C  Last Modified:    17-AUG-09  BAN                                          *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: GENII/SWATER
C     Called by: SWATER
C     Calls: NONE
C     Common blocks referenced: DEVICE, SWPAR, SWINFO
C
C==== Significant Parameter Designation and Description ======================
C
C Parameter Set/
C Name      Used   Type    Location  Parameter Description
C --------- -----  ------  --------- -------------------------------------
C    
C==== Modification History ===================================================
C   Date      Who  Modification Description
C  ---------  ---  ------------------------------------------------------------
C  07-Jan-97  DLS  Added common block OPT
C  08-Jan-98  DLS  Revised for addition of flow dilution model
C  17-AUG-09  BAN  Added NRC recirculation models
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE GETDATA(SETDATA,NLINES,IS,IL)
C
C==== COMMON Block Definitions ===============================================
C
        include 'DEVICE.CMN'
        INCLUDE 'FNAMES.CMN'
        include 'SWPAR.CMN'
        INCLUDE 'SWINFO.CMN'
        INCLUDE 'OPT.CMN'
C
C==== DIMENSION Statements ===================================================
C
C     NONE
C 
C==== Variable Declarations ==================================================
C
      LOGICAL GETLOG
      CHARACTER*7 ACUTE,CHRONIC
      CHARACTER*15  RIVER,NSLAKE
	character*80 setdata
      INTEGER GETINT
C
C==== DATA Statements ========================================================
C
      DATA ACUTE/'Acute  '/,   CHRONIC/'Chronic'/
      DATA RIVER/'River          '/, NSLAKE/'Near-shore lake'/
C
C---- Get debug flag ---------------------------------------------------------
C
      IS=IS
      IZ = 0
      DEBUG = GETLOG(SETDATA,NLINES,'GNSWDEBUG     ',IZ,IZ,
     .                          IZ,IZ,IZ,IZ)
      KSWTYPE = GETINT(SETDATA,NLINES,'GNSWType      ',IZ,IZ,
     .                          IZ,IZ,IZ,IZ)
C
C---- Test for acute or chronic surface water model --------------------------
C
      IF(SWTYPE.EQ.ACUTE) THEN                        ! Start acute
        IF(SWMODL.NE.RIVER) THEN                      ! Not river, error
          WRITE(NERR,2000) RUNFNM,SWTYPE,SWMODL
          NERROR = NERROR + 1
 2000     FORMAT(' Error in water model selection in ',A8,'.WFF'/
     .    '   For acute releases, only the river model is available'/
     .    '   Release specified = ',A7,'.  Model specified = ',A15)
          GO TO 99
        ELSE                                          ! Start acute river
C                 
C----- Get parameters for the acute river model -----------------------------
C
          IF(IL.GT.0) THEN
            IZ = 0
            SWTT(IL)= GETREAL(SETDATA,NLINES,'GNSWTT        ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWFLOW(IL) = 0.0
            SWFLOW(IL) = GETREAL(SETDATA,NLINES,'GNSWDSCHG     ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            IF(SWFLOW(IL).LE.0.) THEN
               WRITE(NERR,2001) SWFLOW(IL),IL
               NERROR = NERROR + 1
 2001          FORMAT(' Error in GETDATA.  Bad value for SWFLOW'/
     .         ' SWFLOW = ',1PE10.3,' IL =',I3)
               GO TO 99
            ENDIF
          ELSE
            TREL = 0.
            TREL = GETREAL(SETDATA,NLINES,'GNSWTREL      ',IZ,IZ,IZ,
     .                     IZ,IZ,IZ)
            IF(TREL.LE.0.) THEN
              WRITE(NERR,2002) TREL,IL
              NERROR = NERROR + 1
 2002         FORMAT(' Error in GETDATA.  Bad value for TREL'/
     .        '  TREL = ',E10.3,' IL =',I3)
              GO TO 99
            ENDIF
          ENDIF
C                                                      ! End acute input
        ENDIF                                          ! Start chronic input
C
      ELSE IF(SWTYPE.EQ.CHRONIC) THEN
        IF(DEBUG) write(NSLS,101)
 101    format(' Chronic release selected, now in GETDATA')
C
C  Add parameters for NRC recirculation models  BAN 17-AUG-09
C
        IPOND = GETINT(SETDATA,NLINES,'GNIPOND       ',IZ,IZ,
     .                          IZ,IZ,IZ,IZ)
	  IF(IPOND .NE. 0) THEN
	    PONDOUT = GETREAL(SETDATA,NLINES,'GNPONDOUT     ',IZ,IZ,
     .                       IZ,IZ,IZ,IZ)
	    PONDDIS = GETREAL(SETDATA,NLINES,'GNPONDDIS     ',IZ,IZ,
     .                       IZ,IZ,IZ,IZ)
	    PONDVOL = GETREAL(SETDATA,NLINES,'GNPONDVOL     ',IZ,IZ,
     .                       IZ,IZ,IZ,IZ)
	    PONDYRS = GETREAL(SETDATA,NLINES,'GNPONDYRS     ',IZ,IZ,
     .                       IZ,IZ,IZ,IZ)
	  END IF
C       END ADDITION BAN 17-AUG-09
        IF(SWMODL.EQ.RIVER) THEN     ! Chronic river model parameters
C  
C----- River width (SWIDTH), depth (SWDPTH), velocity (SWFLOW) --------------
C      downstream distance (SWLSX), cross-stream distance (SWOSY)
C
          IF(IL.GT.0) THEN
            IF(KSWTYPE.EQ.1) THEN
C  - read data for chronic river model
            SWIDTH(IL)=GETREAL(SETDATA,NLINES,'GNSWIDTH      ',IL,IZ,
     .                       IZ,IZ,IZ,IZ)
            SWDPTH(IL)=GETREAL(SETDATA,NLINES,'GNSWDPTH      ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWFLOW(IL)=GETREAL(SETDATA,NLINES,'GNSWVELOC     ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWLSX(IL)= GETREAL(SETDATA,NLINES,'GNSWLSX       ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWOSY(IL)= GETREAL(SETDATA,NLINES,'GNSWOSY       ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWTT(IL)= GETREAL(SETDATA,NLINES,'GNSWTT        ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            IF(SWIDTH(IL).LE.0.) THEN
              WRITE(NERR,2003) SWIDTH(IL),IL
              NERROR = NERROR + 1
 2003         FORMAT(' Error in GETDATA.  Bad value for SWIDTH'/
     .       '  SWIDTH = ',E10.3,' IL =',I3)
            ENDIF
            IF(SWFLOW(IL).LE.0.) THEN
              WRITE(NERR,2004) SWFLOW(IL),IL
              NERROR = NERROR + 1
 2004         FORMAT(' Error in GETDATA.  Bad value for SWFLOW'/
     .        '  SWFLOW = ',E10.3,' IL =',I3)
            ENDIF
            IF(SWDPTH(IL).LE.0.) THEN
              WRITE(NERR,2005) SWDPTH(IL),IL
              NERROR = NERROR + 1
 2005         FORMAT(' Error in GETDATA.  Bad value for SWDPTH'/
     .        '  SWDPTH = ',E10.3,' IL =',I3)
            ENDIF
            IF(DEBUG) WRITE(NSLS,3000) IL,SWIDTH(IL),SWDPTH(IL),
     .                SWLSX(IL),SWOSY(IL),SWFLOW(IL),SWTT(IL)
 3000       FORMAT(' In GETDATA, IL, B,D,X,Y,U,T =',I3/
     .             1P6E10.2)
            ELSE IF(KSWTYPE.EQ.3) THEN
C  - read data for flow dilution model
            SWFLOW(IL)=GETREAL(SETDATA,NLINES,'GNSWDSCHG     ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWTT(IL)= GETREAL(SETDATA,NLINES,'GNSWTT        ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            ENDIF
            IF(SWFLOW(IL).LE.0.) THEN
              WRITE(NERR,2004) SWFLOW(IL),IL
              NERROR = NERROR + 1
            ENDIF
          ELSE          ! first call for river chronic (IL = 0)
            TREL = 0.
            TREL = GETREAL(SETDATA,NLINES,'GNSWTREL      ',IZ,IZ,IZ,
     .                     IZ,IZ,IZ)
            IF(TREL.LE.0.) THEN
              WRITE(NERR,2002) TREL,IL
              NERROR = NERROR + 1
            ENDIF
          ENDIF
C
        ELSE IF (SWMODL.EQ.NSLAKE) THEN
C  chronic near-shore lake model input parameters
C  
C----- Shore source depth (SWDZ), depth (SWDPTH), velocity (SWFLOW) ---------
C      downstream distance (SWLSX), cross-stream distance (SWOSY)
C
          IF(IL.GT.0) THEN
            SWDPTH(IL)=GETREAL(SETDATA,NLINES,'GNSWDPTH      ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWFLOW(IL)=GETREAL(SETDATA,NLINES,'GNSWVELOC     ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWLSX(IL)= GETREAL(SETDATA,NLINES,'GNSWLSX       ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWOSY(IL)= GETREAL(SETDATA,NLINES,'GNSWOSY       ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            SWTT(IL)= GETREAL(SETDATA,NLINES,'GNSWTT        ',IL,IZ,
     .                          IZ,IZ,IZ,IZ)
            IF(SWFLOW(IL).LE.0.) THEN
              WRITE(NERR,2004) SWFLOW(IL),IL
              NERROR = NERROR + 1
            ENDIF
            IF(SWDPTH(IL).LE.0.) THEN
              WRITE(NERR,2005) SWDPTH(IL),IL
              NERROR = NERROR + 1
            ENDIF
            IF(DEBUG) WRITE(NSLS,3001) IL,SWDPTH(IL),SWLSX(IL),
     .                SWOSY(IL),SWFLOW(IL),SWTT(IL)
 3001       FORMAT(' In GETDATA, IL, D,X,Y,U,T =',I3,1P5E10.2)
          ELSE          ! first call for river chronic (IL = 0)
            SWDZ=GETREAL(SETDATA,NLINES,'GNSWZDEP      ',IZ,IZ, 
     .                       IZ,IZ,IZ,IZ)
            TREL = 0.
            TREL = GETREAL(SETDATA,NLINES,'GNSWTREL      ',IZ,IZ,IZ,
     .                     IZ,IZ,IZ)
            IF(TREL.LE.0.) THEN
              WRITE(NERR,2002) TREL,IL
              NERROR = NERROR + 1
            ENDIF
          ENDIF
        ELSE
C
C----  Neither river or lake selected for chronic.  write error message
C
          WRITE(NERR,3002) SWMODL
          NERROR = NERROR + 1
 3002     FORMAT(' Error in specification of water body type.  Neither', 
     .           ' river nor lake selected.'/
     .           ' SWMODL = ',A20)
        ENDIF
      ELSE
C
C----  Neither acute or chronic selected, write error message
C
          WRITE(NERR,3003) SWTYPE
          NERROR = NERROR + 1
 3003     FORMAT(' Error in specification of release type.  Neither', 
     .           ' acute nor chronic selected.'/
     .           ' SWTYPE = ',A7)
      ENDIF
C
 99   RETURN
      END
C====================== END OF MODULE GETDATA ============================
