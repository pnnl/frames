C    OLD.FOR                     Version Date: 09-Jul-98                  *
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.     *
C*****************************************************************************
C                                                                            *
C   SUBROUTINE OLD                                                           *
C                                                                            *
C  OLD controls calculation of DOSE AND RISK using MEPAS structure for       *
C  specified locations.  Input and output use files from the Framework       *
C  control program definition (.GID and RIF) with output to file HIF.        *
C                                                                            *
C  Written by:       BA NAPIER                                               *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    09-JUN-98                                               *
C  Last Modified:    09-JUN-98      BAN                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: HEALTH1
C     Called by: HEALTH1
C     Calls: 
C     Common blocks referenced: 
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     CONID     S/U     CHR    LOCAL     Contaminant id from FUI set in GID
C     CID       S/U     CHR    SWINFO    Contaminant id from WFF file
C     NERR       U      INT    DEVICE    Logical unit for ERR file
C     NERROR    S/U     INT              Error counter
C     NRECPT     U      INT    FNAMES    Index number of receptor to use 
C     NSITE      U      INT    FNAMES    Index number of site to be used
C     NUMEXP     U      INT    EXINFO    Number of exposure location to be used
C     NHIF       U      INT    DEVICE    Logical unit for HIF file (output
C     NRIF       U      INT    DEVICE    Logical unit for RIF file (input)
C
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ----------------------------------------------------------
C   09-Jul-98  BAN  Initial programming started
c   26 July 2002 BAN Replaced .eq. with SEQI in 6 places
C   8 July 2006  BAN Increase field size for time outputs
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE OLD(CONAME,NPRG,NTIMES,DELTIM,NHEOUT,NPOINTS,TPATH)
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
C      INCLUDE 'AFLAGS.CMN'
C      INCLUDE 'AGES.CMN'
	INCLUDE 'ALLPAR.CMN'
C      INCLUDE 'ANMPAR.CMN'
C      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'RCPINFO.CMN'
      INCLUDE 'FNAMES.CMN'
C      INCLUDE 'FODPAR.CMN'
C      INCLUDE 'FLUX.CMN'
C      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'RADATA.CMN'
	INCLUDE 'SETF.CMN'
C      INCLUDE 'SOLPAR.CMN'
C      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
c SOME STUFF TO COMPILE IT WITH AHAZ BOTTOM END
	LOGICAL  WARN,FOUNDE, SEQI
	CHARACTER*20 CONAME
	CHARACTER*22 EXNAME, EXPTYPE
	CHARACTER*7 EU
	CHARACTER*4 RISK, DOSE
	CHARACTER*6 UT, UR
	CHARACTER*22  INTAKE, RADOSE, CONINT, CONAMEP, NOCALC
	CHARACTER*17 RADOUT(4)
	CHARACTER*12 EXROUTE, CONIDP
	INTEGER CTYPE, TPATH 
	DIMENSION VALIN(1681), VALOUT(1681)
	DATA RADOUT(1) /'cancer incidence '/
	DATA RADOUT(2) /'cancer fatalities'/
	DATA RADOUT(3) /'                 '/
c	DATA RADOUT(4) /'radiation dose   '/
c	DATA DOSE /'Sv  '/
	DATA RADOUT(4) /'Sv               '/
	DATA DOSE /'dose'/
	DATA INTAKE /'intake                '/
	data RADOSE /'radiation dose        '/
	DATA CONINT /'concentration         '/
	DATA NOCALC /'no calculation        '/
	DATA NDOSE /4/,NRISK /4/, RISK /'risk'/
	DATA ZEROSKIP /0.0/
C	DATA NO/16/
C
      NORG = 1
C
	NCM = LENWORD(CONAME,20)
	NID = LENWORD(CONID,12)
        WRITE(NHIF,7001) CONAME(1:NCM),CONID(1:NID),NPRG,NTIMES
 7001   FORMAT('"',A,'","',A,'",',I2,',',I4,',')
C
C---- For each time period read intake data and calculate health impacts -----
C
        DO ITM = 1,NTIMES
          IPDAT = 1
c          CALL PDATINH(CONID,IPDAT,IERR)
		CALL DBread(CONID,IERR) 
          IF(IERR.GT.0) GO TO 999
           READ(NRIF,*) TSTART,UT,TREL,UR,NEXPTH
           NUT = LENWORD(UT,6)
           NUR = LENWORD(UR,6)
           NXOUT = NEXPTH
           TEXP = TREL
           IF(DELTIM.LT.TREL) TEXP = DELTIM
           NXOUT = NEXPTH * NHEOUT
           WRITE(NHIF,7005) TSTART,UT(1:NUT),TEXP,UR(1:NUR),NXOUT
 7005      FORMAT(1PE10.3,',"',A,'",',1PE10.3,',"',A,'",',I3,',')
           DO IEX = 1,NEXPTH
C
C  ****NEED TO FIND A BETTER WAY TO GET POPULATION DATA IN
C
              READ(NRIF,*) EXPOP,EXNAME,EXROUTE,EU,EXPTYPE
              NX = LENWORD(EXNAME,22)
              NR = LENWORD(EXROUTE,12)
              NE = LENWORD(EXPTYPE,22)
              READ(NRIF,*) (VALIN(I),I=1,NPOINTS)
              CTYPE = 0
              RTYPE = 0
              IF(SEQI(EXPTYPE,INTAKE,6)) THEN
                 RTYPE = 1
              ELSE IF(SEQI(EXPTYPE,RADOSE,14)) THEN
                 RTYPE = 2
              ELSE IF(SEQI(EXPTYPE,CONINT,13)) THEN
                 RTYPE = 1
              ELSE IF(SEQI(EXPTYPE,NOCALC,14)) THEN
                 RTYPE = 3
              ELSE
                 WRITE(NERR,6000) CONAME(1:NCM),EXNAME(1:NX),
     .                            EXROUTE(1:NR),EXPTYPE(1:NE)
 6000            FORMAT(' Error reading RIF for ',A,' pathway ',A,1x,A/
     .                  '   exposure type unidentified: ',A)
                 IERR = IERR + 1
                 GO TO 999
              ENDIF
C
C----- Calculate health impacts for current exposure pathway -----------------
C       and write results for current exposure pathway -----------------------
C
                WARN = .FALSE.
	IF (RTYPE .NE. 3) THEN !skip if "NO CALCULATION"
                IF(HEINC) THEN
                  OTYPE = 1
	            NO = LENWORD(RADOUT(1),17)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,
     .              NPOINTS,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE) THEN
                    WRITE(NHLS,2006) EXNAME(1:NX),EXROUTE(1:NR)
 2006 FORMAT(' WARNING: HIF EXPOSURE PATHWAY UNKNOWN: ',A,1X,A)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    RISK(1:NRISK),RADOUT(1)(1:NO)
 3001 FORMAT(F9.0,',"',A,'","',A,'","',A,'","',A,'",')
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) VALOUT(I)
                    ENDDO
 3002 FORMAT(1PE9.2,',')
                  ENDIF
                ENDIF
                IF(HEFAT) THEN
                  OTYPE = 2
                  NO=LENWORD(RADOUT(2),17)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,
     .              NPOINTS,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                    WRITE(NHLS,2006) EXNAME(1:NX),EXROUTE(1:NR)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    RISK(1:NRISK), RADOUT(2)(1:NO)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) VALOUT(I)
                    ENDDO
                  ENDIF
                ENDIF
                IF(HECEDE) THEN
                  OTYPE = 4
                  NO=LENWORD(RADOUT(4),17)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,
     .              NPOINTS,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                    WRITE(NHLS,2006) EXNAME(1:NX),EXROUTE(1:NR)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    DOSE(1:NDOSE), RADOUT(4)(1:NO)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) VALOUT(I)
                    ENDDO
                  ENDIF
                ENDIF
	ELSE          !write zero output lines for "NO CALCULATION"
         IF (HEINC) THEN
	            WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    RISK(1:NRISK),RADOUT(1)(1:16)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) ZEROSKIP
	              ENDDO
	   ENDIF
	   IF (HEFAT) THEN 
	            WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    RISK(1:NRISK), RADOUT(2)(1:17)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) ZEROSKIP
	              ENDDO
	   ENDIF
         IF (HECEDE) THEN
	            WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    DOSE(1:NDOSE), RADOUT(4)(1:2)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) ZEROSKIP
	              ENDDO
	   ENDIF
	ENDIF           !skip if "NO CALCULATION"
C
C----- End of processing for radionuclide results ----------------------------
C
           END DO   ! Loop on exposure pathways for parent, IEX
C
C---- If progeny data, read and process --------------------------------------
C     Only used for radionuclides, may assume MTYPE(i) = 1
C
           IF(NPRG.GT.0) THEN
              DO IPR = 1,NPRG
                 READ(NRIF,*) CONAMEP,CONIDP,NEXPTH
                 NCM = LENWORD(CONAMEP,20)
                 NID = LENWORD(CONIDP,12)
                 NXOUT = NEXPTH
C                 IF(MTYPE(1).EQ.1) NXOUT = NXOUT * NHEOUT
                 NXOUT = NXOUT * NHEOUT
                 WRITE(NHIF,7001) CONAMEP(1:NCM),CONIDP(1:NID),NXOUT
C
C---- Call PDATINI to get pollutant specific parameters for progeny ----------
C
                 IPDAT = 1
c                 CALL PDATINH(CONIDP,IPDAT,IERR)
                 CALL DBREAD(CONIDP,IERR)
                 IF(IERR.GT.0) GO TO 999
                 DO IEX = 1,NEXPTH
C                   READ(NRIF,*) EXPOP,EXNAME,EXROUTE,EU
                    READ(NRIF,*) EXPOP,EXNAME,EXROUTE,EU,EXPTYPE
                    NX = LENWORD(EXNAME,20)
                    NR = LENWORD(EXROUTE,12)
                    NU = LENWORD(EU,7)
                    NE = LENWORD(EXPTYPE,22)
                    READ(NRIF,*) (VALIN(I),I=1,NPOINTS)
c
                    RTYPE = 0
                    IF(SEQI(EXPTYPE,INTAKE,6)) THEN
                      RTYPE = 1
                    ELSE IF(SEQI(EXPTYPE,RADOSE,14)) THEN
                      RTYPE = 2
                    ELSE IF(SEQI(EXPTYPE,CONINT,13)) THEN
                      RTYPE = 1
	              ELSE IF(SEQI(EXPTYPE,NOCALC,14)) THEN
		           RTYPE = 3
                    ELSE
                 WRITE(NERR,6000) CONAMEP(1:NCM),EXNAME(1:NX),
     .                            EXROUTE(1:NR),EXPTYPE(1:NE)
                     IERR = IERR + 1
                     GO TO 999
                   ENDIF
C
C----- Calculate health impacts for current exposure pathway -----------------
C       and write results for current exposure pathway -----------------------
C
                WARN = .FALSE.
	IF (RTYPE .NE. 3) THEN !skip if "NO CALCULATION"
                IF(HEINC) THEN
                  OTYPE = 1
                  NO=LENWORD(RADOUT(1),17)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,
     .              NPOINTS,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE) THEN
                    WRITE(NHLS,2006) EXNAME(1:NX),EXROUTE(1:NR)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    RISK(1:NRISK),RADOUT(1)(1:NO)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) VALOUT(I)
                    ENDDO
                  ENDIF
                ENDIF
                IF(HEFAT) THEN
                  OTYPE = 2
                  NO=LENWORD(RADOUT(2),17)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,
     .              NPOINTS,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                    WRITE(NHLS,2006) EXNAME(1:NX),EXROUTE(1:NR)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    RISK(1:NRISK),RADOUT(2)(1:NO)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) VALOUT(I)
                    ENDDO
                  ENDIF
                ENDIF
                IF(HECEDE) THEN
                  OTYPE = 4
	            NO=LENWORD(RADOUT(4),17)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,
     .              NPOINTS,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                    WRITE(NHLS,2006) EXNAME(1:NX),EXROUTE(1:NR)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    DOSE(1:NDOSE),RADOUT(4)(1:NO)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) VALOUT(I)
                    ENDDO
                  ENDIF
                ENDIF
	ELSE          !write zero output lines for "NO CALCULATION"
         IF (HEINC) THEN
	            WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    RISK(1:NRISK),RADOUT(1)(1:16)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) ZEROSKIP
	              ENDDO
	   ENDIF
	   IF (HEFAT) THEN 
	            WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    RISK(1:NRISK), RADOUT(2)(1:17)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) ZEROSKIP
	              ENDDO
	   ENDIF
         IF (HECEDE) THEN
	             WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),
     .                    DOSE(1:NDOSE), RADOUT(4)(1:2)
                    DO I=1,NPOINTS
                    WRITE(NHIF,3002) ZEROSKIP
	              ENDDO
	   ENDIF
	ENDIF   !skip if "NO CALCULATION"
C                ENDIF
C
                 END DO   ! Loop on exposure pathways for progeny
C
              END DO  ! Loop on progeny
C
           END IF  ! If on progeny included
C
        END DO   ! Loop on time periods
C
  999 continue
      RETURN
      END
C=================== END OF MODULE OLD ====================================
