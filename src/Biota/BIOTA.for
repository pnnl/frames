C    BIOTA.FOR                      Version Date: 29 Oct 2009              
C   Copyright 2009 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     PROGRAM  BIOTA                                         *
C                                                                            *
C  Program BIOTA  controls calculation of exposure media concentrations      *
C  for one  location specified.  Input and output use files from the         *
C  Framework control program definition (.WCF, .ATO, .SRC, GID) with output  *
C  to file BTF.  ADAPTED FROM GENII/CHRONIC                                  *
C  Written by:       BA NAPIER                                               *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    29 OCT 2009                                             *
C  Last Modified:    29 OCT 2009   BAN                                       *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: BIOTA  main program
C     Called by: None
C     Calls: GETSET, HEADIN, HEADOUT, GETDATA,
C     Common blocks referenced: DEVICE, FNAMES
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     CONID     S/U     CHR    LOCAL     Contaminant id from FUI set in GID
C     CID       S/U     CHR    SWINFO    Contaminant id from WFF file
C     GIDFNM     U      CHR    FNAMES    File prefix for .GID file
C     LINMAX     U      CHR    PARAMETER Maximum number of lines allowed in 
C                                        the interal array SETDATA for data
C                                        sets taken from the GID file
C     NERR       U      INT    DEVICE    Logical unit for ERR file
C     NERROR    S/U     INT              Error counter
C     NGID       U      INT    DEVICE    Logical unit for GID file
C     NRIVER     U      INT    FNAMES    Index number of river to be used
C     NSITE      U      INT    FNAMES    Index number of site to be used
C     NUMEFX     U      INT    EXINFO    Number of exposure location to be used
C     NWCF       U      INT    DEVICE    Logical unit for WCF file (waterborne)
C     NATO       U      INT    DEVICE    Logical unit for ATO file (atmospheric)
C     NBTF       U      INT    DEVICE    Logical unit for BTF file (output) now EPF
C     REL(100)  S/U     REAL   LOCAL     Release rate array from WFF file
C     RIVNAM     U      CHR    SWINFO    Name of river for current run
C     RUNFNM     U      CHR    FNAMES    File name for run files and data set
C                                        name in GID
C     SETDATA    S      CHR    GID file  Storage array for GID data set 
C     SWOUT      U      REAL   LOCAL     Array of summed water concentrations
C     TIME(100) S/U     REAL   LOCAL     Time point array from WFF file
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ----------------------------------------------------------
C     29OCT09  BAN  INITIAL DEVELOPMENT 
C ============================================================================
C
      PROGRAM BIOTA 
C
C---- Include Statements for Parameter and Common Declarations ---------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AFLAGS.CMN'
      INCLUDE 'AIRINFO.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXINFO.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'FLUX.CMN'
      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER GETINT, EFXMAX, TPATH
      INTEGER AQUNUM,RIVNUM,AIRNUM
      CHARACTER*12 CONPID(100,LENCHAIN),RNAMES(LENCHAIN),
     .             LNAMES(LENCHAIN), XNAMES(LENCHAIN)
      CHARACTER*32 GETSTR, EFXNAM, EFXSRCNA
      CHARACTER*32 AQUNAM(10),RIVNAM(10),AIRNAM(10)
      CHARACTER*32 AQUNAME,RIVNAME,AIRNAME
      CHARACTER*80 SETDATA(LINEMAX)
      CHARACTER*14 FUI
      CHARACTER*1 B
      CHARACTER*14 EFXTYPE(20),SWC,AIRC,USRNAM
      CHARACTER*12 CONID(100),chnam(LENCHAIN)
      CHARACTER*14 WTYPE
      CHARACTER*3 C14N,H3N
	CHARACTER*5 H3E
	CHARACTER*24 USRI, USRAIR, USRSW, USRD
      LOGICAL FILTER, SEQI, iskip
      LOGICAL FOUNDA, FOUNDM
C
C---- Data Statements --------------------------------------------------------
C
      DATA C14N/'C14'/, H3N/'H3 '/, H3E/'H3EL'/
      DATA FUI/'FUI           '/
      DATA B/' '/
      DATA   SWC/'Surface Water '/
      DATA  AIRC/'Air           '/
      DATA   USRD/'User Defined            '/
	DATA USRAIR/'ATO Air Module  '/
	DATA  USRSW/'WCF Surface Water Module'/
C
C---- Initialize parameter values --------------------------------------------
C
      DOSWX   = .FALSE.
      AIR     = .FALSE.
      DOAIRX  = .FALSE.
C
C---- Get file names ---------------------------------------------------------
C
      CALL GETNAM
      NGIDNM = INDEX(GIDFNM,B) - 1
      IF(NGIDNM.LE.0) NGIDNM = 8
      NRUNAM = INDEX(RUNFNM,B) - 1
      IF(NRUNAM.LE.0) NRUNAM = 128
C
C----- Open files for this run -----------------------------------------------
C  
C-------- Global Input Data file (.GID) 
C
      OPEN (UNIT=NGID,FILE=GIDFNM(1:NGIDNM)//'.GID',STATUS='OLD')
C
C
C----- Open FILENAMES.DAT for names of files read by GENII/ENV ---------------
C
      CALL OPNFIL(3,0,1)
	CALL OPNFIL(1,0,NRMD)
C
C-------- Run Error Message file (.ERR)
C
      OPEN (UNIT=NERR,FILE=RUNFNM(1:NRUNAM)//'.ERR',STATUS='UNKNOWN')
C
C-------- Run Output listing file (.BTS)
C
      OPEN (UNIT=NELS,FILE=RUNFNM(1:NRUNAM)//'.BTS',STATUS='UNKNOWN')
C
C-------- Exposure component PDCF listing file (.BTF->EPF in order to use viewers)
C
      OPEN (UNIT=NBTF,FILE=RUNFNM(1:NRUNAM)//'.EPF',STATUS='UNKNOWN')
C
C----- Write run argument values ---------------------------------------------
C
      WRITE(NELS,100) GIDFNM,RUNFNM,NSITE,NUMEFX,GLFNAME
 100  FORMAT(' GIDFNM  =',A70/
     .       ' RUNFNM  =',A70/
     .       ' NSITE   =',I3/
     .       ' NUMEFX  =',I3/
     .       ' GLFNAME =',A32)

C  
C----- Get Framework User Interface data from GID file -----------------------     
C
      FILTER = .TRUE.
      CALL GETSET(NGID,NERR,FUI,NLINES,SETDATA,NSITE,FILTER)
      FILTER = .FALSE.
C
C----- Start of Analysis
C
C----- Was the FUI data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2000) NLINES
 2000   FORMAT(' The FUI data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Get name of site ------------------------------------------------------
C
      IS = NSITE                 ! Number of sites from ID file
	I1 = 1                     ! FRAMES has a second index (unused??)
      SITNAM = GETSTR(SETDATA,NLINES,'SiteName      ',IS,IZ,IZ,IZ,IZ,IZ)
      conName= GETSTR(SETDATA,NLINES,'conName       ',IS,I1,IZ,IZ,IZ,IZ)
	WRITE(NELS,112) SITNAM, conName
 112  FORMAT(' SiteName = ',A32, 'Database name =', A32)
c
C----- Get number of exposure location ---------------------------------------
C
      EFXMAX = GETINT(SETDATA,NLINES,'EFXNum        ',IS,IZ,IZ,IZ,IZ,IZ)
      IF(EFXMAX.LT.NUMEFX.OR.NUMEFX.LE.0) THEN
        WRITE(NERR,2001) NUMEFX, EFXMAX
 2001   FORMAT(' Error in specification of exposure location number'/
     .         ' Value given: ',I3,'  Maximum allowed: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
      IE = NUMEFX                ! Number of sites from ID file
C
C----- Get name of exposure location -----------------------------------------
C
      IE = NUMEFX                ! Number of sites from ID file
      EFXNAM = GETSTR(SETDATA,NLINES,'EFXName       ',IS,IE,IZ,IZ,IZ,IZ)
      expx   =GETREAL(SETDATA,NLINES,'EFXX          ',IS,IE,IZ,IZ,IZ,IZ)
      expy   =GETREAL(SETDATA,NLINES,'EFXY          ',IS,IE,IZ,IZ,IZ,IZ)
      EFXXO= Expx * 1000.
      EFXYO= Expy * 1000.
      WRITE(NELS,102) EFXNAM,EFXXO,EFXYO
 102  FORMAT(' EFXName  = ',A32/' Coordinates (x,y) (m) = ',1p2e10.2)
C
C----- Get exposure transport source types -----------------------------------
C      There are EFXTypeNum values for the current location
C
      NEFXTYPE=GETINT(SETDATA,NLINES,'EFXTypeNum    ',IS,IE,IZ,IZ,IZ,IZ)
      WRITE(NELS,106) NEFXTYPE
 106  FORMAT(' EFXTypeNum  = ',I3)
      IF(NEFXTYPE.LE.0) THEN
        WRITE(NERR,2002) NEFXTYPE
 2002   FORMAT(' Error in specification of number of exposure types '/
     .         ' Value must be greater than zero.  Found: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Identify source types for the exposure location -----------------------
C
      NSW = 0
      NAIR = 0
      DO IT = 1,NEFXTYPE
        EFXTYPE(IT)=GETSTR(SETDATA,NLINES,'EFXType       ',IS,IE,IT,IZ,
     .                IZ,IZ)
        EFXSRCNA = GETSTR(SETDATA,NLINES,'EFXSRCName    ',IS,IE,IT,IZ,
     .                IZ,IZ)
C
C----- Check current exposure source type for "Surface Water" -----------------
C
        IF(SEQI(EFXTYPE(IT),SWC,14)) THEN ! surface water source
c        ELSEIF(EFXTYPE(IT).EQ.SWC) THEN ! surface water source
          NSW = NSW + 1
C
C--- Match EFXSrcName(it) to "RivName" to find the name of the
C            river contributing to the exposure.
C
        RIVNUM=GETINT(SETDATA,NLINES,'RivNum        ',IS,IZ,IZ,IZ,
     .                IZ,IZ)
        NS = 0
        DO IR = 1,RIVNUM
          RIVNAME=GETSTR(SETDATA,NLINES,'RivName       ',IS,IR,IZ,IZ,
     .                  IZ,IZ)
          IF(RIVNAME.EQ.EFXSRCNA) THEN
            NS = NS+1
            RIVNAM(NS) = RIVNAME
          ENDIF
        END DO
        IF(NS.LE.0) THEN
C
C---  write error message that the Surface Water source was not found in
C     the GID FUI info.
C
          WRITE(NERR,2009) EFXSRCNA, RIVNAME
 2009     FORMAT(' Error in SW source name.  Not found in GID/FUI.'/
     .    '   EFXSrcName: ',a32,' Last RivName: ',A32)
          NERROR = NERROR + 1
          GO TO 999
        ENDIF
C
C----- Check current exposure source type for "Air" --------------------------
C
        ELSEIF(SEQI(EFXTYPE(IT),AIRC,14)) THEN ! air transport source
C        ELSEIF(EFXTYPE(IT).EQ.AIRC) THEN ! air transport source
          NAIR = NAIR + 1
C
C---- Match EFXSrcName(it) to "AirName" to find the name of the
C            air source contributing to the EFXosure.  
C 
        AIRNUM=GETINT(SETDATA,NLINES,'AirNum        ',IS,IZ,IZ,IZ,
     .                IZ,IZ)
        NA = 0
        DO IA = 1,AIRNUM
          AIRNAME=GETSTR(SETDATA,NLINES,'AirName       ',IS,IA,IZ,IZ,
     .                  IZ,IZ)
          IF(AIRNAME.EQ.EFXSRCNA) THEN
            NA = NA+1
            AIRNAM(NA) = AIRNAME
          ENDIF
        END DO
        IF(NA.LE.0) THEN
C
C--- Write error message that the Air source was not found in the GID FUI info.
C
          WRITE(NERR,2010) EFXSRCNA, AIRNAME
 2010     FORMAT(' Error in Air source name.  Not found in GID/FUI.'/
     .    '   EFXSrcName: ',a32,' Last AirName: ',A32)
          NERROR = NERROR + 1
          GO TO 999
        ENDIF
C
C----- Identify User Defined Inputs ------------------------
c
        ELSEIF (SEQI(EFXTYPE(IT),USRD,12)) THEN ! USER DEFINED source
C--- COULD STILL BE EITHER AIR or SURFACE WATER
C
        USRNUM=GETINT(SETDATA,NLINES,'UsrNum        ',IS,IZ,IZ,IZ,IZ,IZ)
	  NU=0
c 
        IR = IT
c
        USRI= GETSTR(SETDATA, NLINES,'UsrModel      ',IS,IR,IZ,IZ,IZ,IZ) 
c	    
	  IF (SEQI(USRI,USRAIR,8)) THEN ! air known source  	    
	    NAIR = NAIR+1
C      match names
          USRNAM=GETSTR(SETDATA,NLINES,'UsrName       ',IS,IR,IZ,IZ,IZ,
     .                   IZ)
	    AIRNAM(NAIR) = USRNAM
	    NA=NA+1
c	     
	  ELSEIF (SEQI(USRI,USRSW,8))THEN ! surface water known source  
	    NSW = NSW+1
C      match names
          USRNAM=GETSTR(SETDATA,NLINES,'UsrName       ',IS,IR,IZ,IZ,IZ,
     .                   IZ)
	    RIVNAM(NSW) = USRNAM
	    NS=NS+1
c	  
        ENDIF
C
        ELSE
C
C----- The source type was not "Surface Water", "Air", or "USER DEFINED".
C      Write an error message
C
          WRITE(NERR,2003) EFXTYPE(IT)
 2003     FORMAT(' Error in GID/FUI specification of exposure source '
     . 'type.'/'    Must be Surface Water, Air,or User defined'
     .  '.  Found: ',A4)
          NERROR = NERROR + 1
        ENDIF
      END DO
C
C----- Get number of constituents included in the run ------------------------
C
      NUMCON = GETINT(SETDATA,NLINES,'NumCon        ',IS,IZ,IZ,IZ,IZ,IZ)
      WRITE(NELS,103) NUMCON
 103  FORMAT(' Number of constituents is ',I3)
C
C----- Get names of each constituent, number of progeny, and progeny names
C
      NUMGOOD = 0
      ICIN = 1
      DO IC = 1,NUMCON
        CONID(ICIN) = GETSTR(SETDATA,NLINES,'FSCASID       ',IS,IC,
     .                   IZ,IZ,IZ,IZ)            ! Name of parent
        NDS(ICIN) = GETINT(SETDATA,NLINES,'NDS           ',IS,IC,IZ,
     .                 IZ,IZ,IZ)                 ! Number of progeny
C
C---- Test input name to eliminate non-radionuclides -------------------------
C     Non-radionuclides have CAS numbers
C
        IF(CONID(ICIN)(1:1).LT.'A') THEN
          WRITE(NELS,2013) CONID(ICIN)
2013      FORMAT(' Warning: Pollutant name in master list is not a ',
     .    'radionuclide: ignore ',A12)
          CONID(ICIN) = B
        ELSE
          NUMGOOD = NUMGOOD + 1
          WRITE(NELS,104) CONID(ICIN),NDS(ICIN)
 104      FORMAT(' Parent name ',A12,' with progeny # ',I4)
          IF(NDS(ICIN).GT.0) THEN
            DO IP = 1,NDS(ICIN)
              CONPID(ICIN,IP) = GETSTR(SETDATA,NLINES,'FSCASID       ',
     .                             IS,IC,IP,IZ,IZ,IZ)  ! Name of progeny
              RNAMES(IP) = CONPID(ICIN,IP)
            END DO
c
c   ---  Capture branched chains here
c
     
            Do ip = 1,nds(icin)
              CONPID(ICIN,IP+nds(icin)) = GETSTR(SETDATA,NLINES,'SSCASID
     .       ',                      IS,IC,IP,IZ,IZ,IZ)  ! Name of progeny
	  if(CONPID(ICIN,IP+nds(icin)) .ne. 'NOT FOUND')then
ccc   check for duplicates, don't add if already in list
ccc
              Itally = 1
	        do ip2=1,nds(icin)
	        if(conpid(icin,IP+nds(icin)) .eq. RNAMES(ip2))Itally = 0
	        end do
	        if(itally .eq. 1) then
              RNAMES(IP+nds(icin)) = CONPID(ICIN,IP+nds(icin))
	        nds(icin) = nds(icin) + 1
	        end if
ccc
	  end if
            end do
c
            WRITE(NELS,105) (CONPID(ICIN,IP),IP=1,NDS(ICIN))
 105    FORMAT(' Progeny name(s) ',5A12)
          ENDIF
          CALL RMDGET(CONID(ICIN),RNAMES,NDS(ICIN),LNAMES,ICROS,
     .                 NOCHM,chnam)
          ICIN = ICIN + 1
        ENDIF
      END DO
      IF(NUMCON.NE.NUMGOOD) THEN
        WRITE(NELS,2014) NUMCON,NUMGOOD
 2014   FORMAT(' Warning: some pollutants are not radionuclides. '/
     .  '   Sought ',I3,' found 'i3,'.')
C     
      ENDIF
      NUMCON = NUMGOOD
c
c----- Get data set for exposure location ------------------------------------
c
      CALL GETSET(NGID,NERR,EFXNAM,NLINES,SETDATA,NSITE,FILTER)
      WRITE(NELS,1115) EFXNAM,NLINES
 1115 FORMAT(' Exposure data set sought in GID: ',A16,' lines =',i4)
C
C----- Was the Run data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2004) NLINES
 2004   FORMAT(' The Run data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
c
c----- Call REDCAS to read parameter values for this run ---------------------
c
      CALL REDCAS(SETDATA,NLINES)
c
c----- Test timing parameters ------------------------------------------------
c
      WRITE(NELS,3001)BEFORE,BEFIRR,BEFAIR,NTKEND,RELEND
 3001 FORMAT(' Timing parameters (years):'/
     .'  BEFORE, time zero to start of intake           ',F5.1/
     .'  BEFIRR, years of water irrigation before intake',I5/
     .'  BEFAIR, years of air deposition before intake  ',I5/
     .'  NTKEND, duration of intake period              ',F5.1/
     .'  RELEND, time from zero to end of release       ',F5.1)
      IF(BEFORE.LT.float(BEFIRR)) THEN
        WRITE(NERR,2006) BEFORE,BEFIRR
 2006   FORMAT(' Error in specification of prior irrigation period. '/
     .   ' ---   BEFORE = ',1pe10.2,' BEFIRR = ',i3)
        WRITE(NELS,3001)BEFORE,BEFIRR,BEFAIR,NTKEND,RELEND
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
      IF(BEFORE.LT.float(BEFAIR)) THEN
        WRITE(NERR,2007) BEFORE,BEFAIR
 2007   FORMAT(' Error in specification of prior air deposition ',
     .  'period.'/' ---   BEFORE = ',1pe10.2,' BEFAIR = ',i3)
        WRITE(NELS,3001)BEFORE,BEFIRR,BEFAIR,NTKEND,RELEND
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
      IF(NTKEND.LT.1.) NTKEND = 1.   ! Fix exposure period to a minimum of 1 year.
C
C---- Evaluate maximum time over which air concentation may be needed --------
C
       TMAX = BEFAIR + NTKEND
c
C----- Set logical flags for transport routes included -----------------------
C
      IF(NSW.GT.0.AND.NS.GT.0) DOSWX = .TRUE.
      NSW = NS
      IF(NAIR.GT.0.AND.NA.GT.0) THEN
        AIR = .TRUE.
        DOAIRX = .TRUE.
      ENDIF
      NAIR = NA
      write(NELS,1111) ns,na,doswx,doairx
 1111 format(' Results of source search:   SW  Air '/
     .       '    Number found for each:',2I5/
     .       '    Logical flag settings:',2L5)
C
C----- Call BTFHEAD to write heading information to the BTF PDCF file --------
c
       CALL BTFHEAD(NS,NA,RIVNAM,AIRNAM)
C
C----- Get water concentration data (river) each source -----------
C
C
C----- Get surface water concentration data for each source ------------------
C
      IF(DOSWX)  THEN
c
c----- Determine if surface water is used for prior irrigation ---------------
c
        DOPRIOR = .FALSE.
        IF(BEFIRR.GT.0) THEN
          IF(IRRSR.EQ.2) DOPRIOR = .TRUE.
        ENDIF
        TPATH = 2
        WTYPE = SWC
        DO IS = 1,NSW
          NUMNUC = 0
C
C-------- Open run Water Concentration File (.WCF) ---------------------------
C
       OPEN (UNIT=NWCF,FILE=GIDFNM(1:NGIDNM)//'.WCF',STATUS='UNKNOWN')
C
c----- read beginning of WCF file and position to desired data set -----------
c
       CALL MARKIN(TPATH,RIVNAM(IS),FOUNDM)
       IF(.NOT.FOUNDM) GO TO 65
          CALL WCFIN(WTYPE,EFXNAM,NUMNUC)
          IF(NUMNUC.LE.0) THEN
            WRITE(NERR,1116) WTYPE,RIVNAM(IS)
 1116       FORMAT(' Error reading WCF.  Data set not found for:',a14,
     .             ' for ',A20)
            NERROR = NERROR + 1
          ELSE
            write(NELS,1117) rivnam(is),numnuc
 1117       format(' WCF set found for: ',a20,' Numnuc =',i3)
          ENDIF
C
C----- Write data set heading to BTF file for this data set ------------------
c
          CALL BTFDSET(TPATH,EFXNAM,NPOINTS,NUMNUC)
C
          DO IN = 1,NUMNUC
            INUC = IN
            DO IC = 1,9
              RNAMES(IC) = ' '
            ENDDO
            C14 = .FALSE.
            H3  = .FALSE.
            CALL WCFDAT(CONID,CONPID,NUMCON,NDS,RNAMES,NPRG,NIC,ISKIP)
c
           IF(ISKIP .EQ. .TRUE.) GO TO 457
            IF(NIC.GT.0) THEN
            DO IX = 1,NDS(NIC)
              XNAMES(IX) = CONPID(NIC,IX)
            ENDDO
            CALL RMDGET(CONID(NIC),XNAMES,NDS(NIC),RNAMES,ICROS,
     .                  nochm,chnam)
            IF(SEQI(CHNAM(1),C14N,3)) C14 = .TRUE.
            IF(SEQI(CHNAM(1),H3N,2))  H3  = .TRUE.
            CALL SETELAW(RNAMES,NOCHM)
            CALL SETANION()
            NUCTOT = NOCHM
            CALL GETLEACH(SETDATA,NLINES,CHNAM,NOCHM)
            CALL DBreadC(CHNAM,NOCHM)  ! replacement for ENVLIB
		  IF(NERROR.GT.0) GO TO 999
            CALL ENV(TPATH) !TO DO ANALYSIS FOR THIS SURFACE WATER SOURCE TERM
		  ENDIF
            CALL BTFDAT(INUC,RNAMES,NUCTOT,TPATH,NPOINTS) ! Write results for chain to BTF
  457      CONTINUE
          END DO
 65       CLOSE(NWCF)
        END DO
      ENDIF
C
C----- Get air concentration data for each source --------------------------
C
      IF(AIR.AND.DOAIRX)  THEN
        WTYPE = AIRC
        TPATH = 3
        DO IA = 1,NAIR
          NUMNUC = 0
C
C-------- Open run Air Concentration File (.ATO) ---------------------------
C
       OPEN (UNIT=NATO,FILE=GIDFNM(1:NGIDNM)//'.ATO',STATUS='UNKNOWN')
c
c----- read beginning of ATO file and position to desired data set -----------
c
          CALL MARKIN(TPATH,AIRNAM(IA),FOUNDM)
          IF(.NOT.FOUNDM) GO TO 75
          CALL ATOIN(AIRNAM(IA),NUMNUC,FOUNDA)
          IF(.NOT.FOUNDA.OR.NUMNUC.LE.0) THEN
            WRITE(NERR,1120) wtype,AIRNAM(IA)
 1120       FORMAT(' Error reading ATO.  Data set not found for:',a14,
     .             ' for ',A20)
            NERROR = NERROR + 1
          ELSE
            write(NELS,1121) airnam(ia),numnuc
 1121       format(' ATO set found for: ',a20,' Numnuc =',i3)
          ENDIF
C
C--- Open temporary file for exposure pathway data for this parent and chain
C
          DOPRIOR=.FALSE.
          IF(BEFAIR.GT.0) DOPRIOR = .TRUE.
          ICALL = 0
          NPOINTS = NAX1 * NAX2
          DO IN = 1,NUMNUC
            OPEN (UNIT=NATP,FILE='EFXOS.ATP',STATUS='UNKNOWN')
            C14 = .FALSE.
            H3  = .FALSE.
            H3EL = .FALSE.
            CALL ATODAT(CHNAM,NOCHM,NPOINTS)
              IF(ONEPOINT) THEN
                NPOINTS = 1
              ENDIF
            IF(SEQI(CHNAM(1),C14N,3)) C14 = .TRUE.
            IF(SEQI(CHNAM(1),H3N,2))  H3  = .TRUE.
            IF(SEQI(CHNAM(1),H3E,5))  H3EL  = .TRUE.
            IF(IN.EQ.1) CALL BTFDSET(TPATH,EFXNAM,NPOINTS,NUMNUC)
            CALL SETELAW(CHNAM,NOCHM)
            CALL SETANION()
            NUCTOT = NOCHM
            CALL GETLEACH(SETDATA,NLINES,CHNAM,NOCHM)
	      CALL DBreadC(CHNAM,NOCHM)  ! replacement for ENVLIB
            IF(NERROR.GT.0) GO TO 999
              DO ITYPE = 1,3   ! Loop on atmospheric concentration types
                APTYPE = ITYPE
                DO INC = 1,NOCHM 
                  CMEM = INC
                  CALL SETAIR(NOCHM)
                  CALL ENV(TPATH)
                END DO
              END DO   
              CALL BTFGRID(CHNAM,NOCHM,ICALL)
          END DO     ! Loop on number of constituents (parents)
 75       CLOSE(NATO)
        END DO    ! Loop on number of air sources to evaluate
      ENDIF
C
C----- Soil & GW contamination are not sources for GENII biota exposure ---------
C
  999  CONTINUE
C
C----- Close files ----------------------------------------------------------
C
      CLOSE(NGID)
      CLOSE(NRMD)
      CLOSE(NBTF)
      IF(NERROR.LE.0) THEN
        CLOSE(NERR,STATUS='DELETE')
      ELSE
        CLOSE(NERR)
      ENDIF
      CLOSE(NELS)
C
C----- END OF MODULE BIOTA --------------------------------------------------
C
      END

