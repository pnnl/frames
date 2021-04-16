C    ACUTE.FOR                      Version Date:13-May 1998               
C   Copyright 1997 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     PROGRAM  ACUTE                                         *
C                                                                            *
C  Program ACUTE  controls calculation of exposure media concentrations      *
C  for one  location specified.  Input and output use files from the         *
C  Framework control program definition.  Input: WCF, ATO, SRC, GID.  Output *
C  to file EPF                                                               *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    11-Jun-97                                               *
C  Last Modified:    13-May-1998   BAN                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: ACUTE main program
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
C     NUMEXP     U      INT    EXINFO    Number of exposure location to be used
C     NWCF       U      INT    DEVICE    Logical unit for WCF file (waterborne)
C     NATO       U      INT    DEVICE    Logical unit for ATO file (atmospheric)
C     NSRC       U      INT    DEVICE    Logical unit for SRC file (soil source)
C     NEPF       U      INT    DEVICE    Logical unit for EPF file (output)
C     REL(100)  S/U     REAL   LOCAL     Release rate array from WFF file
C     RIVNAM     U      CHR    SWINFO    Name of river for current run
C     RUNFNM     U      CHR    FNAMES    File name for run files and data set
C                                        name in GID
C     SETDATA    S      CHR    GID file  Storage array for GID data set 
C     TIME(100) S/U     REAL   LOCAL     Time point array from WCF file
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ----------------------------------------------------------
C   11-Jun-97  DLS  Initial programming starting with EXPOS main program.
C   19-Mar-98  BAN  Remove ACUTE references to groundwater and soil
c   23-mAR-98  BAN  Fix FORMATs 116, 117, 2012, 2015
C   13-May-98  BAN  Remove unused calls to OPNFIL
C    5 JAN 99  BAN  Remove date call
c    4 jun 01  BAN  Add skip for non-radionuclides to SW path
c   30 Oct 01  BAN  Replaced ENVLIB with DBreadA
c   17 Oct 04  BAN  Added GETLEACH calls
c   5 dec 06   BAN  Capture branched chain inputs
c   10 April 07 BAN  account for duplicates in branched chains
c   21 AUG  08  BAN  correct text for H3EL in data statement
C
C==== SUBROUTINE CALL ========================================================
C
      PROGRAM ACUTE 
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
      INCLUDE 'RAD.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER GETINT, EXPMAX, TPATH
      INTEGER AQUNUM,RIVNUM,AIRNUM,SRCNUM
      CHARACTER*12 CONPID(100,9),RNAMES(9),LNAMES(9), XNAMES(9)
C      CHARACTER*12 PNAMES(9)
      CHARACTER*32 GETSTR, EXPNAM, EXPSRCNA
      CHARACTER*32 AQUNAM(10),RIVNAM(10),AIRNAM(10),SRCNAM(10)
      CHARACTER*32 AQUNAME,RIVNAME,AIRNAME,SRCNAME
      CHARACTER*80 SETDATA(LINEMAX)
      CHARACTER*14 FUI
      CHARACTER*1 B
      CHARACTER*14 EXPTYPE(20),GWC,SWC,AIRC,SOILC,USRNAM
      CHARACTER*12 CONID(100), CHNAM(CHAINLEN)
      CHARACTER*3 C14N,H3N
	CHARACTER*5 H3E
	CHARACTER*24 USRI,USRAIR,USRSW,USRD
C      CHARACTER*14 WTYPE
      LOGICAL FILTER, SEQI
      LOGICAL FOUND, FOUNDP, FOUNDA, FOUNDM
C
C---- Data Statements --------------------------------------------------------
C
      DATA C14N/'C14'/, H3N/'H3 '/, H3E/'H3EL'/
      DATA FUI/'FUI           '/
      DATA B/' '/
      DATA   SWC/'Surface Water '/
      DATA   GWC/'Aquifer       '/
      DATA  AIRC/'Air           '/
      DATA SOILC/'Soil          '/
	DATA   USRD/'User Defined            '/
	DATA USRAIR/'ATO Acute Air Module    '/
	DATA  USRSW/'WCF Surface Water Module'/
C
C---- Initialize parameter values --------------------------------------------
C
      DOGWX   = .FALSE.
      DOSWX   = .FALSE.
      AIR     = .FALSE.
      DOAIRX  = .FALSE.
      DOSOILX = .FALSE.
      NGW = 0
      NG = 0
      NSW = 0
      NS = 0
      NAIR = 0
      NA = 0
      NSOIL = 0
      NL = 0     
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
C-------- Radionuclide master decay data library, RMDLIB.DAT 
C
C      OPEN (UNIT=NRMD,FILE='RMDLIB.DAT',STATUS='OLD')
C
C----- Open FILENAMES.DAT for names of files read by GENII/ENV ---------------
C revise call to add LUN BAN 27 Aug 2012
      LUN = 1
c      CALL OPNFIL(3,0,1)
      CALL OPNFIL(3,0,LUN)
	CALL OPNFIL(1,0,NRMD)

C-------- Run Error Message file (.ERR)
C
      OPEN (UNIT=NERR,FILE=RUNFNM(1:NRUNAM)//'.ERR',STATUS='UNKNOWN')
C
C-------- Run Output listing file (.ELS)
C
      OPEN (UNIT=NELS,FILE=RUNFNM(1:NRUNAM)//'.ELS',STATUS='UNKNOWN')
C
C-------- Exposure component PDCF listing file (.EPF) -----------------------
C
      OPEN (UNIT=NEPF,FILE=RUNFNM(1:NRUNAM)//'.EPF',STATUS='UNKNOWN')
C
C----- Write run argument values ---------------------------------------------
C
	WRITE(NELS,100) GIDFNM,RUNFNM,NSITE,NUMEXP,GLFNAME
 100  FORMAT(' GIDFNM  =',A70/
     .       ' RUNFNM  =',A70/
     .       ' NSITE   =',I3/
     .       ' NUMEXP  =',I3/
     .       ' GLFNAME =',A32)
C
C  
C----- Get Framework User Interface data from GID file -----------------------     
C
      FILTER = .TRUE.
      CALL GETSET(NGID,NERR,FUI,NLINES,SETDATA,NSITE,FILTER)
      FILTER = .FALSE.
C
C----- Start of Analysis
C----- Get day and time ------------------------------------------------------
C
C      CALL MAKDA2
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
	WRITE(NELS,101) SITNAM, conName
 101  FORMAT(' SiteName = ',A32, 'Database name =', A32)
C
C----- Get number of exposure location ---------------------------------------
C
      EXPMAX = GETINT(SETDATA,NLINES,'ExpNum        ',IS,IZ,IZ,IZ,IZ,IZ)
      IF(EXPMAX.LT.NUMEXP.OR.NUMEXP.LE.0) THEN
        WRITE(NERR,2001) NUMEXP, EXPMAX
 2001   FORMAT(' Error in specification of exposure location number'/
     .         ' Value given: ',I3,'  Maximum allowed: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
      IE = NUMEXP                ! Number of sites from ID file
C
C----- Get name of exposure location -----------------------------------------
C
      IE = NUMEXP                ! Number of sites from ID file
      EXPNAM = GETSTR(SETDATA,NLINES,'ExpName       ',IS,IE,IZ,IZ,IZ,IZ)
      EXPX   =GETREAL(SETDATA,NLINES,'ExpX          ',IS,IE,IZ,IZ,IZ,IZ)
      EXPY   =GETREAL(SETDATA,NLINES,'ExpY          ',IS,IE,IZ,IZ,IZ,IZ)
      WRITE(NELS,102) EXPNAM,EXPX,EXPY
 102  FORMAT(' ExpName  = ',A32/' Coordinates (x,y) = ',1p2e10.2)
C
C----- Get exposure transport source types -----------------------------------
C      There are ExpTypeNum values for the current location
C
      NEXPTYPE=GETINT(SETDATA,NLINES,'ExpTypeNum    ',IS,IE,IZ,IZ,IZ,IZ)
      WRITE(NELS,106) NEXPTYPE
 106  FORMAT(' ExpTypeNum  = ',I3)
      IF(NEXPTYPE.LE.0) THEN
        WRITE(NERR,2002) NEXPTYPE
 2002   FORMAT(' Error in specification of number of exposure types '/
     .         ' Value must be greater than zero.  Found: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Identify source types for the exposure location -----------------------
C
      NGW = 0
      NSW = 0
      NAIR = 0
      NSOIL = 0
      DO IT = 1,NEXPTYPE
        EXPTYPE(IT)=GETSTR(SETDATA,NLINES,'ExpType       ',IS,IE,IT,IZ,
     .                IZ,IZ)
        EXPSRCNA = GETSTR(SETDATA,NLINES,'ExpSRCName    ',IS,IE,IT,IZ,
     .                IZ,IZ)
C
C----- Check current exposure source type for "Aquifer" ----------------------
C
        IF(SEQI(EXPTYPE(IT),GWC,14)) THEN   ! groundwater source
c        IF(EXPTYPE(IT).EQ.GWC) THEN   ! groundwater source
          NGW = NGW + 1
C
C----- Match ExpSrcName(it) to "AquName" to find the name of the
C            aquifer contributing to the exposure.  
C
        AQUNUM=GETINT(SETDATA,NLINES,'AquNum        ',IS,IZ,IZ,IZ,
     .                IZ,IZ)
        NG = 0
        DO IG = 1,AQUNUM
          AQUNAME=GETSTR(SETDATA,NLINES,'AquName       ',IS,IG,IZ,IZ,
     .                  IZ,IZ)
          IF(AQUNAME.EQ.EXPSRCNA) THEN
            NG = NG+1
            AQUNAM(NG) = AQUNAME
          ENDIF
        END DO
        IF(NG.LE.0) THEN
C
C---  write error message that the Aquifer source was not found in the GID FUI info.
C
          WRITE(NERR,2008) EXPSRCNA, AQUNAME
 2008     FORMAT(' Error in aquifer source name. Not found in GID/FUI.'/
     .    '   ExpSrcName: ',a32,' Last AquName: ',A32)
          NERROR = NERROR + 1
          GO TO 999
        ENDIF
C
C----- Check current exposure source type for "Surface Water" -----------------
C
        ELSEIF(SEQI(EXPTYPE(IT),SWC,14)) THEN ! surface water source
c        ELSEIF(EXPTYPE(IT).EQ.SWC) THEN ! surface water source
          NSW = NSW + 1
C
C--- Match ExpSrcName(it) to "RivName" to find the name of the
C            river contributing to the exposure.
C
        RIVNUM=GETINT(SETDATA,NLINES,'RivNum        ',IS,IZ,IZ,IZ,
     .                IZ,IZ)
        NS = 0
        DO IR = 1,RIVNUM
          RIVNAME=GETSTR(SETDATA,NLINES,'RivName       ',IS,IR,IZ,IZ,
     .                  IZ,IZ)
          IF(RIVNAME.EQ.EXPSRCNA) THEN
            NS = NS+1
            RIVNAM(NS) = RIVNAME
          ENDIF
        END DO
        IF(NS.LE.0) THEN
C
C---  write error message that the Surface Water source was not found in
C     the GID FUI info.
C
          WRITE(NERR,2009) EXPSRCNA, RIVNAME
 2009     FORMAT(' Error in SW source name.  Not found in GID/FUI.'/
     .    '   ExpSrcName: ',a32,' Last RivName: ',A32)
          NERROR = NERROR + 1
          GO TO 999
        ENDIF
C
C----- Check current exposure source type for "Air" --------------------------
C
        ELSEIF(SEQI(EXPTYPE(IT),AIRC,14)) THEN ! air transport source
c        ELSEIF(EXPTYPE(IT).EQ.AIRC) THEN ! air transport source
          NAIR = NAIR + 1
C
C---- Match ExpSrcName(it) to "AirName" to find the name of the
C            air source contributing to the exposure.  
C 
        AIRNUM=GETINT(SETDATA,NLINES,'AirNum        ',IS,IZ,IZ,IZ,
     .                IZ,IZ)
        NA = 0
        DO IA = 1,AIRNUM
          AIRNAME=GETSTR(SETDATA,NLINES,'AirName       ',IS,IA,IZ,IZ,
     .                  IZ,IZ)
          IF(AIRNAME.EQ.EXPSRCNA) THEN
            NA = NA+1
            AIRNAM(NA) = AIRNAME
          ENDIF
        END DO
        IF(NA.LE.0) THEN
C
C--- Write error message that the Air source was not found in the GID FUI info.
C
          WRITE(NERR,2010) EXPSRCNA, AIRNAME
 2010     FORMAT(' Error in Air source name.  Not found in GID/FUI.'/
     .    '   ExpSrcName: ',a32,' Last AirName: ',A32)
          NERROR = NERROR + 1
          GO TO 999
        ENDIF
C
C----- Identify source for soil contamination --------------------------------
c
        ELSEIF(SEQI(EXPTYPE(IT),SOILC,4)) THEN !SIOL INITIAL SOURCE
c        ELSEIF(EXPTYPE(IT).EQ.SOILC) THEN ! soil initial source
          NSOIL = NSOIL + 1
C
C--- Match ExpSrcName(it) to "SrcName" to find the name of the
C      source contributing to the initial soil cons.  Search the SCF 
C       file to get data for this source
C
        SRCNUM=GETINT(SETDATA,NLINES,'SrcNum        ',IS,IZ,IZ,IZ,
     .                IZ,IZ)
        NL = 0
        DO IL = 1,SRCNUM
          SRCNAME=GETSTR(SETDATA,NLINES,'SrcName       ',IS,IL,IZ,IZ,
     .                  IZ,IZ)
          IF(SRCNAME.EQ.EXPSRCNA) THEN
            NL = NL+1
            SRCNAM(NL) = SRCNAME
          ENDIF
        END DO
        IF(NL.LE.0) THEN
C
C---  Write error message that the Soil source was not found in the GID FUI info.
C
          WRITE(NERR,2011) EXPSRCNA, SRCNAME
 2011     FORMAT(' Error in Soil source name.  Not found in GID/FUI.'/
     .    '   ExpSrcName: ',a32,' Last SrcName: ',A32)
          NERROR = NERROR + 1
          GO TO 999
        ENDIF
C
C----- Identify User Defined Inputs ------------------------
c
        ELSEIF (SEQI(EXPTYPE(IT),USRD,12)) THEN ! USER DEFINED source
c for now, tell people that they cant use USER DEFINED
         write(nerr,3002)
 3002    format( ' USER DEFINED inputs are not currently compatible.'/
     .        'Please use the GENII dispersion modules.')
	   nerror=nerror+1
	   go to 999
c
C--- COULD STILL BE EITHER AIR, SURFACE WATER, OR GROUNDWATER
               
C
        USRI= GETSTR(SETDATA, NLINES,'UsrModel      ',IS,IT,IZ,IZ,IZ,IZ) 
	  IF (SEQI(USRI,USRAIR,24)) THEN ! air known source     
	    NAIR = NAIR+1
C      match names
          USRNAM=GETSTR(SETDATA,NLINES,'UsrName       ',IS,IT,IZ,IZ,IZ,
     .                   IZ)
	    AIRNAM(NAIR) = USRNAM
	    NA=NA+1
	  ELSEIF (SEQI(USRI,USRSW,24))THEN ! surface water known source    
	    NSW = NSW+1
C      match names
          USRNAM=GETSTR(SETDATA,NLINES,'UsrName       ',IS,IT,IZ,IZ,IZ,
     .                   IZ)
	    RIVNAM(NSW) = USRNAM
	    NS=NS+1
C   THERE IS NO ACUTE GROUNDWATER (and shouldn't have any "soil" either)
        ENDIF
C
        ELSE
C
C----- The source type was not "Aquifer", "Surface Water", "Air","Soil", OR "USER DEFINED".
C      Write an error message
C
          WRITE(NERR,2003) EXPTYPE(IT)
 2003     FORMAT(' Error in GID/FUI specification of exposure source '
     . 'type.'/'    Must be Surface Water, Air, or User Defined. ',
     . ' Found: ',A4)
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
c   --- Capture branched chain members here
c
            do ip=1,nds(icin)
              CONPID(ICIN,IP+nds(icin)) = GETSTR(SETDATA,NLINES,'SSCASID
     .       ',  IS,IC,IP,IZ,IZ,IZ)  ! Name of progeny
	      if (conpid(icin,ip+nds(icin)) .ne. 'NOT FOUND') then
ccc    check for duplicates, don't add if already in list
ccc    BAN 10 April 2007
                Itally = 1
	          do ip2 = 1, nds(icin)
	          if(conpid(icin,ip+nds(icin)) .eq. RNAMES(ip2)) Itally=0
	          end do
	          if (Itally .eq. 1) then
	          rnames(ip+nds(icin)) = conpid(icin,ip+nds(icin))
	          nds(icin) = nds(icin)+1
	          end if
ccc
	      end if
	      end do

            WRITE(NELS,105) (CONPID(ICIN,IP),IP=1,NDS(ICIN))
 105    FORMAT(' Progeny name(s) ',5A12)
          ENDIF
      CALL RMDGET(CONID(ICIN),RNAMES,NDS(ICIN),LNAMES,ICROS,NOCHM,chnam)
          ICIN = ICIN + 1
        ENDIF
      END DO
      IF(NUMCON.NE.NUMGOOD) THEN
        WRITE(NELS,2014) NUMCON,NUMGOOD
 2014   FORMAT(' Warning: some pollutants are not radionuclides. '/
     .  '   Sought ',I3,' found 'i3,'.')
C       NERROR = NERROR + 1
      ENDIF
      NUMCON = NUMGOOD
c
c----- Get data set for exposure location ------------------------------------
c
      CALL GETSET(NGID,NERR,EXPNAM,NLINES,SETDATA,NSITE,FILTER)
      WRITE(NELS,1115) EXPNAM,NLINES
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
      CALL REDCAS(SETDATA,NLINES,NSITE,NUMEXP)
c
c----- Test timing parameters ------------------------------------------------
c
      WRITE(NELS,3001)BEFORE,LOIC,BEFIRR,BEFAIR,NTKEND,RELEND
 3001 FORMAT(' Timing parameters (years):'/
     .'  BEFORE, time zero to start of intake           ',F5.1/
     .'  LOIC, time of loss of institutional control    ',I5/
     .'  BEFIRR, years of water irrigation before intake',I5/
     .'  BEFAIR, years of air deposition before intake  ',I5/
     .'  NTKEND, duration of intake period, yr          ',1PE10.2/
     .'  RELEND, time from zero to end of release, yr   ',0PF5.1)
      IF(BEFORE.LT.float(LOIC)) THEN
        WRITE(NERR,2005) BEFORE,LOIC
 2005   FORMAT(' Error in specification of loss of institutional ',
     . 'control time.'/' ---   BEFORE = ',1pe10.2,' LOIC = ',i3)
        WRITE(NERR,3001)BEFORE,LOIC,BEFIRR,BEFAIR,NTKEND,RELEND   
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
      IF(BEFORE.LT.float(BEFIRR).OR.LOIC.LT.BEFIRR) THEN
        WRITE(NERR,2006) BEFORE,BEFIRR,LOIC
 2006   FORMAT(' Error in specification of prior irrigation period. '/
     .  ' ---   BEFORE = ',1pe10.2,' BEFIRR = ',i3,' LOIC = ',i3)
        WRITE(NELS,3001)BEFORE,LOIC,BEFIRR,BEFAIR,NTKEND,RELEND
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
      IF(BEFORE.LT.float(BEFAIR).OR.LOIC.LT.BEFAIR) THEN
        WRITE(NERR,2007) BEFORE,BEFAIR,LOIC
 2007   FORMAT(' Error in specification of prior air deposition ',
     .  'period.'/' ---   BEFORE = ',1pe10.2,' BEFAIR = ',i3,
     .  ' LOIC = ',i3)
        WRITE(NELS,3001)BEFORE,LOIC,BEFIRR,BEFAIR,NTKEND,RELEND
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
      IF(NTKEND.LT.1.) NTKEND = 1.   ! Fix exposure period to a minimum of 1 year.
c
c----- Open output file and write heading ------------------------------------
c
C      CALL OPNFIL(2,1,13)
C      LUN = 21
C      CALL OPNFIL(2,0,LUN)
C      CALL HEADNG(LUN)
C
C----- Set logical flags for transport routes included -----------------------
C
      IF(NGW.GT.0.AND.NG.GT.0) DOGWX = .TRUE.
      NGW = NG
      IF(NSW.GT.0.AND.NS.GT.0) DOSWX = .TRUE.
      NSW = NS
      IF(NAIR.GT.0.AND.NA.GT.0) THEN
        AIR = .TRUE.
        DOAIRX = .TRUE.
      ENDIF
      NAIR = NA
      IF(NSOIL.GT.0.AND.NL.GT.0) DOSOILX = .TRUE.
      NSOIL = NL
      write(NELS,1111) ng,ns,na,nl,dogwx,doswx,
     .  doairx, dosoilx
 1111 format(' Results of source search:   GW   SW  Air Soil'/
     .       '    Number found for each:',4I5/
     .       '    Logical flag settings:',4L5)
C
C----- Check source types and write error messages if an invalid type
C      is given for GENII ACUTE.  Only SW and AIR are allowed.
C
      IF(NGW.GT.0) THEN
         WRITE(NERR,5005) NGW
 5005    FORMAT(' Error in specification of source types.  A ',
     .   'ground water source was given.'/'   The GENII Acute ',
     .   'exposure component does not allow acute groundwater ',
     .   ' analyses.')
        NERROR = NERROR + 1
        GO TO 999
      END IF
      NGW = 0
C
      IF(NSOIL.GT.0) THEN
         WRITE(NERR,5006) NSOIL
 5006    FORMAT(' Error in specification of source types.  A ',
     .   'SOIL source was given.'/'   The GENII Acute ',
     .   'exposure component does not allow acute soil ',
     .   ' analyses.')
        NERROR = NERROR + 1
        GO TO 999
      END IF
      NSOIL = 0

C
C----- Call EPFHEAD to write heading information to the EPF PDCF file --------
c
       CALL EPFHEADA(NS,NA,RIVNAM,AIRNAM)
C
C----- Get water concentration data (river) each source -----------
c
c----- Get surface water concentration data for each source ------------------
c
      IF(DOSWX)  THEN
c
        TPATH = 2
C        WTYPE = SWC
        DO IS = 1,NSW
          NUMNUC = 0
C
C-------- Open run Water Concentration File (.WCF) ---------------------------
C
       OPEN (UNIT=NWCF,FILE=GIDFNM(1:NGIDNM)//'.WCF',STATUS='UNKNOWN')
C
C----- read beginning of WCF file and position to desired data set -----------
C
       CALL MARKIN(TPATH,RIVNAM(IS),FOUNDM)
       IF(.NOT.FOUNDM) GO TO 65
          CALL WCFIN(TPATH,EXPNAM,NUMNUC)
          IF(NUMNUC.LE.0) THEN
            WRITE(NERR,1116) TPATH,RIVNAM(IS)
 1116       FORMAT(' Error reading water concentration file:', I5, A10)
            NERROR = NERROR + 1
          ELSE
            write(NELS,1117) rivnam(is),numnuc
 1117       FORMAT(' Water concentration file:', A10, I5)
          ENDIF
C
C---- Write data set heading to EPF file for this data set ------------------
C 
         CALL EPFDSET(TPATH,EXPNAM,NPOINTS,NUMNUC)
C 
          DO IN = 1,NUMNUC
            DO IC = 1,9
              RNAMES(IC) = ' '
            ENDDO
            CALL WCFDAT(RNAMES,NPRG)
            IF(NERROR.GT.0) GO TO 999
c
c----- Test radionuclide name against master list for validity ---------------
c
            FOUND = .FALSE.
            C14 = .FALSE.
            H3  = .FALSE.
            DO IC = 1,NUMCON
              IF(RNAMES(1).EQ.CONID(IC)) THEN
                FOUND = .TRUE.
                NIC = IC
                IF(SEQI(RNAMES(1),C14N,3)) C14 = .TRUE.
                IF(SEQI(RNAMES(1),H3N,3))  H3  = .TRUE.
                GO TO 120
              ENDIF
            END DO
 120        IF(.NOT.FOUND) THEN
              WRITE(NELS,2012) RNAMES(1)
 2012         FORMAT(' NOT FOUND:', A10)
              WRITE(NEPF, 2015) TRIM(RNAMES(1)), TRIM(RNAMES(1))
 2015         FORMAT('"',A,'","',A,',0,0')
            ELSE
c
c---- test progeny names, if any
c
             IF(NPRG.GT.0) THEN
              DO IP = 1,NPRG
                FOUNDP = .FALSE.
                DO IMP = 1,NDS(NIC)
                  IF(RNAMES(IP+1).EQ.CONPID(NIC,IMP)) THEN
                    FOUNDP = .TRUE.
                  ENDIF
                END DO
C commented out by BAN 31 March 2000 to avoid problems with database
C                IF(.NOT.FOUNDP) THEN
C                  WRITE(NERR,2015) IP,RNAMES(IP+1)
C 2015             FORMAT('PROGENY NOT FOUND:', I5, 9A10)
C                  NERROR = NERROR + 1
C                ENDIF
              END DO
 
             ENDIF
            NPR = NDS(IN)
            DO ICM = 1,NPR
              RNAMES(ICM) = CONPID(IN,ICM)
            ENDDO
            CALL RMDGET(CONID(IN),RNAMES,NPR,XNAMES,ICROS,NOCHM,chnam)
            CALL SETELAW(XNAMES,NDS(NIC))
            CALL SETANION()
            NUCTOT = NDS(NIC) + 1
c            CALL ENVLIB(TPATH)   ! Get environmental transfer factors for chain
c
            CALL GETLEACH(SETDATA,NLINES,CHNAM,NOCHM)
c
	CALL DBreadA(CHNAM,NOCHM)  ! REPLACEMENT FOR ENVLIB
            IF(NERROR.GT.0) GO TO 999
            CALL ENV(TPATH) !TO DO ANALYSIS FOR THIS SURFACE WATER SOURCE TERM
            CALL EPFDAT(XNAMES,NUCTOT) ! Write results for chain to EPF
          ENDIF
          END DO
 65       CLOSE(NWCF)
        END DO
      ENDIF
c
c----- Get air concentration data for each source --------------------------
c
      IF(AIR.AND.DOAIRX)  THEN
C        WTYPE = AIRC
        TPATH = 3
        DO IA = 1,NAIR
          NUMNUC = 0
c
c-------- Open run Air Concentration File (.ATO) ---------------------------
c
         OPEN (UNIT=NATO,FILE=GIDFNM(1:NGIDNM)//'.ATO',STATUS='UNKNOWN')
c
c----- read beginning of ATO file and position to desired data set -----------
c
         CALL MARKIN(TPATH,AIRNAM(IA),FOUNDM)
         IF(.NOT.FOUNDM) GO TO 75
          CALL ATOIN(AIRNAM(IA),NUMNUC,FOUNDA)
          IF(NERROR.GT.0) GO TO 999
          IF(.NOT.FOUNDA.OR.NUMNUC.LE.0) THEN
            WRITE(NERR,1120) AIRNAM(IA)
 1120       FORMAT(' Error reading ATO.  Data set not found for:',a20)
            NERROR = NERROR + 1
          ELSE
            write(NELS,1121) airnam(ia),numnuc
 1121       format(' ATO set found for: ',a20,' Numnuc =',i3)
          ENDIF
C
c--- Open temporary file for exposure pathway data for this parent and chain
c
          ICALL = 0
          NPOINTS = NAX1 * NAX2
          DO IN = 1,NUMNUC
            OPEN (UNIT=NATP,FILE='EXPOS.ATP',STATUS='UNKNOWN')
            DO IC = 1,9
              RNAMES(IC) = ' '
            ENDDO
            CALL ATODAT(RNAMES,NPRG,NPOINTS)
            IF(ONEPOINT) THEN
              NPOINTS = 1
            ENDIF
            IF(NERROR.GT.0) GO TO 999
c
c----- Test radionuclide name against master list for validity ---------------
c
            FOUND = .FALSE.
            C14 = .FALSE.
            H3  = .FALSE.
            H3EL = .FALSE.
            DO IC = 1,NUMCON
              IF(RNAMES(1).EQ.CONID(IC)) THEN
                FOUND = .TRUE.
                NIC = IC
                IF(SEQI(RNAMES(1),C14N,3)) C14 = .TRUE.
                IF(SEQI(RNAMES(1),H3N,2))  H3  = .TRUE.
                IF(SEQI(RNAMES(1),H3E,5))  H3EL  = .TRUE.
                GO TO 130
              ENDIF
            END DO
 130        IF(.NOT.FOUND) THEN
              WRITE(NELS,2012) RNAMES(1)
            ELSE
            DO ICM = 1,NPRG
              RNAMES(ICM) = RNAMES(ICM+1)
            ENDDO
              IF(IN.EQ.1) CALL EPFDSET(TPATH,EXPNAM,NPOINTS,NUMNUC)
c  changed index on CONID from IN to IC  BAN 28 Sept 2006
             CALL RMDGET(CONID(Ic),RNAMES,NPRG,XNAMES,ICROS,NOCHM,chnam)
              CALL SETELAW(XNAMES,nochm)
              CALL SETANION()
              NUCTOT = NDS(NIC) + 1
c
            CALL GETLEACH(SETDATA,NLINES,CHNAM,NOCHM)
c              CALL ENVLIB(TPATH) ! Get environmental transfer factors for chain
	CALL DBreadA(CHNAM,NOCHM)  ! REPLACEMENT FOR ENVLIB
              IF(NERROR.GT.0) GO TO 999
c
c----- Evaluate unit concentration factors for each air/deposition type ------
c
              DO ITYPE = 1,3   ! Loop on atmospheric concentration types
                APTYPE = ITYPE
                DO INC = 1,NUCTOT
                  CMEM = INC
                  CALL SETAIR(NUCTOT)
                  CALL ENV(TPATH)
                END DO
              END DO
c
c----- Read unit concentration file and calculate results for each point -----
c
              CALL EPFGRID(XNAMES,NUCTOT,ICALL)
              CLOSE(NATP,STATUS='DELETE')
c
            ENDIF    ! If on found radionuclide in master list
c
          END DO     ! Loop on number of constituents (parents)
c
 75       CLOSE(NATO)
        END DO    ! Loop on number of air sources to evaluate
      ENDIF
c
c
  999  CONTINUE
C
C----- Close files ----------------------------------------------------------
C
      CLOSE(NGID)
      CLOSE(NRMD)
      CLOSE(NEPF)
      IF(NERROR.LE.0) THEN
        CLOSE(NERR,STATUS='DELETE')
      ELSE
        CLOSE(NERR)
      ENDIF
      CLOSE(NELS)
C
C----- END OF MODULE ACUTE --------------------------------------------------
C
      END

