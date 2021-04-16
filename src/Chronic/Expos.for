C    EXPOS.FOR                      Version Date: 22-Sep-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     PROGRAM  EXPOS                                         *
C                                                                            *
C  Program EXPOS  controls calculation of exposure media concentrations      *
C  for one  location specified.  Input and output use files from the         *
C  Framework control program definition (.WCF, .ATO, .SRC, GID) with output  *
C  to file EPF                                                               *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    22-Nov-96                                               *
C  Last Modified:    22-Sep-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: EXPOS  main program
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
C     SWOUT      U      REAL   LOCAL     Array of summed water concentrations
C     TIME(100) S/U     REAL   LOCAL     Time point array from WFF file
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ----------------------------------------------------------
C   22-Nov-96  DLS  Initial programming started
C   14-May-97  DLS  Start of revision for FRAMEWORK testing
C   22-May-97  DLS  Revised atmospheric pathway calls to ENV/EPFGRID
C   29-May-97  DLS  Redirected write statements to files, not the screen.
C   22-Sep-97  DLS  Added testing for input chain members not being in RMDLIB
c   4-jUNE-2001 BAN skip non-radionuclides in water processing
C   24-oCT-2001 BAN removed references to LOIC - not used in this module
C   16-Spe-2004 BAN Allow FRAMES 1.5 User Defined sources
c   18 Jun 2006 BAN Cut down the specificity of User Defined names to allow 1.7 DES files
c   10 April 2007  BAN  Check for duplicates in branch chain decay  
c   26 Feb 2008 BAN Repair connectivity for multiple User Defined inputs
c   21 Aug 2008 BAN Again repair connectivity - don't need loop, though
C==== SUBROUTINE CALL ========================================================
C
      PROGRAM EXPOS 
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
      INTEGER GETINT, EXPMAX, TPATH
      INTEGER AQUNUM,RIVNUM,AIRNUM
c      INTEGER AQUNUM,RIVNUM,AIRNUM,SRCNUM
      CHARACTER*12 CONPID(100,LENCHAIN),RNAMES(LENCHAIN),
     .             LNAMES(LENCHAIN), XNAMES(LENCHAIN)
      CHARACTER*32 GETSTR, EXPNAM, EXPSRCNA
      CHARACTER*32 AQUNAM(10),RIVNAM(10),AIRNAM(10)
      CHARACTER*32 AQUNAME,RIVNAME,AIRNAME
      CHARACTER*80 SETDATA(LINEMAX)
      CHARACTER*14 FUI
      CHARACTER*1 B
      CHARACTER*14 EXPTYPE(20),GWC,SWC,AIRC,USRNAM
      CHARACTER*12 CONID(100),chnam(LENCHAIN)
      CHARACTER*14 WTYPE
      CHARACTER*3 C14N,H3N
	CHARACTER*5 H3E
	CHARACTER*24 USRI, USRAIR, USRSW, USRGW, USRD
      LOGICAL FILTER, SEQI, iskip
      LOGICAL FOUNDA, FOUNDM
C
C---- Data Statements --------------------------------------------------------
C
      DATA C14N/'C14'/, H3N/'H3 '/, H3E/'H3EL'/
      DATA FUI/'FUI           '/
      DATA B/' '/
      DATA   SWC/'Surface Water '/
      DATA   GWC/'Aquifer       '/
      DATA  AIRC/'Air           '/
      DATA   USRD/'User Defined            '/
	DATA USRAIR/'ATO Air Module  '/
	DATA  USRGW/'WCF Aquifer Module      '/
	DATA  USRSW/'WCF Surface Water Module'/
C
C---- Initialize parameter values --------------------------------------------
C
      DOGWX   = .FALSE.
      DOSWX   = .FALSE.
      AIR     = .FALSE.
      DOAIRX  = .FALSE.
c      DOSOILX = .FALSE.
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
C  BAN 29 Aug 2012 added LUN = 1 to avoid Intel compilation error
      LUN = 1
      CALL OPNFIL(3,0,LUN)
c      CALL OPNFIL(3,0,1)
	CALL OPNFIL(1,0,NRMD)
C
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
	WRITE(NELS,112) SITNAM, conName
 112  FORMAT(' SiteName = ',A32, 'Database name =', A32)
c
c      WRITE(NELS,101) SITNAM
c 101  FORMAT(' SiteName = ',A32)
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
      EXPXO= EXPX * 1000.
      EXPYO= EXPY * 1000.
      WRITE(NELS,102) EXPNAM,EXPXO,EXPYO
 102  FORMAT(' ExpName  = ',A32/' Coordinates (x,y) (m) = ',1p2e10.2)
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
c      NSOIL = 0
      DO IT = 1,NEXPTYPE
        EXPTYPE(IT)=GETSTR(SETDATA,NLINES,'ExpType       ',IS,IE,IT,IZ,
     .                IZ,IZ)
        EXPSRCNA = GETSTR(SETDATA,NLINES,'ExpSRCName    ',IS,IE,IT,IZ,
     .                IZ,IZ)
C
C----- Check current exposure source type for "Aquifer" ----------------------
C
        IF(SEQI(EXPTYPE(IT),GWC,14)) THEN   ! groundwater source
C        IF(EXPTYPE(IT).EQ.GWC) THEN   ! groundwater source
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
C        ELSEIF(EXPTYPE(IT).EQ.AIRC) THEN ! air transport source
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
C----- Identify User Defined Inputs ------------------------
c
        ELSEIF (SEQI(EXPTYPE(IT),USRD,12)) THEN ! USER DEFINED source
C--- COULD STILL BE EITHER AIR, SURFACE WATER, OR GROUNDWATER
C
        USRNUM=GETINT(SETDATA,NLINES,'UsrNum        ',IS,IZ,IZ,IZ,IZ,IZ)
c	  NU=0
cc  don't need this loop, it is being done in the IT loop already
c        IR = IT
cc	  DO IR = 1,USRNUM
c        USRI= GETSTR(SETDATA, NLINES,'UsrModel      ',IS,IR,IZ,IZ,IZ,IZ) 
cc	    
c	  IF (SEQI(USRI,USRAIR,8)) THEN ! air known source  	    
c	    NAIR = NAIR+1
cC      match names
c          USRNAM=GETSTR(SETDATA,NLINES,'UsrName       ',IS,IR,IZ,IZ,IZ,
c     .                   IZ)
c	    AIRNAM(NAIR) = USRNAM
c	    NA=NA+1
c	     
c	  ELSEIF (SEQI(USRI,USRSW,8))THEN ! surface water known source  
c	    NSW = NSW+1
C      match names
c          USRNAM=GETSTR(SETDATA,NLINES,'UsrName       ',IS,IR,IZ,IZ,IZ,
c     .                   IZ)
c	    RIVNAM(NSW) = USRNAM
c	    NS=NS+1
cc	  
c	  ELSEIF (SEQI(USRI,USRGW,8))THEN ! groundwater known source
c          NGW = NGW+1
Cc      match names
c          USRNAM=GETSTR(SETDATA,NLINES,'UsrName       ',IS,IR,IZ,IZ,IZ,
c     .                   IZ)
c	    AQUNAM(NGW) = USRNAM
c	    NG=NG+1
c        ENDIF
cc	  ENDDO ! ON UsrNum
Cc
        do ir = 1, UsrNum
	   USRI = GETSTR(SETDATA,NLINES,'UsrModel      ',IS,IR,IZ,IZ,IZ,IZ)
	   USRNAM = GETSTR(SETDATA,NLINES,'USRNAME        ',IS,IR,IZ,IZ,IZ,IZ)
         IF (SEQI(USRNAM,EXPSRCNA,5)) THEN
	    IF (SEQI(USRI,USRAIR,8)) THEN
	     NAIR = NAIR + 1
	     AIRNAM(NAIR) = USRNAM
	     NA = NA + 1
	    ELSEIF (SEQI(USRI,USRSW,8)) THEN
	     NSW = NSW + 1
	     RIVNAM(NSW) = USRNAM
	     NS = NS + 1
	    ELSEIF (SEQI(USRI,USRGW,8)) THEN
	     NGW = NGW + 1
	     AQUNAM(NSW) = USRNAM
	     NG = NG + 1
	    ENDIF
	   ENDIF
	  END DO  ! ON UsrNum
c
        ELSE
C
C----- The source type was not "Aquifer", "Surface Water", "Air", or "USER DEFINED".
C      Write an error message
C
          WRITE(NERR,2003) EXPTYPE(IT)
 2003     FORMAT(' Error in GID/FUI specification of exposure source '
     . 'type.'/'    Must be Aquifer, Surface Water, Air,or User defined'
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
ccc   BAN 10 APril 2007
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
      CALL REDCAS(SETDATA,NLINES)
c
c----- Test timing parameters ------------------------------------------------
c
C      WRITE(NELS,3001)BEFORE,LOIC,BEFIRR,BEFAIR,NTKEND,RELEND
      WRITE(NELS,3001)BEFORE,BEFIRR,BEFAIR,NTKEND,RELEND
 3001 FORMAT(' Timing parameters (years):'/
     .'  BEFORE, time zero to start of intake           ',F5.1/
C     .'  LOIC, time of loss of institutional control    ',I5/
     .'  BEFIRR, years of water irrigation before intake',I5/
     .'  BEFAIR, years of air deposition before intake  ',I5/
     .'  NTKEND, duration of intake period              ',F5.1/
     .'  RELEND, time from zero to end of release       ',F5.1)
C      IF(BEFORE.LT.float(LOIC)) THEN
C        WRITE(NERR,2005) BEFORE,LOIC
C 2005   FORMAT(' Error in specification of loss of institutional ',
C     . 'control time.'/' ---   BEFORE = ',1pe10.2,' LOIC = ',i3)
C        WRITE(NERR,3001)BEFORE,LOIC,BEFIRR,BEFAIR,NTKEND,RELEND   
C        NERROR = NERROR + 1
C        GO TO 999
c      ENDIF
c      IF(BEFORE.LT.float(BEFIRR).OR.LOIC.LT.BEFIRR) THEN
      IF(BEFORE.LT.float(BEFIRR)) THEN
C        WRITE(NERR,2006) BEFORE,BEFIRR,LOIC
        WRITE(NERR,2006) BEFORE,BEFIRR
 2006   FORMAT(' Error in specification of prior irrigation period. '/
C     .  ' ---   BEFORE = ',1pe10.2,' BEFIRR = ',i3,' LOIC = ',i3)
     .   ' ---   BEFORE = ',1pe10.2,' BEFIRR = ',i3)
C        WRITE(NELS,3001)BEFORE,LOIC,BEFIRR,BEFAIR,NTKEND,RELEND
        WRITE(NELS,3001)BEFORE,BEFIRR,BEFAIR,NTKEND,RELEND
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C      IF(BEFORE.LT.float(BEFAIR).OR.LOIC.LT.BEFAIR) THEN
      IF(BEFORE.LT.float(BEFAIR)) THEN
C        WRITE(NERR,2007) BEFORE,BEFAIR,LOIC
        WRITE(NERR,2007) BEFORE,BEFAIR
 2007   FORMAT(' Error in specification of prior air deposition ',
     .  'period.'/' ---   BEFORE = ',1pe10.2,' BEFAIR = ',i3)
C     .  ' LOIC = ',i3)
C        WRITE(NELS,3001)BEFORE,LOIC,BEFIRR,BEFAIR,NTKEND,RELEND
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
c----- Open output file and write heading ------------------------------------
c
c      CALL OPNFIL(2,1,13)
c      LUN = 21
c      CALL OPNFIL(2,0,LUN)
c      CALL HEADNG(LUN)
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
c      IF(NSOIL.GT.0.AND.NL.GT.0) DOSOILX = .TRUE.
c      NSOIL = NL
      write(NELS,1111) ng,ns,na,dogwx,doswx,
     .  doairx
 1111 format(' Results of source search:   GW   SW  Air '/
     .       '    Number found for each:',3I5/
     .       '    Logical flag settings:',3L5)
C
C----- Call EPFHEAD to write heading information to the EPF PDCF file --------
c
       CALL EPFHEAD(NG,NS,NA,AQUNAM,RIVNAM,AIRNAM)
C
C----- Get water concentration data (aquifer or river) each source -----------
C
       IF(DOGWX)  THEN
c
c----- Determine if groundwater is used for prior irrigation -----------------
c
        DOPRIOR = .FALSE.
        IF(BEFIRR.GT.0) THEN
          IF(TFOOD) THEN
            DO ITF = 1,NTF
              IF(TFD(ITF).AND.IRRST(ITF).EQ.1) DOPRIOR = .TRUE.
            END DO
          END IF
          IF(ANFOOD) THEN
C           DO IAN = 1,NAN+2
              IF(ANF(1).AND.(IRRSA(1).EQ.1.OR.IRRSA(5).
     .         EQ.1)) DOPRIOR = .TRUE.
              IF(ANF(3).AND.(IRRSA(3).EQ.1.OR.IRRSA(6).
     .         EQ.1))DOPRIOR = .TRUE.
              IF(ANF(2).AND.IRRSA(2).EQ.1) DOPRIOR = .TRUE.
              IF(ANF(4).AND.IRRSA(4).EQ.1) DOPRIOR = .TRUE.
C              IF(ANF(IAN).AND.IRRSA(IAN).EQ.1) DOPRIOR = .TRUE.
C           END DO
          END IF
          IF(IRRSR.EQ.1) DOPRIOR = .TRUE.
        ENDIF
        WTYPE = GWC
        TPATH = 1
c
c----- Evaluate each groundwater source term ---------------------------------
c
        DO IG = 1,NGW
          NUMNUC = 0
C
C-------- Open run Water Concentration File (.WCF) ---------------------------
C
       OPEN (UNIT=NWCF,FILE=GIDFNM(1:NGIDNM)//'.WCF',STATUS='UNKNOWN')
c
c----- read beginning of WCF file and position to desired data set -----------
c
       CALL MARKIN(TPATH,AQUNAM(IG),FOUNDM)
       IF(.NOT.FOUNDM) GO TO 55
          WRITE(NELS,1112) ig,aqunam(ig)
 1112     format(' Call WCFIN for GW set',i2,' - ',a32)
          CALL WCFIN(WTYPE,EXPNAM,NUMNUC)
c
c----- Test number of radionuclide parents defined for WCF file --------------
c       Must be greater than 0
c
          IF(NUMNUC.LE.0) THEN
            WRITE(NERR,1116) WTYPE,AQUNAM(IG)
 1116       FORMAT(' Error reading WCF.  Data set not found for:',a14,
     .             ' for ',A20)
            NERROR = NERROR + 1
            GO TO 999
          ELSE
            write(NELS,1117) aqunam(ig),numnuc
 1117       format(' WCF set found for: ',a20,' Numnuc =',i3)
          ENDIF
C
C----- Write data set heading to EPF file for this data set ------------------
c
          CALL EPFDSET(TPATH,EXPNAM,NPOINTS,NUMNUC)
c
c---- Begin loop on radionuclides in WCF file for groundwater analysis -------
c
          DO IN = 1,NUMNUC
            INUC = IN
            DO IC = 1,9
              RNAMES(IC) = ' '
            ENDDO
            C14 = .FALSE.
            H3  = .FALSE.
            CALL WCFDAT(CONID,CONPID,NUMCON,NDS,RNAMES,NPRG,NIC,ISKIP)
C
           IF(ISKIP .EQ. .TRUE.) GO TO 456
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
C           NUCTOT = NDS(NIC) + 1
            CALL GETLEACH(SETDATA,NLINES,CHNAM,NOCHM)
C            CALL ENVLIB(nochm)   ! Get environmental transfer factors for chain
            CALL DBreadC(CHNAM,NOCHM)  ! replacement for ENVLIB
            IF(NERROR.GT.0) GO TO 999
            CALL ENV(TPATH) !TO DO ANALYSIS FOR THIS GROUND WATER SOURCE TERM
            ENDIF
            CALL EPFDAT(INUC,RNAMES,NUCTOT,TPATH,NPOINTS)  ! Write results for chain to EPF
  456      CONTINUE
          END DO
 55       CLOSE(NWCF)
        END DO
      ENDIF
C
C----- Get surface water concentration data for each source ------------------
C
      IF(DOSWX)  THEN
c
c----- Determine if surface water is used for prior irrigation ---------------
c
        DOPRIOR = .FALSE.
        IF(BEFIRR.GT.0) THEN
          IF(TFOOD) THEN
            DO ITF = 1,NTF
              IF(TFD(ITF).AND.IRRST(ITF).EQ.2) DOPRIOR = .TRUE.
            END DO
          END IF
          IF(ANFOOD) THEN
c            DO IAN = 1,NAN+2
              IF(ANF(1).AND.(IRRSA(1).EQ.2.OR.IRRSA(5).
     .         EQ.2)) DOPRIOR = .TRUE.
              IF(ANF(3).AND.(IRRSA(3).EQ.2.OR.IRRSA(6).
     .         EQ.2))DOPRIOR = .TRUE.
              IF(ANF(2).AND.IRRSA(2).EQ.2) DOPRIOR = .TRUE.
              IF(ANF(4).AND.IRRSA(4).EQ.2) DOPRIOR = .TRUE.
c              IF(ANF(IAN).AND.IRRSA(IAN).EQ.2) DOPRIOR = .TRUE.
c            END DO
          END IF
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
          CALL WCFIN(WTYPE,EXPNAM,NUMNUC)
          IF(NUMNUC.LE.0) THEN
            WRITE(NERR,1116) WTYPE,RIVNAM(IS)
            NERROR = NERROR + 1
          ELSE
            write(NELS,1117) rivnam(is),numnuc
          ENDIF
C
C----- Write data set heading to EPF file for this data set ------------------
c
          CALL EPFDSET(TPATH,EXPNAM,NPOINTS,NUMNUC)
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
C            DO ICM = 1,NPRG
C              RNAMES(ICM) = RNAMES(ICM+1)
C            ENDDO
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
C           NUCTOT = NDS(NIC) + 1
            CALL GETLEACH(SETDATA,NLINES,CHNAM,NOCHM)
C            CALL ENVLIB(nochm)   ! Get environmental transfer factors for chain
            CALL DBreadC(CHNAM,NOCHM)  ! replacement for ENVLIB
		  IF(NERROR.GT.0) GO TO 999
            CALL ENV(TPATH) !TO DO ANALYSIS FOR THIS SURFACE WATER SOURCE TERM
		  ENDIF
            CALL EPFDAT(INUC,RNAMES,NUCTOT,TPATH,NPOINTS) ! Write results for chain to EPF
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
            OPEN (UNIT=NATP,FILE='EXPOS.ATP',STATUS='UNKNOWN')
            C14 = .FALSE.
            H3  = .FALSE.
            H3EL = .FALSE.
            CALL ATODAT(CHNAM,NOCHM,NPOINTS)
C            IF(NOCHM.GT.0) THEN
              IF(ONEPOINT) THEN
                NPOINTS = 1
              ENDIF
            IF(SEQI(CHNAM(1),C14N,3)) C14 = .TRUE.
            IF(SEQI(CHNAM(1),H3N,2))  H3  = .TRUE.
            IF(SEQI(CHNAM(1),H3E,5))  H3EL  = .TRUE.
            IF(IN.EQ.1) CALL EPFDSET(TPATH,EXPNAM,NPOINTS,NUMNUC)
C           CALL RMDGET(CONID(IN),RNAMES,NPRG,XNAMES,ICROS,
C    .                  nochm,chnam)
            CALL SETELAW(CHNAM,NOCHM)
            CALL SETANION()
            NUCTOT = NOCHM
C           NUCTOT = NDS(IN) + 1
            CALL GETLEACH(SETDATA,NLINES,CHNAM,NOCHM)
c            CALL ENVLIB(nochm)   ! Get environmental transfer factors for chain
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
              CALL EPFGRID(CHNAM,NOCHM,ICALL)
C              CLOSE(NATP,STATUS='DELETE')
C            ELSE

C            ENDIF
          END DO     ! Loop on number of constituents (parents)
 75       CLOSE(NATO)
        END DO    ! Loop on number of air sources to evaluate
      ENDIF
C
C----- Soil contamination is not a source for GENII chronic exposure ---------
C
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
C----- END OF MODULE EXPOS --------------------------------------------------
C
      END

