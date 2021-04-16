C   NEAR.FOR                        Version Date: 22-Aug-97               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     PROGRAM  NEAR                                          *
C                                                                            *
C  Program NEAR   controls calculation of exposure media concentrations      *
C  for one  location specified.  Input and output use files from the         *
C  Framework control program definition (.WCF, .ATO, .SCF, GID) with output  *
C  to file EPF.  The analysis is for the Near-field option of GENII          *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    22-Aug-97  (from EXPOS, GENII chronic exposure          *
C  Last Modified:    22-Aug-97      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: NEAR   main program
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
C   22-Aug-97  DLS  Initial programming started
c   4 JAN 99   BAN  REMOVED MAKDA2 Call
C   24 Oct 2001 BAN  C14 and H3 flags set in wrong place - all nuclides treated as them
C   13 May 2004 BAN  Commented out much of DLS file checking to allow FRAMES 1.5
c   5 Dec 2006  BAN  Capture branched chains
c   10 April 07 BAN  Check for duplicates in branched chains
C==== SUBROUTINE CALL ========================================================
C
      PROGRAM NEAR  
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'AFLAGS.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'EXINFO.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'FLUX.CMN'
C      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'PARMTR.PAR'
	INCLUDE 'RAD.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER GETINT, EXPMAX
      INTEGER SRCNUM
C*****INTEGER AQUNUM,RIVNUM,AIRNUM,SRCNUM
      CHARACTER*12 CONPID(100,9),RNAMES(9),LNAMES(9), XNAMES(9)
      CHARACTER*32 GETSTR, EXPNAM, EXPSRCNA
C*****CHARACTER*32 AQUNAM(10),RIVNAM(10),AIRNAM(10)
      CHARACTER*32 SRCNAME,SRCNAM(10),NONE
      CHARACTER*80 SETDATA(LINEMAX)
      CHARACTER*14 FUI, CSM
      CHARACTER*1 B
      CHARACTER*14 EXPTYPE(20),SOURCE,VADOSE     
      CHARACTER*14 GWC,SWC,AIRC,SOILC
      CHARACTER*12 CONID(100),chnam(9)
C*****CHARACTER*14 WTYPE
      LOGICAL FILTER, SEQI
      LOGICAL FOUND, FOUNDP, FOUNDA, FOUNDM
C
C---- Data Statements --------------------------------------------------------
C
      DATA FUI/'FUI           '/
	DATA CSM/'CSM           '/
      DATA B/' '/
      DATA   SWC/'Surface Water '/
      DATA   GWC/'Aquifer       '/
      DATA  AIRC/'Air           '/
      DATA SOILC/'Soil          '/
      DATA SOURCE/'Source        '/
      DATA VADOSE/'Vadose        '/
      DATA NONE/'None                            '/
C
C---- Initialize parameter values --------------------------------------------
C
C      DOSOILX = .FALSE.
      SURFS  = .FALSE.
      DEEPS  = .FALSE.
      WASTEP = .FALSE.
	nerror = 0
C
C---- Get file names ---------------------------------------------------------
C
      CALL GETNAM
      NGIDNM = INDEX(GIDFNM,B) - 1
      IF(NGIDNM.LE.0) NGIDNM = 128
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
C----- Open FILENAME.DAT for names of files read by GENII/ENV ----------------
C     BAN added LUN = 1 instead for Intel compiler
      CALL OPNFIL(3,0,LUN)
C      CALL OPNFIL(3,0,1)
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
      EXPNAM = GETSTR(SETDATA,NLINES,'ExpName       ',IS,IE,IZ,IZ,IZ,IZ)
      EXPX   =GETREAL(SETDATA,NLINES,'ExpX          ',IS,IE,IZ,IZ,IZ,IZ)
      EXPY   =GETREAL(SETDATA,NLINES,'ExpY          ',IS,IE,IZ,IZ,IZ,IZ)
      WRITE(NELS,102) EXPNAM,EXPX,EXPY
 102  FORMAT(' ExpName  = ',A32/' Coordinates (x,y) = ',1p2e10.2)
C
C----- Get exposure transport source types -----------------------------------
C      There are ExpTypeNum values for the current location
C
c      NEXPTYPE=GETINT(SETDATA,NLINES,'ExpTypeNum    ',IS,IE,IZ,IZ,IZ,IZ)
c      WRITE(NELS,106) NEXPTYPE
c 106  FORMAT(' ExpTypeNum  = ',I3)
c      IF(NEXPTYPE.LE.0) THEN
c        WRITE(NERR,2002) NEXPTYPE
c 2002   FORMAT(' Error in specification of number of exposure types '/
c     .         ' Value must be greater than zero.  Found: ',I3)
c        NERROR = NERROR + 1
c        GO TO 999
c      ENDIF
cC
cC----- Identify source types for the exposure location -----------------------
cC
c      NSOIL = 0
c      NL = 0
c      DO IT = 1,NEXPTYPE
c        EXPTYPE(IT)=GETSTR(SETDATA,NLINES,'ExpType       ',IS,IE,IT,IZ,
c     .                IZ,IZ)
c        EXPSRCNA = GETSTR(SETDATA,NLINES,'ExpSRCName    ',IS,IE,IT,IZ,
c     .                IZ,IZ)
cC
cC----- Check current exposure source type for "Aquifer" ----------------------
cC      If found, write error message and stop
cC
c        IF(SEQI(EXPTYPE(IT),GWC,14)) THEN   ! groundwater source
cC
cC----- The source type is "aquifer", which is not allowed for the GENII
cC      Near-Field exposure component
cC
c          WRITE(NERR,5000) GWC(1:7),GWC(1:7)
c 5000     FORMAT(' Error in source type for GENII Near Field',
c     .           ' Module.  ',A,' is not allowed.'/
c     .           ' Disconnect ',A,' glyph and rerun.')
c          NERROR = NERROR + 1
c          GO TO 999
cC
cC----- Check current exposure source type for "Surface Water" -----------------
cC
c        ELSEIF(SEQI(EXPTYPE(IT),SWC,14)) THEN ! surface water source
cC
cC----- The source type is "surface water", which is not allowed for the GENII
cC      Near-Field exposure component
cC
c          WRITE(NERR,5000) SWC(1:13),SWC(1:13)
c          NERROR = NERROR + 1
c          GO TO 999
cC
cC----- Check current exposure source type for "Air" --------------------------
cC
c        ELSEIF(SEQI(EXPTYPE(IT),AIRC,3)) THEN ! air transport source
cC
cC----- The source type is "air", which is not allowed for the GENII
cC      Near-Field exposure component
cC
c          WRITE(NERR,5000) AIRC(1:3),AIRC(1:3)
c          NERROR = NERROR + 1
c          GO TO 999
cC
cC----- Identify source for soil contamination --------------------------------
cc
c c        ELSEIF(SEQI(EXPTYPE(IT),SOILC,4).OR.
c     .          SEQI(EXPTYPE(IT),SOURCE,6).OR.
c     .          SEQI(EXPTYPE(IT),VADOSE,6)) THEN ! soil initial source
c           NSOIL = NSOIL + 1
cC
cC--- Match ExpSrcName(it) to "SrcName" to find the name of the
cC      source contributing to the initial soil cons.  Search the SCF 
cC       file to get data for this source
cC
c         SRCNUM=GETINT(SETDATA,NLINES,'SrcNum        ',IS,IZ,IZ,IZ,
c     .                IZ,IZ)
c         DO IL = 1,SRCNUM
c           SRCNAME=GETSTR(SETDATA,NLINES,'SrcName       ',IS,IL,IZ,IZ,
c     .                  IZ,IZ)
c           IF(SRCNAME.EQ.EXPSRCNA) THEN
c             NL = NL+1
c             SRCNAM(NL) = SRCNAME
c           ENDIF
c         END DO
c         IF(NL.LE.0) THEN
cC
cC---  Write error message that the Soil source was not found in the GID FUI info.
cC
c           WRITE(NERR,2011) EXPSRCNA, SRCNAME
c 2011     FORMAT(' Error in Soil source name.  Not found in GID/FUI.'/
c     .    '   ExpSrcName: ',a32,' Last SrcName: ',A32)
c           NERROR = NERROR + 1
c           GO TO 999
c         ENDIF
cC
c        ELSE
cC
cC----- The source type was not "Aquifer", "Surface Water", "Air", or "Soil".
cC      Write an error message
cC
c          WRITE(NERR,2003) EXPTYPE(IT)
c 2003     FORMAT(' Error in GID/FUI specification of exposure source '
c     . 'type.'/'    Not Aquifer, Surface Water, Air, or Soil. ',
c     . ' Found: ',A4)
c          NERROR = NERROR + 1
c        ENDIF
c      END DO   ! End Do on IT, number of source types
c      IF(NERROR.GT.0) GO TO 999
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
c The following 2 lines are in the wrong place BAN 10/24/2001
c		if(conid(icin) .eq. 'C14')C14 = .TRUE.
c	    if(conid(icin) .eq. 'H3')H3 = .TRUE.
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
C
C   ---  Capture branched chains here
C
     
            Do ip = 1,nds(icin)
              CONPID(ICIN,IP+nds(icin)) = GETSTR(SETDATA,NLINES,'SSCASID
     .       ',                      IS,IC,IP,IZ,IZ,IZ)  ! Name of progeny
	  if(CONPID(ICIN,IP+nds(icin)) .ne. 'NOT FOUND')then
ccc   check for duplicates, don't add if already in list
ccc   BAN 10 April 2007
              Itally = 1
	        do ip2 = 1, nds(icin)
	        if(conpid(icin,IP+nds(icin)) .eq. RNAMES(ip2))Itally = 0
	        end do
	        if(Itally .eq. 1) then
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
          CALL RMDGET(CONID(ICIN),RNAMES,NDS(ICIN),LNAMES,ICROS,nochm,
     .                 chnam)
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
      WRITE(NELS,3001)BEFORE,NTKEND
 3001 FORMAT(' Timing parameters (years):'/
     .'  BEFORE, time zero to start of intake           ',F5.1/
     .'  NTKEND, duration of intake period              ',F5.1)
      IF(NTKEND.LT.1.) NTKEND = 1.   ! Fix exposure period to a minimum of 1 year.
C
C----- Call EPFHEAD to write heading information to the EPF PDCF file --------
c
c   ban 27aUG 2012 SET nl = 0
      NL = 0
       CALL EPFHEAD(NL,SRCNAM)
C
C----- Get soil concentration data for each source ---------------------------
C
c      IF(DOSOILX)  THEN
c
c        DO IL = 1,NSOIL
          NUMNUC = 0
C
C-------- Open run Soil Concentration File (.SCF) ---------------------------
C
       OPEN (UNIT=NSCF,FILE=GIDFNM(1:NGIDNM)//'.SCF',STATUS='OLD')
C
c----- Read beginning of SCF file and position to desired data set -----------
c      First find surface soil data, if any
C
       SRCNAME = SURSOIL
       IF(SRCNAME.NE.NONE) THEN
         CALL MARKIN(SRCNAME,FOUNDM)
         IF(.NOT.FOUNDM) GO TO 35
         SURFS = .TRUE.
         ITYPE = 1
C         CALL SCFIN(ITYPE,SRCNAME,NUMNUC)
         CALL SCFIN(ITYPE,EXPNAM,NUMNUC)
         ns = lenword(srcnamE,32)
         IF(NUMNUC.LE.0) THEN
           WRITE(NERR,1116) SRCNAME(1:ns)
 1116      FORMAT(' Error reading SCF. Surface soil data set not found:'
     .     ,1x,A)
           NERROR = NERROR + 1
           GO TO 999
         ELSE
           write(NELS,1117) SRCNAME(1:ns),numnuc
 1117      FORMAT(' SCF surface soil set found for: ',a,' Number of ',
     .     'nuclides:',i3)
           ISOIL = 1
           CALL SCFDAT(CONID,NUMCON,NUMNUC,ISOIL)
           IF(NERROR.GT.0) GO TO 999
         ENDIF
       ENDIF  ! End if on SURF
C
C----- Next find deep soil data, if any
C
C       IF(DEEPS) THEN

 35      SRCNAME = DEPSOIL
       IF(SRCNAME.NE.NONE) THEN
         CALL MARKIN(SRCNAME,FOUNDM)
         IF(.NOT.FOUNDM) GO TO 45
         DEEPS = .TRUE.
         ITYPE = 2
         CALL SCFIN(ITYPE,SRCNAME,NUMNUC)
         ns = lenword(srcname,32)
         IF(NUMNUC.LE.0) THEN
           WRITE(NERR,1118) SRCNAME(1:ns)
 1118      FORMAT(' Error reading SCF.  Deep soil data set not found: '
     .     ,A)
           NERROR = NERROR + 1
           GO TO 999
         ELSE
           write(NELS,1119) SRCNAME(1:ns),numnuc
 1119      FORMAT(' SCF deep soil set found for: ',a,' Number of ',
     .     'nuclides:',i3)
           ISOIL = 2
           CALL SCFDAT(CONID,NUMCON,NUMNUC,ISOIL)
           IF(NERROR.GT.0) GO TO 999
         ENDIF
       ENDIF  ! End if
C
C----- Next find waste package data, if any ----------------------------------
C
C       IF(WASTEP) THEN
 45      SRCNAME = WASTPAK
       IF(SRCNAME.NE.NONE) THEN
         CALL MARKIN(SRCNAME,FOUNDM)
         IF(.NOT.FOUNDM) GO TO 55
         WASTEP = .TRUE.
         ITYPE = 3
         CALL SCFIN(ITYPE,SRCNAME,NUMNUC)
         ns = lenword(srcname,32)
         IF(NUMNUC.LE.0) THEN
           WRITE(NERR,1120) SRCNAME(1:ns)
 1120      FORMAT(' Error reading SCF.  Waste package data set not ',
     .      'found: ',A)
           NERROR = NERROR + 1
           GO TO 999
         ELSE
           write(NELS,1121) SRCNAME(1:ns),numnuc
 1121      FORMAT(' SCF Waste package set found for: ',a,' Number of ',
     .     'nuclides:',i3)
           ISOIL = 3
           CALL SCFDAT(CONID,NUMCON,NUMNUC,ISOIL)
           IF(NERROR.GT.0) GO TO 999
         ENDIF
      ENDIF  ! End if on WASTEP
C
  55  IF(.NOT.SURFS.AND..NOT.DEEPS.AND..NOT.WASTEP) THEN
        WRITE(NERR,1122) SURSOIL,DEPSOIL,WASTPAK
 1122   FORMAT(' Error in source specification: none recognized'/
     .         ' Surface Soil name:  ',a/
     .         ' Deep Soil name:     ',a/
     .         ' Waste Package name: ',a)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Write data set heading to EPF file for this data set ------------------
C
           CALL EPFDSET(EXPNAM,NUMCON)
C
C---- Do analysis for each parent/chain in GID master list that is a radionuclide 
C
      DO IN = 1,NUMCON
C
		C14 = .FALSE.
		H3 = .FALSE.
		if(conid(in) .eq. 'C14')C14 = .TRUE.
	    if(conid(in) .eq. 'H3')H3 = .TRUE.
         NIC = IN
         DO IC = 1,9
            RNAMES(IC) = ' '
            RNAMES(IC) = CONPID(NIC,IC)
         ENDDO
         NPRG = NDS(NIC)
         CALL RMDGET(CONID(NIC),RNAMES,NPRG,XNAMES,ICROS,nochm,chnam)
         CALL SETELAW(CHNAM,NDS(NIC))
         NUCTOT = NDS(NIC) + 1
         CALL GETLEACH(SETDATA,NLINES,CHNAM,NOCHM)
C         CALL ENVLIB(nochm)   ! Get environmental transfer factors for chain
	CALL DBreadN(CHNAM, NOCHM)    !REPLACEMENT FOR ENVLIB
         IF(NERROR.GT.0) GO TO 999
         CALL ENV(NIC) !TO DO ANALYSIS FOR THIS PARENT/SOIL SOURCE TERM
         CALL EPFDAT(nic,CHNAM,NUCTOT) ! Write results for chain to EPF
c         ENDIF
      END DO
 65       CLOSE(NSCF)
c        END DO
c      ENDIF
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
C----- END OF MODULE NEAR  --------------------------------------------------
C
      END
