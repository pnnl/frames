C    SWATER.FOR                      Version Date: 08-Jan-98               
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
C*****************************************************************************
C                                                                            *
C                     PROGRAME SWATER                                        *
C                                                                            *
C  Program SWATER controls calculation of surface water concentrations       *
C  for each location specified.  Input and output use files from the         *
C  Framework control program definition (.WFF and .WCF)                      *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    30-Oct-96                                               *
C  Last Modified:    08-Jan-98      DLS                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: SWATER main program
C     Called by: None
C     Calls: SWCAL, HEADIN, HEADOUT, GETDATA
C     Common blocks referenced: OPT, SWPAR, DEVICE, SWINFO, FNAMES
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
C     NGID       U      INT    DEVICE    Logical unit for GID file
C     NRIVER     U      INT    FNAMES    Index number of river to be used
C     NSITE      U      INT    FNAMES    Index number of site to be used
C     NUMEXP    S/U     INT    SWINFO    Number of exposure locations
C     NWCF       U      INT    DEVICE    Logical unit for WCF file
C     NWFF       U      INT    DEVICE    Logical unit for WFF file
C     REL(100)  S/U     REAL   LOCAL     Release rate array from WFF file
C     RIVNAM     U      CHR    SWINFO    Name of river for current run
C     RUNFNM     U      CHR    FNAMES    File name for run files and data set
C                                        name in GID
C     SETDATA    S      CHR    GID file  Storage array for GID data set 
C     SWOUT      U      REAL   LOCAL     Array of summed water concentrations
C     TIME(100) S/U     REAL   LOCAL     Time point array from WFF file
C==== Modification History ===================================================
C     Date     Who  Modification Description
C   ---------  ---  ------------------------------------------------------
C   30-Oct-96  DLS  Initial programming started
C   07-Nov-96  DLS  Completion of initial testing
C   30-Apr-97  DLS  Modify WFF read for new file format.
C   13-May-97  DLS  Adjusted word lengths for compatibility
C   29-May-97  DLS  Added test of NTIMES to skip processing when no input data
C   09-Sep-97  DLS  Revised WFF read to include number of fluxes, NFLUX
C   08-Jan-97  DLS  Revised for addition of flow dilution model
c   6 Dec 2006 BAN  Capture branched chains
C   29 Dec 2006 BAN Allow WFF files from Aquifers
c   10 April 2007 BAN Check for duplicates in branched chain decay
C==== PROGRAM STATEMENT ======================================================
C
      PROGRAM SWATER
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'SWCON.CMN'
      INCLUDE 'SWINFO.CMN'
      INCLUDE 'SWPAR.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER GETINT, EXPNUM
c      INTEGER GETINT, EXPNUM, IEX(10)
      INTEGER LUN
      CHARACTER*12 CONPID(100,9),PID, conpid2(100,9)
      CHARACTER*32 GETSTR
c      CHARACTER*32 GETSTR, EXSRCIN
      CHARACTER*80 SETDATA(LINEMAX)
      CHARACTER*14 SURWTR
      CHARACTER*128 FUI
      CHARACTER*1 B
      CHARACTER*8 XU,YU,ZU,WFLXU,TU,FLXU,OVRLND
	CHARACTER*7 AQUIFER
c      CHARACTER*14 EXPTYP,SW
      CHARACTER*12 CONID(100),CIDIN
      CHARACTER*20 CNAMIN,PNAM
      REAL SWOUT(10,20,9)
      REAL TIME(100),REL(100), T0
      LOGICAL SKIP             ! skip flag for media in .WFF
      LOGICAL RFOUND ! flag to indicate the marker was found in WFF file.
      LOGICAL SEQI
C
C---- Data Statements --------------------------------------------------------
C
      DATA T0/0.0/
      DATA FUI/'FUI           '/
      DATA B/' '/
c      DATA SW/'Surface Water '/
      DATA SURWTR/'Surface water '/
	DATA OVRLND/'Overland'/
	DATA AQUIFER/'Aquifer'/
C
C---- Initialize parameter values --------------------------------------------
C
      DEBUG = .true.
      NERROR = 0
      DO I = 1,10
        DO J = 1,20
          DO K = 1,9
            SWOUT(I,J,K) = 0.0
          END DO
        END DO
      END DO
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
       LUN = 1
C       CALL OPNFIL(3,0,1)
       CALL OPNFIL(3,0,LUN)
       CALL OPNFIL(1,0,NRMD)
       REWIND (NRMD)
C
C-------- Run Water Flux File (.WFF)
C
      OPEN (UNIT=NWFF,FILE=GIDFNM(1:NGIDNM)//'.WFF',STATUS='OLD')
C
C-------- Run Water Concentration File (.WCF)
C
      OPEN (UNIT=NWCF,FILE=RUNFNM(1:NRUNAM)//'.WCF',STATUS='UNKNOWN')
C
C-------- Run Error Message file (.ERR)
C
      OPEN (UNIT=NERR,FILE=RUNFNM(1:NRUNAM)//'.ERR',STATUS='UNKNOWN')
C
C-------- Run Output listing file (.SLS)
C
      OPEN (UNIT=NSLS,FILE=RUNFNM(1:NRUNAM)//'.SLS',STATUS='UNKNOWN')
      WRITE(NSLS,100) GIDFNM,RUNFNM,NSITE,NRIVER,GLFNAME
 100  FORMAT(' GIDFNM =',A48/' RUNFNM =',A48/' NSITE =',I3/' NRIVER =',
     .       I3/' GLFNAME =',A48)
C
C----- Start of Analysis
C  
C----- Get Framework User Interface data from GID file -----------------------     
C
      CALL GETSET(NGID,NERR,NERROR,FUI,NLINES,SETDATA) 
C
C----- Was the FUI data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2000) NLINES
        NERROR = NERROR + 1
 2000   FORMAT(' The FUI data set was not found, NLINES =',I5)
        GO TO 999
      ENDIF
C
C----- Get name of site ------------------------------------------------------
C
      IS = NSITE                 ! Number of sites from ID file
      SITNAM = GETSTR(SETDATA,NLINES,'SiteName      ',IS,IZ,IZ,IZ,IZ,IZ)
      WRITE(NSLS,101) SITNAM
 101  FORMAT(' SiteName = ',A32)
C
C----- Get water body (river) number and name of water body ------------------
C
      IR = NRIVER
      RIVNAM = GETSTR(SETDATA,NLINES,'RivName       ',IS,IR,
     .                  IZ,IZ,IZ,IZ)
      WRITE(NSLS,105) NRIVER,RIVNAM
 105  FORMAT(' NRIVER and RIVNAM = ',I5,2x,A32)
C
C----- Get contamination source names for selected river ---------------------
C
      NRIVSRC=GETINT(SETDATA,NLINES,'RivSrcNum     ',IS,IR,IZ,IZ,IZ,IZ)
      IF(DEBUG) WRITE(NSLS,115) NRIVSRC
 115  FORMAT(' Number of River Sources is ',I3)
      DO IRS = 1,NRIVSRC
        RIVSRC(IRS) = GETSTR(SETDATA,NLINES,'RivSrcName    ',IS,IR,
     .                 IRS,IZ,IZ,IZ)
      END DO
      IF(DEBUG) write(NSLS,116) (RIVSRC(I),I=1,NRIVSRC)
 116  format(' River source names: ',A32/(21x,A32))
C
C----- Get number of constituents included in the run --------------------------
C
      NUMCON = GETINT(SETDATA,NLINES,'NumCon        ',IS,IZ,IZ,IZ,IZ,IZ)
      IF(DEBUG) WRITE(NSLS,104) NUMCON
 104  FORMAT(' Number of constituents is ',I3)
C
C----- Get names of each constituent, number of progeny, and progeny names
C
      DO IC = 1,NUMCON
        CONID(IC) = GETSTR(SETDATA,NLINES,'FSCASID       ',IS,IC,
     .                   IZ,IZ,IZ,IZ)            ! Name of parent
        NDS(IC) = GETINT(SETDATA,NLINES,'NDS           ',IS,IC,IZ,
     .                 IZ,IZ,IZ)                 ! Number of progeny
        IF(DEBUG) WRITE(NSLS,102) CONID(IC),NDS(IC)
 102    FORMAT(' Parent name ',A12,' with progeny # ',I4)
        IF(NDS(IC).GT.0) THEN
          DO IP = 1,NDS(IC)
            CONPID(IC,IP) = GETSTR(SETDATA,NLINES,'FSCASID       ',
     .                             IS,IC,IP,IZ,IZ,IZ)  ! Name of progeny
          END DO
C
C   ---  Capture branched chains here: BAN 6 Dec 2006
C
            iadd = 1
            Do ip = 1,nds(ic)
              CONPID2(IC,IP) = GETSTR(SETDATA,NLINES,'SSCASID
     .     ',                      IS,IC,IP,IZ,IZ,IZ)  ! Name of progeny
	  if(CONPID2(IC,IP) .ne. 'NOT FOUND')then  
ccc   check for duplicates, don't add if already in the list
ccc   BAN 10 April 2007
              Itally = 1
              do ip2 = 1,nds(ic)
	        if(conpid2(ic,IP) .eq. conpid(ic,ip2)) Itally = 0
	        end do
	        if(Itally .eq. 1) then
              conpid(ic,iadd+nds(ic)) = CONPID2(IC,IP)
	        nds(ic) = nds(ic) + 1
	        iadd = iadd+1
	        end if
ccc
	  end if
            end do
c

          IF(DEBUG) WRITE(NSLS,103) (CONPID(IC,IP),IP=1,NDS(IC))
 103    FORMAT(' Progeny name(s) ',5A12)
        ENDIF
      END DO
C
C----- Determine number and name of usage locations (from FUI section) -------
C
c      IE = 1
c      NUMEXP = 0
c      EXPNUM = GETINT(SETDATA,NLINES,'ExpNum        ',IS,IZ,IZ,IZ,
c    .                 IZ,IZ)
c      IF(DEBUG) write(NSLS,106) expnum
c106   format(' Number of exposure locations from FUI ',I3)
c      DO IE = 1, EXPNUM
c        NMED = GETINT(SETDATA,NLINES,'ExpTypeNum    ',IS,IE,IZ,
c    .                 IZ,IZ,IZ)
c        IF(DEBUG) write(NSLS,108) ie,nmed
c108     format(' Number of media for exposure loc. ',I2,' is ',I3)
c        DO IM = 1,NMED
c          EXPTYP  = GETSTR(SETDATA,NLINES,'ExpType       ',IS,IE,
c    .                       IM,IZ,IZ,IZ)
c          IF(DEBUG) write(NSLS,109) is,ie,im,exptyp
c109       format(' For indexes ',3I2,' the type is ',A4)
c          IF(EXPTYP.EQ.SW) THEN     ! include only SW pathways
c            EXSRCIN = GETSTR(SETDATA,NLINES,'ExpSrcName    ',
c    .               IS,IE,IM,IZ,IZ,IZ)
c            IF(EXSRCIN.EQ.RIVNAM) THEN  ! include only for current river
c              NUMEXP = NUMEXP + 1
c              EXLOCNAM(NUMEXP)=GETSTR(SETDATA,NLINES,'ExpName       ',
c    .               IS,IE,IZ,IZ,IZ,IZ)
c              IEX(NUMEXP) = IE     !save exposure index used in GID file
c              EXSRCNAM(NUMEXP) = EXSRCIN
c              GO TO 30
c            ENDIF
c          END IF
c        END DO
c 30     CONTINUE
c      END DO
C
C---- Get data set from GID for this analysis -------------------------------
C
      CALL GETSET(NGID,NERR,NERROR,GLFNAME,NLINES,SETDATA) 
       IF(DEBUG) write(NSLS,1003) nlines,setdata(1)
 1003  format(' Number of lines in run GID data is ',I3,' First 1 is'/
     .   1x,a60)
C
C----- Determine number and name of usage locations (from River section) ----
C
       EXPNUM = GETINT(SETDATA,NLINES,'GNSWExpNum    ',IZ,IZ,IZ,IZ,
     .                 IZ,IZ)
       NUMEXP = EXPNUM
       IF(DEBUG) write(NSLS,106) expnum
 106   format(' Number of exposure locations from FUI/River ',I3)
       DO IE = 1, NUMEXP
          EXLOCNAM(IE)=GETSTR(SETDATA,NLINES,'GNSWEXPNAME   ',
     .          IE,IZ,IZ,IZ,IZ,IZ)
       END DO
C
C----- Write heading information to Water Concentration File -----------------
C
       CALL HEADOUT()
       IF(DEBUG) WRITE(NSLS,107) NUMEXP,(EXLOCNAM(I),I=1,NUMEXP)
c       IF(DEBUG) WRITE(NSLS,110)  (EXSRCNAM(I),I=1,NUMEXP)
 107   FORMAT(' Number of exposure locations is ',I3/
     . ' Names are: ',A32/20(12x,a32/))
c 110   FORMAT(' Source names for above locations are '/
c     . '            ',A32/20(12x,a32/))
C
C---- Get release type (acute/chronic) ---------------------------------------
C
      SWTYPE = GETSTR(SETDATA,NLINES,'GNSWRelType   ',
     .               IZ,IZ,IZ,IZ,IZ,IZ)
c
c---- Get name of model ------------------------------------------------------
c
      SWMODL = GETSTR(SETDATA,NLINES,'GNSWWaterType ',
     .               IZ,IZ,IZ,IZ,IZ,IZ)
      write(NSLS,111) SWTYPE,SWMODL
 111  format(' Release type is ',A7,' and water model is ',A15)
C
C----- Loop on number of water sources for the current water body ------------
C
      DO IRIVS = 1,NRIVSRC
C
C-----  Find data set in Water Flux File for this river source name ----------
C
      CALL HEADIN(IRIVS,RFOUND)
      IF(RFOUND) THEN
C
C----- Start analysis for each water usage location --------------------------
C----- First get medium (location) data from WFF file ------------------------
C
      LOCNUM = 1
C
C----- Loop on the number of media in the WFF file ---------------------------
C
      NUMED = 0            ! count index on media included
      DO IMED = 1,NMEDIA
        READ(NWFF,*) MEDNAM,MEDTYPE,XL,XU,YL,YU,ZL,ZU,WATFLUX,WFLXU,NCON
        READ(NWFF,*) TU,FLXU,NFLUX
        IF(NFLUX.GT.0) THEN
          DO IFLX = 1,NFLUX
            READ(NWFF,*) TFLX     ! SKIP WATER FLUX INFORMATION
          END DO
          TFLX = TFLX
        ENDIF
        WRITE(NSLS,200) LOCNUM,MEDNAM,MEDTYPE,XL,XU,YL,YU,ZL,ZU,
     .                WATFLUX,WFLXU,NCON
 200    FORMAT(' Medium number:',I3,' information.'/'  Location name: ',
     .     A32/' Medium type: ',A14,' x area = ',F8.1,A8,' y area = ',
     .     F8.1,A8/' z area = ',F8.1,A8,' Water Flux = ',1pe10.3,A8/
     .     ' Number of constituents (parents) at this location ',I4)
        SKIP = .TRUE.
CCC  ADDED 'AQUIFER' AS A POTENTIAL SOURCE OF WFF FILE 12/29/2006 BAN
           IF((SEQI(MEDTYPE,SURWTR,13)) .or. (SEQI(MEDTYPE,OVRLND,8))
     .        .or. (SEQI(MEDTYPE,AQUIFER,7))) 
     .    THEN    ! include surface water types 
             NUMED = NUMED + 1           
             SKIP = .FALSE.
             IF(DEBUG) write(NSLS,2005) medtype,rivnam
 2005        format(' Found source: ',A20,' for river: ',A20)
           ENDIF
C
C----- CALL GETDATA to get parameter values for the site from GID ------------
C
            IF(.NOT.SKIP) THEN
              ICALL = 0
              CALL GETDATA(SETDATA,NLINES,IS,ICALL)
            ENDIF
C
C----- Read data from WFF file to determine the release during the release ---
C      period.  Do for each parent radionuclide, and any progeny
C
        DO IC = 1,NCON
C
C---- Read identification line for constituent ------------------------------
C
          READ(NWFF,*) CNAMIN,CIDIN,TU,FLXU,NTIMES,NFLX,NPROG(IC)
C          READ(NWFF,*) CNAMIN,CIDIN,TU,FLXU,NTIMES,NPROG(IC)
          TU = TU
          FLXU = FLXU
          INDX(1,IC) = 0
          IF(NUMED.LE.1) THEN
            CNAM(1,IC) = CNAMIN
            CID(1,IC) = CIDIN
            DO IX = 1,NUMCON                      ! Test to see if in master list
              IF(CONID(IX).EQ.CID(1,IC)) THEN
                INDX(1,IC) = IX
              ENDIF
            END DO
            IF(INDX(1,IC).LE.0) THEN                 ! Not in master list, error msg.
              WRITE(NERR,2003) CNAM(1,IC),CID(1,IC)
              NERROR = NERROR + 1
 2003         FORMAT(' Error in SWATER.  Constituent in WFF not in',
     .        ' master list from GID'/' Names: ',A20,1x,A12)
            ENDIF
          ELSE
            IF(CIDIN.NE.CID(1,IC)) THEN
              WRITE(NSLS,2007) IMED,CID(1,IC),CIDIN
 2007         FORMAT(' Error in SWATER reading WFF pollutant names,'
     .        ,' for medium #',I3/
     .        ' Expected ',A8,' but found ',A8,'. Skip analysis')
              GO TO 999
            ENDIF
          ENDIF
C
C----- Initialize water array for chain --------------------------------------
C
          DO IN = 1,NPROG(IC)+1
            QSW(IN) = 0.0
          END DO
C
C----- Read release data for parent constituent -----------------------------
C
          IF(NTIMES.GT.0) THEN
            DO IT = 1,NTIMES
                IF(NFLX.EQ.1) THEN
                   READ(NWFF,*) TIME(IT),REL(IT)
                ELSE
                   READ(NWFF,*) TIME(IT),R1,R2
                   REL(IT) = R1 + R2
                ENDIF
              REL(IT) = REL(IT) * BQPCI
            END DO
C
C----- Calculate release in time period TREL for parent constituent ---------
C      For acute: total release, Bq
C      For chronic: average release rate Bq/y
C
            IF(.NOT.SKIP) THEN
              QSW(1) = AVGCON(T0,TREL,TIME(1),TIME(NTIMES),
     .                             REL,TIME)
C
CC              IF(SWTYPE.EQ.'Acute  ') QSW(1) = QSW(1) * TREL
C
C               Revised 26 April 2010 BAN: Acute time units not matching...
              IF(SWTYPE.EQ.'Acute  ') QSW(1) = QSW(1) * 
     .                               (TIME(NTIMES) - TIME(1))


            ENDIF
          ENDIF
C
C---- If there are progeny, read data for each progeny ----------------------
C
          IF(NPROG(IC).GT.0) THEN
            DO IP = 1,NPROG(IC)
C
C---- Read identification line for constituent ------------------------------
C
C              READ(NWFF,*) CNAMIN,CIDIN,TU,FLXU,NTIMES,PNAM,PID,NFLX
              READ(NWFF,*) CNAMIN,CIDIN,TU,FLXU,NTIMES,NFLX,PNAM,PID
              INDX(1+IP,IC) = 0
              PNAM = PNAM
              PID = PID
              ICC = INDX(1,IC)
              IF(NUMED.LE.1) THEN    ! for 1st medium, save progeny id
                CNAM(1+IP,IC) = CNAMIN
                CID(1+IP,IC) = CIDIN
                DO IX = 1,NDS(IC)           ! Test to see if in master list
                  IF(CONPID(ICC,IX).EQ.CID(1+IP,IC)) THEN
                    INDX(1+IP,IC) = IX
                  ENDIF
                END DO
                IF(INDX(1+IP,IC).LE.0) THEN  ! Not in master list, error msg.
                   WRITE(NERR,2004) CNAM(1+IP,IC),CID(1+IP,IC)
                   NERROR = NERROR + 1
 2004             FORMAT(' Error in SWATER.  Progeny in WFF not in',
     .    '       master list from GID'/' Names: ',A20,1x,A12)
                ENDIF
              ELSE   ! for subsequent media, be sure same progeny names are used
                IF(CIDIN.NE.CID(1+IP,IC)) THEN
                  WRITE(NSLS,2007) IMED,CID(1+IP,IC),CIDIN
                  GO TO 999
                ENDIF
              ENDIF
C
C----- Read release data for progeny constituent ----------------------------
C
              IF(NTIMES.GT.0) THEN
                DO IT = 1,NTIMES
                  IF(NFLX.EQ.1) THEN
                     READ(NWFF,*) TIME(IT),REL(IT)
                  ELSE
                     READ(NWFF,*) TIME(IT),R1,R2
                     REL(IT) = R1 + R2
                  ENDIF
                  REL(IT) = REL(IT) * BQPCI
                END DO
C
C----- Calculate total release in time period TREL for progeny constituents -
C
                 IF(.NOT.SKIP) THEN
                   QSW(1+IP) = AVGCON(T0,TREL,TIME(1),
     .                                   TIME(NTIMES),REL,TIME)
C                   IF(SWTYPE.EQ.'Acute  ') QSW(1+IP) = QSW(1+IP)*TREL
C
C               Revised 26 April 2010 BAN: Acute time units not matching...
                  IF(SWTYPE.EQ.'Acute  ') QSW(1+IP) = QSW(1+IP) * 
     .                               (TIME(NTIMES) - TIME(1))
                 ENDIF
               ENDIF
            END DO    ! End of loop on progeny
          END IF      ! End of if there are progeny
          IF(DEBUG) write(NSLS,113) CID(1,IC),NPROG(IC),CID(1,IC),QSW(1)
 113      format(' Radionuclide ',A6,' has ',I2,
     .  ' progeny.'/' Total release: ',A6,' = ',1PE10.2,' Bq')
          if(nprog(IC).gt.0) then
            do ip = 1,nprog(IC)
              IF(DEBUG) write(NSLS,114) CID(IP+1,IC),QSW(1+IP)
 114          format('                ',A6,' = ',1PE10.2,' Bq')
            enddo
          endif
          IF(SKIP) GO TO 50
C
C---- Get radionuclide decay chain data from RMDLIB for radionuclide IC -----
C
          CALL RMDGET( CID(1,IC),IC,nochm)
          if(nochm.gt.0) nprog(ic) = nochm
C
C----- Loop on number of usage locations, NUMEXP ----------------------------
C
          DO IL = 1,NUMEXP
            IF(DEBUG) write(NSLS,2006) imed,ic,il,mednam,exsrcnam(il)
 2006       format(' IMED,IC,IL,MEDNAM,EXSRCNAM(IL):'/
     .             3I3,2A20)
C
C----- CALL GETDATA to get parameter values for usage location from GID -----
C
            CALL GETDATA(SETDATA,NLINES,IS,IL)
            IF(DEBUG) write(NSLS,112) il,swflow(IL),trel,mixr       
 112    format(' For location ',I2,' swflow,trel,mixr =',1p3e10.3)
C
C----- Calculate average water concentration information for release period -
C      For 1st medium, set values.  For subsequent media add values.
C
            CALL SWCONCAL(IL,IC)                 ! calculate SWCON(i)
            IF(NUMED.LE.1.and.IRIVS.EQ.1) THEN   ! Set for first medium/source
              SWOUT(IL,IC,1) = SWCON(1)
            ELSE
              SWOUT(IL,IC,1) = SWOUT(IL,IC,1) + SWCON(1)
            ENDIF
            IF(NPROG(IC).GT.0) THEN        ! set for progeny
              DO IP = 1,NPROG(IC)
                IF(NUMED.LE.1.AND.IRIVS.EQ.1) THEN
                  SWOUT(IL,IC,1+IP) = SWCON(1+IP)
                ELSE
                  SWOUT(IL,IC,1+IP) = SWOUT(IL,IC,1+IP) + SWCON(1+IP)
                ENDIF
              END DO
            ENDIF
          END DO    ! End of loop on exposure location, to WCF
 50     END DO    ! End of loop on parent constituents in WFF
C
C----- Write information out for each location (outer loop) -----------------
C
  80  END DO  ! End of loop on media in WFF file
      ENDIF    ! End if structure on marker NFOUND in WFF file
      END DO   ! End loop on river sources in WFF file.
        DO IL =1,NUMEXP
          CALL CONOUT(IL,NCON,SWOUT)
        END DO ! End loop on locations for printing.
 999  CONTINUE
C
C----- Close files ----------------------------------------------------------
C
      CLOSE(NGID)
      CLOSE(NRMD)
      CLOSE(NWFF)
      CLOSE(NWCF)
      IF(NERROR.LE.0) THEN
        CLOSE(NERR,STATUS='DELETE')
      ELSE
        CLOSE(NERR)
      ENDIF
      CLOSE(NSLS)
C
C----- END OF MODULE SWATER -------------------------------------------------
C
      END

