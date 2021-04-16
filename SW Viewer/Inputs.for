C    INPUTS.FOR                     Version Date: 15-Jan-2003                *
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.     *
C*****************************************************************************
C                                                                            *
C                 SUBROUTINE INPUTS                                          *
C                                                                            *
C  This routine finds the correct pieces of input for the summary            * 
C                                                                            *
C  Written by:       BA NAPIER                                               *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    15-Jan-2003                                             *
C  Last Modified:    15-Jan-2003    BAN                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: NESHAPS main program
C     Called by: NESHAPS
C     Calls: 
C     Common blocks referenced: 
C
C==== Significant Parameter Designation and Description ======================
C
C     Parameter Set/
C     Name      Used   Type    Location  Parameter Description
C     --------- -----  ------  --------- -------------------------------------
C     GIDFNM     U      CHR    FNAMES    File prefix for .GID file
C     LINMAX     U      CHR    PARAMETER Maximum number of lines allowed in 
C                                        the interal array SETDATA for data
C                                        sets taken from the GID file
C     NERR       U      INT    DEVICE    Logical unit for ERR file
C     NERROR    S/U     INT              Error counter
C     NGID       U      INT    DEVICE    Logical unit for GID file
C     NRECPT     U      INT    FNAMES    Index number of receptor to use 
C     NSITE      U      INT    FNAMES    Index number of site to be used
C     NEPA       U      INT    DEVICE    Logical unit for NES file (output
C     NRIF       U      INT    DEVICE    Logical unit for RIF file (input)
C     RUNFNM     U      CHR    FNAMES    File name for run files and data set
C                                        name in GID
C     SETDATA    S      CHR    GID file  Storage array for GID data set 
C
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ----------------------------------------------------------
C   15-Jan-03  BAN  Initial programming started
C    3-Mar -03 BAN  Capture filenames before writing over
c
C
C==== SUBROUTINE CALL ========================================================
C
      SUBROUTINE INPUTS(FacNam, FacStrt, FacCity, UsrNam,PFilNam, 
     . GIDFNM, RUNFNM, NSITE, NUMNES, GLFNAM, METHOD,
     .  NESSRCNA, NESSRCNUM)
C
C==== COMMON Block Definitions ===============================================
C
      include 'NESHAPS.CMN'
	include 'DATABLKS.CMN'
      PARAMETER (LINEMAX=10000)
C     
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER NESMAX, NESSNUM, getint
      CHARACTER*12 RNAMES,RNAME
      CHARACTER*32 nesSRCNA(10), EXPNAM
      CHARACTER*32 NESNAM
      CHARACTER*120 SETDATA(LINEMAX)
      CHARACTER*14 FUI, MEDTYPE
	CHARACTER*132 SITENAM
	CHARACTER*1 B
	CHARACTER*80 FACNAM, FACSTRT, FACCITY, USRNAM, PFilNam
	CHARACTER*32 NAME1, NAME2, NAME3, NAME4
	character*80 getstr
      LOGICAL FILTER
	LOGICAL SEQI
C
      character*128 numnes1, nsite1, gidfnm, runfnm
	character*32 glfnam
C---- Data Statements --------------------------------------------------------
C
      DATA FUI/'FUI           '/
      DATA IZ/0/
	DATA B/' '/

C
C---- Initialize parameter values --------------------------------------------
C
C
C---- Get file names ---------------------------------------------------------
C
      CALL GETNAM(GIDFNM,RUNFNM,NSITE,NUMNES,GLFNAM)
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
C-------- Run Error Message file (.ERR)
C
      OPEN (UNIT=NERR,FILE=RUNFNM(1:NRUNAM)//'.ERR',STATUS='UNKNOWN')
C
C-------- Run Output listing file (.SHP)
C
      OPEN (UNIT=NSHP,FILE=RUNFNM(1:NRUNAM)//'.SHP',STATUS='UNKNOWN')
C
C-------- SURFACE WATER Summary output file (.EPA) -----------------------
C
      OPEN (UNIT=NEPA,FILE=RUNFNM(1:NRUNAM)//'.EPA',STATUS='UNKNOWN')
C
C----- Write run argument values to SHP report file --------------------------
C
      WRITE(NSHP,111) GIDFNM,RUNFNM,NSITE,NUMnes,GLFNAM
 111  FORMAT(' GIDFNM  =',A70/
     .       ' RUNFNM  =',A70/
     .       ' NSITE   =',I3/
     .       ' NUMnes  =',I3/
     .       ' GLFNAM =',A32)
C
C----- Get Framework User Interface data from GID file -----------------------     
C
      FILTER = .TRUE.
      CALL GETSET(NGID,NERR,FUI,NLINES,SETDATA,NSITE,FILTER)
      FILTER = .FALSE.
C
C----- Was the FUI data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2211) NLINES
 2211   FORMAT(' The FUI data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Get name of "site" (e.g. SCENARIO 1)-----------------------------------
C
      IS = NSITE                 ! Number of sites from ID file
	I1 = 1                     ! FRAMES has a second index (unused??)
C
C----- Get number of NES Glyphs---------------------------------------
C
C      
      NESMAX = GETINT(SETDATA,NLINES,'nesNum        ',IS,IZ,IZ,IZ,IZ,IZ)
      IF(NESMAX.LT.NUMnes.OR.NUMnes.LE.0) THEN
        WRITE(NERR,2212) NUMnes, NESMAX
 2212   FORMAT(' Error in specification of NUMBER OF SW viewers'/
     .         ' Value given: ',I3,'  Maximum allowed: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Get name of THIS SW VIEWER GLYPH ----------------------------
	IH = NUMnes
      nesNAM = GETSTR(SETDATA,NLINES,'NESName       ',IS,IH,IZ,IZ,IZ,IZ)
      WRITE(NSHP,113) NESNAM
 113  FORMAT(' NesName  = ',A32)
C
C----- Get number of Health Impact glyphs feeding into this one -------------
C
      NESSNUM=GETINT(SETDATA,NLINES,'NesSrcNum     ',IS,IH,IZ,IZ,IZ,IZ)
      WRITE(NSHP,116) NesSNUM
 116  FORMAT(' NesSrcNum  = ',I3)
      IF(NESSNUM.LE.0) THEN
        WRITE(NERR,2213) NESSNUM
 2213   FORMAT(' Error in specification of number of HEALTH GLYPHS '/
     .         ' Value must be greater than zero.  Found: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Identify NAMES for the Health Glyphs -----------------------
C
      DO IT = 1,NESSNUM
        NesSRCNA(IT)=GETSTR(SETDATA,NLINES,'NesSrcName    ',IS,IH,IT,IZ,
     .                IZ,IZ)
      END DO
CCCC
CCCC   Space to discover the entire traceback to the sources
CCCC
C      First find the heiName.   For this application, there should be ONLY ONE
c 
      do it = 1, nessnum
      if(seqi(NesSrcNa(IT),'hei',3))NesSrcName = NesSRCNA(it)
	end do
C
C      Now, scan the FUI Section of the GID file for HEI info
      heiNum = GETINT(SETDATA,NLINES,'heiNUM        ',IS,IZ,IZ,IZ,
     .                 IZ,IZ)
	 DO IT = 1, heiNUM
	 NAME1 = GETSTR(SETDATA,NLINES,'heiNAME       ',IS,IT,IZ,
     .                 IZ,IZ,IZ)     
	   IF (SEQI(NAME1,NesSrcName,5)) THEN
	     I2 = IT
	     heiNAME = NAME1
	   END IF
	 END DO
	 heiSrcNum = GETINT(SETDATA,NLINES,'heiSrcNum     ',IS,I2,IZ,
     .                    IZ,IZ,IZ)
	 heiSrcName = GETSTR(SETDATA,NLINES,'heiSrcName    ',IS,I2,
     .                heiSrcNum,IZ,IZ,IZ)
C     NOW WE HAVE ALL THE HEI DATA, GO FOR RCP DATA (ONLY 1 ALLOWED)
      rcpNum = GETINT(SETDATA,NLINES,'rcpNum        ',IS,IZ,IZ,IZ,
     .                  IZ,IZ)
	DO IT = 1, rcpNUM
	 NAME2 = GETSTR(SETDATA,NLINES,'rcpNAME       ',IS,IT,IZ,
     .                 IZ,IZ,IZ)     
	   IF (SEQI(NAME2,heiSrcName,5)) THEN
	     I3 = IT
	     rcpNAME = NAME2
	   END IF
	 END DO
	 rcpSrcNum = GETINT(SETDATA,NLINES,'rcpSrcNum     ',IS,I3,IZ,
     .                    IZ,IZ,IZ)
	 rcpSrcName = GETSTR(SETDATA,NLINES,'rcpSrcName    ',IS,I3,
     .                rcpSrcNum,IZ,IZ,IZ)
c      NOW WE HAVE ALL THE RCP DATA, GO FOR THE EXP DATA (ONLY 1 ALLOWED)
      expNum = GETINT(SETDATA,NLINES,'expNum        ',IS,IZ,IZ,IZ,
     .                  IZ,IZ)
	DO IT = 1, expNUM
	 NAME3 = GETSTR(SETDATA,NLINES,'expNAME       ',IS,IT,IZ,
     .                 IZ,IZ,IZ)     
	   IF (SEQI(NAME3,rcpSrcName,5)) THEN
	     I4 = IT
	     expNAME = NAME3
	   END IF
	 END DO
	 expSrcNum = GETINT(SETDATA,NLINES,'expSrcNum     ',IS,I4,IZ,
     .                    IZ,IZ,IZ) 
	 expSrcName = GETSTR(SETDATA,NLINES,'expSrcName    ',IS,I4,
     .                expSrcNum,IZ,IZ,IZ)
C
C  because the easting/northing is so important, get them, too
C
        expX=getreal(setdata,nlines,'expx          ',is,i4,iz,iz,iz,iz)
        expY=getreal(setdata,nlines,'expy          ',is,i4,iz,iz,iz,iz)
C     NOW WE HAVE ALL THE EXP DATA, GO FOR THE RIVER DATA
      rivNum = GETINT(SETDATA,NLINES,'rivNum        ',IS,IZ,IZ,IZ,
     .                  IZ,IZ)
	DO IT = 1, rivNUM
	 NAME4 = GETSTR(SETDATA,NLINES,'rivNAME       ',IS,IT,IZ,
     .                 IZ,IZ,IZ)     
	   IF (SEQI(NAME4,expSrcName,5)) THEN
	     I5 = IT
	     rivNAME = NAME4
	   END IF
	 END DO
	 rivSrcNum = GETINT(SETDATA,NLINES,'rivSrcNum     ',IS,I5,IZ,
     .                    IZ,IZ,IZ) 
       DO IU = 1, rivSrcNum
	 rivSrcName(IU) = GETSTR(SETDATA,NLINES,'rivSrcName    ',IS,I5,
     .                  IU,IZ,IZ,IZ)
C     WE CAN SHORT CIRCUIT A STEP HERE AND ASSIGN SrcName's,
C         SO NOW WE HAVE ALL THE INFO NEEDED
       srcName(IU) = rivSrcName(IU)   
	 srcNUM = rivSrcNum      
       END DO

CCCC
CCCC
C     Read other necessary info from the GID file
      NUMCON = GETINT(SETDATA,NLINES,'NUMCON        ',IS,IZ,IZ,IZ,IZ,IZ)
	DO INC = 1, NUMCON
	NDS(INC)=GETINT(SETDATA,NLINES,'NDS           ',IS,INC,IZ,IZ,
     .               IZ,IZ)
	END DO

CCC
C----- Get data set for this glyph ------------------------------------
C
      CALL GETSET(NGID,NERR,NESNAM,NLINES,SETDATA,NSITE,FILTER)
      WRITE(NSHP,1115) NESNAM,NLINES
 1115 FORMAT(' NESHAPS data set sought in GID: ',A16,' lines =',i4)
C
C----- Was the Run data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2214) NLINES
 2214   FORMAT(' The Run data set was not found, NLINES =',I5)
         NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Call REDCAS to read parameter values for this run ---------------------
C
      CALL REDCAS(SETDATA,NLINES, FacNam, FacStrt, FacCity, UsrNam, 
     . method, PFilNam, IremSv, ipthnuc)
	 pathnuc = .false.
	if (ipthnuc .gt. 0) pathnuc = .true.
C
  999 CONTINUE
      return
      END
C=================== END OF MODULE INPUTS ====================================
