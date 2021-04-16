C    INTAKE.FOR                     Version Date: 21-Nov-01                  *
C   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.     *
C*****************************************************************************
C                                                                            *
C                     PROGRAM  INTAKE                                        *
C                                                                            *
C  Program INTAKE controls calculation of receptor intake for specified      *
C  receptor locations.  Input and output use files from the Framework        *
C  control program definition (.GID and EPF) with output to file RIF.        *
C                                                                            *
C  Written by:       Dl Strenge                                              *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    22-Nov-96                                               *
C  Last Modified:    21-Nov 01      BAN                                      *
C                                                                            *
C*****************************************************************************
C
C==== Modular Organization ===================================================
C
C     Module of: INTAKE  main program
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
C     NRECPT     U      INT    FNAMES    Index number of receptor to use 
C     NSITE      U      INT    FNAMES    Index number of site to be used
C     NUMEXP     U      INT    EXINFO    Number of exposure location to be used
C     NEPF       U      INT    DEVICE    Logical unit for EPF file (input)
C     NRIF       U      INT    DEVICE    Logical unit for RIF file (output)
C     RUNFNM     U      CHR    FNAMES    File name for run files and data set
C                                        name in GID
C     SETDATA    S      CHR    GID file  Storage array for GID data set 
C
C==== Modification History ===================================================
C     Date     Who  Modification Description
C     -------- ---  ----------------------------------------------------------
C   05-Jun-97  DLS  Initial programming started
C   26-Oct 2001 BAN  Minor bug in reading of multiple data sets from same exp location
C   21 Nov 2001 BAN  Change to review all inputs in master list, line 355
C   26 July 2002 BAN  Installed a trap for zero ExpTypeNum (hopefully temporary)
C   16 Sept 2002 BAN  Installed WL and WLM
C   30 Sept 2004 BAN  Moved write of # of sets to here from RIFHEAD
C
C==== SUBROUTINE CALL ========================================================
C
      PROGRAM INTAKE
C
C---- Include Statements for Parameter and Common Declarations ---------------
C      
      INCLUDE 'AFLAGS.CMN'
      INCLUDE 'AGES.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'CONIN.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'RCPINFO.CMN'
      INCLUDE 'FNAMES.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'FLUX.CMN'
      INCLUDE 'NUCNAM.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'RADATA.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
	INCLUDE 'RADON.CMN'
C
C---- Variable Type Declarations ---------------------------------------------
C
      INTEGER GETINT, RCPMAX, RCPSNUM, NEXPSRC(10)
      INTEGER EXPNUM
      CHARACTER*12 RNAMES,RNAME
      CHARACTER*32 GETSTR, RCPSRCNA(10), EXPNAM
      CHARACTER*32 RCPNAM    
      CHARACTER*80 SETDATA(LINEMAX)
      CHARACTER*14 FUI, MEDTYPE
      CHARACTER*1 B
	character*7 val
      LOGICAL FILTER
      LOGICAL FOUND, FOUNDM, FOUNDR, ACUTE
	DIMENSION WLtemp(10,1681), WLMtemp(10,1681)
C
C---- Data Statements --------------------------------------------------------
C
      DATA FUI/'FUI           '/
      DATA B/' '/
C
C---- Initialize parameter values --------------------------------------------
C
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
C     OPEN (UNIT=NRMD,FILE='RMDLIB.DAT',STATUS='OLD')
C
C-------- Run Error Message file (.ERR)
C
      OPEN (UNIT=NERR,FILE=RUNFNM(1:NRUNAM)//'.ERR',STATUS='UNKNOWN')
C
C-------- Run Output listing file (.ELS)
C
      OPEN (UNIT=NRLS,FILE=RUNFNM(1:NRUNAM)//'.RLS',STATUS='UNKNOWN')
C
C-------- Exposure component PDCF listing file (.EPF) -----------------------
C
      OPEN (UNIT=NRIF,FILE=RUNFNM(1:NRUNAM)//'.RIF',STATUS='UNKNOWN')
C
C----- Write run argument values ---------------------------------------------
C
      WRITE(NRLS,100) GIDFNM,RUNFNM,NSITE,NUMRCP,GLFNAME
 100  FORMAT(' GIDFNM  =',A70/
     .       ' RUNFNM  =',A70/
     .       ' NSITE   =',I3/
     .       ' NUMRCP  =',I3/
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
      SITNAM = GETSTR(SETDATA,NLINES,'SiteName      ',IS,IZ,IZ,IZ,IZ,IZ)
      WRITE(NRLS,101) SITNAM
 101  FORMAT(' SiteName = ',A32)
C
C----- Get number of receptor location ---------------------------------------
C
      RCPMAX = GETINT(SETDATA,NLINES,'RcpNum        ',IS,IZ,IZ,IZ,IZ,IZ)
      IF(RCPMAX.LT.NUMRCP.OR.NUMRCP.LE.0) THEN
        WRITE(NERR,2001) NUMRCP, RCPMAX
 2001   FORMAT(' Error in specification of receptor location number'/
     .         ' Value given: ',I3,'  Maximum allowed: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Get name of receptor location -----------------------------------------
C
      IR = NUMRCP                ! Number of sites from ID file
      RCPNAM = GETSTR(SETDATA,NLINES,'RcpName       ',IS,IR,IZ,IZ,IZ,IZ)
      RCPX   =GETREAL(SETDATA,NLINES,'RcpX          ',IS,IR,IZ,IZ,IZ,IZ)
      RCPY   =GETREAL(SETDATA,NLINES,'RcpY          ',IS,IR,IZ,IZ,IZ,IZ)
      WRITE(NRLS,102) RCPNAM,RCPX,RCPY
 102  FORMAT(' RcpName  = ',A32/' Coordinates (x,y) = ',1p2e10.2)
C
C----- Get number of receptor sources ----------------------------------------
C
      RCPSNUM=GETINT(SETDATA,NLINES,'RcpSrcNum     ',IS,IR,IZ,IZ,IZ,IZ)
      WRITE(NRLS,106) RCPSNUM
 106  FORMAT(' RcpSrcNum  = ',I3)
      IF(RCPSNUM.LE.0) THEN
        WRITE(NERR,2002) RCPSNUM
 2002   FORMAT(' Error in specification of number of receptor sources '/
     .         ' Value must be greater than zero.  Found: ',I3)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Identify source types for the receptor location -----------------------
C
      DO IT = 1,RCPSNUM
        RCPSRCNA(IT)=GETSTR(SETDATA,NLINES,'RcpSrcName    ',IS,IR,IT,IZ,
     .                IZ,IZ)
      END DO
C
C---- Get number of exposure sources for current receptor --------------------
C     location.  Save number of sources.
C
      NR = 0
      EXPNUM = GETINT(SETDATA,NLINES,'ExpNum        ',IS,IZ,IZ,IZ,
     .                IZ,IZ)
      DO IE = 1,EXPNUM
        EXPNAM = GETSTR(SETDATA,NLINES,'ExpName       ',IS,IE,IZ,IZ,
     .                IZ,IZ)
        DO IR = 1,RCPSNUM
          IF(EXPNAM.EQ.RCPSRCNA(IR)) THEN
            NEXPSRC(IR) = GETINT(SETDATA,NLINES,'ExpTypeNum    ',IS,IE,
     .      IZ,IZ,IZ,IZ)
            NR = NR + NEXPSRC(IR)
          ENDIF
        END DO
      END DO
C TEMPORARY FIX
      IF(NR.EQ.0)NR=1
C
C
C----- Get number of constituents included in the run ------------------------
C
      NUMCON = GETINT(SETDATA,NLINES,'NumCon        ',IS,IZ,IZ,IZ,IZ,IZ)
      WRITE(NRLS,103) NUMCON
 103  FORMAT(' Number of constituents is ',I3)
C
C----- Get names of each constituent, number of progeny, and progeny names
C
      NUMGOOD = 0
      ICIN = 1
      DO IC = 1,NUMCON
        CONID(ICin) = GETSTR(SETDATA,NLINES,'FSCASID       ',IS,IC,
     .                   IZ,IZ,IZ,IZ)            ! Name of parent
        NDS(ICin) = GETINT(SETDATA,NLINES,'NDS           ',IS,IC,IZ,
     .                 IZ,IZ,IZ)                 ! Number of progeny
C
C---- Test input name to eliminate non-radionuclides -------------------------
C     Non-radionuclides have CAS numbers
C
        IF(CONID(ICin)(1:1).LT.'A') THEN
          WRITE(NRLS,2013) CONID(ICin)
2013      FORMAT(' Warning: Pollutant name in master list is not a ',
     .    'radionuclide: ignore ',A12)
          CONID(ICin) = B
        ELSE
          NUMGOOD = NUMGOOD + 1
          WRITE(NRLS,104) CONID(ICin),NDS(ICin)
 104      FORMAT(' Parent name ',A12,' with progeny # ',I4)
          IF(NDS(ICin).GT.0) THEN
            DO IP = 1,NDS(ICin)
              CONPID(ICin,IP) = GETSTR(SETDATA,NLINES,'FSCASID       ',
     .                             IS,IC,IP,IZ,IZ,IZ)  ! Name of progeny
            END DO
            WRITE(NRLS,105) (CONPID(ICin,IP),IP=1,NDS(ICin))
 105    FORMAT(' Progeny name(s) ',5A12)
          ENDIF
          ICIN = ICIN + 1
        ENDIF
      END DO
      IF(NUMCON.NE.NUMGOOD) THEN
        WRITE(NRLS,2014) NUMCON,NUMGOOD
 2014   FORMAT(' Warning: some pollutants are not radionuclides. '/
     .  '   Sought ',I3,' found 'i3,'.')
      ENDIF
C
C----- Get data set for receptor location ------------------------------------
C
      CALL GETSET(NGID,NERR,RCPNAM,NLINES,SETDATA,NSITE,FILTER)
      WRITE(NRLS,1115) RCPNAM,NLINES
 1115 FORMAT(' Receptor data set sought in GID: ',A16,' lines =',i4)
C
C----- Was the Run data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2004) NLINES
 2004   FORMAT(' The Run data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----- Call REDCAS to read parameter values for this run ---------------------
C
      CALL REDCAS(SETDATA,NLINES)
C
C----- Call RIFHEAD to WRITE heading information to the RIF PDCF file -------
c
       CALL RIFHEAD(NR,RCPSNUM,RCPSRCNA)
C
C----- Evaluate each exposure source for this receptor -----------------------
C
      DO IR = 1,RCPSNUM
        NUMNUC = 0
C
C-------- Open run exposure concentration file (.EPF) ---------------------------
C
      OPEN (UNIT=NEPF,FILE=GIDFNM(1:NGIDNM)//'.EPF',STATUS='UNKNOWN')
C
C----- read beginning of EPF file and position to desired data set -----------
C
      CALL MARKIN(RCPSRCNA(IR),FOUNDM)
      IF(.NOT.FOUNDM) GO TO 55
        CALL EPFNUM(NUMSET)
        IF(NERROR.GT.0) GO TO 999
C
C---   Tell RIF file how many sets it has (taken from RIFHEAD) 30 Sept 2004
C
         Write(nrif,400) numset
 400  FORMAT(I3,',')
C
C---------Open file for radon wokring level data
C
      OPEN (UNIT=NWLM,FILE=GIDFNM(1:NGIDNM)//'.WLM',STATUS='UNKNOWN',
     .      POSITION = 'APPEND')
C
C-----
      DO IS = 1,NUMSET
        FOUNDR = .FALSE.
        CALL  EPFRCP(ACUTE, NUMSET,RCPSRCNA(IR),MEDTYPE,NPOINTS,NUMNUC,
     .  FOUNDR)
        IF(FOUNDR) THEN
C
C----- Test number of radionuclide parents defined for EPF file --------------
C       Must be greater than 0
C
        IF(NUMNUC.LE.0) THEN
          WRITE(NERR,1116) RCPNAM
 1116     FORMAT(' Error reading EPF.  Data set not found for:',a14)
          NERROR = NERROR + 1
          GO TO 999
        ELSE
          write(NRLS,1117) RCPNAM,numnuc
 1117     format(' EPF set found for: ',a20,' Numnuc =',i3)
        ENDIF
	   Lradon = .False.
C
C-------- Do analysis for each age group (NAGES) --------------------------------
C
c		WLtemp = 0.0
c	    WLMtemp = 0.0
	   WL = 0.0
	   WLM = 0.0
       DO IAGE = 1,NAGES
c	   WL = 0.0
c	   WLM = 0.0
		WLtemp = 0.0
	    WLMtemp = 0.0
C
C----- Write data set heading to RIF file for this data set ------------------
C
         CALL RIFDSET(RCPSRCNA(IR),MEDTYPE,NPOINTS,NUMNUC,IAGE)
C
C---- Begin loop on radionuclides in EPF file for current source -------------
C
         DO IN = 1,NUMNUC
            RNAMES = ' '
C
C----- Call EPFRAD to read data for current parent ---------------------------
C
          CALL EPFRAD(RNAMES,NPRG,NTIMES)
C
C----- Call RIFRCP to write data for current parent ---------------------------
C
          CALL RIFRAD(RNAMES,NPRG,NTIMES)
C
C----- Test radionuclide name against master list for validity ---------------
c
           FOUND = .FALSE.
C  NUNUC changed to NUMCON here, BAN 21 Nov 2001
           DO IC = 1,NUMCON
              IF(RNAMES.EQ.CONID(IC)) THEN
                 FOUND = .TRUE.
              ENDIF
           END DO
           IF(.NOT.FOUND) THEN
              WRITE(NERR,2012) RNAMES
 2012       FORMAT(' Error in EPF radionuclide name: not in master ',
     .  'list.'/'   Name found was: ',A12,' Analysis not performed')
              NERROR = NERROR + 1
              GO TO 999
C
              CONTINUE
C
           ELSE
C
C----- Loop on number of integrating time periods for current parent --------
C
            DO IT = 1,NTIMES
C
             CALL EPFRIF(SETDATA,NLINES,NPRG,NPOINTS,RNAMES,IAGE,ACUTE)
             IF(NERROR.GT.0) GO TO 999
C
C ----- Intermediate accumulation of working levels
	      DO IP = 1,NPOINTS
		   WLtemp(IT,IP) = WLtemp(IT,IP) + WL(IAGE,IP)
	       WLMtemp(IT,IP) = WLMtemp(IT,IP) + WLM(IAGE,IP)
             IF (WLtemp(IT,IP) .gt. 0.0) Lradon = .True.
	      END DO
	      WL = 0.0
	      WLM = 0.0
C
            END DO    ! End of loop on IT, number of time period

C
           END IF  ! If on .NOT.FOUND for radionuclide parent name

         END DO   ! End of loop on parent radionuclides
C
           IF(IAGE.LT.NAGES) THEN
             REWIND(NEPF)
C  Need to increment over data sets based on IS, to move past earlier sets
C  BAN 26 Oct 2001
               CALL MARKIN(RCPSRCNA(IR),FOUNDM)
	if (is .gt. 1) then
      do iban = 1,is
 1234 read(nepf,*) val
      if (val .ne. "chronic") then
	go to 1234
	else
	if (iban .eq. is)backspace nepf
      endif
	end do
	end if 
      if (is .eq. 1) CALL EPFNUM(NUMSET) 
	       CALL EPFRCP(ACUTE,NUMSET,RCPSRCNA(IR),MEDTYPE,NPOINTS,
     .	   NUMNUC,FOUNDR)   
          ENDIF
C
C-----Write radon results for WL and WLM to *.WLM file for this time, age group----
C
        IF (Lradon) THEN
	  DO IT = 1, NTIMES
	   Write (NWLM,2021) IAGE, IT	   
	   Write (NWLM,2022) (WLtemp(IT,IP),IP = 1,NPOINTS)
	   Write (NWLM,2023) IAGE, IT
	   Write (NWLM,2022) (WLMtemp(IT,IP),IP = 1,NPOINTS)
 2021   FORMAT('Radon Progeny Working Levels for age group ',I1,', time'
     .         ,I3,' are:')
 2022   FORMAT(1pE10.2,9E10.2)
 2023   FORMAT('Radon Working Level Months for age group ',I1,', time'
     .         ,I3,' are:')
	  END DO
	  END IF      ! Lradon print if
        END DO    ! End of loop on IAGE, number of age groups     
       END IF  ! If on FOUNDR
	   IF (.NOT. Lradon) Write (NWLM, 2020) RCPNAM, EXPNAM
 2020    FORMAT(' No Radon Working Levels needed for FRAMES Receptor ',
     .           'Icon ',A5,' from Exposure Icon ',A5)
       END DO    ! End of loop on IS, number of data sets in current EPF set.
       CLOSE(NEPF)
      END DO    ! End of loop on IR, number of exposure sources
C
  999  CONTINUE
C
C----- Close files ----------------------------------------------------------
C
 55   CLOSE(NGID)
      CLOSE(NEPF)
C     CLOSE(NRMD)
      CLOSE(NRIF)
      IF(NERROR.LE.0) THEN
        CLOSE(NERR,STATUS='DELETE')
      ELSE
        CLOSE(NERR)
      ENDIF
      CLOSE(NRLS)
	CLOSE(NWLM)
C
C----- END OF MODULE INTAKE -------------------------------------------------
C
      END

