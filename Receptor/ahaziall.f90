!    Last change:  MAS  29 Apr 2006    7:15 pm
!   MEPAS AHAZ: AHAZ.FOR             Version Date:  29-Apr-06
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           MAIN PROGRAM                                     *
!                                                                            *
!  Main Program AHAZI Annual HAZard analysis evaluation program              *
!                 The main program HAZ controls the calculation of human     *
!                 health impacts  for each pollutant                         *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01-Nov-1995 (Converted from HAZ2)                       *
!  Last Modified:    29-Apr-2006  MAS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ4.0
!     Called by: NONE
!     Calls: SUBROUTINES PDATIN, USEDAT, EXPOS, GDK
!     Common blocks referenced: BLKDAT, BACKGD, DEVICE, H3, C14, SWPATH, GWPATH,
!                               HPDAT, PSET1, AIRDAT, ATPATH, TITLS, COUPLE,
!                               LOCNAM, SLPATH, FDPATH, MASBAL, PSET3, POS,
!                               RISKCM, PSET2
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!     GDKFAC(20)  -    REAL    Internal  Deposition decay correction factor
!                                        for each pollutant (air pathway), yr.
!     IPDAT       -    int     Internal  Flag to print library data.
!     KNPATH(7)   -    INT     Internal  Number of usage locations for each
!                                        transport pathway
!     NIN         s/u  INT     C-DEVICE  Logical unit for input records.
!     NPOL        S/U  INT     C-PSET1   Number of pollutants for current run.
!     TRITM       S/U  LOGIC   C-H3      Logical flag to indicate if current
!                                        pollutant is tritium
!     CARBON      S/U  LOGIC   C-C14     Logical flag to indicate if current
!                                        pollutant is carbon-14
!
!==== Modification History ===================================================
!
!   Date      Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  17-Jun-97  DLS  Initial programming for intake component to MEPAS 4.0.
!  08-Sep-97  DLS  Change version date for INCALC changes
!  07-Oct-98  DLS  Change version date
!  24-May-99  DLS  Change version date
!  20-Jul-99  DLS  Removed reading of population information from GID file
!  29-Apr-06  MAS  Added adherence factor for sediment (ADHSED)
!==== SUBROUTINE CALL ========================================================
!
      PROGRAM AHAZI
!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'DEVICE.FTN'
        INCLUDE 'SWPATH.FTN'
        INCLUDE 'GWPATH.FTN'
        INCLUDE 'PFLAGS.FTN'
        INCLUDE 'PSET1.FTN'
        INCLUDE 'PSET2.FTN'
        INCLUDE 'PSET3.FTN'
        INCLUDE 'AIRDAT.FTN'
        INCLUDE 'ATPATH.FTN'
        INCLUDE 'TITLS.FTN'
        INCLUDE 'COUPLE.FTN'
        INCLUDE 'LOCNAM.FTN'
        INCLUDE 'SLPATH.FTN'
        INCLUDE 'FDPATH.FTN'
        INCLUDE 'MASBAL.FTN'
        INCLUDE 'RISKCM.FTN'
        INCLUDE 'TIMES.FTN'
        INCLUDE 'DECAY.FTN'
        INCLUDE 'PARMTR.PAR'
        INCLUDE 'PTHUSE.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION KNPATH(5)
      REAL VALIN(360),VALOUT(360), EXPX(360), EXPY(360)
!
!==== Variable Declarations ==================================================
!
      CHARACTER*30 EPATH(25)    ! Exposure pathway titles
      CHARACTER*1 B,C
      CHARACTER*14 FUI
      CHARACTER*14 CSM
      CHARACTER*3 CON                                 ! Source type for testing = "con"
      CHARACTER*14 CONI                               ! Source type for finding database data in GID
      INTEGER ICONNUM                                 ! Index of Contaminant database ID
      INTEGER MODSRCNUM                               ! Number of source modules connected to exp module
      CHARACTER*5 MODSRCTYPE                          ! Source type, search for "con"
      CHARACTER*10 MODID                              ! Search ID for contaminant database ID
      CHARACTER*32 MODNAME                            ! MODULE NAME
      CHARACTER*20 EXNAME,CONAME,VERN
      CHARACTER*32 EXPNAM
      CHARACTER*12 EXROUTE
      CHARACTER*12 EXPRUT(26)
      CHARACTER*20 EXPLAB(26)
      CHARACTER*22 ETYPOUT
      CHARACTER*6  EU,UT,UR
      CHARACTER*7  STYPE
      CHARACTER*9  EUOUT
      CHARACTER*32 ELOC
      CHARACTER*32 RCPSRCNA(10)
      CHARACTER*14 MEDTYPE,TYPES(6),typest
      CHARACTER*12 CONID
      CHARACTER*80 GETSTR
      LOGICAL FOUNDM, FOUNDE
      LOGICAL FILTER
      CHARACTER (LEN=10) :: DATE, TIME, ZONE
      CHARACTER*8 MMDDYY,HHMMSSHH
      CHARACTER (LEN=3) :: CMO, CMONTH(12)
      INTEGER TPATH, GETINT
	  INTEGER(4) statarray(12), ISTAT
      INTEGER :: DT(8)
      CHARACTER*120 SETDATA(LINEMAX)
      LOGICAL SEQI
      CHARACTER(LEN=400) :: CLINE   ! Command line arguments
!
!==== DATA Statements ========================================================
!
      DATA CMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/
      DATA FUI/'FUI           '/
      DATA CSM/'CSM           '/
      DATA CONI/'              '/
      DATA CON/'con'/
      DATA B/' '/
      DATA TYPES/'Aquifer       ', &
                 'Surface water ', &
                 'Air           ', &
                 'Air           ', &
                 'Soil          ', &
                 'Sediment      '/                     ! addition of "sediment" for future expansion
       DATA EPATH(1)/'Drinking water ingestion      '/
       DATA EPATH(2)/'Shower: dermal                '/
       DATA EPATH(3)/'Shower: ingestion             '/
       DATA EPATH(4)/'Leafy vegetable ingestion     '/
       DATA EPATH(5)/'Other vegetable ingestion     '/
       DATA EPATH(6)/'Meat ingestion                '/
       DATA EPATH(7)/'Milk ingestion                '/
       DATA EPATH(8)/'Fin fish ingestion            '/
       DATA EPATH(9)/'Shell fish ingestion          '/
      DATA EPATH(10)/'Swimming: water ingestion     '/
      DATA EPATH(11)/'Swimming: dermal absorption   '/
      DATA EPATH(12)/'Shoreline: dermal             '/
      DATA EPATH(13)/'Shoreline: ingestion          '/
      DATA EPATH(14)/'Soil ingestion                '/
      DATA EPATH(15)/'Soil dermal                   '/
      DATA EPATH(16)/'Special food intake           '/
      DATA EPATH(17)/'Showering inhalation          '/
      DATA EPATH(18)/'Air inhalation                '/
      DATA EPATH(19)/'Soil resuspension inhalation  '/
      DATA EPATH(20)/'Swimming: external            '/
      DATA EPATH(21)/'Boating: external             '/
      DATA EPATH(22)/'Shoreline: external           '/
      DATA EPATH(23)/'Soil: external                '/
      DATA EPATH(24)/'Air: external                 '/
      DATA EPATH(25)/'Indoor air VOC inhalaton      '/
!
      DATA EXPLAB/'Water               ',  &
                  'Shower              ',  &
                  'Shower              ',  &
                  'Leafy vegetables    ',  &
                  'Other vegetables    ',  &
                  'Meat                ',  &
                  'Milk                ',  &
                  'Fish                ',  &
                  'Crustacea           ',  &
                  'Swimming            ',  &
                  'Swimming            ',  &
                  'Shoreline           ',  &
                  'Shoreline           ',  &
                  'Soil                ',  &
                  'Soil                ',  &
                  'Food                ',  &
                  'Shower              ',  &
                  'Air                 ',  &
                  'Soil                ',  &
                  'Swimming            ',  &
                  'Boating             ',  &
                  'Shoreline           ',  &
                  'Soil                ',  &
                  'Air                 ',  &
                  'Indoor air          ',  &
                  'Ground              '/
      DATA EXPRUT/'ingestion   ',  &       !
                  'dermal      ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'dermal      ',  &       !
                  'dermal      ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'dermal      ',  &       !
                  'ingestion   ',  &       !
                  'inhalation  ',  &       ! 17
                  'inhalation  ',  &       ! 18
                  'inhalation  ',  &       ! 19
                  'external    ',  &
                  'external    ',  &
                  'external    ',  &
                  'external    ',  &
                  'external    ',  &
                  'inhalation  ',  &       ! 25
                  'external    '/
!
!     VERN = ' VERSION 04-Apr-05 '
!     VERN = ' VERSION 24-Oct-05 '
      VERN = ' VERSION 10-Nov-05 '
!
!----- Get FACILITY AND WASTE UNIT (file)NAMES ------------------------
!
      IZ = 0
      IERR = 0

!---  Read the command line to get parameters for the run
!     GETCL returns command line entries after the program name
!     GETCL is a Lahey Fortran 90 language extension
!!
      ICOMP=2 ! Digital/Compaq FORTRAN
      IF(ICOMP.EQ.1)THEN ! Lahey Fortran 90
        IERR = 0
!        CALL GETCL(CLINE)    ! activate for Lahey Fortran 90
!        READ(CLINE,*,ERR=5,END=5) GIDFNM,HINFNM,SITNUM,RCPNUM,MODNAME
      ELSE  ! COMPAQ FORTRAN
        call GetArg(1,GIDFNM,status)
        if(status.lt.0) GO TO 5  
        call GetArg(2,HINFNM,status) 
        if(status.lt.0) GO TO 5  
        call GetArg(3,CLINE,status)
        if(status.lt.0) GO TO 5  
        READ(CLINE,'(I5)') SITNUM
        call GetArg(4,CLINE,status)                                                         
        if(status.lt.0) GO TO 5 
        READ(CLINE,'(I5)') RCPNUM
        call GetArg(5,MODNAME,status)
        if(status.lt.0) GO TO 5  
     ENDIF


      GO TO 6
 5    ierr = 1
      WRITE(nerr,*) 'Error reading command line'
      GO TO 999
  6   continue
      WRITE(*,*) TRIM(GIDFNM),TRIM(HINFNM),SITNUM,RCPNUM,TRIM(MODNAME)
!
!---- Open GID primary information file for run, GID -------------------------
!
      OPEN (UNIT=NGID, FILE=TRIM(GIDFNM)//'.GID',STATUS='OLD')
!
!---- Open files receptor output file for current run, RIF -------------------
!
      OPEN (UNIT=NRIF, FILE=TRIM(HINFNM)//'.RIF',STATUS='UNKNOWN')
!
!---- open error message file, ERR -------------------------------------------
!
      open(UNIT=NERR,STATUS='UNKNOWN',FILE=TRIM(HINFNM)//'.ERR')
!
!---- open output listing file, RLS ------------------------------------------
!
      OPEN (UNIT=NRLS, FILE=TRIM(HINFNM)//'.RLS', STATUS='UNKNOWN')
!
!  Open and read from the CSM section to get the identifier for the contaminant data (instead of FUI section)
!
      FILTER = .TRUE.
      CALL GETSET(NGID,NERR,CSM,NLINES,SETDATA,SITNUM,FILTER)
!
!       Get number of modules in this case for this site
!
        NUMMOD = GETINT(SETDATA,NLINES,'Nummod        ',SITNUM,IZ,IZ,IZ,IZ,IZ)
!
!  Test value for nummod, must be 1 or greater
!
        IF(NUMMOD.LE.0) THEN
           WRITE(nerr,'(a,I5)') ' Bad value for number of modules found in CSM section of GID file - stop',NUMMOD
           ierr = ierr + 1
           GO TO 999
        ENDIF
!
!     Loop on number of modules defined to find the contaminant module name
!
!        WRITE(NRLS,'(A,I5,A,A)') ' Number of modules = ',NUMMOD,' MODNAME = ', MODNAME
!
        DO IMOD = 1,NUMMOD
!
!          Get module name for current module
!
           MODID = GETSTR(SETDATA,NLINES,'ModId         ',SITNUM,IMOD,IZ,IZ,IZ,IZ)
!           WRITE(NRLS,'(A,I5,A,A)') ' For module: ',IMOD,' MODID = ', MODID
!
!          Test to see if this module is the current module name (this MEPAS exposure module)
!
           IF(SEQI(MODID,MODNAME,5)) THEN
!
!             Get number of source modules attached to this exposure module, including DB module
!
              MODSRCNUM = GETINT(SETDATA,NLINES,'ModSrcNum     ',SITNUM,IMOD,IZ,IZ,IZ,IZ)
!
!             Loop on number of source modules to find "con" module
!
              DO ISRC = 1, MODSRCNUM
!
!                Get type of source module
!
                 MODSRCTYPE = GETSTR(SETDATA,NLINES,'ModSrcType    ',SITNUM,IMOD,ISRC,IZ,IZ,IZ)
!
!                Check to see if current source module is of type "con"
!
                 IF(SEQI(MODSRCTYPE,CON,3)) THEN
!                Get type of source module
!
                 CONI = '     '
                 CONI     = GETSTR(SETDATA,NLINES,'ModSrcID      ',SITNUM,IMOD,ISRC,IZ,IZ,IZ)
                    ICONNUM = ISRC
                    GO TO 7
                 ENDIF
              END DO    ! end loop in number of source modules attached to this exposure module
           ENDIF
        END DO          ! end loop on number of modules in the case
!
!  Error condition at this point.  Contaminant database ID not found
!
      WRITE(nerr,'(a)') ' Contaminant module ID not found in CSM section of GID file - stop'
      IERR = IERR + 1
      GO TO 999
   7  CONTINUE
!
!   Set name for contaminant database ID to "con" plus number (ICONNUM)
!
!     CONI(1:3) = 'con'
!     CONI(5:5) = ' '
!      WRITE(*,'(A)') CONI
!     IF(ICONNUM.LE.9) THEN
!        WRITE(CONI(4:4),'(I1)') ICONNUM
!         WRITE(*,'(A)') CONI
!     ELSE
!        WRITE(CONI(4:5),'(I2)') ICONNUM
!         WRITE(*,'(A)') CONI
!     ENDIF
!
!---- Find FUI section in global data file -----------------------------------
!
!      FILTER = .TRUE.
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,FUI,NLINES,SETDATA,SITNUM,FILTER)
      FILTER = .FALSE.
      IF(NLINES.LE.0) THEN
         WRITE(NERR, 1101 ) 
 1101    FORMAT(' Error in reading FUI data set from GID, not found')
         IERR = IERR + 1
         GO TO 999
      ENDIF
!
!----- REPORT HEADER
!
      WRITE (nrls,'(''Multimedia Environmental Pollutant Assessment'','' System (MEPAS)'')')
      WRITE (nrls,'(''Annual Exposure Version'')')
      WRITE (nrls,'(''MEPAS Human Health Analysis Input Summary'',2x,a19)')VERN
      WRITE (nrls,'(''Pacific Northwest Laboratory'')')
      WRITE (nrls,'(''Richland, WA 99352'')')
      WRITE (nrls,'(''Developed for the U.S. Department of Energy'')')
!
!      WRITE(NRLS,'(A,A)') ' First line of SETDATA ',setdata(1)
!
      CALL DATE_AND_TIME(DATE,TIME,ZONE,DT)
      IYR = DT(1)
      IMO = DT(2)
      IDAY = DT(3)
      CMO = CMONTH(IMO)
      IHR  = DT(5)
      IMINT = DT(6)
      ISEC = DT(7)
!     WRITE(nrls,1111) MMDDYY,HHMMSSHH
      WRITE(nrls,1111) IDAY,CMO,IYR,IHR,IMINT,ISEC
1111  FORMAT(' Run Date and Time: ',I2,'-',A3,'-',I4,3X,I2,':',I2,':',I2)
      WRITE(*,8900) VERN
 8900 FORMAT(' ***** MEPAS Intake Module: ',A,' *******')
!
!---- Get receptor source name -----------------------------------------------
!
       IST = SITNUM
       ICP = RCPNUM
!
!---- Get number of exposure glyphs included in the current case
!
       NUMEXP =  GETINT(SETDATA,NLINES,'ExpNum        ',IST,IZ,IZ,IZ,IZ,IZ)
       NUMUSR =  GETINT(SETDATA,NLINES,'usrNum        ',IST,IZ,IZ,IZ,IZ,IZ)
       NUMSET = 0
       NUMRCPSR = GETINT(SETDATA,NLINES,'RcpSrcNum     ',IST,ICP,IZ,IZ,IZ,IZ)
!       WRITE(nrls,1234) NUMEXP,numusr,numrcpsr
!1234   FORMAT(' NUMEXP,NUMUSR,RcpSrcNum ',3I5)
       DO IRC = 1,NUMRCPSR
         RCPSRCNA(IRC) = GETSTR(SETDATA,NLINES,'RcpSrcName    ',IST,ICP,IRC,IZ,IZ,IZ)
!   get number or transport glphs attached to the source
         DO IEX = 1,NUMEXP
           EXPNAM = GETSTR(SETDATA,NLINES,'ExpName       ',IST,IEX,IZ,IZ,IZ,IZ)
           IF(EXPNAM.EQ.RCPSRCNA(IRC)) THEN
              NUMEXPST = GETINT(SETDATA,NLINES,'ExpTypeNum    ',IST,IEX,IZ,IZ,IZ,IZ)
              IF(NUMEXPST.LE.0) NUMEXPST = 1
              NUMSET = NUMSET + NUMEXPST
           ENDIF
         END DO
!   get number or transport glphs attached to the source
          DO IEX = 1,NUMUSR
            EXPNAM = GETSTR(SETDATA,NLINES,'usrName       ',IST,IEX,IZ,IZ,IZ,IZ)
            IF(EXPNAM.EQ.RCPSRCNA(IRC)) THEN
               NUMEXPST = GETINT(SETDATA,NLINES,'usrTypeNum    ',IST,IEX,IZ,IZ,IZ,IZ)
               IF(NUMEXPST.LE.0) NUMEXPST = 1
               NUMSET = NUMSET + NUMEXPST
            ENDIF
          END DO
       END DO
!
!---- Get receptor data set from GID file ------------------------------------
!
!      WRITE(nrls,1235) NUMSET,MODNAME
!1235   FORMAT(' NUMSET ',I5,' Module name ',a)
      FILTER = .TRUE.
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,MODNAME,NLINES,SETDATA,SITNUM,FILTER)
      IF(NLINES.LE.0) THEN
         WRITE(NERR, 1100 ) 
 1100    FORMAT(' Error in reading run data set from GID, not found')
         IERR = IERR + 1
         GO TO 999
      ENDIF
!
!----- Read default values from GID run data set. -----------------------------
!
      CALL BLKREAD(SETDATA,NLINES,EPATH)
!
!----- Read input record type 2, exposure pathway selection flags
!
       DO IP = 1,25
          KEXPTH(IP) = GETINT(SETDATA,NLINES,'KEXPTH        ',IP,IZ,IZ,IZ,IZ,IZ)
       END DO
        WRITE(nrls,8803)  (KEXPTH(I),I=1,25)
 8803   FORMAT(' KEXPTH',25(I1,','))
!
!----- Read input record type 3, control parameters ------------------
!
        IERR=0
!
!----- Read flag for inhalation impact analysis (air concentration or average daily intake)
!
        INHALE = 0
        INHALE = GETINT(SETDATA,NLINES,'INHALE        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(INHALE.LE.0.AND.INHALE.GT.1) THEN
            WRITE(NERR,*) ' Error in value specified for INHALE, must be 0 or 1, found ',INHALE
            GO TO 999
        ENDIF
        IF(INHALE.EQ.0) THEN
            WRITE(NRLS,*) ' Inhalation impacts from chemicals will be based on air concentrations'
        ELSE
            WRITE(NRLS,*) ' Inhalation impacts from chemicals will be based on average daily intake'
        ENDIF
!
!----- Turn on all transport pathways so necessary factors are generated. ---------
!----- At this point, it is not known what transport or exposure pathways
!----- will be included in the input EPF file.  All pathways must be set up.
!
        DO IP = 1,5
          KNPATH(IP) = 1
        END DO
        KNPATH(4) = 0
!        WRITE(nrls,8802) (KNPATH(I),I=1,5)
! 8802   FORMAT(7(I2,','))
!
!----- Test pathway selection values, print error message ---------------
!
        NUMPTH = 5
!
!----- Write lines describing special conditions for this run ------------
!
          WRITE(nrls,2201)
 2201     format(' Summary Intake Factors calculated for all cases')
!
!   Write line for use for chemicals toxicity output
!
          WRITE(nrls,2205)
 2205     FORMAT(' Perform chemical impact analyses for all toxicity types')
!
!----- Define usage location data for each transport pathway to be used
!----- in the analysis for this site
!
      DO 20 IPATH=1,5
        IF(KNPATH(IPATH).GT.0) THEN
          CALL GETINTAK(SETDATA,NLINES,IPATH,IERR)
!          WRITE(nrls,'(a,2I5)') ' Return from GETINTAK, ipath, ierr: ',IPATH,IERR
          CALL USEDAT(SETDATA,NLINES,IPATH,IERR)
!          WRITE(nrls,'(a,2I5)') ' Return from USEDAT, ipath, ierr: ',IPATH,IERR
        ENDIF
        IF(IERR.GT.0) GO TO 999
   20 CONTINUE
!
! *************************************************************************
!
!----- Write header lines to RIF file ----------------------------------------
!
       WRITE(NRIF,6000)
 6000  FORMAT(' 1,'/' Receptor Intake File generated by MEPAS 4.0 intake module')
!
!----- Write number of sets to RIF file --------------------------------------
!
!       WRITE(NRIF,7003) NUMSET
! 7003  FORMAT(I3,',')
!
!---- LOOP ON EXPOSURE SOURCES FOR THIS RECEPTOR -----------------------------
!
    DO IR = 1, NUMRCPSR
!
!---- Read environmental concentration data for this usage location
!
!---- Open EPF file ----------------------------------------------------------
!
      OPEN (UNIT=NEPF, FILE=TRIM(GIDFNM)//'.EPF',STATUS='UNKNOWN')
!
!---- Find marker data set in EPF file
!
        CALL MARKIN(RCPSRCNA(IR),foundm)
          IF(.NOT.FOUNDM) THEN
             IERR = IERR + 1
             GO TO 999
          ENDIF
!
!---- Read header lines for this set -----------------------------------------
!
        READ(NEPF,*) NLNS
        DO IL = 1,NLNS
           READ(NEPF,*) C
        END DO
        C = C
!
!---- Read number of data sets to consider -----------------------------------
!
       READ(NEPF,*) NSETS
       IF(NSETS.LE.0) THEN
         WRITE(NERR,2001) NSETS
 2001    FORMAT(' Error in EPF file, number of data sets bad =',i5)
         IERR = IERR + 1
         GO TO 999
       ENDIF
!
!----- Write number of sets to RIF file --------------------------------------
!
       WRITE(NRIF,7003) NSETS
 7003  FORMAT(I3,',')
!
!---- Loop on data sets in EPF file ------------------------------------------
!
 88   continue
      DO IS = 1,NSETS
!
!---- Read first line of EPF data set and test for inclusion -----------------
!
        NAGES = 1
        READ(NEPF,*,end=995) STYPE,ELOC,MEDTYPE,NPOINTS,NCONST
!       IF(STYPE.NE.'chronic') THEN
       IF(.NOT.SEQI(STYPE,'chronic',7)) THEN           ! Case insensitive test
          NR = LEN_TRIM(STYPE)
          WRITE(NERR,2002) STYPE(1:NR)
 2002     FORMAT(' Error in EPF file, type not chronic: ',A)
          IERR = IERR + 1
          GO TO 999
       ENDIF
        NE = LEN_TRIM(ELOC)
        NM = LEN_TRIM(MEDTYPE)
        WRITE(NRIF,7004) ELOC(1:NE),MEDTYPE(1:NM),NPOINTS,NAGES,NCONST
 7004   FORMAT('"chronic","',A,'","',A,'",',I3,',',I2,',',I3,',')
!
!---- Identify transport medium type, MEDTYPE --------------------------------
!
        TPATH = 0
        DO IT = 1,6
           typest = types(it)
           IF(SEQI(MEDTYPE,TYPEST,14)) THEN
              TPATH = IT
           END IF
        END DO
        IF(TPATH.EQ.4) TPATH = 3
!
!---- Set exposure duration for output to RIF file ---------------------------
!
        IF(TPATH.EQ.1) THEN
          TRELOUT = DGWED
        ELSEIF(TPATH.EQ.2) THEN
          TRELOUT = DSWED
        ELSEIF(TPATH.EQ.3) THEN
          TRELOUT = ATED
        ELSEIF(TPATH.EQ.5) THEN
          TRELOUT = SMED
        ELSEIF(TPATH.EQ.6) THEN
          TRELOUT = FDED                     ! modify for addition of "sediment" analysis
        ENDIF
!
        IF(TPATH.LE.0) THEN
           WRITE(NERR,2005) MEDTYPE,(types(i),i=1,6)
 2005      FORMAT(' Error in EPF, medium type not recognized: ',a/' Valid medium types are:'/&
                  6(2x,a/))
           IERR = IERR +1
           GO TO 999
        ENDIF
!
!---- Read coordinate data ---------------------------------------------------
!
        DO IP = 1,NPOINTS
          READ(NEPF,*) EXPX(IP),UT,EXPY(IP),UR
          WRITE(NRIF,7000) EXPX(IP),EXPY(IP)
 7000     FORMAT(F9.1,',"km",',f9.1,',"km",')
        END DO
!
!---- Write age group bounds for adult
!
        TA1 = tage1
        TA2 = tage2
        IF(tage2.le.0.) ta2 = 70.
        WRITE(NRIF,7002) TA1,TA2
 7002   FORMAT(F5.0,',',F5.0,',"yr",')
!
!---- Do for each constituent in EPF file ------------------------------------
!
        DO IPOL = 1,NCONST
!
!---- Read constituent name, progeny and number of time periods ----
!
        READ(NEPF,*) CONAME,CONID,NPRG,NTIMES
        NCM = LEN_TRIM(CONAME)
        NID = LEN_TRIM(CONID)
        WRITE(NRIF,7001) CONAME(1:NCM),CONID(1:NID),NPRG,NTIMES
 7001   FORMAT('"',A,'","',A,'",',I2,',',I4,',')
!
!---- Call PDATINI to get pollutant specific parameters for constituent ------
!
!    ipdat set to 0 to inhibit printing of pollutant information (too much data)
        IPDAT = 0
!        IPDAT = 1
!
!----- Call GETSET to load GID file information from chemical database use CONI for set name.
!
        FILTER = .TRUE.         ! To include data for current site number NUMSIT  ! Activate for Chem decay
        CALL GETSET(NGID,NERR,CONI,NLINES,SETDATA,SITNUM,FILTER)                  ! Activate for Chem decay
!
!----- Read physical data from GID, contaminant data section for each chain member
!
         IPDAT = 1
         CALL CHEM_DATA_READ(SETDATA,NLINES,CONID,IPDAT,TPATH,IERR)    ! ** Activate for Chem decay & reading from GID-CONi
         FILTER = .FALSE.
         IF(IERR.GT.0) GO TO 999
!
!        CALL PDATINI(CONID,IPDAT)
        KT = MTYPE(1)
        NTYPES = 1
        IF(KT.GT.1) NTYPES = 2
!
!---- For each time period read exposure data and calculate intakes ----------
!
        DO ITM = 1,NTIMES
           READ(NEPF,*) TSTART,UT,TREL,UR,NEXPTH
           NUT = LEN_TRIM(UT)
           NUR = LEN_TRIM(UR)
           nexpout = nexpth
           IF(kt.gt.1) nexpout = nexpout * 2
           WRITE(NRIF,7005) TSTART,UT(1:NUT),TRELOUT,UR(1:NUR),NEXPOUT
 7005      FORMAT(F9.2,',"',A,'",',F9.2,',"',A,'",',I3,',')
           DO IEX = 1,NEXPTH
              READ(NEPF,*) EXNAME,EXROUTE,EU
              READ(NEPF,*) (VALIN(I),I=1,NPOINTS)
              NE = LEN_TRIM(EXROUTE)
              NN = LEN_TRIM(EXNAME)
              FINITE = .FALSE.
              IF(SEQI(EU(1:1),'S',1)) FINITE = .TRUE.
!
              DO ITYPE = 1,NTYPES  ! loop on impact types
                mting = itype
                mtinh = itype
                IF(kt.eq.1) then
                   mting = 3
                   mtinh = 3
                endif
!
!              CALL INCALC(EXNAME,EXROUTE,TPATH,TRELOUT,      &
              CALL INCALC(EXNAME,EXROUTE,TPATH,TRELOUT,mting,mtinh,    &
                   NPOINTS,VALIN,VALOUT,POPOUT,ETYPOUT,EUOUT,FOUNDE)
              IF(.NOT.FOUNDE) THEN
                 WRITE(NRLS,2006) EXNAME(1:NN),EXROUTE(1:NE)
 2006            FORMAT(' Error in EPF, exposure pathway unknown: ',A,1X,A)
                 WRITE(NERR,2006) EXNAME(1:NN),EXROUTE(1:NE)
                 WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
 2010            FORMAT(1X,A,A,A,/20(A,A,2x,A,A/))
                 GO TO 995
!                 WRITE(NRIF,3003) POPOUT,EXNAME(1:NN),EXROUTE(1:NE)
! 3003            FORMAT(F10.0,',"',A,'","',A,'","NU","No calculation",')
!                 WRITE(NRIF,3002) (VALOUT(I),I=1,NPOINTS)
              ELSE
                 NU = LEN_TRIM(EUOUT)
                 NT =  LEN_TRIM(ETYPOUT)
                 WRITE(NRIF,3001) POPOUT,EXNAME(1:NN),EXROUTE(1:NE),  &
                         EUOUT(1:NU), ETYPOUT(1:NT)
 3001            FORMAT(F9.0,',"',A,'","',A,'","',A,'","',A,'",')
                 WRITE(NRIF,3002) (VALOUT(I),I=1,NPOINTS)
 3002            FORMAT(1PE9.2, ',',359(E9.2,','))
              ENDIF
!
              END DO  ! End loop on health inpact types (1 for rads, 2 for chems)
!
           END DO   ! Loop on exposure pathways for parent
!
!---- If progeny data, read and process --------------------------------------
!
           IF(NPRG.GT.0) THEN
              DO IPR = 1,NPRG
                 READ(NEPF,*) CONAME,CONID,NEXPTH
                 NCM = LEN_TRIM(CONAME)
                 NID =  LEN_TRIM(CONID)
                 WRITE(NRIF,7001) CONAME(1:NCM),CONID(1:NID),NEXPTH
                 mting = 3
                 mtinh = 3
!
!---- Call PDATINI to get pollutant specific parameters for progeny ----------
!
!     IPDAT set to 0 to inhibit printing of pollutant data (too much data)
                 IPDAT = 0
         CALL CHEM_DATA_READ(SETDATA,NLINES,CONID,IPDAT,TPATH,IERR)    ! ** Activate for Chem decay & reading from GID-CONi
         FILTER = .FALSE.
         IF(IERR.GT.0) GO TO 999
!                 CALL PDATINI(CONID,IPDAT)
                 DO IEX = 1,NEXPTH
                    READ(NEPF,*) EXNAME,EXROUTE,EU
                    READ(NEPF,*) (VALIN(I),I=1,NPOINTS)
                    NE = LEN_TRIM(EXROUTE)
                    NN = LEN_TRIM(EXNAME)
                    CALL INCALC(EXNAME,EXROUTE,TPATH,TRELOUT,mting,mtinh,   &
                      NPOINTS,VALIN,VALOUT,POPOUT,ETYPOUT,EUOUT,FOUNDE)
                    IF(.NOT.FOUNDE) THEN
                 WRITE(NRLS,2006) EXNAME(1:NN),EXROUTE(1:NE)
                 WRITE(NERR,2006) EXNAME(1:NN),EXROUTE(1:NE)
                 WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
                 GO TO 995
!                 WRITE(NRIF,3003) POPOUT, EXNAME(1:NN),EXROUTE(1:NE)
!                 WRITE(NRIF,3002) (VALOUT(I),I=1,NPOINTS)
                    ELSE
                       NU = LEN_TRIM(EUOUT)
                       NT = LEN_TRIM(ETYPOUT)
                       WRITE(NRIF,3001) POPOUT,EXNAME(1:NN),    &
                       EXROUTE(1:NE),EUOUT(1:NU),ETYPOUT(1:NT)
                       WRITE(NRIF,3002) (VALOUT(I),I=1,NPOINTS)
                    ENDIF
                 END DO   ! Loop on exposure pathway
!
              END DO  ! Loop on progeny
!
           END IF  ! If on progeny included
!
        END DO   ! Loop on time periods
!
       END DO  ! Loop on constituents
!
      END DO  !  Loop on number of sets in EPF file, IS
!
    END DO  !  Loop on number of exposure sources
!
995   CLOSE (NPIN)
      ENDFILE (nrls)
      CLOSE (NGID)
      CLOSE (NSIF)
      ENDFILE (NERR)
      
      LENERR = 0
!        INQUIRE (UNIT=NERR,FLEN = LENERR)
	  ISTAT = FSTAT (NERR, statarray)
	  IF (.NOT. ISTAT) THEN
        LENERR = statarray(8)
      ENDIF
	  IF(LENERR.GT.0) THEN
        CLOSE (NERR)
!        WRITE(NRLS,'(A)') ' Error file closed, not deleted ')
      ELSE
        CLOSE (NERR,STATUS="DELETE")
!        WRITE(NRLS,'(A)') ' Error file closed and deleted ')
      ENDIF
      CLOSE (nrls)
!
   85 CONTINUE
      GOTO 999
!
!----- Print error messages for input end-of-file errors
!
!----- Format Statements ------------------------------------------------
!
  100 FORMAT(7I3,I5)
  102 FORMAT(I3,I6,5I3,I6)
  104 FORMAT(6I3,i6)
  101 FORMAT(1X,5I3)
  103 FORMAT(25I2)
  200 FORMAT(3A4,E10.3,I3,I3)
  300 FORMAT(E10.2,I5,2E10.2)
  310 FORMAT(2X,3I5,E10.3)
  320 FORMAT(2X,2I5,2X,10X,2A4,2X,4E10.3)
  400 FORMAT(2X,I5,37X,E10.1)
  500 FORMAT(A80)
 1000 FORMAT(' ERROR IN RECORD TYPE 3, PATHWAY ',I2,' HAS BAD',        &
       ' VALUE FOR NUMBER OF USAGE LOCATIONS.  VALUE READ =',I3)
 1002 FORMAT(' END-OF-FILE ENCOUNTERED ON UNIT',I3,' WHILE READING '   &
      /' AIR AND SOIL CONCENTRATION DATA - STOP')
 1003 FORMAT(' END-OF-FILE ENCOUNTERED ON UNIT',I3,' WHILE READING '   &
      /' WATER CONCENTRATION DATA - STOP')
 1004 FORMAT(' END-OF-FILE ENCOUNTERED ON UNIT',I3,' WHILE READING '   &
      /' RECORD TYPE 3 - STOP')
 1005 FORMAT(' END-OF-FILE ENCOUNDTERD ON UNIT',I3,' WHILE READING '   &
      /' RECORD TYPES 1 AND 2 - STOP')
 1006 FORMAT(' ERROR IN VALUE GIVEN FOR NUMBER OF TIME PERIODS,',' NPER =',I4)
 1007 FORMAT(' ERROR READING WATER FILE FOR POLLUTANT ',I5,2X,2A4)
 1008 FORMAT(' .WIN WASTE UNIT NUMBER DOES NOT MATCH ORDER, EXPECTED', &
       I3,', BUT FOUND',I3)
 1009 FORMAT(' END-OF-FILE ENCOUNTERED ON UNIT',I3,' WHILE READING '   &
      /' AIR RELEASE RATE DATA - STOP')
 2101 FORMAT( ' Number of Constituents is:',I3)
!
! *************************************************************************C
!
  999 CONTINUE
      END
!=================== END OF MODULE HAZ2 =====================================
!   MEPAS HAZ2: EXFCT.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           FUNCTION EXFCT                                   *
!                                                                            *
!  FUNCTION EXFCT Derived from a module of LADTAP II, LATEST                 *
!                   MODIFICATION - FEBRUARY 4, 1985                          *
!                   FUNCTION EXFCT EVALUATES THE EXPRESSION 1.-EXP(-X)       *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE RISKF
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     EXFCT     S              FUNCTION  FUNCTION VALUE RETURNED TO
!                                        CALLING PROGRAM
!     X         U              ARGUMENT  ARGUMENT FOR EVALUATION OF THE
!                                        FUNCTION, DIMENSIONLESS
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!
!==== SUBROUTINE CALL ========================================================
!
      FUNCTION EXFCT (X) 
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
!
!----- START CALCULATIONS, SET DEFAULT FUNCTION VALUE
!
      EXFCT=1.                                                      
!
!     ---- FOR ARGUMENT >= 30. RETURN A VALUE OF 1.0
!
      IF (X.GE.30.) RETURN                                          
      IF (X.LE.0.01) GO TO 1                                        
!
!     ---- ARGUMENT IS BETWEEN 0.01 AND 30., EXP MAY BE USED
!
      EXFCT=1.0-EXP(-X)                                             
      RETURN                                                        
!
!     ---- FOR ARGUMENT <= 0.01 USE APPROXIMATION FOR 1-EXP(-X)
!
  1   IF(X.LT.1.E-6) THEN
        EXFCT = X
      ELSE
        X2=X*X                                                      
        X3=X*X2                                                     
        X4=X*X3                                                     
        EXFCT=X+X3/6.0-X2/2.0-X4/24.0                               
      ENDIF
      RETURN                                                        
!
!============ END OF MODULE EXFCT ============================================
      END                                                           

!   MEPAS HAZ2: ARETEN.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ARETEN                                *
!                                                                            *
!  Function ARETEN calculates the soil accumulation factor for the air       *
!      exposure pathway.  The factor is to include the total uptake by       *
!      ingestion over a 70 year period from one year of deposition.          *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    09/24/85 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE EXPOS
!     Calls: NONE
!     Common blocks referenced:
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!
!     AFS             S    Real  Argument  Soil retention factor
!
!     ALD             U    Real  Argument  Physical decay constant for the
!                                         current pollutant, days-1.  This
!                                         is the input value.
!     ALY            S/U   Real  Internal  Same as ALD in units of year-1.
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ARETEN(ALD,AFS)
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!     NONE
!
!
!--- Start of calculations ----------------------------------------------
!
      AR = 1.0
      ALY = ALD * 365.25
!
!--- Loop over the remaining 69 years -----------------------------------
!
      DO 10 N = 1,69
      ARG = ALY * FLOAT(N)
      IF(ARG.GT.16) GO TO 20
      AR = AR + EXP(-ARG)
   10 CONTINUE
   20 AFS = AR / 70.
      RETURN
!
!======== END OF MODULE ARETEN ===========================================
      END
!   MEPAS HAZ2: GDK.FOR             Version Date: 11-24-1991
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GDK                                   *
!                                                                            *
!  Subroutine GDK calculates the deposition decay factor for a period of T   *
!                 days and returns a decay factor for eacy pollutant in      *
!                 array GDKFAC.                                              *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    09/19/91 (Converted to PC)                              *
!  Last Modified:    30-July-1992  DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: Main Program
!     Calls: NONE
!     Common blocks referenced: PSET1, PSET3, DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     GDKFAC(ip)  S    REAL    Argument  Decay correction factor for dep-
!                                        osition to ground for a period of
!                                        T days.  Units are years.
!
!     NPOL        U    INT.    PSET1     Number of pollutant considered.
!
!     SLAM(ip)    U    REAL    PSET3     Decay constant for eacy pollutant
!                                        in soil, 1/days.
!
!     T           U    REAL    Argument  Time period over which deposition is
!                                        accumulated and decayed, days.
!
!     YPD         S/U  REAL    Internal  Units conversion factor, year/day.
!
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!
!     28-JUL-92    DLS  Modification of heading information
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GDK(T,GDKFAC)
!
!==== COMMON Block Definitions ===============================================
!
        include 'PSET1.FTN'
        include 'PSET3.FTN'
        include 'DEVICE.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION GDKFAC(20)
!
!==== Variable Declarations ==================================================
!
!    NONE
!
!==== DATA Statements ========================================================
!
!   Number of terms in exponential approximation
      DATA NTERMS/11/
!   Argument test limit
      DATA XXX/0.001/
!   Years per day
      DATA YPD/0.00273785/
!
!----- Start calculations. Initialize daily intake array ----------------
!
!   Loop over number of pollutants in input list.
!
      DO 1001 IP = 1, NPOL
!
!   Test for non-zero value of SLAM(IP) to avoid division by zero.  If zero,
!   then set GDKFAC to zero and write warning message.
        IF(SLAM(IP).LE.0.) THEN
          GDKFAC(IP) = 0.0
          WRITE(NRLS,1000) IP, SLAM(IP)
 1000     FORMAT(' Invalid value for soil decay constant in GDK',  &
        ' for pollutant',I3,1PE10.3)
        ENDIF
!
!  Test magnitude of argument to determine method of analysis.
!
        ARG = T * SLAM(IP)
        IF(ARG.LT.XXX) THEN
!
!  For small arguments, use special equation to avoid loss of
!  significant figures.
!
          SUM = 0.5
          DENOM = 2.0
          DO 1002 IT = 3,NTERMS
            DENOM = DENOM*FLOAT(IT)
            FAC = -1./DENOM
            SUM = SUM + FAC * ARG
1002      Continue
           GDKFAC(IP) = SUM * T * YPD
        ELSE
!
!  For larger arguments, use standard equation.
          OME = 1. - EXP(-ARG)
          TERM = OME/ARG
          GDKFAC(IP) = (1.- TERM)/SLAM(IP) * YPD
        ENDIF
!
!  End of pollutant loop.
1001  Continue
      RETURN
      END
!================= END OF MODULE GDK =====================================
!   MEPAS HAZ2: DERMAL.FOR            Version Date: 06-Dec-95
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE DERMAL                                *
!                                                                            *
!  Subroutine DERMAL  This subroutine calculates the intake of a pollutant   *
!                     through the skin as intake per event.  For chemicals   *
!                     output units are mg/d/cm2 per event, and for radio-    *
!                     nuclides the output units are pCi/d/cm2 per event.     *
!                     The medium is water.                                   *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    10/01/92                                                *
!  Last Modified:    06-Dec-1995  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: GROUND, SURFWT
!     Calls:  None
!     Common blocks referenced: PSET1, DERMDAT
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     IPOL      U     Integer  Argument  Pollutant index for analysis
!     ITYPE     U     Integer  Argument  Type of exposure: 1-showering,
!                                                2-swimming
!     TEFFCT    S     Real     Argument  Effective intake duration (hr)
!     PI        S/U   Real     Internal  Numerical value for pi
!     CWCM      S/U   Real     Internal  Water conc. in working units (1/cm3)
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!    05-Oct-92     DLS  Commented out testing print statement
!    03-AUG-93     DLS  Add option to use inorganic model for all dermal intake
!    15-Dec-93     DLS  Changed output parameter to effective exposure time,
!                       as parameter TEFFCT.  Removed CW from input.
!    17-Dec-93     DLS  Updated index pointers to dermal pathways to be consistent
!                       with current exposure pathway definitions
!    29-Dec-93     DLS  Removed units conversion (0.001 L/cm3) so it can be
!                       applied at time of generation of SIFs.
!    06-Dec-95     DLS  Removed IPOL from call list because is always 1
!=== SUBROUTINE CALL ========================================================
!
      SUBROUTINE DERMAL(ITYPE,TEFFCT)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE  'PSET1.FTN'
      include  'DERMDAT.FTN'
      include 'pflags.ftn'
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
      DATA PI/3.141592654/
!
      IPOL = 1
      TEFFCT = 0.0
      CWCM = 1.0      ! unit water concentration
!
!  Set event duration: showering (1) or swimming (2)
!
      IF(ITYPE.GT.1)  THEN 
        TEVENT = TESWIM
        KEXP = KEXPTH(11)    ! Swimming dermal exposure flag
      ELSE
        TEVENT = TESHWR
        KEXP = KEXPTH(2)     ! Shower dermal exposure flag
      ENDIF
!
      IF(MTYPE(IPOL).LT.2.OR.CKOW(IPOL).LE.0.0.or.KEXP.GT.1) THEN
!
!  Evaluate effective intake duration for inorganic chemicals and radionuclides
!
        TEFFCT = TEVENT
      ELSE
!
!  Evalute effective intake time for organic chemicals
!
        BCAP = CKOW(IPOL) * 1.E-4                  ! Step 2, Eq. 5.12
        DIFEXP = 0.0061 * WM(IPOL)                 ! Step 3
        DSC = 0.001905 * SKINL * 10**(-DIFEXP)     !         Eq. 5.13
        TAU = (SKINL*SKINL)/(6.*DSC)               ! Step 4, Eq. 5.14
        IF(BCAP.LE.0.1) THEN                       ! Step 5
          TSTAR = 2.4 * TAU                                ! Eq. 5.15
        ELSEIF(BCAP.LE.1.17) THEN
          TSTAR = TAU * (8.4 + 6.*ALOG10(BCAP))            ! Eq. 5.16
        ELSE                                                         
          CEE = (1. + 3.*BCAP)/3.                          ! Eq. 5.19
          BEE = (2./PI)*(1.+BCAP)*(1.+BCAP) - CEE          ! Eq. 5.18
          TSTAR = 6. * TAU * (BEE - SQRT(BEE*BEE - CEE*CEE) )  ! Eq. 5.17
        ENDIF
!
!  Evaluate DEVENT based on TSTAR and TEVENT
!
        IF(TEVENT.LT.TSTAR) THEN                   ! Step 6
          ARG = (6.*TAU*TEVENT)/PI
          TERM = 2.*CWCM
          TEFFCT = TERM*SQRT(ARG)                  ! Eq. 5.20
        ELSE
          TERM1 = CWCM
          TERM2 = TEVENT/(1.+BCAP)
          TERM3 = (1.+3.*BCAP)/(1.+BCAP)
          TEFFCT = TERM1*(TERM2+2.*TAU*TERM3)      ! Eq. 5.21
        ENDIF
      ENDIF
!
      RETURN
      END
!=================== END OF MODULE DERMAL =============================
!   MEPAS AHAZ: FDINPT.FOR             Version Date: 11-Feb-1997
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE FDINPT                                *
!                                                                            *
!  Subroutine FDINPT This subroutine read the input parameters for the       *
!                    exposure pathway of direct ingestion of food, based on  *
!                    measured food concentrations in the vicinity of a site. *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    11-Feb-1997  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: USEDAT
!     Calls: NONE
!     Common blocks referenced: FDPATH, LOCNAM, DEVICE, PSET1
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!     28-Aug-92    DLS  Add input of body weight, exposure duration and
!                       averaging time
!     13-Oct-92    DLS  Remove averaging time from input, set = exposure dur.
!     21-Oct-93    DLS  Add write to *.INT file
!     20-Dec-93    DLS  Added generation of SIFs
!     18-Mar-94    DLS  Set BW and ED when SIFs are input
!     20-Apr-94    DLS  Moved FDED to common block FDPATH
!     24-Sep-94    KJC  Change the way data is written to int file
!     17-Jan-96    DLS  Revised for use in 100-year update
!     11-Feb-97    DLS  Added name "Measured Food" to population output line
!                       in .INA file
!==== SUBROUTINE CALL ========================================================
!
       SUBROUTINE FDINPT(SETDATA,NLINES)
!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'PARMTR.PAR'
        include 'FDPATH.FTN'
        include 'LOCNAM.FTN'
        include 'DEVICE.FTN'
        include 'PSET1.FTN'
        include 'PTHUSE.FTN'
        include 'PFLAGS.FTN'
!
!==== DIMENSION Statements ===================================================
!
!==== Variable Declarations ==================================================
!
      CHARACTER*11 FDUNTS(3)
      CHARACTER*13 FDNAME
      CHARACTER*120 SETDATA(LINEMAX)
!
!==== DATA Statements ========================================================
!
      DATA FDNAME/'Measured Food'/
      DATA FDUNTS/'  kg/kg/d  ','  kg/kg/d  ','     kg    '/
!
      WRITE(NRLS,300) char(12)
      IPATH=6
!        IF(KSIF.LE.1.) THEN
!          IF(KSIF.LE.0.OR.BW.GT.0..OR.ED.GT.0.) THEN
            IF(BW.LE.0.) BW = 70.
            IF(ED.LE.0.) ED = 70.
            FDBW = GETREAL(SETDATA,NLINES,'FDBW          ',IZ,IZ,IZ,IZ,IZ,IZ)
            FDED = GETREAL(SETDATA,NLINES,'FDED          ',IZ,IZ,IZ,IZ,IZ,IZ)
            VAL  = GETREAL(SETDATA,NLINES,'UFD           ',IZ,IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) UFD = VAL
!            VAL  = GETREAL(SETDATA,NLINES,'POPMF         ',IZ,IZ,IZ,IZ,IZ,IZ)
!            IF(VAL.GT.0.) POPMF = VAL
            IF(FDBW.GT.0.) BW = FDBW
            IF(FDED.GT.0.) ED = FDED
            IF(KEXPTH(16).GT.0) THEN
              TERM = FREQ(16) * UFD
              CALL SETSIF(TERM,BW,ED,FDSIF(1),FDSIF(2),FDSIF(3))
            ENDIF
!          ENDIF
!        ELSE       !KSIF = 2
!          IF(BW.LE.0.) BW = 70.
!          IF(ED.LE.0.) ED = 70.
!          FDBW = BW
!          FDED = ED
!        ENDIF
!
  10  CONTINUE
!
!----- Write SIF values
!
      write(NRLS,2300)
 2300 format(' Summary of SIF values used for the special food pathway'  &
       /' Location Name        SIF Non-c   SIF Carc   SIF Rads.','   Source')
        WRITE(NRLS,2301) (LOCNAM(I,IPATH),I=1,5),(FDSIF(J),J=1,3)
        write(NRLS,2303) (FDUNTS(J),J=1,3)
 2301   FORMAT(5A4,1p,3e11.3,3X,A5)
 2303   FORMAT(20X,3A11)
!
        WRITE(nrls,8801)  FDNAME,(LOCNAM(I,IPATH),I=1,5)
 8801   FORMAT('"',A,'","',5A4,'",0,0,0,',15('0.0,'),1PE8.2)
!
  100 FORMAT(5A4,5E10.2)
  200 FORMAT(1X,5A4,1PE10.2,1X,E10.2,2x,E10.2,5x,E10.2,6x,E10.2)
  300 FORMAT(a1,' POPULATION DATA FOR MEASURED FOOD INGESTION',    &
       ' PATHWAY',//'     LOCATION NAME   Intake Rate'             &
      ,' Population  Body Weight  Exp. Duration  ',/               &
                    '                      kg/day    ',            &
       '                 kg           yr     '/)
  400 FORMAT (E10.2)
!
      RETURN
      END
!=================== END OF MODULE FDINPT =================================
!   MEPAS AHAZ: GETNAM.FOR             Version Date: 23-Jun-97
!   Copyright 1989, 1992 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETNAM                                *
!                                                                            *
!  Subroutine GETNAM This subroutine gets the file names from the FACIL.ID   *
!                    file.                                                   *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    23-Jun-1997   DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: AHAZ
!     Calls: NONE
!     Common blocks referenced: DEVICE, COUPLE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!     10-DEC-93    DLS  Modification to read prefix for *.SIF file (rectyp 10)
!     23-Jun-97    DLS  Modified to read FRAMES arguments.
!==== SUBROUTINE CALL ========================================================
!
!      SUBROUTINE GETNAM()
!
!==== COMMON Block Definitions ===============================================
!
!        include 'DEVICE.FTN'
!        include 'COUPLE.FTN'
!
!==== DIMENSION Statements ===================================================
!
!       CHARACTER*128 RECIN
!       CHARACTER*1 RECTYP
!
!==== Variable Declarations ==================================================
!
!==== DATA Statements ========================================================
!
!     NONE
!
!      OPEN (NTMP, FILE='FACIL.ID', STATUS='OLD')
!
!  Find input record with RECTYP = 1: Name for FUIname
!
!   10 READ (NTMP,1003) RECIN
!      READ (RECIN,1004) RECTYP
!      IF (RECTYP.NE.'1') GOTO 10
!      READ (RECIN,1005) GIDFNM
!      REWIND (NTMP)
!
!  Find input record with RECTYP = 2: Name for HINFNM
!
!   11 READ (NTMP,1003) RECIN
!      READ (RECIN,1004) RECTYP
!      IF (RECTYP.NE.'2') GOTO 11
!      READ (RECIN,1005) HINFNM
!      REWIND (NTMP)
!!
!!  Find input record with RECTYP = 3: Number of Site. SITNUM
!!
!   12 READ (NTMP,1003) RECIN
!      READ (RECIN,1004) RECTYP
!      IF (RECTYP.NE.'3') GOTO 12
!      READ (RECIN,1001) SITNUM
!      REWIND (NTMP)
!!
!  Find input record with RECTYP = 4: Number of Site. RCPNUM
!
!   13 READ (NTMP,1003) RECIN
!      READ (RECIN,1004) RECTYP
!      IF (RECTYP.NE.'4') GOTO 13
!      READ (RECIN,1001) RCPNUM
!      REWIND (NTMP)
!
!  Find input record with RECTYP = 5: Name for NAME
!
!   14 READ (NTMP,1003) RECIN
!      READ (RECIN,1004) RECTYP
!     IF (RECTYP.NE.'5') GOTO 14
!      READ (RECIN,1005) NAME
!      REWIND (NTMP)
!!
! 1001 FORMAT (3X,I3)
! 1002 FORMAT (3X,A8,2X,A8,2X,A8,2X,A8)
! 1003 FORMAT (A128)
! 1004 FORMAT (1X,A1)
! 1005 FORMAT (3X,A128)
!!
!      CLOSE (NTMP)
!
!      RETURN
!      END
!
!====================== END OF MODULE GETNAM =============================
!
!   MEPAS HAZ2: SLINPT.FOR             Version Date: 11-Feb-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SLINPT                                *
!                                                                            *
!  Subroutine SLINPT This subroutine read the input parameters for the       *
!                    exposure pathway ofdirect ingestion of soil, based on   *
!                    measured soil concentrations in the vicinity of a site. *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    01/19/89 (Converted to PC)                              *
!  Last Modified:    11-Feb-97 DLS                                           *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: SLPATH, LOCNAM, DEVICE, PSET1
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     28-JUL-92    DLS  Modification of heading information
!     27-Aug-92    DLS  Added input of BW, ED, AT, and US
!     12-Oct-92    DLS  Removed AT from input, set equal to ED
!     21-Oct-93    DLS  Add write to *.INT file
!     20-Dec-93    DLS  Added generation of SLSIF values
!     10-Feb-94    DLS  Final changes for SIF upgrade
!     18-Mar-94    DLS  Set ED, BW, and US when SIFs are input
!     10-Oct-94    DLS  Added agricultural pathways (5-8)
!     04-Dec-95    DLS  Expanded intake parameters for receptor location
!     11-Feb-97    DLS  Added name "Soil" to population data output line in
!                       .INA file
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SLINPT(SETDATA,NLINES)
!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'PARMTR.PAR'
        include 'SLPATH.FTN'
        include 'LOCNAM.FTN'
        include 'DERMDAT.FTN'
        include 'DEVICE.FTN'
        include 'PSET1.FTN'
        include 'PTHUSE.FTN'
        include 'PFLAGS.FTN'
!
!==== DIMENSION Statements ===================================================
!
!==== Variable Declarations ==================================================
!
      CHARACTER*5 SMTYPE(3)
      CHARACTER*11 SMUNTS(3,8)
      CHARACTER*(*) SETDATA(LINEMAX)
      INTEGER IZ, KMSPTH(8)
      REAL(KIND=4) :: BWONE                                           ! 26-Apr-01
      REAL(KIND=4), DIMENSION(2,2) :: SIFXXX                          ! 26-Apr-01
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
      DATA KMSPTH/4,5,6,7,14,15,19,23/
      DATA C10EM3/0.001/,C10EM6/1.E-6/,C365/365.25/
      DATA SMTYPE/'Non-c','Carc.','Rad. '/
      DATA SMUNTS/'  kg/kg/d  ','  kg/kg/d  ','    kg     ', &   !DS
                  '  kg/kg/d  ','  kg/kg/d  ','    kg     ', &   !DD
                  '  kg/kg/d  ','  kg/kg/d  ','    kg     ', &   !DI
                  '           ','           ','    h      ', &   !DE
                  '  kg/kg/d  ','  kg/kg/d  ','    kg     ', &   !LV
                  '  kg/kg/d  ','  kg/kg/d  ','    kg     ', &   !OV
                  '  kg/kg/d  ','  kg/kg/d  ','    kg     ', &   !MT
                  '  kg/kg/d  ','  kg/kg/d  ','    kg     '/     !MK
      DATA BWONE/1.0/                                                 ! 26-Apr-01
!
      WRITE(nrls,300) char(12)
      BW = GETREAL(SETDATA,NLINES,'BODYWT        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ED = GETREAL(SETDATA,NLINES,'EXPDUR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      US = GETREAL(SETDATA,NLINES,'SMUS          ',IZ,IZ,IZ,IZ,IZ,IZ)
!       DO IP = 1,8
!         IPX = KMSPTH(IP)
!         VAL = GETREAL(SETDATA,NLINES,'POPMS         ',IPX,IZ,IZ,IZ,IZ,IZ)
!         IF(VAL.GT.0.) POPMS(IP) = VAL
!       END DO
!        SMED = 70.
      IF(ED.GT.0.) THEN
         SMED = ED
      ELSE
         IERR = IERR + 1
         WRITE(NERR,'(A,1P,E10.3)') ' Error in exposure duration, must be > 0: ',ED
      ENDIF
!
      IF(BW.LE.0.) THEN
         IERR = IERR + 1
         WRITE(NERR,'(A,1P,E10.3)') ' Error in body weight (soil), must be > 0: ',BW
      ENDIF
!
!----- Calculate SIFs for Soil Ingestion Pathway (14) -----------------
!
              IF(USLSL.LE.0.) USLSL = US                  ! 4/Dec/95
              TERM = FREQ(14)*USLSL*C10EM3                    ! 4/Dec/95
              CALL SETSIF(TERM,BW,ED,SLSIF(1,1),SLSIF(2,1),SLSIF(3,1) )
!
!----- Calculate SIFs for Soil Dermal Pathway (15) -----------------
!
              IF(UVSLSL.LE.0.) UVSLSL = EVSOIL            ! 4/Dec/95
              TERM = FREQ(15)*ADHFAC*ASKIN(3)*C10EM6*UVSLSL   ! 4/Dec/95
              CALL SETSIF(TERM,BW,ED,SLSIF(1,2),SLSIF(2,2),SLSIF(3,2) )
!
!----- Calculate SIFs for Soil Resuspension Inhalation (19) -----------
!
              IF(UBRESL.LE.0.) UBRESL = UBRES             ! 4/Dec/95
              TERM = FREQ(19) *        UBRESL                  ! 3/Jun/98
              CALL SETSIF(TERM,BW,ED,SLSIF(1,3),SLSIF(2,3),SLSIF(3,3) )
              IF(INHALE.EQ.0) THEN                                               ! 26-Apr-01
                 TERM = FREQ(19)                                  ! 3/Jun/98     ! 26-Apr-01
                 CALL SETSIF(TERM,BWONE,ED,SLSIF(1,3),SLSIF(2,3),SIFXXX(1,1) )   ! 26-Apr-01
              ENDIF                                                              ! 26-Apr-01
!
!----- Calculate SIFs for Soil External Pathway (23) -----------------
!
              IF(UEXTSL.LE.0.) UEXTSL = UEXT               ! 4/Dec/95
              TERM = FREQ(23)*UEXTSL*(SHO*FTO+SHI*FTI)         ! 4/Dec/95
              SLSIF(3,4) = TERM * ED * C365
!
!----- Calculate SIFs for Food Ingestion Pathways (4,7) -----------------
!
            DO ICRP = 4,7
                ICP = ICRP + 1  ! ag path index in SLSIF array
                IF(UAGSL(ICRP-3).LE.0.) UAGSL(ICRP-3)=UAG(ICRP-3) ! 4/Dec/95
                TERM = FREQ(ICRP)*UAGSL(ICRP-3)                 ! 4/Dec/95
                CALL SETSIF(TERM,BW,ED,SLSIF(1,ICP),SLSIF(2,ICP),SLSIF(3,ICP) )
            END DO
!
        WRITE(nrls,200) (LOCNAM(I,5),I=1,5),bw,ed,ED,USLSL
!
  10  CONTINUE
!
!----- Write headings for SIF output values
!
      WRITE(nrls,2300) 
 2300 FORMAT(' Pollutant ','       Soil Exposure Pathways'/          &
             '   Type    ',' Ingestion     Dermal   Resuspension',   &
             '   External',' Agricultural (lv, ov, mt, mk)')
!
!----- Write SIFs for current location, 8 across, for the 8 pathways
!
!
        write(nrls,2301) SMTYPE(1),(SLSIF(1,J),J=1,8)
        write(nrls,2303) (SMUNTS(1,J),J=1,8)
 2303   format('  Units   ',8A11)
        write(nrls,2301) SMTYPE(2),(SLSIF(2,J),J=1,8)
        write(nrls,2303) (SMUNTS(2,J),J=1,8)
        write(nrls,2301) SMTYPE(3),(SLSIF(3,J),J=1,8)
        write(nrls,2303) (SMUNTS(3,J),J=1,8)
 2301   format(3x,A5,3x,1p,8e11.3)
!
  100 FORMAT(5A4,5E10.2)
  200 FORMAT(1X,5A4,1PE10.2,3x,e10.2,3(6x,e10.2))
  300 FORMAT(a1,' DATA FOR MEASURED SOILS INGESTION',       &
       ' PATHWAY',//'    BODY WEIGHT kg',                   &
       ' EXP DURAT. (y)  AVE. TIME (y)   INTAKE g/d ',/)
       RETURN
      END
!
!============== END OF MODULE SLINPT ========================================
!   MEPAS HAZ2: PERMK.FOR             Version Date: 4-Feb-1993
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           FUNCTION PERMK                                   *
!                                                                            *
!  FUNCTION PERMK: Calculation of skin permeability constant from Octanol-   *
!                  water partition coefficient and molecular weight
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    25-Aug 1992                                             *
!  Last Modified:     4-Feb-1993  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE PDATIN
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     CKOW       U     Real    Argument  Octanol-water partition coefficient
!     RMW        U     Real    Argument  Molecular weight of the chemical
!     LCKOW      S/U   Real    Internal  Log of octanol-water part. coeff.
!     KEXP       U     Integer Argument  Flag for model selction:
!                                        1 - EPA, 2 - McKone (if Kow, MW ok)
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     4-Feb-93     DLS  Add McKone model, and KEXP to argument list
!==== SUBROUTINE CALL ========================================================
!
      FUNCTION PERMK(CKOW,RMW,KEXP)
!
!==== COMMON Block Definitions ===============================================
!
!     NONE
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
      REAL LCKOW
!
!==== DATA Statements ========================================================
!
      DATA SKINCM/0.0025/
!
!  Test KEXP to determine model selected.
!  McKone model for permeability constant for water contact from Risk Analysis
!  Journal, December 1992, Equations 34 and 36.
      IF(KEXP.EQ.2) THEN      ! McKone model selected.  Use if possible
        IF(CKOW.LE.1.E3.AND.RMW.LE.280.) THEN
          PERMK = RMW**(-0.6) * (2.4E-6 + 3.E-5 *CKOW**(0.8))/SKINCM
        ELSE
          PERMK = 0.0
        ENDIF
      ENDIF
      IF(KEXP.EQ.1.OR.PERMK.LE.0.0) THEN
!
! Correlations from EPA Dermal Exposure Assessment: Principles and Applications
!  EPA/600/8-91/011B January 1992, Equation 5-8
!
!  Calculate log of octonal water partition coefficient
!
        LCKOW = ALOG10(CKOW)
!
!  Calculate exponent
!
        EXPON = -2.72 + 0.71*LCKOW - 0.0061 * RMW
!
        PERMK = 10.**EXPON      
!
      END IF
      RETURN
!====== END OF MODULE PERMK ===================================================
      END
!   MEPAS HAZ2: BLKDAT.FOR             Version Date: 29-Apr2006
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           BLOCK DATA BLKDAT                                *
!                                                                            *
!  BLOCK DATA BLKDAT This data block contains default parameter              *
!                    values for RAPS.                                        *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    02/08/86 (Converted to PC)                              *
!  Last Modified:    29-Apr-2006  MAS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: CRLOC, AGCLAS, AQCLAS, DEVICE, RECLAS, PTHUSE, CRPATH, RISKCM
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     variable        S    CHAR  Argument  nnnnnnnnnnnnnnnnnnnnnn
!                     U    INT   Internal
!                     S/U  REAL  Common
!                          DBLE  External
!
!==== Modification History ===================================================
!
!     Date          Who  Modification Description
!     --------      ---  ------------------------------------------------------
!     1 April 1986  DLS  Added definition of DEVICE logical units
!     28-JUL-92     DLS  Modification of heading information
!     29-OCT-92     DLS  Added RFDLIM value for common block RISKCOM
!     05-NOV-92     DLS  Removed UWB, added USOIL, UBRSH.
!                        Changed values for UAG, UDW, ASKIN for average exp.
!     25-NOV-92     DLS  Added DRPATH.FTN and TEDIRD definition
!     3-AUG-93      DLS  Changed shower ingestion to rate (L/hr) from vol. L
!                        and added Andelman model parameters, ANDFC, ANDFR
!     20-Aug-93     DLS  Added TXT unit number to data for HPORNL
!     06-Oct-93     DLS  Added shoreline/resuspension/boating parameters for
!                        common blocks DERMDAT and PTHUSE
!     21-OCT-93     DLS  Added NINT to common block DEVICE parameters
!     25-OCT-93     DLS  Set resuspension factor to 1.E-6 /m
!     28-Dec-93     DLS  Added SWF, SBRF, and SSRF (PTHUSE)
!     30-Dec-93     DLS  Added SHO,FTO,SHI,FTI to PTHUSE definitions
!     22-Mar-94     DLS  Set soil thickness to 0.04 (sediment, soil, measured
!                        soil) for consistency with use of HEAST external dose
!                        slope factors in PDATIN.
!     10-Oct-94     DLS  Added USL, FFC and FFW in CRPATH parameters
!     22-Feb-95     DLS  Set rad. effects factors to 630 and 730 (ICRP 60)
!        "           "   Set resuspension factor to 1.E-8
!        "           "   Set shoreline sediment ingestion rate to .1 g/hr
!     21-Nov-95     DLS  Added NERR to common block DEVICE
!     21-Nov-95     DLS  Added FIRR to common block CRLOC
!     28-Nov-95     DLS  Added NPRM to common block DEVICE
!     28-Nov-95     DLS  Added common block LEACH to BLKDAT
!     13-Dec-95     DLS  Changed air input file unit from NCNC to NARR
!     04-Jan-96     DLS  Added NSOIL to common block DEVICE
!     17-Jan-96     DLS  Added NFOOD to common block DEVICE
!     29-Apr-06     MAS  Added adherence factor for sediment (ADHSED)
!==== SUBROUTINE CALL ========================================================
!
      BLOCK DATA BLKDAT
!
!==== COMMON Block Definitions ===============================================
!
	include 'CRLOC.FTN'
	include 'AGCLAS.FTN'
	include 'AQCLAS.FTN'
	include 'AQPATH.FTN'
	include 'DEVICE.FTN'
	include 'DWPATH.FTN'
	include 'HPORNL.FTN'
	include 'RECLAS.FTN'
	include 'PTHUSE.FTN'
	INCLUDE 'CRPATH.FTN'
	INCLUDE 'RISKCM.FTN'
	INCLUDE 'DERMDAT.FTN'
	INCLUDE 'DRPATH.FTN'
	INCLUDE 'LEACH.FTN'
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
!----- Parameter definitions for COMMON Block AGCLAS ---------------------
!
      DATA NAGC/30/
!
!----- Parameter definitions for COMMON Block AQCLAS ---------------------
!
      DATA NAQC/10/
!
!-----  Parameter definitions for COMMON Block AQPATH --------------------
!
      DATA TFSH(2)/10./
      DATA TINV(2)/10./
!
!----- Parameter definitions for COMMON Block CRPATH ---------------------
!
      DATA YLD/2.0, 2.0, 0.7, 0.7/
      DATA TCRP/2.,60.,20.,4./
      DATA RET/0.25/,DEN/240./
      DATA UMT,UMK,WMT,WMK/68.,55.,50.,60./
      DATA TRN/1.0,0.1,0.1,1.0/
      DATA FFC/1.0,1.0/,FWC/1.0,1.0/  ! Added 10-Oct-94
      DATA USL/0.5,0.5/     ! Animal soil intake, kg/d, meat and milk
!
!----- Logical I/O unit definitions for COMMON Block DEVICE --------------
!
      DATA NGID,NRLS,NEPF,NDBLU,NAIN,NPIN/5,6,12,10,2,8/
      DATA NFAC,NARR,NTMP,INUNIT,NRIF/4,7,9,16,17/
      DATA NSIF,NERR,NPRM,NSOIL,NFOOD,NCIN/18,19,20,21,22,23/
!
!----- Parameter definitions for COMMON Block DWPATH
!
      DATA TWAT/3*0.5/
!      DATA LTRTL/3*0/
!
!----- Parameter definitions for COMMON Block PTHUSE ---------------------
!
      DATA (FREQ(I),I=1,10)/1.,1.,1.,1.,1.,1.,1.,1.,1.,1.0/
      DATA (FREQ(I),I=11,20)/1.,1.,1.,1.,1.,1.,1.,1.,1.,1./
      DATA (FREQ(I),I=21,25)/1.,1.,1.,1.,1./
      DATA UDW(1)/2.0/
      DATA UDW(2)/2.0/
      DATA UDW(3)/2.0/
      DATA UBW/0.06/              ! Inadvertent shower ingestion rate, L/hr
      DATA UBR/20./               ! Inhalation rate, m3/d
      DATA USW/0.10/              ! Inadvertent ingestion swimming, L/h
      DATA (UHR(I),I=1,3)/ 0.033,  0.033, 0.033/ ! exposure times, h/d
      DATA (UAG(I),I=1,4)/ 0.021, 0.13, 0.065, 0.075/  ! Food ingestion, kg/d
      DATA (UAQ(I),I=1,2)/ 0.0065, 0.0027/   ! Aquatic food ingestion, kg/d
      DATA UBRSH/20./             ! Inhalation while showering, m3/d
      DATA UBRES/20./             ! Inhalation for resuspension, m3/d
      DATA RESFAC/1.E-8/          ! Resuspension factor, m-1
      DATA USOIL/.100/            ! Soil ingestion rate, g/d
      DATA USED/0.100/            ! Sediment ingestion rate, g/hr
      DATA UEXT/24./              ! Daily external exposure time, hr/d
      DATA SWF/0.2/               ! Shore width factor, -
      DATA SBRF/0.5/              ! Boating external dose reduction factor
      DATA SSRF/1.0/              ! Soil external dose reduction factor
      DATA SHI/1.0/,FTI/0.83/     ! Indoor shielding factor and time fraction
      DATA SHO/1.0/,FTO/0.17/     ! Outdoor shielding factor and time fraction
!
!----- Parameter definitions for COMMON Block RISKCOM---------------------
!
      DATA REFRSK/1.E-6/
      DATA C365/365.25/,TCOM/70./,HCOM/630./,RFDLIM/0.0/,HINC/730./
!
!----- Parameter definitions for COMMON Block DERMDAT --------------------
!
      DATA SKINL/0.001/                 ! Thickness of stratum corneum
      DATA TESHWR/0.167/,EVSHWR/1.0/    ! Showering time/frequency
      DATA TESWIM/0.5/,  EVSWIM/0.066/  ! Swimming time/frequency
      DATA TEBOAT/0.5/,  EVBOAT/0.066/  ! Boating time/frequency
      DATA TESHOR/2.0/,  EVSHOR/0.017/  ! Shoreline time/frequency
      DATA EVSOIL/1.0/                  ! Soil contact events/day
      DATA ASKIN/20000.,20000.,5800.,10000./ ! Skin contact areas
      DATA ADHFAC/1.0/                  ! Soil adherence factor mg/cm2
      DATA ADHSED/1.0/                  ! Sediment adherence factor mg/cm2
      DATA ANDFC/0.5/,ANDFR/0.1/        ! Andelman factors for chems/radon222
      DATA TSS/0.04/,  RHOSS/1.5/       ! Shoreline thickness m, and density
      DATA TMS/0.04/,  RHOMS/1.5/       ! Measured soil thickness & density
      DATA TAS/0.04/,  RHOAS/1.5/       ! Atmospheric deposition, soil thickness
!                                         and density
!
!----- Parameter definitions for COMMON Block DRPATH
!
      DATA TEDIRD/24./
!
!----- Parameter definitions for COMMON Block LEACH
!
      DATA THICK/15./
      DATA MOISTC/.3/
      DATA BULKD/1.5/
      DATA VLEACH/5./
!
!============ END OF MODULE BLKDAT =========================================
      END
!   MEPAS HAZ2: USEDAT.FOR             Version Date: 18-Dec-1995
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE USEDAT                                *
!                                                                            *
!  Subroutine USEDAT controls input of pathway and usage location data.      *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    04/01/86 (Converted to PC)                              *
!  Last Modified:    18-Dec. -1995  DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE FUNCTION ....
!     Calls: FUNCTIONS...
!            SUBROUTINES....
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     1 April 1986 DLS  Moved format statements to end of subroutine
!     2 Feb 1987   DLS  Removed input options on aquatic and agricultural
!                       food class definitions
!     10 June 1987 DLS  Remove read of INREC and call to RECIN(INREC)
!     23 July 1992 DLS  Remove NPOL from argument list (Not used)
!     28-JUL-92    DLS  Modification of heading information
!     13-Sep-94    DLS  elimiated reference to DFRPLC
!     18-Dec-95    DLS  Revised use of IPATH = 2 to Surface water and
!                       IPATH = 3 to regional atmospheric and IPATH = 4 to
!                       atmospheric agricultural
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE USEDAT(SETDATA,NLINES,IPATH,IERR)
!
!==== COMMON Block Definitions ===============================================
!
        include 'DEVICE.FTN'
        INCLUDE 'PARMTR.PAR'
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
      CHARACTER*(*) SETDATA(LINEMAX)
!
!==== DATA Statements ========================================================
!
!     IPATH is the index on the pathway for which data is to be provided in
!       this call.
!
!----- Test argument parameters for proper range ----------------------------C
!
      IF(IPATH.LT.1.OR.IPATH.GT.5) THEN
        WRITE(6,1000) IPATH
 1000 FORMAT('1 BAD VALUE OF IPATH IN USEDAT; = ',I5)
         IERR = IERR + 1
         GO TO 999
      ENDIF
!
!----- Call appropriate subroutines for input of usage location data    -----C
!          Groundwater exposue data - GWINPT
!          Surface water exposure data - SWINPT
!          Atmospheric exposure data - ATINPT
!
      IF( IPATH.EQ.1) then
        CALL GWINPT(SETDATA,NLINES,IERR)
      else IF(IPATH.EQ.2) then
        CALL SWINPT(SETDATA,NLINES,IERR)
      else IF( IPATH.EQ.3) then
        CALL ATINPR(SETDATA,NLINES,IERR)
        CALL ATINPA(SETDATA,NLINES,IERR)
      else IF( IPATH.EQ.5) then
        CALL SLINPT(SETDATA,NLINES)
      endif
!
 999  RETURN
!================== END OF MODULE USEDAT ====================================
      END
!
!   MEPAS HAZ2: SWINPT.FOR             Version Date: 29-Apr-06
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SWINPT                                *
!                                                                            *
!  Subroutine SWINPT This module reads records to define the usage practices *
!                    at each usage location for overland water pathways      *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    02/28/86 (Converted to PC)                              *
!  Last Modified:    29-Apr-2006  MAS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZI
!     Called by: SUBROUTINE USEDAT
!     Calls: NONE
!     Common blocks referenced: AQCLAS, PROD, RECLAS, SWPATH, AGCLAS, DEVICE,
!                               LOCNAM, CRLOC, AQPATH, PTHUSE, DWPATH
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!==== Modification History ===================================================
!
!   Date      Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  01-Apr-86  DLS  Moved format statements to end of subroutine
!  10-Jun-87  DLS  Add 4th parameter in call to SETPOP
!                    Modify Format 100 to read from 2 records
!  19-Oct-89  DLS  Parameter TRANSW added to read statement
!  05-Mar-90  JWB  Modified SW output to HPI file
!  28-JUL-92  DLS  Modification of heading information
!  26-Aug-92  DLS  Addition of read for body weight, exposure duration
!                  and averaging time (for non-carcinogens)
!  12-Oct-92  DLS  Removed averaging time from input, set to exp.dura.
!  28-Jan-93  DLS  Eliminated call to DFRPLC.  Replaced by BLKREAD
!  21-Oct-93  DLS  Add write to *.INT file
!  20-Dec-93  DLS  Added evaluation of SIFs
!  27-Jan-94  DLS  Added TESHWR to dermal SIF from showering
!  10-Feb-94  DLS  Final corrections to SIF formulations/units
!  18-Mar-94  DLS  Set BW and ED when SIFs are input
!  20-Apr-94  DLS  Moved BW and ED to common block SWPATH
!  04-Dec-95  DLS  Added intake parameters by receptor location
!  19-Dec-95  DLS  Fixed error in shower frequency usage from 4-Dec-95
!  11-Feb-97  DLS  Added name "Surface water" to population data line
!                  in .INA file
!  12-Aug-97  DLS  Added read of population data array POPSW
!  13-Aug-97  DLS  Added explicit pathway for "Indoor inhalation", No. 25.
!  29-Apr-06  MAS  Added adherence factor for sediment (ADHSED)
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SWINPT(SETDATA,NLINES,IERR)
!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'PARMTR.PAR'
        include 'AQCLAS.FTN'
        include 'PROD.FTN'
        include 'PFLAGS.FTN'
        include 'RECLAS.FTN'
        include 'SWPATH.FTN'
        include 'AGCLAS.FTN'
        include 'DEVICE.FTN'
        include 'LOCNAM.FTN'
        include 'crloc.FTN'
        include 'aqpath.FTN'
        include 'pthuse.FTN'
        include 'dwpath.FTN'
        include 'dermdat.FTN'
!
!==== DIMENSION Statements ===================================================
!
      INTEGER KSWPTH(18)
      INTEGER GETINT
!
!==== Variable Declarations ==================================================
!
!      character*4 cltrtl
      CHARACTER*2 SWNAM(18)
      CHARACTER*5 SWTYPE(3)
      CHARACTER*11 SWUNTS(3,18)
      CHARACTER*(*) SETDATA(LINEMAX)
      REAL(KIND=4) :: BWONE                                           ! 26-Apr-01
      REAL(KIND=4), DIMENSION(2,2) :: SIFXXX                          ! 26-Apr-01
!
!==== DATA Statements ========================================================
!
      DATA KSWPTH/1,2,3,4,5,6,7,8,9,10,11,12,13,17,20,21,22,25/
      DATA C70/70./,C10EM3/1.E-3/,C10EM6/1.E-6/,C10E3/1.E3/
      DATA C365/365.25/,C24/24./
      DATA SWTYPE/'Non-c','Carc.','Rad. '/
      DATA SWNAM/'DW','SD','SW','LV','OV','MT','MK','FF','SF',   &
                 'WW','WD','HD','HS','SI','WE','BE','HE','ID'/
      DATA SWUNTS/'     L/kg/d','     L/kg/d','     L     ',  &   ! DW
                  'L h/kg/d/cm','L h/kg/d/cm','  L h/cm   ',  &   ! SD
                  '     L/kg/d','     L/kg/d','     L     ',  &   ! SW
                  '    kg/kg/d','    kg/kg/d','     kg    ',  &   ! LV
                  '    kg/kg/d','    kg/kg/d','     kg    ',  &   ! OV
                  '    kg/kg/d','    kg/kg/d','     kg    ',  &   ! MT
                  '     L/kg/d','     L/kg/d','     L     ',  &   ! MK
                  '    kg/kg/d','    kg/kg/d','     kg    ',  &   ! FF
                  '    kg/kg/d','    kg/kg/d','     kg    ',  &   ! SF
                  '     L/kg/d','     L/kg/d','     L     ',  &   ! WW
                  'L h/kg/d/cm','L h/kg/d/cm','   L h/cm  ',  &   ! WD
                  ' kg ev/kg/d',' kg ev/kg/d','    kg ev  ',  &   ! HD
                  '    kg/kg/d','    kg/kg/d','     kg    ',  &   ! HS
                  '    m3/kg/d','    m3/kg/d','     m3    ',  &   ! SI
                  '           ','           ','     h     ',  &   ! WE
                  '           ','           ','     h     ',  &   ! BE
                  '    m3/kg/d','    m3/kg/d','     m3    ',  &   ! SI
                  '           ','           ','     h     '/      ! HE
       DATA BWONE/1.0/                                                   ! 26-Apr-01
!
!----- Start calculations.  Initialize error index and data arrays. -------
!
      IERR=0
      KSDR = 0
      KSAQ = 0
      KSIR = 0
      KSR  = 0
      DSWED = 0.
      DSWBW = 0.
!
!----- Read one record for each location, Record Type 6c --------------------
!
       IDR = GETINT(SETDATA,NLINES,'KSDR          ',IZ,IZ,IZ,IZ,IZ,IZ)
       IAQ = GETINT(SETDATA,NLINES,'KSAQ          ',IZ,IZ,IZ,IZ,IZ,IZ)
       IIR = GETINT(SETDATA,NLINES,'KSIR          ',IZ,IZ,IZ,IZ,IZ,IZ)
       ISR = GETINT(SETDATA,NLINES,'KSR           ',IZ,IZ,IZ,IZ,IZ,IZ)
       ED  = GETREAL(SETDATA,NLINES,'EXPDUR        ',IZ,IZ,IZ,IZ,IZ,IZ)
       BW  = GETREAL(SETDATA,NLINES,'BODYWT        ',IZ,IZ,IZ,IZ,IZ,IZ)
!
!----- Read population exposed for each exposure pathway included ------------
!
!      DO IP = 1,18
!        IPX = KSWPTH(IP)
!          POPSW(IP) = GETREAL(SETDATA,NLINES,'POPSW         ',IPX,IZ,IZ,IZ,IZ,IZ)
!      END DO
!
!----- Test value given for agricultural index ------------------------------
!
        IF(IIR.GT.NAGC) THEN
          WRITE(nrls,1000) IIR,NAGC
          IERR=IERR+1
        ENDIF
!
!----- Test value given for aquatic food index ------------------------------
!
        IF(IAQ.LT.0.OR.IAQ.GT.NAQC) THEN
          WRITE(nrls,1002) IAQ,NAQC
          IERR=IERR+1
        ENDIF
!
!----- Test value given for recreational index ------------------------------
!
        IF(ISR.LT.0.OR.ISR.GT.NREC) THEN
          WRITE(nrls,1003) ISR,NREC
          IERR=IERR+1
        ENDIF
!
!----- Set values into master parameters -----------------------------------
!
        IF(IERR.GT.0) GO TO 10
        KSDR = IDR
        KSAQ = IAQ
        KSIR = IIR
        KSR  = ISR
!
        DO IC = 1,4
          IF(UAGSW(IC).LE.0.) UAGSW(IC) = UAG(IC) ! 4/Dec/95
        END DO
!
        DO IC = 1,2
          IF(UAQSW(IC).LE.0.) UAQSW(IC) = UAQ(IC) ! 4/Dec/95
        END DO
!
!  Evaluate SIFs if a value is given for BW or ED for the location
!
          IF(ED.LE.0.) ED = C70   ! Default exposure duration 70 years
          DSWED=ED
          IF(BW.LE.0.) BW = C70   ! Default body weight 70 kg
          DSWBW=BW        
!  Set SIFs for drinking water pathway
            TERM = UDW(2) * FREQ(1)
            CALL SETSIF(TERM,BW,ED,SWSIF(1,1),SWSIF(2,1),SWSIF(3,1) )
!  Set SIFs for shower dermal contact pathway
            IF(USHWRS.LE.0.) USHWRS = EVSHWR             ! 4/Dec/95
            TERM = C10EM3 * ASKIN(1) * FREQ(2) * TESHWR *    &   ! 4/Dec/95
                    USHWRS                                   ! 4/Dec/95
            CALL SETSIF(TERM,BW,ED,SWSIF(1,2),SWSIF(2,2),SWSIF(3,2) )
!  Set SIFs for shower inadvertent ingestion pathway
            IF(USHWRS.LE.0.) USHWRS = EVSHWR             ! 4/Dec/95
            TERM = UBW*TESHWR*FREQ(3)*USHWRS                 ! 4/Dec/95
            CALL SETSIF(TERM,BW,ED,SWSIF(1,3),SWSIF(2,3),SWSIF(3,3) )
!  Set SIFs for agricultural product pathways
          DO IC = 4,7
              IF(UAGSW(IC-3).LE.0.) UAGSW(IC-3) = UAG(IC-3) ! 4/Dec/95
              TERM = UAGSW(IC-3)*FREQ(IC)                      ! 4/Dec/95
              CALL SETSIF(TERM,BW,ED,SWSIF(1,IC),SWSIF(2,IC),SWSIF(3,IC) )
          END DO
!  Set SIFs for aquatic food product pathways
          DO IC = 8,9
              IF(UAQSW(IC-7).LE.0.) UAQSW(IC-7) = UAQ(IC-7) ! 4/Dec/95
              TERM = UAQSW(IC-7)*FREQ(IC)                      ! 4/Dec/95
              CALL SETSIF(TERM,BW,ED,SWSIF(1,IC),SWSIF(2,IC),SWSIF(3,IC) )
          END DO
!  Set SIFs for swimming inadvertent ingestion pathway
            IF(UEVSWIM.LE.0.) UEVSWIM = EVSWIM             ! 4/Dec/95
            TERM = USW*TESWIM*FREQ(10)*UEVSWIM                 ! 4/Dec/95
            CALL SETSIF(TERM,BW,ED,SWSIF(1,10),SWSIF(2,10),SWSIF(3,10) )
!  Set SIFs for swimming dermal water contact pathway
            IF(UEVSWIM.LE.0.) UEVSWIM = EVSWIM             ! 4/Dec/95
            TERM = C10EM3*ASKIN(2)*FREQ(11)*UEVSWIM*TESWIM     ! 4/Dec/95
            CALL SETSIF(TERM,BW,ED,SWSIF(1,11),SWSIF(2,11),SWSIF(3,11) )
!  Set SIFs for shoreline sediment dermal contact pathway
            IF(UEVSHOR.LE.0.) UEVSHOR = EVSHOR             ! 4/Dec/95
            TERM = C10EM6*ASKIN(4)*ADHSED*FREQ(12)*UEVSHOR     ! 4/Dec/95
            CALL SETSIF(TERM,BW,ED,SWSIF(1,12),SWSIF(2,12),SWSIF(3,12) )
!  Set SIFs for shoreline sediment ingestion pathway
            IF(UEVSHOR.LE.0.) UEVSHOR = EVSHOR             ! 4/Dec/95
            TERM = C10EM3*USED*TESHOR*UEVSHOR*FREQ(13)         ! 4/Dec/95
            CALL SETSIF(TERM,BW,ED,SWSIF(1,13),SWSIF(2,13),SWSIF(3,13) )
!  Set SIFs for shower inhalation pathway
            IF(UBRSHS.LE.0.) UBRSHS = UBRSH                ! 4/Dec/95
            TERM = UBRSHS*TESHWR*EVSHWR*FREQ(17)/C24   ! 4/Dec/95
            CALL SETSIF(TERM,BW,ED,SWSIF(1,14),SWSIF(2,14),SWSIF(3,14) )
            IF(INHALE.EQ.0) THEN                                             ! 26-Apr-01
               TERM = TESHWR*EVSHWR*FREQ(17)/C24         !  4/Dec/95         ! 26-Apr-01
               CALL SETSIF(TERM,BWONE,ED,SWSIF(1,14),SWSIF(2,14),SIFXXX(1,1)) ! 26-Apr-01
            ENDIF                                                            ! 26-Apr-01
!  Set SIFs for indoor inhalation pathway
            TERM = UBRSHS*FREQ(25)                           ! 4/Dec/95
            CALL SETSIF(TERM,BW,ED,SWSIF(1,18),SWSIF(2,18),SWSIF(3,18) )
            IF(INHALE.EQ.0) THEN                                             ! 26-Apr-01
               TERM = FREQ(25)                                  ! 4/Dec/95   ! 26-Apr-01
               CALL SETSIF(TERM,BWONE,ED,SWSIF(1,18),SWSIF(2,18),SIFXXX(1,1))! 26-Apr-01
            ENDIF                                                            ! 26-Apr-01
!  Set SIFs for swimming external pathway
            IF(UEVSWIM.LE.0.) UEVSWIM = EVSWIM             ! 4/Dec/95 
            TERM = TESWIM*UEVSWIM*FREQ(20)                     ! 4/Dec/95 
            SWSIF(3,15) = TERM * ED * C365
!  Set SIFs for boating external pathway
            IF(UEVBOAT.LE.0.) UEVBOAT = EVBOAT             ! 4/Dec/95 
            TERM = TEBOAT*UEVBOAT*FREQ(21)*SBRF                ! 4/Dec/95 
            SWSIF(3,16) = TERM * ED * C365
!  Set SIFs for shoreline external pathway
            IF(UEVSHOR.LE.0.) UEVSHOR = EVSHOR             ! 4/Dec/95 
            TERM = TESHOR*UEVSHOR* FREQ(22)*SWF                ! 4/Dec/95 
            SWSIF(3,17) = TERM * ED * C365
   10 CONTINUE
!-----
!        if( ltrtl(2) .eq. 0 ) cltrtl = ' No '
!        if( ltrtl(2) .eq. 1 ) cltrtl = ' Yes'
!-----
          WRITE(nrls,2000) char(12)
 2000     FORMAT(a1,20x,'USAGE LOCATION DATA FOR SURFACE WATER PATHWAY')
!-----
!----- writing out the usage location parameter chart(s)
!----- Always print the index and population numbers
!
!        write(nrls,2001) ksdr
! 2001   format('Drink Index:  (None)',1x,I10)
!        write(nrls,2003) ksr
! 2003   format('Rec Index:    (None)',1x,I10)
!        write(nrls,2005) ksaq
! 2005   format('Aquatic Index:(None)',1x,I10)
!        write(nrls,2008) ksir
! 2008   format('Irr Index:    (None)',1x,I10)
!        write(nrls,3010) cltrtl
! 3010   format('Water Treated:yes/no  ',1p,4x,a4,3x)
!
!----- Write parameters dependent on calculation of SIFs
!
            write(nrls,2100) dswbw
 2100       format('Body Weight:  Kg    ',1p,1x,E10.3)
            write(nrls,2101) dswed
 2101       format('Exposure Duration: y',1p,1x,E10.3)
            write(nrls,3011) udw(2)
 3011       format('H2O Ing Rate: L/day ',1p,1x,E10.3)
            write(nrls,2204) uagsw(1)
 2204       format('Leafy Veg Rate: kg/d',1p,1x,E10.3)
            write(nrls,2205) uagsw(2)
 2205       format('Other Veg Rate: kg/d',1p,1x,E10.3)
            write(nrls,2206) uagsw(3)
 2206       format('Meat Ing. Rate: kg/d',1p,1x,E10.3)
            write(nrls,2207) uagsw(4)
 2207       format('Milk Ing. Rate: L/g ',1p,1x,E10.3)
            write(nrls,2208) uaqsw(1)
 2208       format('Fin Fish Rate:  kg/d',1p,1x,E10.3)
            write(nrls,2209) uaqsw(2)
 2209       format('Shellfish Rate: kg/d',1p,1x,E10.3)
!
!----- For all cases write SIFs
!----- Write source used for SIF values, and body weight and exp. duration
!
        write(nrls,2300)        
 2300   format('Summary Intake Factors')
 2050   format('Source of SIF values',2x,A8)
 2301   format(A2,':',A5,1x,A11,1p,1x,E10.3)
        DO IEX = 1,18
            IF(IEX.LE.14.OR.IEX.EQ.18) THEN
              write(nrls,2301) SWNAM(IEX),SWTYPE(1),SWUNTS(1,IEX),SWSIF(1,IEX)
              write(nrls,2301) SWNAM(IEX),SWTYPE(2),SWUNTS(2,IEX),SWSIF(2,IEX)
            ENDIF
            write(nrls,2301) SWNAM(IEX),SWTYPE(3),SWUNTS(3,IEX),SWSIF(3,IEX)
        END DO
!
          write(nrls,'(79(''=''))')
!          write(nrls,'(''Drinking Index ---->'',''  0 = No Drinking or Bathing'','', 1 = Bathing Only, and'',/,21x,'&
!           '2 = Drinking and Bathing'')')
!          write(nrls,266)
! 266      FORMAT('Recreation Index -->  1 = Swimming Only',  &
!                 ', 2 = Boating Only,',/,21x,                &
!                 ' 3 = Shoreline Activities Only, and',      &
!                 ' 4 = All of the Above')
!          write(nrls,'("Aquatic Index ----->  0 = No Aquatic food",", 1 = Aquatic Food")')
!          write(nrls,267)
!267       FORMAT('Irrigation Index --> -1 = Animals Drinking',   &
!                 ' Only, 0 = No Farm Production,',/,             &
!                 21x,' 1 = Crops and Animals [Feed and Water],', &
!                 ' and',/21x,' 2 = Crops and Animals Feed Only')
!
 2999 continue
!
 999  RETURN
!
!----- Format statements ----------------------------------------------------
!
  100 FORMAT(5A4,4I5,4E10.2/8E10.2)
 1000 FORMAT('ERROR IN AGRICULTURAL CLASS INDEX IN SWINPT,'/        &
      ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
 1001 FORMAT('END-0F-FILE ENCOUNTERED ON UNIT ',I3,' WHILE READING' &
       /' SURFACE WATER TRANSPORT USAGE LOCATION DATA')
 1002 FORMAT('ERROR IN AQUATIC FOOD CLASS INDEX IN SWINPT,'/        &
      ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
 1003 FORMAT('ERROR IN RECREATIONAL CLASS INDEX IN SWINPT,'/        &
      ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
!
!========================== END OF MODULE SWINPT ============================
!
      END
!   MEPAS/AHAZI GWINPT.FOR             Version Date: 29-Oct-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GWINPT                                *
!                                                                            *
!  Subroutine GWINPT This module reads records to define the usage practices *
!                    at each usage location for ground water pathways.       *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    02/28/86 (Converted to PC)                              *
!  Last Modified:    29-Oct-1997     DLS                                     *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZI4.0
!     Called by: SUBROUTINE USEDAT
!     Calls: NONE
!     Common blocks referenced: GWPATH, AGCLAS, DEVICE, LOCNAME,
!      PROD, CRLOC, AQPATH, PTHUSE, DWPATH
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     --- ------------------------------------------------------
!     10-Jun-1987  DLS  Added 4th parameter in call to SETPOP
!     05-Mar-1990  JWB  Modified GW output to HPI file
!     28-JUL-92    DLS  Modification of heading information
!     31-AUG-92    DLS  Add input of body weight, exposure duration, and
!                       averaging time
!     13-Oct-92    DLS  Remove averaging time from input, set = exposure dur.
!     28-Jan-93    DLS  Eliminated call to DFRPLC.  Replaced by BLKREAD.
!     21-Oct-93    DLS  Add write to *.INT file
!     09-Dec-93    DLS  Add input/calculation of SIF factors
!     27-Jan-94    DLS  Corrected units conversion for Dermal while showering
!     10-Feb-94    DLS  Final changes for SIF upgrate, units
!     18-Mar-94    DLS  Set BW and ED when SIFs are input
!     20-Apr-94    DLS  Moved DGWED and DGWBW to common block GWPATH
!     04-Dec-94    DLS  Expanded intake parameters to receptor location
!     11-Feb-97    DLS  Added name "Ground water" to output data line in .INA
!     02-Jul-97    DLS  Revised for use in FRAMES/MEPAS 4.0.  Read from GID,
!                       only one location.
!  13-Aug-97  DLS  Added explicit pathway for "Indoor inhalation"
!  29-Oct-97  DLS  Correct shower inhalation path to use correct inhalation
!                  rate UBRSH instead of UBRES
!==== SUBROUTINE CALL  ========================================================
!
      SUBROUTINE GWINPT(SETDATA,NLINES,IERR)
!
!==== COMMON Block Definitions ===============================================
!
        INCLUDE 'PARMTR.PAR'
        include 'GWPATH.FTN'
        include 'AGCLAS.FTN'
        include 'DEVICE.FTN'
        include 'PFLAGS.FTN'
        include 'PROD.FTN'
        include 'crloc.FTN'
        include 'aqpath.FTN'
        include 'pthuse.FTN'
        include 'dwpath.FTN'
        include 'dermdat.FTN'
!
!==== DIMENSION Statements ===================================================
!
!     Dimension lvprd(10), vprd(10), mtprd(10),
!    .          mkprd(10), lll(10)
!      DIMENSION dgwbw(10), dgwed(10)  ! moved to Common GWPATH
!
!==== Variable Declarations ==================================================
!
!      character*4 cltrtl
      CHARACTER*2 GWNAM(9)
      CHARACTER*5 GWTYPE(3)
      CHARACTER*11 GWUNTS(3,9)
      CHARACTER*(*) SETDATA(LINEMAX)
      INTEGER KGWPTH(9),GETINT
      REAL(KIND=4) :: BWONE                                           ! 26-Apr-01
      REAL(KIND=4), DIMENSION(2,2) :: SIFXXX                          ! 26-Apr-01
!
!==== DATA Statements ========================================================
!
      DATA KGWPTH/1,2,3,4,5,6,7,17,25/
      DATA C10EM3/1.E-3/,C10E3/1.E3/
      DATA GWTYPE/'Non-c','Carc.','Rad. '/
      DATA GWNAM/'DW','SD','SW','LV','OV','MT','MK','SI','ID'/
      DATA GWUNTS/'     L/kg/d','     L/kg/d','     L     ',  &   ! 1-DW
                  'L h/kg/d/cm','L h/kg/d/cm','   L h/cm  ',  &   ! 1-SD
                  '     L/kg/d','     L/kg/d','     L     ',  &   ! 1-SW
                  '    kg/kg/d','    kg/kg/d','     kg    ',  &   ! 1-LV
                  '    kg/kg/d','    kg/kg/d','     kg    ',  &   ! 1-OV
                  '    kg/kg/d','    kg/kg/d','     kg    ',  &   ! 1-MT
                  '     L/kg/d','     L/kg/d','     L     ',  &   ! 1-MK
                  '     L/kg/d','     L/kg/d','     L     ',  &   ! 1-SI
                  '     L/kg/d','     L/kg/d','     L     '/      ! 1-ID
      DATA BWONE/1.0/                                                 ! 26-Apr-01
!
!----- Start calculations.  Initialize error index and data arrays. -------
!
      IERR=0
!
!----- Read one record for each location, Record Type 6a --------------------
!
        IDR = GETINT(SETDATA,NLINES,'KGDR          ',IZ,IZ,IZ,IZ,IZ,IZ)
        IIR = GETINT(SETDATA,NLINES,'KGIR          ',IZ,IZ,IZ,IZ,IZ,IZ)
        BW = GETREAL(SETDATA,NLINES,'BODYWT        ',IZ,IZ,IZ,IZ,IZ,IZ)
        ED = GETREAL(SETDATA,NLINES,'EXPDUR        ',IZ,IZ,IZ,IZ,IZ,IZ)
!
!----- Test value given for agricultural index ------------------------------
!
        IF(IIR.GT.NAGC) THEN
          WRITE(nrls,1000) IIR,NAGC
          IERR=IERR+1
          GO TO 10
        ENDIF
!
!----- Set values into master parameters -----------------------------------
!
        KGDR=IDR
        KGIR=IIR
        DGWBW=70.
        DGWED=70.
        DO IC = 1,4
           IF(UAGGW(IC).LE.0.) UAGGW(IC) = UAG(IC) ! 1-May-97
        END DO
!
            IF(BW.LE.0.) BW = 70.    !  Default body weight = 70 kg.
            DGWBW = BW
            IF(ED.LE.0.) ED = 70.    !  Default exposure duration = 70 yrs
            DGWED = ED
!
!  Set SIFs for drinking water ingestion (path 1)
!
              TERM = UDW(1)*FREQ(1)
              CALL SETSIF(TERM,BW,ED,GWSIF(1,1),GWSIF(2,1),GWSIF(3,1))
!
!  Set SIFs for shower dermal water contact (path 2)
!
              IF(USHWRG.LE.0.) USHWRG = EVSHWR              ! 4-Dec-95
              TERM = ASKIN(1) * FREQ(2) * TESHWR *USHWRG*C10EM3 ! 4-Dec-95
              CALL SETSIF(TERM,BW,ED,GWSIF(1,2),GWSIF(2,2),GWSIF(3,2))
!
!  Set SIFs for shower inadvertent ingestion (path 3)
!
              IF(USHWRG.LE.0.) USHWRG = EVSHWR              ! 4-Dec-95
              TERM = UBW*TESHWR*FREQ(3)*USHWRG                  ! 4-Dec-95
              CALL SETSIF(TERM,BW,ED,GWSIF(1,3),GWSIF(2,3),GWSIF(3,3))
!
!  Set SIFs for agricultural product ingestion pathways (paths 4 - 7)
!
            DO IC = 4,7
                IF(UAGGW(IC-3).LE.0.) UAGGW(IC-3) = UAG(IC-3) ! 4-Dec-95
                TERM = UAGGW(IC-3)*FREQ(IC)                      ! 4-Dec-95
                CALL SETSIF(TERM,BW,ED,GWSIF(1,IC),GWSIF(2,IC),GWSIF(3,IC))
            END DO
!
!  Set SIFs for shower inhalation (path 17, array index 8 for MEPAS model)
!                                 (path 17, array index 9 for Andelman model)
!  Calculate product of common terms depending on type of inhalation model
!  to be used: KEXPTH(17) = 1 for Original MEPAS, = 2 for EPA/Andelman
!
            IF(UBRSHG.LE.0.) UBRSHG = UBRSH               ! 4-Dec-95
            TERM = UBRSHG*TESHWR*EVSHWR*FREQ(17)/24.      ! 4-Dec-95
            CALL SETSIF(TERM,BW,ED,GWSIF(1,8),GWSIF(2,8),GWSIF(3,8))
            IF(INHALE.EQ.0) THEN                                             ! 26-Apr-01
               TERM = TESHWR*EVSHWR*FREQ(17)/24.          ! 4-Dec-95         ! 26-Apr-01
               CALL SETSIF(TERM,BWONE,ED,GWSIF(1,8),GWSIF(2,8),SIFXXX(1,1))  ! 26-Apr-01
            ENDIF                                                            ! 26-Apr-01
            TERM = UBRSHG*FREQ(17)        !EPA/Andelman     ! 4-Dec-95
            CALL SETSIF(TERM,BW,ED,GWSIF(1,9),GWSIF(2,9),GWSIF(3,9))
            IF(INHALE.EQ.0) THEN                                             ! 26-Apr-01
               TERM = FREQ(17)        !EPA/Andelman     ! 4-Dec-95           ! 26-Apr-01
               CALL SETSIF(TERM,BWONE,ED,GWSIF(1,9),GWSIF(2,9),SIFXXX(1,1))  ! 26-Apr-01
            ENDIF                                                            ! 26-Apr-01
            IF(BW.LE.0.) BW = 70.    !  Default body weight = 70 kg.
            DGWBW = BW
            IF(ED.LE.0.) ED = 70.    !  Default exposure duration = 70 yrs
            DGWED = ED
   10 CONTINUE
!-----
!        if( ltrtl(1) .eq. 0 ) cltrtl = ' No '
!        if( ltrtl(1) .eq. 1 ) cltrtl = ' Yes'
!----- modify usage location printout --- JWB - 01/22/90
!-----
      WRITE(nrls,2000) char(12)
 2000 FORMAT(a1,20x,'USAGE LOCATION DATA FOR GROUNDWATER PATHWAY')
!-----
!----- writing out the usage location name
!-----
       lll = 1
          write(nrls,3002) lll
          write(nrls,'(31(''-''))')
 3002     format(//'  Location        ',2x,('  Location '),  &
                   /,' Parameter    Units ',(5x,I2,4x) )
!-----
!----- writing out the usage location parameter chart(s)
!-----
!        write(nrls,2001) kgdr
! 2001   format('Drink Index:  (None)',1x,I10)
!        write(nrls,2003) kgir
! 2003   format('Irr Index:    (None)',1x,I10)
        if(kexpth(4).gt.0) then
          write(nrls,2204) uagGW(1)
 2204   format('Leaf Veg Ing: kg/d  ',1p,1x,E10.3)
        endif
        if(kexpth(5).gt.0) then
          write(nrls,2205) uagGW(2)
 2205   format('Othr Veg Ing: kg/d  ',1p,1x,E10.3)
        endif
        if(kexpth(6).gt.0) then
          write(nrls,2206) uagGW(3)
 2206   format('Meat Ing.:    kg/d  ',1p,1x,E10.3)
        endif
        if(kexpth(7).gt.0) then
          write(nrls,2207) uagGW(4)
 2207   format('Milk Ing:     L/d   ',1p,1x,E10.3)
        endif
!
! Write results dependent on method for definition of SIFs
! For original MEPAS and optional calculations, write each parameter
!
!        IF(KSIF.LE.1) THEN
!
!  Write source used for SIF values, SIF File or Calc.
!
        write(nrls,2011) udw(1)
 2011   format('H2O Ing Rate: L/day ',1p,1x,E10.3)
        write(nrls,2100) dgwbw
 2100   format('Body Weight:  kg    ',1p,1x,E10.3)
        write(nrls,2101) dgwed
 2101   format('Exposure Duration: y',1p,1x,E10.3)
!        ENDIF
!
!  For all cases write SIFs
!
        write(nrls,2300) 
        DO IEX = 1,9
          write(nrls,2301) GWNAM(IEX),GWTYPE(1),GWUNTS(1,IEX),gwsif(1,IEX)
          write(nrls,2301) GWNAM(IEX),GWTYPE(2),GWUNTS(2,IEX),gwsif(2,IEX)
          write(nrls,2301) GWNAM(IEX),GWTYPE(3),GWUNTS(3,IEX),gwsif(3,IEX)
 2300   format('Summary Intake Factors')
 2301   format(A2,':',A5,1X,A11,1p,1x,E10.3)
        END DO
!        write(nrls,2008) cirr(1)
! 2008   format('Irr. Rate: L/mo/m2  ',1p,1x,E10.3)
!        write(nrls,2012) tgrw(1,1)
! 2012   format('Harvest Time: days  ',1p,1x,E10.3)
!        write(nrls,2009) twat(1)
! 2009   format('Transit Time: days  ',1p,1x,E10.3)
!        write(nrls,2010) cltrtl
! 2010   format('Water Treated:yes/no  ',1p,4x,a4,3x)
          write(nrls,'(79(''=''))')
!          write(nrls,268)
!268       FORMAT('Drinking Index ---->',               &
!                 '  0 = No Drinking or Bathing',       &
!                 ', 1 = Bathing Only, and',            &
!                 /,21x,' 2 = Drinking and Bathing')
!          write(nrls,269)
!269       FORMAT('Irrigation Index --> -1 = Animals Drinking',      &
!                 ' Only, 0 = No Farm Production,',/                 &
!                 21x,' 1 = Crops and Animals [Feed and Water],',    &
!                 ' and',/21x,' 2 = Crops and Animals Feed Only')
!          write(nrls,270)
!270       FORMAT(/62('='),/,' KEY: Reference for',   &
!                     ' Usage Location Name',/)
 2999 continue
!
      RETURN  
!
!----- Format Statements ---------------------------------------------------
!
  100 FORMAT(5A4,2I5,9E10.2)
 1000     FORMAT('ERROR IN AGRICULTURAL CLASS INDEX IN GWINPT,'/   &
          ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
 1001 FORMAT('END-0F-FILE ENCOUNTERED ON UNIT ',I3,' WHILE READING' &
       /' GROUNDWATER TRANSPORT USAGE LOCATION DATA')
!
!===================== END OF MODULE GWINPT ==================================
!
      END
!   MEPAS AHAZ: LAMAX.FOR             Version Date: 18-Dec-95
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************C
!
!  Subroutine LAMAX  finds the pollutant index for the maximum soil and
!           groundwater environmental half times for use in CROPS                             *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07-Nov-95  (From HAZ2 version)                          *
!  Last Modified:    18-Dec-95      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: GROUND, ATMOS, SWINPT
!     Calls: None
!     Common blocks referenced: DECAY, PSET3
!
!==== Significant Parameter Designation and Description ======================
!
!  Parameters in argument list
!  Name        Type             Purpose
!  ---------   ------    ---------------------------------------------------
!---------------------------------------------------------------------------
!
!  Major Local Parameters:
!     Name    Type                  Purpose
!   -------  ------  ---------------------------------------------------
!    IPIN      INT      Parent index for chain to consider
!    IEIN      INT      Progeny position in chain
!    IPSX      INT      Output index for pollutant for which to use SLAM
!    IPGX      INT      Output index for pollutant for which to use GLAM
! --------------------------------------------------------------------------
!
!==== Modification History ===================================================
!
!     Date       Who  Modification Description
!     --------   ---  --------------------------------------------------------
!    18-Dec-95   DLS Removed NUC = 1 (set in PDATIN)
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE LAMAX(IPIN,IEIN,IPSX,IPGX)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DECAY.FTN'
      INCLUDE 'PSET3.FTN'
!
!==== DIMENSION Statements ===================================================
!
!     None
!
!==== Variable Declarations ==================================================
!
      INTEGER IPIN,IEIN,IPSX,IPGX
!
!==== DATA Statements ========================================================
!
!     None
! ****************************************************************************
!
      IPGX = IPIN
      IPSX = IPIN
      IF(NUC.GT.1.AND.IEIN.GT.1) THEN
        JIPIN = IEIN
        IPSX = JIPIN
        IPGX = JIPIN
        SLMX = SLAM(JIPIN)
        GLMX = GLAM(JIPIN)
        DO IE = 1, IEIN-1
          JTP = IE
          SLTX = SLAM(JTP)
          IF(SLMX.GT.SLTX) THEN
            SLMX = SLTX
            IPSX = JTP
          ENDIF
          GLTX = GLAM(JTP)
          IF(GLMX.GT.GLTX) THEN
            GLMX = GLTX
            IPGX = JTP
          ENDIF
        END DO
      ENDIF
      RETURN
      END        
!-------------  End file LAMAX.FOR -------------------------
!  This module is part of the TABLES software package for generation of
!  dosimetry factors for the Nuclear Regulatory Commission project on
!  Residual Radioactive Contamination from Decommissioning.
!
!
      SUBROUTINE AMSET(PLSD,IP,AM,MAXP)
!
!  Subroutine to set initial activity into array for input to groundwater
!  three-box model processor, and air-to-plant two-box model processor.
!  For regular chain decay, the parent activity set to 1. and all daughters
!  are set to zero.  For "+C" parents, the parent activity is set to 1. and
!  daughters are set to equilibrium values based on decay branching data.
!
!
!  Developed by:   Pacific Northwest Laboratory
!                  Occupational & Environmental Health Protection Section
!                  P. O. Box 999
!                  Richland, WA 99352
!
!         Software Design Description:  D.L. Strenge
!         Initial Coding:  D. L. Strenge
!         Software Custodian: ...
!         Initial preparation: 21-Dec-1990
!         Initial testing complete: (date)
!
!         Modification History:
!    Date  Change #  Summary of Modification                     Changed by
!  ------- --------  ------------------------------------------- ----------
!
!
! --------------------------------------------------------------------------
!
!    Calling Module(s): AGPPTF, GWCALC
!    Subordinate Modules: None
!
!    COMMON Blocks Referenced by AMSET:     ..., ...,
!
!  Parameters in argument list
!  Name          Type             Purpose
!  ---------     ------    --------------------------------------------------
!  PLSD          Logical   Indicator for "+D" representation.
!  IP            Integer   Position of parent in master list.
!  AM(27)        Real      Initial activity of each chain member
!  MAXP          Integer*2 Total number of positions to consider (9 or 27)
!---------------------------------------------------------------------------
!
!  Other major parameters (including from COMMON Blocks) used in the module.
!  Name        Type             Purpose
!  ---------   ------    ---------------------------------------------------
!
!
! --------------------------------------------------------------------------
!
!  COMMON BLOCK definitions
      INCLUDE 'RMDATA.FTN'
!
! --------------------------------------------------------------------------
!  Dimension Statements/Type Statements
!
      DOUBLE PRECISION, DIMENSION(27) :: AM
      DOUBLE PRECISION, DIMENSION(9)  :: FD
      LOGICAL PLSD
      INTEGER IP, JC, IPC, IN, MAXP
!
!  Set parent activity to 1.0 and all other positions to zero.
!
      AM(1) = 1.0D0
      DO IC = 2,MAXP
        AM(IC) = 0.0D0
      END DO
!
!  If this is for a regular chain, then return.  Otherwise (if +C) set
!  daughter activities for chain in first NCM compartments, using decay
!  branching fractions.
!
      IF(PLSD) THEN
        NCM = NOCM(IP)
        FD(1) = 1.0
        DO IN = 2,NCM
          FD(IN) = 0.0
          DO IPC = 1,2
            JC = JFROM(IP,IPC,IN)
            IF(JC.GT.0) FD(IN) = FD(IN) + DKF(IP,IPC,IN)*FD(JC)
          END DO
          AM(IN) = FD(IN)
        END DO
      ENDIF
      RETURN
      END
!-------------------------------- End of AMSET.FOR ----------------------c
!   MEPAS AHAZI BLKREAD.FOR             Version Date: 29-Apr-06
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE BLKREAD                               *
!                                                                            *
!  Subroutine BLKREAD This data block allows changes to default              *
!                     parameter values.                                      *
!                                                                            *
!  Written by:       Dennis Strenge/J.G. Droppo                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    11/03/91 (Converted to PC)                              *
!  Last Modified:    29-Apr-2006  MAS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZI
!     Called by: NONE
!     Calls: NONE
!     Common blocks referenced: RISKCM
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     variable        S    CHAR  Argument  nnnnnnnnnnnnnnnnnnnnnn
!                     U    INT   Internal
!                     S/U  REAL  Common
!                          DBLE  External
!
!==== Modification History ===================================================
!
!   Date      Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  04-Feb-92  jgd  commented out extra print statements
!  28-JUL-92  DLS  Modification of heading information
!  02-OCT-92  DLS  Added parameters for common block DERMDAT (items 63 through
!                  66).  Fixed subscript problem for reference to array RLIST
!                  subscript > 5.
!  07-OCT-92  DLS  Add parameters for common blocks AQPATH, DWPATH, and CRPATH.
!  03-NOV-92  DLS  Add UBRSH to group 64, shower breathing rate
!  09-NOV-92  DLS  Add write statements for each parameter group
!  25-NOV-92  DLS  Add input of TEDIRD in DRPATH.FTN, group 67
!   5-FEB-93  DLS  Correct parameter descriptions in output lines
!  03-AUG-93  DLS  Add group 68, Andelman factors
!  06-Oct-93  DLS  Added groups 69,70,71,72,73: new group 57
!  10-Feb-94  DLS  Added group 74 - 75: shielding factors
!  21-Mar-94  DLS  Added groups 76 - 80: FREQ(25) array
!  29-Mar-94  DLS  Fix print format for FREQ
!  04-May-94  DLS  Corrected units in output format for shower ing.rate
!  21-Jul-94  DLS  Corrected units on USW to L/h, swimming ingestion
!  12-Sep-94  DLS  Added input of cancer incidence factor HINC to group 63 for
!                  radionuclides.
!  10-Oct-94  DLS  Added feed and water contamination fraction as group 19
!                  (FFC and FWC in common CRPATH), and USL(2) in group
!  21-Nov-95  DLS  Added FIRR in group ..
!  18-Dec-95  DLS  Added write statement for file title line
!  06-Aug-97  DLS  Modified to use only the parameters necessary for the intake
!                  component
!  19-Aug-97  DLS  Modified to read all data from GID file, always.
!  09-Sep-98  DLS  Added test to limit FTO + FTI <= 1.0 by prorating each
!                  value read.
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE  BLKREAD(SETDATA,NLINES,EPATH)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'DERMDAT.FTN'
      include 'DEVICE.FTN'
      INCLUDE 'PTHUSE.FTN'
!
!==== DIMENSION Statements ===================================================
!
!==== Variable Declarations ==================================================
!
      CHARACTER*(*) SETDATA(LINEMAX)
      CHARACTER*30 EPATH(25)
!
!==== DATA Statements ========================================================
!
!---- Start of Analysis ------------------------------------------------------
!
!    Write heading for report of changed data
!
      WRITE(NRLS,300) 
 300  FORMAT(/' Default parameter summary')
!
      write(NRLS,200)
 200  FORMAT(' Parameter    Value    Description (units)'/        &
             ' --------- --------- -----------------------------------')
!
!---- Set Default values from GID file --------------------------------------
!
!--  Read age group age limits ----------------------------------------------
!
      VAL1 = GETREAL(SETDATA,NLINES,'TAGE1         ',IZ,IZ,IZ,IZ,IZ,IZ)
      TAGE1 = VAL1
      VAL2 = GETREAL(SETDATA,NLINES,'TAGE2         ',IZ,IZ,IZ,IZ,IZ,IZ)
      TAGE2 = VAL2
      WRITE(NRLS,134) TAGE1,TAGE2
 134  FORMAT(' TAGE1, TAGE2',f5.0,1x,f5.0,' Age group limits, years')
!
!-- Drinking water ingestion rate for groundwater
!
      VAL = GETREAL(SETDATA,NLINES,'UDWGW         ',IZ,IZ,IZ,IZ,IZ,IZ)
      UDW(1) = VAL
      WRITE(NRLS,101) UDW(1)
 101      FORMAT(' UDW(1)    ',F10.3,' Groundwater ingestion rate, L/d')
!
!-- Drinking water ingestion rate for surface water --------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'UDWSW         ',IZ,IZ,IZ,IZ,IZ,IZ)
      UDW(2) = VAL
      WRITE(NRLS,102) UDW(2)
 102      FORMAT(' UDW(2)    ',F10.3,' Surface water ingestion rate, ','L/d')
!
!-- UAG, Intake rates for agricultural products ------------------------------
!
      DO IA = 1,4
        VAL = GETREAL(SETDATA,NLINES,'UAG           ',IA,IZ,IZ,IZ,IZ,IZ)
        UAG(IA)= VAL
      END DO
      WRITE(NRLS,111) (UAG(IA),IA=1,4)
 111      FORMAT(' UAG(1)    ',F10.3,' Leafy vegetable ingestion rate,', &
                 ' kg/d',/                                               &
                 ' UAG(2)    ',F10.3,' Other vegetable ingestion rate,', &
                 ' kg/d',/                                               &
                 ' UAG(3)    ',F10.3,' Meat ingestion rate, kg/d',/       &
                 ' UAG(4)    ',F10.3,' Milk ingestion rate, L/d')
!
!-- UAQ, Intake rates for aquatic foods --------------------------------------
!
      DO IA = 1,2
        VAL = GETREAL(SETDATA,NLINES,'UAQ           ',IA,IZ,IZ,IZ,IZ,IZ)
        UAQ(IA)= VAL
      END DO
      WRITE(NRLS,112) (UAQ(IA),IA=1,2)
 112      FORMAT(' UAQ(1)    ',F10.3,' Finfish ingestion rate, kg/d',/   &
                 ' UAQ(2)    ',F10.3,' Shell fish ingestion rate,',      &
                 ' kg/d')
!
!-- Inhalation rate for air inhalation ---------------------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'UBR           ',IZ,IZ,IZ,IZ,IZ,IZ)
      UBR = VAL
      WRITE(NRLS,103) UBR
 103      FORMAT(' UBR       ',F10.3,' Air inhalation rate, m3/d')
!
!-- Inhalation rate for resuspension exposure --------------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'UBRES         ',IZ,IZ,IZ,IZ,IZ,IZ)
      UBRES = VAL
      WRITE(NRLS,106) UBRES
 106      FORMAT(' UBRES     ',F10.3,' Resuspension inhalation rate,',' m3/d')
!
!-- Inhalation rate for indoor or showering exposure -------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'UBRSH         ',IZ,IZ,IZ,IZ,IZ,IZ)
      UBRSH = VAL
      WRITE(NRLS,128) UBRSH
 128  FORMAT(' UBRSH     ',F10.3,' Indoor or showering inhalation',' rate, m3/d')
!
!-- UBW, ingestion rate while showering, m3/d -------------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'UBW           ',IZ,IZ,IZ,IZ,IZ,IZ)
        UBW = VAL
      WRITE(NRLS,115) UBW
 115      FORMAT(' UBW       ',F10.3,' Shower water ingestion rate',', L/hr')
!
!-- TESHWR, Duration of a showering event, hr --------------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'TESHWR        ',IZ,IZ,IZ,IZ,IZ,IZ)
        TESHWR = VAL
      WRITE(NRLS,113) TESHWR
 113      FORMAT(' TESHWR    ',F10.3,' Duration of one shower event,',' hr/event')
!
!-- EVSHWR, frequency of showering, events/d ---------------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'EVSHWR        ',IZ,IZ,IZ,IZ,IZ,IZ)
        EVSHWR = VAL
      WRITE(NRLS,114) EVSHWR
 114      FORMAT(' EVSHWR    ',F10.3,' Frequency of shower events,',' showers/d')
!
!-- FTI Fraction of time spent indoors ---------------------------------------
!
      FTI = GETREAL(SETDATA,NLINES,'FTI           ',IZ,IZ,IZ,IZ,IZ,IZ)
!
!-- FTO Fraction of time spent outdoors --------------------------------------
!
      FTO = GETREAL(SETDATA,NLINES,'FTO           ',IZ,IZ,IZ,IZ,IZ,IZ)
!
!---- Test indoor and outdoor time fraction and adjust to not exceed 1.0 -----
!
      FTOT = FTO + FTI
      IF(FTOT.GT.1.) THEN
         FTO = FTO * 1./FTOT
         FTI = FTI * 1./FTOT
      ENDIF
      WRITE(NRLS,107) FTI  
 107      FORMAT(' FTI       ',F10.3,' Fraction of time spent indoors',  &
                 ' for external ground exposure')
      WRITE(NRLS,108) FTO  
 108      FORMAT(' FTO       ',F10.3,' Fraction of time spent outdoors', &
                 ' for external ground exposure')
!
!-- SHI Shielding factor while indoors, external soil ------------------------
!
      SHI = GETREAL(SETDATA,NLINES,'SHI           ',IZ,IZ,IZ,IZ,IZ,IZ)
      WRITE(NRLS,109) SHI  
 109      FORMAT(' SHI       ',F10.3,' Shielding factor while indoors',  &
                 ' for external ground exposure')
!
!-- SHO Shielding factor while outdoor, external soil ------------------------
!
      SHO = GETREAL(SETDATA,NLINES,'SHO           ',IZ,IZ,IZ,IZ,IZ,IZ)
      WRITE(NRLS,110) SHO  
 110      FORMAT(' SHO       ',F10.3,' Shielding factor while outdoors'  &
                ,' for external ground exposure')
!
!--    Daily exposure time for external exposure to soil (hr/d) -------------
!
      VAL = 0.0
      VAL = GETREAL(SETDATA,NLINES,'UEXT          ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(VAL.GT.0.) THEN
          UEXTAT = VAL
          UEXTSL = VAL
      ENDIF
        WRITE(NRLS,132) val
 132      FORMAT(' UEXT      ',F10.3,' Daily external exposure to soil, ',' hr/d')
!
!-- USW, inadvertent ingestion rate while swimming --------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'USW           ',IZ,IZ,IZ,IZ,IZ,IZ)
        USW = VAL
        WRITE(NRLS,117) USW
 117      FORMAT(' USW       ',F10.3,' Swimming ingestion rate, ',' L/hr')
!
!-- TESWIM, duration of a swimming event, hr ---------------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'TESWIM        ',IZ,IZ,IZ,IZ,IZ,IZ)
        TESWIM = VAL
      WRITE(NRLS,118) TESWIM
 118      FORMAT(' TESWIM    ',F10.3,' Duration of a swimming event,',' hr/event')
!
!-- EVSWIM, frequency of swimming events, events/d ---------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'EVSWIM        ',IZ,IZ,IZ,IZ,IZ,IZ)
        EVSWIM = VAL
      WRITE(NRLS,119) EVSWIM
 119      FORMAT(' EVSWVM    ',F10.3,' Frequency of swimming events,',' events/d')
!
!-- SBRF     -----------------------------------------------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'SBRF          ',IZ,IZ,IZ,IZ,IZ,IZ)
        SBRF = VAL
        WRITE(NRLS,122) SBRF
 122      FORMAT(' SBRF      ',F10.3,' Geometry factor for boating',' exposure')
!
!-- TEBOAT, duration of a boating event, hr ----------------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'TEBOAT        ',IZ,IZ,IZ,IZ,IZ,IZ)
        TEBOAT = VAL
      WRITE(NRLS,120) TEBOAT
 120      FORMAT(' TEBOAT    ',F10.3,' Duration of a boating event,',' hr/event')
!
!-- EVBOAT, frequency of boating events, events/d ----------------------------
!
        VAL = GETREAL(SETDATA,NLINES,'EVBOAT        ',IZ,IZ,IZ,IZ,IZ,IZ)
        EVBOAT = VAL
      WRITE(NRLS,121) EVBOAT
 121      FORMAT(' EVBOAT    ',F10.3,' Frequency of boating events,',' events/d')
!
!-- USED     -----------------------------------------------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'USED          ',IZ,IZ,IZ,IZ,IZ,IZ)
      USED = VAL
      WRITE(NRLS,125) USED
 125  FORMAT(' USED      ',F10.3,' Sediment ingestion rate, ',' g/hr')
!
!-- TESHOR   -----------------------------------------------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'TESHOR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      TESHOR = VAL
      WRITE(NRLS,123) TESHOR
 123      FORMAT(' TESHOR    ',F10.3,' Duration of a shoreline event,',  &
                ' hr/event')
!
!-- EVSHOR   -----------------------------------------------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'EVSHOR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      EVSHOR = VAL
      WRITE(NRLS,124) EVSHOR
 124      FORMAT(' EVSHOR    ',F10.3,' Frequency of shoreline events,',  &
                ' events/d')
!
!-- SWF, Shore width factor for external exposure to shoreline sediments -----
!
      VAL = GETREAL(SETDATA,NLINES,'SWF           ',IZ,IZ,IZ,IZ,IZ,IZ)
      SWF = VAL
      WRITE(NRLS,126) SWF
 126  FORMAT(' SWF       ',F10.3,' Shore width factor for external ',' exposure')
!
!-- USOIL    -----------------------------------------------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'USOIL         ',IZ,IZ,IZ,IZ,IZ,IZ)
      USOIL = VAL
      WRITE(NRLS,129) USOIL
 129  FORMAT(' USOIL     ',F10.3,' Soil ingestion rate, mg/d')
!
!-- EVSOIL   -----------------------------------------------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'EVSOIL        ',IZ,IZ,IZ,IZ,IZ,IZ)
      EVSOIL = VAL
      WRITE(NRLS,130) EVSOIL
 130  FORMAT(' EVSOIL    ',F10.3,' Frequency of soil contact ',' events, events/d')
!
!-- AKSIN(1), skin area for exposure to le------------------------------------
!-- AKSIN(2), skin area for exposure to  -------------------------------------
!-- AKSIN(3), skin area for exposure to --------------------------------------
!-- AKSIN(4), skin area for exposure to --------------------------------------
!
      DO IA = 1,4
        VAL = GETREAL(SETDATA,NLINES,'ASKIN         ',IA,IZ,IZ,IZ,IZ,IZ)
        ASKIN(IA) = VAL
      END DO
      WRITE(NRLS,116) (ASKIN(IA),IA=1,4)
 116      FORMAT(' ASKIN(1)  ',F10.3,' Area of skin exposed to shower',  &
                 ' water, cm2'/                                          &
                 ' ASKIN(2)  ',F10.3,' Area of skin exposed to swimming' &
                ,' water, cm2'/                                          &
                 ' ASKIN(3)  ',F10.3,' Area of skin exposed to soil'     &
                ,' dermal contact, cm2'/                                 &
                 ' ASKIN(4)  ',F10.3,' Area of skin exposed to sediment' &
                ,' dermal contact, cm2')
!
!-- ADHFAC, adherence factor for soil and sediment contact with skin ---------
!
      VAL = GETREAL(SETDATA,NLINES,'ADHFAC        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ADHFAC = VAL
      WRITE(NRLS,133) ADHFAC
 133  FORMAT(' ADHFAC    ',F10.3,' Adherence factor for soil, mg/cm2')
!
!-- ADHSED, adherence factor for sediment contact with skin ---------
!
      VAL = GETREAL(SETDATA,NLINES,'ADHSED        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ADHSED = VAL
      IF (ADHSED.LE.0.) THEN        ! older case files (i.e., without ADHSED
        ADHSED = ADHFAC             ! will set sediment adherence factor equal
      ENDIF                         ! to soil adherence factor
      WRITE(NRLS,135) ADHSED
 135  FORMAT(' ADHSED    ',F10.3,' Adherence factor for sediment, mg/cm2')
!
!-- SKINL, thickness of skin for dermal absorption ---------------------------
!
      VAL = GETREAL(SETDATA,NLINES,'SKINL         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SKINL = VAL
      WRITE(NRLS,127) SKINL
 127  FORMAT(' SKINL     ',F10.3,' Skin thickness for dermal absorption'  &
               , ' microns')
!
!----- Read annual frequency factors for each exposure pathway ---------------
!
      DO IA = 1,24
         FREQ(IA)=GETREAL(SETDATA,NLINES,'FREQ          ',IA,IZ,IZ,IZ,IZ,IZ)
         NE = LEN_TRIM(EPATH(IA))
         WRITE(NRLS,131) IA,FREQ(IA),EPATH(IA)(1:NE)
 131     FORMAT(' FREQ(',I2,')  ',F10.3,' Annual frequency factor for',1X,A)
      END DO
!
      RETURN
!
!================ END OF MODULE BLKREAD ====================================
!
      END
!
!   MEPAS AHAZ: NEWPOL.FOR             Version Date: 07-Nov-95
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE NEWPOL                                *
!                                                                            *
!  Subroutine NEWPOL test to see if current pollutant has been evaluated     *
!                    previously in the same run.                             *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    07/11/95 (Converted from HAZ2 version)                  *
!  Last Modified:    07-Nov-95      DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: SUBROUTINE AHAZ
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     CAS(2)     U     REAL    Argument  Name of pollutant to be tested
!     NEW        S     LOGIC.  Argument  Logical flag, TRUE if new pollutant
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE NEWPOL(CAS,NEW)
!
!==== COMMON Block Definitions ===============================================
!
!==== DIMENSION Statements ===================================================
!
      CHARACTER*4 CAS(3),OLDCAS(3,200)
!
!==== Variable Declarations ==================================================
!
      LOGICAL NEW
      INTEGER NPOL
!
!==== DATA Statements ========================================================
!
      DATA NPOL/0/
!
      NEW = .TRUE.
      IF(NPOL.LE.0) THEN
        OLDCAS(1,1) = CAS(1)
        OLDCAS(2,1) = CAS(2)
        OLDCAS(3,1) = CAS(3)
        NPOL = 1
      ELSE
        DO 100 IP = 1,NPOL
          IF(CAS(1).NE.OLDCAS(1,IP)) GO TO 100
          IF(CAS(2).NE.OLDCAS(2,IP)) GO TO 100
          IF(CAS(3).NE.OLDCAS(3,IP)) GO TO 100
          NEW = .FALSE.
          GO TO 200
 100    CONTINUE
        NPOL = NPOL + 1
        OLDCAS(1,NPOL) = CAS(1)
        OLDCAS(2,NPOL) = CAS(2)
        OLDCAS(3,NPOL) = CAS(3)
      ENDIF
 200  RETURN
!================ END OF MODULE NEWPOL =====================================
      END
!   MEPAS AHAZ: GETCON.FOR             Version Date:  21-Nov-95
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETCON                                *
!                                                                            *
!  This subroutine reads one set of water concentration vs. time data from   *
!                 the general water PDCF in free field format.               *
!                 The number of time periods is input.                       *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    21-Nov-1995 (New for 100 year update)                   *
!  Last Modified:    21 Nov 1995  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: AHAZ
!     Calls: NONE
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!      NTIMC      U    INT     Argument  Number of data lines to read
!      XMF        U    Real    Argument  Units conversion factor
!      CWTIME     S    Real    Argument  Array of water concentration values
!      TIMES      S    Real    Argument  Array of time points for water data
!      TSTART     S    Real    Argument  Start of water concentration times
!      TEND       S    Real    Argument  End of water concentration times
!      IERR       S    Int     Argument  Error flag for file read errors
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   21-Nov-1995    DLS  Created for 100 year annual update version
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETCON(NTIMC,XMF,CWTIME,TIMES,TRELST,TEND,IERR) 
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DEVICE.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION CWTIME(100),TIMES(100)
!
!==== Variable Declarations ==================================================
!
      INTEGER NTIMC,IERR
      REAL TRELST,TEND
!
!==== DATA Statements ========================================================
!
!==== INITIALIZATIONS ========================================================
!
      IERR = 0
!
!==== START OF ANALYSIS ======================================================
!
!  Test value of NTIMC for range
!
      IF(NTIMC.LE.0.OR.NTIMC.GT.100) THEN
        IERR = IERR + 1
        WRITE(NERR,200) NTIMC
 200    FORMAT(' NTIMC out of range in GETCON =',I6)
        RETURN
      ENDIF
      DO ITIM = 1,NTIMC
        READ(NCIN,*,END=998,ERR=999) TIMES(ITIM),CWTIME(ITIM)
        CWTIME(ITIM) = CWTIME(ITIM) * XMF
      END DO
      TRELST = TIMES(1)
      TEND = TIMES(NTIMC)
!
!==== N0RMAL RETURN ==========================================================
!
      RETURN
!
!==== PROCESS READ ERROR CONDITIONS ==========================================
!
998   WRITE(NERR,100) ITIM
 100  FORMAT(' End of file on water concentration input in GETCON',  &
            /' for time period ',I6)
      IERR = IERR + 1
      RETURN
999   WRITE(NERR,300) ITIM
 300  FORMAT(' Error in reading water concentration file in GETCON',  &
            /' during time period ',I6)
      IERR = IERR + 1
      RETURN
      END
!=================== END OF MODULE GETCON ===================================
!   MEPAS HAZ2: ATINPA.FOR             Version Date: 12-Aug-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ATINPA                                *
!                                                                            *
!  Subroutine ATINPA This module reads records to define the usage           *
!             practices at each atmospheric agricultural usage location      *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    02/28/86 (Converted to PC)                              *
!  Last Modified:    12-Aug-97   DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZI
!     Called by: SUBROUTINE USEDAT
!     Calls: NONE
!     Common blocks referenced: AIRDAT, ATPATH, AGCLAS, DEVICE, LOCNAM, PSET1, PROD
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     variable        S    CHAR  Argument  nnnnnnnnnnnnnnnnnnnnnn
!                     U    INT   Internal
!                     S/U  REAL  Common
!                          DBLE  External
!
!==== Modification History ===================================================
!
!     Date           Who  Modification Description
!     --------       ---  ------------------------------------------------------
!     April 1, 1986  DLS  Moved format statements to end of subroutine
!     June 10, 1987  DLS  Additional parameter in call to SETPOP
!     March 5, 1990  JWB  Modified output for HPI file
!     28-JUL-92      DLS  Modification of heading information
!     27-Aug-92      DLS  Add input of body weight, exposure duration, and
!                         averaging time
!     12-OCT-92      DLS  Deleted averaging time from input, set to exp.dura.
!     01-Dec-92      DLS  Added user input of parameters for inhalation and
!                         soil ingestion paths (ATBWI,ATEDI,ATATI,ATUSI,EXQI
!                         and DXQI
!     01-Dec-92      DLS  Added user definition of distance and direction to
!                         location of MI for inhalation and soil pathways
!     05-Feb-93      DLS  Initialization of EXQI and DXQI arrays added
!     21-Oct-93      DLS  Add write to *.INT file
!     30-Dec-93      DLS  Added generation of SIFs
!     11-Jan-94      DLS  Finished SIF modifications
!     10-Feb-94      DLS  Completed changes for SIF update (units)
!     18-Mar-94      DLS  Set BW and ED when SIFs are input
!     31-May-94      DLS  Corrected exposure duration units to y, in output
!     27-Jun-94      DLS  Use KNSR to print emission heading for air-as-source
!     01-Dec-94      DLS  Fixed call AIRPTH call list syntax error
!     04-Dec-95      DLS  Expanded intake parameters for receptor location
!     04-Dec-95      DLS  Corrected units on air inhalation SIF for printing
!     13-Dec-95      DLS  Removed reading of air release rate file
!     13-Dec-95      DLS  Added XOQDOQ.FTN for normalized population parameters
!     13-Dec-95      DLS  Removed NU from argument list
!     18-Dec-95      DLS  Created new version that analyzes only agricultural
!                         data for atmospheric releases
!     11-Feb-97      DLS  Added name "Agricultural Air" to population output
!                         line in .INA file
!  12-Aug-97  DLS  Added input of population data for agricultural pathways
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ATINPA(SETDATA,NLINES,IERR)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'PARMTR.PAR'
      include 'AIRDAT.FTN'
      include 'ATPATH.FTN'
      include 'AGCLAS.FTN'
      include 'DERMDAT.FTN'
      include 'DEVICE.FTN'
      include 'LOCNAM.FTN'
      include 'PFLAGS.FTN'
      include 'PSET1.FTN'
      include 'PROD.FTN'
      include 'PTHUSE.FTN'
      include 'XOQDOQ.FTN'
!
!==== DIMENSION Statements ===================================================
!
       CHARACTER*2 ATANAM(4)
       CHARACTER*5 ATTYPE(3)
       CHARACTER*11 ATAUNT(3,4)
       INTEGER IZ
       CHARACTER*(*) SETDATA(LINEMAX)
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
      DATA ATANAM/'LV','OV','MT','MK'/
      DATA ATTYPE/'Non-c','Carc.','Rad. '/
      DATA ATAUNT/'    kg/kg/d','    kg/kg/d','      kg   ',  &  !lv
                  '    kg/kg/d','    kg/kg/d','      kg   ',  &  !ov
                  '    kg/kg/d','    kg/kg/d','      kg   ',  &  !mk
                  '     L/kg/d','     L/kg/d','       L   '/     !mk
!
!----- Start calculations.  Initialize error index. -------------------------
!
      IERR=0
!
!
!---- process data for agricultural locations
!
!----- Read data for agricultural usage locations
!
!
!----- Read population exposed for each farm product -------------------------
!
      DO IC = 4,7
!           VAL = GETREAL(SETDATA,NLINES,'POPAIR        ',IC,IZ,IZ,IZ,IZ,IZ)
!           IF(VAL.GT.0.) POPAIR(IC-3) = VAL
           VAL = GETREAL(SETDATA,NLINES,'FREQ          ',IC,IZ,IZ,IZ,IZ,IZ)
           IF(VAL.GT.0.) FREQ(IC) = VAL
      END DO
!
!----- Read body weight and exposure duration for this run -------------------
!
          VAL = GETREAL(SETDATA,NLINES,'BODYWT        ',IZ,IZ,IZ,IZ,IZ,IZ)
          IF(VAL.GT.0.) BW = VAL
          VAL = GETREAL(SETDATA,NLINES,'EXPDUR        ',IZ,IZ,IZ,IZ,IZ,IZ)
          IF(VAL.GT.0.) ED = VAL
              IF(BW.LE.0.) BW = 70.
              ATBW = BW
              IF(ED.LE.0.) ED = 70.
              ATED = ED
!
!  Set SIFs for agricultural foods for
!
              DO IC = 4,7     ! Agricultural pathwayss
                  IF(UAGAT(IC-3).LE.0.) UAGAT(IC-3) = UAG(IC-3)
                  TERM = UAGAT(IC-3)*FREQ(IC)
                  CALL SETSIF(TERM,BW,ED,ATGSIF(1,IC-3),ATGSIF(2,IC-3),ATGSIF(3,IC-3) )
              END DO
!
!----- Set values into arrays ----------------------------------------------
!
          DO IC = 1,4
            IF(UAGAT(IC).LE.0.) UAGAT(IC) = UAG(IC)
          END DO
  10    CONTINUE
!
!----- Write current usage location data to output report ------------------
!-----
         LLL = 1
          write(NRLS,2001) LLL
          write(NRLS,'(28(''-''))')
 2001     format( 'Usage Location    ',2x,('  Location '),/,' Parameter    Units ',5x,I2 )
          write(NRLS,2100) atbw
 2100     format('Body weight:  kg    ',1p,1x,E10.3)
          write(NRLS,2101) ATED
 2101     format('Exposure Duration: y',1p,1x,E10.3)
        DO IEX = 1,4
            write(NRLS,2301) ATANAM(IEX),ATTYPE(1),ATAUNT(1,IEX),ATGSIF(1,IEX)
            write(NRLS,2301) ATANAM(IEX),ATTYPE(2),ATAUNT(2,IEX),ATGSIF(2,IEX)
            write(NRLS,2301) ATANAM(IEX),ATTYPE(3),ATAUNT(3,IEX),ATGSIF(3,IEX)
        END DO
 2301       FORMAT(A2,':',A5,1X,A11,1P,5(1X,E10.3))
 2302       FORMAT('Source of SIF Values',5(2x,A8,1x))
!          write(NRLS,2008) POPAIR(1)
! 2008     format('Leaf Veg Population:',1p,1x,E10.3)
          write(NRLS,2204) uagAT(1)
 2204     format('Leaf Veg rate:kg/d  ',1p,1x,E10.3)
!          write(NRLS,2009) POPAIR(2)
! 2009     format('Other Population:   ',1p,1x,E10.3)
          write(NRLS,2205) uagAT(2)
 2205     format('Othr Veg rate:kg/d  ',1p,5(1x,E10.3))
!          write(NRLS,2010) POPAIR(3)
! 2010     format('Meat Population:    ',1p,1x,E10.3)
          write(NRLS,2206) uagAT(3)
 2206     format('Meat Ing rate:kg/d  ',1p,1x,E10.3)
!          write(NRLS,2011) POPAIR(4)
! 2011     format('Milk Population:    ',1p,1x,E10.3,//)
          write(NRLS,2207) uagAT(4)
 2207     format('Milk Ing rate: L/d  ',1p,1x,E10.3)
   11 CONTINUE
!
      IF(IERR.GT.0) STOP 402
      RETURN
!
!----- Format Statements ----------------------------------------------------
!
  100 FORMAT(5A4,I5,9E10.1)
  200 FORMAT(6E10.3)
  300 FORMAT(I5)
  600 FORMAT(7E10.1/7E10.1)
 3000 FORMAT('ERROR IN AGRICULTURAL CLASS INDEX IN ATINPA,'/,       &
          ' VALUE GIVEN =',I4,', THE MAXIMUM ALLOWED IS ',I3)
 3001 FORMAT('END-OF-FILE ENCOUNTERED ON UNIT ',I3,' WHILE READING',&
         /' ATMOSPHERIC TRANSPORT USAGE LOCATION DATA')
 3002 FORMAT('ERROR IN AGRICULTURAL LOCATION SPECIFICATION FOR ',   &
         'LOCATION',I3/, ' DIRECTION INDEX = ',I4,', DISTANCE = ',1PE10.2)
!
!============== END OF MODULE ATINPA ========================================
      END 
!     MEPAS RADCON: AVGCON.FOR            Version Date: 20-Nov-1995
!     Copyright 1989 by Battelle Memorial Institute. All rights reserved.
! ****************************************************************************
!                                                                            *
!                         FUNCTION AVGCON                                    *
!                                                                            *
!  FUNCTION AVGCON finds the average concentration between times T0 and      *
!  T1 by integrating between these two times and dividing by the input.      *
!  time period, ntimes.                                                      *
!  The two concentration values C0 and C1 are obtained through linear        *
!  interpolation of the source (PNT(J)) and time (TIM(J)) table.             *
!  This function is called from the subroutine HAZ.                          *
!                                                                            *
!  Written by:       James Stroh                                             *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    08/30/93                                                *
!  Last Modified:    20-Nov-1995 DLS (from FUNCTION AVG70Y)                  *
!                                                                            *
! ***************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: HAZ
!     Called by: SUBROUTINE HAZ
!     Calls to: NONE
!     Common blocks referenced: INTER, CTIME, INDEX3
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/           Location
!     Name      Used   Type    Export/Import  Parameter Description
!     --------- -----  ------  -------------- --------------------------------
!     T0        USED   REAL    ARG - IMPORT   Start time for the integral
!
!     T1        USED   REAL    LOCAL          End time for the integral
!
! (   M1        USED   INT     ARG - IMPORT   Parent number) Use Deleted
!
!     NYR       USED   INT     ARG            Number of years in averaging
!                                             period (years).  20-Nov-1995
!
!    PNT        USED   REAL    ARG            Concentration array
!
!    TIME       USED   REAL    ARG            Time points corresponding to
!                                             concentration values (years)
!
!     CO        USED   REAL    LOCAL          Concentration corresponding to
!                                             T0
!     C1        USED   REAL    LOCAL          Concentration corresponding to
!                                             T1
!     TEND1     USED   REAL    COMMON - IMP   END OF concentration data
!
!     TSTRT1    USED   REAL    COMMON - IMP   START OF concentration data
!
!     JSTART    USED   INT     LOCAL          Starting node for conc. and time
!                                             pair.
!     JEND      USED   INT     LOCAL          Ending node for conc. and time
!                                             pair.
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     11/02/93     JPM  Corrected "subscript out of range" error that occurred
!                       when TEND1 and TSTRT1 were an even multiple of 70.
!
!     20-Nov-95    DLS  Modified program to use variable time period, instead
!                       of the fixed 70 year time period.
!
!==== FUNCTION CALL ========================================================
!
      FUNCTION AVGCON (T0,NYR,TSTRT1,TEND1,PNT,TIME,IERRF)
!
!----COMMON Block Definitions ------------------------------------------------
!    None
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION PNT(1000),TIME(1000)
!
!==== Variable Declarations ==================================================
!
      REAL T1,C0, C1, Area
      INTEGER J, JSTART, JEND, NYR
!
!==== DATA Statements ========================================================
!
!     None
!
! If the starting time is out of range, return a zero concentration
!
      IERRF = 0
      IF (T0 .LT. TSTRT1 .OR. T0 .GE. TEND1 ) THEN
      AVGCON = 0.0            ! Modified 20-Nov-1995 DLS
      RETURN
      END if
! *************************************************************
      IF(NYR.LE.0) GO TO 999
      TYR = FLOAT(NYR)
      T1 = T0 + TYR
!
! If the end point of the integral is greater than TEND1, reset the end point
! to equal TEND1 (minus some small delta so that the interp routine is ok).
!
      IF ( T1 .GE. TEND1 )    T1 = TEND1 * 0.9999999
!
! Find the data points which surround the start time
!
      J = 2
      DO WHILE ( TIME(J) .LE. T0 )
          J = J+1
      END DO

! Interpolate to find the starting concentration for the integration
      C0 = ( (pnt(j) - pnt(j-1))/(time(j) - time(j-1)) )  &
          * (T0 - time(j-1)) + pnt(j-1)
      JSTART = J
!
! Find the data points which surround the end time
! (Let J start from where JSTART left off)
!
      DO WHILE ( TIME(J) .LE. T1 )
          J = J + 1
      END DO

! Interpolate to find the ending concentration for the integration
      C1 = ( (pnt(j) - pnt(j-1))/(time(j) - time(j-1)) )  &
          * (T1 - time(j-1)) + pnt(j-1)
      JEND = J
!
!  Find the area under the curve for the 70 year interval.
!       Area of a trapazoid:  A = 1/2 * dt * ( F(0) + F(1) )
!
      j = JSTART
      if( JSTART .LT. JEND ) then
          Area = 0.5 * ( time(j) - T0 ) * ( PNT(j) + C0 )
          j = j+1
          Do while ( j .lt. JEND )
              Area = Area + 0.5 * ( time(j) - time(j-1) )  &
                                * ( PNT(j)  + PNT(j-1) )
              j = j+1
          end do
          Area = Area + 0.5*( T1 - time(j-1)) * ( C1 + PNT(j-1) )
      else
          Area = TYR/2. * (C0 + C1)
      end if
!
! Divide the area by the time interval to get the average concentration
!
      AVGCON = Area / TYR
      RETURN
 999  CONTINUE
!     WRITE(*,100) NYR, TO
      IERRF = 1
      RETURN
      END
!   MEPAS AHAZ: GETREL.FOR             Version Date:  11-Sep-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETREL                                *
!                                                                            *
!  This subroutine reads one set of atmospheric release rate data from the   *
!                 atmospheric output file .ARR.                              *
!                 The number of time periods is input.                       *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    14-Dec-1995 (New for 100 year update)                   *
!  Last Modified:    11-Ssep1996  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by: AHAZ
!     Calls: NONE
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!      NTIMA      U    INT     Argument  Number of data lines to read
!      XMF        U    Real    Argument  Units conversion factor
!  CATIME(7,281)  S    Real    Argument  Array of air release rates by class
!      TIMES      S    Real    Argument  Array of time points for air data
!     TRELST      S    Real    Argument  Start of air release rate times
!      TEND       S    Real    Argument  End of air release rate times
!     NCREL(7)    S  Logical   Argument  Flag to indicate if a release class
!                                        has any positive values for release
!      IERR       S    Int     Argument  Error flag for file read errors
!
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   14-Dec-1995    DLS  Created for 100 year annual update version
!   11-Sep-1996    DLS  Increased dimensions on CATIME and TIMES to 281
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETREL(NTIMA,XMF,CATIME,TIMES,TRELST,TEND,NCREL,IERR)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DEVICE.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION CATIME(7,281),TIMES(281),NCREL(7)
!
!==== Variable Declarations ==================================================
!
      INTEGER NTIMA,IERR
      LOGICAL NCREL
      REAL TRELST,TEND
!
!==== DATA Statements ========================================================
!
!==== INITIALIZATIONS ========================================================
!
      IERR = 0
!
!==== START OF ANALYSIS ======================================================
!
!  Test value of NTIMA for range
!
      IF(NTIMA.LE.0.OR.NTIMA.GT.281) THEN
        IERR = IERR + 1
        WRITE(NERR,200) NTIMA
 200    FORMAT(' NTIMA out of range in GETREL =',I6)
        RETURN
      ENDIF
      DO IC = 1,7
        NCREL(IC) = .FALSE.
      END DO
      DO ITIM = 1,NTIMA
        READ(NARR,*,END=998,ERR=999) TIMES(ITIM),(CATIME(I,ITIM),I=1,7)
        DO IC = 1,7
          CATIME(IC,ITIM) = CATIME(IC,ITIM) * XMF
          IF(CATIME(IC,ITIM).GT.0.) NCREL(IC) = .TRUE.
        END DO
      END DO
      TRELST = TIMES(1)
      TEND = TIMES(NTIMA)
!
!==== N0RMAL RETURN ==========================================================
!
      RETURN
!
!==== PROCESS READ ERROR CONDITIONS ==========================================
!
998   WRITE(NERR,100) ITIM
 100  FORMAT(' End of file on air release rate input in GETREL',  &
            /' for time period ',I6)
      IERR = IERR + 1
      RETURN
999   WRITE(NERR,300) ITIM
 300  FORMAT(' Error in reading air release rate file in GETREL', &
            /' during time period ',I6)
      IERR = IERR + 1
      RETURN
      END
!=================== END OF MODULE GETREL ===================================
!   GETSTR.FOR                         Version Date: 29-Oct-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETSTR                                *
!                                                                            *
!  Subroutine GETSTR extracts a string parameter value from the .GID file.   *
!                                                                            *
!  Written by:       Karl Castleon                                           *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-95                                               *
!  Last Modified:    29-Oct-96      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL USE
!     Called by:
!     Calls: MOVETO, SEQ
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    SETDATA     U      CHAR   Argument  Array of line images to search for an
!                                        integer parameter
!    NAME        U      CHAR   Argument  Name of parameter sought.
!    C1          U      INT    Argument  CONSTITUENT COUNTER
!    C2          U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
!    C3          U      INT    Argument  LOCATION COUNTER
!    C4          U      INT    Argument  FLUX COUNTER
!    C5          U      INT    Argument  MONTH COUNTER
!    C6          U      INT    Argument  MISCELANEOUS COUNTER
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     06-Dec-95    DLS  Added heading information
!     29-Oct-96    DLS  Converted for use with the Framework program
!==== SUBROUTINE CALL ========================================================
!
      CHARACTER*80 FUNCTION GETSTR(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES,LINE
      CHARACTER NAME*14
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER USUNTS*8,UNITS*8,TNAME*14,VALUE*80
!
!---- Start of analyses ------------------------------------------------------
!
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,VALUE
        GETSTR=VALUE
      ELSE
        GETSTR='NOT FOUND'
      END IF
      END
!   GETINT.FOR                       Version Date: 29-Oct-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETINT                                *
!                                                                            *
!  Subroutine GETINT extracts an integer value from the .GID file            *
!                                                                            *
!  Written by:       Karl Castleton                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-93                                               *
!  Last Modified:    29-Oct-96      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ
!     Called by: SUBROUTINE GETINTAK
!     Calls: MOVETO
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     SETDATA    U      CHAR   Argument  Array of line images for extraction
!                                        of an integer parameter
!     ***THE ARRAY MUST ALREADY BE FILED***
!     NAME       U      CHAR   Argument  Name of parameter sought
!     C1         U      INT    Argument  CONSTITUENT COUNTER
!     C2         U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
!     C3         U      INT    Argument  LOCATION COUNTER
!     C4         U      INT    Argument  FLUX COUNTER
!     C5         U      INT    Argument  MONTH COUNTER
!     C6         U      INT    Argument  MISCELANEOUS COUNTER
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     06-Dec-95    DLS  Added heading information
!==== SUBROUTINE CALL ========================================================
!
      INTEGER FUNCTION GETINT(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES
      CHARACTER NAME*14
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER UNITS*8,USUNTS*8,TNAME*14
      INTEGER VALUE
!
!---- Start of Analysis
!    MOVETO finds the record having the desired parameter value
!
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,VALUE
!
        GETINT=VALUE
      ELSE
        GETINT=0
      END IF
      END
!
!   MEPAS HAZ2: SEQ.FOR                Version Date: 06-Dec-95
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SEQ                                   *
!                                                                            *
!  Subroutine SEQ is used by functions to extract values from the .PRM file  *
!                    values if a value was found in the .PRM file.           *
! THIS CHECKS TO SEE IF THE NAMES OF THE PARAMETERS MATCH. RETURNS
! A .TRUE. IF THEY DO MATCH .FALSE. IF THEY DON'T. IT COMPARES ONLY
! THE FIRST 14 CHARACTERS AND STOPS AT THE FIRST SPACE IT FINDS
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    29-Nov-95                                               *
!  Last Modified:    04-Dec-95      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ
!     Called by: SUBROUTINE HAZ
!     Calls: SUBROUTINE GET.. ROUTINES FOR READING .PRM FILES
!     Common blocks referenced: PTHUSE,DERMDAT,DRPATH,DEVICE,COUPLE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    NAME1       U      STR    Argument  Name of parameter from .PRM file
!    NAME2       U      STR    Argument  Name of parameter searching for
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     06-Dec-95    DLS  Added heading information
!==== SUBROUTINE CALL ========================================================
!
      LOGICAL FUNCTION SEQ(NAME1,NAME2)
!
!---- Variable type declarations ---------------------------------------------
!
      CHARACTER*14 NAME1,NAME2
      LOGICAL TEMP
      INTEGER POS
!
      TEMP=.TRUE.
      POS=1
 320  IF (NAME1(POS:POS) .EQ. NAME2(POS:POS)) THEN
      POS=POS+1
      ELSE
      TEMP=.FALSE.
      END IF
      IF (POS .LE. 14 .AND. TEMP .AND. NAME1(POS:POS) .GT. ' '   &
        .AND. NAME2(POS:POS) .GT. ' ') THEN
      GOTO 320
      END IF
      SEQ=TEMP
      END
!   MEPAS HAZ2: GETINTAK.FOR           Version Date: 02-Jul-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETINTAK                              *
!                                                                            *
!  Subroutine GETINTAK extracts data from the .GID file and resets default   *
!                    values if a value was found in the .GID file.           *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    29-Nov-95                                               *
!  Last Modified:    02-Jul-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ
!     Called by: SUBROUTINE HAZ
!     Calls: SUBROUTINE GET.. ROUTINES FOR READING .PRM FILES
!     Common blocks referenced: PTHUSE,DERMDAT,DRPATH,DEVICE,COUPLE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    IPATH       U      INT    Argument  Transport path indicator (1-GW, etc).
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     30-Nov-95    DLS  Corrected index order on UDW
!     04-Dec-95    DLS  Expanded input to include receptor locations
!     19-Dec-95    DLS  Changed SW to 2, Air regional = 3, Air agr. = 4
!                       Fixed spelling error in UEXTAT
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETINTAK(SETDATA,NLINES,IPATH,IERR)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'PTHUSE.FTN'
      INCLUDE 'DERMDAT.FTN'
      INCLUDE 'DRPATH.FTN'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'COUPLE.FTN'
!
!==== DIMENSION Statements ===================================================
!
!
!==== Variable Declarations ==================================================
!
      INTEGER IPATH,IZ
      CHARACTER*(*) SETDATA(LINEMAX)
      REAL GETREAL
      INTEGER :: IERR
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
!
!---- Get data --------------------------------------------------------------
!
!---- Drinking water intake rate, L/d ---------------------------------------
!
        VAL = 0.0
        IF(IPATH.EQ.1) THEN
           VAL = GETREAL(SETDATA,NLINES,'UDWGW         ',IZ,IZ,IZ,IZ,IZ,IZ)
        ELSE IF(IPATH.EQ.2) THEN
           VAL = GETREAL(SETDATA,NLINES,'UDWSW         ',IZ,IZ,IZ,IZ,IZ,IZ)
        ENDIF
        IF(VAL.GT.0.) THEN   ! If a value was found, then set UDW
           CALL LINOUT(IPATH,VAL,1)
           UDW(IPATH) = VAL
        ENDIF
!
!---- Shower frequency, showers/day -----------------------------------------
!
      VAL = 0.0
      IF(IPATH.EQ.1.OR.IPATH.EQ.2) THEN
         VAL = GETREAL(SETDATA,NLINES,'EVSHWR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
      IF(VAL.GT.0.) THEN
        IF(IPATH.EQ.1) USHWRG = VAL
        IF(IPATH.EQ.2) USHWRS = VAL
        CALL LINOUT(IPATH,VAL,2)
      ENDIF
!
!---- Swimming events per day, events/day -----------------------------------
!
        VAL = 0.0
        VAL = GETREAL(SETDATA,NLINES,'EVSWIM        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) THEN
          CALL LINOUT(IPATH,VAL,10)
          UEVSWIM = VAL
        ENDIF
!
!---- Shoreline events per day, events/day ----------------------------------
!
        VAL = 0.0
        VAL = GETREAL(SETDATA,NLINES,'EVSHOR        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) THEN
          CALL LINOUT(IPATH,VAL,12)
          UEVSHOR = VAL
        ENDIF
!
!---- Soil ingestion rate, g/day --------------------------------------------
!
      VAL = 0.0
      IF(IPATH.EQ.3) THEN
        VAL = GETREAL(SETDATA,NLINES,'USOIL         ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.LT.0.0) THEN
           IERR = IERR + 1
           WRITE(NERR,'(A,1P,E10.3)') ' Error in soil ingestion rate value, must be >= 0 ',VAL
        ENDIF
      ELSE IF(IPATH.EQ.5) THEN
        VAL = GETREAL(SETDATA,NLINES,'USOIL         ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.LT.0.0) THEN
           IERR = IERR + 1
           WRITE(NERR,'(A,1P,E10.3)') ' Error in soil ingestion rate value, must be >=0 ',VAL
        ENDIF
      ENDIF
        IF(VAL.GT.0.) THEN
          IF(IPATH.EQ.3) THEN
            CALL LINOUT(IPATH,VAL,14)
            USLAT = VAL
          ELSE IF(IPATH.EQ.5) THEN
            CALL LINOUT(IPATH,VAL,14)
            USLSL = VAL
          END IF
        END IF
!
!---- Soil dermal events, events/d ------------------------------------------
!
      VAL = 0.0
      IF(IPATH.EQ.3) THEN
        VAL = GETREAL(SETDATA,NLINES,'EVSOIL        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ELSE IF(IPATH.EQ.5) THEN
        VAL = GETREAL(SETDATA,NLINES,'EVSOIL        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
        IF(VAL.GT.0.) THEN
          IF(IPATH.EQ.3) THEN
            CALL LINOUT(IPATH,VAL,15)
            UVSLAT = VAL
          ELSE IF(IPATH.EQ.5) THEN
            CALL LINOUT(IPATH,VAL,15)
            UVSLSL = VAL
          END IF
        END IF
!
!---- Shower inhalation rate, m^3/d -----------------------------------------
!
      VAL = 0.0
      IF(IPATH.EQ.1) THEN
         VAL = GETREAL(SETDATA,NLINES,'UBRSH         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ELSE IF(IPATH.EQ.2) THEN
         VAL = GETREAL(SETDATA,NLINES,'UBRSH         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
      IF(VAL.GT.0.) THEN
        CALL LINOUT(IPATH,VAL,17)
        IF(IPATH.EQ.1) UBRSHG = VAL
        IF(IPATH.EQ.2) UBRSHS = VAL
      ENDIF
!
!---- Air inhalation rate, m^3/d --------------------------------------------
!
      VAL = 0.0
      IF(IPATH.EQ.3) THEN
        VAL = GETREAL(SETDATA,NLINES,'UBRAT         ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) THEN
          CALL LINOUT(IPATH,VAL,18)
          UBRAT = VAL
        ENDIF
      ENDIF
!
!---- Inhalation rate for soil resuspension, m^3/d --------------------------
!
      VAL = 0.0
      IF(IPATH.EQ.3) THEN
        VAL = GETREAL(SETDATA,NLINES,'UBRES         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ELSE IF(IPATH.EQ.5) THEN  
        VAL = GETREAL(SETDATA,NLINES,'UBRES         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
      IF(VAL.GT.0.) THEN
        IF(IPATH.EQ.3) THEN
          CALL LINOUT(IPATH,VAL,19)
          UBRESA = VAL
        ELSE IF(IPATH.EQ.5) THEN
          CALL LINOUT(IPATH,VAL,19)
          UBRESL = VAL
        ENDIF
      ENDIF
!
!---- Boating events per day, events/day ------------------------------------
!
      VAL = 0.0
      IF(IPATH.EQ.2) THEN
        VAL = GETREAL(SETDATA,NLINES,'EVBOAT        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) THEN
          CALL LINOUT(IPATH,VAL,21)
          UEVBOAT = VAL
        ENDIF
      ENDIF
!
!---- Exposure time for external soil radiation, hr/d -----------------------
!
      VAL = 0.0
      IF(IPATH.EQ.3.OR.IPATH.EQ.5) THEN
        VAL = GETREAL(SETDATA,NLINES,'UEXT          ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
      IF(VAL.GT.0.) THEN
        IF(IPATH.EQ.3) THEN
          CALL LINOUT(IPATH,VAL,23)  
          UEXTAT = VAL
        ELSE IF(IPATH.EQ.5) THEN
          CALL LINOUT(IPATH,VAL,23)
          UEXTSL = VAL
        END IF
      ENDIF
!
!---- End of loop on receptor locations -------------------------------------
!
      RETURN
!================ END OF MODULE GETINTAK ===================================
      END
!   MEPAS HAZ2: LINOUT.FOR             Version Date: 02-Jul-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE LINOUT                                *
!                                                                            *
!  Subroutine LINOUT writes a line of information describing a change in     *
!                    intake parameter value, from the .PRM file, read by     *
!                    subroutine GETINTAK
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    29-Nov-95                                               *
!  Last Modified:    02-Jul-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZI
!     Called by: SUBROUTINE GETINTAK
!     Calls: NONE
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    NOUT        U      INT    DEVICE    Logical unit of output file
!    IPATH       U      INT    Argument  Transport path indicator (1-GW, etc).
!    VAL         U      REAL   Argument  Value to be printed
!    IEX         U      INT    Argument  Exposure pathway index
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   04-Dec-1995    DLS  Added location index, ILOC
!   18-Dec-1995    DLS  Changed SW to 2, Air regional = 3, air agr. = 4
!   02-Jul-97      DLS  Removed location dependence (ILOC)
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE LINOUT(IPATH,VAL,IEX)
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'DEVICE.FTN'
!
!==== DIMENSION Statements ===================================================
!
!==== Variable Declarations ==================================================
!
      INTEGER IPATH,IEX
      CHARACTER*37 TEXT(25),TOUT
!-- Inline function for different len function for different compilers
      character*37 S
!      integer mylen
!      mylen(S)=NBLANK(S)
!
!==== DATA Statements ========================================================
!
      DATA  TEXT/'Water ingestion rate, L/d            ',    &
                 'Shower frequency, showers/d          ',    &
                 '3                                    ',    &
                 'Leafy vegetable ingestion rate, kg/d ',    &
                 'Other vegetable ingestion rate, kg/d ',    &
                 'Meat ingestion rate, kg/d            ',    &
                 'Milk ingestion rate, L/d             ',    &
                 'Finfish ingestion rate, kg/d         ',    &
                 'Shell fish ingestion rate, kg/d      ',    &
                 'Swimming events per day, events/d    ',    &
                 '11                                   ',    &
                 'Shoreline events per day, events/d   ',    &
                 '13                                   ',    &
                 'Soil ingestion rate, g/d             ',    &
                 'Soil event frequency, events/d       ',    &
                 '16                                   ',    &
                 'Shower inhalation rate, m3/d         ',    &
                 'Air inhalation rate, m3/d            ',    &
                 'Resuspension inhalation rate, m3/d   ',    &
                 '20                                   ',    &
                 'Boating events per day, events/d     ',    &
                 '22                                   ',    &
                 'Soil external exposure time, hr/d    ',    &
                 'Air external exposure time, hr/d     ',    &
                 'Direct radiation time, hr/d          '/
!
!---- Write a line describing the changed parameter -------------------------
!  For "transport" route, A37, was set to ',1pe8.2
!
        IF(IPATH.EQ.1) THEN
         TOUT = TEXT(IEX)
         WRITE(NRLS,100) 'groundwater', TRIM(TOUT),VAL
        ELSE IF(IPATH.EQ.2) THEN
         TOUT = TEXT(IEX)
         WRITE(NRLS,100) 'surface water',TRIM(TOUT),VAL
        ELSE IF(IPATH.EQ.3) THEN
         TOUT = TEXT(IEX)
         WRITE(NRLS,100) 'air regional',TRIM(TOUT),VAL
        ELSE IF(IPATH.EQ.4) THEN
         TOUT = TEXT(IEX)
         WRITE(NRLS,100) 'air agricultural',TRIM(TOUT),VAL
        ELSE IF(IPATH.EQ.5) THEN
         TOUT = TEXT(IEX)
         WRITE(NRLS,100) 'measured soil',TRIM(TOUT),VAL
        ELSE IF(IPATH.EQ.7) THEN
         TOUT = TEXT(IEX)
         WRITE(NRLS,100) 'direct radiation',TRIM(TOUT),VAL
        ENDIF
!
!----- FORMAT statement -----------------------------------------------------
!
 100  FORMAT(' Value changed for ',A,' route: ',A,' set to ',1PE10.3)
!
      RETURN
!================ END OF MODULE LINOUT =====================================
      END
!
!   MEPAS HAZ2: ATINPR.FOR             Version Date: 07-Oct-1998
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE ATINPR                                *
!                                                                            *
!  Subroutine ATINPR This module reads records to define the usage           *
!             practices at each usage location for airborne regional         *
!             exposure pathways (individual and population.                  *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    18-Dec-95 (from ATINP2)                                 *
!  Last Modified:    07-Oct-1998  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINE USEDAT
!     Calls: NONE
!     Common blocks referenced: AIRDAT, ATPATH, AGCLAS, DEVICE, LOCNAM, PSET1, PROD
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter      Set/
!     Name           Used  Type  Location  Parameter Description
!     -------------- ----  ----  --------  ------------------------------
!     variable        S    CHAR  Argument  nnnnnnnnnnnnnnnnnnnnnn
!                     U    INT   Internal
!                     S/U  REAL  Common
!                          DBLE  External
!
!==== Modification History ===================================================
!
!   Date      Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  01-Apr-86  DLS  Moved format statements to end of subroutine
!  10-Jun-87  DLS  Additional parameter in call to SETPOP
!  05-Mar-90  JWB  Modified output for HPI file
!  28-JUL-92  DLS  Modification of heading information
!  27-Aug-92  DLS  Add input of body weight, exposure duration, and
!                  averaging time
!  12-OCT-92  DLS  Deleted averaging time from input, set to exp.dura.
!  01-Dec-92  DLS  Added user input of parameters for inhalation and
!                  soil ingestion paths (ATBWI,ATEDI,ATATI,ATUSI,EXQI
!                  and DXQI
!  01-Dec-92  DLS  Added user definition of distance and direction to
!                  location of MI for inhalation and soil pathways
!  05-Feb-93  DLS  Initialization of EXQI and DXQI arrays added
!  21-Oct-93  DLS  Add write to *.INT file
!  30-Dec-93  DLS  Added generation of SIFs
!  11-Jan-94  DLS  Finished SIF modifications
!  10-Feb-94  DLS  Completed changes for SIF update (units)
!  18-Mar-94  DLS  Set BW and ED when SIFs are input
!  31-May-94  DLS  Corrected exposure duration units to y, in output
!  27-Jun-94  DLS  Use KNSR to print emission heading for air-as-source
!  01-Dec-94  DLS  Fixed call AIRPTH call list syntax error
!  04-Dec-95  DLS  Expanded intake parameters for receptor location
!  04-Dec-95  DLS  Corrected units on air inhalation SIF for printing
!  13-Dec-95  DLS  Removed reading of air release rate file
!  13-Dec-95  DLS  Added XOQDOQ.FTN for normalized population parameters
!  13-Dec-95  DLS  Removed NU from argument list
!  11-Feb-97  DLS  Added name "Regional air" to poplution output line
!                  in .INA file
!  07-Oct-98  DLS  Eliminated call to get KAIN from GID, not there, set to 1
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE ATINPR(SETDATA,NLINES,IERR) 
!
!==== COMMON Block Definitions ===============================================
!
      include 'parmtr.par'
      include 'AIRDAT.FTN'
      include 'ATPATH.FTN'
      include 'AGCLAS.FTN'
      include 'DERMDAT.FTN'
      include 'DEVICE.FTN'
      include 'LOCNAM.FTN'
      include 'PFLAGS.FTN'
      include 'PSET1.FTN'
      include 'PROD.FTN'
      include 'PTHUSE.FTN'
      include 'XOQDOQ.FTN'
!
!==== DIMENSION Statements ===================================================
!
       CHARACTER*2 ATMNAM(6)
       CHARACTER*5 ATTYPE(3)
       CHARACTER*11 ATMUNT(3,6)
       INTEGER KAMPTH(6)
       CHARACTER*(*) SETDATA(LINEMAX)
       REAL(KIND=4) :: BWONE                                      ! 26-Apr-01
       REAL(KIND=4), DIMENSION(2,2) :: SIFXXX
!
!==== Variable Declarations ==================================================
!
!     NONE
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
      DATA ATMNAM/'DS','DD','AI','DI','DE','AE'/
      DATA KAMPTH/14,15,18,19,23,24/
      DATA ATTYPE/'Non-c','Carc.','Rad. '/
      DATA ATMUNT/'    m2/kg/d','    m2/kg/d','      m2   ', & !ds
                  ' m2 ev/kg/d',' m2 ev/kg/d','    m2 ev  ', & !dd
                  '    m3/kg/d','    m3/kg/d','      m3   ', & !ai
                  '    m2/kg/d','    m2/kg/d','      m2   ', & !di
                  '           ','           ','      h    ', & !de
                  '           ','           ','      h    '/  !ae
      DATA C70/70./,C10EM6/1.E-6/,C365/365.25/,C10EM3/0.001/
      DATA BWONE/1.0/                                           ! 26-Apr-01
!
!----- Start calculations.  Initialize error index. -----------------------
!
      IERR=0
!
      IIN = 1
      DO IP = 1,6
         IC = KAMPTH(IP)
!           VAL = GETREAL(SETDATA,NLINES,'POPAIR        ',IC,IZ,IZ,IZ,IZ,IZ)
!           IF(VAL.GT.0.) POPAIR(IP+4) = VAL
           VAL = GETREAL(SETDATA,NLINES,'FREQ          ',IC,IZ,IZ,IZ,IZ,IZ)
           IF(VAL.GT.0.) FREQ(IC) = VAL
      END DO
!
!----- Read body weight and exposure duration for this run -------------------
!
          VAL = GETREAL(SETDATA,NLINES,'BODYWT        ',IZ,IZ,IZ,IZ,IZ,IZ)
          IF(VAL.GT.0.) BW = VAL
          VAL = GETREAL(SETDATA,NLINES,'EXPDUR        ',IZ,IZ,IZ,IZ,IZ,IZ)
          IF(VAL.GT.0.) ED = VAL
!
      KAIN = IIN
!
             IF(BW.LE.0.) BW = C70
             ATBW = BW
             IF(ED.LE.0.) ED = C70
             ATED = ED
             ATUSE = USOIL
!
!  Set SIFs for individual analyses
!  Soil ingestion SIFs (pathway 14)
!
               TERM = FREQ(14)*ATUSE*C10EM3                        ! 4-Dec-95
!
               CALL SETSIF(TERM,BW,ED,ATRSIF(1,1),ATRSIF(2,1),ATRSIF(3,1) )
!
!  Soil dermal SIFs (pathway 15)
!
               TERM = FREQ(15)*C10EM6*ADHFAC*ASKIN(3)*EVSOIL      ! 4-Dec-95
               CALL SETSIF(TERM,BW,ED,ATRSIF(1,2),ATRSIF(2,2),ATRSIF(3,2) )
!
!  Air inhalation SIFs (pathway 18)                             ! 26-Apr-01
!
               TERM = FREQ(18)*UBR         ! air inhalation     ! 4-Dec-95
               CALL SETSIF(TERM,BW,ED,ATRSIF(1,3),ATRSIF(2,3),ATRSIF(3,3) )
               IF(INHALE.EQ.0) THEN                                               ! 26-Apr-01
                  TERM = FREQ(18)             ! air inhalation                    ! 26-Apr-01
                  CALL SETSIF(TERM,BWONE,ED,ATRSIF(1,3),ATRSIF(2,3),SIFXXX(1,1) ) ! 26-Apr-01
               ENDIF                                                              ! 26-Apr-01
!
!  Soil resuspension inhalation (pathway 19)
!
               TERM = FREQ(19)*UBRES      ! soil res. inhalation ! 26-May-98
               CALL SETSIF(TERM,BW,ED,ATRSIF(1,4),ATRSIF(2,4),ATRSIF(3,4) )
               IF(INHALE.EQ.0) THEN                                               ! 26-Apr-01
                  TERM = FREQ(19)         ! soil res. inhalation                  ! 26-Apr-01
                  CALL SETSIF(TERM,BWONE,ED,ATRSIF(1,4),ATRSIF(2,4),SIFXXX(1,1) ) ! 26-Apr-01
               ENDIF                                                              ! 26-Apr-01
!
!  Soil External dose (pathway 23)
!
               IF(UEXTAT.LE.0.) UEXTAT = UEXT                      ! 4-Dec-95
               TERM = FREQ(23)*UEXTAT*(SHO*FTO + SHI*FTI)          ! 4-Dec-95
               ATRSIF(3,5) = TERM * ED * C365 
!
!  Air External exposure (pathway 24)
!
               IF(UEXTAT.LE.0.) UEXTAT = UEXT                      ! 4-Dec-95
               TERM = FREQ(24)*UEXTAT        ! air external        ! 4-Dec-95
               ATRSIF(3,6) = TERM * ED * C365
!
         KAIN=IIN
!
!----- Write current usage location data to output report ------------------
!
!----- determine the number of tables to be printed (dependent on NLOC)
!
!
!----- Write table of parameter values for user input of maximum individual
!      and population inhalation and soil exposure pathways (MIXQDQ>0)
!
      WRITE(NRLS,2000) char(12)
 2000 FORMAT(1a,18x,'Summary of Atmospheric Usage Location Data'/)
         WRITE(NRLS,700) 
 700   FORMAT(/'Summary of user defined parameters for inhalation and', &
              ' soil ingestion pathways'/                               &
              ' Parameter    units     Value    '/                      &
              '------------ -------- ---------')
!
!----- Write SIFs for regional population and individual
!
        DO IEX = 1,6
            IF(IEX.LE.4) write(NRLS,2301) ATMNAM(IEX),ATTYPE(1),    &
                         ATMUNT(1,IEX), ATRSIF(1,IEX)
            IF(IEX.LE.4) write(NRLS,2301) ATMNAM(IEX),ATTYPE(2),    &
                         ATMUNT(2,IEX), ATRSIF(2,IEX)
            write(NRLS,2301) ATMNAM(IEX),ATTYPE(3),ATMUNT(3,IEX),   &
                             ATRSIF(3,IEX)
 2301       format(A2,':',A5,1X,A11,1p,5(1x,E10.3))
        END DO
!
         WRITE(NRLS,701) ATBW
 701   FORMAT('Body Weight   kg    ',F10.1)
         WRITE(NRLS,702) ATED
 702   FORMAT('Exp. Time     yr    ',F10.1)
         WRITE(NRLS,703) USLAT
 703   FORMAT('Soil Ing.   g/day   ',F10.3)
      RETURN
!
!----- Format Statements ----------------------------------------------------
!
 200  FORMAT(6E10.3)
  300 FORMAT(I5,E10.0,I5,6E10.1)
  500 FORMAT(E10.1,I5,5E10.1)
  600 FORMAT(7E10.1/7E10.1)
 3001 FORMAT('END-OF-FILE ENCOUNTERED ON UNIT ',I3,' WHILE READING',  &
         /' ATMOSPHERIC TRANSPORT USAGE LOCATION DATA')
!
!============== END OF MODULE ATINPR ========================================
      END 
!   INTAKE:MARKIN.FOR                      Version Date: 18-Jun-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                     SUBROUTINE MARKIN                                      *
!                                                                            *
!  Program MARKIN controls finding of the correct marker in the EPF file.    *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    20-Dec-96                                               *
!  Last Modified:    18-Jun-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENII/INTAKE
!     Called by: INTAKE
!     Calls: NONE
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
!   Parameter Set/
!   Name      Used  Type    Location  Parameter Description
!  --------- ----- -------  --------- -------------------------------------
!   NAME       U   Char*32  Argument  Name of marker being sought
!   FOUNDM     S   Logical  Argument  Flag set true if marker found
!==== Modification History ===================================================
!     Date    Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  15-May-97  DLS  Initial programming started for EXPOS
!  18-Jun-97  DLE  Initial programming for INTAKE
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE MARKIN(NAME,FOUNDM)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      INCLUDE 'DEVICE.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER*1 C
      CHARACTER*32 MARK,NAME
      LOGICAL FOUNDM, SEQI
!
!---- Data Statements --------------------------------------------------------
!
      DATA N32/32/
!
!---- Set input unit for reading ---------------------------------------------
!
      FOUNDM = .FALSE.
!
!---- Read a marker line ----------------------------------------------------
!
 10   READ(NEPF,*,ERR=999,END=999) MARK,NLTEXT    ! Read a marker and number of lines
       IF(.NOT.SEQI(NAME,MARK,N32)) THEN
        DO IL = 1,NLTEXT
          READ(NEPF,*) C
        END DO
        C = C
        GO TO 10
      END IF
      FOUNDM = .TRUE.
!
      RETURN
!
!---- Error in file read.  Marker name not found. ---------------------------
!
 999  WRITE(NERR,100) NAME,MARK
 100  FORMAT(' Error in reading marker information '/' Sought: ',a20,'  Last found: ',a20)
      RETURN
!
!----- END OF MODULE MARKIN -------------------------------------------------
!
      END

!   INCALC.FOR AHAZI                Version Date: 08-Sep-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE INCALC                                *
!                                                                            *
!  Subroutine INCALC tests exposure pathway name and route, and if found,    *
!       calculates intakes from the medium concentrations                    *
!                                                                            *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    18-Jun-97  DL Strenge                                   *
!  Last Modified:    08-Sep-97  DLS                                          *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZI
!     Called by: ANAZI
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!
!
!==== Modification History ===================================================
!    Date      Who  Modification Description
!   ---------  ---  ------------------------------------------------------
!   07-Jan-97  DLS  Initial programming started
!   08-Sep-97  DLS  Corrected reference to ATGSIF crop type subscript
!   27-Nov-00  DLS  Added use of SEQI for case insensitive string checking
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE INCALC(EXNAME,EXROUTE,TPATH,TREL,mting,mtinh,NPOINTS,  &
               VALIN,VALOUT,POPOUT,ETYPOUT,EUOUT,FOUNDE)
!
!---- Include statements -----------------------------------------------------
!
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'DERMDAT.FTN'
      INCLUDE 'PSET1.FTN'
      INCLUDE 'PFLAGS.FTN'
      INCLUDE 'DECAY.FTN'
      INCLUDE 'GWPATH.FTN'
      INCLUDE 'SWPATH.FTN'
      INCLUDE 'SLPATH.FTN'
      INCLUDE 'ATPATH.FTN'
      INCLUDE 'FDPATH.FTN'
      INCLUDE 'PTHUSE.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      REAL VALIN(360), VALOUT(360), YRHR
      LOGICAL FOUNDE
      INTEGER NPOINTS,  TPATH
      INTEGER N12,N20
      CHARACTER*12 EXPRUT(26), EXROUTE
      CHARACTER*20 EXPLAB(26), EXNAME
      CHARACTER*22 ETYPOUT,ETYPE(5)
      CHARACTER*9 EUOUT,EUNITS(8)
      LOGICAL SEQI
!
!---- Data Statements --------------------------------------------------------
!
      DATA N12/12/,N20/20/
      DATA YRHR/1.141E-4/
      DATA EUNITS/'mg/kg/day',    &   ! chemicals
                  'Bq       ',    &   ! rads - ing, inh, dermal
                  'Bq/kg    ',    &   ! rads - ext: soil, sediment
                  'Bq/l     ',    &   ! rads - ext: water
                  'Bq/m^3   ',    &   ! rads - ext: air, semi-infinite ! ^ added 2/2001
                  'Sv       ',    &   ! rads - ext: air, finite
                  'N/A      ',    &   ! Chem - ext: air, soil
                  'mg/m^3   '/        ! chemical air concentration
      DATA EXPLAB/'Water               ',  &
                  'Shower              ',  &
                  'Shower              ',  &
                  'Leafy vegetables    ',  &
                  'Other vegetables    ',  &
                  'Meat                ',  &
                  'Milk                ',  &
                  'Fish                ',  &
                  'Crustacea           ',  &
                  'Swimming            ',  &
                  'Swimming            ',  &
                  'Shoreline           ',  &
                  'Shoreline           ',  &
                  'Soil                ',  &
                  'Soil                ',  &
                  'Food                ',  &
                  'Shower              ',  &
                  'Air                 ',  &
                  'Soil                ',  &
                  'Swimming            ',  &
                  'Boating             ',  &
                  'Shoreline           ',  &
                  'Soil                ',  &
                  'Air                 ',  &
                  'Indoor air          ',  &
                  'Ground              '/
      DATA EXPRUT/'ingestion   ',  &       !
                  'dermal      ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'dermal      ',  &       !
                  'dermal      ',  &       !
                  'ingestion   ',  &       !
                  'ingestion   ',  &       !
                  'dermal      ',  &       !
                  'ingestion   ',  &       !
                  'inhalation  ',  &       ! 17
                  'inhalation  ',  &       ! 18
                  'inhalation  ',  &       ! 19
                  'external    ',  &
                  'external    ',  &
                  'external    ',  &
                  'external    ',  &
                  'external    ',  &
                  'inhalation  ',  &       ! 25
                  'external    '/
!
!----- Exposure type ---------------------------------------------------------
!
      DATA ETYPE/'noncarcinogenic       ',  &
                 'carcinogenic          ',  &
                 'intake                ',  &
                 'concentration         ',  &
                 'radiation dose        '/
!
!---- Find exposure name and route in list of known pathways -----------------
!
      FOUNDE = .FALSE.
      FAC = 1.0
      SIF = 0.0
      POPOUT = 1.0
      DO IP = 1,26
         IF(SEQI(EXNAME,EXPLAB(IP),N20)) THEN
!
            IF(SEQI(EXROUTE,EXPRUT(IP),N12)) THEN
!
                FOUNDE = .TRUE.
                IPATH = IP
                GO TO 10
            ENDIF
         ENDIF
      END DO
!
!---- Set pollutant type for inhalation and ingestion ------------------------
!
 10   MT = MTYPE(1)
      IF(IPATH.EQ.26) IPATH = 23
      IF(MT.EQ.1) EUOUT = EUNITS(2)
      IF(MT.GT.1) THEN
          EUOUT = EUNITS(1)
          IF(INHALE.EQ.0) THEN
             IF(IPATH.EQ.17.OR.IPATH.EQ.18.OR.IPATH.EQ.19.OR.IPATH.EQ.25) THEN
                EUOUT = EUNITS(8)
             ENDIF
          ENDIF
      ENDIF
!
!---- If pathway found, set pathway cross index for SIF factors --------------
!
      IF(FOUNDE) THEN
         IF(TPATH.EQ.1) THEN   !  Set groundwater index
            IF(IPATH.LE.7) THEN
               SIF = GWSIF(MTING,IPATH)
               IF(IPATH.EQ.2) THEN
                  FAC = PERM(1)
                  IF(KEXPTH(2).LT.2) THEN
                     ITYPE = 1
                     CALL DERMAL(ITYPE,TEFFCT)  ! Intake per shower
                     FAC = FAC * TEFFCT / TESHWR
                  ENDIF
               ENDIF
!               POPOUT = POPGW(IPATH)
               ETYPOUT = ETYPE(MTING)
            ELSE IF(IPATH.EQ.17) THEN
               SIF = GWSIF(MTINH,8)
               ETYPOUT = ETYPE(MTINH)
            ELSE IF(IPATH.EQ.25) THEN
               SIF = GWSIF(MTINH,9)
!               POPOUT = POPGW(9)
                  ETYPOUT = ETYPE(MTINH)
            ELSE
              FOUNDE = .FALSE.
            ENDIF
         ELSE IF(TPATH.EQ.2) THEN   ! Set surface water index
           IF(IPATH.LE.13) THEN
               SIF = SWSIF(MTING,IPATH)
               IF(IPATH.EQ.2.OR.IPATH.EQ.11) THEN
                  FAC = PERM(1)
                  IF(IPATH.EQ.2.AND.KEXPTH(2).EQ.1) THEN
                     ITYPE = 1
                     CALL DERMAL(ITYPE,TEFFCT)  ! Intake per shower
                     FAC = FAC * TEFFCT / TESHWR
                  ENDIF
                  IF(IPATH.EQ.11.AND.KEXPTH(11).EQ.1) THEN
                     ITYPE = 2    ! Swimming dermal absorption index
                     CALL DERMAL(ITYPE,TEFFCT)  ! Intake per swim
                     FAC = FAC * TEFFCT / TESWIM
                  ENDIF
               ENDIF
               ETYPOUT = ETYPE(MTING)
               IF(IPATH.EQ.12) THEN             ! shoreline dermal
                  FAC = ABSKN(1)
               ENDIF
           ELSE IF(IPATH.EQ.17) THEN            ! shower inhalation
               SIF = SWSIF(MTINH,14)
               ETYPOUT = ETYPE(MTINH)
           ELSE IF(IPATH.EQ.25) THEN            ! indoor inhalation
               SIF = SWSIF(MTINH,18)
               ETYPOUT = ETYPE(MTINH)
           ELSE IF(IPATH.EQ.20) THEN            ! external swimming
               SIF = SWSIF(MTING,15) * YRHR
               IF(TREL.GT.0.) THEN
                  SIF = SIF / TREL
               ELSE
                  WRITE(NRLS,1000) 
 1000             FORMAT(' Error: release time in EPF zero')
               ENDIF
!               POPOUT = POPSW(15)
               ETYPOUT = ETYPE(4)
               EUOUT = EUNITS(4)
               IF(MT.GT.1) EUOUT = EUNITS(7)
           ELSE IF(IPATH.EQ.21) THEN            ! external boating
               SIF = SWSIF(MTING,16) * YRHR
               IF(TREL.GT.0.) THEN
                  SIF = SIF / TREL
               ELSE
                  WRITE(NRLS,1000) 
               ENDIF
!               POPOUT = POPSW(16)
               ETYPOUT = ETYPE(4)
               EUOUT = EUNITS(4)
               IF(MT.GT.1) EUOUT = EUNITS(7)
           ELSE IF(IPATH.EQ.22) THEN            ! external shoreline
               SIF = SWSIF(MTING,17) * YRHR
               IF(TREL.GT.0.) THEN
                  SIF = SIF / TREL
               ELSE
                  WRITE(NRLS,1000) 
               ENDIF
!               POPOUT = POPSW(17)
               ETYPOUT = ETYPE(4)
               EUOUT = EUNITS(3)
               IF(MT.GT.1) EUOUT = EUNITS(7)
           ELSE
              FOUNDE = .FALSE.
           ENDIF
         ELSE IF(TPATH.EQ.3) THEN   ! Set atmospheric index
           IF(IPATH.GT.3.AND.IPATH.LT.8) THEN
               SIF = ATGSIF(MTING,IPATH-3)
!               POPOUT = POPAIR(IPATH-3)
               ETYPOUT = ETYPE(MTING)
           ELSE IF(IPATH.EQ.14) THEN   !  soil ingestion
               SIF = ATRSIF(MTING,1)
!               POPOUT = POPAIR(5)
               ETYPOUT = ETYPE(MTING)
           ELSE IF(IPATH.EQ.15) THEN   !  soil dermal
               SIF = ATRSIF(MTING,2)
               FAC = ABSKN(1)
               ETYPOUT = ETYPE(MTING)
           ELSE IF(IPATH.EQ.18) THEN   ! air inhalation
               SIF = ATRSIF(MTINH,3)
               ETYPOUT = ETYPE(MTINH)
           ELSE IF(IPATH.EQ.19) THEN   ! resuspension inhalation
               SIF = ATRSIF(MTINH,4)
               ETYPOUT = ETYPE(MTINH)
           ELSE IF(IPATH.EQ.23) THEN   ! external soil
               SIF = ATRSIF(MTING,5) * YRHR
               IF(TREL.GT.0.) THEN
                  SIF = SIF / TREL
               ELSE
                  WRITE(NRLS,1000) 
               ENDIF
!               POPOUT = POPAIR(9)
               ETYPOUT = ETYPE(4)
               EUOUT = EUNITS(3)
               IF(MT.GT.1) EUOUT = EUNITS(7)
           ELSE IF(IPATH.EQ.24) THEN   ! external air
               SIF = ATRSIF(MTING,6) * YRHR
               IF(TREL.GT.0.) THEN
                  SIF = SIF / TREL
               ELSE
                  WRITE(NRLS,1000) 
               ENDIF
               ETYPOUT = ETYPE(4)
               EUOUT = EUNITS(5)
               IF(FINITE) EUOUT = EUNITS(6)
               IF(MT.GT.1) EUOUT = EUNITS(7)
           ELSE
             FOUNDE = .FALSE.
           ENDIF
         ELSE IF(TPATH.EQ.5) THEN   ! Set measured soil index
           IF(IPATH.GT.3.AND.IPATH.LT.8) THEN
              SIF = SLSIF(MTING,IPATH+1)
              ETYPOUT = ETYPE(MTING)
           ELSE IF(IPATH.EQ.14) THEN       ! Soil ingestion 
              SIF = SLSIF(MTING,1)
              ETYPOUT = ETYPE(MTING)
           ELSE IF(IPATH.EQ.15) THEN       ! Soil dermal
              SIF = SLSIF(MTING,2)
              FAC = ABSKN(1)
              ETYPOUT = ETYPE(MTING)
           ELSE IF(IPATH.EQ.19) THEN       ! Soil resuspension inhalation
              SIF = SLSIF(MTINH,3)
              ETYPOUT = ETYPE(MTINH)
           ELSE IF(IPATH.EQ.23) THEN       ! Soil external
              SIF = SLSIF(MTING,4) * YRHR
               IF(TREL.GT.0.) THEN
                  SIF = SIF / TREL
               ELSE
                  WRITE(NRLS,1000) 
               ENDIF
              ETYPOUT = ETYPE(4)
              EUOUT = EUNITS(3)
              IF(MT.GT.1) EUOUT = EUNITS(7)
           ELSE
              FOUNDE = .FALSE.
           ENDIF
         ENDIF
      ENDIF
      IF(.NOT.FOUNDE) THEN
        DO IPN = 1,NPOINTS
          VALOUT(IPN) = 0.0
        END DO
        GO TO 999
      ENDIF
!
!---- Calculate intake for current constituent type and pathway --------------
!
      DO IPN = 1,NPOINTS
         VALOUT(IPN) = VALIN(IPN) * SIF * FAC
      END DO
!
 999  RETURN
!
!----- END OF MODULE EPFDAT -------------------------------------------------
!
      END

!    GETSET.FOR                      Version Date: 28-Oct-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETSET                                *
!                                                                            *
!  Subroutine GETSET reads a set of lines from the GID file                  *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    28-Oct-96                                               *
!  Last Modified:    28-Oct-96      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL USE
!     Called by: ...
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     NGID       U      INT    Argument  Logical unit of the .GID file
!     NERR       U      INT    Argument  Logical unit for writing error messages
!     ***THE FILES MUST ALREADY BE OPEN***
!     NLINES     U      INT    Argument  Number of lines to read
!     SETDATA    S      CHR    GID file  Storage array for GID data set
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETSET(NGID,NERR,NAME,NLINES,SETDATA,NSITE,FILTER)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      LOGICAL MOVESET, FILTER
      INTEGER NGID,NLINES,NERR,NGOOD
      CHARACTER*120 SETDATA(linemax),SETLINE
      CHARACTER*14 NAME,PNAM
!
!---- Data Statements --------------------------------------------------------
!
!
!---- Start of Analysis
!    MOVESET finds the location of the first record on the set, and the
!    number of lines to be read (NLINES)
!
      IF (MOVESET(NGID,NAME,NLINES)) THEN
!
!----- Set name was found in the .GID file.  Read a data set ---------------
!
        IF(NLINES.LE.0.OR.NLINES.GT.linemax) THEN   ! Test NLINES for range
          WRITE(NERR,100) NLINES
 100      FORMAT(' Error in GID specification of NLINES: ',I5)
        ELSE                            ! Read data set
          NGOOD = 0
          DO ILN = 1,NLINES
            IF(FILTER) THEN
              READ(NGID,'(A120)',ERR=300,END=400) SETLINE
              READ(SETLINE,*) PNAM,ISITE
                IF(ISITE.EQ.NSITE) THEN
                   NGOOD = NGOOD + 1
                   SETDATA(NGOOD) = SETLINE
                ENDIF
            ELSE 
              READ(NGID,'(A120)',ERR=300,END=400) SETDATA(ILN)
            ENDIF
          END DO
          IF(FILTER) NLINES = NGOOD
        ENDIF
      ELSE
        WRITE(NERR,105) NAME
 105    FORMAT(' Error in GID read.  Set name not found. ',A14)
      ENDIF
      PNAM = PNAM
      RETURN
 300  WRITE(NERR,101)ILN
 101  FORMAT(' Error in GID set read at line ILN: ',I5)
      RETURN
 400  WRITE(NERR,102) ILN, NLINES
 102  FORMAT(' Error in GID set read. EOF after reading ',I4,' of',I4,' lines')
      RETURN
!----- END OF MODULE GETSET -------------------------------------------------
      END

!   GETREAL.FOR                        Version Date: 28-Oct-96
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETREAL                               *
!                                                                            *
!  Subroutine GETREAL   extracts a real value from the SETDATA array         *
!   from the .GID file                                                       *
!                                                                            *
!  Written by:       Karl Castleton                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-93                                               *
!  Last Modified:    28-Oct-96 DLS for Framework Application                 *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL USE
!     Called by: CALLING PROGRAM
!     Calls: SUBROUTINE MOVETO
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    SETDATA     U      CHAR   Argument  Input data array from .GID file
!    ***THE DATA SET MUST ALREADY BE READ FROM GID FILE***
!    NLINES      U      INT    Argument  Number of lines used in SETDATA
!    NAME        U      CHAR   Argument  Name of parameter in .GID file
!    C1          U      INT    Argument  CONSTITUENT COUNTER
!    C2          U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
!    C3          U      INT    Argument  LOCATION COUNTER
!    C4          U      INT    Argument  FLUX COUNTER
!    C5          U      INT    Argument  MONTH COUNTER
!    C6          U      INT    Argument  MISCELANEOUS COUNTER
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     04-Dec-95    DLS  Added heading information
!     28-Oct-96    DLS  Modified to read from SETDATA array instead of GID
!==== SUBROUTINE CALL ========================================================
!
      REAL FUNCTION GETREAL(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES,LINE
      CHARACTER*(*) NAME
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER UNITS*8,USUNTS*8,TNAME*14
      REAL VALUE
!
!---- Start of analysis
!     Logical function MOVETO finds the location of the record in the
!     parameter file.  If found, MOVETO = .TRUE., otherwise = .FALSE.
!
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,VALUE
        GETREAL=VALUE
      ELSE
        GETREAL=0.0
      END IF
      END

 
!   MOVESET.FOR                          Version Date: 14-May-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                 LOGICAL FUNCTION MOVESET                                   *
!                                                                            *
!  Subroutine MOVESET finds the location of the sought set of parameters in  *
!  the .GID file.                                                            *
!                                                                            *
!  Written by:       Karl Castleton (as function MOVETO)                     *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-93                                               *
!  Last Modified:    14-May-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL USAGE
!     Called by: SUBROUTINE GETSET
!     Calls: SEQI
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    NGID        U      INT    Argument  Logical unit of the .GID file
!    NAME        U      CHAR   Argument  Name of SET sought
!    NLINES      U      INT    Argument  Number of lines in set sought
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     06-Dec-95    DLS  Added heading information
!     28-Oct-96    DLS  Modified from MOVETO to MOVESET for Framework Application
!     29-Oct-96    DLS  Change argument names to NGID and NLINES for consistency
!     14-May=97    DLS  Skip past a data set by reading NLINES instead of
!                       searching each line for the marker name.
!==== SUBROUTINE CALL ========================================================
!
      LOGICAL FUNCTION MOVESET(NGID,NAME,NLINES)
!
!---- Variable type declarations ---------------------------------------------
!
      INTEGER NGID,NLINES
      CHARACTER*(*) NAME
      LOGICAL SEQI
      CHARACTER TNAME*32
      DATA N14/14/
!
!---- Start of analysis ------------------------------------------------------
!
      TNAME=' '
!
!---- Rewind .GID file to top ------------------------------------------------
!
      REWIND(NGID)
      MOVESET=.FALSE.
!
!---- Read a line from the .GID file (name, index values, and data -----------
!
 300  READ (NGID,*,ERR=303,END=303) TNAME,NLINES
!
!---- Test name and index values against values sought -----------------------
!
      IF (.NOT. SEQI(TNAME,NAME,N14))  THEN
        DO IL = 1,NLINES                        ! New do loop to read
          READ(NGID,*,ERR=303,END=303) TNAME    ! NLINES skipping past
        END DO                                  ! unneeded data set
        GO TO 300
      ELSE !
        MOVESET=.TRUE.
      END IF
 303  END
!--------- END OF MODULE MOVESET ---------------------------------------------
!   MOVETO.FOR                              Version Date: 28-Oct-96
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE MOVETO                                *
!                                                                            *
!  Subroutine MOVETO finds the location of the sought parameter in the .GID  *
!                 data set.                                                  *
! THIS ROUTINE MOVES TO THE ARRAY POSITION YOU ARE LOOKING                   *
! A FUNCTION VALUE OF .TRUE. IF THE RECORD IS .FOUND. FALSE IF               *
! IT IS NOT                                                                  *
!                                                                            *
!  Written by:       Karl Castleton                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Nov-93                                               *
!  Last Modified:    28-Oct-96      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: GENERAL
!     Called by: SUBROUTINE GETINT, GETSTR, GETREALNC
!     Calls: SEQ
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!    SETDATA     U      CHAR   Argument  Data set array to search
!    NLINES      U      INT    Argument  Number of lines in data set used
!    NAME        U      CHAR   Argument  Name of the parameter sought
!    C1          U      INT    Argument  CONSTITUENT COUNTER
!    C2          U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
!    C3          U      INT    Argument  LOCATION COUNTER
!    C4          U      INT    Argument  FLUX COUNTER
!    C5          U      INT    Argument  MONTH COUNTER
!    C6          U      INT    Argument  MISCELANEOUS COUNTER
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!     06-Dec-95    DLS  Added heading information
!     28-Oct-96    DLS  Modified to read from SETDATA instead of file
!  14-Aug-97  DLS  Changed SEQ to SEQI to do case-insensitive comparison
!==== SUBROUTINE CALL ========================================================
!
         LOGICAL FUNCTION MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,line)
!
      INCLUDE 'DEVICE.FTN'
!
!---- Variable type declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6
      CHARACTER*(*) NAME
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL SEQI
      INTEGER T1,T2,T3,T4,T5,T6
      CHARACTER TNAME*14
      DATA N14/14/
!
!---- Start of analysis ------------------------------------------------------
!
      LINE = 0
      TNAME=' '
      T1=0
      T2=0
      T3=0
      T4=0
      T5=0
      T6=0
!
!---- Initialize output ------------------------------------------------------
!
      ILN = 0
      MOVETO=.FALSE.
!
!---- Extract a line from the SETDATA array (name, index values, and data ----
!
 300  ILN = ILN + 1
      IF(ILN.GT.NLINES) GO TO 303
!     WRITE(NERR(ILN),*) SETDATA(ILN)
      READ (SETDATA(ILN),*,ERR=303,END=303) TNAME,T1,T2,T3,T4,T5,T6
!
!---- Test name and index values against values sought -----------------------
!
      IF ((.NOT. SEQI(TNAME,NAME,N14) ) .OR. C1 .NE.T1 .OR. C2 .NE. T2  &
        .OR. C3 .NE. T3 .OR. C4 .NE. T4 .OR. C5 .NE. T5                 &
        .OR. C6 .NE. T6) THEN
        GO TO 300
      ELSE !
        LINE = ILN
        MOVETO=.TRUE.
      END IF
 303  END
!================= END OF MODULE MOVETO ===================================
!
!   MEPAS AHAZ: Chem_Data_Read.F95             Version Date:  03-Nov-2004
!
!   Copyright 1989-2004 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE Chem_Data_Read                        *
!                                                                            *
!  Subroutine Chem_Data_Read reads the exposure pathway parameters from the  *
!                    CONi section of the GID file for one parent constituent *
!                    plus associated progeny                                 *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    08 Dec 04 (From AHAZX CHEM_DATA_READ                    *
!  Last Modified:                                                            *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZXALL
!     Called by: SUBROUTINE AHAZX, main program
!     Calls: SUBROUTINES BFCAL, BICAL, BVCAL, FEMCAL, FMICAL
!     Common blocks referenced: PSET1, PSET3, DEVICE, TITLS, KLASS
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!
!     ARGUMENT LIST:
!
!    SETDATA CHAR    Images of GID file section CONi (i = icon for chemical database)
!    NLINES  INT     Number of lines in SETDAT to consider
!    IPDATF  INT     Print flag for constituent data (>0 if not previously printed)
!    IERR    INT     Error message flag
!
!==== Modification History ===================================================
!
!   Date      Who  Modification Description
!  --------   ---  ------------------------------------------------------
!  08-Dec-04  DLS  Created from Chem_Data_Read for AHAZX
!
!==== SUBROUTINE CALL ========================================================
!
    SUBROUTINE CHEM_DATA_READ(SETDATA,NLINES,CONID,IPDAT,TPATH,IERR)
!
!==== COMMON Block Definitions ===============================================
!
      include 'PSET1.FTN'
      include 'PSET2.FTN'
      include 'PSET3.FTN'
      include 'DERMDAT.FTN'
      include 'DEVICE.FTN'
      include 'TITLS.FTN'
      include 'KLASS.FTN'
      include 'PFLAGS.FTN'
      include 'RISKCM.FTN'
      include 'COUPLE.FTN'
      include 'DECAY.FTN'
!
!     Define all paramters used in this module
!
!      IMPLICIT NONE
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION lll(20)
      DIMENSION CAS(3), CASID(3,20), ffname(17,20)
!      DIMENSION CAS(3), PNAME(5), CASID(3,20), FNAME(17),ffname(17,20)
!
!==== Variable Declarations ==================================================
!
      CHARACTER*4 CAS, CASID, ffname
      CHARACTER*4 RN1, RN2
      CHARACTER*5 RADON
      CHARACTER*12 CONID
      CHARACTER*1 B
!
      CHARACTER(LEN=*), DIMENSION(*) :: SETDATA
      INTEGER :: NLINES         ! Number of lines of GID file data included in array SETDATA
      INTEGER :: IPDAT          ! Print flag, > 0 indicates parameters for this constituet are
                                ! to be printed to the ELS file.
      INTEGER :: TPATH          ! Medium flag: 1 - GW, 2 - SW, 3 - Air, 5 - soil
      INTEGER :: IERR           ! Error flag, > 0 indicates error condition on RETURN
!
!==== DATA Statements ========================================================
!
      DATA RN1/'RN22'/,RN2/'2   '/
      DATA RADON/'RN222'/
      DATA B/' '/
!
!==== Variable Declarations ==================================================
!
      CHARACTER(LEN=80) :: PNAME
      CHARACTER(LEN=80) :: fname
!
!     GID variable names
!
      CHARACTER(LEN=12) :: FSCASID         ! Name of current constituent
      CHARACTER(LEN=12), DIMENSION(20,2) :: FSID ! Names of each chain member (parent in position 1)
!      CHARACTER*12 CONID
      !
!     Function type definitions
!
      LOGICAL :: SEQI                ! function for comparing two strings case-insensitive
      CHARACTER*80 GETSTR
!      CHARACTER(LEN=80) :: GETSTR            ! Function for extrating a character parameter value from GID data
      REAL(KIND=4) :: GETREAL        ! Function for extrating a real parameter value from GID data
      INTEGER GETINT                 ! Function for extrating an integer parameter value from GID data
      !
!==== DATA Statements ========================================================
!
!----- Start of Calculations --------------------------------------------
!----- Initialize index values
!
      NF = 0
!
!----- Get number of constituents included in the analysis
!
!       WRITE(NRLS,'(a,2i5)') ' In Chem_Data_Read, number of data lines is: ',NLINES
!       WRITE(NRLS,'(a,a,2i5)') ' Line 1 is: ',SETDATA(1)
       NUMCON = GETINT(SETDATA,NLINES,'NUMCON        ',SITNUM,IZ,IZ,IZ,IZ,IZ)
!
!   If number of constituents in the chemical database is < 1, error
!
      IF(NUMCON.LT.1) GO TO 997
!
!      WRITE(NRLS,'(A,A)') ' Searching for parent in GID: ',CONID
!
!   Loop on number of constituents in GID file, searching for current constituent (POLID(i),i=1,3)
!
      DO IP = 1,NUMCON
!
         FSCASID = GETSTR(SETDATA,NLINES,'FSCASID       ',SITNUM,IP,IZ,IZ,IZ,IZ)
!         WRITE(NRLS,'(a,a,2i5)') ' Searching for parent of chain, FSCASID, NUMCON: ',FScasID, numcON
!
         IF(.NOT.SEQI(FSCASID,CONID,12)) GO TO 10
           POLID(1,1) = FSCASID(1:4)
           POLID(2,1) = FSCASID(5:8)
           POLID(3,1) = FSCASID(9:12)
!
!         IF(.NOT.SEQI(FSCASID(1:4),POLID(1,1),4)) GO TO 10
!         IF(.NOT.SEQI(FSCASID(5:8),POLID(2,1),4)) GO TO 10
!         IF(.NOT.SEQI(FSCASID(9:12),POLID(3,1),4)) GO TO 10
!
!----    Constituent found, transfer to read parameters for this parent
!
         FNAME = GETSTR(SETDATA,NLINES,'FSCNAME       ',SITNUM,IP,IZ,IZ,IZ,IZ)
         KTYPE = GETINT(SETDATA,NLINES,'CLKTYPE       ',SITNUM,IP,IZ,IZ,IZ,IZ)
         MTYPE(1)=1                     ! Radionuclides
         IF(KTYPE.LT.1) MTYPE(1) = 2    ! Chemicals
!
         IPARENT = IP
!         WRITE(NRLS,'(a,a,i5)') ' Found parent, constituent number: ',FSCASID, IP
         GO TO 12
!
  10     CONTINUE
!
      END DO
!
!---- At this point, the constituent was not found in the GID CONi section.
!     Write error message and return.
!
     GO TO 996        ! error message on constituent not found, RETURN
!
!---- Continue point when constituent was found
!
  12 CONTINUE
!
!---- Read parameters from CON section of GID file
!
!     Read GI absorption fraction
!
     RFONE = GETREAL(SETDATA,NLINES,'CLFONEI       ',SITNUM,IP,IZ,IZ,IZ,IZ)
!     WRITE(nrls,'(a,f10.5)') ' CLFONE  ',rfone
!
!     Read skin absorption fraction
!
     RABSKN = GETREAL(SETDATA,NLINES,'CLABSKN       ',SITNUM,IP,IZ,IZ,IZ,IZ)
!     WRITE(nrls,'(a,f10.5)') ' CLABSKN ',rabskn
!
!     Read skin permeability factor
!
     RKPERM = GETREAL(SETDATA,NLINES,'CLPERMK       ',SITNUM,IP,IZ,IZ,IZ,IZ)
!     WRITE(nrls,'(a,f10.5)') ' CLPERMK ',rkperm
!
!     Read octonal water partition coefficient
!
     RCKOW = GETREAL(SETDATA,NLINES,'CLKOW         ',SITNUM,IP,IZ,IZ,IZ,IZ)
!     WRITE(nrls,'(a,f10.5)') ' CLCKOW  ',rckow
!
!     Read molecular weight
!
     RWM   = GETREAL(SETDATA,NLINES,'CLWM          ',SITNUM,IP,IZ,IZ,IZ,IZ)
!     WRITE(nrls,'(a,f10.5)') ' CLMW    ',rMW
!
!     Read Henry's Law Constant
!
     RHLC  = GETREAL(SETDATA,NLINES,'CLHLC         ',SITNUM,IP,IZ,IZ,IZ,IZ)
!     WRITE(nrls,'(a,f10.5)') ' CLMW    ',rMW
!
!
      NUC = 1
!
!----- Set parameters for each chain member
!
       IP = 1
!
      ANAME(1,IP)=FNAME(1:4)
      ANAME(2,IP)=FNAME(5:8)
      ANAME(3,IP)=FNAME(9:12)
      ANAME(4,IP)=FNAME(13:16)
      ANAME(5,IP)=FNAME(17:20)
!      WRITE(NRLS,'(A)') ' ANAME '
!      DO 50 IL=1,5
!        WRITE(NRLS,'(3X,A)') ANAME(IL,IP)
! 50  CONTINUE
      do 55 il=1,17
        ffname(il,ip) = fname((IL-1)*4+1:(IL-1)*4+4)
 55   continue
      DO 60 IL=1,3
       CASID(IL,IP)=POLID(IL,1)
 60   CONTINUE
!
!  Set octonal water partition coefficient input
!
      CKOW(IP) = RCKOW
!
!  Set toxicity parameters according to value given for IREFX
!
!      FONE(IP) = 0.0
      ABSKN(IP) = 0.0
      FONE(IP) = RFONE
      IF(RFONE.LE.0.) FONE(IP) = 1.0
!
!   Set dermal skin absorption fraction for contact with soil.
!   Default for organics 1.0, default for inorganics 0.001.
!
      IF(RABSKN.LE.0.0) THEN
         IF(RCKOW.LE.0.) THEN   ! Test for organic (organics have Kow)
            ABSKN(IP) = 1.E-3    ! Inorganic and radionuclides
         ELSE
            ABSKN(IP) = 1.       ! Organic
         ENDIF
      ELSE
         ABSKN(IP) = RABSKN     ! Use value given in CHEMLIB database
      ENDIF
!
!------  Set skin permeability constant ----------------------------------
!
      IF(RKPERM.GT.0.) THEN
         PERM(IP) = RKPERM
!
! If skin permeability is not given, then calculate from KOW and MW, if given
!
      ELSE IF(RCKOW.GT.0..AND.RWM.GT.0.) THEN     ! Organic chemicals
         KEXP = 1                                  ! EPA model
         PERM(IP) = PERMK(RCKOW,RWM,KEXP)
!
! Default for all other pollutants is 0.001 cm/hr
!
      ELSE
         PERM(IP) = 0.001                        ! Default value
      ENDIF
      ANDF(IP) = ANDFO          ! Default for all pollutants
      IF(MTYPE(IP).GT.1) THEN   ! Chemicals
         IF(RHLC.GT.1.E-5.AND.RWM.LT.200.) ANDF(IP) = ANDFC  ! Set to default
      ELSE
!
         IF(SEQI(FSCASID,RADON,5)) THEN
         !IF(POLID(1,IP).EQ.RN1.AND.POLID(2,IP).EQ.RN2) THEN
            ANDF(IP) = ANDFR
         ENDIF
      ENDIF
!
  80  CONTINUE
!
!------ Write summary of contaminant specific factors -------------------------
!
       write(nrls,277)
  277  FORMAT(//'Num Constituent Names', &
             ' -------------',&
             ' CASID ---- Name ----------------------')
!
        write(nrls,2001) (CASID(I,IP),I=1,2), FNAME
 2001   format('ID / CAS #:      (None)         ',2x,1p,2a4,4x,a)
        write(nrls,2036) perm(ip)
 2036   format('Skin Perm. Const: cm/hr         ',1p,4(1x,E10.3,1x))
        write(nrls,2039) abskn(ip)
 2039   format('Skin Absorption Fraction:       ',1p,4(1x,E10.3,1x))  
        write(nrls,2040) RCKOW
 2040   format('Octonal-Water Partition Coeff.  ',1p,4(1x,E10.3,1x))
        write(nrls,2125) ANDF(ip)
 2125   format('Andelman Factor: (L/m^3)        ',1p,4(1x,E10.3,1x))
        write(nrls,2041) RWM
 2041   format('Molecular Weight                ',1p,4(1x,E10.3,1x))
        write(nrls,2042) RHLC
 2042   format('Henrys Law Constant             ',1p,4(1x,E10.3,1x))
!
!
! ************************************************************************
!    End of search loop for chain members
! ************************************************************************
      GO TO 2000
!
!---- Error condition finding pollutant data
!
!-----  ALL DESIRED POLLUTANTS WERE NOT FOUND IN THE DATA BASE. ---------------
!
 996  WRITE(nRls,1011) POLID(1:3,1)
      IF(IP.GT.0.AND.IP.LT.20) WRITE(NrLS,1091) POLID(1:3,IP)
 1091 FORMAT(' Constituent not found is ',3a)
      WRITE(nerr,1011) POLID(1:3,1)                            ! write to error message file
 1011 FORMAT(/' Data base error, all constituents not found in GID'/'Parent is ',3a)
      IF(IP.GT.0.AND.IP.LT.20) WRITE(Nerr,1091) POLID(1:3,IP)  ! write to error message file
      IERR = IERR + 1
      GO TO 900
  997 WRITE(nRls,1014) NUMCON
      WRITE(nerr,1014) NUMCON        ! write to error message file
 1014 FORMAT(/' DATA BASE ERROR - NO CONSTITUENTS SPECIFIED IN GID, NUMCON =',I5)
      IERR = IERR + 1
      GO TO 900
!
 2000 CONTINUE
!
 900 RETURN
!
!================= END OF MODULE CHEM_DATA_READ =============================
!
      END
!
!   MEPAS AHAZI PDATINI.FOR             Version Date:  18-Jun-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE PDATINI                               *
!                                                                            *
!  Subroutine PDATIN reads the exposure pathway data library and selects     *
!                    data for requested pollutants. Some pathway paramenters *
!                    are also calcutated.                                    *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    11 Nov 95 (From HAZ PDATIN version 11-Apr-94            *
!  Last Modified:    18 Jun 97 DLS                                           *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZI
!     Called by: SUBROUTINE HAZ, main program
!     Calls: SUBROUTINES BFCAL, BICAL, BVCAL, FEMCAL, FMICAL
!     Common blocks referenced: PSET1, PSET2, PSET3, DEVICE, TITLS, KLASS
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     variable    S    CHAR    Argument  nnnnnnnnnnnnnnnnnnnnnn
!                 U    INT     Internal
!                 S/U  REAL    Common
!                      DBLE    External
!     IREFX       U    INT     SELECTION INDEX FOR TOXICITY VALUES (0 - DEFAULT,
!                RADIONUCLIDES 0 - DEFAULT DOSE FACTORS, >=1 - SLOPE FACTORS
!                CHEMICALS     0 - DEFAULT TOX VALUES, 1 - CPF ONLY, 2 RFD ONLY
!
!     ARGUMENT LIST:
!     IPDAT       U    INT     DATA PRINT CONTROL INDEX, PRINT IF IPDAT > 0
!     IRIS        U    INT     USE ONLY TOXICITY VALUES OF INDEX <= IRIS
!     IHEAST      U    INT     USE ONLY HEAST RAD SLOPE FACTORS IF > 0
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!  11 Nov 1995     DLS  Created from PDATIN for HAZ2
!  21 Nov 1995     DLS  Fixed logic so DEX and DIMR are always set if available
!  18 Dec 1995     DLS  Added common block DECAY and set NUC = 1 for chemicals
!  18 Jun 1997     DLS  Modificied for use in MEPAS 4.0 INTAKE component
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE PDATINI(CONID,IPDAT)    
!
!==== COMMON Block Definitions ===============================================
!
      include 'PSET1.FTN'
      include 'PSET2.FTN'
      include 'PSET3.FTN'
      include 'DERMDAT.FTN'
      include 'DEVICE.FTN'
      include 'TITLS.FTN'
      include 'KLASS.FTN'
      include 'PFLAGS.FTN'
      include 'RISKCM.FTN'
      include 'COUPLE.FTN'
      include 'DECAY.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DIMENSION lll(20)
      DIMENSION CAS(3), PNAME(5), CASID(3,20), FNAME(17),ffname(17,20)
!
!==== Variable Declarations ==================================================
!
      CHARACTER*4 CAS, PNAME, CASID, FNAME, ffname
      CHARACTER*4 RN1, RN2
      CHARACTER*12 CONID
      CHARACTER*1 B
!
!==== DATA Statements ========================================================
!
      DATA RN1/'RN22'/,RN2/'2   '/
      DATA B/' '/
!
!----- Start of Calculations --------------------------------------------
!----- Initialize index values
!
      NF = 0
!
!----- Open chemical database file --------------------------------------
!
      OPEN (UNIT=NDBLU, FILE=TRIM(HINFNM)//'.CHM',STATUS='OLD')
!
! ***********************************************************************
!
!  Find parent constituent in Chemical Database.  Statement 10 is return
!  point for incrementing search in Chemical database.
!
! ***********************************************************************
!
!----- READ FIRST RECORD OF DATA BASE, NUMBER OF ENTRIES AND TITLE
!
      READ(NDBLU,1000,END=999,ERR=998) NPDB,PDTITL
!   If number of constituents in the chemical database is < 1, error
      IF(NPDB.LT.1) GO TO 997
!  LOOP OVER ENTRIES IN DATA BASE
      NP = 0
 10   NP = NP + 1
      IF(NP.GT.NPDB) GO TO 996
!  READ NEXT ENTRY IN MASTER DATA LIBRARY
      READ(NDBLU,1005,END=999,ERR=998) CAS,(FNAME(I),I=1,17),    & ! Line 1
       KTYPE,PNAME,RWM,RVAP,RHLC,RSOL,RKOC,RCKOW,RKPERM,         & ! Line 2
       RTHALF,RGHALF,RWHALF,RSHALF,RDFG,RDFA,RDEX,RDSH,RDIMR,    & ! Line 3
       RBFF,RBFI,RBFV,RBMT,RBMK,RWPF,RVDP,KRCLS,                 & ! Line 4
       RCPFH,RCPFG,RRFDH,RRFDG,IITXIH,IITXIG,IIHISI,IIPPI,IITFI, & ! Line 5
       RFONE,RABSKN,RDIFFC,                                      &
       KCPFH,KCPFG,KRFDH,KRFDG                                     ! line 6
!
!  Check for match with parent constituent.
!
      IF(CAS(1).NE.CONID(1:4)) GO TO 10
      IF(CAS(2).NE.CONID(5:8)) GO TO 10
      IF(CAS(3).NE.CONID(9:12)) GO TO 10
      MTYPE(1)=KTYPE
      RVAP= RVAP
      RHLC = RHLC
      RSOL = RSOL
      RKOC = RKOC
      WM(1) = RWM
      CKOW(1)=RCKOW
      RTHALF=RTHALF
      RGHALF=RGHALF
      RWHALF=RWHALF
      RSHALF=RSHALF
      RDFG=RDFG
      RDFA=RDFA
      RDEX=RDEX
      RDSH=RDSH
      RDIMR=RDIMR
      RBFF=RBFF
      RBFI=RBFI
      RBFV=RBFV
      RBMT=RBMT
      RBMK=RBMK
      RWPF=RWPF
      RVDP=RVDP
      KRCLS=KRCLS
      RCPFH=RCPFH
      RCPFG=RCPFG
      RRFDH=RRFDH
      RRFDG=RRFDG
      IITXIH=IITXIH
      IITXIG=IITXIG
      IIHISI=IIHISI
      IIPPI=IIPPI
      IITFI=IITFI
      RDIFFC= RDIFFC
      KCPFH = KCPFH
      KCPFG = KCPFG
      KRFDH = KRFDH
      KRFDG = KRFDG
!
!---- Primary constituent found in Chemical Database.  Check type.
!
      REWIND(NDBLU)
      NUC = 1
!
!----- Set parameters for each chain member
!
       IP = 1
!
      DO 50 IL=1,5
        ANAME(IL,IP)=PNAME(IL)
  50  CONTINUE
      do 55 il=1,17
        ffname(il,ip) = fname(il)
 55   continue
      DO 60 IL=1,3
       CASID(IL,IP)=CAS(IL)
 60   CONTINUE
!
!  Set toxicity parameters according to value given for IREFX
!
      FONE(IP) = 0.0
      ABSKN(IP) = 0.0
      IF(KTYPE.EQ.1) THEN       ! Radionuclides
!
!   Set internal dose factor for dermal absorption for radionuclides
!
        DFS(IP) = RRFDG
!
!      ELSE               ! Chemical pollutants
!
!        IF(IREFX.EQ.1) THEN
!           MTYPE(IP) = 2
!        ELSE IF(IREFX.EQ.2) THEN
!           MTYPE(IP) = 5
!        ENDIF
      ENDIF
        FONE(IP) = RFONE
        IF(RFONE.LE.0.) FONE(IP) = 1.0
!
!   Set dermal skin absorption fraction for contact with soil.
!   Default for organics 1.0, default for inorganics 0.001.
!
      IF(RABSKN.LE.0.0) THEN
       IF(RCKOW.LE.0.) THEN   ! Test for organic (organics have Kow)
         ABSKN(IP) = 1.E-3    ! Inorganic and radionuclides
       ELSE
         ABSKN(IP) = 1.       ! Organic
       ENDIF
      ELSE
       ABSKN(IP) = RABSKN     ! Use value given in CHEMLIB database
      ENDIF
!
!------  Set skin permeability constant ----------------------------------
!
      IF(RKPERM.GT.0.) THEN
        PERM(IP) = RKPERM
!
! If skin permeability is not given, then calculate from KOW and MW, if given
!
      ELSE IF(RCKOW.GT.0..AND.RWM.GT.0.) THEN     ! Organic chemicals
        KEXP = 1                                  ! EPA model  
        PERM(IP) = PERMK(RCKOW,RWM,KEXP)
!
! Default for all other pollutants is 0.001 cm/hr
!
      ELSE
        PERM(IP) = 0.001                        ! Default value
      ENDIF
      ANDF(IP) = ANDFO          ! Default for all pollutants
      IF(MTYPE(IP).GT.1) THEN   ! Chemicals
       IF(RHLC.GT.1.E-5.AND.RWM.LT.200.) ANDF(IP) = ANDFC  ! Set to default
      ELSE
       IF(POLID(1,IP).EQ.RN1.AND.POLID(2,IP).EQ.RN2) THEN
         ANDF(IP) = ANDFR
       ENDIF
      ENDIF
  80  CONTINUE
      NF=NF+1
      IF(NF.GE.NUC) GO TO 2000
!
! ************************************************************************
!    End of search loop for chain members
! ************************************************************************
      GO TO 2000
!-----  ALL DESIRED POLLUTANTS WERE NOT FOUND IN THE DATA BASE.
 996  WRITE(nERR,1011)
      GO TO 900
  997 WRITE(nERR,1014) NPDB
      go to 900
 998  WRITE(NERR,8887)
 8887 FORMAT(' Read error on chemical database file.')
      go to 900
 999  WRITE(NERR,8888)
 8888 FORMAT(' Read end of file on chemical database file.')
      go to 900
 2000 REWIND(NDBLU)
!----- Print reports of library data if IPDAT > 0.
!
      IF(IPDAT.GT.0) THEN
      npol = NUC
       write(nrls,277)
  277  FORMAT(//'Num Constituent Names', &
             ' ------------------------------------------',&
             ' CASID ')
!             ' CASID ------ IREFX ')
       do 5999 ip = 1,npol
        WRITE(nrls,1111) ip,(fFNAME(I,ip),I=1,15),(CASID(I,IP),I=1,3)
!        WRITE(nrls,1111) ip,(fFNAME(I,ip),I=1,15),(CASID(I,IP),I=1,3),IREFX
  1111   FORMAT(I2,2x,15A4,1x,3A4)
!  1111   FORMAT(I2,2x,15A4,1x,3A4,I4)
  5999 continue
!      WRITE(nrls,1112)
! 1112 FORMAT('-------------------'/                                 &
!       'IREFX for radionuclides:  0 - use database dose factors,',  &
!       ' unless HEAST requested'/                                   &
!       '                          1 - use slope factors'/           &
!       'IREFX for chemicals:      0 - use database toxicity values'/&
!       '                          1 - use CPF values only'/         &
!       '                          2 - use RfD values only')
!
!-----
!----- modify constituent database printout --- JWB - 01/17/90
!-----
!----- determine the number of tables to be printed (dependent on NPOL)
!-----
        newpol = 1
        iii = 1
        ICHART = 1
       jj = newpol - ichart
       if ( jj .gt. 0.0 ) then
         nnn = ((ichart - 1) * 4) + 1
         jjj = 4 * ichart
       else
         nnn = ((ichart -1) * 4) + 1
         jjj = iii + ((ichart -1) * 4)
       end if
!-----
!----- writing out the constituent names
!-----
      do 4999 nml = nnn, jjj
        lll(nml) = nml
 4999   continue
        kkl = jjj - nnn
       if( ichart .eq. 1) then
         WRITE(nrls,1001) char(12),PDTITL(1:76),NPDB
       else
         WRITE(nrls,1122) char(12),PDTITL(1:76),NPDB
       endif
       if( kkl .eq. 0 ) then
         write(nrls,3002) (lll(nml), nml=nnn,jjj)
         write(nrls,'(41(''-''))')
 3002     format( '  Constituent   Parameter   ',5x,           &
                    ('Constituent '),                          &
                   /,'   Parameter      Units         ',(5x,I2,5x) )
       else if( kkl .eq. 1 ) then
         write(nrls,3003) (lll(nml), nml=nnn,jjj)
         write(nrls,'(53(''-''))')
 3003     format( '  Constituent   Parameter   ',5x,           &
                   2('Constituent '),                          &
                   /,'   Parameter      Units         ',2(5x,I2,5x) )
       else if( kkl .eq. 2 ) then
         write(nrls,3004) (lll(nml), nml=nnn,jjj)
         write(nrls,'(65(''-''))')
 3004     format( '  Constituent   Parameter   ',5x,           &
                   3('Constituent '),                          &
                   /,'   Parameter      Units         ',3(5x,I2,5x) )
       else if( kkl .eq. 3 ) then
         write(nrls,3005) (lll(nml), nml=nnn,jjj)
         write(nrls,'(80(''-''))')
 3005     format( '  Constituent   Parameter   ',5x,           &
                   4('Constituent '),                          &
                   /,'   Parameter      Units         ',4(5x,I2,5x) )
       end if
!-----
!----- writing out the constituent parameter chart(s)
!-----
       write(nrls,2001) ((CASID(I,IP),I=1,2), ip=nnn,jjj)
 2001   format('ID / CAS #:      (None)         ',1p,4(2x,2a4,2x))
       write(nrls,2002) (mtype(ip),ip=nnn,jjj)
 2002   format('Toxicity Type:   (None)         ',4(1x,I10,1x))
       write(nrls,2036) (perm(ip), ip=nnn,jjj)
 2036   format('Skin Perm. Const: cm/hr         ',1p,4(1x,E10.3,1x))  
       write(nrls,2039) (abskn(ip), ip=nnn,jjj)
 2039   format('Skin Absorption Fraction:       ',1p,4(1x,E10.3,1x))  
       write(nrls,2040) (fone(ip), ip=nnn,jjj)
 2040   format('GI Tract Absorption Fraction:   ',1p,4(1x,E10.3,1x))  
       write(nrls,2125) (ANDF(ip), ip=nnn,jjj)
 2125   format('Andelman Factor: (L/m^3)        ',1p,4(1x,E10.3,1x))
!
       write(nrls,'(/,79(''*''))')
       write(nrls,278)
278    FORMAT('Toxicity Type 1: Radionuclide carcinogenic',  &
              ' inhalation and ingestion')
       write(nrls,280)
280    FORMAT('Toxicity Type 2: Chemical carcinogenic',      &
              ' inhalation and ingestion')
       write(nrls,281)
281    FORMAT('Toxicity Type 3: Chemical carcinogenic',      &
              ' inhalation, non-carcinogenic ingestion')
       write(nrls,282)
282    FORMAT('Toxicity Type 4: Chemical non-carcinogenic',  &
              ' inhalation, carcinogenic ingestion')
       write(nrls,283)
283    FORMAT('Toxicity Type 5: Chemical non-carcinogenic',  &
              ' inhalation and ingestion')
!
      ENDIF
900   CLOSE(NDBLU)
      RETURN
!
!----- Format Statements -------------------------------------------------
!
 1000 FORMAT(I4,A80)
 1001 FORMAT(a1,14X,'Summary of Data from MEPAS Constituent Database',/,4x,A76,&
      /,10x,'This Database Contains Entries for ',I4,' Constituents'/)
 1122 FORMAT(a1,11X,'Summary of Data from MEPAS Constituent Database',' (Cont)',/,4x,A76,&
      /,10x,'This Database Contains Entries for ',I4,' Constituents'/)
 1016 FORMAT(/'POLLUTANT NAME            CP INH.     CP ING. ',&
        '   RFD INH.    RFD ING.   ')
 1021 format(26x,'Kg*day/mg    Kg*day/mg   mg/Kg/day   mg/Kg/day')
 1020 format(24x,' 1/days       1/days      1/days      1/days',&
       '      rem/pCi     rem/pCi')
!                                            XXXXX
 1005 FORMAT(3A4,17A4/I2,5A4,F8.2,6E10.0//9E10.0//7E10.0,I5//&
       4E10.0,5I5,3E10.0/I3,7X,I3,7X,I3,7X,I3)
 1006 FORMAT(1X,5A4,2X,1PE10.1,2X,5(E10.1,2X))
 1017 FORMAT(1X,5A4,3A4,I3,2X,1PE10.1,2X,5(E10.1,2X))
 1007 FORMAT(1X,5A4,1PE10.1,3X,4(E10.1,2X),5E10.1)
 1008 FORMAT(1X,5A4,1PE10.1,6E10.1,2X,I3)
 1011 FORMAT(/'DATA BASE ERROR, ALL POLLUTANTS NOT FOUND')
 1012 FORMAT(/'DATA BASE ERROR - INPUT READ ERROR, IPO =',I4)
 1013 FORMAT(/'DATA BASE ERROR - END OF FILE ON UNIT',I3,' IPO =',I4)
 1014 FORMAT(/'DATA BASE ERROR - NO POLLUTANTS SPECIFIED, NPDB =',I5)
 1015 FORMAT(/,'POLLUTANT NAME       ID/CAS NO.  TYPE    MOL. WT.   VAP PRES',&
       '    HENRYS C    WAT. SOL.      KOC         KOW')
 1019 FORMAT(41X,' g/mole     mm - Hg   atm-m3/mole     mg/L        L/g', &
       '    DIMENSIONLESS')
!================= END OF MODULE PDATIN ===================================
      END
!   SEQI.FOR                         Version Date: 14-Aug-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                     LOGICAL FUNCTION SEQI                                  *
!
!  Subroutine to compare two strings in case-insensitive mode
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    14-Aug-97                                               *
!  Last Modified:    14-Aug-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZ
!     Called by:
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     A          U      CHAR   Argument  First word to be compared
!     B          U      CHAR   Argument  Second word to be compared
!     NCAR       U      INT    Argument  Number of characters to be tested
!==== Modification History ===================================================
!     Date         Who  Modification Description
!     --------     ---  ------------------------------------------------------
!   14-Aug-97      DLS  Initial programming started
!==== SUBROUTINE CALL ========================================================
!
      LOGICAL FUNCTION SEQI(A,B,NCAR)
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER*(*) A,B
      CHARACTER*1 AC,BC
      INTEGER NCAR
      LOGICAL SEQ
!
!---- Loop on number of characters to be compared.  Stop checking when -------
!     two characters do not agree (when both are expressed as capitals
!
      SEQ = .FALSE.
      DO IC = 1,NCAR
        AC = A(IC:IC)
        BC = B(IC:IC)
        IF(AC.GE.'a'.AND.AC.LE.'z') AC = CHAR(ICHAR(AC) - 32)
        IF(BC.GE.'a'.AND.BC.LE.'z') BC = CHAR(ICHAR(BC) - 32)
        IF(AC.NE.BC) GO TO 100
      END DO
!
!----- If control reaches this point then no differences were detected in ----
!      the two words.  Return a 'true' value for the function.
!
      SEQ = .TRUE.
 100  SEQI = SEQ
      RETURN
!
!----- END OF MODULE SEQI ----------------------------------------------------
!
      END
!   MEPAS HAZ: SETSIF.FOR             Version Date: 16-Dec-93
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE SETSIF                                *
!                                                                            *
!  Subroutine SETSIF This module evaluates SIF values for non-carc., carc.,  *
!                    and rads, for one case                                  *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    16-Dec-93                                               *
!  Last Modified:    16-Dec-1993  DLS                                        *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/HAZ2
!     Called by: SUBROUTINES GWINPT, SWINPT, ATINPT, SLINPT, FDINPT
!     Calls: NONE
!     Common blocks referenced: None
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!     TERM      U      Real    Argument  Coefficient to all SIF values
!     BW        U      Real    Argument  Body weight
!     ED        U      Real    Argument  exposure duration
!     SIFN      S      Real    Argument  SIF for non-carcinogens
!     SIFC      S      Real    Argument  SIF for carcinogens
!     SIFR      S      Real    Argument  SIF for radionuclides
!     C70      S/U     Real    Internal  Constant, 70 year averaging period
!     C365     S/U     Real    Internal  Constant, days per year
!==== Modification History ===================================================
!
!     Date         Who  Modification Description
!     --------     --- ------------------------------------------------------
!
!==== SUBROUTINE CALL  ========================================================
!
       SUBROUTINE SETSIF(TERM,BW,ED,SIFN,SIFC,SIFR)
!
!==== COMMON Block Definitions ===============================================
!
!        None
!
!==== DIMENSION Statements ===================================================
!
!        None
!
!==== Variable Declarations ==================================================
!
      REAL TERM,BW,ED,SIFN,SIFC,SIFR,C70,C365
!
!==== DATA Statements ========================================================
!
      DATA C365/365.25/,C70/70./
!
!----- Start calculations.  Initialize error index and data arrays. -------
!
      SIFN = TERM/BW                  !non-carc
      SIFC = TERM*ED/(BW*C70)         !carc.
      SIFR = TERM*ED*C365             !rad.
!
      RETURN
!
!===================== END OF MODULE SETSIF ==================================
!
      END



!
!===================== Start OF MODULE GETFSIZ ==================================
!
      SUBROUTINE GETFSIZ(FID,SIZE,UNITNO)
      USE DFLIB
      CHARACTER*(*) FID
      INTEGER UNITNO, SIZE
      INTEGER*4 iresult, handle
      TYPE (FILE$INFO) info
      handle = FILE$FIRST
      iresult = GETFILEINFOQQ(FID,info,handle)
      if( iresult.NE.0 ) THEN
        SIZE=info.length
      ELSE
        SIZE=0
      END IF
      RETURN
      END


