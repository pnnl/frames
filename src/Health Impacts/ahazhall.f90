!     Last change:  MAS   03-Apr-2006   10:19 am
MODULE Con_data_mod
!!! Module Con_Data_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to constituents selected for the
!    analysis: radiation dose factors, toxicity parameters
!
!  History:
!
!    DL Strenge : 30-Apr-2001: Version 1.0
!
!  Code Block:
!
!   General parameters
      INTEGER :: NUMCON                         ! Number of constituents in run (FUI section of GID)
      CHARACTER(LEN=32),  ALLOCATABLE ::CASID(:,:)   ! Constituent ID (rad name or CasID)
      INTEGER, ALLOCATABLE :: MTYPE(:)          ! RAD/CHEM type (1 - 5)
!   Radionuclide variables
      REAL(KIND=4),  ALLOCATABLE :: RDFG(:,:)   ! Internal dose factor for ingestion
      REAL(KIND=4),  ALLOCATABLE :: RDFH(:,:)   ! Internal dose factor for inhalation
      REAL(KIND=4),  ALLOCATABLE :: RDFS(:,:)   ! Internal dose factor for skin absorption
      REAL(KIND=4),  ALLOCATABLE :: RDEX(:,:)   ! External dose factor from air immersion
      REAL(KIND=4),  ALLOCATABLE :: RDSH(:,:)   ! External dose factor from ground shine
      REAL(KIND=4),  ALLOCATABLE :: RDIMR(:,:)  ! External dose factor from water immersion
      REAL(KIND=4),  ALLOCATABLE :: RSFH(:,:)   ! cancer slope factor for inhalation
      REAL(KIND=4),  ALLOCATABLE :: RSFG(:,:)   ! cancer slope factor for ingestion - water
      REAL(KIND=4),  ALLOCATABLE :: RSFGS(:,:)  ! cancer slope factor for ingestion - soil
      REAL(KIND=4),  ALLOCATABLE :: RSFGF(:,:)  ! cancer slope factor for ingestion - food
      REAL(KIND=4),  ALLOCATABLE :: RSFS(:,:)   ! cancer slope factor for soil contamination (external)
      INTEGER,  ALLOCATABLE :: RNPRG(:)         ! Number of projeny for each constituent (parent)
!   Chemical variables
      REAL(KIND=4),  ALLOCATABLE :: RCPFH(:)  ! cancer potency factor for inhalation
      REAL(KIND=4),  ALLOCATABLE :: RCPFG(:)  ! cancer potency factor for ingestion - water
      REAL(KIND=4),  ALLOCATABLE :: RCPFGS(:) ! cancer potency factor for ingestion - soil
      REAL(KIND=4),  ALLOCATABLE :: RCPFGF(:) ! cancer potency factor for ingestion - food
      REAL(KIND=4),  ALLOCATABLE :: RURFH(:)  ! cancer unit risk factor for inhalation
      REAL(KIND=4),  ALLOCATABLE :: RRFDH(:)  ! non-cancer Reference Dose for inhalation
      REAL(KIND=4),  ALLOCATABLE :: RRFDG(:)  ! non-cancer Reference Dose for ingestion - water
      REAL(KIND=4),  ALLOCATABLE :: RRFDGS(:) ! non-cancer Reference Dose for ingestion - soil
      REAL(KIND=4),  ALLOCATABLE :: RRFDGF(:) ! non-cancer Reference Dose for ingestion - food
      REAL(KIND=4),  ALLOCATABLE :: RRFCH(:)  ! non-cancer Reference Concenration for inhalation
      REAL(KIND=4),  ALLOCATABLE :: RFONE(:)   ! F1 for chemicals only
!
!!! End Module Con_Data_Mod ++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

MODULE Errors_Mod
!!! Module Errors_Mod +++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to writing error messages to
!    the report file.  The messages are written using the routine PRTERR.
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!
!  Code Block:
!
      INTEGER :: IRPT_ERR ! Unit number for writing the error messages
      INTEGER, PARAMETER :: MAXMES = 5 ! Maximum number of lines in an error message
      CHARACTER(LEN=72), DIMENSION(MAXMES) :: MESSAG ! Vector of message lines
!
!!! End Module Errors_Mod ++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE

!      MEPAS AHAZ: AHAZH.FOR             Version Date:  14-Oct-98
!      Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                               *
!                              MAIN PROGRAM                                     *
!                                                                               *
!     Main Program AHAZH Annual HAZard analysis evaluation program              *
!                    The main program HAZ controls the calculation of human     *
!                    health impacts  for each pollutant                         *
!                                                                               *
!     Written by:       Dennis Strenge                                          *
!                       Battelle Pacific Northwest Laboratories                 *
!                       P.O. Box 999                                            *
!                       Richland, WA 99352                                      *
!                                                                               *
!     Creation Date:    19-Jun-1997 (Converted from AHAZ)                       *
!     Last Modified:    14-Oct-1998  DLS                                        *
!                                                                               *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZH
!     Called by: NONE
!     Calls: SUBROUTINES PDATINH, GETNAM, GETSET, MARKIN, RISKCAL
!     Common blocks referenced: BLKDAT, BACKGD, DEVICE, H3, C14, SWPATH, GWPATH,
!                               HPDAT, PSET1, AIRDAT, ATPATH, TITLS, COUPLE,
!                               LOCNAM, SLPATH, FDPATH, PSET3, POS,
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
!  19-Jun-97  DLS  Initial programming for health component to MEPAS 4.0.
!  06-Aug-97  DLS  Additional modifications for health component
!  14-Oct-98  DLS  Changed output health impacts for rads to "cancer fatalities"
!  19-Sep-00  DLS  Changed GID input parameter names as follows:
!                     Look for HeiSrcNum  instead of HeiRcpNum
!                     Look for HeiSrcName instead of HEIRCP
!==== SUBROUTINE CALL ========================================================
!
      PROGRAM AHAZH
!
!==== COMMON Block Definitions ===============================================
!
!
        USE Con_data_mod
        USE Errors_mod
        INCLUDE 'PARMTR.PAR'
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
!       INCLUDE 'MASBAL.FTN'
        INCLUDE 'RISKCM.FTN'
        INCLUDE 'TIMES.FTN'
!       INCLUDE 'DECAY.FTN'
!
!==== DIMENSION Statements ===================================================
!
!      DIMENSION KNPATH(7)
      REAL VALIN(360),VALOUT(360), EXPX(360), EXPY(360)
!
!==== Variable Declarations ==================================================
!
!
      CHARACTER(LEN=5) :: CALLER = 'AHAZH'  ! Name of this routine
      CHARACTER*1 B,C
      CHARACTER*22 EXNAME,CONAME,EXPTYPE,CONAMEP
      CHARACTER*12 EXROUTE
      CHARACTER*6  UT,UR
      CHARACTER*9  EU
      CHARACTER*7  STYPE
      CHARACTER*32 ELOC,GETSTR,RCPNAM,RCPNAME(30)
      CHARACTER*32 EXPNAME,EXPNAM
      CHARACTER*12 EXPRUT(26)
      CHARACTER*20 EXPLAB(26)
      CHARACTER*13 MEDTYPE,TYPES(6)
      CHARACTER*12 CONID,CONIDP
      CHARACTER*4 HO,RISK,HI,DOSE
      CHARACTER*14 FUI
      LOGICAL FOUNDM, FOUNDE, WARN, FOUNDC
      LOGICAL FILTER
      LOGICAL SEQI
 !     CHARACTER*8 MMDDYY,HHMMSSHH
      INTEGER TPATH, CTYPE, RTYPE, OTYPE
      INTEGER NU,NRISK,NHI,NDOSE
      INTEGER NCAN,NNON
      INTEGER NRADOUT(4), GETINT
      CHARACTER*37 RADOUT(4)
      CHARACTER*120 SETDATA(LINEMAX)
      CHARACTER*22 CANCER, NONCAN, INTAKE, RADOSE, OUTYPE, CONINT
      CHARACTER (LEN=10) :: DATE, TIME, ZONE
      CHARACTER (LEN=3) :: CMONTH(12),CMO
      INTEGER :: DT(8)
      CHARACTER(LEN=400) :: CLINE ! Command line image
      CHARACTER*14 CSM
      CHARACTER*3 CON                                 ! Source type for testing = "con"
      CHARACTER*14 CONI                               ! Source type for finding database data in GID
      INTEGER :: NUMRCP
      INTEGER :: NUMRCPGL
      INTEGER :: NUMEXPGL
      INTEGER :: NUMSET
      INTEGER :: NRCP
      CHARACTER*10 MODID                              ! Search ID for contaminant database ID
      INTEGER MODSRCNUM                               ! Number of source modules connected to exp module
      INTEGER(4) statarray(12), ISTSAT
      CHARACTER*5 MODSRCTYPE                          ! Source type, search for "con"
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
      DATA RADOUT/'cancer incidence                     ', &
                  'cancer fatalities                    ', &
                  'cancer plus severe hereditary effects', &
                  'Sv                                   '/   ! Modified 2-Sept-2004 DLS
!                 'radiation dose                       '/   ! for HIF specification changes
      DATA NRADOUT/16,17,37,2/
      DATA NORGS/1/,NSITES/1/
      DATA CANCER/'carcinogenic          '/
      DATA NONCAN/'noncarcinogenic       '/
      DATA INTAKE/'intake                '/
      DATA RADOSE/'radiation dose        '/
      DATA CONINT/'concentration         '/
      DATA NNON/15/, NCAN/12/
      DATA RISK/'Risk'/, HI/'HI  '/, DOSE/'Dose'/    ! Modified 2-Sept-2004 DLS
!      DATA RISK/'Risk'/, HI/'HI  '/, DOSE/'Sv  '/   ! for HIF specification changes
      DATA NRISK/4/,     NHI/2/,     NDOSE/4/        ! Modified 2-Sept-2004 DLS
!      DATA NRISK/4/,     NHI/2/,     NDOSE/2/       ! for HIF specification changes
      DATA FUI/'FUI         '/
      DATA CSM/'CSM           '/
      DATA CON/'con'/
      DATA B/' '/
      DATA TYPES/'Aquifer      ',  &
                 'Surface water',  &
                 'Air          ',  &
                 'Air          ',  &
                 'Soil         ',  &
                 'Sediment     '/          ! changed to "sediment" for future expansion
      DATA CMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'/
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
!
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


!      VERN = '(VERSION 17-Dec-04)'
!      VERN = '(VERSION 25-Aug-05)'
!      VERN = '(VERSION 07-Nov-05)'
       VERN = '(VERSION 03-Apr-06)'

!---  Read the command line to get parameters for the run
!     GETCL returns command line entries after the program name
!     GETCL is a Lahey Fortran 90 language extension
!
      ICOMP=2 ! Digital/Compaq FORTRAN
      IF(ICOMP.EQ.1)THEN ! Lahey Fortran 90
        IERR = 0
!        CALL GETCL(CLINE)    ! activate for Lahey Fortran 90
!        READ(CLINE,*,ERR=5,END=5) GIDFNM,HINFNM,SITNUM,HINUM,NAME
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
        READ(CLINE,'(I5)') HINUM
        call GetArg(5,NAME,status)
        if(status.lt.0) GO TO 5  
     ENDIF

      GO TO 6
 5    ierr = 1
      MESSAG(1) = 'Error reading command line'
      GO TO 999
  6   continue
      WRITE(*,*) TRIM(GIDFNM),TRIM(HINFNM),SITNUM,HINUM,TRIM(NAME)
!
!---- Open files for current run ---------------------------------------------
!
      NU = 1
      OPEN (UNIT=NGID, FILE=TRIM(GIDFNM)//'.GID',STATUS='OLD')
      OPEN (UNIT=NHIF, FILE=TRIM(HINFNM)//'.HIF',STATUS='UNKNOWN')
!
!---- open error message file, ERR -------------------------------------------
!
      open(UNIT=NERR,STATUS='UNKNOWN',FILE=TRIM(HINFNM)//'.ERR')
      IRPT_ERR = nerr
      OPEN (UNIT=nhls, FILE=TRIM(HINFNM)//'.HLS', STATUS='UNKNOWN')
!
!----- Write header to HLS report file ---------------------------------------
!
      WRITE (nhls,'(''Multimedia Environmental Pollutant Assessment'','' System (MEPAS)'')')
      WRITE (nhls,'(''Annual Exposure Version'')')
      WRITE (nhls,'(''MEPAS Human Health Analysis Input Summary'',2x,a19)')VERN
      WRITE (nhls,'(''Pacific Northwest Laboratory'')')
      WRITE (nhls,'(''Richland, WA 99352'')')
      WRITE (nhls,'(''Developed for the U.S. Department of Energy'')')
!
!
      CALL DATE_AND_TIME(DATE,TIME,ZONE,DT)
      IYR   = DT(1)
      IMO   = DT(2)
      IDAY  = DT(3)
      CMO   = CMONTH(IMO)
      IHR   = DT(5)
      IMINT = DT(6)
      ISEC  = DT(7)
      WRITE(nhls,1111) IDAY,CMO,IYR,IHR,IMINT,ISEC
1111  FORMAT(' Run Date and Time: ',I2,'-',A3,'-'I4,3X,I2,':',I2,':',I2)
!
!----- Write line to screen
!
      WRITE(*,8900) VERN
 8900 FORMAT(' ******* MEPAS Health Impacts - Version: ',A,' *******')
!
!----- Read FUI section from GID file to find receptor names -----------------
!
      NLINES = 0
      IERR=0
      FILTER = .TRUE.
!      CALL GETSET(NGID,NERR,CSM,NLINES,SETDATA,SITNUM,FILTER)
     CALL GETSET(NGID,NERR,FUI,NLINES,SETDATA,SITNUM,FILTER)
!      FILTER = .FALSE.
!
!      WRITE(NHLS,'(A,I6,/A,A)') ' Read from FUI Section, NLINES = ',nlines
!      do i = 1,nlines
!         WRITE(nhls,'(a)') SETDATA(i)
!      end do
!
!
      IF(NLINES.LT.1) THEN
         WRITE(NERR,5000) NLINES
 5000    FORMAT(' Error finding CSM section of GID.  NLINES = ',I4)
         IERR = IERR + 1
         GO TO 999
      ENDIF
!
      IS = SITNUM
      IH = HINUM
!
!---- Get number of receptor glyphs attached to this health glyph
!
      NUMRCP = GETINT(SETDATA,NLINES,'HEISrcNum     ',IS,IH,IZ,IZ,IZ,IZ)
!
!---- Get total number of receptor glyphs defined for this case
!
      NUMRCPGL = GETINT(SETDATA,NLINES,'RcpNum        ',IS,IZ,IZ,IZ,IZ,IZ)
!
!---- Get total number of exposure glyphs defined for this case
!
      NUMEXPGL = GETINT(SETDATA,NLINES,'ExpNum        ',IS,IZ,IZ,IZ,IZ,IZ)
!
!---- Determine the total number of data sets to be written to the HIF
!
      NUMSET = 0
      IF(NUMRCP.LE.0) THEN
        NUMSET = 1
      ELSE
!
        DO IR = 1,NUMRCP    !  Loop on receptor attached glyphs
!
!---- read name of receptor
!
          RCPNAME(IR)= GETSTR(SETDATA,NLINES,'HEISrcName    ',IS,IH,IR,IZ,IZ,IZ)
!
          DO IRG = 1, NUMRCPGL  ! Loop on total number of receptors
!
!---- get receptor name
!
             RCPNAM = GETSTR(SETDATA,NLINES,'RcpName       ',IS,IRG,IZ,IZ,IZ,IZ)
!
!---- is this receptor one of the attached receptors?
!
             IF(RCPNAME(IR).EQ.RCPNAM) THEN
!
!---- get number of transport exposure glyphs attached to this receptor
!
                NRCP = GETINT(SETDATA,NLINES,'RcpSrcNum     ',IS,IRG,IZ,IZ,IZ,IZ)
                IF(NRCP.LT.0) THEN
                  NUMSET = NUMSET + 1
                ELSE
!
!----- loop on exposure glyphs to find those attached to the current
!      receptor glyph
!
              DO IE = 1,NRCP
!
!-----  get exposure name
!
               EXPNAME = GETSTR(SETDATA,NLINES,'RcpSrcName    ',IS,IRG,IE,IZ,IZ,IZ)
               DO IEX = 1,NUMEXPGL
                 EXPNAM = GETSTR(SETDATA,NLINES,'ExpName       ',IS,IEX,IZ,IZ,IZ,IZ)
                 IF (EXPNAM.EQ.EXPNAME) THEN
                   NEXPST = GETINT(SETDATA,NLINES,'ExpTypeNum    ',IS,IEX,IZ,IZ,IZ,IZ)
                   IF (NEXPST.LE.0) NEXPST = 1 
                     NUMSET = NUMSET + NEXPST  
                   END IF    ! End if on exposure glyph found
                  END DO   ! End do on IEX = 1,total exposure glyphs
                END DO   ! End do on IE = 1,NRCP (attached exposure glyphs)
              END IF   ! End if on NRCP > 0
            END IF   ! End if on RCPNAME found
          END DO   ! End do on total receptors NUMRCPGL
        END DO   ! End do on NUMRCP
      ENDIF    ! End if on NUMRCP > 0
!
!---- Read constituent data for this run from the CON section of the GID
!
      FILTER = .TRUE.
      CALL GETSET(NGID,NERR,CSM,NLINES,SETDATA,SITNUM,FILTER)
!
!
!      WRITE(NHLS,'(A,I6,/A,A)') ' Read from CSM Section, NLINES = ',nlines
!      do i = 1,nlines
!         WRITE(nhls,'(a)') SETDATA(i)
!      end do
!
!     Find name of CON module
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
        DO IMOD = 1,NUMMOD
!
!          Get module name for current module
!
           MODID = GETSTR(SETDATA,NLINES,'ModId         ',SITNUM,IMOD,IZ,IZ,IZ,IZ)
!
!          Test to see if this module is the current module name (this MEPAS exposure module)
!
           IF(SEQI(MODID,NAME,5)) THEN
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
                    ICONNUM = ISRC
!                Get type of source module
!
                 CONI = '     '
                 CONI     = GETSTR(SETDATA,NLINES,'ModSrcID      ',SITNUM,IMOD,ISRC,IZ,IZ,IZ)
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
!----- Call GETSET to load GID file information from chemical database use CONI for set name.
!
      NLINES = 0
      DO I = 1,LINEMAX
         SETDATA(I) = ' '
      END DO
      FILTER = .TRUE.         ! To include data for current site number NUMSIT  ! Activate for Chem decay
!      WRITE(NHLS,'(A,A)') ' Reading Con section, con name is: ',CONI
      CALL GETSET(NGID,NERR,CONI,NLINES,SETDATA,SITNUM,FILTER)                  ! Activate for Chem decay
!
      CALL READ_CON_DATA(SETDATA,NLINES,IERR)
      IF(IERR.GT.0) THEN
         WRITE(NHLS,*) 'Error encountered reading constituent data - STOP'
         GO TO 999
      ENDIF
!
!----- Get run name data set from GID file -----------------------------------
!
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,NAME,NLINES,SETDATA,SITNUM,FILTER)
      IF(NLINES.LT.1) THEN
         WRITE(NERR,5002) NLINES
 5002    FORMAT(' Error finding Health section of GID.  NLINES = ',I4)
         IERR = IERR + 1
         GO TO 999
      ENDIF
!
!----- Write a header line to the HIF file -----------------------------------
!
      WRITE(NHIF,5004)
 5004 FORMAT(' 1,'/' Health Impacts File generated by MEPAS 4.0 ','health impacts component')
!
!----- Write number of data sets and media type
!
!!!       WRITE(NHIF,7003) NUMSET
!!! 7003  FORMAT(I3,',')
!
!---- Call GETDATA to read from the GID data set -----------------------------
!
      CALL GETDATA(SETDATA,NLINES)
!
!----- Loop on number of receptors for this health point ---------------------
!
      DO 50 IR = 1,NUMRCP
         RCPNAM = RCPNAME(IR)
!
!---- Open RIF file ----------------------------------------------------------
!
      OPEN (UNIT=NRIF, FILE=TRIM(GIDFNM)//'.RIF',STATUS='UNKNOWN')
!
!---- Find marker data set in RIF file
!
        CALL MARKIN(RCPNAM,foundm)
          IF(.NOT.FOUNDM) THEN
             IERR = IERR + 1
             GO TO 999
          ENDIF
!
!----- Write a summary report of pathways allowable for this run ---------
!
!----- Write lines describing special conditions for this run ------------
!
!          WRITE(nhls,2100) IRIS
! 2100     FORMAT(' Chemical toxicity data source flag =',I2,':')
!          IF(IRIS.LE.0.OR.IRIS.GE.3) THEN
!             WRITE(nhls,2102)
!          ELSE
!             IF(IRIS.EQ.1) WRITE(nhls,2103)
!             IF(IRIS.EQ.2) WRITE(nhls,2203)
!          ENDIF
! 2102   FORMAT(' Chemical toxicity values in the MEPAS chemical database are used.')
! 2103   FORMAT(' Only current IRIS approved chemical toxicity values are used.')
! 2203   FORMAT(' Only current IRIS or HEAST chemical toxicity values are used.')
!            WRITE(nhls,2104)
!            WRITE(nhls,2109)
            IF(IHEAST.LE.0) THEN
              NHEOUT = 0
              WRITE(nhls,2104)
              IF(HEINC) THEN
                WRITE(nhls,2108)
                NHEOUT = 1
              ENDIF
              IF(HEFAT) THEN
                WRITE(nhls,2110)
                NHEOUT = NHEOUT + 1
              ENDIF
              IF(HEFSH) THEN
                WRITE(nhls,2111)
                NHEOUT = NHEOUT + 1
              ENDIF
              IF(HECEDE) THEN
                WRITE(nhls,2109)
                NHEOUT = NHEOUT + 1
              ENDIF
            ELSE
              WRITE(nhls,2105)
              HEINC = .TRUE.
              HEFAT = .FALSE.
              HEFSH = .FALSE.
!              HECEDE= .FALSE.
              IF(HEINC) WRITE(NHLS,2108)
              WRITE(NHLS,2112)
              NHEOUT = 1
              IF(HECEDE) NHEOUT = NHEOUT + 1
            ENDIF
!
!----- Output option format statements ---------------------------------------
!
 2104   FORMAT(' Radiation dose factors from EPA Federal Guidance ',  &
               ' Reports No. 11 and 12 are used.')
 2105   FORMAT(' Slope factors from EPA HEAST reports are used for',  &
               ' radiation risk calculations, when available.')
 2109   FORMAT(' Radionuclide consequenses will be reported as ',     &
                 'radiation dose (Sv).')
 2108   FORMAT(' Radionuclide consequenses will be reported as ',     &
                 'cancer incidence risk.')
 2110   FORMAT(' Radionuclide consequenses will be reported as ',     &
                 'cancer fatality risk.')
 2111   FORMAT(' Radionuclide consequenses will be reported as ',     &
                 'risk of cancer incidence plus '/                    &
               '   severe hereditary effects.')
 2112   FORMAT(' Because EPA HEAST slope factors have been selected,'/&
               '   the only risk reported will be cancer incidence.', &
               '   ALL OTHER RISK OUTPUTS HAVE BEEN DISABLED.')
!
!---- Write lines for selection of chemical output ---------------------------
!
          WRITE(NHLS,2205) 
          IF(CHEMRISK) WRITE(nhls,2206)
          IF(CHEMHI) WRITE(nhls,2207) 
 2205     FORMAT(' Chemical health impact endpoints are evaluated',    &
                     ' based on input '/                               &
                 '   information in the Receptor Intake File (RIF)',   &
                     ' and the following selections.'/                 &
                 '   If the RIF file does not contain information for',&
                     ' an endpoint, it is not calculated.')
 2206     FORMAT(' Perform chemical impact analyses for carcinogenic effects')
 2207     FORMAT(' Perform chemical impact analyses for non-carcinogenic effects')
!
!---- Read header lines for this set -----------------------------------------
!
        READ(NRIF,*) NLNS
        DO IL = 1,NLNS
           READ(NRIF,*) C
        END DO
        C = C
!
!---- Read number of data sets to consider -----------------------------------
!
       READ(NRIF,*) NSETS
!       READ(NRIF,*) NSETS,STYPE
       IF(NSETS.LE.0) THEN
         WRITE(NERR,2001) NSETS
 2001    FORMAT(' Error in RIF file, number of data sets bad =',i5)
         IERR = IERR + 1
         GO TO 999
       ENDIF
!
!----- Write number of data sets and media type
!
       WRITE(NHIF,7003) NSETS
 7003  FORMAT(I3,',')
!
!---- Loop on data sets in RIF file ------------------------------------------
!
 88   continue
      DO IS = 1,NSETS
!
!---- Read first line of RIF data set and test for inclusion -----------------
!
        READ(NRIF,*,end=995) STYPE,ELOC,MEDTYPE,NPOINTS,NAGES,NCONST
!
        IF(.NOT.SEQI(STYPE,'chronic',7)) THEN           ! Case insensitive test
          NS = LEN_TRIM(STYPE)
          WRITE(NERR,2002) STYPE(1:NS)
 2002     FORMAT(' Error in RIF file, type not chronic: ',A)
          IERR = IERR + 1
          GO TO 999
        ENDIF
        NE = LEN_TRIM(ELOC)
        NM = LEN_TRIM(MEDTYPE)
        WRITE(NHIF,7004) ELOC(1:NE),MEDTYPE(1:NM),NPOINTS,NAGES,NCONST,NSITES, NORGS
 7004   FORMAT('"chronic","',A,'","',A,'",'I3,',',I2,',',I2,',',I2,',',I2,',')
        WRITE(NHIF,7007)
 7007   FORMAT('"all sites",'/'"total body",')
!
!---- Identify transport medium type, MEDTYPE --------------------------------
!
        TPATH = 0
!
        DO IT = 1,6
           IF(SEQI(MEDTYPE,TYPES(IT),13)) THEN
              TPATH = IT
           END IF
        END DO
        IF(TPATH.EQ.4) TPATH = 3
!
!----- Write error message for bad medium type -------------------------------
!
        IF(TPATH.LE.0) THEN
           WRITE(NERR,2005) MEDTYPE,(TYPES(I),I=1,6)
 2005      FORMAT(' Error in RIF, medium type not recognized: ',a/' Valid medium types are:'/&
                  6(2x,a/))
           IERR = IERR +1
           GO TO 999
        ENDIF
!
!---- Read coordinate data ---------------------------------------------------
!
        DO IP = 1,NPOINTS
          READ(NRIF,*) EXPX(IP),UT,EXPY(IP),UR
          WRITE(NHIF,7000) EXPX(IP),EXPY(IP)
 7000     FORMAT(F9.1,',"km",',f9.1,',"km",')
        END DO
!
!---- Loop on age groups -----------------------------------------------------
!
       DO IA = 1,NAGES
!
!---- read age group break points for current age group ----------------------
!
        READ(NRIF,*) TA1,TA2
        WRITE(NHIF,7006) TA1,TA2
 7006   FORMAT(F5.0,',',F5.0,',"yr",')
        DELTIM = TA2 - TA1
!
!---- Do for each constituent in RIF file ------------------------------------
!
        DO IPOL = 1,NCONST
!
!---- Read constituent name, progeny and number of time periods ----
!
        READ(NRIF,*) CONAME,CONID,NPRG,NTIMES
        NCM = LEN_TRIM(CONAME)
        NID = LEN_TRIM(CONID)
        WRITE(NHIF,7001) CONAME(1:NCM),CONID(1:NID),NPRG,NTIMES
 7001   FORMAT('"',A,'","',A,'",',I2,',',I4,',')
!
!
!---- For each time period read intake data and calculate health impacts -----
!
        DO ITM = 1,NTIMES
!          IF(ITM.LE.1) THEN
!
!         Find contaminant in list of constituents
!
!          ENDIF
!          IPDAT = 1
!
!---- Identify contaminant in master list and set index
!
          icdx = 0
          FOUNDC = .FALSE.
          do IC = 1,NUMCON
             IF(SEQI(CONID,CASID(IC,1),8)) THEN
                ICDX = IC
                FOUNDC = .TRUE.
                EXIT
             ENDIF
          end do
          IF(.NOT.FOUNDC) THEN
             IERR = 1
             MESSAG(1) ='Error: contaminant name from RIF not in master list: '//TRIM(conid)
             CALL PRTERR(IERR,CALLER,1)
             GO TO 999
          ENDIF
          IP = 0
          IERR = 0
!
!---- Call SET_DATA to read pollutant specific parameters ---------------------
!
          CALL SET_DATA(ICDX,IP,IERR)
          IF(IERR.GT.0) GO TO 999
           READ(NRIF,*) TSTART,UT,TREL,UR,NEXPTH
           NUT = LEN_TRIM(UT)
           NUR = LEN_TRIM(UR)
           NXOUT = NEXPTH
           TEXP = TREL
           IF(DELTIM.LT.TREL) TEXP = DELTIM
           IF(MTYPE(ICDX).EQ.1) NXOUT = NEXPTH * NHEOUT
           WRITE(NHIF,7005) TSTART,UT(1:NUT),TEXP,UR(1:NUR),NXOUT
 7005      FORMAT(F9.2,',"',A,'",',F9.2,',"',A,'",',I3,',')
           DO IEX = 1,NEXPTH
              READ(NRIF,*) EXPOP,EXNAME,EXROUTE,EU,EXPTYPE
              IF(INHALE.EQ.0)  THEN
                 IF(.NOT.SEQI(EU,'mg/m^3',6)) THEN
                    SKIP_INHALE = .TRUE.
                 ELSE
                    SKIP_INHALE = .FALSE.
                 ENDIF
              ENDIF
              IF(INHALE.EQ.1.) THEN
                IF(.NOT.SEQI(EU,'mg/kg/d',7)) THEN
                   SKIP_INHALE = .TRUE.
                ELSE
                   SKIP_INHALE = .FALSE.
                ENDIF
              ENDIF
! debug
!          WRITE(NHIF,*) TRIM(conid),' INHALE, SKIP_INHALE, EU',INHALE, SKIP_INHALE,TRIM(EU)
! debug
              NX = LEN_TRIM(EXNAME)
              NR = LEN_TRIM(EXROUTE)
              NE = LEN_TRIM(EXPTYPE)
              READ(NRIF,*) (VALIN(I),I=1,NPOINTS)
              CTYPE = 0
              RTYPE = 0
              IF(EXPTYPE.EQ.CANCER) THEN
                 IF(CHEMRISK) CTYPE = 1
                 OUTYPE = CANCER
                 NO = NCAN
                 HO = RISK
                 NU = NRISK
              ELSE IF (EXPTYPE.EQ.NONCAN) THEN
                 IF(CHEMHI) CTYPE = 2
                 OUTYPE = NONCAN
                 NO = NNON
                 HO = HI
                 NU = NHI
              ELSE IF(EXPTYPE.EQ.INTAKE) THEN
                 RTYPE = 1
              ELSE IF(EXPTYPE.EQ.RADOSE) THEN
                 RTYPE = 2
              ELSE IF(EXPTYPE.EQ.CONINT) THEN
                 RTYPE = 1
              ELSE
                 WRITE(NERR,6000) CONAME(1:NCM),EXNAME(1:NX),EXROUTE(1:NR),EXPTYPE(1:NE)
 6000            FORMAT(' Error reading RIF for ',A,' pathway ',A,1x,A/  &
                        '   exposure type unidentified: ',A)
                 WRITE(NERR,2011) ' Valid Exposure Types are:', (types(i),I=1,6)
 2011            FORMAT(1X,A,A,A,/20(A,A,2x,A,A/))
!
                 IERR = IERR + 1
                 GO TO 999
              ENDIF
!
!----- Calculate health impacts for current exposure pathway ------------------
!       and write results for current exposure pathway
!
              IF(MTYPE(ICDX).GT.1) THEN     ! Chemicals
                CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,NPOINTS,CTYPE,CTYPE,VALIN,VALOUT,FOUNDE)
                IF(.NOT.FOUNDE) THEN
                   WRITE(NERR,2006) EXNAME(1:NX),EXROUTE(1:NR)
 2006              FORMAT(' Error: RIF exposure pathway unknown: ',A,1X,A)
                   WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
 2010              FORMAT(1X,A,A,A,/20(A,A,2x,A,A/))
                ELSE
                  WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),HO(1:NU),OUTYPE(1:NO)
 3001           FORMAT(F9.0,',"',A,'","',A,'","',A,'","',A,'",')
                  DO I = 1,NPOINTS
                    WRITE(NHIF,3002) VALOUT(I)
                  END DO
 3002           FORMAT(1PE9.2,',')
! 3002           FORMAT(1PE9.2, ',',359(E9.2,','))
                ENDIF
!
!----- End of chemical effects, now do radionuclides -------------------------
!
              ELSE
                WARN = .FALSE.
                IF(HEINC) THEN
                  OTYPE = 1
                  NO = NRADOUT(1)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,  &
                    NPOINTS,OTYPE,RTYPE,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE) THEN
                    WRITE(NERR,2006) EXNAME(1:NX),EXROUTE(1:NR)
                    WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                          RISK(1:NRISK),RADOUT(1)(1:NO)
                    DO I = 1,NPOINTS
                      WRITE(NHIF,3002) VALOUT(I)
                    END DO
!                    WRITE(NHIF,3002) (VALOUT(I),I=1,NPOINTS)
                  ENDIF
                ENDIF
                IF(HEFAT) THEN
                  OTYPE = 2
                  NO = NRADOUT(2)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,      &
                    NPOINTS,OTYPE,RTYPE,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                    WRITE(NERR,2006) EXNAME(1:NX),EXROUTE(1:NR)
                    WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                          RISK(1:NRISK),RADOUT(2)(1:NO)
                    DO I = 1,NPOINTS
                      WRITE(NHIF,3002) VALOUT(I)
                    END DO
!                    WRITE(NHIF,3002) (VALOUT(I),I=1,NPOINTS)
                  ENDIF
                ENDIF
                IF(HEFSH) THEN
                  OTYPE = 3
                  NO = NRADOUT(3)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,  &
                    NPOINTS,OTYPE,RTYPE,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                    WRITE(NERR,2006) EXNAME(1:NX),EXROUTE(1:NR)
                    WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                          RISK(1:NRISK),RADOUT(3)(1:NO)
                    DO I = 1,NPOINTS
                      WRITE(NHIF,3002) VALOUT(I)
                    END DO
!                    WRITE(NHIF,3002) (VALOUT(I),I=1,NPOINTS)
                  ENDIF
                ENDIF
                IF(HECEDE) THEN
                  OTYPE = 4
                  NO = NRADOUT(4)
                  CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,  &
                    NPOINTS,OTYPE,RTYPE,VALIN,VALOUT,FOUNDE)
                  IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                    WRITE(NERR,2006) EXNAME(1:NX),EXROUTE(1:NR)
                    WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
                    WARN = .TRUE.
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                          DOSE(1:NDOSE),RADOUT(4)(1:NO)
                    DO I = 1,NPOINTS
                      WRITE(NHIF,3002) VALOUT(I)
                    END DO
!                    WRITE(NHIF,3002) (VALOUT(I),I=1,NPOINTS)
                  ENDIF
                ENDIF
!                ENDIF
              ENDIF
!
!----- End of processing for radionuclide results ----------------------------
!
           END DO   ! Loop on exposure pathways for parent
!
!---- If progeny data, read and process --------------------------------------
!     Only used for radionuclides, may assume MTYPE(i) = 1 if NPRG > 0
!
           IF(NPRG.GT.0) THEN
              DO IPR = 1,NPRG
                 READ(NRIF,*) CONAMEP,CONIDP,NEXPTH
!
!---- Identify progeny contaminant in master list and set index
!
                 IPX = 0
                 FOUNDC = .FALSE.
                 do IC = 1,NUMCON
                    IF(SEQI(CONIDP,CASID(IC,IPR+1),8)) THEN
                       IPX = IPR
                       FOUNDC = .TRUE.
                       EXIT
                    ENDIF
                 end do
                 IF(.NOT.FOUNDC) THEN  ! progeny not found, write null record and skip data
!                Set parameter values to zero and continue to generate zero results
                    do  IP = 1,NPOINTS
                       VALOUT(IP) = 0.0
                    end do
                 ELSE      ! Progeny found, process data
                    IERR = 0
                    CALL SET_DATA(ICDX,IPX,IERR)
                    IF(IERR.GT.0) GO TO 999
!
                 ENDIF    ! End if on progeny found in master list
!
                 NCM = LEN_TRIM(CONAMEP)
                 NID = LEN_TRIM(CONIDP)
                 NXOUT = NEXPTH
                 IF(MTYPE(ICDX).EQ.1) NXOUT = NXOUT * NHEOUT
                 WRITE(NHIF,7001) CONAMEP(1:NCM),CONIDP(1:NID),NXOUT
!
!---- Call PDATINI to get pollutant specific parameters for progeny ----------
!
!                 IPDAT = 1
!                 CALL PDATINH(CONIDP,IPDAT,IERR)
                 IF(IERR.GT.0) GO TO 999
                 DO IEX = 1,NEXPTH              ! loop on exposure pathways for progeny
                    READ(NRIF,*) EXPOP,EXNAME,EXROUTE,EU,EXPTYPE
                    IF(INHALE.EQ.0)  THEN
                       IF(.NOT.SEQI(EU,'mg/m^3',6)) THEN
                         SKIP_INHALE = .TRUE.
                       ELSE
                         SKIP_INHALE = .FALSE.
                       ENDIF
                    ENDIF
!
                    IF(INHALE.EQ.1.) THEN
                       IF(.NOT.SEQI(EU,'mg/kg/d',7)) THEN
                          SKIP_INHALE = .TRUE.
                       ELSE
                          SKIP_INHALE = .FALSE.
                       ENDIF
                    ENDIF
                    NX = LEN_TRIM(EXNAME)
                    NR = LEN_TRIM(EXROUTE)
                    NU = LEN_TRIM(EU)
                    NE = LEN_TRIM(EXPTYPE)
                    READ(NRIF,*) (VALIN(I),I=1,NPOINTS)
!
                    RTYPE = 0
                    IF(EXPTYPE.EQ.INTAKE) THEN
                       RTYPE = 1
                    ELSE IF(EXPTYPE.EQ.RADOSE) THEN
                       RTYPE = 2
                    ELSE IF(EXPTYPE.EQ.CONINT) THEN
                       RTYPE = 1
                    ELSE
                 WRITE(NERR,6000) CONAMEP(1:NCM),EXNAME(1:NX),EXROUTE(1:NR),EXPTYPE(1:NE)
                 WRITE(NERR,2011) ' Valid Exposure Types are:', (types(i),I=1,6)
                      IERR = IERR + 1
                      GO TO 999
                    ENDIF
!
!----- Calculate health impacts for current exposure pathway -----------------
!       and write results for current exposure pathway -----------------------
!
                WARN = .FALSE.
                IF(HEINC) THEN
                  OTYPE = 1
                  NO = NRADOUT(1)
                  IF(FOUNDC) THEN
                     CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,  &
                        NPOINTS,OTYPE,RTYPE,VALIN,VALOUT,FOUNDE)
                     IF(.NOT.FOUNDE) THEN
                       WRITE(NERR,2006) EXNAME(1:NX),EXROUTE(1:NR)
                       WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
                       WARN = .TRUE.
                     ELSE
                       WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                             RISK(1:NRISK),RADOUT(1)(1:NO)
                       DO I = 1,NPOINTS
                         WRITE(NHIF,3002) VALOUT(I)
                       END DO
!                    WRITE(NHIF,3002) (VALOUT(I),I=1,NPOINTS)
                     ENDIF
                  ELSE
                     WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                           RISK(1:NRISK),RADOUT(1)(1:NO)
                     DO I = 1,NPOINTS
                        WRITE(NHIF,3002) VALOUT(I)
                     END DO
                  ENDIF
                ENDIF
                IF(HEFAT) THEN
                  OTYPE = 2
                  NO = NRADOUT(2)
                  IF(FOUNDC) THEN
                    CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,  &
                      NPOINTS,OTYPE,RTYPE,VALIN,VALOUT,FOUNDE)
                    IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                      WRITE(NERR,2006) EXNAME(1:NX),EXROUTE(1:NR)
                      WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
                      WARN = .TRUE.
                    ELSE
                      WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                            RISK(1:NRISK),RADOUT(2)(1:NO)
                      DO I = 1,NPOINTS
                        WRITE(NHIF,3002) VALOUT(I)
                      END DO
!                      WRITE(NHIF,3002) (VALOUT(I),I=1,NPOINTS)
                    ENDIF
                  ELSE
                     WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                           RISK(1:NRISK),RADOUT(2)(1:NO)
                     DO I = 1,NPOINTS
                        WRITE(NHIF,3002) VALOUT(I)
                     END DO
                  ENDIF
                ENDIF
                IF(HEFSH) THEN
                  OTYPE = 3
                  NO = NRADOUT(3)
                  IF(FOUNDC) THEN
                    CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,   &
                      NPOINTS,OTYPE,RTYPE,VALIN,VALOUT,FOUNDE)
                    IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                      WRITE(NERR,2006) EXNAME(1:NX),EXROUTE(1:NR)
                      WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
                      WARN = .TRUE.
                    ELSE
                      WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                            RISK(1:NRISK),RADOUT(3)(1:NO)
                      DO I = 1,NPOINTS
                        WRITE(NHIF,3002) VALOUT(I)
                      END DO
!                      WRITE(NHIF,3002) (VALOUT(I),I=1,NPOINTS)
                    ENDIF
                  ELSE
                     WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                           RISK(1:NRISK),RADOUT(3)(1:NO)
                     DO I = 1,NPOINTS
                        WRITE(NHIF,3002) VALOUT(I)
                     END DO
                  ENDIF
                ENDIF
                IF(HECEDE) THEN
                  OTYPE = 4
                  NO = NRADOUT(4)
                  IF(FOUNDC) THEN
                    CALL RISKCALC(EXNAME,EXROUTE,EU,TPATH,  &
                      NPOINTS,OTYPE,RTYPE,VALIN,VALOUT,FOUNDE)
                    IF(.NOT.FOUNDE.AND..NOT.WARN) THEN
                      WRITE(NERR,2006) EXNAME(1:NX),EXROUTE(1:NR)
                      WRITE(NERR,2010) ' Valid Exposure Pathways and Routes are:', (EXPLAB(I),EXPRUT(I),I=1,26)
                      WARN = .TRUE.
                    ELSE
                      WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR),&
                            DOSE(1:NDOSE),RADOUT(4)(1:NO)
                      DO I = 1,NPOINTS
                        WRITE(NHIF,3002) VALOUT(I)
                      END DO
!                    WRITE(NHIF,3002) (VALOUT(I),I=1,NPOINTS)
                    ENDIF
                  ELSE
                    WRITE(NHIF,3001)EXPOP,EXNAME(1:NX),EXROUTE(1:NR), &
                         RISK(1:NRISK),RADOUT(4)(1:NO)
                    DO I = 1,NPOINTS
                      WRITE(NHIF,3002) VALOUT(I)
                    END DO
                  ENDIF
!
                ENDIF
!                ENDIF
!
                 END DO   ! Loop on exposure pathways for progeny
!
              END DO  ! Loop on progeny
!
           END IF  ! If on progeny included
!
        END DO   ! Loop on time periods
!
       END DO  ! Loop on constituents
!
!---- END LOOP on age groups
!
       END DO
!
      END DO  !  Loop on number of sets in RIF file, IS
!
      CLOSE(NRIF)
 50   CONTINUE  ! Loop on number of receptors for health point
!
995   CLOSE (NPIN)
      ENDFILE (nhls)
      CLOSE (nhls)
      ENDFILE (NERR)
      LENERR = 0
!      INQUIRE (UNIT=NERR,FLEN = LENERR)
    ISTAT = FSTAT (NERR, statarray)
    IF (.NOT. ISTAT) THEN
        LENERR = statarray(8)
      ENDIF
      IF(LENERR.GT.0) THEN
        CLOSE (NERR)
      ELSE
        CLOSE (NERR,STATUS="DELETE")
      ENDIF
   85 CONTINUE
      GO TO 999
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
 1006 FORMAT(' ERROR IN VALUE GIVEN FOR NUMBER OF TIME PERIODS,',  &
                 ' NPER =',I4)
 1007 FORMAT(' ERROR READING WATER FILE FOR POLLUTANT ',I5,2X,2A4)
 1008 FORMAT(' .WIN WASTE UNIT NUMBER DOES NOT MATCH ORDER, EXPECTED', &
       I3,', BUT FOUND',I3)
 1009 FORMAT(' END-OF-FILE ENCOUNTERED ON UNIT',I3,' WHILE READING '   &
      /' AIR RELEASE RATE DATA - STOP')
 2101 FORMAT( ' Number of Constituents is:',I3)
! *************************************************************************C
!
  999 CONTINUE
      STOP
      END
!=================== END OF MODULE AHAZH ====================================
!   MEPAS HAZ2: BLKDAT.FOR             Version Date: 28-Mar-2006
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
!  Last Modified:    28-Mar-2006  MAS                                        *
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
!     --------      ---  -----------------------------------------------------
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
!     28-Mar-06     MAS  Added CHEMLIM, RADLIM value to common block RISKCM
!==== SUBROUTINE CALL ========================================================
!
      BLOCK DATA BLKDAT
!
!==== COMMON Block Definitions ===============================================
!
!  include 'CRLOC.FTN'
!       include 'AGCLAS.FTN'
!  include 'AQCLAS.FTN'
!  include 'AQPATH.FTN'
  include 'DEVICE.FTN'
!  include 'DWPATH.FTN'
!  include 'RECLAS.FTN'
!  include 'PTHUSE.FTN'
!  INCLUDE 'CRPATH.FTN'
        INCLUDE 'RISKCM.FTN'
!  INCLUDE 'DERMDAT.FTN'
!  INCLUDE 'DRPATH.FTN'
!        INCLUDE 'LEACH.FTN'
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
!
!----- Logical I/O unit definitions for COMMON Block DEVICE --------------
!
      DATA NIN,NHLS,NRIF,NDBLU,NAIN,NPIN/5,6,12,10,2,8/
      DATA NFAC,NARR,NTMP,INUNIT,txt,NHIF/4,7,9,16,15,17/
      DATA NGID,NERR,NPRM,NSOIL,NFOOD/18,19,20,21,22/
!
!----- Parameter definitions for COMMON Block RISKCM---------------------
!
      DATA RFDLIM/0.0/,CHEMLIM/0.0/,RADLIM/0.0/
!   health effects conversion factors, risk / rem
      DATA HECONINC/6.0E-4/,HECONFAT/5.0E-4/,HECONFSH/7.3E-4/
!
!============ END OF MODULE BLKDAT =========================================
!
      END
!
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
      CHARACTER*32 MARK
      CHARACTER*(*) :: NAME
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
 10   READ(NRIF,*,ERR=999,END=999) MARK,NLTEXT    ! Read a marker and number of lines
       IF(.NOT.SEQI(NAME,MARK,N32)) THEN
        DO IL = 1,NLTEXT
          READ(NRIF,*) C
        END DO
        C = C
        GO TO 10
      END IF
      FOUNDM = .TRUE.
!
      RETURN
!
!--- Error in file read.  Marker name not found. ---------------------------
!
 999  WRITE(NERR,100) NAME,MARK
 100  FORMAT(' Error in reading marker information '/' Sought: ',a20,'  Last found: ',a20)
!
!----- END OF MODULE MARKIN -------------------------------------------------
!
      END
!
!   MEPAS AHAZ: PDATINH.FOR             Version Date:  23-Jun-97
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE PDATINH                               *
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
!  Last Modified:    23 Jun 97 DLS                                           *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZH
!     Called by: SUBROUTINE AHAZH, main program
!     Calls: SUBROUTINES NONE
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
!==== SUBROUTINE CALL ========================================================
!
!      SUBROUTINE PDATINH(CONID,IPDAT,IERR)
!
!==== COMMON Block Definitions ===============================================
!
!      include 'PSET1.FTN'
!      include 'PSET2.FTN'
!     include 'PSET3.FTN'
!     include 'DERMDAT.FTN'
!      include 'DEVICE.FTN'
!      include 'TITLS.FTN'
!      include 'KLASS.FTN'
!      include 'PFLAGS.FTN'
!      include 'RISKCM.FTN'
!      include 'COUPLE.FTN'
!     include 'DECAY.FTN'
!
!==== DIMENSION Statements ===================================================
!
!      DIMENSION CAS(3), PNAME(5), CASID(3,20), FNAME(17),ffname(17,20)
!      DIMENSION YESNO(20,4)
!
!==== Variable Declarations ==================================================
!
!      CHARACTER*4 CAS, PNAME, CASID, FNAME, ffname, yesno, yes, no
!      CHARACTER*4 NA
!      CHARACTER*80 XXX
!      CHARACTER*1 B
!      CHARACTER*12 CONID, NEWNAM(300)
!      LOGICAL NEWPOL
!!
!!==== DATA Statements ========================================================
!!
!      DATA NEWNUM/0/
!      DATA YES/' Yes'/
!      DATA  NO/'  No'/
!      DATA  NA/'  NA'/
!      DATA B/' '/
!!
!!----- Start of Calculations --------------------------------------------
!!----- Initialize index values
!!
!      NF = 0
!      NEWPOL = .TRUE.
!!
!!----- Open chemical database file --------------------------------------
!!
!      NHINAM = INDEX(HINFNM,B) - 1
!      IF(NHINAM.LE.0) NHINAM = 128
!      OPEN (UNIT=NDBLU,FILE=HINFNM(1:NHINAM)//'.CHM',STATUS='OLD')
!!
!! ***********************************************************************
!!
!!  Find parent constituent in Chemical Database.  Statement 10 is return
!!  point for incrementing search in Chemical database.
!!
!! ***********************************************************************
!!
!!----- READ FIRST RECORD OF DATA BASE, NUMBER OF ENTRIES AND TITLE
!!
!      READ(NDBLU,1000,END=999,ERR=998) NPDB,PDTITL
!!   If number of constituents in the chemical database is < 1, error
!      IF(NPDB.LT.1) GO TO 997
!!  LOOP OVER ENTRIES IN DATA BASE
!      NP = 0
! 10   NP = NP + 1
!      IF(NP.GT.NPDB) GO TO 996
!!  READ NEXT ENTRY IN MASTER DATA LIBRARY
!      READ(NDBLU,1005,END=999,ERR=998) CAS,(FNAME(I),I=1,17),  & ! Line 1
!       KTYPE,PNAME,                                            & ! Line 2
!                                   RDFG,RDFA,RDEX,RDSH,RDIMR,  & ! Line 3
!       RCPFH,RCPFG,RRFDH,RRFDG,IIT,IIT,IIT,IIT,IIT,            & ! Line 5
!       RFONE,                                                  &
!       KCPFH,KCPFG,KRFDH,KRFDG                                   ! line 6
! 1005 FORMAT(3A4,17A4/I2,5A4//40X,5E10.0////                   &
!       4E10.0,5I5,E10.0/I3,7X,I3,7X,I3,7X,I3)
!!
!!  Check for match with parent constituent.
!!
!      IF(CAS(1).NE.CONID(1:4)) GO TO 10
!      IF(CAS(2).NE.CONID(5:8)) GO TO 10
!      IF(CAS(3).NE.CONID(9:12)) GO TO 10
!      MTYPE(1)=KTYPE
!      IIT = IIT
!!
!!----- Check to see if this is a new pollutant -------------------------------
!!
!      IF(NEWNUM.GT.0) THEN
!         DO INEW = 1,NEWNUM
!            IF(NEWNAM(INEW).EQ.CONID) THEN
!               NEWPOL = .FALSE.
!               GO TO 22
!            ENDIF
!         END DO
!         NEWNUM = NEWNUM + 1
!         NEWNAM(NEWNUM) = CONID
!         NEWPOL = .TRUE.
!      ELSE
!         NEWNUM = NEWNUM + 1
!         NEWNAM(NEWNUM) = CONID
!         NEWPOL = .TRUE.
!      ENDIF
! 22   CONTINUE
!!
!!---- Primary constituent found in Chemical Database.  Check type. -----------
!!
!      REWIND(NDBLU)
!      NUC = 1
!!
!!----- Set parameters for each chain member ----------------------------------
!!
 !     IP = 1
!!
!! ************************************************************************
!!
!!   PROCESS DATA FOR POLLUTANT (AS FIRST CHAIN MEMBER, IP = 1)
!!
!! ************************************************************************
!!
!      DO 50 IL=1,5
!        ANAME(IL,IP)=PNAME(IL)
!  50  CONTINUE
!      do 55 il=1,17
!        ffname(il,ip) = fname(il)
! 55   continue
!      DO 60 IL=1,3
!       CASID(IL,IP)=CAS(IL)
! 60   CONTINUE
!!
!!----- Set toxicity parameters according to value given for IREFX ------------
!!
!      CPFH = 0.0
!      CPFG = 0.0
!      CPFS = 0.0
!      RFDH = 0.0
!      RFDG = 0.0
!      DFG = 0.0
!      DFA = 0.0
!      DFS = 0.0
!      DIMR= 0.0
!      DSH = 0.0
!      DEX = 0.0
!      FONE = 1.0
!      IF(RFONE.GT.0.) FONE = RFONE
!      IF(KTYPE.EQ.1) THEN       ! Radionuclides
!        YESNO(1,1) = NA
!        YESNO(1,2) = NA
!        YESNO(1,3) = NA
!        YESNO(1,4) = NA
!!
!!----- Set internal dose factor for dermal absorption for radionuclides ------
!!
!        DFS = RRFDG
!        DFG=RDFG
!        DFA=RDFA
!        DSH=RDSH
!        DIMR=RDIMR
!        DEX=RDEX
!!
!!----- Use slope factors for radionuclides (calculate dose factors from
!!      given slope factors)
!!
!        CPFH = RCPFH
!        CPFG = RCPFG
!        CPFS = RRFDH
!      ELSE               ! Chemical pollutants
!        CPFH = 0.0
!        CPFG = 0.0
!        RFDH = 0.0
!        RFDG = 0.0
!        YESNO(1,1) = NO
!        YESNO(1,2) = NO
!        YESNO(1,3) = NO
!        YESNO(1,4) = NO
!        IF(KCPFH.EQ.1) YESNO(1,1) = YES
!        IF(KCPFG.EQ.1) YESNO(1,2) = YES
!        IF(KRFDH.EQ.1) YESNO(1,3) = YES
!        IF(KRFDG.EQ.1) YESNO(1,4) = YES
!        IF(IRIS.GT.0.AND.IRIS.LE.6) THEN  ! IRIS set to 0 or 2 (CETOXT)
!          IF(KCPFH.LE.IRIS) CPFH = RCPFH
!          IF(KCPFG.LE.IRIS) CPFG = RCPFG
!          IF(KRFDH.LE.IRIS) RFDH = RRFDH
!          IF(KRFDG.LE.IRIS) RFDG = RRFDG
!        ELSE
!          CPFH = RCPFH
!          CPFG = RCPFG
!          RFDH = RRFDH
!          RFDG = RRFDG
!        ENDIF
!      ENDIF
!!
!!  End of seting toxicity parameters
!!
!      NF=NF+1
!      IF(NF.GE.NUC) GO TO 2000
!!
!! ************************************************************************
!!    End of search loop for chain members
!! ************************************************************************
!!
!      GO TO 2000
!!-----  ALL DESIRED POLLUTANTS WERE NOT FOUND IN THE DATA BASE.
! 996  WRITE(NERR,1011)
!      IERR = IERR + 1
!      GO TO 900
!  998 WRITE(NERR,1012) NP
!!      WRITE(*,1012)    IPO
!      BACKSPACE (NDBLU)
!      READ(NDBLU,'(A80)',END=999,ERR=999) XXX
!      WRITE(NERR,*) XXX
!      IERR = IERR + 1
!      GO TO 900
!  999 WRITE(NERR,1013) NDBLU,NP
!!      WRITE(*,1013)    NDBLU,IPO
!      IERR = IERR + 1
!      GO TO 900
!  997 WRITE(NERR,1014) NPDB
!!      WRITE(*,1014)    NPDB
!      IERR = IERR + 1
!      GO TO 900
!!
!!----- Print reports of library data if IPDAT > 0.
!!
! 2000 REWIND(NDBLU)
!      IF(IPDAT.GT.0.AND.NEWPOL) THEN
!!      npol = NUC
!      write(NHLS,'(//''Num Constituent Names ------------------------------------------ CASID  '')')
!       WRITE(NHLS,1111) ip,(fFNAME(I,ip),I=1,15),(CASID(I,IP),I=1,3)
! 1111   FORMAT(I2,2x,15A4,1x,3A4,I4)
!!-----
!!----- modify constituent database printout --- JWB - 01/17/90
!!-----
!         WRITE(NHLS,1001) char(12),PDTITL(1:76),NPDB
!         write(NHLS,3002)
!         write(NHLS,'(41(''-''))')
! 3002     format( '  Constituent   Parameter   ',5x,('Constituent '), &
!                   /,'   Parameter      Units         ')
!!-----
!!----- writing out the constituent parameter chart(s)
!!-----
!       write(NHLS,2001) (CASID(I,IP),I=1,2)
! 2001   format('ID / CAS #:      (None)         ',1p,4(2x,2a4,2x))
!       write(NHLS,2002) mtype(ip)
! 2002   format('Toxicity Type:   (None)         ',4(1x,I10,1x))
!       write(NHLS,2040) fone
! 2040   format('GI Tract Absorption Fraction:   ',1p,1x,E10.3)
!       write(NHLS,2013) dfg
! 2013   format('Ing Dose Fact:   1/Kg or rem/pCi',1p,4(1x,E10.3,1x))
!       write(NHLS,2014) dfa
! 2014   format('Inh Dose Fact:   1/Kg or rem/pCi',1p,4(1x,E10.3,1x))
!       write(NHLS,2041) dfs
! 2041   format('Dermal Dose Fact:  rem/pCi      ',1p,4(1x,E10.3,1x))
!       write(NHLS,2015) dex
! 2015   format('Air Imm Fact:    rem*m3/pCi*h   ',1p,4(1x,E10.3,1x))
!       write(NHLS,2016) dsh
! 2016   format('Soil Exp Fact:   rem*m2/pCi*h   ',1p,4(1x,E10.3,1x))
!       write(NHLS,2017) dimr
! 2017   format('Water Imm Fact:  rem*L/pCi*h    ',1p,4(1x,E10.3,1x))
!!  write information on inhalation cancer potency factors
!       write(NHLS,2026) cpfh
! 2026   format('Inh Slope Factor: (see below)   ',1p,1x,E10.3)
!       write(NHLS,2126) YESNO(1,1)
! 2126   format('EPA IRIS approved CPF inh?      ',1x,A10)
!!  write information on ingestion cancer potency factors
!       write(NHLS,2027) cpfg
! 2027   format('Ing Slope Factor: (see below)   ',1p,4(1x,E10.3,1x))
!       write(NHLS,2127) YESNO(ip,2)
! 2127   format('EPA IRIS approved CPF ing?      ',4(1x,A10,1x))
!!  write information on inhalation RfD values
!       write(NHLS,2028) rfdh
! 2028   format('Inh Ref Dose: (see below)       ',1p,4(1x,E10.3,1x))
!       write(NHLS,2128) YESNO(ip,3)
! 2128   format('EPA IRIS approved RfD inh?      ',4(1x,A10,1x))
!!   write information on ingestion RfD values
!       write(NHLS,2029) rfdg
! 2029   format('Ing Ref Dose:    mg/(Kg*day)    ',1p,4(1x,E10.3,1x))
!       write(NHLS,2129) YESNO(ip,4)
! 2129   format('EPA IRIS approved RfD ing?      ',4(1x,A10,1x))
!!
!       write(NHLS,'(79(''*''))')
!       IF(MTYPE(1).EQ.1) THEN
!       write(NHLS,'(''Toxicity Type 1: Radionuclide carcinogenic inhalation and ingestion'')')
!       ELSEIF(MTYPE(1).EQ.2) THEN
!       write(NHLS,'(''Toxicity Type 2: Chemical carcinogenic inhalation and ingestion'')')
!       ELSEIF(MTYPE(1).EQ.3) THEN
!       write(NHLS,'(''Toxicity Type 3: Chemical carcinogenic inhalation, non-carcinogenic ingestion'')')
!       ELSEIF(MTYPE(1).EQ.4) THEN
!       write(NHLS,'(''Toxicity Type 4: Chemical non-carcinogenic inhalation, carcinogenic ingestion'')')
!       ELSEIF(MTYPE(1).EQ.5) THEN
!       write(NHLS,'(''Toxicity Type 5: Chemical non-carcinogenic inhalation and ingestion'')')
!       ENDIF
!       write(NHLS,2037)
! 2037 format('Units of inhalation and ingestion slope factors: '/        &
!             '    Radionuclides - risk per pCi intake,'/                 &
!             '    Chemicals     - (d*kg/mg)'/                            &
!             'Units of Inh RfD:      chemicals - Inh RfD (mg/(kg*d)'/,   &
!             '                   radionuclides - external ',             &
!             'dose slope factor, (risk/yr per pCi/g soil)')
!       write(NHLS,'(38(''*''),/,''KEY for Constituent Table'')')
!         write(NHLS,2035)  (aname(i,1),i=1,5)
! 2035     format('Constituent is ',5a4)
!      ENDIF
! 900  RETURN
!
!----- Format Statements -------------------------------------------------
!
! 1000 FORMAT(I4,A80)
! 1001 FORMAT(a1,14X,'Summary of Data from MEPAS Constituent Database'   &
!            ,/,4x,A76,                                                  &
!      /,10x,'This Database Contains Entries for ',I4,' Constituents'/)
! 1122 FORMAT(a1,11X,'Summary of Data from MEPAS Constituent Database',  &
!                  ' (Cont)',/,4x,A76,                                   &
!      /,10x,'This Database Contains Entries for ',I4,' Constituents'/)
!1016 FORMAT(/'POLLUTANT NAME            CP INH.     CP ING. ',         &
!        '   RFD INH.    RFD ING.   ')
! 1021 format(26x,'Kg*day/mg    Kg*day/mg   mg/Kg/day   mg/Kg/day')
! 1020 format(24x,' 1/days       1/days      1/days      1/days',        &
!       '      rem/pCi     rem/pCi')
! 1006 FORMAT(1X,5A4,2X,1PE10.1,2X,5(E10.1,2X))
! 1017 FORMAT(1X,5A4,3A4,I3,2X,1PE10.1,2X,5(E10.1,2X))
! 1007 FORMAT(1X,5A4,1PE10.1,3X,4(E10.1,2X),5E10.1)
! 1008 FORMAT(1X,5A4,1PE10.1,6E10.1,2X,I3)
! 1011 FORMAT(/' DATA BASE ERROR, ALL POLLUTANTS NOT FOUND')
! 1012 FORMAT(/' DATA BASE ERROR - INPUT READ ERROR, NP =',I4)
! 1013 FORMAT(/' DATA BASE ERROR - END OF FILE ON UNIT',I3,' NP =',I4)
! 1014 FORMAT(/' DATA BASE ERROR - NO POLLUTANTS SPECIFIED, NPDB =',I5)
! 1015 FORMAT(/,'POLLUTANT NAME       ID/CAS NO.  TYPE    MOL. WT.  '    &
!        ' VAP PRES'                                                     &
!       '    HENRYS C    WAT. SOL.      KOC         KOW')
! 1019 FORMAT(41X,' g/mole     mm - Hg   atm-m3/mole     mg/L        L/g',  &
!       '    DIMENSIONLESS')
!
!================= END OF MODULE PDATIN ===================================
!      END
!   HEALTH:SETFAC.FOR                      Version Date: 30-Oct-97
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                     FUNCTION   SETFAC                                      *
!                                                                            *
!  Function SETFAC determines the risk conversion factor for a path a        *
!           pollutant type                                                   *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    19-Jun-97    DL Strenge                                 *
!  Last Modified:    30-Oct-97      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: AHAZH/HEALTH
!     Called by: RISKCALC
!     Calls: NONE
!     Common blocks referenced: DEVICE
!
!==== Significant Parameter Designation and Description ======================
!
! Parameter Set/
!   Name    Used Type Location  Parameter Description
!  ------  ----- ---- --------  -------------------------------------
!  KTYPE     U   Int  Argument  index of pollutant type (1-5)
!  INTAKE    U   Int  Argument  routte index (1-4)
!  OTYPE     U   INT  Argument  index of endpoint type to calculate
!  RTYPE     U   INT  Argument  radionuclide input type (1 - Bq, 2 - Sv)
!==== Modification History ===================================================
!     Date    Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  19-Jun-97  DLS  Initial programming for AHAZH
!  07-Aug-97  DLS  Added OTYPE to call list
!  30-Oct-97  DLS  Fixed use of dose factors for dermal pathways
!==== SUBROUTINE CALL ========================================================
!
      REAL FUNCTION SETFAC(KTYPE,INTAKE,IPATH,OTYPE,RTYPE)
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      USE Con_Data_Mod
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'PSET1.FTN'
      INCLUDE 'PSET2.FTN'
      INCLUDE 'RISKCM.FTN'
      INCLUDE 'TIMES.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      INTEGER KTYPE,IPATH,OTYPE,RTYPE
      INTEGER, DIMENSION(27) :: MEDSET
      INTEGER :: MEDIUM
!
!---- Data Statements --------------------------------------------------------
!
      DATA C8766/8766./   !   Conversion factor, hours per year
      DATA C1EM3/1.E-3/   !   Conversion factor, kg/g
!
!---- Set cross index for ingestion medium, water, soil, food ----------------
!
      DATA MEDSET/1,0,1,6*3,1,0,0,2,2,0,3,11*0/    ! modify to include shoreline sediment
!
!----- Initialize factor to zero ---------------------------------------------
!
      FAC = 0.0
      MEDIUM = MEDSET(IPATH)
!      WRITE(NHLS,'(A,3I3)') ' In SETFAC, INTAKE, IPATH, MEDIUM:',INTAKE, IPATH, MEDIUM
!
!----- Evaluate for radionuclides, KTYPE = 1 ---------------------------------
!
      IF(KTYPE.LE.1) THEN           ! Radionuclides
!
!----- Set health effects conversion factor for requested output, OTYPE ------
!
       HEFAC = 0.01             ! Sv/rem
       IF(IHEAST.NE.1.OR.INTAKE.EQ.3) THEN  ! use ICRP radiation dose factors
!                                           ! and health effects converion
!                                           ! factors if requested and for Dermal
!                                           ! exposure pathways
        IF(OTYPE.EQ.1) THEN     
          HEFAC = HECONINC
        ELSEIF(OTYPE.EQ.2) THEN
          HEFAC = HECONFAT
        ELSEIF(OTYPE.EQ.3) THEN
          HEFAC = HECONFSH
        ELSE
          HEFAC = 0.01            ! Sv/rem
        ENDIF
!
!----- Set radionuclide input type conversion factor, pCi/Bq for intake ------
!
        PCIFAC = 1.0
        IF(RTYPE.EQ.1) PCIFAC = 27.027
!
!----- For input as intake (Bq), use dose conversion factors to get dose in rem
!
        IF(RTYPE.EQ.1) THEN
!
!----- Set dose conversion factor --------------------------------------------
!
            IF(INTAKE.EQ.1) THEN
               FAC = DFG
!      WRITE(NHLS,'(A,1p,e10.3)') ' In SETFAC, FAC = ',FAC
            ELSE IF(INTAKE.EQ.2) THEN
               FAC = DFA 
            ELSE IF (INTAKE.EQ.3) THEN
               FAC = DFS
            ELSE                      ! Set external dose factors
               IF(IPATH.EQ.20) THEN
                  FAC = DIMR * C8766          ! Swimming external
               ELSEIF(IPATH.EQ.21) THEN
                  FAC = DIMR * C8766          ! Boating external
               ELSEIF(IPATH.EQ.22) THEN
                  FAC = DSH  * C8766 * SOILD  ! Shoreline external
               ELSEIF(IPATH.EQ.23) THEN
                  FAC = DSH  * C8766 * SOILD  ! Soil external
               ELSEIF(IPATH.EQ.24) THEN
                  FAC = DEX  * C8766          ! Air external, semi-infinite plume
               ENDIF
               FAC = FAC * TEXP
            ENDIF
        ELSE
            FAC = 100.0
        ENDIF
!
!----- End of seting factors for ICRP methods, now do for HEAST slope factors
!
       ELSE
         IF(RTYPE.EQ.1) THEN  ! Only can do slope factor method for Bq intake
            HEFAC = 1.0
            PCIFAC = 27.027
            IF(INTAKE.EQ.1) THEN
!
!-----      Set radionuclide slope factor (cancer potency) factor by intake medium
!           MEDIUM = 1 for water, 2 for soil, and 3 for foods
!
               IF(MEDIUM.EQ.1) THEN
                  FAC = CPFG
               ELSEIF(MEDIUM.EQ.2) THEN
                  FAC = CPFGS
               ELSEIF(MEDIUM.EQ.3) THEN
                  FAC = CPFGF
               END IF
!      WRITE(NHLS,'(A,1p,e10.3)') ' In SETFAC, FAC = ',FAC
!
            ELSE IF(INTAKE.EQ.2) THEN
               FAC = CPFH
            ELSE IF (INTAKE.EQ.3) THEN
               FAC = CPFG / FONE
            ELSE                      ! Set external dose factors
               IF(IPATH.EQ.22) THEN
                  FAC = CPFS * C1EM3   ! Shorline external
               ELSEIF(IPATH.EQ.23) THEN
                  FAC = CPFS * C1EM3   ! Soil external
               ENDIF
               FAC = FAC * TEXP
            ENDIF
         ELSE
            FAC = 0.0
         ENDIF
         IF(OTYPE.EQ.4) THEN
            HEFAC = 0.01            ! Sv/rem
            PCIFAC = 1.0
            IF(RTYPE.EQ.1) PCIFAC = 27.027
!
!----- Set dose conversion factor --------------------------------------------
!
               IF(INTAKE.EQ.1) THEN
!
!-----      Set dose conversion factor by intake medium ----------------------
!           MEDIUM = 1 for water, 2 for soil, and 3 for foods
!
!               IF(MEDIUM.EQ.1) THEN
                  FAC = DFG
!               ELSEIF(MEDIUM.EQ.2) THEN
!                  FAC = DFGS
!               ELSEIF(MEDIUM.EQ.3) THEN
!                  FAC = DFGF
!               END IF
!
               ELSE IF(INTAKE.EQ.2) THEN
                  FAC = DFA 
               ELSE IF (INTAKE.EQ.3) THEN
                  FAC = DFS
               ELSE                      ! Set external dose factors
                  IF(IPATH.EQ.20) THEN
                     FAC = DIMR * C8766          ! Swimming external
                  ELSEIF(IPATH.EQ.21) THEN
                     FAC = DIMR * C8766          ! Boating external
                  ELSEIF(IPATH.EQ.22) THEN
                     FAC = DSH  * C8766 * SOILD  ! Shoreline external
                  ELSEIF(IPATH.EQ.23) THEN
                     FAC = DSH  * C8766 * SOILD  ! Soil external
                  ELSEIF(IPATH.EQ.24) THEN
                     FAC = DEX  * C8766          ! Air external, semi-infinite plume
                  ENDIF
                  FAC = FAC * TEXP
               ENDIF
         ENDIF
       ENDIF
        FAC = FAC * HEFAC * PCIFAC
!
!---- End of analysis for radionuclides
!
      ELSE                       ! Chemicals
        IF(INTAKE.EQ.1) THEN        ! Ingestion intake
           IF(OTYPE.EQ.1) THEN   ! Carcinogen by ingestion
              FAC = CPFG      !SLOPE FACTOR ING
               IF(MEDIUM.EQ.1) THEN
                  FAC = CPFG
               ELSEIF(MEDIUM.EQ.2) THEN
                  FAC = CPFGS
               ELSEIF(MEDIUM.EQ.3) THEN
                  FAC = CPFGF
               END IF
!      WRITE(NHLS,'(A,1p,e10.3)') ' In SETFAC, chemical carcinogen FAC = ',FAC
!
           ELSEIF(OTYPE.EQ.2) THEN    ! Non-carciongen by ingestion
               IF(MEDIUM.EQ.1) THEN
                  IF(RFDG.GT.0.) FAC = 1./RFDG     !RFD ING water
               ELSEIF(MEDIUM.EQ.2) THEN
                  IF(RFDGS.GT.0.) FAC = 1./RFDGS   !RFD ING soil
               ELSEIF(MEDIUM.EQ.3) THEN
                  IF(RFDGF.GT.0.) FAC = 1./RFDGF   !RFD ING food
               END IF
!      WRITE(NHLS,'(A,1p,e10.3)') ' In SETFAC, chemical non-carcinogen FAC = ',FAC
!
           ENDIF
        ELSE IF(INTAKE.EQ.2) THEN   ! Inhalation intake
           IF(OTYPE.EQ.1.AND..NOT.SKIP_INHALE) THEN        ! Carcinogen by inhalation
              FAC = CPFH           !SLOPE FACTOR INH
           ELSEIF(OTYPE.EQ.2.AND..NOT.SKIP_INHALE) THEN    ! Non-carciongen by inhalation
              IF(RFDH.GT.0.) FAC = 1./RFDH        !RFD INH
           ENDIF
        ELSE IF(INTAKE.EQ.3) THEN   ! Dermal intake
           IF(OTYPE.EQ.1) THEN        ! Carcinogen by ingestion
              FAC = CPFG/FONE   !SLOPE FACTOR ING
           ELSEIF(OTYPE.EQ.2) THEN    ! Non-carciongen by ingestion
              IF(RFDG.GT.0.)  FAC = 1./(RFDG*FONE)   !RFD ING
           ENDIF
        ENDIF
      ENDIF
      SETFAC = FAC
      RETURN
!
!----- END OF MODULE SETFAC -------------------------------------------------
!
      END
!
!   MEPAS AHAZ: GETNAM.FOR             Version Date: 05-Aug-97
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
!  Last Modified:    05-Aug-1997   DLS                                       *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZI
!     Called by: AHAZI
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
!   Date      Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  28-JUL-92  DLS  Modification of heading information
!  10-DEC-93  DLS  Modification to read prefix for *.SIF file (rectyp 10)
!  23-Jun-97  DLS  Modified to read FRAMES arguments.
!  05-Aug-97  DLS  Modified for use in Health Impacts component
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETNAM()
!
!==== COMMON Block Definitions ===============================================
!
        include 'DEVICE.FTN'
        include 'COUPLE.FTN'
!
!==== DIMENSION Statements ===================================================
!
!     NONE
!
!==== Variable Declarations ==================================================
!
!==== DATA Statements ========================================================
!
!     NONE
!
      OPEN (NTMP, FILE='FACIL.ID', STATUS='OLD')
!
!  Find input record with RECTYP = 1: Name for FUIname
!
   10 READ (NTMP,1003) RECIN
      READ (RECIN,1004) RECTYP
      IF (RECTYP.NE.'1') GOTO 10
      READ (RECIN,1005) GIDFNM
      REWIND (NTMP)    
!
!  Find input record with RECTYP = 2: Name for HINFNM
!
   11 READ (NTMP,1003) RECIN
      READ (RECIN,1004) RECTYP
      IF (RECTYP.NE.'2') GOTO 11
      READ (RECIN,1005) HINFNM
      REWIND (NTMP)    
!
!  Find input record with RECTYP = 3: Number of Site. SITNUM
!
   12 READ (NTMP,1003) RECIN
      READ (RECIN,1004) RECTYP
      IF (RECTYP.NE.'3') GOTO 12
      READ (RECIN,1001) SITNUM
      REWIND (NTMP)    
!
!  Find input record with RECTYP = 4: Number of Site. HINUM
!
   13 READ (NTMP,1003) RECIN
      READ (RECIN,1004) RECTYP
      IF (RECTYP.NE.'4') GOTO 13
      READ (RECIN,1001) HINUM
      REWIND (NTMP)    
!
!  Find input record with RECTYP = 5: Name for NAME
!
   14 READ (NTMP,1003) RECIN
      READ (RECIN,1004) RECTYP
      IF (RECTYP.NE.'5') GOTO 14
      READ (RECIN,1005) NAME  
      REWIND (NTMP)    
 1001 FORMAT (3X,I3)
 1002 FORMAT (3X,A8,2X,A8,2X,A8,2X,A8)
 1003 FORMAT (A128)
 1004 FORMAT (1X,A1)
 1005 FORMAT (3X,A128)
      CLOSE (NTMP)
      RETURN
      END
!
!====================== END OF MODULE GETNAM =============================
!
! RISKCALC.FOR AHAZH                Version Date: 03-Apr-06
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE RISKCALC                              *
!                                                                            *
!  Subroutine RISKCALC tests exposure pathway name and route, and if found,  *
!       calculates risks from the intake amounts                             *
!                                                                            *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    19-Jun-97  DL Strenge                                   *
!  Last Modified:    03-Apr-06  MAS                                          *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZH
!     Called by: ANAZH
!     Calls: NONE
!     Common blocks referenced: NONE
!
!==== Significant Parameter Designation and Description ======================
!
!  Parameter  Set/
!   Name      Used  Type  Location  Parameter Description
!  --------- ----- ------ --------- -------------------------------------
!  EXNAME      U    CHAR  Argument  Exposure pathway medium name
!  EXROUTE     U    CHAR  Argument  Exposure pathway route name
!  EU          U    CHAR  Argument  Exposure pathway units
!  TPATH       U    INT   Argument  Transport pathway index  (1-GW, 2-SW,
!                                   3-Air, 5-meas. soil, 6-meas. food
!  NPOINTS     U    INT   Argument  Number of location (points) to evaluate
!  OTYPE       U    INT   Argument  Output type to be evaluated
!  RTYPE       U    INT   Argument
!  VALIN       U    REAL  Argument  Input "intakes" for each point
!  VALOUT      S    REAL  Argument  Calculated output results for each point
!  FOUNDE      S    LOG   Argument  Flag to indicate exposure pathway was found
!==== Modification History ===================================================
!    Date     Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  07-Jan-97  DLS  Initial programming started
!  07-Aug-97  DLS  Revised to calculate for specific endpoints, OTYPE
!  03-Apr-06  MAS  Added reporting threshold limits CHEMLIM, RADLIM
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE RISKCALC(EXNAME,EXROUTE,EU,TPATH,NPOINTS,OTYPE,  &
                          RTYPE,VALIN,VALOUT,FOUNDE)
!
!---- Include statements -----------------------------------------------------
!
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'PSET1.FTN'
      INCLUDE 'PFLAGS.FTN'
!     INCLUDE 'DECAY.FTN'
      INCLUDE 'GWPATH.FTN'
      INCLUDE 'SWPATH.FTN'
      INCLUDE 'SLPATH.FTN'
      INCLUDE 'ATPATH.FTN'
      INCLUDE 'FDPATH.FTN'
      INCLUDE 'RISKCM.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      REAL VALIN(360), VALOUT(360)
      LOGICAL FOUNDE
      INTEGER NPOINTS,TPATH, OTYPE, RTYPE
      CHARACTER*7 EU
      CHARACTER*12 EXPRUT(27), EXROUTE, ROUTYP(4)
      CHARACTER*20 EXPLAB(27), EXNAME
      LOGICAL SEQI
!
!---- Data Statements --------------------------------------------------------
!
      DATA EXPLAB/'Water               ',     &
                  'Shower              ',     &
                  'Shower              ',     &
                  'Leafy vegetables    ',     &
                  'Other vegetables    ',     &
                  'Meat                ',     &
                  'Milk                ',     &
                  'Fish                ',     &
                  'Crustacea           ',     &
                  'Swimming            ',     &
                  'Swimming            ',     &
                  'Shoreline           ',     &
                  'Shoreline           ',     &
                  'Soil                ',     &
                  'Soil                ',     &
                  'Food                ',     &
                  'Shower              ',     &
                  'Air                 ',     &
                  'Soil                ',     &
                  'Swimming            ',     &
                  'Boating             ',     &
                  'Shoreline           ',     &
                  'Soil                ',     &
                  'Air                 ',     &
                  'Indoor air          ',     &
                  'Ground              ',     &
                  'Ground              '/
      DATA ROUTYP/'ingestion   ',    &
                  'inhalation  ',    &
                  'dermal      ',    &
                  'external    '/
      DATA EXPRUT/'ingestion   ',     &   ! 1
                  'dermal      ',     &   ! 0
                  'ingestion   ',     &   ! 1
                  'ingestion   ',     &   ! 3
                  'ingestion   ',     &   ! 3
                  'ingestion   ',     &   ! 3
                  'ingestion   ',     &   ! 3
                  'ingestion   ',     &   ! 3
                  'ingestion   ',     &   ! 3
                  'ingestion   ',     &   ! 1
                  'dermal      ',     &   ! 0
                  'dermal      ',     &   ! 0
                  'ingestion   ',     &   ! 2
                  'ingestion   ',     &   ! 2
                  'dermal      ',     &   ! 0
                  'ingestion   ',     &   ! 3
                  'inhalation  ',     &   ! 0
                  'inhalation  ',     &   ! 0
                  'inhalation  ',     &   ! 0
                  'external    ',     &   ! 0
                  'external    ',     &   ! 0
                  'external    ',     &   ! 0
                  'external    ',     &   ! 0
                  'external    ',     &   ! 0
                  'inhalation  ',     &   ! 0
                  'external    ',     &   ! 0
                  'dermal      '/         ! 0
!
!---- Find exposure name in list of known pathways ---------------------------
!
      FOUNDE = .FALSE.
      DO IP = 1,27
         IF(SEQI(EXNAME,EXPLAB(IP),20)) THEN
            IF(SEQI(EXROUTE,EXPRUT(IP),12)) THEN
!         IF(EXNAME.EQ.EXPLAB(IP)) THEN
!            IF(EXROUTE.EQ.EXPRUT(IP)) THEN
                IPATH = IP
!
!----- Find exposure route in list of known routes ---------------------------
!
              DO IPP = 1,4
                IF(SEQI(EXROUTE,ROUTYP(IPP),12)) THEN
!                 IF(EXROUTE.EQ.ROUTYP(IP)) THEN
                   FOUNDE = .TRUE.
                   INTAKE = IPP
                ENDIF
              END DO
!
            ENDIF
         ENDIF
      END DO
      IF(IPATH.EQ.26) IPATH = 23
      IF(IPATH.EQ.27) IPATH = 15
      KTYPE = NTYPE
!
!---- If pathway found, set pathway cross index for SIF factors --------------
!
      IF(FOUNDE) THEN
!      WRITE(NHLS,'(A/I5,I5,I5,I5,I5)') ' Found pathway, KTYPE, INTAKE, IPATH, OTYPE, RTYPE ',KTYPE,INTAKE,IPATH,OTYPE,RTYPE
         FAC = 1.0
         IF(TPATH.EQ.1) THEN   !  Set groundwater index
              FAC = SETFAC(KTYPE,INTAKE,IPATH,OTYPE,RTYPE)
         ELSE IF(TPATH.EQ.2) THEN   ! Set surface water index
              FAC = SETFAC(KTYPE,INTAKE,IPATH,OTYPE,RTYPE)
         ELSE IF(TPATH.EQ.3) THEN   ! Set atmospheric index
              FAC = SETFAC(KTYPE,INTAKE,IPATH,OTYPE,RTYPE)
         ELSE IF(TPATH.EQ.5) THEN   ! Set measured soil index
              FAC = SETFAC(KTYPE,INTAKE,IPATH,OTYPE,RTYPE)
         ENDIF
      ELSE
        GO TO 999
      ENDIF
!
!---- Calculate health inpacts for current constituent type and pathway -----
!
999   IF(.NOT.FOUNDE) THEN
        DO IPN = 1,NPOINTS
           VALOUT(IPN) = 0.0
        END DO
      ELSE
        DO IPN = 1,NPOINTS
           VALOUT(IPN) = VALIN(IPN) * FAC
           IF(KTYPE.GT.1.AND.OTYPE.EQ.1) THEN              ! Carcinogen
             IF(VALOUT(IPN).LT.CHEMLIM) VALOUT(IPN) = 0.0  ! reporting threshold limit
           ELSE IF(KTYPE.GT.1.AND.OTYPE.EQ.2) THEN         ! Non-carcinogen
             IF(VALOUT(IPN).LT.RFDLIM) VALOUT(IPN) = 0.0   ! reporting threshold limit
           ELSE IF(KTYPE.EQ.1.AND.OTYPE.EQ.1) THEN         ! Radiation cancer incidence
             IF(VALOUT(IPN).LT.RADLIM) VALOUT(IPN) = 0.0   ! reporting threshold limit
           ELSE IF(KTYPE.EQ.1.AND.OTYPE.EQ.2) THEN         ! Radiation total fatalities
             IF(VALOUT(IPN).LT.RADLIM) VALOUT(IPN) = 0.0   ! reporting threshold limit
           ELSE IF(KTYPE.EQ.1.AND.OTYPE.EQ.3) THEN         ! Radiation cancer and severe hereditary
             IF(VALOUT(IPN).LT.RADLIM) VALOUT(IPN) = 0.0   ! effects reporting threshold limit
           ENDIF
        END DO
      ENDIF
      RETURN
!
!----- END OF MODULE RISKCALC -----------------------------------------------
!
      END
!
!   MEPAS AHAZH GETDATA.FOR            Version Date: 28-Mar-06
!   Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                           SUBROUTINE GETDATA                               *
!                                                                            *
!  Subroutine GETDATA reads data from GID run dataset                        *
!                                                                            *
!  Written by:       Dennis Strenge                                          *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    05-Aug-97                                               *
!  Last Modified:    28-Mar-06      MAS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS/AHAZH
!     Called by: AHAZH, main program
!     Calls: SUBROUTINE GET.. ROUTINES FOR READING .GID FILES
!     Common blocks referenced: PFLAGS,RISKCM
!
!==== Significant Parameter Designation and Description ======================
!
!     Parameter Set/
!     Name      Used   Type    Location  Parameter Description
!     --------- -----  ------  --------- -------------------------------------
!   KEXPTH(25)   S     INT     PFLAGS    Exposure pathway flags
!
!
!
!
!
!
!==== Modification History ===================================================
!
!   Date      Who  Modification Description
!  ---------  ---  ------------------------------------------------------
!  06-Aug-97  DLS  Initial preparation for HEALTH component
!  28-Mar-06  MAS  Added reporting threshold limits CHEMLIM, RADLIM
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE GETDATA(SETDATA,NLINES)
!
      USE Con_Data_Mod
!
!==== COMMON Block Definitions ===============================================
!
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'PFLAGS.FTN'
      INCLUDE 'RISKCM.FTN'
!
!==== DIMENSION Statements ===================================================
!
      DATA TSOIL/0.04/  ! Default soil thickness for areal to mass conversion
      DATA DSOIL/1.5/   ! Default soil density for areal to mass conversion
!
!==== Variable Declarations ==================================================
!
      CHARACTER*(*) SETDATA(LINEMAX)
      INTEGER GETINT
      LOGICAL GETLOG, CETOXT
      REAL GETREAL
!
!==== DATA Statements ========================================================
!
      DATA IZ/0/
!
!---- Get Chemical health impact parameters ----------------------------------
!
      CHEMRISK = GETLOG(SETDATA,NLINES,'CHEMRISK      ',IZ,IZ,IZ,IZ,IZ,IZ)
      CHEMHI =   GETLOG(SETDATA,NLINES,'CHEMHI        ',IZ,IZ,IZ,IZ,IZ,IZ)
      VAL =      GETREAL(SETDATA,NLINES,'RFDLIM        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) RFDLIM = VAL
      VAL =      GETREAL(SETDATA,NLINES,'CHEMLIM       ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) CHEMLIM = VAL
      CETOXT = GETLOG(SETDATA,NLINES,'CETOXT        ',IZ,IZ,IZ,IZ,IZ,IZ)
      IRIS = 0
      IF(CETOXT) IRIS = 2
!
!----- Get radionuclide health impact parameters -----------------------------
!
      IHEAST = 0
      IVAL = GETINT(SETDATA,NLINES,'IHEAST        ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(IVAL.GT.0) IHEAST = IVAL
      HEINC = GETLOG(SETDATA,NLINES,'HEINC         ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(HEINC) THEN
        VAL = GETREAL(SETDATA,NLINES,'HECONINC      ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0) HECONINC = VAL * 0.01  ! Convert to risk/rem from risk/Sv
      ENDIF
      HEFAT = GETLOG(SETDATA,NLINES,'HEFAT         ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(HEFAT) THEN
        VAL = GETREAL(SETDATA,NLINES,'HECONFAT      ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0) HECONFAT = VAL * 0.01  ! Convert to risk/rem from risk/Sv
      ENDIF
      HEFSH = GETLOG(SETDATA,NLINES,'HEFSH         ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(HEFSH) THEN
        VAL = GETREAL(SETDATA,NLINES,'HECONFSH      ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0) HECONFSH = VAL * 0.01  ! Convert to risk/rem from risk/Sv
      ENDIF
      HECEDE= GETLOG(SETDATA,NLINES,'HECEDE        ',IZ,IZ,IZ,IZ,IZ,IZ)
      INHALE = GETINT(SETDATA,NLINES,'INHALE        ',IZ,IZ,IZ,IZ,IZ,IZ)
      WRITE(nhls,'(/a/a)') ' When necessary, conversion from reference dose to reference concentration is performed',&
      '  using a multiplication factor of (70 kg)/(20 m3/day) = 3.5'
      WRITE(nhls,'(a/a)') ' When necessary, conversion from slope factor to unit risk factor for inhalation is ',&
      '  performed using a multiplication factor of (20 m3/day)/(70 kg) = 0.285714'
      IF(INHALE.EQ.0) THEN
         WRITE(nhls,'(a)') ' Inhalation impacts calculated using air concentrations'
      ELSE
         WRITE(nhls,'(/a)') ' Inhalation impacts calculated using average daily intake'
      ENDIF
      VAL =      GETREAL(SETDATA,NLINES,'RADLIM        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) RADLIM = VAL
!
!----- If HEAST slope factors are to be used for external ground exposure ----
!      then it is necessary to read the soil thickness and density to
!      convert from areal density to mass density for use of slope factor
!
      IF(IHEAST.EQ.1) THEN
!
!----- When HEAST slope factors are to be used for radionuclides, then -------
!      only cancer incidence or radiation dose can be calculated
!
        HEFAT = .FALSE.
        HEFSH = .FALSE.
      ENDIF
!  When dose factors are to be used, convert to m2 basis
!  Always read soil properties because may be needed.
!
        VAL = GETREAL(SETDATA,NLINES,'TSOIL         ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) TSOIL = VAL
        VAL = GETREAL(SETDATA,NLINES,'DSOIL         ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) DSOIL = VAL
        SOILD = 1.E3* TSOIL * DSOIL     ! Mass to areal conversion, kg/m2
      RETURN
!
!================ END OF MODULE GETDATA ====================================
!
      END
!
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
      CHARACTER*(*) SETDATA(linemax)
      CHARACTER*120 SETLINE
      CHARACTER*(*) NAME
      CHARACTER*14 PNAM
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
!        WRITE(NHLS,'(A,A,I5)') ' In GETSET, found set, name, nlines = ',NAME,nlines

        IF(NLINES.LE.0.OR.NLINES.GT.linemax) THEN   ! Test NLINES for range
          WRITE(NERR,100) NLINES,linemax
!          WRITE(NHLS,100) NLINES,linemax
 100 FORMAT(' Error in GID spec. of NLINES: ',I5,' Max. is ',I5)
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
!        WRITE(NHLS,105) NAME
 105    FORMAT(' Error in GID read.  Set name not found. ',A14)
      ENDIF
      PNAM = PNAM
      RETURN
 300  WRITE(NERR,101)ILN
!     WRITE(NHLS,101)ILN
 101  FORMAT(' Error in GID set read at line ILN: ',I5)
      RETURN
 400  WRITE(NERR,102) ILN, NLINES
!      WRITE(NHLS,102) ILN, NLINES
 102  FORMAT(' Error in GID set read. EOF after reading ',I4,' of',I4,' lines')
      RETURN
!----- END OF MODULE GETSET -------------------------------------------------
      END
!
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
      CHARACTER*(*) NAME
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
        GETINT=VALUE
      ELSE
        GETINT=0
      END IF
      END
!
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
!
! GETSTR.FOR                         Version Date: 29-Oct-96
!  Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                           *
!                          SUBROUTINE GETSTR                                *
!                                                                           *
! Subroutine GETSTR extracts a string parameter value from the .GID file.   *
!                                                                           *
! Written by:       Karl Castleon                                           *
!                   Battelle Pacific Northwest Laboratories                 *
!                   P.O. Box 999                                            *
!                   Richland, WA 99352                                      *
!                                                                           *
! Creation Date:    16-Nov-95                                               *
! Last Modified:    29-Oct-96      DLS                                      *
!                                                                           *
! ***************************************************************************
!
!=== Modular Organization ===================================================
!
!    Module of: GENERAL USE
!    Called by:
!    Calls: MOVETO, SEQ
!    Common blocks referenced: NONE
!
!=== Significant Parameter Designation and Description ======================
!
!    Parameter Set/
!    Name      Used   Type    Location  Parameter Description
!    --------- -----  ------  --------- -------------------------------------
!   SETDATA     U      CHAR   Argument  Array of line images to search for an
!                                       integer parameter
!   NAME        U      CHAR   Argument  Name of parameter sought.
!   C1          U      INT    Argument  CONSTITUENT COUNTER
!   C2          U      INT    Argument  MEDIA COUNTER FOR GROUND WATER PSZ AND SZ
!   C3          U      INT    Argument  LOCATION COUNTER
!   C4          U      INT    Argument  FLUX COUNTER
!   C5          U      INT    Argument  MONTH COUNTER
!   C6          U      INT    Argument  MISCELANEOUS COUNTER
!=== Modification History ===================================================
!
!    Date         Who  Modification Description
!    --------     ---  ------------------------------------------------------
!    06-Dec-95    DLS  Added heading information
!    29-Oct-96    DLS  Converted for use with the Framework program
!=== SUBROUTINE CALL ========================================================
!
      CHARACTER*80 FUNCTION GETSTR(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES,LINE
      CHARACTER*(*) NAME
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
!
! GETLOG.FOR                       Version Date: 12-Nov-96
!  Copyright 1989 by Battelle Memorial Institute.  All rights reserved.
! ***************************************************************************
!                                                                           *
!                          SUBROUTINE GETLOG                                *
!                                                                           *
! Subroutine GETLOG extracts a logical  value from the .GID file            *
!                                                                           *
! Written by:       Karl Castleton/DL Strenge                               *
!                   Battelle Pacific Northwest Laboratories                 *
!                   P.O. Box 999                                            *
!                   Richland, WA 99352                                      *
!                                                                           *
! Creation Date:    16-Nov-93                                               *
! Last Modified:    12-Nov-96 DLS Generated from GETINT                     *
!                                                                           *
! ***************************************************************************
!
!=== Modular Organization ===================================================
!
!    Module of: general
!    Called by:
!    Calls: MOVETO
!    Common blocks referenced: NONE
!
!=== Significant Parameter Designation and Description ======================
!
!    Parameter Set/
!    Name      Used   Type    Location  Parameter Description
!    --------- -----  ------  --------- -------------------------------------
!    SETDATA    U      CHAR   Argument  Array of line images for extraction
!                                       of a logical parameter
!    ***THE ARRAY MUST ALREADY BE FILED***
!    NAME       U      CHAR   Argument  Name of parameter sought
!    C1         U      INT    Argument  first counter
!    C2         U      INT    Argument  second counter
!    C3         U      INT    Argument  third counter
!    C4         U      INT    Argument  fourth counter
!    C5         U      INT    Argument  fifth counter
!    C6         U      INT    Argument  sixth counter
!=== Modification History ===================================================
!
!    Date         Who  Modification Description
!    --------     ---  ------------------------------------------------------
!    06-Dec-95    DLS  Added heading information
!    12-Nov-96    DLS  Converted for reading a logical parameter value
!=== SUBROUTINE CALL ========================================================
!
      LOGICAL FUNCTION GETLOG(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6)
!
!---- Variable Type Declarations ---------------------------------------------
!
      include 'parmtr.par'
      INTEGER C1,C2,C3,C4,C5,C6,NLINES
      CHARACTER*(*) NAME
      CHARACTER*(*) SETDATA(linemax)
      LOGICAL MOVETO
      INTEGER T1,T2,T3,T4,T5,T6,REF
      CHARACTER UNITS*8,USUNTS*8,TNAME*14
      LOGICAL VALUE
!
!---- Start of Analysis
!    MOVETO finds the record having the desired parameter value
!
      IF (MOVETO(SETDATA,NLINES,NAME,C1,C2,C3,C4,C5,C6,LINE)) THEN
        READ(SETDATA(LINE),*) TNAME,T1,T2,T3,T4,T5,T6,REF,USUNTS,UNITS,VALUE
        GETLOG=VALUE
      ELSE
        GETLOG=.FALSE.
      END IF
      END
!
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
      INTEGER :: N5
      DATA N14/14/
      DATA n5/5/
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
!      WRITE(*,'(A,A)') ' In MOVESET, Looking for SET NAME : ',NAME
 300  READ (NGID,*,ERR=303,END=303) TNAME,NLINES
!
!---- Test name and index values against values sought -----------------------
!
!      WRITE(*,'(A,A)') ' In MOVESET, GID SET NAME IS: ',TNAME
!      IF (.NOT. SEQI(TNAME,NAME,N14))  THEN
      IF (.NOT. SEQI(TNAME,NAME,N5))  THEN
        DO IL = 1,NLINES                        ! New do loop to read
          READ(NGID,*,ERR=303,END=303) TNAME    ! NLINES skipping past
        END DO                                  ! unneeded data set
        GO TO 300
      ELSE !
        MOVESET=.TRUE.
      END IF
 303  END
!
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
!
!   INTAKE:READ_CON_DATA                   Version Date: 30-Apr-2001
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                     SUBROUTINE READ_CON_DATA                               *
!                                                                            *
!  Subroutine READ_CON_DATA reads constituent data from the FUI section of   *
!  the GID file for all constitents selected for this analysis               *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    30-Apr-01                                               *
!  Last Modified:    30-Apr-01      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS
!     Called by: AHAZH
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
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE READ_CON_DATA(SETDATA,NLINES,IERR)
!
      USE Errors_Mod
      USE Con_Data_Mod
!
      IMPLICIT NONE
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      INCLUDE 'PARMTR.PAR'
      include 'COUPLE.FTN'
      INCLUDE 'DEVICE.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER(LEN=13) :: CALLER = 'READ_CON_DATA'  ! Name of this routine
      CHARACTER*(*) :: SETDATA
      INTEGER :: NLINES,IZ,IS,IH,IERR,IC,IOS,IP
      INTEGER :: GETINT
      INTEGER :: KTYPE
      REAL GETREAL
      REAL(KIND=4) :: CURF, CRFC, VAL
      CHARACTER*32 GETSTR
      CHARACTER(LEN=32), DIMENSION(5) :: TYPOUT
      !
!---- Data Statements --------------------------------------------------------
!
      DATA IZ/0/
      DATA  TYPOUT/'Radionuclide                    ',&
                   'Carcinogen                      ',&
                   'Carcinogen (Inh) Noncarc (Ing)  ',&
                   'Carcinogen (Ing) Noncarc (Inh)  ',&
                   'Noncarcinogen                   '/
!
!---- Start of analysis ------------------------------------------------------
!
      IERR = 0
      IS = SITNUM
      IH = HINUM
!
!---- Get number of constituents selected for analysis
!
!      writE(nhls,'(a,I8)') ' In Read_Con_data: number of lines in data set is: ',NLINES
      NUMCON = GETINT(SETDATA,NLINES,'NUMCON        ',IS,IZ,IZ,IZ,IZ,IZ)
!
!      writE(nhls,'(a,I8)') ' Number of constituents read from GID is: ',NUMCON
      IF(NUMCON.LE.0) THEN
          IERR = 1
          MESSAG(1) ='Error in number of constituents read from GID file'
          MESSAG(2) ='Must be greater than zero'
          CALL PRTERR(IERR,CALLER,2)
          GO TO 9999
      ELSE         ! Allocate space for constituent parameter arrays

        IF( ALLOCATED(CASID) ) DEALLOCATE( CASID )
        ALLOCATE( CASID(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating CASID(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RNPRG) ) DEALLOCATE( RNPRG )
        ALLOCATE( RNPRG(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RNPRG(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(MTYPE) ) DEALLOCATE( MTYPE )
        ALLOCATE( MTYPE(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating MTYPE(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
!
        IF( ALLOCATED(RDFG) ) DEALLOCATE( RDFG )
        ALLOCATE( RDFG(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RDFG(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RDFH) ) DEALLOCATE( RDFH )
        ALLOCATE( RDFH(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RDFH(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RDFS) ) DEALLOCATE( RDFS )
        ALLOCATE( RDFS(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RDFS(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RDEX) ) DEALLOCATE( RDEX )
        ALLOCATE( RDEX(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RDEX(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RDSH) ) DEALLOCATE( RDSH )
        ALLOCATE( RDSH(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RDSH(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RDIMR) ) DEALLOCATE( RDIMR )
        ALLOCATE( RDIMR(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RDIMR(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RSFH) ) DEALLOCATE( RSFH )
        ALLOCATE( RSFH(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RSFH(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RSFG) ) DEALLOCATE( RSFG )
        ALLOCATE( RSFG(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RSFG(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RSFGS) ) DEALLOCATE( RSFGS )
        ALLOCATE( RSFGS(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RSFGS(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RSFGF) ) DEALLOCATE( RSFGF )
        ALLOCATE( RSFGF(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RSFGF(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RSFS) ) DEALLOCATE( RSFS )
        ALLOCATE( RSFS(Numcon,20), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RSFS(NUMCON,20) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
!   Chemical variables
        IF( ALLOCATED(RFONE) ) DEALLOCATE( RFONE )
        ALLOCATE( RFONE(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RFONE(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RCPFH) ) DEALLOCATE( RCPFH )
        ALLOCATE( RCPFH(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RCPFH(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RCPFG) ) DEALLOCATE( RCPFG )
        ALLOCATE( RCPFG(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RCPFG(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RCPFGS) ) DEALLOCATE( RCPFGS )
        ALLOCATE( RCPFGS(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RCPFGS(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RCPFGF) ) DEALLOCATE( RCPFGF )
        ALLOCATE( RCPFGF(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RCPFGF(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RURFH) ) DEALLOCATE( RURFH )
        ALLOCATE( RURFH(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RURFH(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RRFDH) ) DEALLOCATE( RRFDH )
        ALLOCATE( RRFDH(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RRFDH(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RRFDG) ) DEALLOCATE( RRFDG )
        ALLOCATE( RRFDG(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RRFDG(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RRFDGS) ) DEALLOCATE( RRFDGS )
        ALLOCATE( RRFDGS(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RRFDGS(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RRFDGF) ) DEALLOCATE( RRFDGF )
        ALLOCATE( RRFDGF(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RRFDGF(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
        IF( ALLOCATED(RRFCH) ) DEALLOCATE( RRFCH )
        ALLOCATE( RRFCH(Numcon), STAT= IOS )
        IF( IOS .NE. 0 ) THEN
           IERR = 3
           MESSAG(1) ='Error allocating RRFCH(NUMCON) data array, IOS =   '
           WRITE(MESSAG(1)(59:),'(I3)')IOS
           CALL PRTERR( IERR, CALLER, 1 )
           GO TO 9999
        END IF
      ENDIF
!
!---- Read parameter values for each constituent from the FUI section of GID
!
!    Loop on constituents
!
     CRFC = 70./20.
     CURF = 20./70.          ! conversion from cancer potency to unit risk
     DO IC = 1,NUMCON

       KTYPE = GETINT(SETDATA,NLINES,'CLKTYPE       ',IS,IC,IZ,IZ,IZ,IZ)
       MTYPE(IC) = 1                 ! Radionuclides
       IF(KTYPE.LT.1) MTYPE(IC) = 2  ! Chemicals
       RFONE(IC) = 1.0
       VAL = GETREAL(SETDATA,NLINES,'CLFONEI       ',IS,IC,IZ,IZ,IZ,IZ)
       IF(VAL.GT.0.) RFONE(IC) = VAL
       CASID(IC,1) = GETSTR(SETDATA,NLINES,'FSCASID       ',IS,IC,IZ,IZ,IZ,IZ)
       WRITE(NHLS,1000) TRIM(CASID(IC,1)),TRIM(typout(MTYPE(IC))),RFONE(IC)
 1000  FORMAT(/' Constituent ID               ',a/ &
              '   Type                       ',a/&
              '   GI absorption fraction     ',F8.3)
       IF(MTYPE(IC).LT.0.OR.MTYPE(IC).GT.5) THEN
           IERR = 1
           MESSAG(1) = 'Error in constituent type, out of 1 to 5 range'
           WRITE(MESSAG(2),'(A,I4)') 'Found ',MTYPE(IC)
           CALL PRTERR(IERR,CALLER,2)
           GO TO 9999
       ELSE IF(MTYPE(IC).EQ.1) THEN
!   Radionuclide variables
          RDFG(IC,1) = GETREAL(SETDATA,NLINES,'CLRDFGI       ',IS,IC,IZ,IZ,IZ,IZ)
          RDFH(IC,1) = GETREAL(SETDATA,NLINES,'CLDFAD        ',IS,IC,IZ,IZ,IZ,IZ)
          RDFS(IC,1) = GETREAL(SETDATA,NLINES,'CLRDFS        ',IS,IC,IZ,IZ,IZ,IZ)
          RDEX(IC,1) = GETREAL(SETDATA,NLINES,'CLDEX         ',IS,IC,IZ,IZ,IZ,IZ)
          RDSH(IC,1) = GETREAL(SETDATA,NLINES,'CLDSH         ',IS,IC,IZ,IZ,IZ,IZ)
          RDIMR(IC,1)= GETREAL(SETDATA,NLINES,'CLDIMR        ',IS,IC,IZ,IZ,IZ,IZ)
          RNPRG(IC)  =  GETINT(SETDATA,NLINES,'NDS           ',IS,IC,IZ,IZ,IZ,IZ)
          WRITE(NHLS,1001) RDFG(IC,1),RDFH(IC,1),RDFS(IC,1),RDEX(IC,1),RDSH(IC,1),RDIMR(IC,1),RNPRG(IC)
 1001     FORMAT('  Radiation dosimetry data    '/ &
                 '    Ingestion dose factor     ',1pE10.2,' rem/pCi'/ &
                 '    Inhalation dose factor    ',E10.2,' rem/pCi'/ &
                 '    Dermal dose factor        ',E10.2,' rem/pCi'/ &
                 '    Air external dose factor  ',E10.2,' rem/hr per pCi/m^3'/ &
                 '    Ground dose factor        ',E10.2,' rem/hr per pCi/m^2'/ &
                 '    Water dose factor         ',E10.2,' rem/hr per pCi/L'/ &
                 '    Number of progeny in chain',I5)
!
!----     Read radionuclide inhalation slope factor
!
          RSFH(IC,1) = GETREAL(SETDATA,NLINES,'CLSFH         ',IS,IC,IZ,IZ,IZ,IZ)
!
!----     Read ingestion slope factors for radionuclides by medium, get from SF fields
!
          RSFG(IC,1)  = GETREAL(SETDATA,NLINES,'CLSFG         ',IS,IC,IZ,IZ,IZ,IZ)
          RSFGS(IC,1) = GETREAL(SETDATA,NLINES,'CLSFGS        ',IS,IC,IZ,IZ,IZ,IZ)
          RSFGF(IC,1) = GETREAL(SETDATA,NLINES,'CLSFGF        ',IS,IC,IZ,IZ,IZ,IZ)
!
!----     Read dermal slope factor for radionuclides
!
          RSFS(IC,1) = GETREAL(SETDATA,NLINES,'CLRFDH        ',IS,IC,IZ,IZ,IZ,IZ)
!
!----     Test input ingestion slope factors for parent, if zero set to available values
!
          IF(RSFG(IC,1).GT.0.) THEN
             IF(RSFGS(IC,1).LE.0.) THEN
                RSFGS(IC,1) = RSFG(IC,1)
             ENDIF
             IF(RSFGF(IC,1).LE.0.) THEN
                RSFGF(IC,1) = RSFG(IC,1)
             ENDIF
          ELSEIF(RSFGS(IC,1).GT.0.) THEN
             IF(RSFG(IC,1).LE.0.) THEN
                RSFG(IC,1) = RSFGS(IC,1)
             ENDIF
             IF(RSFGF(IC,1).LE.0.) THEN
                RSFGF(IC,1) = RSFGS(IC,1)
             ENDIF
          ELSEIF(RSFGF(IC,1).GT.0.) THEN
             IF(RSFG(IC,1).LE.0.) THEN
                RSFG(IC,1) = RSFGF(IC,1)
             ENDIF
             IF(RSFGS(IC,1).LE.0.) THEN
                RSFGS(IC,1) = RSFGF(IC,1)
             ENDIF
          ENDIF
!
!----     Write  summary of radionuclide slope factors used. ------------------
!
          WRITE(NHLS,1002) RSFH(IC,1),RSFG(IC,1),RSFGS(IC,1),RSFGF(IC,1),RSFS(IC,1)
 1002     FORMAT('  Radiation slope factors     '/ &
                 '    Inhalation slope factor         ',1pE10.2,' risk/pCi'/ &
                 '    Ingestion slope factor - water  ',E10.2,' risk/pCi'/ &
                 '    Ingestion slope factor - soil   ',E10.2,' risk/pCi'/ &
                 '    Ingestion slope factor - food   ',E10.2,' risk/pCi'/ &
                 '    Ground slope factor       ',E10.2,' risk/yr per pCi/g')
          IF(RNPRG(IC).GT.0) THEN
             IF(RNPRG(IC).GT.20) THEN
                 IERR = 1
                 MESSAG(1) = 'Database error, too many progeny, max is 20'
                 MESSAG(2) = 'Problem constituent is: '//TRIM(CASID(IC,1))
                 CALL PRTERR(IERR,CALLER,2)
                 GO TO 9999
             ENDIF
             DO IP = 1,RNPRG(IC)
                CASID(IC,IP+1) = GETSTR(SETDATA,NLINES,'FSCASID       ',IS,IC,IP,IZ,IZ,IZ)
                WRITE(NHLS,1004) TRIM(CASID(IC,ip+1))
 1004     FORMAT('  Progeny name                ',a)
                RDFG(IC,IP+1) = GETREAL(SETDATA,NLINES,'CLRDFGI       ',IS,IC,IP,IZ,IZ,IZ)
                RDFH(IC,IP+1) = GETREAL(SETDATA,NLINES,'CLDFAD        ',IS,IC,IP,IZ,IZ,IZ)
                RDFS(IC,IP+1) = GETREAL(SETDATA,NLINES,'CLRDFS        ',IS,IC,IP,IZ,IZ,IZ)
                RDEX(IC,IP+1) = GETREAL(SETDATA,NLINES,'CLDEX         ',IS,IC,IP,IZ,IZ,IZ)
                RDSH(IC,IP+1) = GETREAL(SETDATA,NLINES,'CLDSH         ',IS,IC,IP,IZ,IZ,IZ)
                RDIMR(IC,IP+1)= GETREAL(SETDATA,NLINES,'CLDIMR        ',IS,IC,IP,IZ,IZ,IZ)
                RSFH(IC,IP+1) = GETREAL(SETDATA,NLINES,'CLSFH         ',IS,IC,IP,IZ,IZ,IZ)
                RSFG(IC,IP+1)  = GETREAL(SETDATA,NLINES,'CLSFG         ',IS,IC,IP,IZ,IZ,IZ) ! SF water
                RSFGS(IC,IP+1) = GETREAL(SETDATA,NLINES,'CLSFGS        ',IS,IC,IP,IZ,IZ,IZ) ! SF soil
                RSFGF(IC,IP+1) = GETREAL(SETDATA,NLINES,'CLSFGF        ',IS,IC,IP,IZ,IZ,IZ) ! SF food
                RSFS(IC,IP+1) = GETREAL(SETDATA,NLINES,'CLRFDH        ',IS,IC,IP,IZ,IZ,IZ)
!
!----     Test input ingestion slope factors for progeny, if zero set to available values
!
          IF(RSFG(IC,IP+1).GT.0.) THEN
             IF(RSFGS(IC,IP+1).LE.0.) THEN
                RSFGS(IC,IP+1) = RSFG(IC,IP+1)
             ENDIF
             IF(RSFGF(IC,IP+1).LE.0.) THEN
                RSFGF(IC,IP+1) = RSFG(IC,IP+1)
             ENDIF
          ELSEIF(RSFGS(IC,IP+1).GT.0.) THEN
             IF(RSFG(IC,IP+1).LE.0.) THEN
                RSFG(IC,IP+1) = RSFGS(IC,IP+1)
             ENDIF
             IF(RSFGF(IC,IP+1).LE.0.) THEN
                RSFGF(IC,IP+1) = RSFGS(IC,IP+1)
             ENDIF
          ELSEIF(RSFGF(IC,IP+1).GT.0.) THEN
             IF(RSFG(IC,IP+1).LE.0.) THEN
                RSFG(IC,IP+1) = RSFGF(IC,IP+1)
             ENDIF
             IF(RSFGS(IC,IP+1).LE.0.) THEN
                RSFGS(IC,IP+1) = RSFGF(IC,IP+1)
             ENDIF
          ENDIF
                WRITE(NHLS,1003) RDFG(IC,IP+1),RDFH(IC,IP+1),RDFS(IC,IP+1),RDEX(IC,IP+1),RDSH(IC,IP+1),RDIMR(IC,IP+1)
 1003     FORMAT('    Progeny dosimetry data    '/ &
                 '    Ingestion dose factor       ',1pE10.2,' rem/pCi'/ &
                 '    Inhalation dose factor      ',1pE10.2,' rem/pCi'/ &
                 '    Dermal dose factor          ',E10.2,' rem/pCi'/ &
                 '    Air external dose factor    ',E10.2,' rem/hr per pCi/m^3'/ &
                 '    Ground dose factor          ',E10.2,' rem/hr per pCi/m^2'/ &
                 '    Water dose factor           ',E10.2,' rem/hr per pCi/L')
!                WRITE(NHLS,1002) RSFH(IC,IP+1),RSFG(IC,IP+1),RSFS(IC,IP+1)
                 WRITE(NHLS,1002) RSFH(IC,IP+1),RSFG(IC,IP+1),RSFGS(IC,IP+1),RSFGF(IC,IP+1),RSFS(IC,IP+1)
             END DO
          ENDIF
       ELSE
!
!----  Read toxicity factor for chemical --------------------------------------
!      MCTYPE NOT EQUAL TO 1
          RCPFH(IC) = GETREAL(SETDATA,NLINES,'CLCPFH        ',IS,IC,IZ,IZ,IZ,IZ)  ! inhalation slope factor
!
!----     Read ingestion slope factors by intake media
!
          RCPFG(IC) =  GETREAL(SETDATA,NLINES,'CLCPFG        ',IS,IC,IZ,IZ,IZ,IZ)  ! ingestion slope factor - water
          RCPFGS(IC) = GETREAL(SETDATA,NLINES,'CLCPFGS       ',IS,IC,IZ,IZ,IZ,IZ)  ! ingestion slope factor - soil
          RCPFGF(IC) = GETREAL(SETDATA,NLINES,'CLCPFGF       ',IS,IC,IZ,IZ,IZ,IZ)  ! ingestion slope factor - food
!
!----     Test input ingestion slope factors for chemical, if zero set to available values
!
          IF(RCPFG(IC).GT.0.) THEN
             IF(RCPFGS(IC).LE.0.) THEN
                RCPFGS(IC) = RCPFG(IC)
             ENDIF
             IF(RCPFGF(IC).LE.0.) THEN
                RCPFGF(IC) = RCPFG(IC)
             ENDIF
          ELSEIF(RCPFGS(IC).GT.0.) THEN
             IF(RCPFG(IC).LE.0.) THEN
                RCPFG(IC) = RCPFGS(IC)
             ENDIF
             IF(RCPFGF(IC).LE.0.) THEN
                RCPFGF(IC) = RCPFGS(IC)
             ENDIF
          ELSEIF(RCPFGF(IC).GT.0.) THEN
             IF(RCPFG(IC).LE.0.) THEN
                RCPFG(IC) = RCPFGF(IC)
             ENDIF
             IF(RCPFGS(IC).LE.0.) THEN
                RCPFGS(IC) = RCPFGF(IC)
             ENDIF
          ENDIF
!
!----     Read inhalation unit risk factor
!
          RURFH(IC) = GETREAL(SETDATA,NLINES,'CLURISKH      ',IS,IC,IZ,IZ,IZ,IZ)  ! inhalation unit risk factor
          RURFH(IC) = RURFH(IC) * 1000.         ! convert from risk/(ug/m^3) to risk/(mg/m^3)
!
!----     Read inhalation reference dose
!
          RRFDH(IC) = GETREAL(SETDATA,NLINES,'CLRFDH        ',IS,IC,IZ,IZ,IZ,IZ)
!
!----     Read ingestion reference doses by intake media
!
          RRFDG(IC) =  GETREAL(SETDATA,NLINES,'CLRFDG        ',IS,IC,IZ,IZ,IZ,IZ)  ! ingestion RFD water
          RRFDGS(IC) = GETREAL(SETDATA,NLINES,'CLRFDGS       ',IS,IC,IZ,IZ,IZ,IZ)  ! ingestion RFD soil
          RRFDGF(IC) = GETREAL(SETDATA,NLINES,'CLRFDGF       ',IS,IC,IZ,IZ,IZ,IZ)  ! ingestion RFD food
!
!----     Test input ingestion reference dose factors for chemical, if zero set to available values
!
          IF(RRFDG(IC).GT.0.) THEN
             IF(RRFDGS(IC).LE.0.) THEN
                RRFDGS(IC) = RRFDG(IC)
             ENDIF
             IF(RRFDGF(IC).LE.0.) THEN
                RRFDGF(IC) = RRFDG(IC)
             ENDIF
          ELSEIF(RRFDGS(IC).GT.0.) THEN
             IF(RRFDG(IC).LE.0.) THEN
                RRFDG(IC) = RRFDGS(IC)
             ENDIF
             IF(RRFDGF(IC).LE.0.) THEN
                RRFDGF(IC) = RRFDGS(IC)
             ENDIF
          ELSEIF(RRFDGF(IC).GT.0.) THEN
             IF(RRFDG(IC).LE.0.) THEN
                RRFDG(IC) = RRFDGF(IC)
             ENDIF
             IF(RRFDGS(IC).LE.0.) THEN
                RRFDGS(IC) = RRFDGF(IC)
             ENDIF
          ENDIF
!
!----     Read inhalation reference concentration
!
          RRFCH(IC) = GETREAL(SETDATA,NLINES,'CLRFCH        ',IS,IC,IZ,IZ,IZ,IZ)
          IF(RRFCH(IC).LE.0.AND.RRFDH(IC).GT.0.) THEN
             RRFCH(IC) = RRFDH(IC) * CRFC    ! conversion from reference dose to concentration
          ENDIF
          IF(RRFDH(IC).LE.0.AND.RRFCH(IC).GT.0.) THEN
             RRFDH(IC) = RRFCH(IC) / CRFC    ! conversion from reference concentration to reference dose
          ENDIF
          IF(RURFH(IC).LE.0.AND.RCPFH(IC).GT.0.) THEN
             RURFH(IC) = RCPFH(IC) * CURF   ! conversion from slope factor to unit risk factor for inhalation
          ENDIF
          IF(RCPFH(IC).LE.0.AND.RURFH(IC).GT.0.) THEN
             RCPFH(IC) = RURFH(IC) / CURF
          ENDIF
!
!----     Read ingestion unit risk factors
!
          WRITE(NHLS,1005) RCPFG(IC), RCPFGS(IC), RCPFGF(IC), RCPFH(IC), RURFH(IC), &
                           RRFDH(IC), RRFDG(IC), RRFDGS(IC),RRFDGF(IC), RRFCH(IC)
 1005     FORMAT('  Chemical toxicity date      '/ &
                 '    Ingestion slope factor - water   ',1pE10.2,' (mg/kg/d)^-1'/ &
                 '    Ingestion slope factor - soil    ',1pE10.2,' (mg/kg/d)^-1'/ &
                 '    Ingestion slope factor - food    ',1pE10.2,' (mg/kg/d)^-1'/ &
                 '    Inhalation slope factor          ',E10.2,  ' (mg/kg/d)^-1'/ &
                 '    Unit risk factor Inhal.          ',E10.2,  ' risk/(ug/m^3)'/ &
                 '    Inhalation Reference dose        ',E10.2,  ' mg/kg/day'/ &
                 '    Ingestion Reference dose - water ',E10.2,  ' mg/kg/day'/ &
                 '    Ingestion Reference dose - soil  ',E10.2,  ' mg/kg/day'/ &
                 '    Ingestion Reference dose - food  ',E10.2,  ' mg/kg/day'/ &
                 '    Inhalation Reference Conc.       ',E10.2,  ' mg/m^3')
       ENDIF
!
     END DO  ! End do loop on constituents

 9999 RETURN
!
!----- END OF MODULE READ_CON_DATA -------------------------------------------
!
      END
!
      SUBROUTINE PRTERR( IERR, CALLER, MLINES )
!!************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine prints a standard error message to the report file.
!!
!!  Call List Variables:
!!
!!    Variable Type         Description
!!    -------- -----------  ---------------------------------------
!!    IERR     (Integer)    Error number from the calling routine
!!    CALLER   (Character)  Name of the calling routine
!!    MLINES   (Integer)    Number of lines in the error message
!!
!!  Module Variables:
!!
!!    Variable Type         Description
!!    -------- -----------  ---------------------------------------
!!    IRPT_ERR (Integer)    Error number from the calling routine
!!    MESSAG   (Character)  Message vector
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!
!!************************************************************************
!
! *** Global variables
!
      USE Errors_Mod
!
! *** Call list variables
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: IERR
      INTEGER, INTENT(IN) :: MLINES
      CHARACTER(LEN=*), INTENT(IN) :: CALLER
!
! *** Local variables
!
      INTEGER :: I ! Local looping control variable
!
!---- Executable code ---------------------------------------------------
!
! *** Write out the error number and the calling routine
!
      IF( IERR .EQ. 0 ) THEN
        WRITE(IRPT_ERR,1000) TRIM(CALLER)
 1000   FORMAT(/'Message originating in routine ',A)
      ELSE
        WRITE(IRPT_ERR,1010) IERR, TRIM(CALLER)
 1010   FORMAT(/'Error number ',I4,' encountered in routine ',A)
      END IF
!
! *** Write out the first line of the error message
!
      IF( MLINES .GT. 0 ) THEN
        WRITE(IRPT_ERR,1020) TRIM( MESSAG(1) )
 1020   FORMAT('Message: ',A)
      END IF
!
! *** Write out any trailing lines for the message
!
      IF( MLINES .GT. 1 ) THEN
        DO I = 2, MIN(MLINES,MAXMES)
          WRITE(IRPT_ERR,1030) TRIM( MESSAG(I) )
 1030   FORMAT(9X,A)
        END DO
      END IF
!
      RETURN
      END SUBROUTINE
!
!   INTAKE: SET_DATA                   Version Date: 30-Apr-2001
!   Copyright 1996 by Battelle Memorial Institute.  All rights reserved.
! ****************************************************************************
!                                                                            *
!                     SUBROUTINE SET_DATA                                    *
!                                                                            *
!  Subroutine SET_DATA sets data from arrays into specific parameters for    *
!  the current constituent                                                   *
!                                                                            *
!  Written by:       Dl Strenge                                              *
!                    Battelle Pacific Northwest Laboratories                 *
!                    P.O. Box 999                                            *
!                    Richland, WA 99352                                      *
!                                                                            *
!  Creation Date:    30-Apr-01                                               *
!  Last Modified:    30-Apr-01      DLS                                      *
!                                                                            *
! ****************************************************************************
!
!==== Modular Organization ===================================================
!
!     Module of: MEPAS
!     Called by: AHAZH
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
!
!==== SUBROUTINE CALL ========================================================
!
      SUBROUTINE SET_DATA(IC,IP,IERR)
!
      USE Errors_Mod
      USE Con_Data_Mod
!
      IMPLICIT NONE
!
!---- Include Statements for Parameter and Common Declarations ---------------
!
      include 'COUPLE.FTN'
      INCLUDE 'DEVICE.FTN'
      INCLUDE 'PSET1.FTN'
      INCLUDE 'PSET2.FTN'
      INCLUDE 'RISKCM.FTN'
!
!---- Variable Type Declarations ---------------------------------------------
!
      CHARACTER(LEN=8) :: CALLER = 'SET_DATA'  ! Name of this routine
      INTEGER :: IC,IP,IERR,KTYPE
      REAL :: URFH, RFCH
!
!----- Set toxicity parameters -----------------------------------------------
!
      IF(RFONE(IC).GT.0.) FONE = RFONE(IC)
      KTYPE = MTYPE(IC)
      NTYPE = KTYPE
      IF(KTYPE.EQ.1) THEN
!   Radionuclide variables
         DFG = RDFG(IC,IP+1)      ! PSET2
         DFA = RDFH(IC,IP+1)      ! PSET2
         DFS = RDFS(IC,IP+1)      ! PSET2
         DIMR= RDIMR(IC,IP+1)     ! PSET2
         DSH = RDSH(IC,IP+1)      ! PSET2
         DEX = RDEX(IC,IP+1)      ! PSET2
!   Radionuclide slope factors (HEAST)
         CPFH  = RSFH(IC,IP+1)     ! PSET1
         CPFG  = RSFG(IC,IP+1)     ! PSET1 - WATER
         CPFGS = RSFGS(IC,IP+1)    ! PSET1 - SOIL
         CPFGF = RSFGF(IC,IP+1)    ! PSET1 - FOOD
         CPFS  = RSFS(IC,IP+1)     ! PSET1
      ELSE
!   Chemical variables
         CPFH = RCPFH(IC)            ! PSET1
         CPFG =  RCPFG(IC)           ! PSET1 - WATER
         CPFGS = RCPFGS(IC)          ! PSET1 - SOIL
         CPFGF = RCPFGF(IC)          ! PSET1 - FOOD
         URFH = RURFH(IC)            ! LOCAL
         RFDH = RRFDH(IC)            ! PSET1
         RFDG  = RRFDG(IC)           ! PSET1 - WATER
         RFDGS = RRFDGS(IC)          ! PSET1 - SOIL
         RFDGF = RRFDGF(IC)          ! PSET1 - FOOD
         RFCH = RRFCH(IC)            ! LOCAL
         FONE = RFONE(IC)            ! PSET1
         IF(INHALE.EQ.0) THEN
            CPFH = URFH
            RFDH = RFCH
         ENDIF
      ENDIF
!
!      WRITE(NHIF,*) ' In SET_DATA: ic, mtype, cpfh',mtype(ic),cpfh
!      WRITE(NHIF,*) ' In SET_DATA: ic, mtype, cpfh',mtype(ic),cpfh
 9999 RETURN
!
!----- END OF MODULE SET_DATA -------------------------------------------
!
      END
!
