      PROGRAM VIEWER
C     A FRAMES/GENII OUTPUT FORMATING PROGRAM
C     B. A. NAPIER
C     MOST RECENT MODIFICATION:
C       BAN  INITIAL PROGRAMMING   26 DECEMBER 2001
C       BAN  enhancement/FORMATTING     16 Jan 2002
C       BAN  Enhance MI finding with POPI  15 April 2002 
c       BAN  Expand date/time to *10
C       BAN  Only one NesSrcName (i.e., one hei file) is allowed per neshaps viewer
c       BAN  BUT - the NesSrcNam may have more than 4 characters; take max 3/24/03
c       BAN  ALLOCATE some arrays at maximum 3 endpoints * 28 pathways if NTIMES>1
C       BAN  Only count JNUC on the first time iteration 13 April 2006
C       BAN  Minor correction for 1x1 grids
C       BAN  Move zeroing functions within NTIMES loop to separate acute time pathways 1/16/07
C       BAN  Added several output options (unit selection, pathway-by-nuclide output) 9/14/09
c       BAN  Resized FoodFile and PFilNam to 80 char
C
C     SIZING ASSUMPTIONS -
C       100 NUCLIDES & PROGENY
C       41 X 41 MAXIMUM GRID SIZE
C       30 PATHWAYS, 3 ENDPOINTS
C       UNLIMITED TIMES
C
C
      USE DFLIB   !This is a DEC Fortran thing
C
      INCLUDE 'NESHAPS.CMN'
      INCLUDE 'DATABLKS.CMN'
	include 'datarcp.cmn'
C
      ALLOCATABLE CANCERF(:,:,:,:,:), CANCERI(:,:,:,:,:)
      ALLOCATABLE DOSE(:,:,:,:,:)
      ALLOCATABLE POP(:,:,:), POPI(:,:,:),FOOD(:,:,:)
      ALLOCATABLE SUMD(:,:,:), SUMI(:,:,:), SUMF(:,:,:)
      ALLOCATABLE SUMDP(:,:), SUMPF(:,:), SUMPI(:,:)
      DIMENSION SITENAM(15), ORGNAM(24)
      DIMENSION DIST(41), DIR(41), remSv(4), PremSv(4)
      DIMENSION LOWAGE(6), UPAGE(6)
      DIMENSION NUKID(100),T(2),PATH(90),ROUTE(90),UNIT(90),ENDPT(90)
      DIMENSION EDbyNUC(100), RFbyNUC(100), RIbyNUC(100)
      DIMENSION EDbyNUCP(100), RFbyNUCP(100), RIbyNUCP(100)
      DIMENSION EDbyPATH(90), RFbyPATH(90), RIbyPATH(90)
      DIMENSION EDbyPATHP(90), RFbyPATHP(90), RIbyPATHP(90)
	dimension pathnam(8),totprod(90), TOTCONS(90)
	dimension AvePWtDose(100,90,25), AvePWtCanI(100,90,15),
     .          AvePWtCanF(100,90,15)
C
      CHARACTER*17  CANFAT, CANINC, RADDOSE, ENDPT
      CHARACTER*128 GIDFNM, RUNFNM
      CHARACTER*32  FacNam, FacStrt, UsrNam,FacCity
      CHARACTER*8   HEINAM
      CHARACTER*10  TMPDAT, TMPTIM
      CHARACTER*1   B, DUM
      CHARACTER*12  ACCHRON, SOURCE, HPATH, ROUTE
      CHARACTER*10  SITENAM, ORGNAM, VERSION, PremSvNam(8)
      CHARACTER*6   NUKID
      CHARACTER*22  PATH, pathnam
      CHARACTER*7   UNIT
      CHARACTER*3   yesno
      CHARACTER*32  GLFNAM
      CHARACTER*32   NESSRCNA(10) 
	character*4   remSvnam(4)
	CHARACTER*120 shortref
	CHARACTER*80  FOODFILE,PFilNam
C
      INTEGER UPAGE
C
      LOGICAL LEXIST, MEXIST, DOSETF, CANITF, CANFTF, POPTF,FODTF
      LOGICAL SEQI, PASS1, PASS2, PASS3, PASS4, PASS5, PASS6
      LOGICAL PASS7, PASS8, PASS9, PASS10, PASS11
	logical pthon(90),nexist
C
      DATA CANFAT/'cancer fatalities'/, CANINC/'cancer incidence'/
      DATA RADDOSE/'Sv            '/
      DATA B/' '/
      DATA NUKMAX/100/
	data remSvnam/'Sv','mSv','rem','mrem'/
	data remSv/1.0, 1000.0, 100.0, 100000.0/
	data PremSv/1.0, 1.0, 100.0, 100.0/
	data PremSvNam/'Person-Sv','Person-Sv','Person-rem','Person-rem',
     .'P-Sv','P-Sv','Prem','Prem'/
C
C     OPEN INPUT AND OUTPUT FILES
C
      VERSION = '2.10.1'

      CALL INPUTS(FacNam, FacStrt, FacCity, UsrNam, PFilNam,
     .            GIDFNM, RUNFNM, NSITE, NUMNES, GLFNAM, METHOD,
     .             NESSRCNA, NESSCRNUM, FOODFILE,ifodprd)
C
      NGIDNM = INDEX(GIDFNM,B)-1
C
      OPEN(UNIT=NHIF,FILE = GIDFNM(1:NGIDNM)//'.HIF',STATUS = 'OLD')
      INQUIRE(FILE=GIDFNM(1:NGIDNM)//'.ATO',EXIST = LEXIST)
      IF (LEXIST) THEN
        OPEN(UNIT=NATO,FILE = GIDFNM(1:NGIDNM)//'.ATO',STATUS = 'OLD')
      ELSE
        PRINT 1
        WRITE(NEPA,1)
    1   FORMAT('DISPERSION/DEPOSITION FILE (ATO) NOT AVAILABLE.  STOP.')
        STOP
      END IF
C
C     SEE IF A POPULATION FILE IS WANTED
C

      NPOPNM = INDEX(PFilNam,B)-1
      IF (METHOD .EQ. 2) THEN
       inquire(file=PFilNam, exist=mexist)
        if (mexist) then
          OPEN(UNIT=NPOP,FILE = PFilNam(1:NPOPNM) ,STATUS = 'OLD')
          POPTF = .TRUE.
        else
          print 992
  992     format('The POPULATION file does not seem to be there -',
     .     ' check spelling,',/,', path name, or existence')
        end if
      END IF
c
c    SEE IF A FOOD NETWORK FILE IS WANTED
c
      NFODNM = INDEX(FOODFILE,B)-1
      IF (IFODPRD .NE. 0) THEN
       inquire(file=FOODFILE, exist=Nexist)
        if (Nexist) then
          OPEN(UNIT=NFOD,FILE = FOODFILE(1:NFODNM) ,STATUS = 'OLD')
          FODTF = .TRUE.
	    read(nfod,*)dum, dum, dum
	    READ(nfod,*)MPTHS,MDIST,MDIR
	    allocate (food(8,mdist,mdir))
	     call infood(mpths,mdist,mdir,food)
        else
          print 993
  993     format('The FOOD DISTRIBUTION file does not seem to be there',
     .     ' - check spelling,',/,', path name, or existence')
        end if
      END IF
C
C     FIND START OF HIF MODULE
C
      INUM = 0
   11 READ(NHIF,*)HEINAM, NUMLIN
C
C   Revised BAN 12 Mar 2003 - only one NesSrcName allowed
c   RE-revised BAN 24 Mar 2003 - match number of characters
C  
      NRUNNM1 = INDEX(NESSRCName,B)-1
      NRUNNM2 = INDEX(HEINAM,B)-1
      NRUNNM = MAX(NRUNNM1, NRUNNM2)
      IF (SEQI(HEINAM, NESSRCName, NRUNNM)) GO TO 10
      DO I=1,NUMLIN
        READ(NHIF, '(A1)') DUM
      END DO
      GO TO 11
   10 CONTINUE
C
C     READ PAST HEADER LINES
C
      READ(NHIF,*)NHEAD
      DO I=1,NHEAD
        READ(NHIF,'(A1)')DUM
      END DO
C
C     READ NUMBER OF DATASETS TO PROCESS
C     THIS IS AN OUTER LOOP FOR THE ENTIRE REMAINDER
C     (IT IS LIKELY THAT NEARLY ALL NESHAPS RESULTS WILL ONLY HAVE 1 SET)
C
      READ(NHIF,*)NSETS
      DO ISET = 1, NSETS
C
C     MAKE SURE THAT WE ONLY ALLOCATE SPACE ONCE PER SET
C
      PASS1 = .TRUE.
      PASS2 = .TRUE.
      PASS3 = .TRUE.
      PASS4 = .TRUE.
      PASS5 = .TRUE.
      PASS6 = .TRUE.
      PASS7 = .TRUE.
      PASS8 = .TRUE.
      PASS9 = .TRUE.
      PASS10 = .TRUE.
      PASS11 = .TRUE.
C
      READ(NHIF,*)ACCHRON,SOURCE,HPATH,NPOINTS,NAGES,NUKES,NSITES,NORGAN
      READ(NHIF,*)(SITENAM(J), J = 1, NSITES)
      READ(NHIF,*)(ORGNAM(J), J = 1, NORGAN) 
C
C     IT LOOKS EASIER TO USE THE GRID DEFINITIONS FROM THE ATO FILE
C     SO READ PAST THE LOCATIONS HERE
C
      DO J = 1, NPOINTS
        READ(NHIF,'(A1)')DUM
      END DO 
C 
C     GET THE GRID INFO FROM THE ATO FILE
C     HERE, DIR IS THE X DIRECTION, DIST IS THE Y DIRECTION
C
      CALL ATOREAD(1, NDIST, DIST, NDIR, DIR, airname)
C
C     FOR NOW, THE POPULATION FILE MUST BE OF THE SAME DIMENSIONS AS THE ATO FILE
C     ALSO, THE FOOD DISTRIBUTION FILE MUST BE OF THE SAME DIMENSIONS
C
      IF(ifodprd .and. ((MDIST .NE. NDIST) .OR. (MDIR .NE. NDIR))) THEN
	  WRITE (NEPA,5)
    5   FORMAT('THE FOOD PRODUCTION FILE DOES NOT MATCH THE ',
     .  'ATO FILE; STOP')
	  STOP
	END IF

C     THE POPI ARRAY IS 0.0 IF NOBODY LIVES THERE, 1.0 IF INHABITED.  
C     POPI IS ALL 1.0 IF NO POPULATION FILE IS ATTACHED
C
      IF (PASS11) ALLOCATE (POPI(NAGES,NDIR,NDIST))
      IF (PASS11) POPI = 1.0
      PASS11 = .FALSE.
C
      IF (POPTF) THEN
        IF (PASS1) ALLOCATE (POP(NAGES,NDIR,NDIST))
	  read(npop,*)dum, dum, dum
	  read(npop,*)iag,indst,indr
	  if(iag .ne. nages .or. indr .ne. ndir .or. indst .ne. ndist)
     .     go to 1000
        PASS1 = .FALSE.
        DO IA = 1, NAGES
	    read(npop,*) dum
          DO IDR = 1, NDIR
            READ (NPOP,*,ERR=1000) (POP(IA,IDR,IDT), IDT = 1, NDIST)
            DO IDT = 1, NDIST
              IF (POP(IA,IDR,IDT) .EQ. 0.0) POPI(IA,IDR,IDT) = 0.0
            END DO
          END DO
        END DO
      END IF
      GO TO 1001
 1000 PRINT 4
      WRITE(NEPA, 4)
    4 FORMAT('THE POPULATION FILE DOES NOT MATCH THE DISPERSION ARRAY.',
     .'STOP.')
      STOP
 1001 CONTINUE
C
C     PRINT IDENTIFYING  INFORMATION IN OUTPUT FILE
C
      IF (METHOD .NE. 0)THEN
c----- print version, and date and time run
c
      CALL DATE_AND_TIME( TMPDAT, TMPTIM )
      TMPTIM = TMPTIM(1:2)//':'//TMPTIM(3:4)//':'//TMPTIM(5:6)//'  '
      TMPDAT = TMPDAT(5:6)//'-'//TMPDAT(7:8)//'-'//TMPDAT(1:4)
        write (NEPA, 805) VERSION, TMPDAT, TMPTIM
        WRITE (NEPA, 802) FACNAM
        WRITE (NEPA, 803) FACSTRT, FACCITY
        WRITE (NEPA, 804) USRNAM
805   FORMAT(' 0,',/,' 1,',/,'---- GENII ',A10,' Run on: ',A10,'  at ',
     .A10,' ----------------------------------------------------')
802   FORMAT (//' FACILITY NAME: ',11x, A32)
803   FORMAT (' FACILITY MAILING ADDRESS: ',A32)
804   FORMAT (//' INPUT PREPARED BY: ',7x, A32, ///)
      END IF
C
C     PRINT HEADER INFO IN OUTPUT FILE
C

      WRITE(NEPA,800) GIDFNM,HEINAME,NAGES,NUKES,NORGAN,NSITES
  800 FORMAT('GENII VERSION 2',/,'----SUMMARY REPORT FOR CASE:
     . ',A50,'-----------------------------',/,
     . 'FOR IMPACT ICON: ',A32,
     .//,'THE HEALTH IMPACTS FILE FOR THIS CASE CONTAINS INFORMATIO'
     .,'N ON:',/,10X,I3,' AGE GROUP(S)',/,10X,I3,' RADIONUCLIDES (COUNT'
     .,'ING DECAY PROGENY SEPARATELY IN CHAINS)',/
     .,10X,I3,' ORGAN(S)/TISSUE(S) FOR RADIATION DOSE',/,10X,I3,' POTE'
     .,'NTIAL CANCER SITES')
  801 FORMAT(/,10X,I3,' EXPOSURE PERIOD(S).',/,/,107('-'))
      IPRT1=0
C
      WRITE(NEPA, 806)
  806 FORMAT (//,'---- SUMMARY OF CASE INPUT DATA --------------------',
     .'-------------------------------------------------------')
C
       CALL OUTSRC
       CALL OUTAIR
       CALL OUTEXP
       CALL OUTRCP
c
c dump the "references" file contents, too.
       inquire(file=GIDFNM(1:NGIDNM)//'.REF', exist=Nexist)
        if (Nexist) then
	    write(nepa,322)
  322     format('ADDITIONAL NOTES:',/)
          OPEN(UNIT=Nref,FILE = GIDFNM(1:NGIDNM)//'.REF',STATUS = 'OLD')
	    read(nref,*)dum, dum, dum
 4321	    READ(nref,*, end=54321)kref,shortref
	    if(kref .gt. 0) write (nepa,321)shortref
	    go to 4321
  321     format(a100)
54321     continue
        end if
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     NOW COME BACK AND FINISH READING THE HIF FILE
C       BY AGE GROUP
C         BY NUCLIDE AND PROGENY
C           BY TIME PERIOD
C             BY PATHWAY
C               BY OUTCOME (FATAILITY, INCIDENCE, DOSE)
C                 FOR ALL LOCATIONS
C
      DO IAGE = 1, NAGES
      PRINT 998, iage
  998 FORMAT ('Working on age group ', I3)
C
C     AGES IS A BIG OUTER LOOP
C
        DOSETF = .FALSE.
        CANFTF = .FALSE.
        CANITF = .FALSE.    
        READ(NHIF,*) LOWAGE(IAGE), UPAGE(IAGE) 
        JNUC = 0
        IPRT1 = 0
        DO INUKE = 1, NUKES
          JNUC = JNUC+1
          READ(NHIF,*)NUKID(JNUC),DUM,NPROG,NTIMES
          IF (IPRT1 .EQ. 0) THEN
          WRITE(NEPA,801)NTIMES	
	      if (mexist) then
	        WRITE(NEPA,2)PFilNam
    2         FORMAT('POPULATION DATA FROM FILE ',A32,/)
c  dump the pop file 
            WRITE (NEPA,771) (DIST(I), I=1, NDIST)
  771       FORMAT('AGE GROUP ','DIRECTION/DISTANCE:',41(2X,F6.0))

		 DO IDR = 1, NDIR
            WRITE (NEPA,772) LOWAGE(IAGE), UPAGE(IAGE), DIR(IDR),
     .                     (POP(IAGE,IDR,IDT), IDT = 1, NDIST)
  772       FORMAT(I4,' - ',I3,4X, F6.0, T30,41(F8.0))
           END DO

              if (ifodprd .ne. 0) then
	          write(nepa,222)foodfile
  222           format('FOOD DISTRIBUTION NETWORK FROM FILE ',A32,/)
              END IF
            else
            WRITE(NEPA,3)
    3         FORMAT('INDIVIDUAL DOSE CALCULATION ONLY, NO POPULATION')
            end if
            IPRT1 = 1
          END IF
          DO ITIME = 1,NTIMES
            READ(NHIF,*)T(ITIME), DUM, TINC, DUM, NVALS
c
            nvalm=nvals
            if(ntimes .gt. 1) nvalm=84
c
            DO IVAL = 1, NVALS  ! VALS HERE ARE #PATHWAYS * #ENDPOINTS
              READ(NHIF,*)BADPOP, PATH(IVAL), ROUTE(IVAL), UNIT(IVAL), 
     .        ENDPT(IVAL)
              IF (SEQI(ENDPT(IVAL),CANFAT,8)) THEN
                DO IPT = 1, NPOINTS
                  CANFTF = .TRUE.
                  IF (PASS2) ALLOCATE (CANCERF(NTIMES,NUKMAX,NPOINTS,
     .                                 NVALm,NSITES))
                  IF (PASS2) CANCERF = 0.0
                  PASS2 = .FALSE.
            READ(NHIF,*)(CANCERF(ITIME,JNUC,IPT,IVAL,ISITE),
     .                         ISITE = 1,NSITES)
                END DO
              ELSE IF (SEQI(ENDPT(IVAL),CANINC,8)) THEN
                DO IPT = 1, NPOINTS
                  CANITF = .TRUE.
                  IF (PASS3) ALLOCATE (CANCERI(NTIMES,NUKMAX,NPOINTS,
     .                                 NVALm,NSITES))
                  IF (PASS3) CANCERI = 0.0
                  PASS3 = .FALSE.
            READ(NHIF,*)(CANCERI(ITIME,JNUC,IPT,IVAL,ISITE),
     .                         ISITE = 1,NSITES)
                END DO
              ELSE IF (SEQI(ENDPT(IVAL),RADDOSE,8)) THEN            
            DO IPT = 1, NPOINTS
                  DOSETF = .TRUE.
                  IF (PASS4) ALLOCATE (DOSE(NTIMES,NUKMAX,NPOINTS,
     .                                 NVALm,NORGAN))
                  IF (PASS4) DOSE = 0.0
                  PASS4 = .FALSE.
            READ(NHIF,*)(DOSE(ITIME,JNUC,IPT,IVAL,IORG),
     .                         IORG = 1,NORGAN)
                END DO
              END IF 
            END DO ! VALS = PATHWAYS * ENDPOINTS
C     FIGURE OUT HOW MANY PATHWAYS THERE REALLY ARE 
            MCF=0
          MCI=0
          MDS=0
          IF (CANFTF) MCF=1
            IF (CANITF) MCI=1
            IF (DOSETF) MDS=1
            MCOUNT=MCF+MCI+MDS
            NPATHWAY = NVALS/MCOUNT
C
            IF (NPROG .GT.0) THEN
            DO IPROG = 1, NPROG
C
C  A CHANGE IN THE NEXT LINE TO KEEP FOR OVERCOUNTING JNUC AFTER TIME 1 ban 13 april 2006
C
              IF (ITIME .EQ. 1)JNUC = JNUC+1
              READ(NHIF,*)NUKID(JNUC),DUM,NVALS
              DO IVAL = 1, NVALS  ! VALS HERE ARE PATHWAYS
                READ(NHIF,*)BADPOP, PATH(IVAL), ROUTE(IVAL), UNIT(IVAL), 
     .          ENDPT(IVAL)
                  IF (SEQI(ENDPT(IVAL),CANFAT,8)) THEN
                    DO IPT = 1, NPOINTS
                      READ(NHIF,*)(CANCERF(ITIME,JNUC,IPT,IVAL,ISITE),
     .                         ISITE = 1,NSITES)
                    END DO
                  ELSE IF (SEQI(ENDPT(IVAL),CANINC,8)) THEN
                    DO IPT = 1, NPOINTS
                      READ(NHIF,*)(CANCERI(ITIME,JNUC,IPT,IVAL,ISITE),
     .                          ISITE = 1,NSITES)
                    END DO
                  ELSE IF (SEQI(ENDPT(IVAL),RADDOSE,8)) THEN            
                    DO IPT = 1, NPOINTS
                      READ(NHIF,*)(DOSE(ITIME,JNUC,IPT,IVAL,IORG),
     .                         IORG = 1,NORGAN)
                    END DO
                  END IF 
              END DO  !  VALS = PATHWAYS
            END DO  !  NPROG
            END IF  !  NPROG
          END DO  !  NTIME
        END DO  !  NUKES
c      END DO !  NAGES keep going, end this loop lower down!
C
C     OK.  IT IS ALL READ IN, NOW ACCUMULATE AND FORMAT
C
C  FIRST, ACCUMULATE OVER PATHWAYS AND NUCLIDES FOR EACH ORGAN FOR ALL POINTS
C    
      print 997
  997 format('It is all read in, now summarizing')
      IF (PASS5 .AND. DOSETF) ALLOCATE (SUMD(NTIMES,NPOINTS,NORGAN))
      IF (PASS5 .AND. DOSETF) SUMD=0.0
      IF (PASS5 .AND. DOSETF)PASS5 = .FALSE.
      IF (PASS6 .AND. DOSETF) ALLOCATE (SUMDP(NTIMES,NORGAN))
      IF (PASS6 .AND. DOSETF) SUMDP=0.0
      IF (PASS6 .AND. DOSETF)PASS6 = .FALSE.
      IF (PASS7 .AND. CANFTF) ALLOCATE (SUMF(NTIMES,NPOINTS,NSITES))
      IF (PASS7 .AND. CANFTF) SUMF=0.0
      IF (PASS7 .AND. CANFTF)PASS7 = .FALSE.
      IF (PASS8 .AND. CANFTF) ALLOCATE (SUMPF(NTIMES,NSITES))
      IF (PASS8 .AND. CANFTF) SUMPF=0.0
      IF (PASS8 .AND. CANFTF)PASS8 = .FALSE.
      IF (PASS9 .AND. CANITF) ALLOCATE (SUMI(NTIMES,NPOINTS,NSITES))
      IF (PASS9 .AND. CANITF) SUMI=0.0
      IF (PASS9 .AND. CANITF)PASS9 = .FALSE.
      IF (PASS10 .AND. CANITF) ALLOCATE (SUMPI(NTIMES,NSITES))
      IF (PASS10 .AND. CANITF) SUMPI=0.0
      IF (PASS10 .AND. CANITF)PASS10 = .FALSE.
      SUMPI=0.0
      DO IT = 1, NTIMES
        DO IPT = 1, NPOINTS
          NDR = (IPT-1)/NDIST+1
          NDST = MOD(IPT,NDIST)
          IF(NDST .EQ. 0) NDST = npoints/ndir
          IF(NDST .EQ. 0) NDST = 1
          DO INUC = 1, JNUC
            DO IVAL = 1, NVALS
              DO IORG = 1, NORGAN
                IF(DOSETF) THEN
                SUMD(IT,IPT,IORG)=SUMD(IT,IPT,IORG)
     .             + DOSE(IT,INUC,IPT,IVAL,IORG)*POPI(IAGE,NDR,NDST)
     .               *remSv(IremSv+1)
                IF(POPTF) THEN
                SUMDP(IT,IORG)=SUMDP(IT,IORG) +
     .                 DOSE(IT,INUC,IPT,IVAL,IORG)*POP(IAGE,NDR,NDST)
     .               *PremSv(IremSv+1)      
                END IF
            END IF 
              END DO
              DO ISITE = 1, NSITES
                IF (CANFTF) THEN
                SUMF(IT,IPT,ISITE)=SUMF(IT,IPT,ISITE)
     .             + CANCERF(IT,INUC,IPT,IVAL,ISITE)*POPI(IAGE,NDR,NDST)
                IF(POPTF) THEN
            SUMPF(IT,ISITE)=SUMPF(IT,ISITE)
     .              + CANCERF(IT,INUC,IPT,IVAL,ISITE)*POP(IAGE,NDR,NDST)
                END IF
                END IF
                IF (CANITF) THEN
                SUMI(IT,IPT,ISITE)=SUMI(IT,IPT,ISITE)
     .             + CANCERI(IT,INUC,IPT,IVAL,ISITE)*POPI(IAGE,NDR,NDST)
                IF (POPTF) THEN
                SUMPI(IT,ISITE)=SUMPI(IT,ISITE)
     .              + CANCERI(IT,INUC,IPT,IVAL,ISITE)*POP(IAGE,NDR,NDST)
                END IF
                END IF
              END DO
            END DO
          END DO
        END DO
      END DO
C
C   FIND HIGHEST INDIVIDUAL LOCATION
C     (IN GENII, EFFECTIVE DOSE IS EITHER ORGAN 1 OR 24, AND TOTAL RISK IS 1 OR 15)
C     PREFERRED SELECTOR IS DOSE, THEN INCIDENCE, THEN FATALITY
C
      PRINT 996
  996 FORMAT('Printing tables')
      DO IT = 1, NTIMES
C   Move these zeroing functions within the time loop   BAN 16 Jan 2007
      IF(DOSETF) EDbyNUC = 0.0
      IF(DOSETF) AvePWtDose = 0.0
      IF(CANITF) RIbyNUC = 0.0
      IF(CANITF) AvePWtCanI = 0.0
      IF(CANFTF) RFbyNUC = 0.0
      IF(CANFTF) AvePWtCanF = 0.0
      IF(DOSETF .AND. POPTF) EDbyNUCP = 0.0
      IF(CANITF .AND. POPTF) RIbyNUCP = 0.0
      IF(CANFTF .AND. POPTF) RFbyNUCP = 0.0
      IF(DOSETF) EDbyPATH=0.0
      IF(CANITF) RIbyPATH=0.0
      IF(CANFTF) RFbyPATH=0.0
      IF(DOSETF .AND. POPTF) EDbyPATHP = 0.0
      IF(CANITF .AND. POPTF) RIbyPATHP = 0.0
      IF(CANFTF .AND. POPTF) RFbyPATHP = 0.0  
c
      MORG = 1
      LORG = 1
      IF (NORGAN .GT. 1)MORG = 24
      IF (NSITES .GT. 1)LORG = 15
      DOSEMAX = 0.0
      CIMAX = 0.0
      CFMAX = 0.0
      IF (DOSETF) THEN
      DO IPT = 1, NPOINTS
        IF(SUMD(IT,IPT,MORG) .GT. DOSEMAX)THEN
          DOSEMAX = SUMD(IT,IPT,MORG)
          IPTMAX = IPT
c
          NDR1 = (IPTMAX-1)/NDIST+1
          NDST1 = MOD(IPTMAX,NDIST)
          IF(NDST1 .EQ. 0) NDST1 = npoints/ndir
          IF(NDST1 .EQ. 0) NDST1 = 1
c
        END IF
      END DO
      END IF
C     USE CANCER INCIDENCE IF DOSE NOT AVAILABLE
      IF((.NOT. DOSETF) .AND. CANITF) THEN
      DO IPT = 1, NPOINTS
        IF(SUMI(IT,IPT,LORG) .GT. CIMAX)THEN
          CIMAX = SUMI(IT,IPT,LORG)
          IPTMAX = IPT
c
          NDR1 = (IPTMAX-1)/NDIST+1
          NDST1 = MOD(IPTMAX,NDIST)
          IF(NDST1 .EQ. 0) NDST1 = npoints/ndir
          IF(NDST1 .EQ. 0) NDST1 = 1
c
        END IF
      END DO
      END IF
C     USE CANCER FATALITIES IF NEITHER OTHER AVAILABLE
      IF((.NOT. DOSETF) .AND. (.NOT. CANITF)) THEN
      DO IPT = 1, NPOINTS
        IF(SUMF(IT,IPT,LORG) .GT. CFMAX)THEN
          CFMAX = SUMF(IT,IPT,LORG)
          IPTMAX = IPT
c
          NDR1 = (IPTMAX-1)/NDIST+1
          NDST1 = MOD(IPTMAX,NDIST)
          IF(NDST1 .EQ. 0) NDST1 = npoints/ndir
          IF(NDST1 .EQ. 0) NDST1 = 1
c
        END IF
      END DO
      END IF
C
C     NEED SEVEN TABLES FOR OPTIONS:     
C
C 1.
      IF (DOSETF .AND. CANITF .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
   19 FORMAT(/,'TIME PERIOD NUMBER',I3,', CORRESPONDING TO TIME',F8.4,
     .' YEARS ------------------------------------------------')
      WRITE(NEPA,20)IPTMAX, NDR1, NDST1, EXPx, EXPy, LOWAGE(IAGE), 
     .UPAGE(IAGE), remSvnam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,21)ORGNAM(I),SUMD(IT,IPTMAX,I), SITENAM(I), 
     .  SUMI(IT,IPTMAX,I), SUMF(IT,IPTMAX,I)
      ELSE
        WRITE(NEPA,22)ORGNAM(I),SUMD(IT,IPTMAX,I)
   20 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,' AT LOCATION X=',F5.1,' KM; Y=',F5.1,' KM',/,
     .'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,
     .'ORGAN',9X,'MAX. DOSE (',A4,')',1X,'TISSUE',5X,'CANCER INCIDENCE',
     .       '   CANCER FATALITIES')
   21 FORMAT(A10,4X, 1PE10.2, 7X, A9, 3X, E10.2, 8X, E10.2)
   22 FORMAT(A10,4X, 1PE10.2)
      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF   ! TABLE FOR (DOSETF .AND. CANITF .AND. CANFTF)
C 2.
      IF (DOSETF .AND. CANITF .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,201)IPTMAX, NDR1, NDST1, expx, expy, LOWAGE(IAGE), 
     .UPAGE(IAGE), remSvNam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,210)ORGNAM(I),SUMD(IT,IPTMAX,I), SITENAM(I), 
     .  SUMI(IT,IPTMAX,I)
      ELSE
        WRITE(NEPA,220)ORGNAM(I),SUMD(IT,IPTMAX,I)
  201 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,' AT LOCATION X=',F5.1,' KM; Y=',F5.1,' KM',/,
     .'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,
     .'ORGAN',9X,'MAX. DOSE (',A4,')',3X,'TISSUE',5X,'CANCER INCIDENCE')
  210 FORMAT(A10,4X, 1PE10.2, 7X, A9, 3X, E10.2)
  220 FORMAT(A10,4X, 1PE10.2)
      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF   ! TABLE FOR (DOSETF .AND. CANITF .AND. (.NOT. CANFTF))
C 3.
      IF (DOSETF .AND. (.NOT. CANITF) .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,202)IPTMAX, NDR1, NDST1, expx, expy, LOWAGE(IAGE), 
     . UPAGE(IAGE), remSvNam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,211)ORGNAM(I),SUMD(IT,IPTMAX,I), SITENAM(I), 
     .  SUMF(IT,IPTMAX,I)
      ELSE
        WRITE(NEPA,221)ORGNAM(I),SUMD(IT,IPTMAX,I)
  202 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,' AT LOCATION X=',F5.1,' KM; Y=',F5.1,' KM',/,
     .'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,'ORGAN',
     .9X,'MAX. DOSE (',A4,')',1X,'TISSUE',5X,'CANCER FATALITIES')
  211 FORMAT(A10,4X, 1PE10.2, 7X, A9, 3X, E10.2)
  221 FORMAT(A10,4X, 1PE10.2)
      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF   ! TABLE FOR (DOSETF .AND. (.NOT. CANITF) .AND. CANFTF)
C 4.
      IF (DOSETF .AND. (.NOT. CANITF) .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,203)IPTMAX, NDR1, NDST1, expx, expy, LOWAGE(IAGE),
     .UPAGE(IAGE), remSvNam(IremSv+1)
      DO I=1,NORGAN
        WRITE(NEPA,212)ORGNAM(I),SUMD(IT,IPTMAX,I)
  203 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,' AT LOCATION X=',F5.1,' KM; Y=',F5.1,' KM',/,
     .'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,
     .'ORGAN',9X,'MAX. DOSE (',A4,')')
  212 FORMAT(A10,4X, 1PE10.2)
      END DO  !  NORGAN PRINTING LOOP
      END IF   ! TABLE FOR (DOSETF .AND. (.NOT. CANITF) .AND. (.NOT.CANFTF))
C 5.
      IF ((.NOT.DOSETF) .AND. CANITF .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,204)IPTMAX, NDR1, NDST1, expx, expy, LOWAGE(IAGE), 
     .UPAGE(IAGE)
      DO I=1,NSITES
        WRITE(NEPA,213)SITENAM(I),SUMI(IT,IPTMAX,I), SUMF(IT,IPTMAX,I)
  204 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,' AT LOCATION X=',F5.1,' KM; Y=',F5.1,' KM',/,
     .'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,
     .'TISSUE',5X,'CANCER INCIDENCE','   CANCER FATALITIES')
  213 FORMAT(A9, 3X, E10.2, 8X, E10.2)
      END DO  !  NSITES PRINTING LOOP
      END IF   ! TABLE FOR ((.NOT. DOSETF) .AND. CANITF .AND. CANFTF)
C 6.
      IF ((.NOT.DOSETF) .AND. CANITF .AND. (.NOT.CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,205)IPTMAX, NDR1, NDST1, expe, expy, LOWAGE(IAGE),
     . UPAGE(IAGE)
      DO I=1,NSITES
        WRITE(NEPA,214)SITENAM(I), SUMI(IT,IPTMAX,I)
  205 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,' AT LOCATION X=',F5.1,' KM; Y=',F5.1,' KM',/,
     .'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,
     .'TISSUE',5X,'CANCER INCIDENCE')
  214 FORMAT(A9, 3X, E10.2)
      END DO  !  NSITES PRINTING LOOP
      END IF   ! TABLE FOR ((.NOT. DOSETF) .AND. CANITF .AND. (.NOT.CANFTF))
C 7.
      IF ((.NOT.DOSETF) .AND. (.NOT. CANITF) .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,206)IPTMAX, NDR1, NDST1, expe, expy, LOWAGE(IAGE),
     . UPAGE(IAGE)
      DO I=1,NSITES
        WRITE(NEPA,215)SITENAM(I), SUMF(IT,IPTMAX,I)
  206 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,' AT LOCATION X=',F5.1,' KM; Y=',F5.1,' KM',/,
     .'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,
     .'TISSUE',5X,'CANCER FATALITIES')
  215 FORMAT(A9, 3X, E10.2)
      END DO  !  NSITES PRINTING LOOP
      END IF   ! TABLE FOR ((.NOT. DOSETF) .AND. (.NOT. CANITF) .AND. CANFTF)
C
C     PRINT LARGE SUMMARY TABLES
C
      IF (DOSETF .AND. (NPOINTS .GT. 1) ) THEN
C     INDIVIDUAL EFFECTIVE DOSE BY DISTANCE AND DIRECTION
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,300)LOWAGE(IAGE), UPAGE(IAGE)
  300 FORMAT('INDIVIDUAL AGE RANGE',I3,' TO',I3,' YEARS')
      WRITE(NEPA,40) remSvNam(IremSv+1), (DIST(I), I=1, NDIST)
   40 FORMAT('INDIVIDUAL EFFECTIVE DOSE BY DISTANCE AND DIRECTION 
     .(',A4,')',/, 7X,41F8.0)
      NDT=1
      DO ND = 1, NDIR
        NDT1 = NDT+NDIST-1
        WRITE(NEPA,41)DIR(ND), (SUMD(IT,I,MORG), I = NDT,NDT1)
        NDT = NDT+NDIST
      END DO
   41 FORMAT(F7.0, 41(1PE8.1))
      END IF
      IF (CANITF .AND. (NPOINTS .GT. 1) ) THEN
C     INDIVIDUAL CANCER INCIDENCE BY DISTANCE AND DIRECTION
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,300)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,42) (DIST(I), I=1, NDIST)
   42 FORMAT('INDIVIDUAL CANCER INCIDENCE BY DISTANCE AND DIRECTION 
     .(RISK)',/, 7x,41F8.0)
      NDT=1
      DO ND = 1, NDIR
        NDT1 = NDT+NDIST-1
        WRITE(NEPA,43)DIR(ND), (SUMI(IT,I,LORG), I = NDT,NDT1)
        NDT = NDT+NDIST
      END DO
   43 FORMAT(F7.0, 41(1PE8.1))
      END IF
      IF (CANFTF .AND. (NPOINTS .GT. 1) ) THEN
C     INDIVIDUAL CANCER FATALITIES BY DISTANCE AND DIRECTION
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,300)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,44) (DIST(I), I=1, NDIST)
   44 FORMAT('INDIVIDUAL CANCER FATALITIES BY DISTANCE AND DIRECTION 
     .(RISK)',/, 7x,41F8.0)
      NDT=1
      DO ND = 1, NDIR
        NDT1 = NDT+NDIST-1
        WRITE(NEPA,45)DIR(ND), (SUMF(IT,I,LORG), I = NDT,NDT1)
        NDT = NDT+NDIST
      END DO
   45 FORMAT(F7.0, 41(1PE8.1))
      END IF
C
C     INDIVIDUAL EFFECTIVE DOSE AND RISK BY NUCLIDE
C
      EDPART = 0.0
	EDTRIT = 0.0
	EDNOBL = 0.0
	EDIOD =  0.0
	EDCARB = 0.0
C
      DO IN = 1, JNUC
        DO IV = 1, NVALS
          IF(DOSETF) EDbyNUC(IN) = EDbyNUC(IN) + 
     .                            DOSE(IT,IN,IPTMAX,IV,MORG)
     .                            *remSv(IremSv+1)
          IF(CANITF) RIbyNUC(IN) = RIbyNUC(IN) + 
     .                             CANCERI(IT,IN,IPTMAX,IV,LORG)
          IF(CANFTF) RFbyNUC(IN) = RFbyNUC(IN) + 
     .                             CANCERF(IT,IN,IPTMAX,IV,LORG)
C
	    IF((SEQI(NUKID(IN),'H3',2)) .OR. (SEQI(NUKID(IN),'OB',2)))THEN
           EDTRIT = EDTRIT + DOSE(IT,IN,IPTMAX,IV,MORG)
     .                    *remSv(IremSv+1)
	    ELSE IF(SEQI(NUKID(IN),'C14',3)) THEN
            EDCARB = EDCARB + DOSE(IT,IN,IPTMAX,IV,MORG)*remSv(IremSv+1)
	    ELSE IF((SEQI(NUKID(IN),'AR',2)) .OR. (SEQI(NUKID(IN),'KR',2))
     .  .OR. (SEQI(NUKID(IN),'XE',2)) .OR. (SEQI(NUKID(IN),'RN',2)))THEN
           EDNOBL = EDNOBL + DOSE(IT,IN,IPTMAX,IV,MORG)*remSv(IremSv+1)
          ELSE IF(SEQI(NUKID(IN),'I1',2)) THEN
	     EDIOD = EDIOD + DOSE(IT,IN,IPTMAX,IV,MORG)*remSv(IremSv+1)
		ELSE
	      EDPART = EDPART + DOSE(IT,IN,IPTMAX,IV,MORG)*remSv(IremSv+1)
          END IF
C
        END DO
      END DO
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,60)IPTMAX, remSvNam(IremSv+1)
      WRITE(NEPA,300)LOWAGE(IAGE), UPAGE(IAGE)
   60 FORMAT('MAXIMUM LOCATION = ',I4,/,'INDIVIDUAL EFFECTIVE DOSE 
     .AND RISK BY NUCLIDE',/,'NUCLIDE',4X,'DOSE (',A4,')  CANCER ',
     .'INCIDENCE  CANCER FATALITIES')
      DO I = 1, JNUC
        WRITE(NEPA,61)NUKID(I),EDbyNUC(I),RIbyNUC(I),RFbyNUC(I)
      END DO
   61 FORMAT(A6,4X,1PE10.2,5X,E10.2,5X,E10.2)
      WRITE(NEPA,69)remSvNam(IremSv+1),EDTRIT,EDCARB,EDNOBL,EDIOD,EDPART
   69 FORMAT(/, 'EFFECTIVE DOSE (',A4,')BY RELEASE CATEGORY AT MAXIMUM ',
     . 'INDIVIDUAL LOCATION:',/,
     . '  TRITIUM (PLUS OBT)  : ', 1PE8.2,/,
     . '  CARBON-14           : ', 1PE8.2,/,
     . '  NOBLE GASES         : ', 1PE8.2,/,
     . '  IODINE RADIONUCLIDES: ', 1PE8.2,/,
     . '  PARTICULATE NUCLIDES: ', 1PE8.2,/)
C
C    INDIVIDUAL EFFECTIVE DOSE AND RISK BY PATHWAY
C
      DO IVAL = 1, NVALS
        DO IN = 1, JNUC
          IF(DOSETF) EDbyPATH(IVAL) = EDbyPATH(IVAL) + 
     .                                DOSE(IT,IN,IPTMAX,IVAL,MORG)
     .                                *remSv(IremSv+1)
          IF(CANITF) RIbyPATH(IVAL) = RIbyPATH(IVAL) +
     .                                CANCERI(IT,IN,IPTMAX,IVAL,LORG)
          IF(CANFTF) RFbyPATH(IVAL) = RFbyPATH(IVAL) +
     .                                CANCERF(IT,IN,IPTMAX,IVAL,LORG)
        END DO
      END DO
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,300)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,62)IPTMAX, remSvNam(IremSv+1)
   62 FORMAT('MAXIMUM LOCATION = ',I4,/,'INDIVIDUAL EFFECTIVE DOSE 
     .AND RISK BY EXPOSURE PATHWAY',/,'PATHWAY',17X,'ROUTE',10X,
     .'DOSE (',A4,')  CANCER INCIDENCE  CANCER FATALITIES')
C     NEED TO ACCOUNT FOR THE NVAL=NPATHWY*MCOUNT PROBLEM HERE
C      RECALL THAT GENII ALWAYS OUTPUTS IN ORDER INCIDENCE, FATALITY, DOSE
      DO I = 1, NVALS, MCOUNT
        WRITE(NEPA,63)PATH(I),ROUTE(I),EDbyPATH(I+MCI+MCF),RIbyPATH(I),
     .                RFbyPATH(I+MCI)
      END DO
   63 FORMAT(A22,A12,4X,1PE10.2,5X,E10.2,5X,E10.2)
C
C   For NRC, inserted By Pathway By Nuclide table for individual doses (only).
C
      if (pathnuc) then
      Write(NEPA,65)
   65 format(120('-'),/,'INDIVIDUAL EFFECTIVE DOSE AND RISK BY PATHWAY '
     .  ,'BY RADIONUCLIDE')
      DO IP = mcount,NVALS,mcount
	WRITE (NEPA,651) PATH(IP), ROUTE(IP), RemSvNam(iREMsV+1)
  651 FORMAT (/,'INDIVIDUAL DOSE FOR PATHWAY: ',A17,A10,/,
     . 12x,'DOSE (',A4,')    INCIDENCE   FATALITIES  CONTRIBUTION')
c  many of the same permutations allowed here as above!
      DO IN=1,JNUC
	  percnt = 0.0
	  if (edbypath(ip) .gt. 0.0) then
	    percnt = Dose(it,in,iptmax,ip,Morg)*100./
     .               (edbyPath(ip)/remSv(iremSv+1))
	  end if
      if (dosetf .and. canitf .and. canftf)then
	WRITE(NEPA,652) NUKID(IN),DOSE(IT,IN,IPTMAX,IP,MORG)
     .  *remSv(iremsv+1), CANCERI(IT,IN,IPTMAX,IP-2,LORG), 
     .   CANCERF(IT,IN,IPTMAX,IP-1,LORG), percnt
      else if (dosetf .and. canitf .and. (.not. canftf))then
		WRITE(NEPA,6522) NUKID(IN),DOSE(IT,IN,IPTMAX,IP,MORG)
     .  *remSv(iremsv+1), CANCERI(IT,IN,IPTMAX,IP-1,LORG),
     .   percnt
      else if (dosetf .and. (.not. canitf) .and. canftf)then
	WRITE(NEPA,6521) NUKID(IN),DOSE(IT,IN,IPTMAX,IP,MORG)
     .  *remSv(iremsv+1), CANCERF(IT,IN,IPTMAX,IP-1,LORG),
     .  percnt
	else if (dosetf .and. (.not. canitf) .and. (.not. canftf))then
	WRITE(NEPA,6523) NUKID(IN),DOSE(IT,IN,IPTMAX,IP,MORG)
     .  *remSv(iremsv+1), percnt
	end if
  652 FORMAT(1x,A8, 2X, 3(2X,1PE10.2),0pf10.2,'%')
 6521 format(1x,a8, 4x, 1pe10.2e2, 14x, 1pe10.2e2,0pf10.2,'%')
 6522 format(1x,a8, 4x, 1pe10.2e2, 3x, 1pe10.2e2, 11x,0pf10.2,'%')
 6523 format(1x,a8, 4x, 1pe10.2e2, 24x,0pf10.2,'%')

	END DO !
	END DO !
      endif
C
C     POPULATION SUMMARY
C#######################
c#######THE FOLLOWING SECTION ONLY APPLIES IF NO FOOD DISTRIBUTION NETWORK USED
C#######################
c
      IF (POPTF .and. (.NOT. FODTF)) THEN   ! POPULATION LOGIC     
C
C     NEED 7 TYPES OF TABLES HERE FOR OPTIONS:
C
C  1.
      IF (DOSETF .AND. CANITF .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,64)LOWAGE(IAGE), UPAGE(IAGE), PremSvNam(IremSv+5)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,21)ORGNAM(I),SUMDP(IT,I), SITENAM(I), 
     .  SUMPI(IT,I), SUMPF(IT,I)
      ELSE
        WRITE(NEPA,22)ORGNAM(I),SUMDP(IT,I)
   64 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'ORGAN',7X,'DOSE (,'A4,')',3X,'TISSUE',5X,'CANCER INCIDENCE',
     .       '   CANCER FATALITIES')
      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. CANITF .AND. CANFTF)
C  2.
      IF (DOSETF .AND. CANITF .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,640)LOWAGE(IAGE), UPAGE(IAGE), PremSvNam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,210)ORGNAM(I),SUMDP(IT,I), SITENAM(I), 
     .  SUMPI(IT,I)
      ELSE
        WRITE(NEPA,220)ORGNAM(I),SUMDP(IT,I)
  640 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'ORGAN',7X,'DOSE (',A10,')',3X,'TISSUE',5X,'CANCER INCIDENCE')
      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. CANITF .AND. (.NOT. CANFTF))
C  3.
      IF (DOSETF .AND. (.NOT. CANITF) .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,641)LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,211)ORGNAM(I),SUMDP(IT,I), SITENAM(I), 
     .  SUMPF(IT,I)
      ELSE
        WRITE(NEPA,220)ORGNAM(I),SUMDP(IT,I), PremSvNam(IremSv+1)
  641 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'ORGAN',7X,'DOSE (',A10,')',3X,'TISSUE',5X,'CANCER FATALITIES')
      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. (.NOT. CANITF) .AND. CANFTF)
C  4.
      IF (DOSETF .AND. (.NOT. CANITF) .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,642)LOWAGE(IAGE), UPAGE(IAGE), PremSvNam(IremSv+1)
      DO I=1,NORGAN
      WRITE(NEPA,212)ORGNAM(I),SUMDP(IT,I)
  642 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'ORGAN',7X,'DOSE (',A10,')')
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. (.NOT. CANITF) .AND. (.NOT. CANFTF))
C  5.
      IF ((.NOT.DOSETF) .AND. CANITF .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,643)LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NSITES
      WRITE(NEPA,213)SITENAM(I), SUMPI(IT,I), SUMPF(IT,I)
  643 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'TISSUE',5X,'CANCER INCIDENCE','   CANCER FATALITIES')
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  ((.NOT. DOSETF) .AND. CANITF .AND. CANFTF)
C  6.
      IF ((.NOT.DOSETF) .AND. CANITF .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,644)LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NSITES
      WRITE(NEPA,214)SITENAM(I), SUMPI(IT,I)
  644 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'TISSUE',5X,'CANCER INCIDENCE')
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  ((.NOT. DOSETF) .AND. CANITF .AND. (.NOT. CANFTF))
C  7.
      IF ((.NOT.DOSETF) .AND. (.NOT. CANITF) .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,645)LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NSITES
      WRITE(NEPA,213)SITENAM(I), SUMPF(IT,I)
  645 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'TISSUE',5X,'CANCER FATALITIES')
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  ((.NOT. DOSETF) .AND. (.NOT. CANITF) .AND. CANFTF)
C
C
      END IF  ! POPULATION LOGIC
C
C     POPULATION EFFECTIVE DOSE BY DISTANCE AND DIRECTION
C
      IF (DOSETF .AND. POPTF .and. (.NOT. FODTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
  301 FORMAT('POPULATION GROUP AGE RANGE',I3,' TO',I3,' YEARS')
      WRITE(NEPA,50) PremSvNam(IremSv+1),(DIST(I), I=1, NDIST)
   50 FORMAT('POPULATION EFFECTIVE DOSE BY DISTANCE AND DIRECTION 
     .(',A10,')',/,7X,41F8.0)
      NDT=1
      DO ND = 1, NDIR
        NDT1 = NDT+NDIST-1
        WRITE(NEPA,51)DIR(ND),((SUMD(IT,I,MORG)*POP(IAGE,ND,I-NDT+1)
     .   *PREMsV(IREMSV+1)/REMsV(IREMSV+1)), 
     .                         I = NDT,NDT1)
        NDT = NDT+NDIST
      END DO
   51 FORMAT(F7.0, 41(1PE8.1))
      END IF
C     POPULATION CANCER INCIDENCE BY DISTANCE AND DIRECTION
      IF (CANITF .AND. POPTF .and. (.not. fodtf)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,52) (DIST(I), I=1, NDIST)
   52 FORMAT('POPULATION CANCER INCIDENCE BY DISTANCE AND DIRECTION 
     .(RISK)',/,7X,41F8.0)
      NDT=1
      DO ND = 1, NDIR
        NDT1 = NDT+NDIST-1
        WRITE(NEPA,53)DIR(ND),((SUMI(IT,I,LORG)*POP(IAGE,ND,I-NDT+1)), 
     .                         I = NDT,NDT1)
        NDT = NDT+NDIST
      END DO
   53 FORMAT(F7.0, 41(1PE8.1))
      END IF
C     POPULATION CANCER FATALITIES BY DISTANCE AND DIRECTION
      IF (CANFTF .AND. POPTF .and. (.not. fodtf)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,54) (DIST(I), I=1, NDIST)
   54 FORMAT('POPULATION CANCER FATALITIES BY DISTANCE AND DIRECTION 
     .(RISK)',/,7X,41F8.0)
      NDT=1
      DO ND = 1, NDIR
        NDT1 = NDT+NDIST-1
        WRITE(NEPA,55)DIR(ND),((SUMF(IT,I,LORG)*POP(IAGE,ND,I-NDT+1)), 
     .                         I = NDT,NDT1)
        NDT = NDT+NDIST
      END DO
   55 FORMAT(F7.0, 41(1PE8.1))
      END IF
C
C     POPULATION EFFECTIVE DOSE AND RISK BY NUCLIDE
C
      IF (POPTF .and. (.NOT. FODTF)) THEN   ! POPULATION LOGIC     
C
      EDPART = 0.0
	EDTRIT = 0.0
	EDNOBL = 0.0
	EDIOD =  0.0
	EDCARB = 0.0
      DO IN = 1, JNUC
        DO IPT = 1,NPOINTS
          NDR = (IPT-1)/NDIST+1
          NDST = MOD(IPT,NDIST)
          IF(NDST .EQ. 0) NDST = NPOINTS/NDIR
          IF(NDST .EQ. 0) NDST = 1
          DO IV = 1, NVALS
           IF (DOSETF) EDbyNUCP(IN) = EDbyNUCP(IN) + 
     .                 DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
           IF (CANITF) RIbyNUCP(IN) = RIbyNUCP(IN) + 
     .                 CANCERI(IT,IN,IPT,IV,LORG) * POP(IAGE,NDR,NDST)
           IF (CANFTF) RFbyNUCP(IN) = RFbyNUCP(IN) + 
     .                 CANCERF(IT,IN,IPT,IV,LORG) * POP(IAGE,NDR,NDST)
C
	    IF((SEQI(NUKID(IN),'H3',2)) .OR. (SEQI(NUKID(IN),'OB',2)))THEN
           EDTRIT = EDTRIT +DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
	    ELSE IF(SEQI(NUKID(IN),'C14',3)) THEN
            EDCARB = EDCARB+DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
	    ELSE IF((SEQI(NUKID(IN),'AR',2)) .OR. (SEQI(NUKID(IN),'KR',2))
     .  .OR. (SEQI(NUKID(IN),'XE',2)) .OR. (SEQI(NUKID(IN),'RN',2)))THEN
           EDNOBL = EDNOBL +DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
          ELSE IF(SEQI(NUKID(IN),'I1',2)) THEN
	     EDIOD = EDIOD + DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
		ELSE
	      EDPART = EDPART+DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
          END IF          	
		END DO
        END DO
      END DO
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,70)PremSvNam(IremSv+5)
   70 FORMAT('POPULATION EFFECTIVE DOSE 
     .AND RISK BY NUCLIDE',/,'NUCLIDE',4X,'DOSE (',A4,')  CANCER ',
     .'INCIDENCE  CANCER FATALITIES')
      DO I = 1, JNUC
        WRITE(NEPA,61)NUKID(I),EDbyNUCP(I),RIbyNUCP(I),RFbyNUCP(I)
      END DO
   71 FORMAT(A6,4X,1PE10.2,5X,E10.2,5X,E10.2)

C
C
      WRITE(NEPA,79)PremSvNam(IremSv+1),EDTRIT,EDCARB,EDNOBL,EDIOD,
     . EDPART
   79 FORMAT(/,'POPULATION EFFECTIVE DOSE (',A10,')BY RELEASE CATEGORY:'
     .,/,'  TRITIUM (PLUS OBT)  : ', 1PE8.2,/,
     .   '  CARBON-14           : ', 1PE8.2,/,
     .   '  NOBLE GASES         : ', 1PE8.2,/,
     .   '  IODINE RADIONUCLIDES: ', 1PE8.2,/,
     .   '  PARTICULATE NUCLIDES: ', 1PE8.2,/)
C
      END IF  ! POPULATION LOGIC
C
C    POPULATION EFFECTIVE DOSE AND RISK BY PATHWAY
C
      IF (POPTF .and. (.NOT. FODTF)) THEN   ! POPULATION LOGIC     
C
      DO IVAL = 1, NVALS
        DO IPT=1,NPOINTS
          NDR = (IPT-1)/NDIST+1
          NDST = MOD(IPT,NDIST)
          IF(NDST .EQ. 0) NDST = NPOINTS/NDIR
          IF(NDST .EQ. 0) NDST = 1
          DO IN = 1, JNUC
            IF(DOSETF) EDbyPATHP(IVAL) = EDbyPATHP(IVAL) +
     .                 DOSE(IT,IN,IPT,IVAL,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
            IF(CANITF) RIbyPATHP(IVAL) = RIbyPATHP(IVAL) + 
     .           CANCERI(IT,IN,IPT,IVAL,LORG) * POP(IAGE,NDR,NDST)
            IF (CANFTF) RFbyPATHP(IVAL) = RFbyPATHP(IVAL) + 
     .           CANCERF(IT,IN,IPT,IVAL,LORG) * POP(IAGE,NDR,NDST)
          END DO
        END DO
      END DO
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,72) PremSvNam(IremSv+5)
   72 FORMAT('POPULATION EFFECTIVE DOSE 
     .AND RISK BY EXPOSURE PATHWAY',/,'PATHWAY',30X,'DOSE (',A4,')  
     .CANCER INCIDENCE  CANCER FATALITIES')
C     NEED TO ACCOUNT FOR THE NVAL=NPATHWY*MCOUNT PROBLEM HERE
C      RECALL THAT GENII ALWAYS OUTPUTS IN ORDER INCIDENCE, FATALITY, DOSE
      DO I = 1, NVALS, MCOUNT
        WRITE(NEPA,63)PATH(I),ROUTE(I),EDbyPATHP(I+MCI+MCF),RIbyPATHP(I)
     .                ,RFbyPATHP(I+MCI)
      END DO
   73 FORMAT(A22,A12,4X,1PE10.2,6X,E10.2,6X,E10.2)
      END IF  !  POPULATION LOGIC

C
C     POPULATION SUMMARY
C#######################
c#######THE FOLLOWING SECTION ONLY APPLIES IF A FOOD DISTRIBUTION NETWORK IS USED
C#######################
C
      IF (POPTF .AND. FODTF) THEN
C
C This section is for population doses WITH food production option
c
      call consump(nages,ndist,NDIR,pop,npoints,PATH,NVALS,food,totprod,
     .             TOTCONS,pthon)
c 


      SUMPI=0.0
	sumdp=0.0
	sumpf=0.0
c
        DO IPT = 1, NPOINTS
          NDR = (IPT-1)/NDIST+1
          NDST = MOD(IPT,NDIST)
          IF(NDST .EQ. 0) NDST = npoints/ndir
          IF(NDST .EQ. 0) NDST = 1
          DO INUC = 1, JNUC
            DO IVAL = 1, NVALS
             if((SEQI(path(ival),'air ',4)) 
     .     .or. (SEQI(path(ival),'ground',6))
     .     .or. (SEQI(path(ival),'Indoor air',10)) 
     .     .or. (SEQI(path(ival),'soil',4))
     .     .or. (.not. pthon(ival))) then
c  "normal" pathways 
              DO IORG = 1, NORGAN
                IF(DOSETF) THEN
                SUMDP(IT,IORG)=SUMDP(IT,IORG) +
     .                 DOSE(IT,INUC,IPT,IVAL,IORG)*POP(IAGE,NDR,NDST)
     .               *PremSv(IremSv+1)      
                END IF
              END DO ! NORGAN
              DO ISITE = 1, NSITES
                IF (CANFTF) THEN
            SUMPF(IT,ISITE)=SUMPF(IT,ISITE)
     .              + CANCERF(IT,INUC,IPT,IVAL,ISITE)*POP(IAGE,NDR,NDST)
                END IF
                IF (CANITF) THEN
                SUMPI(IT,ISITE)=SUMPI(IT,ISITE)
     .              + CANCERI(IT,INUC,IPT,IVAL,ISITE)*POP(IAGE,NDR,NDST)
                END IF
              END DO ! NSITES
	      ELSE
c  "food network" pathways
C  Calculate age-specific production-weighted dose/kg - by nuclide by pathway
C  (remember, we are inside a times loop, inside an age loop)
	        kpth=0
	        if(SEQI(path(ival),'leaf',4))kpth=1
	        if(SEQI(path(ival),'root',4))kpth=2
	        if(SEQI(path(ival),'frui',4))kpth=3
	        if(SEQI(path(ival),'grai',4))kpth=4
	        if(SEQI(path(ival),'meat',4))kpth=5
	        if(SEQI(path(ival),'poul',4))kpth=6
	        if(SEQI(path(ival),'milk',4))kpth=7
	        if(SEQI(path(ival),'eggs',4))kpth=8
              IF (SEQI(ENDPT(IVAL),RADDOSE,8)) THEN 
	         do iorg = 1,norgan
	           if(kpth. le.4) then
                 AvePWtDose(Inuc,Ival,iorg) = AvePWtDose(inuc,ival,iorg) 
     .           + food(kpth,ndr,ndst)*dose(it,inuc,ipt,ival,iorg)/
     .           indconsC(kpth,IAGE)/totprod(ival)
	           else if (kpth .ge.5) then
                 AvePWtDose(Inuc,Ival,iorg) = AvePWtDose(inuc,ival,iorg) 
     .           + food(kpth,ndr,ndst)*dose(it,inuc,ipt,ival,iorg)/
     .           indconsA(kpth-4,IAGE)/totprod(ival)
	           endiF
	           end do ! NORGAN
	         ENDIF ! DOSETF
	         DO ISITE = 1, NSITES
	          IF (SEQI(ENDPT(IVAL),CANINC,8)) THEN
 	           if(kpth. le.4) then
                 AvePWtCanI(Inuc,Ival,iSITE)=AvePWtcANI(inuc,ival,iSITE) 
     .           + food(kpth,ndr,ndst)*CANCERI(IT,INUC,IPT,IVAL,ISITE)/
     .           indconsC(kpth,IAGE)/totprod(ival)
	           else if (kpth .ge.5) then
                 AvePWtCanI(Inuc,Ival,iSITE)=AvePWtCanI(inuc,ival,iSITE) 
     .           + food(kpth,ndr,ndst)*CANCERI(IT,INUC,IPT,IVAL,ISITE)/
     .           indconsA(kpth-4,IAGE)/totprod(ival)
	           endiF
	          ENDIF ! CANiTF
	          IF (SEQI(ENDPT(IVAL),CANFAT,8)) THEN
 	           if(kpth. le.4) then
                 AvePWtCanF(Inuc,Ival,iSITE)=AvePWtcANF(inuc,ival,iSITE) 
     .           + food(kpth,ndr,ndst)*CANCERF(IT,INUC,IPT,IVAL,ISITE)/
     .           indconsC(kpth,IAGE)/totprod(ival)
	           else if (kpth .ge.5) then
                 AvePWtCanF(Inuc,Ival,iSITE)=AvePWtCanF(inuc,ival,ISITE) 
     .           + food(kpth,ndr,ndst)*CANCERF(IT,INUC,IPT,IVAL,ISITE)/
     .           indconsA(kpth-4,IAGE)/totprod(ival)
	           endiF
	          ENDIF ! CANFTF
	         END DO ! NSITES
	        END IF ! AIR/GROUND/SOIL...
            END DO ! NVALS
          END DO ! JNUC
        END DO ! NPOINTS
C CALCUATE FOOD NETWORK POPULATION DOSES
	       DO  IPT = 1, NPOINTS
              NDR = (IPT-1)/NDIST+1
              NDST = MOD(IPT,NDIST)
              IF(NDST .EQ. 0) NDST = npoints/ndir
              IF(NDST .EQ. 0) NDST = 1
	        DO INUC = 1, JNUC
               DO IVAL = 1, NVALS
	          kpthC=0
	          kpthA=0
	          if(SEQI(path(ival),'leaf',4))kpth=1
	          if(SEQI(path(ival),'root',4))kpth=2
	          if(SEQI(path(ival),'frui',4))kpth=3
	          if(SEQI(path(ival),'grai',4))kpth=4
	          if(SEQI(path(ival),'meat',4))kpth=5
	          if(SEQI(path(ival),'poul',4))kpth=6
	          if(SEQI(path(ival),'milk',4))kpth=7
	          if(SEQI(path(ival),'eggs',4))kpth=8
	            IF (KPTH .EQ. 0) GO TO 1234
	            IF (TOTPROD(IVAL) .LE. 0.0) GO TO 1234
	           FRACf = TOTPROD(IVAL)/TOTCONS(IVAL)
	           if (fracf .gt. 1.0) fracf = 1.0
c  CUMULATIVE ORGAN DOSES
	          do iorg = 1,norgan
	           if(kpth. le.4) then
                 SUMDP(IT,IORG)=SUMDP(IT,IORG) +   
     .           POP(IAGE,NDR,NDST)*INDCONSc(KPTH,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,iorg)*PremSv(IremSv+1)
                 else if (kpth .ge.5) then
                 SUMDP(IT,IORG)=SUMDP(IT,IORG) +   
     .           POP(IAGE,NDR,NDST)*INDCONSa(KPTH-4,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,iorg)*PremSv(IremSv+1)
	           END IF
	          END DO ! IORG
C CUMULATIVE ORGAN RISKS
              DO ISITE = 1, NSITES
                 IF (CANFTF) THEN
	             if(kpth. le.4) then
                    SUMPF(IT,ISITE)=SUMPF(IT,ISITE)
     .              + AvePWtCanF(Inuc,Ival,iSITE)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSc(KPTH,iage)
	             ENDIF
	             if(kpth. Ge.5) then
                    SUMPF(IT,ISITE)=SUMPF(IT,ISITE)
     .              + AvePWtCanF(Inuc,Ival,iSITE)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSa(KPTH-4,iage)
                    END IF
                 END IF
                IF (CANITF) THEN
	            if(kpth. le.4) then
                    SUMPI(IT,ISITE)=SUMPI(IT,ISITE)
     .              + AvePWtCanI(Inuc,Ival,iSITE)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSc(KPTH,iage)
	            ENDIF
	            if(kpth. Ge.5) then
                    SUMPI(IT,ISITE)=SUMPI(IT,ISITE)
     .              + AvePWtCanI(Inuc,Ival,iSITE)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSa(KPTH-4,iage)
                  END IF
	          END IF
              END DO ! NSITES

 1234             CONTINUE
               END DO ! IVAL
	        END DO ! INUC
	       END DO ! IPT

C
C     NEED 7 TYPES OF TABLES HERE FOR OPTIONS:
C
C  1.
      IF (DOSETF .AND. CANITF .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,64)LOWAGE(IAGE), UPAGE(IAGE), PremSvNam(IremSv+5)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,21)ORGNAM(I),SUMDP(IT,I), SITENAM(I), 
     .  SUMPI(IT,I), SUMPF(IT,I)
      ELSE
        WRITE(NEPA,22)ORGNAM(I),SUMDP(IT,I)

      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. CANITF .AND. CANFTF)
C  2.
      IF (DOSETF .AND. CANITF .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,640)LOWAGE(IAGE), UPAGE(IAGE), PremSvNam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,210)ORGNAM(I),SUMDP(IT,I), SITENAM(I), 
     .  SUMPI(IT,I)
      ELSE
        WRITE(NEPA,220)ORGNAM(I),SUMDP(IT,I)

      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. CANITF .AND. (.NOT. CANFTF))
C  3.
      IF (DOSETF .AND. (.NOT. CANITF) .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,641)LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,211)ORGNAM(I),SUMDP(IT,I), SITENAM(I), 
     .  SUMPF(IT,I)
      ELSE
        WRITE(NEPA,220)ORGNAM(I),SUMDP(IT,I), PremSvNam(IremSv+1)

      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. (.NOT. CANITF) .AND. CANFTF)
C  4.
      IF (DOSETF .AND. (.NOT. CANITF) .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,642)LOWAGE(IAGE), UPAGE(IAGE), PremSvNam(IremSv+1)
      DO I=1,NORGAN
      WRITE(NEPA,212)ORGNAM(I),SUMDP(IT,I)

      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. (.NOT. CANITF) .AND. (.NOT. CANFTF))
C  5.
      IF ((.NOT.DOSETF) .AND. CANITF .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,643)LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NSITES
      WRITE(NEPA,213)SITENAM(I), SUMPI(IT,I), SUMPF(IT,I)

      END DO  !  NORGAN PRINTING LOOP
      END IF  !  ((.NOT. DOSETF) .AND. CANITF .AND. CANFTF)
C  6.
      IF ((.NOT.DOSETF) .AND. CANITF .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,644)LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NSITES
      WRITE(NEPA,214)SITENAM(I), SUMPI(IT,I)
 
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  ((.NOT. DOSETF) .AND. CANITF .AND. (.NOT. CANFTF))
C  7.
      IF ((.NOT.DOSETF) .AND. (.NOT. CANITF) .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,645)LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NSITES
      WRITE(NEPA,213)SITENAM(I), SUMPF(IT,I)

      END DO  !  NORGAN PRINTING LOOP
      END IF  !  ((.NOT. DOSETF) .AND. (.NOT. CANITF) .AND. CANFTF)
C
C
C
C     POPULATION EFFECTIVE DOSE AND RISK BY NUCLIDE
C    
C
      EDPART = 0.0
	EDTRIT = 0.0
	EDNOBL = 0.0
	EDIOD =  0.0
	EDCARB = 0.0
      DO IN = 1, JNUC
        DO IPT = 1,NPOINTS
          NDR = (IPT-1)/NDIST+1
          NDST = MOD(IPT,NDIST)
          IF(NDST .EQ. 0) NDST = NPOINTS/NDIR
          IF(NDST .EQ. 0) NDST = 1
          DO IV = 1, NVALS
             if((SEQI(path(iv),'air ',4)) 
     .     .or. (SEQI(path(iv),'ground',6))
     .     .or. (SEQI(path(iv),'Indoor air',10)) 
     .     .or. (SEQI(path(iv),'soil',4))
     .     .or. (.not. pthon(iv))) then
c  "normal" pathways 
           IF (DOSETF) EDbyNUCP(IN) = EDbyNUCP(IN) + 
     .                 DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
           IF (CANITF) RIbyNUCP(IN) = RIbyNUCP(IN) + 
     .                 CANCERI(IT,IN,IPT,IV,LORG) * POP(IAGE,NDR,NDST)
           IF (CANFTF) RFbyNUCP(IN) = RFbyNUCP(IN) + 
     .                 CANCERF(IT,IN,IPT,IV,LORG) * POP(IAGE,NDR,NDST)
C
	    IF((SEQI(NUKID(IN),'H3',2)) .OR. (SEQI(NUKID(IN),'OB',2)))THEN
           EDTRIT = EDTRIT +DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
	    ELSE IF(SEQI(NUKID(IN),'C14',3)) THEN
            EDCARB = EDCARB+DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
	    ELSE IF((SEQI(NUKID(IN),'AR',2)) .OR. (SEQI(NUKID(IN),'KR',2))
     .  .OR. (SEQI(NUKID(IN),'XE',2)) .OR. (SEQI(NUKID(IN),'RN',2)))THEN
           EDNOBL = EDNOBL +DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
          ELSE IF(SEQI(NUKID(IN),'I1',2)) THEN
	     EDIOD = EDIOD + DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
		ELSE
	      EDPART = EDPART+DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
          END IF 
		else
c  "food network" pathways - start
                ival = iv
	          inuc=in
	          kpth=0
	          if(SEQI(path(ival),'leaf',4))kpth=1
	          if(SEQI(path(ival),'root',4))kpth=2
	          if(SEQI(path(ival),'frui',4))kpth=3
	          if(SEQI(path(ival),'grai',4))kpth=4
	          if(SEQI(path(ival),'meat',4))kpth=5
	          if(SEQI(path(ival),'poul',4))kpth=6
	          if(SEQI(path(ival),'milk',4))kpth=7
	          if(SEQI(path(ival),'eggs',4))kpth=8
	            IF (KPTH .EQ. 0) GO TO 1235
	            IF (TOTPROD(IVAL) .LE. 0.0) GO TO 1235
	           FRACf = TOTPROD(IVAL)/TOTCONS(IVAL)
	           if (fracf .gt. 1.0) fracf = 1.0
c  CUMULATIVE ORGAN DOSES
                 if (dosetf) then
	           if(kpth. le.4) then
                 EDbyNUCP(IN) = EDbyNUCP(IN) +  
     .           POP(IAGE,NDR,NDST)*INDCONSc(KPTH,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
c - also need to update the H3,C14,noble,iodine,particulate bits here...
	    IF((SEQI(NUKID(IN),'H3',2)) .OR. (SEQI(NUKID(IN),'OB',2)))THEN
           EDTRIT = EDTRIT +
     .           POP(IAGE,NDR,NDST)*INDCONSc(KPTH,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
	    ELSE IF(SEQI(NUKID(IN),'C14',3)) THEN
            EDCARB = EDCARB+
     .           POP(IAGE,NDR,NDST)*INDCONSc(KPTH,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
	    ELSE IF((SEQI(NUKID(IN),'AR',2)) .OR. (SEQI(NUKID(IN),'KR',2))
     .  .OR. (SEQI(NUKID(IN),'XE',2)) .OR. (SEQI(NUKID(IN),'RN',2)))THEN
           EDNOBL = EDNOBL +
     .           POP(IAGE,NDR,NDST)*INDCONSc(KPTH,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
          ELSE IF(SEQI(NUKID(IN),'I1',2)) THEN
	     EDIOD = EDIOD + 
     .           POP(IAGE,NDR,NDST)*INDCONSc(KPTH,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
		ELSE
	      EDPART = EDPART+
     .           POP(IAGE,NDR,NDST)*INDCONSc(KPTH,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
          END IF 
C - H3,c14,noble,iodine,particulate
                 else if (kpth .ge.5) then
                 EDbyNUCP(IN) = EDbyNUCP(IN) +  
     .           POP(IAGE,NDR,NDST)*INDCONSa(KPTH-4,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
c - also need to update the H3,C14,noble,iodine,particulate bits here...
	    IF((SEQI(NUKID(IN),'H3',2)) .OR. (SEQI(NUKID(IN),'OB',2)))THEN
           EDTRIT = EDTRIT +
     .           POP(IAGE,NDR,NDST)*INDCONSa(KPTH-4,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
	    ELSE IF(SEQI(NUKID(IN),'C14',3)) THEN
            EDCARB = EDCARB+
     .           POP(IAGE,NDR,NDST)*INDCONSa(KPTH-4,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
	    ELSE IF((SEQI(NUKID(IN),'AR',2)) .OR. (SEQI(NUKID(IN),'KR',2))
     .  .OR. (SEQI(NUKID(IN),'XE',2)) .OR. (SEQI(NUKID(IN),'RN',2)))THEN
           EDNOBL = EDNOBL +
     .           POP(IAGE,NDR,NDST)*INDCONSa(KPTH-4,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
          ELSE IF(SEQI(NUKID(IN),'I1',2)) THEN
	     EDIOD = EDIOD + 
     .           POP(IAGE,NDR,NDST)*INDCONSa(KPTH-4,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
		ELSE
	      EDPART = EDPART+
     .           POP(IAGE,NDR,NDST)*INDCONSa(KPTH-4,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
          END IF 
C - H3,c14,noble,iodine,particulate
	           END IF
                 end if
C CUMULATIVE ORGAN RISKS

                 IF (CANFTF) THEN
	             if(kpth. le.4) then
                    RFbyNUCP(IN) = RFbyNUCP(IN) +
     .              + AvePWtCanF(Inuc,Ival,nSITEs)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSc(KPTH,iage)
	             ENDIF
	             if(kpth. Ge.5) then
                    RFbyNUCP(IN) = RFbyNUCP(IN) +
     .              + AvePWtCanF(Inuc,Ival,nSITEs)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSa(KPTH-4,iage)
                    END IF
                 END IF
                IF (CANITF) THEN
	            if(kpth. le.4) then
                    RIbyNUCP(IN) = RIbyNUCP(IN) +
     .              + AvePWtCanI(Inuc,Ival,nSITEs)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSc(KPTH,iage)
	            ENDIF
	            if(kpth. Ge.5) then
                    RIbyNUCP(IN) = RIbyNUCP(IN) +
     .              + AvePWtCanI(Inuc,Ival,nSITEs)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSa(KPTH-4,iage)
                  END IF
	          END IF


 1235             CONTINUE
c  "food network" pathways - end		
		end if         	
		END DO
        END DO
      END DO
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,70)PremSvNam(IremSv+5)

      DO I = 1, JNUC
        WRITE(NEPA,61)NUKID(I),EDbyNUCP(I),RIbyNUCP(I),RFbyNUCP(I)
      END DO
C
C
      WRITE(NEPA,79)PremSvNam(IremSv+1),EDTRIT,EDCARB,EDNOBL,EDIOD,
     . EDPART
C
C    POPULATION EFFECTIVE DOSE AND RISK BY PATHWAY
C
C
      DO IVAL = 1, NVALS
c  "normal" pathways 
        DO IPT=1,NPOINTS
          NDR = (IPT-1)/NDIST+1
          NDST = MOD(IPT,NDIST)
          IF(NDST .EQ. 0) NDST = NPOINTS/NDIR
          IF(NDST .EQ. 0) NDST = 1
          DO IN = 1, JNUC
             if((SEQI(path(ival),'air ',4)) 
     .     .or. (SEQI(path(ival),'ground',6))
     .     .or. (SEQI(path(ival),'Indoor air',10)) 
     .     .or. (SEQI(path(ival),'soil',4))
     .     .or. (.not. pthon(ival))) then
            IF(DOSETF) EDbyPATHP(IVAL) = EDbyPATHP(IVAL) +
     .                 DOSE(IT,IN,IPT,IVAL,MORG) * POP(IAGE,NDR,NDST)
     .                 *PremSv(IremSv+1)
            IF(CANITF) RIbyPATHP(IVAL) = RIbyPATHP(IVAL) + 
     .           CANCERI(IT,IN,IPT,IVAL,LORG) * POP(IAGE,NDR,NDST)
            IF (CANFTF) RFbyPATHP(IVAL) = RFbyPATHP(IVAL) + 
     .           CANCERF(IT,IN,IPT,IVAL,LORG) * POP(IAGE,NDR,NDST)

	  else
c  "food network" pathways - start
	          inuc=in
	          kpth=0
	          if(SEQI(path(ival),'leaf',4))kpth=1
	          if(SEQI(path(ival),'root',4))kpth=2
	          if(SEQI(path(ival),'frui',4))kpth=3
	          if(SEQI(path(ival),'grai',4))kpth=4
	          if(SEQI(path(ival),'meat',4))kpth=5
	          if(SEQI(path(ival),'poul',4))kpth=6
	          if(SEQI(path(ival),'milk',4))kpth=7
	          if(SEQI(path(ival),'eggs',4))kpth=8
	            IF (KPTH .EQ. 0) GO TO 1236
	            IF (TOTPROD(IVAL) .LE. 0.0) GO TO 1236
	           FRACf = TOTPROD(IVAL)/TOTCONS(IVAL)
	           if (fracf .gt. 1.0) fracf = 1.0
c  CUMULATIVE ORGAN DOSES
                 if (dosetf) then
	           if(kpth. le.4) then
                 EDbyPATHP(IVAL) = EDbyPATHP(IVAL) +  
     .           POP(IAGE,NDR,NDST)*INDCONSc(KPTH,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
                 else if (kpth .ge.5) then
                 EDbyPATHP(IVAL) = EDbyPATHP(IVAL) +  
     .           POP(IAGE,NDR,NDST)*INDCONSa(KPTH-4,iage)*FRACf*
     .           AvePWtDose(Inuc,Ival,morg)*PremSv(IremSv+1)
	           END IF
                 end if
C CUMULATIVE ORGAN RISKS

                 IF (CANFTF) THEN
	             if(kpth. le.4) then
                    RFbyPATHP(IVAL) = RFbyPATHP(IVAL) + 
     .              + AvePWtCanF(Inuc,Ival,nSITEs)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSc(KPTH,iage)
	             ENDIF
	             if(kpth. Ge.5) then
                    RFbyPATHP(IVAL) = RFbyPATHP(IVAL) + 
     .              + AvePWtCanF(Inuc,Ival,nSITEs)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSa(KPTH-4,iage)
                    END IF
                 END IF
                IF (CANITF) THEN
	            if(kpth. le.4) then
                    RIbyPATHP(IVAL) = RIbyPATHP(IVAL) +
     .              + AvePWtCanI(Inuc,Ival,nSITEs)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSc(KPTH,iage)
	            ENDIF
	            if(kpth. Ge.5) then
                    RIbyPATHP(IVAL) = RIbyPATHP(IVAL) +
     .              + AvePWtCanI(Inuc,Ival,nSITEs)*POP(IAGE,NDR,NDST)
     .              *FRACF*INDCONSa(KPTH-4,iage)
                  END IF
	          END IF


 1236             CONTINUE
c  "food network" pathways - end		
	  end if
          END DO
        END DO
      END DO
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,72) PremSvNam(IremSv+5)

C     NEED TO ACCOUNT FOR THE NVAL=NPATHWY*MCOUNT PROBLEM HERE
C      RECALL THAT GENII ALWAYS OUTPUTS IN ORDER INCIDENCE, FATALITY, DOSE
      DO I = 1, NVALS, MCOUNT
        WRITE(NEPA,63)PATH(I),ROUTE(I),EDbyPATHP(I+MCI+MCF),RIbyPATHP(I)
     .                ,RFbyPATHP(I+MCI)
      END DO
C
C
      END IF
C     END OF FOOD NETWORK POPULATION OUTPUT
       END DO  !  NTIMES
C     RESET STUFF FOR NEXT PASS
C
      IF (IAGE .EQ. NAGES) GO TO 30
      PRINT 995
  995 FORMAT('Zeroing for next pass')
C
      rewind (nref)
      IF(DOSETF) EDbyNUC = 0.0
      IF(CANITF) RIbyNUC = 0.0
      IF(CANFTF) RFbyNUC = 0.0
      IF(DOSETF .AND. POPTF) EDbyNUCP = 0.0
      IF(CANITF .AND. POPTF) RIbyNUCP = 0.0
      IF(CANFTF .AND. POPTF) RFbyNUCP = 0.0
      IF(DOSETF) EDbyPATH=0.0
      IF(CANITF) RIbyPATH=0.0
      IF(CANFTF) RFbyPATH=0.0
      IF(DOSETF .AND. POPTF) EDbyPATHP = 0.0
      IF(CANITF .AND. POPTF) RIbyPATHP = 0.0
      IF(CANFTF .AND. POPTF) RFbyPATHP = 0.0      
      IF(DOSETF) DOSE = 0.0
      IF(DOSETF) SUMD = 0.0
      IF(DOSETF .AND. POPTF) SUMDP = 0.0
      IF(CANITF .AND. POPTF) CANCERI = 0.0
      IF(CANFTF .AND. POPTF) CANCERI = 0.0
      IF(CANITF) SUMI = 0.0
      IF(CANFTF) SUMF = 0.0
      IF(CANITF .AND. POPTF) SUMPI = 0.0
      IF(CANFTF .AND. POPTF) SUMPF = 0.0
C
   30 CONTINUE
      END DO  ! NAGES  NAGES  NAGES  NAGES  NAGES  NAGES  NAGES  NAGES  NAGES NAGES  NAGES  NAGES 
C
      IF (PASS1 .EQ. .FALSE.)DEALLOCATE (POP) 
      IF (PASS2 .EQ. .FALSE.)DEALLOCATE (CANCERF)
      IF (PASS3 .EQ. .FALSE.)DEALLOCATE (CANCERI)
      IF (PASS4 .EQ. .FALSE.)DEALLOCATE (DOSE)
      IF (PASS5 .EQ. .FALSE.)DEALLOCATE (SUMD)
      IF (PASS6 .EQ. .FALSE.)DEALLOCATE (SUMDP)
      IF (PASS7 .EQ. .FALSE.)DEALLOCATE (SUMF)
      IF (PASS8 .EQ. .FALSE.)DEALLOCATE (SUMPF)
      IF (PASS9 .EQ. .FALSE.)DEALLOCATE (SUMI)
      IF (PASS10 .EQ. .FALSE.)DEALLOCATE (SUMPI)
      IF (PASS11 .EQ. .FALSE.)DEALLOCATE (POPI)
      PASS1 = .TRUE.
      PASS2 = .TRUE.
      PASS3 = .TRUE.
      PASS4 = .TRUE.
      PASS5 = .TRUE.
      PASS6 = .TRUE.
      PASS7 = .TRUE.
      PASS8 = .TRUE.
      PASS9 = .TRUE.
      PASS10 = .TRUE.
      PASS11 = .TRUE.
      END DO  !  NSETS
C
C     SUMMARIZE INPUT DATA
C
      if (method .ne. 0) then
       CALL ATOREAD(2, NDIST, DIST, NDIR, DIR, airname)
      endif
C
C     MAKE ANNOUNCEMENTS
C
      PRINT 999, GIDfnm(1:NGIDNM)
  999 FORMAT(' RESULTS MAY BE FOUND IN THE FILE',A68,'.EPA.',/)
      IF (NERROR .eq. 0) THEN
        CLOSE (NERR, STATUS='DELETE')
      END IF
      CLOSE (Nepa)
      close (nshp)
	close (nref)
      STOP 
      END
