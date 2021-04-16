      PROGRAM VIEWER
C     A FRAMES/GENII OUTPUT FORMATING PROGRAM
C     B. A. NAPIER
C     MOST RECENT MODIFICATION:
C       BAN  INITIAL PROGRAMMING   20 Aug 2009
C
C     SIZING ASSUMPTIONS -
C       100 NUCLIDES & PROGENY
C       1 SINGLE LOCATAION
C       30 PATHWAYS, 3 ENDPOINTS
C       UNLIMITED TIMES
C
c     24 aPRIL 2012  BAN  Allow for all-zero doses while finding IPTMAX
C
      USE DFLIB   !This is a DEC Fortran thing
C
      INCLUDE 'NESHAPS.CMN'
      INCLUDE 'DATABLKS.CMN'
      INCLUDE 'SWEAT.CMN'
C
      ALLOCATABLE CANCERF(:,:,:,:,:), CANCERI(:,:,:,:,:)
      ALLOCATABLE DOSE(:,:,:,:,:)
      ALLOCATABLE POP(:,:), POPI(:,:)
      ALLOCATABLE SUMD(:,:,:), SUMI(:,:,:), SUMF(:,:,:)
      ALLOCATABLE SUMDP(:,:), SUMPF(:,:), SUMPI(:,:)
      DIMENSION SITENAM(15), ORGNAM(24)
      DIMENSION LOWAGE(6), UPAGE(6), path1(24)
      DIMENSION NUKID(100),T(2),PATH(90),ROUTE(90),UNIT(90),ENDPT(90)
      DIMENSION EDbyNUC(100), RFbyNUC(100), RIbyNUC(100)
      DIMENSION EDbyNUCP(100), RFbyNUCP(100), RIbyNUCP(100)
      DIMENSION EDbyPATH(90), RFbyPATH(90), RIbyPATH(90)
      DIMENSION EDbyPATHP(90), RFbyPATHP(90), RIbyPATHP(90)
C
      DIMENSION DIR(41), DIST(41), remSv(4), PremSv(4)
C
      CHARACTER*17  CANFAT, CANINC, RADDOSE, ENDPT
      CHARACTER*128 GIDFNM, RUNFNM
      CHARACTER*80  PFilNam, FacNam, FacStrt, UsrNam,FacCity
      CHARACTER*8   HEINAM
      CHARACTER*10  TMPDAT, TMPTIM
      CHARACTER*1   B, DUM
      CHARACTER*12  ACCHRON, SOURCE, HPATH, ROUTE
      CHARACTER*10  SITENAM, ORGNAM, VERSION, PremSvNam(8)
      CHARACTER*6   NUKID
      CHARACTER*22  PATH, path1
      CHARACTER*7   UNIT
      CHARACTER*3   yesno
      CHARACTER*32  GLFNAM
      CHARACTER*32  NESSRCNA(10) 
	character*4   remSvnam(4)
C
      INTEGER UPAGE
C
      LOGICAL LEXIST, MEXIST, DOSETF, CANITF, CANFTF, POPTF
      LOGICAL SEQI, PASS1, PASS2, PASS3, PASS4, PASS5, PASS6
      LOGICAL PASS7, PASS8, PASS9, PASS10, PASS11
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
     .             NESSRCNA, NESSCRNUM)
C
      NGIDNM = INDEX(GIDFNM,B)-1
C
      OPEN(UNIT=NHIF,FILE = GIDFNM(1:NGIDNM)//'.HIF',STATUS = 'OLD')
C
C     SEE IF A POPULATION FILE IS WANTED
C

      NPOPNM = INDEX(PFilNam,B)-1
      IF (METHOD . EQ. 1) THEN
       inquire(file=PFilNam, exist=mexist)
        if (mexist) then
          OPEN(UNIT=NPOP,FILE = PFilNam(1:NPOPNM) ,STATUS = 'OLD')
          POPTF = .TRUE.
        else
          print 992
  992     format('The POPULATION file does not seem to be there - ',
     .     'check spelling,',/,', path name, or existence')
        end if
      END IF
C
C     FIND START OF HIF MODULE
C
      INUM = 0
   11 READ(NHIF,*)HEINAM, NUMLIN
C
C   Only one NesSrcName allowed
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
C     READ THE LOCATIONS HERE (There should only be one...)
C     HERE, DIR IS THE X DIRECTION, DIST IS THE Y DIRECTION
C
      DO J = 1, NPOINTS
	  READ(NHIF,*) DIR(J), DUM, DIST(J), DUM
      END DO 
C The river pathways are single points: don't need POPI
C      IF (PASS11) ALLOCATE (POPI(NAGES,npath1))
C      IF (PASS11) POPI = 1.0
C      PASS11 = .FALSE.
C     THE POPI ARRAY IS 0.0 IF NOBODY LIVES THERE, 1.0 IF INHABITED.  
C     POPI IS ALL 1.0 IF NO POPULATION FILE IS ATTACHED
C
      IF (POPTF) THEN

CCCCCCCCCCC - read from User interface eventually!!!
         read(npop,*)nage1,npath1
        IF (PASS1) ALLOCATE (POP(NAGE1,npath1))
	  pop = 0.0
        PASS1 = .FALSE.
	   do i=1,npath1
	     read(npop,*) path1(i), TOTPROD(I), (pop(j,i),j=1,nage1)
	if (seqi(path1(i),'drinking water',5)) path1(i) = 'water'
c	     do k=1,nage1
c             IF (POP(nage1,i) .EQ. 0.0) POPI(nage1,i) = 0.0
c	     end do
	   end do
      END IF
C
C     PRINT IDENTIFYING  INFORMATION IN OUTPUT FILE
C
      IF (METHOD .NE. 2)THEN
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
802   FORMAT (//' FACILITY NAME: ',11x, A80)
803   FORMAT (' FACILITY MAILING ADDRESS: ',A80,/,27x,A80)
804   FORMAT (//' INPUT PREPARED BY: ',7x, A80, ///)
      END IF
C
C     PRINT HEADER INFO IN OUTPUT FILE
C

      WRITE(NEPA,800) GIDFNM,HEINAME,NAGES,NUKES,NORGAN,NSITES
  800 FORMAT('GENII VERSION 2',/,'---- SURFACE WATER PATHWAY REPORT FOR:
     . ',A40,'-------------------------------------------',/,
     . 'FOR IMPACT ICON: ',A32,
     .//,'THE HEALTH IMPACTS FILE FOR THIS CASE CONTAINS INFORMATIO'
     .,'N ON:',/,10X,I3,' AGE GROUP(S)',/,10X,I3,' RADIONUCLIDES (COUNT'
     .,'ING DECAY PROGENY SEPARATELY IN CHAINS)',/
     .,10X,I3,' ORGAN(S)/TISSUE(S) FOR RADIATION DOSE',/,10X,I3,' POTE'
     .,'NTIAL CANCER SITES')
  801 FORMAT(/,10X,I3,' EXPOSURE PERIOD(S).',/)
      IPRT1=0
CCCCC
C
C     SUMMARIZE INPUT DATA
C
      WRITE(NEPA, 806)
  806 FORMAT (//,'---- SUMMARY OF CASE INPUT DATA --------------------',
     .'----------------------------------------------------------------'
     .,'------')
C
       CALL OUTSRC
       CALL OUTriv
       CALL OUTEXP
       CALL OUTRCP
	IF (POPTF) THEN
	  WRITE(NEPA,807)
  807 FORMAT (//,'---- SUMMARY OF POPULATION INFO --------------------',
     .'----------------------------------------------------------------'
     .,'------',/,'     PATHWAY',T30,'TOTAL PRODUCTION (KG)',
     .'    PERSONS PER PATHWAY BY AGE GROUP')
	  do i=1,npath1
	   IF((.NOT. SEQI(PATH1(I),'BOA',3)) .AND. (.NOT. SEQI(PATH1(I),
     .   'GRO',3)) .AND. (.NOT. SEQI(PATH1(I),'IND',3)) .AND.
     .   (.NOT. SEQI(PATH1(I),'SHO',3)) .AND. (.NOT. SEQI(PATH1(I),
     .   'SOI',3)) .AND. (.NOT. SEQI(PATH1(I),'SWI',3))
     .   .AND. (.NOT. SEQI(PATH1(I),'WAT',3)) 
     .   .AND. (.NOT. SEQI(PATH1(I),'OUT',3))
     .   .AND. (.NOT. SEQI(PATH1(I),'SUS',3))  ) THEN
	     WRITE(NEPA,808) path1(i), TOTPROD(I), (pop(j,i),j=1,nage1)
	   ELSE
	     WRITE(NEPA,809) path1(i), (pop(j,i),j=1,nage1)
	   END IF
	  end do
808     FORMAT(5X,A22,T37,1PE8.2,T53,6(2X,1PE8.2))
809     FORMAT(5X,A22,T53,6(2X,1PE8.2))
C
C  Now we can "adjust" the population by the consumption, and just use the adjusted 
C  population entry to do the actual consumption pop calculations
C
        DO I=1,NPATH1
C    THE HARDEST PART OF THIS IS FIGURING OUT WHICH CONSUMPTION GOES WITH WHICH PRODUCTION
C      1ST DO TERRESTRIAL PLANTS
           DO J=1,4
		   IF(SEQI(PATH1(I),TFNAME(J),4)) THEN
	         TEST=0.0
		     DO K=1,NAGE1
	           TEST=POP(K,I)*UCRP(J,K)*TCRP(J,K)/TOTPROD(I)+TEST
	         END DO
			 IF (TEST. GT. 1.0) THEN
			   DO K=1,NAGE1
			     POP(K,I)=POP(K,I)/TEST
	           END DO
	         END IF
	       END IF
		 END DO 
C      2ND DO TERRESTRIAL ANIMAL PRODUCTS
           DO J=1,4
		   IF(SEQI(PATH1(I),ANMNAME(J),4)) THEN
	         TEST=0.0
		     DO K=1,NAGE1
	           TEST=POP(K,I)*UANM(J,K)*TANM(J,K)/TOTPROD(I)+TEST
	         END DO
			 IF (TEST. GT. 1.0) THEN
			   DO K=1,NAGE1
			     POP(K,I)=POP(K,I)/TEST
	           END DO
	         END IF
	       END IF
		 END DO 
c      3rd do Aquatic stuff
           DO J=1,4
		   IF(SEQI(PATH1(I),fishNAM(J),4)) THEN
	         TEST=0.0
		     DO K=1,NAGE1
	           TEST=POP(K,I)*UAQU(J,K)*TAQU(J,K)/TOTPROD(I)+TEST
	         END DO
			 IF (TEST. GT. 1.0) THEN
			   DO K=1,NAGE1
			     POP(K,I)=POP(K,I)/TEST
	           END DO
	         END IF
	       END IF
		 END DO                   
	  END DO
C  
C   THE OTHER PATHWAYS (AIR, WATER, SHORE, ETC., DON'T NEED TO BE ADJUSTED
C      
      END IF
C
CCCCC
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
c
c  separator of input from output
c
      write(nepa,831)
  831 format(/,122('='))  
c
        DO INUKE = 1, NUKES
          JNUC = JNUC+1
          READ(NHIF,*)NUKID(JNUC),DUM,NPROG,NTIMES
          
	   if(ntimes .gt. 0) then
		IF (IPRT1 .EQ. 0) THEN
          WRITE(NEPA,801)NTIMES
	    write(nepa,831)
            if (poptf) then
              WRITE(NEPA,2)
    2         FORMAT('POPULATION DATA INCLUDED')
            else
            WRITE(NEPA,3)
    3         FORMAT('INDIVIDUAL DOSE CALCULATION ONLY, NO POPULATION')
            end if
            IPRT1 = 1
          END IF
          DO ITIME = 1,NTIMES
            READ(NHIF,*)T(ITIME), DUM, TINC, DUM, NVALS
c
c            nvalm=nvals
c            if(ntimes .gt. 1) nvalm=84
            nvalm=84
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
C  THE NEXT LINE IS TO KEEP FOR OVERCOUNTING JNUC AFTER TIME 1
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
	   endif ! ntimes check
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
          NDST = IPT
           DO INUC = 1, JNUC
            DO IVAL = 1, NVALS
c match pop/path here
               if (poptf) then
               do i=1,npath1
                 if (seqi(Path(ival),path1(i),4)) then
	             Ipth=i
			   end if
	         end do
	         endif
c
              DO IORG = 1, NORGAN
                IF(DOSETF) THEN
                SUMD(IT,IPT,IORG)=SUMD(IT,IPT,IORG)
     .             + DOSE(IT,INUC,IPT,IVAL,IORG)
     .               *remSv(IremSv+1)
                IF(POPTF) THEN
                SUMDP(IT,IORG)=SUMDP(IT,IORG) +
     .                 DOSE(IT,INUC,IPT,IVAL,IORG)*POP(IAGE,IPTh)
     .               *PremSv(IremSv+1) 
                END IF
            END IF 
              END DO
              DO ISITE = 1, NSITES
                IF (CANFTF) THEN
                SUMF(IT,IPT,ISITE)=SUMF(IT,IPT,ISITE)
     .             + CANCERF(IT,INUC,IPT,IVAL,ISITE)
                IF(POPTF) THEN
            SUMPF(IT,ISITE)=SUMPF(IT,ISITE)
     .              + CANCERF(IT,INUC,IPT,IVAL,ISITE)*POP(IAGE,IPTh)
                END IF
                END IF
                IF (CANITF) THEN
                SUMI(IT,IPT,ISITE)=SUMI(IT,IPT,ISITE)
     .             + CANCERI(IT,INUC,IPT,IVAL,ISITE)
                IF (POPTF) THEN
                SUMPI(IT,ISITE)=SUMPI(IT,ISITE)
     .              + CANCERI(IT,INUC,IPT,IVAL,ISITE)*POP(IAGE,IPTh)
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
C   these zeroing functions are within the time loop 
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
c the following line may be temporary... if we keep the concept at all!
          NDR1 = IPT
c
        END IF
      END DO
C  When all doses are zero, IPTMAX not set (may occur in time 1 of 2 for acute cases!)
      IF (IPTMAX .EQ. 0) IPTMAX = 1
C
      END IF
C     USE CANCER INCIDENCE IF DOSE NOT AVAILABLE
      IF((.NOT. DOSETF) .AND. CANITF) THEN
      DO IPT = 1, NPOINTS
        IF(SUMI(IT,IPT,LORG) .GT. CIMAX)THEN
          CIMAX = SUMI(IT,IPT,LORG)
          IPTMAX = IPT
c the following line may be temporary... if we keep the concept at all!
          NDR1 = IPT
c
        END IF
      END DO
C  When all doses are zero, IPTMAX not set (may occur in time 1 of 2 for acute cases!)
      IF (IPTMAX .EQ. 0) IPTMAX = 1
C
      END IF
C     USE CANCER FATALITIES IF NEITHER OTHER AVAILABLE
      IF((.NOT. DOSETF) .AND. (.NOT. CANITF)) THEN
      DO IPT = 1, NPOINTS
        IF(SUMF(IT,IPT,LORG) .GT. CFMAX)THEN
          CFMAX = SUMF(IT,IPT,LORG)
          IPTMAX = IPT
c the following line may be temporary... if we keep the concept at all!
          NDR1 = IPT
c
        END IF
      END DO
C  When all doses are zero, IPTMAX not set (may occur in time 1 of 2 for acute cases!)
      IF (IPTMAX .EQ. 0) IPTMAX = 1
C
      END IF
C
C     NEED SEVEN TABLES FOR OPTIONS:     
C
C 1.
      IF (DOSETF .AND. CANITF .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
   19 FORMAT(/,'TIME PERIOD NUMBER',I3,', CORRESPONDING TO TIME',F8.4,
     .' YEARS ---------------------------------------------------',
     .'------------')
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
      WRITE(NEPA,201)IPTMAX, NDR1, NDST1, LOWAGE(IAGE), UPAGE(IAGE),
     .remSvNam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,210)ORGNAM(I),SUMD(IT,IPTMAX,I), SITENAM(I), 
     .  SUMI(IT,IPTMAX,I)
      ELSE
        WRITE(NEPA,220)ORGNAM(I),SUMD(IT,IPTMAX,I)
  201 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,/,'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
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
      WRITE(NEPA,202)IPTMAX, NDR1, NDST1, LOWAGE(IAGE), UPAGE(IAGE),
     .remSvNam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,211)ORGNAM(I),SUMD(IT,IPTMAX,I), SITENAM(I), 
     .  SUMF(IT,IPTMAX,I)
      ELSE
        WRITE(NEPA,221)ORGNAM(I),SUMD(IT,IPTMAX,I)
  202 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,/,'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
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
      WRITE(NEPA,203)IPTMAX, NDR1, NDST1, LOWAGE(IAGE), UPAGE(IAGE),
     .remSvNam(IremSv+1)
      DO I=1,NORGAN
        WRITE(NEPA,212)ORGNAM(I),SUMD(IT,IPTMAX,I)
  203 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,/,'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,
     .'ORGAN',9X,'MAX. DOSE (',A4,')')
  212 FORMAT(A10,4X, 1PE10.2)
      END DO  !  NORGAN PRINTING LOOP
      END IF   ! TABLE FOR (DOSETF .AND. (.NOT. CANITF) .AND. (.NOT.CANFTF))
C 5.
      IF ((.NOT.DOSETF) .AND. CANITF .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,204)IPTMAX, NDR1, NDST1, LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NSITES
        WRITE(NEPA,213)SITENAM(I),SUMI(IT,IPTMAX,I), SUMF(IT,IPTMAX,I)
  204 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,/,'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,
     .'TISSUE',5X,'CANCER INCIDENCE','   CANCER FATALITIES')
  213 FORMAT(A9, 3X, E10.2, 8X, E10.2)
      END DO  !  NSITES PRINTING LOOP
      END IF   ! TABLE FOR ((.NOT. DOSETF) .AND. CANITF .AND. CANFTF)
C 6.
      IF ((.NOT.DOSETF) .AND. CANITF .AND. (.NOT.CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,205)IPTMAX, NDR1, NDST1, LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NSITES
        WRITE(NEPA,214)SITENAM(I), SUMI(IT,IPTMAX,I)
  205 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,/,'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
     .' YEARS',/,'MAXIMUM EXPOSED INDIVIDUAL LOCATION',/,
     .'TISSUE',5X,'CANCER INCIDENCE')
  214 FORMAT(A9, 3X, E10.2)
      END DO  !  NSITES PRINTING LOOP
      END IF   ! TABLE FOR ((.NOT. DOSETF) .AND. CANITF .AND. (.NOT.CANFTF))
C 7.
      IF ((.NOT.DOSETF) .AND. (.NOT. CANITF) .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,206)IPTMAX, NDR1, NDST1, LOWAGE(IAGE), UPAGE(IAGE)
      DO I=1,NSITES
        WRITE(NEPA,215)SITENAM(I), SUMF(IT,IPTMAX,I)
  206 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,/,'INDIVIDUAL AGE RANGE ',I5,' TO',I5,
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
      WRITE(NEPA,40) remSvNam(IremSv+1)
   40 FORMAT('INDIVIDUAL EFFECTIVE DOSE BY LOCATION (',A4,')',/)

        WRITE(NEPA,41)(SUMD(IT,I,MORG), I = 1, NPOINTS)

   41 FORMAT((1PE8.1))
      END IF
      IF (CANITF .AND. (NPOINTS .GT. 1) ) THEN
C     INDIVIDUAL CANCER INCIDENCE BY DISTANCE AND DIRECTION
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,300)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,42)
   42 FORMAT('INDIVIDUAL CANCER INCIDENCE BY LOCATION (RISK)',/)

        WRITE(NEPA,43) (SUMI(IT,I,LORG), I = 1, NPOINTS)

   43 FORMAT((1PE8.1))
      END IF
      IF (CANFTF .AND. (NPOINTS .GT. 1) ) THEN
C     INDIVIDUAL CANCER FATALITIES BY DISTANCE AND DIRECTION
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,300)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,44)
   44 FORMAT('INDIVIDUAL CANCER FATALITIES BY LOCATION (RISK)',/)

        WRITE(NEPA,45)(SUMF(IT,I,LORG), I = 1, NPOINTS)
 
   45 FORMAT((1PE8.1))
      END IF
C
C     INDIVIDUAL EFFECTIVE DOSE AND RISK BY NUCLIDE
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
        END DO
      END DO
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,60)IPTMAX, remSvNam(IremSv+1)
      WRITE(NEPA,300)LOWAGE(IAGE), UPAGE(IAGE)
   60 FORMAT('MAXIMUM LOCATION = ',I4,/,'INDIVIDUAL EFFECTIVE DOSE 
     .AND RISK BY NUCLIDE',/,'NUCLIDE',4X,'DOSE(',A4,') CANCER INCIDENCE 
     .  CANCER FATALITIES')
      DO I = 1, JNUC
        WRITE(NEPA,61)NUKID(I),EDbyNUC(I),RIbyNUC(I),RFbyNUC(I)
      END DO
   61 FORMAT(A6,4X,1PE10.2,5X,E10.2,5X,E10.2)
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
     .'DOSE(',A4,') CANCER INCIDENCE  CANCER FATALITIES')
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
   65 format(/,120('-'),/,'INDIVIDUAL EFFECTIVE DOSE AND RISK BY '
     .  ,'PATHWAY BY RADIONUCLIDE')
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
	ENDIF ! PATHNUC
CC
C     POPULATION SUMMARY
C
      IF (POPTF) THEN   ! POPULATION LOGIC     
C
C     NEED 7 TYPES OF TABLES HERE FOR OPTIONS:
C
C  1.
      IF (DOSETF .AND. CANITF .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,64)LOWAGE(IAGE), UPAGE(IAGE),PremSvNam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,21)ORGNAM(I),SUMDP(IT,I), SITENAM(I), 
     .  SUMPI(IT,I), SUMPF(IT,I)
      ELSE
        WRITE(NEPA,22)ORGNAM(I),SUMDP(IT,I)
   64 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'ORGAN',7X,'DOSE (',A10,')',3X,'TISSUE',5X,'CANCER INCIDENCE',
     .       '   CANCER FATALITIES')
      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. CANITF .AND. CANFTF)
C  2.
      IF (DOSETF .AND. CANITF .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,640)LOWAGE(IAGE), UPAGE(IAGE),PremSvNam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,210)ORGNAM(I),SUMDP(IT,I), SITENAM(I), 
     .  SUMPI(IT,I)
      ELSE
        WRITE(NEPA,220)ORGNAM(I),SUMDP(IT,I)
  640 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'ORGAN',7X,'DOSE (',A10,')',2X,'TISSUE',5X,'CANCER INCIDENCE')
      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. CANITF .AND. (.NOT. CANFTF))
C  3.
      IF (DOSETF .AND. (.NOT. CANITF) .AND. CANFTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,641)LOWAGE(IAGE), UPAGE(IAGE),PremSvNam(IremSv+1)
      DO I=1,NORGAN
      IF(I .LE. NSITES) THEN
        WRITE(NEPA,211)ORGNAM(I),SUMDP(IT,I), SITENAM(I), 
     .  SUMPF(IT,I)
      ELSE
        WRITE(NEPA,220)ORGNAM(I),SUMDP(IT,I)
  641 FORMAT('POPULATION AGE RANGE ',2I5,' YEARS',/,
     .'ORGAN',7X,'DOSE (',A10,')',3X,'TISSUE',5X,'CANCER FATALITIES')
      END IF
      END DO  !  NORGAN PRINTING LOOP
      END IF  !  (DOSETF .AND. (.NOT. CANITF) .AND. CANFTF)
C  4.
      IF (DOSETF .AND. (.NOT. CANITF) .AND. (.NOT. CANFTF)) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,642)LOWAGE(IAGE), UPAGE(IAGE),PremSvNam(IremSv+1)
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
      IF (DOSETF .AND. POPTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
  301 FORMAT('POPULATION GROUP AGE RANGE',I3,' TO',I3,' YEARS')
      WRITE(NEPA,50) PremSvNam(IremSv+1), (DIST(I), I=1, Npoints)
   50 FORMAT('POPULATION EFFECTIVE DOSE   (',A10,')',/,7X,41F8.0)     
c 
        WRITE(NEPA,51) SUMDP(IT,MORG)

   51 FORMAT((1PE8.1))
      END IF
C     POPULATION CANCER INCIDENCE BY DISTANCE AND DIRECTION
      IF (CANITF .AND. POPTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,52) (DIST(I), I=1, Npoints)
   52 FORMAT('POPULATION CANCER INCIDENCE  (RISK)',/,7X,41F8.0)
c
        WRITE(NEPA,53) SUMPI(IT,LORG)

   53 FORMAT((1PE8.1))
      END IF
C     POPULATION CANCER FATALITIES BY DISTANCE AND DIRECTION
      IF (CANFTF .AND. POPTF) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,54) (DIST(I), I=1, Npoints)
   54 FORMAT('POPULATION CANCER FATALITIES  (RISK)',/,7X,41F8.0)
c 
         WRITE(NEPA,55) SUMPF(IT,LORG)
   55 FORMAT((1PE8.1))
      END IF
C
C     POPULATION EFFECTIVE DOSE AND RISK BY NUCLIDE
C
      IF (POPTF) THEN   ! POPULATION LOGIC     
C
      DO IN = 1, JNUC
        DO IPT = 1,NPOINTS
          DO IV = 1, NVALS
c match pop/path here
               do i=1,npath1
                 if (seqi(Path(iv),path1(i),4)) then
	             Ipth=i
			   end if
	         end do
c
           IF (DOSETF) EDbyNUCP(IN) = EDbyNUCP(IN) + 
     .                 DOSE(IT,IN,IPT,IV,MORG) * POP(IAGE,IPTh)
     .                 *PremSv(IremSv+1)
           IF (CANITF) RIbyNUCP(IN) = RIbyNUCP(IN) + 
     .                 CANCERI(IT,IN,IPT,IV,LORG) * POP(IAGE,IPTh)
           IF (CANFTF) RFbyNUCP(IN) = RFbyNUCP(IN) + 
     .                 CANCERF(IT,IN,IPT,IV,LORG) * POP(IAGE,IPTh)
          END DO
        END DO
      END DO
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,70) PremSvNam(IremSv+5)
   70 FORMAT('POPULATION EFFECTIVE DOSE 
     .AND RISK BY NUCLIDE',/,'NUCLIDE',4X,'DOSE (',A4,
     .')  CANCER INCIDENCE  CANCER FATALITIES')
      DO I = 1, JNUC
        WRITE(NEPA,61)NUKID(I),EDbyNUCP(I),RIbyNUCP(I),RFbyNUCP(I)
      END DO
   71 FORMAT(A6,4X,1PE10.2,5X,E10.2,5X,E10.2)
      END IF  ! POPULATION LOGIC
C
C    POPULATION EFFECTIVE DOSE AND RISK BY PATHWAY
C
      IF (POPTF) THEN   ! POPULATION LOGIC     
C
      DO IVAL = 1, NVALS
	  DO IPT = 1,NPOINTS
          DO IN = 1, JNUC
c match pop/path here
               do i=1,npath1
                 if (seqi(Path(ival),path1(i),4)) then
	             Ipth=i
			   end if
	         end do
c
            IF(DOSETF) EDbyPATHP(IVAL) = EDbyPATHP(IVAL) +
     .                 DOSE(IT,IN,IPT,IVAL,MORG) * POP(IAGE,IPTh)
     .                 *PremSv(IremSv+1)
            IF(CANITF) RIbyPATHP(IVAL) = RIbyPATHP(IVAL) + 
     .           CANCERI(IT,IN,IPT,IVAL,LORG) * POP(IAGE,IPTh)
            IF (CANFTF) RFbyPATHP(IVAL) = RFbyPATHP(IVAL) + 
     .           CANCERF(IT,IN,IPT,IVAL,LORG) * POP(IAGE,IPTh)
          END DO
        END DO
      END DO
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,301)LOWAGE(IAGE), UPAGE(IAGE)
      WRITE(NEPA,72) PremSvNam(iremSv+5)
   72 FORMAT('POPULATION EFFECTIVE DOSE 
     .AND RISK BY EXPOSURE PATHWAY',/,'PATHWAY',32X,'DOSE(',A4,')  
     .CANCER INCIDENCE  CANCER FATALITIES')
C     NEED TO ACCOUNT FOR THE NVAL=NPATHWY*MCOUNT PROBLEM HERE
C      RECALL THAT GENII ALWAYS OUTPUTS IN ORDER INCIDENCE, FATALITY, DOSE
      DO I = 1, NVALS, MCOUNT
        WRITE(NEPA,63)PATH(I),ROUTE(I),EDbyPATHP(I+MCI+MCF),RIbyPATHP(I)
     .                ,RFbyPATHP(I+MCI)
      END DO
   73 FORMAT(A22,A12,4X,1PE10.2,6X,E10.2,6X,E10.2)
      END IF  !  POPULATION LOGIC
      END DO  !  NTIMES
C
C
C     RESET STUFF FOR NEXT PASS
C
      IF (IAGE .EQ. NAGES) GO TO 30
      PRINT 995
  995 FORMAT('Zeroing for next pass')
C
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
      END DO  !  NAGES
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
C     MAKE ANNOUNCEMENTS
C
      PRINT 999, GIDfnm(1:NGIDNM)
  999 FORMAT(' RESULTS MAY BE FOUND IN THE FILE',A68,'.EPA.',/)
      IF (NERROR .le. 0) THEN
        CLOSE (NERR, STATUS='DELETE')
      END IF
      close (nshp)
	close (nhif)
	close (nepa)
      STOP 
      END
