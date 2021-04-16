      PROGRAM VIEWER
C     A FRAMES/GENII OUTPUT FORMATING PROGRAM
C     B. A. NAPIER
C     MOST RECENT MODIFICATION:
C       BAN  INITIAL PROGRAMMING   10 November 2009
C
C     SIZING ASSUMPTIONS -
C       100 NUCLIDES & PROGENY
C       41 X 41 MAXIMUM GRID SIZE
C       10 CRITTERS
C       100 TIMES
C
C
      USE DFLIB   !This is a DEC Fortran thing
C
      INCLUDE 'NESHAPS.CMN'
      INCLUDE 'DATABLKS.CMN'
	include 'datarcp.cmn'
	include 'swpar.cmn'
C
      DIMENSION DIST(41), DIR(41), remSv(4), PremSv(4)
      DIMENSION NUKID(100),T(2),PATH(90),ROUTE(90),UNIT(90)
	DIMENSION BDOSE(100,100,10,1681), SUMBD(100,1681,10)
C
      CHARACTER*128 GIDFNM, RUNFNM
      CHARACTER*32  FacNam, FacStrt, UsrNam,FacCity
      CHARACTER*10  TMPDAT, TMPTIM
      CHARACTER*1   B, DUM
      CHARACTER*12  ACCHRON, SOURCE, HPATH
      CHARACTER*10  SITENAM, VERSION
      CHARACTER*6   NUKID
      CHARACTER*22  PATH, pathnam
      CHARACTER*7   UNIT
      CHARACTER*3   yesno
      CHARACTER*32  GLFNAM
      CHARACTER*32  NESSRCNA(10) 
	character*7   remSvnam(4)
	CHARACTER*18  CRITTER(10)
	character*100 shortref
C
      LOGICAL LEXIST, MEXIST, DOSETF
      LOGICAL SEQI,nexist
C
      DATA B/' '/
      DATA NUKMAX/100/
	data remSvnam/'Gy/yr','mGy/day','rad/day','mrad/yr'/
	data remSv/3.65, 10.0, 1.0, 365000.0/
C
C     OPEN INPUT AND OUTPUT FILES
C
      VERSION = '2.10.1'

      CALL INPUTS(FacNam, FacStrt, FacCity, UsrNam,
     .            GIDFNM, RUNFNM, NSITE, NUMNES, GLFNAM, METHOD,
     .             NESSRCNA, NESSrcNUM)
C
      NGIDNM = INDEX(GIDFNM,B)-1
C
      OPEN(UNIT=NBTF,FILE = GIDFNM(1:NGIDNM)//'.EPF',STATUS = 'OLD')
      INQUIRE(FILE=GIDFNM(1:NGIDNM)//'.ATO',EXIST = LEXIST)
      IF (LEXIST) THEN
        OPEN(UNIT=NATO,FILE = GIDFNM(1:NGIDNM)//'.ATO',STATUS = 'OLD')
      ELSE
        PRINT 1
        WRITE(NEPA,1)
    1   FORMAT('DISPERSION/DEPOSITION FILE (ATO) NOT AVAILABLE')
      END IF
C
C     FIND START OF EFX MODULE
C
      INUM = 0
   11 READ(NBTF,*)efxNAME, NUMLIN
C
C  only one NesSrcName allowed
C  
      NRUNNM1 = INDEX(NESSRCName,B)-1
      NRUNNM2 = INDEX(efxNAME,B)-1
      NRUNNM = MAX(NRUNNM1, NRUNNM2)
      IF (SEQI(efxNAME, NESSRCName, NRUNNM)) GO TO 10
      DO I=1,NUMLIN
        READ(NBTF, '(A1)') DUM
      END DO
      GO TO 11
   10 CONTINUE
C
C     READ PAST HEADER LINES
C
      READ(NBTF,*)NHEAD
      DO I=1,NHEAD
        READ(NBTF,'(A1)')DUM
      END DO
C
C     READ NUMBER OF DATASETS TO PROCESS
C     THIS IS AN OUTER LOOP FOR THE ENTIRE REMAINDER
C     (IT IS LIKELY THAT NEARLY ALL BIOTA RESULTS WILL HAVE 1 OR 2 SETS - AIR & WATER)
C     SETS CANNOT BE ADDED; MAY NOT HAVE THE SAME NUMBER OF LOCATIONS!!!!
C
      READ(NBTF,*)NSETS
      DO ISET = 1, NSETS
C
      READ(NBTF,*)ACCHRON,SOURCE,HPATH,NPOINTS,NUKES
C
c     DETERMINE IF THIS IS AN AIR OR SURFACE WATER CASE
      NDIST = 1
	NDIR  = 1

C        IT LOOKS EASIER TO USE THE GRID DEFINITIONS FROM THE ATO FILE
C        AND, IF IT IS A SW CASE, THERE IS ONLY ONE POINT ANYWAY...
C        SO READ PAST THE LOCATIONS HERE
C
        DO J = 1, NPOINTS
          READ(NBTF,'(A1)')DUM
        END DO 
C 
C        GET THE GRID INFO FROM THE ATO FILE
C        HERE, DIR IS THE X DIRECTION, DIST IS THE Y DIRECTION
C
      IF (SEQI(HPATH,'air',3))then

        CALL ATOREAD(1, NDIST, DIST, NDIR, DIR, airname)
C
      end if
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

      WRITE(NEPA,800) GIDFNM,EFXNAME,NUKES
  800 FORMAT('GENII VERSION 2',/,'----SUMMARY REPORT FOR CASE:
     . ',A50,'-----------------------------',/,
     . 'FOR BIOTA DOSE ICON: ',A32,
     .//,'THE BIOTA DOSE FILE FOR THIS CASE CONTAINS INFORMATIO'
     .,'N ON:',I3,' RADIONUCLIDES (COUNT'
     .,'ING DECAY PROGENY SEPARATELY IN CHAINS)')
  801 FORMAT(/,10X,I3,' EXPOSURE PERIOD(S).',/,/,107('-'))
      IPRT1=0
C
      WRITE(NEPA, 806)
  806 FORMAT (//,'---- SUMMARY OF CASE INPUT DATA --------------------',
     .'-------------------------------------------------------')
C
       CALL OUTSRC
       IF (SEQI(HPATH,'air',3)) CALL OUTAIR
       IF (SEQI(HPATH,'sur',3)) CALL OUTRIV
       CALL OUTBTF(hpath)

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
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     NOW COME BACK AND FINISH READING THE BTF FILE, WITHIN THE SOURCE (AIR/SW) SET
C         BY NUCLIDE AND PROGENY
C           BY TIME PERIOD
C             BY CRITTER TYPE
C                 FOR ALL LOCATIONS
C
        JNUC = 0
        DO INUKE = 1, NUKES
	    if (inuke. eq. 1) jnuc=0
          JNUC = JNUC+1
          READ(NBTF,*)NUKID(JNUC),DUM,NPROG,NTIMES
          IF (IPRT1 .EQ. 0) THEN
          WRITE(NEPA,801)NTIMES
            IPRT1 = 1
          END IF
          DO ITIME = 1,NTIMES
c
	       if(itime .gt. 1)jnuc=jnuc-nprog
c
            READ(NBTF,*)T(ITIME), DUM, TINC, DUM, NVALS
c
            DO IVAL = 1, NVALS  ! VALS HERE ARE #CRITTERS

              READ(NBTF,*)CRITTER(IVAL), DUM, UNIT(IVAL)
              READ(NBTF,*)(BDOSE(JNUC,ITIME,IVAL,IP),
     .                         IP = 1,NPOINTS)
            END DO ! VALS = CRITTERS

            NCRITTER = NVALS
C
            IF (NPROG .GT.0) THEN
            DO IPROG = 1, NPROG
C
              jnuc=jnuc+1
              READ(NBTF,*)NUKID(JNUC),DUM,NVALS
              DO IVAL = 1, NVALS  ! VALS HERE ARE CRITTERS
 
                READ(NBTF,*)CRITTER(IVAL), DUM, UNIT(IVAL)
                READ(NBTF,*)(BDOSE(JNUC,ITIME,IVAL,IP),
     .                         IP = 1,NPOINTS)

              END DO  !  VALS = CRITTERS
            END DO  !  NPROG
            END IF  !  NPROG
          END DO  !  NTIME
        END DO  !  NUKES
C
C     OK.  IT IS ALL READ IN, NOW ACCUMULATE AND FORMAT
C
C  FIRST, ACCUMULATE OVER PATHWAYS AND NUCLIDES FOR EACH ORGAN FOR ALL POINTS
C    
      print 997
  997 format('It is all read in, now summarizing')

      DO IT = 1, NTIMES
        DO IPT = 1, NPOINTS
          NDR = (IPT-1)/NDIST+1
          NDST = MOD(IPT,NDIST)
          IF(NDST .EQ. 0) NDST = npoints/ndir
          IF(NDST .EQ. 0) NDST = 1
          DO INUC = 1, JNUC
            DO IVAL = 1, NVALS ! CRITTER

                SUMBD(IT,IPT,IVAL)=SUMBD(IT,IPT,IVAL)
     .             + bDOSE(INUC,IT,IVAL,IPT)
     .               *remSv(IremSv+1) 
            END DO
          END DO
        END DO
      END DO
C
C   FIND HIGHEST CRITTER LOCATION
C   SELECT BASED UPON "GRASS"?!?!?
C
      PRINT 996
  996 FORMAT('Printing tables')
      DO IT = 1, NTIMES
      DOSEMAX = 0.0
      CIMAX = 0.0
      CFMAX = 0.0
      DO IPT = 1, NPOINTS
        IF(SUMbD(IT,IPT,1) .GT. DOSEMAX)THEN
          DOSEMAX = SUMBD(IT,IPT,1)
          IPTMAX = IPT
c
          NDR1 = (IPTMAX-1)/NDIST+1
          NDST1 = MOD(IPTMAX,NDIST)
          IF(NDST1 .EQ. 0) NDST1 = npoints/ndir
          IF(NDST1 .EQ. 0) NDST1 = 1
c
        END IF
      END DO
C
C     NEED ONE SET OF TABLES:     
C
C 1.
      WRITE(NEPA,19)IT, T(IT)
   19 FORMAT(/,'TIME PERIOD NUMBER',I3,', CORRESPONDING TO TIME',F8.4,
     .' YEARS ------------------------------------------------')
      WRITE(NEPA,20)IPTMAX, NDR1, NDST1, DIR(NDR1),DIST(NDST1), 
     . remSvnam(IremSv+1)
      DO I=1,NVALS
        WRITE(NEPA,22) CRITTER(I),SUMbD(IT,IPTMAX,I)
   20 FORMAT('MAXIMUM POINT =',I4,' CORRESPONDING TO DIRECTION',I4,
     .' AND DISTANCE',I4,' AT LOCATION ',F5.1,';',F9.1,/,
     . 'MAX. DOSE               (',A7,')')
   22 FORMAT(A18,4X, 1PE10.2)
      END DO  !  NVAL PRINTING LOOP
C
C     PRINT LARGE SUMMARY TABLES
C
      IF (NPOINTS .GT. 1 ) THEN
C     DOSE BY DISTANCE AND DIRECTION

      do icrt=1,nvals
	write(NEPA,42)CRITTER(ICRT)
   42 FORMAT(/,A18)
      WRITE(NEPA,40) remSvNam(IremSv+1), (DIST(I), I=1, NDIST)
   40 FORMAT('CRITTER DOSE BY DISTANCE AND DIRECTION 
     .(',A7,')',/, 7X,41F8.0)
      NDT=1
      DO ND = 1, NDIR
        NDT1 = NDT+NDIST-1
        WRITE(NEPA,41)DIR(ND), (SUMbD(IT,I,ICRT), I = NDT,NDT1)
        NDT = NDT+NDIST
      END DO
   41 FORMAT(F7.0, 41(1PE8.1))
	END DO
	END IF

C
C     BIOTA DOSE AND RISK BY NUCLIDE
C
      IF (NPOINTS .EQ. 1) THEN
      WRITE(NEPA,19)IT, T(IT)
      WRITE(NEPA,60)IPTMAX, remSvNam(IremSv+1)

   60 FORMAT('MAXIMUM LOCATION = ',I4,/,'CRITTER DOSE BY NUCLIDE',
     .  /,'NUCLIDE',4X,'DOSE (',A7,')')
	DO ICRT=1,NVALS
	  WRITE(NEPA,611)CRITTER(ICRT)
  611   FORMAT(A18)
        DO I = 1, JNUC
	  prcnt = 0.0
	  if(sumbd(it,1,icrt) .gt. 0.0) then
	   prcnt = BDOSE(I,IT,ICRT,1)*remSv(IremSv+1)*100/sumbd(it,1,icrt)
	  end if
        WRITE(NEPA,61)NUKID(I), BDOSE(I,IT,ICRT,1)*remSv(IremSv+1),prcnt
	  END DO
      END DO
   61 FORMAT(4X,A6,4X,1PE10.2,3x,0PF5.1,1x,'%')
      END IF

	END DO !  NTIMES


C     RESET STUFF FOR NEXT PASS
C
      PRINT 995
  995 FORMAT('Zeroing for next pass')
C
       sumbd=0.0
	if (nexist) rewind (nref)
C
       END DO  !  NSETS

C
C     SUMMARIZE INPUT DATA
C 
      IF (METHOD .EQ. 2) THEN
       CALL ATOREAD(2, NDIST, DIST, NDIR, DIR, airname)
	END IF
C
C     MAKE ANNOUNCEMENTS
C
      PRINT 999, GIDfnm(1:NGIDNM)
  999 FORMAT(' RESULTS MAY BE FOUND IN THE FILE',A68,'.EPA.',/)
      IF (NERROR .eq. 0) THEN
        CLOSE (NERR, status = 'DELETE')
      end if
      close (nbtf)
	close (nepa)
	close (nref)
      STOP 
      END
