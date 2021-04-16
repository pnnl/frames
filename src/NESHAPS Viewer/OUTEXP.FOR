C   NESHAPS:     OUTEXP.FOR             Version Date: 2 Feb-03               
C   Copyright 2003 by Battelle Memorial Institute.  
C                            All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE OUTEXP                                *
C                                                                            *
C  Subroutine OUTEXP Writes the input EXPOSURE CONTROLS to the *.EPA file    *
C                                                                            *
C  Written by:       Bruce Napier                                            *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    2 Feb 03     BAN                                        *
c                                                                            *
c    13 April 2006  BAN correct STORTM output for animal products            *
c                                                                            *
C  Last Modified:    13 April 06     BAN                                     *
C                                                                            *
C*****************************************************************************
C
c
C---- Modular Organization --------------------------------------------------=
C
C     Module of: GENII/NESHAPS
C     Called by: NESHAPS
C     Calls: NONE
C     Common blocks referenced: DATABLKS
C
      SUBROUTINE OUTEXP
      INCLUDE 'DATABLKS.CMN'
	INCLUDE 'NESHAPS.CMN'
	INCLUDE 'DATARCP.CMN'
	LOGICAL FILTER, GETLOG
	CHARACTER*80 GETSTR
	CHARACTER*120 SETDATA(5000), TYPWRIT
	INTEGER NLINES, GETINT
c
C     Animal Products Parameters Block
C     Taken from GENII/CHRONIC/ANMPAR
  
      DIMENSION TRANSA(6), YELDA(6), GRWPA(6)
      DIMENSION      STORTM(6), DIETFR(6)
      DIMENSION                HLDUPA(4)
      DIMENSION       CONSUM(6), SLCONA(4)
      DIMENSION       BIOMA2(6), DRYFA2(6)
      REAL RIRRA, TRANSA, YELDA, GRWPA, STORTM, DIETFR, FMI,
     .     DWFACA, HLDUPA, CONS2, CONSUM, DWATER, IRTIMA,
     .     FRACUT, BIOMA2, DRYFA2
      LOGICAL ANMFED
C----------------------------------------------------------------------
C     Terrestrial Foods Parameters Block
C     tAKEN FROM genii/chronic/fodpar

      DIMENSION  TRANS(4)
      DIMENSION  YELD(4),GRWP(4),HLDUP(4),CONS(4),
     . BIOMAS(4), DRYFAC(4)
      REAL TRANS, YELD, GRWP, HLDUP, CONS,
     .  LEAFRS, TPRODT, BIOMAS, DRYFAC, ABSHUM, RAIN
C-----------------------------------------------------------------------
C     Options / Flags / General 
C
C      LOGICAL  FINITE, INHAL, AIREXT, SLING, HARVST,
C     .        GROUND, TFOOD, TFD(4), ANFOOD, ANF(4), 
C     .        SHINDR, SLINH, DRYSET, WETSET
      LOGICAL FINITE, HARVST, DRYSET, WETSET
      REAL SLDN, SSLDN, DEPFR1, DEPFR2, DPVRES, 
     .     WTIM, RF1, SEDDN
C----------------------------------------------------------------------
c
C     Soil Parameters FROM BlockS /SOLPAR/ & /TIMES/
c
      REAL SURCM
      INTEGER BEFAIR
	CHARACTER*11 TFNAME(4), ANMNAME(6)
      REAL BEFORE, RELEND, NTKEND
	dimension iseadate(8)
	DATA TFNAME/'LEAFY VEG','OTHER VEG','FRUIT','GRAIN'/
	DATA ANMNAME/'MEAT','POULTRY','MILK','EGGS','MEAT ANIMAL',
     .'MILK COW'/
      IZ = 0
	IS = 1
C
C----- Get data from GID file -----------------------     
C
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,expNAME,NLINES,SETDATA,NSITE,FILTER)
C
C----- Was the EXP data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2211) NLINES
 2211   FORMAT(' The src data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
C----  Pathway selection logic flags
C
      FINITE=GETLOG(SETDATA,NLINES,'FINITE        ',IZ,IZ,IZ,IZ,IZ,IZ)
      AIREXT=GETLOG(SETDATA,NLINES,'AIREXT        ',IZ,IZ,IZ,IZ,IZ,IZ)
      INHAL =GETLOG(SETDATA,NLINES,'INHAL         ',IZ,IZ,IZ,IZ,IZ,IZ)
      GROUND=GETLOG(SETDATA,NLINES,'GROUND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      TFOOD =GETLOG(SETDATA,NLINES,'TFOOD         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ANFOOD=GETLOG(SETDATA,NLINES,'ANFOOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
      SHINDR=GETLOG(SETDATA,NLINES,'SHINDR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLING =GETLOG(SETDATA,NLINES,'SLING         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLINH =GETLOG(SETDATA,NLINES,'SLINH         ',IZ,IZ,IZ,IZ,IZ,IZ)
      LEACHOPTION = GETINT(SETDATA,NLINES,'LEACHOPTION   ',IZ,IZ,IZ,
     .            IZ,IZ,IZ)
C
C---  Timing parameters ------------------------------------------------------
C
      NTKEND=GETREAL(SETDATA,NLINES,'NTKEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      RELEND=GETREAL(SETDATA,NLINES,'RELEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BEFAIR=GETINT (SETDATA,NLINES,'BEFAIR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BEFORE=GETREAL(SETDATA,NLINES,'BEFORE        ',IZ,IZ,IZ,IZ,IZ,IZ)
      RF1   =GETREAL(SETDATA,NLINES,'RF1           ',IZ,IZ,IZ,IZ,IZ,IZ)
C
      ACUTIM=GETREAL(SETDATA,NLINES,'ACUTIM        ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C---- Read inhalation values -------------------------------------------
C
      IF (INHAL .OR. SLINH ) THEN
       IRES  =GETINT (SETDATA,NLINES,'IRES          ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
c      IF (SLINH) THEN
c       VAL=GETREAL(SETDATA,NLINES,'AVALSL        ',IZ,IZ,IZ,IZ,IZ,IZ)
c       IF(VAL.GT.0) AVALSL = VAL
c       VAL=GETREAL(SETDATA,NLINES,'XMLF          ',IZ,IZ,IZ,IZ,IZ,IZ)
c       IF(VAL.GT.0.) XMLF = VAL
c      ENDIF
C
C
C---- Read terrestrial food values -------------------------------------
C
      IF (TFOOD) THEN
       DO IT = 1,4
       TFD(IT)=GETLOG(SETDATA,NLINES,'TFD           ',IT,IZ,IZ,IZ,IZ,IZ)
       END DO
        DO IT = 1, 4
          IF (TFD(IT)) THEN
            VAL =  GETREAL(SETDATA,NLINES,'GRWP          ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) GRWP(IT) = VAL
            VAL     = GETREAL (SETDATA,NLINES,'YELD          ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) YELD(IT) = VAL
            VAL      = GETREAL(SETDATA,NLINES,'HLDUP         ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) HLDUP(IT)= VAL
          END IF
        END DO
      ENDIF
C
C---- Read animal product values -------------------------------------
C
      IF (ANFOOD) THEN
       DO IN = 1,4
       ANF(IN)=GETLOG(SETDATA,NLINES,'ANF           ',IN,IZ,IZ,IZ,IZ,IZ)
       END DO
        DO IN = 1, 4
          IF (ANF(IN)) THEN
            VAL=GETREAL(SETDATA,NLINES,'HLDUPA        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)           
            IF(VAL.GE.0.) HLDUPA(IN) = VAL
            VAL=GETREAL(SETDATA,NLINES,'DIETFR        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GE.0.) DIETFR(IN) = VAL
            VAL = GETREAL(SETDATA,NLINES,'GRWPA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) GRWPA(IN) = VAL
            VAL      = GETREAL(SETDATA,NLINES,'YELDA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) YELDA(IN) = VAL
            VAL       =GETREAL(SETDATA,NLINES,'STORTM        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GE.0.) STORTM(IN) = VAL
          ENDIF
        END DO
        IF (ANF(1)) THEN
          IN = 5
            VAL       =GETREAL(SETDATA,NLINES,'DIETFR        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GE.0.) DIETFR(IN) = VAL
            VAL      = GETREAL(SETDATA,NLINES,'GRWPA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) GRWPA(IN) = VAL
            VAL      = GETREAL(SETDATA,NLINES,'YELDA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) YELDA(IN) = VAL
            VAL       =GETREAL(SETDATA,NLINES,'STORTM        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GE.0.) STORTM(IN) = VAL
        ENDIF
        IF (ANF(3)) THEN
          IN = 6
            VAL       =GETREAL(SETDATA,NLINES,'DIETFR        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GE.0.) DIETFR(IN) = VAL
            VAL      = GETREAL(SETDATA,NLINES,'GRWPA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) GRWPA(IN) = VAL
            VAL  = GETREAL(SETDATA,NLINES,'YELDA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) YELDA(IN) = VAL
            VAL       =GETREAL(SETDATA,NLINES,'STORTM        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GE.0.) STORTM(IN) = VAL
        ENDIF
      ENDIF
C
C---- Read default parameter values -----------------------------------
C
      HARVST=GETLOG (SETDATA,NLINES,'HARVST        ',IZ,IZ,IZ,IZ,IZ,IZ)
      VAL = GETREAL (SETDATA,NLINES,'DPVRES        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) DPVRES = VAL
      VAL = GETREAL (SETDATA,NLINES,'LEAFRS        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) LEAFRS = VAL
      RAIN  =GETREAL(SETDATA,NLINES,'RAIN          ',IZ,IZ,IZ,IZ,IZ,IZ)
      VAL  = GETREAL(SETDATA,NLINES,'WTIM          ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) WTIM = VAL
      SURCM =GETREAL(SETDATA,NLINES,'SURCM         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLDN  =GETREAL(SETDATA,NLINES,'SLDN          ',IZ,IZ,IZ,IZ,IZ,IZ)
      SSLDN =GETREAL(SETDATA,NLINES,'SSLDN         ',IZ,IZ,IZ,IZ,IZ,IZ)
      VAL   =GETREAL(SETDATA,NLINES,'ABSHUM        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GE.0.) ABSHUM = VAL
      DO IX = 1,4
       VAL =GETREAL(SETDATA,NLINES,'TRANS         ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) TRANS(IX) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'BIOMAS        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) BIOMAS(IX) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'DRYFAC        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) DRYFAC(IX) = VAL
	 VAL       =GETREAL(SETDATA,NLINES,'SLCONA        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) SLCONA(IX) = VAL
      END DO
      DO IX = 1,6
       VAL       =GETREAL(SETDATA,NLINES,'TRANSA        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) TRANSA(IX) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'CONSUM        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) CONSUM(IX) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'BIOMA2        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) BIOMA2(IX) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'DRYFA2        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) DRYFA2(IX) = VAL
      END DO
c
c----- end of input ----------------------------------------------------
c
C
      write (NEPA,1) expNAME
1     format(//,'---- SUMMARY OF INPUT DATA FOR EXPOSURE MODULE ',a32,
     .'------------------------',/)
	write (NEPA,2) BEFORE, RELEND,NTKEND
2     FORMAT('TERRESTRIAL ACCUMULATION TIME LINE:',/,
     .'   START OF RELEASE (YEARS BEFORE INITIAL EXPOSURE):  ',t60,F4.0,
     ./,
     .'   DURATION OF RELEASE (YEARS SINCE INITIAL RELEASE): ',t60,F4.0,
     ./,
     .'   DURATION OF EXPOSURE (YEARS):                      ',t60,F4.0,
     .//)
C
c --- line for Acute Releases
      if (ACUTIM .gt. 0) then
      open (unit=1234,file='\FRAMES\SEASON.DAT', STATUS='OLD')
	read(1234,*) (iseadate(i), i=1,8)
	write (NEPA,222) (iseadate(i), i=1,8)
  222 format('   ACUTE RELEASE: SEASON DIVIDER DATES FROM SEASON.DAT:'
     . ,/,'   JULIAN HOUR: WINTER:', 2I6,', SPRING:'2i6,', SUMMER:',2i6, 
     . ', FALL:',2I6 //)
      close (1234)
	end if
c
      WRITE (NEPA,3)
3     FORMAT ('GENERAL ENVIRONMENTAL BEHAVIOR CONTROLS:')
	  IF (IRES .EQ. 1) THEN
	    TYPWRIT = 'Mass Loading'
	  ELSE IF (IRES .EQ. 2) THEN
	    TYPWRIT = 'Anspaugh'
	  ELSE 
	    TYPWRIT = 'None'
	END IF
	WRITE (NEPA,4) TYPWRIT
4     FORMAT ('   TYPE OF RESUSPENSION MODEL: ',t60,A32)
 	IF (LEACHOPTION .EQ. 0) THEN
	  TYPWRIT = 'GENII Defaults'
	ELSE IF (LEACHOPTION .EQ.1) THEN
	  TYPWRIT = 'Calculated From User Input '
	ELSE 
	  TYPWRIT = 'Input By User'
	END IF
	WRITE (NEPA,5) TYPWRIT
5     FORMAT ('   TYPE OF SOIL LEACHING MODEL: ',t60,A32)
	IF (AIREXT) THEN
	  IF (FINITE) THEN
	    TYPWRIT = 'Finite Plume'
	  ELSE 
	    TYPWRIT = 'Infinite Plume'
	  END IF
	WRITE (NEPA,6) TYPWRIT
6     FORMAT ('   TYPE OF AIR SUBMERSION MODEL: ',t60,A32)
      END IF
 	IF (SLING) THEN 
	  TYPWRIT = 'ON'
	ELSE 
	  TYPWRIT = 'OFF'
	END IF
	WRITE (NEPA,7) TYPWRIT
7     FORMAT('   INADVERTANT SOIL INGESTION PATHWAY: ',t60,A3)
  	IF (SHINDR) THEN 
	  TYPWRIT = 'ON'
	ELSE 
	  TYPWRIT = 'OFF'
	END IF
	WRITE (NEPA,8) TYPWRIT
8     FORMAT('   ESTIMATION OF INDOOR AIR CONCENTRATIONS: ',t60,A3)
      IF (GROUND) THEN
	  TYPWRIT = 'ON'
	ELSE 
	  TYPWRIT = 'OFF'
	END IF
	WRITE (NEPA,9) TYPWRIT
9     FORMAT('   ESTIMATION OF OUTDOOR SOIL EXTERNAL DOSE RATES: ',t60,
     .           A3)
 	IF (HARVST) THEN
	  TYPWRIT = 'ON'
	ELSE 
	  TYPWRIT = 'OFF'
	END IF
	WRITE (NEPA,10) TYPWRIT
10    FORMAT('   HARVEST REMOVAL OF CONTAMINANTS FROM SOIL: ',t60,A3)
	WRITE (NEPA,11) ABSHUM
11    FORMAT('   ABSOLUTE HUMIDITY FOR H-3 CALCULATIONS, KG/M3:',t60,
     .       1PE8.2)
      WRITE (NEPA, 12) RF1, SSLDN, WTIM, RAIN, LEAFRS, DPVRES,SLDN,SURCM
12    FORMAT ('   FRACTION OF ROOTS IN SURFACE SOILS: ',t60,1PE8.2,/,
     .'   SURFACE SOIL DENSITY, KG/M3: ',t60,1PE8.2,/,
     .'   WEATHERING RATE HALF-TIME FOR VEGETATION, DAYS: ',t60,1PE8.2,/,
     .'   AVERAGE RAINFALL RATE (WHEN RAINING), MM/DAY: ',t60,1PE8.2,/,
     .'   RESUSPENSION FACTOR FOR SOIL-TO-PLANT, 1/M: ',t60,1PE8.2,/,
     .'   LOCAL DEPOSITION VELOCITY FOR RESUSPENDED SOIL, M/S: ',t60,
     . 1PE8.2,/,
     .'   SURFACE SOIL AREAL DENSITY, KG/M2: ',t60, 1PE8.2,/,
     .'   SURFACE SOIL LAYER THICKNESS, M: ',t60, 1PE8.2,//)
C
      IF (TFOOD) THEN
	WRITE (NEPA,13)
13    FORMAT ('                     GROW   STANDING   YIELD   INTAKE  ',
     .'DRY-TO-WET   TRANSLOCATION',/'                     TIME   BIOMASS
     .',
     .'   (KG/M2)  DELAY     RATIO       FACTOR',/,'TERRESTRIAL FOODS   
     .',
     .'(DAYS)  (KG/M2)            (DAYS)',/,'-----------------   ------'
     .,'  -------  -------   -----  ----------   -------------') 
	DO IFD = 1,4
	  IF (TFD(IFD)) THEN
	    WRITE (NEPA,14) TFNAME(IFD),GRWP(IFD),BIOMAS(IFD),YELD(IFD),
     .          HLDUP(IFD),DRYFAC(IFD),TRANS(IFD)
14        FORMAT(6X,A11,F7.0,F11.2,F8.2,F9.2,F12.2,F11.2)
        END IF
	END DO
	END IF
C
      IF (ANFOOD) THEN
	WRITE (NEPA,15)
15    FORMAT (//,'                     GROW   STANDING   YIELD   STORAG'
     .,'E DRY-TO-WET  TRANSLOCATION   DIET    CONSUMPTION   SOIL      IN
     .TAKE',/,
     .'                     TIME   BIOMASS',
     .'   (KG/M2)  TIME      RATIO       FACTOR       FRACTION    RATE',
     .'      INTAKE     DELAY',
     ./,'ANIMAL PRODUCTS     ',
     .'(DAYS)  (KG/M2)            (DAYS)',40X,'(KG/D)',5X,'(KG/D)',5X,
     .'(DAYS)', /,'---------------     ------'
     .,'  -------  -------   -----  ----------   -------------  -------'
     .,'    ------     ------     -------',/,30X,'STORED FEED') 
	JPFLAG = 0
      DO IFD = 1,4
	  IF (ANF(IFD)) THEN
	    WRITE(NEPA,16) ANMNAME(IFD),GRWPA(IFD),BIOMA2(IFD),YELDA(IFD),
     .          STORTM(IFD),DRYFA2(IFD),TRANSA(IFD),DIETFR(IFD),
     .          CONSUM(IFD),SLCONA(IFD), HLDUPA(IFD)
16        FORMAT(6X,A11,F7.0,F11.2,F8.2,F9.2,F12.2,F11.2,F13.2,3F11.2)
        END IF
	END DO
	  IF(ANF(1)) THEN
	  IFD = 5
	  JPFLAG = 1
	  WRITE (NEPA,17)
17      FORMAT(30X,'FRESH FORAGE')
        WRITE(NEPA,16) ANMNAME(IFD),GRWPA(IFD),BIOMA2(IFD),YELDA(IFD),
     .          STORTM(IFD),DRYFA2(IFD),TRANSA(IFD),DIETFR(IFD),
     .          CONSUM(IFD)
	  END IF
	  IF(ANF(3)) THEN
	  IFD = 6
	  IF(JPFLAG .NE. 1)WRITE (NEPA,17)
        WRITE(NEPA,16) ANMNAME(IFD),GRWPA(IFD),BIOMA2(IFD),YELDA(IFD),
     .          STORTM(IFD),DRYFA2(IFD),TRANSA(IFD),DIETFR(IFD),
     .          CONSUM(IFD)
	  END IF
	END IF
C
      WRITE (NEPA,18)
18    FORMAT(//)
C
999   CONTINUE
      RETURN
	END