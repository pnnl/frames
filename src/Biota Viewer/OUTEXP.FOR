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
      SUBROUTINE OUTBTF(hpath)
      INCLUDE 'DATABLKS.CMN'
	INCLUDE 'NESHAPS.CMN'
	INCLUDE 'DATARCP.CMN'
	LOGICAL FILTER, GETLOG, SEQI
	CHARACTER*80 GETSTR
	CHARACTER*120 SETDATA(5000), TYPWRIT
	character*12 hpath
	INTEGER NLINES, GETINT
c
C     Animal Products Parameters Block
C     Taken from GENII/CHRONIC/ANMPAR
c
      DIMENSION       CONSUM(6), SLCONA(6),TRANSA(6)
      DIMENSION       BIOMA2(6), DRYFA2(6)
      REAL RIRRA, TRANSA, FMI,
     .     DWFACA, HLDUPA, CONS2, CONSUM, DWATER, IRTIMA,
     .     BIOMA2, DRYFA2
      LOGICAL ANMFED, aqfood, AQF(4)
C----------------------------------------------------------------------
C     Terrestrial Foods Parameters Block
C     tAKEN FROM genii/chronic/fodpar

      REAL LEAFRS, ABSHUM, RAIN
C-----------------------------------------------------------------------
C     Options / Flags / General 
C
      LOGICAL DRYSET, WETSET
      REAL SLDN, SSLDN, DEPFR1, DEPFR2, DPVRES, 
     .     WTIM, SEDDN
C----------------------------------------------------------------------
c
C     Soil Parameters FROM BlockS /SOLPAR/ & /TIMES/
c
      REAL SURCM
      INTEGER BEFAIR
	CHARACTER*18 TFNAME(1), ANMNAME(5)
      REAL BEFORE, RELEND, NTKEND
	DATA TFNAME/'TERRESTRIAL PLANT'/
	DATA ANMNAME/'TERRESTRIAL MAMMAL','TERRESTRIAL BIRD',
     .            'RIPARIAN MAMMAL','RIPARIAN BIRD','EGGS'/
      IZ = 0
	IS = 1
C
C----- Get data from GID file -----------------------     
C
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,efxNAME,NLINES,SETDATA,NSITE,FILTER)
C
C----- Was the EFX data set found? -------------------------------------------
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
      GROUND=GETLOG(SETDATA,NLINES,'GROUND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      TFOOD =GETLOG(SETDATA,NLINES,'TFOOD         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ANFOOD=GETLOG(SETDATA,NLINES,'ANFOOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
	AQFOOD=GETLOG(SETDATA,NLINES,'AQFOOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
      LEACHOPTION = GETINT(SETDATA,NLINES,'LEACHOPTION   ',IZ,IZ,IZ,
     .            IZ,IZ,IZ)
C
C---  Timing parameters ------------------------------------------------------
C
      NTKEND=GETREAL(SETDATA,NLINES,'NTKEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      RELEND=GETREAL(SETDATA,NLINES,'RELEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BEFAIR=GETINT (SETDATA,NLINES,'BEFAIR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BEFORE=GETREAL(SETDATA,NLINES,'BEFORE        ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C---  READ AQUATIC CRITTER PATHS
C
      IF (AQFOOD) THEN
	  DO IAQ = 1,4
	AQF(IAQ)=GETLOG(SETDATA,NLINES,'AQF           ',IAQ,IZ,IZ,IZ,IZ,
     . IZ)
	  END DO
	END IF
C
C---- Read terrestrial CRITTER values -------------------------------------
C
      IF (TFOOD) THEN
       DO IT = 1,1
       TFD(IT)=GETLOG(SETDATA,NLINES,'TFD           ',IT,IZ,IZ,IZ,IZ,IZ)
       END DO
      ENDIF
C
C
C---- Read default parameter values -----------------------------------
C
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
C
      DO IX = 1,6
       VAL       =GETREAL(SETDATA,NLINES,'CONSUM        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) CONSUM(IX) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'BIOMA2        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) BIOMA2(IX) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'DRYFA2        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) DRYFA2(IX) = VAL
	 VAL       =GETREAL(SETDATA,NLINES,'SLCONA        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) SLCONA(IX) = VAL
	 ANF(IX)    =GETLOG(SETDATA,NLINES,'ANF           ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
      END DO
c
c----- end of input ----------------------------------------------------
c
C
      write (NEPA,1) EFXNAME
1     format(//,'---- SUMMARY OF INPUT DATA FOR BIOTA MODULE ',a32,
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
      WRITE (NEPA,3)
3     FORMAT ('GENERAL ENVIRONMENTAL BEHAVIOR CONTROLS:')
 	IF (LEACHOPTION .EQ. 1) THEN
	  TYPWRIT = 'GENII Defaults'
	ELSE IF (LEACHOPTION .EQ.2) THEN
	  TYPWRIT = 'Calculated From User Input '
	ELSE 
	  TYPWRIT = 'Input By User'
	END IF
	WRITE (NEPA,5) TYPWRIT
5     FORMAT ('   TYPE OF SOIL LEACHING MODEL: ',t60,A32)
	  TYPWRIT = 'ON'
	WRITE (NEPA,9) TYPWRIT
9     FORMAT('   ESTIMATION OF OUTDOOR SOIL EXTERNAL DOSE RATES: ',t60,
     .           A3)
	WRITE (NEPA,11) ABSHUM
11    FORMAT('   ABSOLUTE HUMIDITY FOR H-3 CALCULATIONS, KG/M3:',t60,
     .       1PE8.2)
      WRITE (NEPA, 12) SSLDN, WTIM, RAIN, LEAFRS, DPVRES,SLDN,SURCM
12    FORMAT ('   SURFACE SOIL DENSITY, KG/M3: ',t60,1PE8.2,/,
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
13    FORMAT ('                              STANDING ',
     .'  DRY-TO-WET ',/'                              BIOMASS'
     .,'     RATIO',/,'TERRESTRIAL PLANTS      ',
     .'      (KG/M2)            ',/,'------------------------',
     .'      -------      -------   ') 
	DO IFD = 1,1
	  IF (TFD(IFD)) THEN
	    WRITE (NEPA,14) TFNAME(IFD),BIOMA2(6),
     .          DRYFA2(6)
14        FORMAT(6X,A18,2X,f11.2,F11.2)
        END IF
	END DO
	END IF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC      IF (ANFOOD) THEN
	WRITE (NEPA,15)
15    FORMAT (//,28X,'STANDING   '
     .,'DRY-TO-WET     CONSUMPTION   SOIL',/,
     .28X,'BIOMASS',
     .'     RATIO           RATE',
     .'      INTAKE',
     ./,'ANIMAL TYPES             ',
     .'   (KG/M2) ',19X,'(KG/D)',5X,'(KG/D)',
     ./,'---------------------     '
     .,'  -------   ----------   '
     .,'    ------     ------ ') 
C
      DO IFD = 1,5
	  IF (ANF(IFD)) THEN
	    WRITE(NEPA,16) ANMNAME(IFD),BIOMA2(IFD),
     .          DRYFA2(IFD),
     .          CONSUM(IFD),SLCONA(IFD)
16        FORMAT(6X,A18,F11.2,F11.2,4X,F11.2,F11.2)
        END IF
	END DO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC	END IF
C
c      IF (AQFOOD .AND. SEQI(HPATH,'SURF',4)) THEN
	IF (SEQI(HPATH,'SURF',4)) THEN
      WRITE (NEPA,18)
	 WRITE (NEPA,20)
20     FORMAT('AQUATIC BIOTA ALSO CONSIDERED IN THIS CASE:')
       IF (AQF(1)) WRITE (NEPA,21) 'FISH'
       IF (AQF(2)) WRITE (NEPA,21) 'CRUSTACEANS'
       IF (AQF(3)) WRITE (NEPA,21) 'MOLLUSCS'
       IF (AQF(4)) WRITE (NEPA,21) 'WATER PLANTS (ALGAE)'
       WRITE (NEPA,19)
21     FORMAT (4X,A20)
      END IF
C
	IF (SEQI(HPATH,'AIR',3)) THEN
	  WRITE(NEPA,18)
	  WRITE(NEPA,22)
	END IF
C

18    FORMAT(//)
19    FORMAT (/,'**** THE FOLLOWING SECTION CONTAINS RESULTS FOR RELEA',
     .'SES TO SURFACE WATER ONLY ****',/,
     .'**** ANY AIR RESULTS ARE ELSEWHERE ****')
22    FORMAT(/,'**** THE FOLLOWING SECTION CONTAINS RESULTS FOR RELEA',
     .'SES TO AIR ONLY ****',/,
     .'**** ANY SURFACE WATER RESULTS ARE ELSEWHERE ****')
C
999   CONTINUE
      RETURN
	END