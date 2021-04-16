C  BIOTA:                 Version Date:  14-Sep-99
C----------------------------------------------------------------------
C
      SUBROUTINE REDCAS (SETDATA,NLINES)
C
C     This module reads the BITOA program input file.
C
C     Module of Program BIOTA of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 14-Sep-99
C     
C
C-----------------------------------------------------------------------
C   Date      Who   Description of Change
C  ---------  ---  -------------------------------------------------------
C  
c  15Dec09	BAN	  UI and code interchange T.bird & R.mammal...
C------------------------------------------------------------------------
C
C---- Include Statements ------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'AQUPAR.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
	INCLUDE 'BIOTDF.CMN'
C
C---- Type Statements --------------------------------------------------------
C
      INTEGER GETINT
      REAL GETREAL
      LOGICAL GETLOG, TFbird, TFmam
      CHARACTER*80 SETDATA(1000)
C
C---- Set default conditions -------------------------------------------
C
      RF1 = 1.0
C
C----  Pathway selection logic flags
C
      AIREXT=.true.
      RECRE = .TRUE.
      GROUND=.true.
      AQFOOD=GETLOG(SETDATA,NLINES,'AQFOOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
      TFOOD =GETLOG(SETDATA,NLINES,'TFOOD         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ANFOOD=GETLOG(SETDATA,NLINES,'ANFOOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C---  Timing parameters ------------------------------------------------------
C
      NTKEND=GETREAL(SETDATA,NLINES,'NTKEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      RELEND=GETREAL(SETDATA,NLINES,'RELEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BEFAIR=GETINT (SETDATA,NLINES,'BEFAIR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BEFIRR=GETINT (SETDATA,NLINES,'BEFIRR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BEFORE=GETREAL(SETDATA,NLINES,'BEFORE        ',IZ,IZ,IZ,IZ,IZ,IZ)
C      LOIC  =GETINT (SETDATA,NLINES,'LOIC          ',IZ,IZ,IZ,IZ,IZ,IZ)
c      RF1   =GETREAL(SETDATA,NLINES,'RF1           ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C---- Read external exposure values ------------------------------------
C      
      IF (GROUND) THEN
       resirr=GETLOG (SETDATA,NLINES,'RESIRR        ',IZ,IZ,IZ,IZ,IZ,IZ)
         irrsr=0
	   if (resirr) irrsr=2
       RIRRR= GETREAL(SETDATA,NLINES,'RIRRR         ',IZ,IZ,IZ,IZ,IZ,IZ)
       IRTIMR=GETREAL(SETDATA,NLINES,'IRTIMR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
C
C---- Read recreational exposure values --------------------------------
C
       if (anfood .or. aqfood) then
       DO IR = 1,3,2
           rec(ir) = .true.
       END DO
       ENDIF
C
      IF (AQFOOD) THEN
       DO IQ = 1,NAQ
       AQF(IQ)=GETLOG(SETDATA,NLINES,'AQF           ',IQ,IZ,IZ,IZ,IZ,IZ)
      VAL = GETREAL (SETDATA,NLINES,'RATIME        ',IQ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GE.0.) RATIME(IQ) = VAL
       END DO
       ISALT=GETLOG(SETDATA,NLINES,'ISALT         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
C
C---- Read terrestrial plant values -------------------------------------
C
      IF (TFOOD) THEN
       DO IT = 1,NTF
       TFD(IT)=GETLOG(SETDATA,NLINES,'TFD           ',IT,IZ,IZ,IZ,IZ,IZ)
       END DO
        DO IT = 1, NTF
c
            GRWP(IT) = 365.
            TRANS(IT) = 1.0
        END DO
      ENDIF
C
C---- Read animal critter values -------------------------------------
C
      IF (ANFOOD .or. TFOOD) THEN
       DO IN = 1,NAN
       ANF(IN)=GETLOG(SETDATA,NLINES,'ANF           ',IN,IZ,IZ,IZ,IZ,IZ)
       END DO
        DO IN = 1, NAN
            GRWPA(IN) = 365.
        END DO
      ENDIF
C
C---- Read default parameter values -----------------------------------
C
      VAL = GETREAL (SETDATA,NLINES,'DPVRES        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GE.0.) DPVRES = VAL
      VAL = GETREAL (SETDATA,NLINES,'LEAFRS        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GE.0.) LEAFRS = VAL
      RAIN  =GETREAL(SETDATA,NLINES,'RAIN          ',IZ,IZ,IZ,IZ,IZ,IZ)
      DRYSET = GETLOG(SETDATA,NLINES,'DRYSET        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(DRYSET) THEN
          VAL = GETREAL (SETDATA,NLINES,'DEPFR1        ',IZ,IZ,IZ,IZ,IZ,
     .                   IZ)
          IF(VAL.GE.0.) DEPFR1 = VAL
        ENDIF
      WETSET = GETLOG(SETDATA,NLINES,'WETSET        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(WETSET) THEN
          VAL = GETREAL (SETDATA,NLINES,'DEPFR2        ',IZ,IZ,IZ,IZ,IZ,
     .                   IZ)
          IF(VAL.GE.0.) DEPFR2 = VAL
        ENDIF
      VAL  = GETREAL(SETDATA,NLINES,'WTIM          ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GE.0.) WTIM = VAL
      SURCM =GETREAL(SETDATA,NLINES,'SURCM         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLDN  =GETREAL(SETDATA,NLINES,'SLDN          ',IZ,IZ,IZ,IZ,IZ,IZ)
      SSLDN =GETREAL(SETDATA,NLINES,'SSLDN         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SEDDN =GETREAL(SETDATA,NLINES,'SEDDN         ',IZ,IZ,IZ,IZ,IZ,IZ)
      VAL   =GETREAL(SETDATA,NLINES,'ABSHUM        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GE.0.) ABSHUM = VAL
c
c  the old plant parameters are now the 6th "animal" parameter     
c
        VAL       =GETREAL(SETDATA,NLINES,'BIOMA2        ',6,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GE.0.) BIOMAS(1) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'DRYFA2        ',6,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GE.0.) DRYFAC(1) = VAL
c
      DO IX = 1,5

       VAL       =GETREAL(SETDATA,NLINES,'DWATER        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GE.0.) DWATER(IX) = VAL
	 VAL       =GETREAL(SETDATA,NLINES,'SLCONA        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GE.0.) SLCONA(IX) = VAL
             TRANSA(IX) = 1.0
       VAL       =GETREAL(SETDATA,NLINES,'CONSUM        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GE.0.) CONSUM(IX) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'BIOMA2        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GE.0.) BIOMA2(IX) = VAL
       VAL       =GETREAL(SETDATA,NLINES,'DRYFA2        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GE.0.) DRYFA2(IX) = VAL
      END DO
c
c----- end of input ----------------------------------------------------
c
C    Short section to interchange the T.bird and R.mammal controls
      TFbird = ANF(3)
	TFmam = ANF(2)
	a1=dwater(3)
	a2=dwater(2)
	b1=slcona(3)
	b2=slcona(2)
	c1=consum(3)
	c2=consum(2)
	d1=bioma2(3)
	d2=bioma2(2)
	e1=dryfa2(3)
	e2=dryfa2(2)
	ANF(2)=TFbird
	ANF(3)=TFmam
	dwater(2)=a1
	dwater(3)=a2
	slcona(2)=b1
	slcona(3)=b2
	consum(2)=c1
	consum(3)=c2
	bioma2(2)=d1
	bioma2(3)=d2
	dryfa2(2)=e1
	dryfa2(3)=e2     


      RETURN
C
C-----------------------------------------------------------------------
      END      


