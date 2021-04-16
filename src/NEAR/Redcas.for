C  EXPOS:                 Version Date:  13-Nov-1997
C----------------------------------------------------------------------
C
      SUBROUTINE REDCAS (SETDATA,NLINES)
C
C     This module reads the ENV program input file for the EDUP Near-Field
C     module.
C
C     Module of Program ENV of the GENII Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 06-Sep-97  DLS
C     
C-----------------------------------------------------------------------
C   Date      Who   Description of Change
C  ---------  ---  -------------------------------------------------------
C  06-Sep-97  DLS  Deleted input of consumption rates
C  13-Nov-97  DLS  Added parameters BTPRE and ARID for biotic transport
C  14-SEP-99  BAN  Added parameter SLCONA for soil ingestion by animals
C------------------------------------------------------------------------
C
C---- Include Statements ------------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
c     INCLUDE 'AQUPAR.CMN'
      INCLUDE 'ENVPAR.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
      INCLUDE 'DEVICE.CMN'
C
C---- Type Statements --------------------------------------------------------
C
      INTEGER GETINT
      REAL GETREAL
      LOGICAL GETLOG
      CHARACTER*80 SETDATA(LINEMAX)
      CHARACTER*32 GETSTR
C
C---- Set default conditions -------------------------------------------
C
      RF1 = 1.0
      RF2 = 1.0
C
C---- Read input options and general parameters ------------------------
C
      DEBUG =  GETLOG(SETDATA,NLINES,'DEBUG         ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C----  Pathway selection logic flags
C
C***  INHAL =GETLOG(SETDATA,NLINES,'INHAL         ',IZ,IZ,IZ,IZ,IZ,IZ)
      GROUND=GETLOG(SETDATA,NLINES,'GROUND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      TFOOD =GETLOG(SETDATA,NLINES,'TFOOD         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ANFOOD=GETLOG(SETDATA,NLINES,'ANFOOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLING =GETLOG(SETDATA,NLINES,'SLING         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLINH =GETLOG(SETDATA,NLINES,'SLINH         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLDRML=GETLOG(SETDATA,NLINES,'SLDRML        ',IZ,IZ,IZ,IZ,IZ,IZ)
      SURSOIL=GETSTR(SETDATA,NLINES,'SURSOIL       ',IZ,IZ,IZ,IZ,IZ,IZ)
      DEPSOIL=GETSTR(SETDATA,NLINES,'DEPSOIL       ',IZ,IZ,IZ,IZ,IZ,IZ)
      WASTPAK=GETSTR(SETDATA,NLINES,'WASTPAK       ',IZ,IZ,IZ,IZ,IZ,IZ)
C     FINITE=GETLOG(SETDATA,NLINES,'FINITE        ',IZ,IZ,IZ,IZ,IZ,IZ)
C     AIREXT=GETLOG(SETDATA,NLINES,'AIREXT        ',IZ,IZ,IZ,IZ,IZ,IZ)
C     DRINK =GETLOG(SETDATA,NLINES,'DRINK         ',IZ,IZ,IZ,IZ,IZ,IZ)
C     RECRE =GETLOG(SETDATA,NLINES,'RECRE         ',IZ,IZ,IZ,IZ,IZ,IZ)
C     AQFOOD=GETLOG(SETDATA,NLINES,'AQFOOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
C     SHINDR=GETLOG(SETDATA,NLINES,'SHINDR        ',IZ,IZ,IZ,IZ,IZ,IZ)
C     SHDRML=GETLOG(SETDATA,NLINES,'SHDRML        ',IZ,IZ,IZ,IZ,IZ,IZ)
C     SWING =GETLOG(SETDATA,NLINES,'SWING         ',IZ,IZ,IZ,IZ,IZ,IZ)
C     SHING =GETLOG(SETDATA,NLINES,'SHING         ',IZ,IZ,IZ,IZ,IZ,IZ)
C     SWDRML=GETLOG(SETDATA,NLINES,'SWDRML        ',IZ,IZ,IZ,IZ,IZ,IZ)
C     SRDRML=GETLOG(SETDATA,NLINES,'SRDRML        ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C---  Timing parameters ------------------------------------------------------
C
      NTKEND=GETREAL(SETDATA,NLINES,'NTKEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      RELEND=GETREAL(SETDATA,NLINES,'RELEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BEFORE=GETREAL(SETDATA,NLINES,'BEFORE        ',IZ,IZ,IZ,IZ,IZ,IZ)
      TSOIL =GETREAL(SETDATA,NLINES,'TSOIL         ',IZ,IZ,IZ,IZ,IZ,IZ)
      LOIC  =GETINT (SETDATA,NLINES,'LOIC          ',IZ,IZ,IZ,IZ,IZ,IZ)
      RF1   =GETREAL(SETDATA,NLINES,'RF1           ',IZ,IZ,IZ,IZ,IZ,IZ)
      RF2   =GETREAL(SETDATA,NLINES,'RF2           ',IZ,IZ,IZ,IZ,IZ,IZ)
C      BEFAIR=GETINT (SETDATA,NLINES,'BEFAIR        ',IZ,IZ,IZ,IZ,IZ,IZ)
C      BEFIRR=GETINT (SETDATA,NLINES,'BEFIRR        ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C-----  Buried and deep soil parameters --------------------------------------
C
      DEEP  =GETLOG(SETDATA,NLINES,'DEEP          ',IZ,IZ,IZ,IZ,IZ,IZ)
      BURWAS=GETLOG(SETDATA,NLINES,'BURWAS        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BIOT  =GETLOG(SETDATA,NLINES,'BIOT          ',IZ,IZ,IZ,IZ,IZ,IZ)
      BTPRE =GETLOG(SETDATA,NLINES,'BTPRE         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ARID  =GETLOG(SETDATA,NLINES,'ARID          ',IZ,IZ,IZ,IZ,IZ,IZ)
      PACKHL=GETREAL(SETDATA,NLINES,'PACKHL        ',IZ,IZ,IZ,IZ,IZ,IZ)
      WASDEP=GETREAL(SETDATA,NLINES,'WASDEP        ',IZ,IZ,IZ,IZ,IZ,IZ)
      FRSIZ =GETREAL(SETDATA,NLINES,'FRSIZ         ',IZ,IZ,IZ,IZ,IZ,IZ)
      MANULR=GETREAL(SETDATA,NLINES,'MANULR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      OVRBRD=GETREAL(SETDATA,NLINES,'OVRBRD        ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C----- Check value of WASDEP for zero ----------------------------------------
C
      IF(WASDEP.LE.0.) THEN
         NERROR = NERROR + 1
         WRITE(NERR,100) WASDEP
 100     FORMAT("Error in value for WASDEP, must be >0",
     .          "  Value found = ",1pE10.2)
      ENDIF
C
C---- Read external exposure values ------------------------------------
C      
C     IF (GROUND) THEN
C      RESIRR=GETLOG (SETDATA,NLINES,'RESIRR        ',IZ,IZ,IZ,IZ,IZ,IZ)
C      IRRSR= GETINT (SETDATA,NLINES,'IRRSR         ',IZ,IZ,IZ,IZ,IZ,IZ)
C      RIRRR= GETREAL(SETDATA,NLINES,'RIRRR         ',IZ,IZ,IZ,IZ,IZ,IZ)
C      IRTIMR=GETREAL(SETDATA,NLINES,'IRTIMR        ',IZ,IZ,IZ,IZ,IZ,IZ)
C     ENDIF
C
C---- Read recreational exposure values --------------------------------
C
C     IF (RECRE) THEN
C      DO IR = 1,NRE
C        REC(IR)=GETLOG(SETDATA,NLINES,'REC           ',IR,IZ,IZ,IZ,
C    .                  IZ,IZ)
C      END DO
C     ENDIF
C
C---- Read inhalation values -------------------------------------------
C
      IF (SLINH) THEN
       IRES  =GETINT (SETDATA,NLINES,'IRES          ',IZ,IZ,IZ,IZ,IZ,IZ)
       VAL= GETREAL(SETDATA,NLINES,'AVALSL        ',IZ,IZ,IZ,IZ,IZ,IZ)
       IF(VAL.GT.0) AVALSL = VAL
       VAL =GETREAL(SETDATA,NLINES,'XMLF          ',IZ,IZ,IZ,IZ,IZ,IZ)
       IF(VAL.GT.0.) XMLF = VAL
      ENDIF
C
C---- Read drinking water values -------------------------------------
C
C     IF (DRINK) THEN
C      DWSRC =GETINT (SETDATA,NLINES,'DWSRC         ',IZ,IZ,IZ,IZ,IZ,IZ)
C      DWTRET=GETLOG (SETDATA,NLINES,'DWTRET        ',IZ,IZ,IZ,IZ,IZ,IZ)
C      HOLDDW=GETREAL(SETDATA,NLINES,'HOLDDW        ',IZ,IZ,IZ,IZ,IZ,IZ)
C     ENDIF
C
C---- Read aquatic foods values -----------------------------------------
C
C     IF (AQFOOD) THEN
C      DO IQ = 1,NAQ
C      AQF(IQ)=GETLOG(SETDATA,NLINES,'AQF           ',IQ,IZ,IZ,IZ,IZ,IZ)
C      END DO
C      ISALT=GETLOG(SETDATA,NLINES,'ISALT         ',IZ,IZ,IZ,IZ,IZ,IZ)
C       DO IQ = 1, NAQ
C         IF (AQF(IQ)) THEN
C           HLDUP2(IQ)=GETREAL(SETDATA,NLINES,'HLDUP2        ',IQ,
C    .                         IZ,IZ,IZ,IZ,IZ)
C         ENDIF
C       END DO
C     ENDIF
C
C---- Read terrestrial food values -------------------------------------
C
      IF (TFOOD) THEN
       DO IT = 1,NTF
       TFD(IT)=GETLOG(SETDATA,NLINES,'TFD           ',IT,IZ,IZ,IZ,IZ,IZ)
       END DO
        DO IT = 1, NTF
          IF (TFD(IT)) THEN
            VAL =  GETREAL(SETDATA,NLINES,'GRWP          ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) GRWP(IT) = VAL
C           IRRST(IT) = GETINT (SETDATA,NLINES,'IRRST         ',IT,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           VAL=  GETREAL(SETDATA,NLINES,'RIRR          ',IT,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           IF(VAL.GT.0.) RIRR(IT) = VAL
C           VAL = GETREAL(SETDATA,NLINES,'IRTIMT        ',IT,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           IF(VAL.GT.0.) IRTIMT(IT) = VAL
C           VAL     = GETREAL (SETDATA,NLINES,'YELD          ',IT,
C    .                         IZ,IZ,IZ,IZ,IZ)
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
C       ANMFED=GETLOG(SETDATA,NLINES,'ANMFED        ',IZ,IZ,IZ,IZ,IZ,IZ)
       DO IN = 1,NTF
       ANF(IN)=GETLOG(SETDATA,NLINES,'ANF           ',IN,IZ,IZ,IZ,IZ,IZ)
       END DO
        DO IN = 1, NAN
          IF (ANF(IN)) THEN
            VAL=GETREAL(SETDATA,NLINES,'HLDUPA        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)           
            IF(VAL.GE.0.) HLDUPA(IN) = VAL
C           VAL=GETREAL(SETDATA,NLINES,'DWFACA        ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           IF(VAL.GE.0.) DWFACA(IN) = VAL
            VAL=GETREAL(SETDATA,NLINES,'DIETFR        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GE.0.) DIETFR(IN) = VAL
            VAL = GETREAL(SETDATA,NLINES,'GRWPA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IF(VAL.GT.0.) GRWPA(IN) = VAL
C           IRRSA(IN)= GETINT (SETDATA,NLINES,'IRRSA         ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           VAL= GETREAL(SETDATA,NLINES,'RIRRA         ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           IF(VAL.GT.0.) RIRRA(IN) = VAL
C           VAL       =GETREAL(SETDATA,NLINES,'IRTIMA        ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           IF(VAL.GT.0.) IRTIMA(IN) = VAL
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
C           IRRSA(IN)= GETINT (SETDATA,NLINES,'IRRSA         ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           VAL      = GETREAL(SETDATA,NLINES,'RIRRA         ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           IF(VAL.GT.0.) RIRRA(IN) = VAL
C           VAL       =GETREAL(SETDATA,NLINES,'IRTIMA        ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           IF(VAL.GT.0.) IRTIMA(IN) = VAL
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
C           IRRSA(IN)= GETINT (SETDATA,NLINES,'IRRSA         ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           VAL      = GETREAL(SETDATA,NLINES,'RIRRA         ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           IF(VAL.GT.0.) RIRRA(IN) = VAL
C           VAL       =GETREAL(SETDATA,NLINES,'IRTIMA        ',IN,
C    .                         IZ,IZ,IZ,IZ,IZ)
C           IF(VAL.GT.0.) IRTIMA(IN) = VAL
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
c      IUNIT =GETINT (SETDATA,NLINES,'IUNIT         ',IZ,IZ,IZ,IZ,IZ,IZ)
c      SOLUNT=GETINT (SETDATA,NLINES,'SOLUNT        ',IZ,IZ,IZ,IZ,IZ,IZ)
      HARVST=GETLOG (SETDATA,NLINES,'HARVST        ',IZ,IZ,IZ,IZ,IZ,IZ)
      VAL = GETREAL (SETDATA,NLINES,'DPVRES        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) DPVRES = VAL
      VAL = GETREAL (SETDATA,NLINES,'LEAFRS        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) LEAFRS = VAL
C     RAIN  =GETREAL(SETDATA,NLINES,'RAIN          ',IZ,IZ,IZ,IZ,IZ,IZ)
      DRYSET = GETLOG(SETDATA,NLINES,'DRYSET        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(DRYSET) THEN
          VAL = GETREAL (SETDATA,NLINES,'DEPFR1        ',IZ,IZ,IZ,IZ,IZ,
     .                   IZ)
          IF(VAL.GT.0.) DEPFR1 = VAL
        ENDIF
C     WETSET = GETLOG(SETDATA,NLINES,'WETSET        ',IZ,IZ,IZ,IZ,IZ,IZ)
C       IF(WETSET) THEN
C         VAL = GETREAL (SETDATA,NLINES,'DEPFR2        ',IZ,IZ,IZ,IZ,IZ,
C    .                   IZ)
C         IF(VAL.GT.0.) DEPFR2 = VAL
C       ENDIF
      VAL  = GETREAL(SETDATA,NLINES,'WTIM          ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GT.0.) WTIM = VAL
C     ANDKRN=GETREAL(SETDATA,NLINES,'ANDKRN        ',IZ,IZ,IZ,IZ,IZ,IZ)
C     ANDKR =GETREAL(SETDATA,NLINES,'ANDKR         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SURCM =GETREAL(SETDATA,NLINES,'SURCM         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLDN  =GETREAL(SETDATA,NLINES,'SLDN          ',IZ,IZ,IZ,IZ,IZ,IZ)
      SSLDN =GETREAL(SETDATA,NLINES,'SSLDN         ',IZ,IZ,IZ,IZ,IZ,IZ)
C     SEDDN =GETREAL(SETDATA,NLINES,'SEDDN         ',IZ,IZ,IZ,IZ,IZ,IZ)
      VAL   =GETREAL(SETDATA,NLINES,'ABSHUM        ',IZ,IZ,IZ,IZ,IZ,IZ)
        IF(VAL.GE.0.) ABSHUM = VAL
      DO IX = 1,4
       VAL =GETREAL(SETDATA,NLINES,'TRANS         ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
         IF(VAL.GT.0.) TRANS(IX) = VAL
C      VAL       =GETREAL(SETDATA,NLINES,'DWATER        ',IX,IZ,IZ,IZ,
C    .  IZ,IZ)
C        IF(VAL.GT.0.) DWATER(IX) = VAL
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
C       IF(IX.LE.5) NVU(IX)=GETREAL(SETDATA,NLINES,'NVU           ',IX,
C    .   IZ,IZ,IZ,IZ,IZ)
      END DO
      DO IX = 1,3
       VAL=GETREAL(SETDATA,NLINES,'YELDBT        ',IX,IZ,IZ,IZ,
     .                    IZ,IZ)
         IF(VAL.GT.0.) YELDBT(IX) = VAL
C      SVU(IX)=GETREAL(SETDATA,NLINES,'SVU           ',IX,IZ,IZ,IZ,IZ,
C    .  IZ)
      END DO
c
c----- end of input ----------------------------------------------------
c
      RETURN
C
C-----------------------------------------------------------------------
      END      


