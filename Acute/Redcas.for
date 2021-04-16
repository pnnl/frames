C  REDCAS/ACUTE           Version Date:  8-Mar-07  BAN
C----------------------------------------------------------------------
C
C      SUBROUTINE REDCAS (SETDATA,NLINES,NSITE,NUMEXP)
C
C     This module reads the input data from the GID run data set
C
C     Module of Program ACUTE of the GENII/EDUP Software Package
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Last Modification: 8-Mar-07  BAN
C
C----- Parameter Descriptions ------------------------------------------------
C  SETDATA     CHAR    Argument  Data strings from GID file
C  NLINES      INT     Argument  Number of data lines in SETDATA to be used
C  NSITE       INT     Argument  Index of site to be run for GID data
C  NUMEXP      INT     Argument  Index of exposure location to be run
C-----------------------------------------------------------------------------
C  Revision History
C       8 Mar 07   BAN   Remove FRACUT, Replace with FRACFR -> FRANFR
C
C-----------------------------------------------------------------------------
C
        SUBROUTINE REDCAS (SETDATA,NLINES,NSITE,NUMEXP)
C
C---- INCLUDE STATEMENTS -----------------------------------------------------
C
      INCLUDE 'PARMTR.PAR'
      INCLUDE 'AIRPAR.CMN'
      INCLUDE 'ANMPAR.CMN'
      INCLUDE 'AQUPAR.CMN'
c      INCLUDE 'ENVPAR.CMN'
      INCLUDE 'EXTPAR.CMN'
      INCLUDE 'FODPAR.CMN'
      INCLUDE 'OPT.CMN'
      INCLUDE 'SOLPAR.CMN'
      INCLUDE 'TIMES.CMN'
C
C---- TYPE STATEMENTS --------------------------------------------------------
C
      CHARACTER*80 SETDATA(LINEMAX)
      INTEGER GETINT
      REAL GETREAL
      LOGICAL GETLOG
C
C---- Set default conditions -------------------------------------------
C
      IS=NSITE
      IE=NUMEXP
C
C---- Fraction of roots in surface soil layer --------------------------
C
      RF1 = 1.0
C
C---- Read input options and general parameters ------------------------
C
      DEBUG =  GETLOG(SETDATA,NLINES,'DEBUG         ',IS,IE,IZ,IZ,IZ,IZ)
C
C----  Pathway selection logic flags
C
      FINITE=GETLOG(SETDATA,NLINES,'FINITE        ',IZ,IZ,IZ,IZ,IZ,IZ)
      AIREXT=GETLOG(SETDATA,NLINES,'AIREXT        ',IZ,IZ,IZ,IZ,IZ,IZ)
      INHAL =GETLOG(SETDATA,NLINES,'INHAL         ',IZ,IZ,IZ,IZ,IZ,IZ)
      DRINK =GETLOG(SETDATA,NLINES,'DRINK         ',IZ,IZ,IZ,IZ,IZ,IZ)
      RECRE =GETLOG(SETDATA,NLINES,'RECRE         ',IZ,IZ,IZ,IZ,IZ,IZ)
      GROUND=GETLOG(SETDATA,NLINES,'GROUND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      AQFOOD=GETLOG(SETDATA,NLINES,'AQFOOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
      TFOOD =GETLOG(SETDATA,NLINES,'TFOOD         ',IZ,IZ,IZ,IZ,IZ,IZ)
      ANFOOD=GETLOG(SETDATA,NLINES,'ANFOOD        ',IZ,IZ,IZ,IZ,IZ,IZ)
      SHINDR=GETLOG(SETDATA,NLINES,'SHINDR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      SHDRML=GETLOG(SETDATA,NLINES,'SHDRML        ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLING =GETLOG(SETDATA,NLINES,'SLING         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLINH =GETLOG(SETDATA,NLINES,'SLINH         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SWING =GETLOG(SETDATA,NLINES,'SWING         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SHING =GETLOG(SETDATA,NLINES,'SHING         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLDRML=GETLOG(SETDATA,NLINES,'SLDRML        ',IZ,IZ,IZ,IZ,IZ,IZ)
      SWDRML=GETLOG(SETDATA,NLINES,'SWDRML        ',IZ,IZ,IZ,IZ,IZ,IZ)
      SRDRML=GETLOG(SETDATA,NLINES,'SRDRML        ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C---  Timing parameters ------------------------------------------------------
C
      NTKEND=GETREAL(SETDATA,NLINES,'NTKEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      RELEND=GETREAL(SETDATA,NLINES,'RELEND        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ACUTIM=GETREAL(SETDATA,NLINES,'ACUTIM        ',IZ,IZ,IZ,IZ,IZ,IZ)
      BEFORE=GETREAL(SETDATA,NLINES,'BEFORE        ',IZ,IZ,IZ,IZ,IZ,IZ)
      RF1   =GETREAL(SETDATA,NLINES,'RF1           ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C---- Read external exposure parameters --------------------------------------
C      
      IF (GROUND) THEN
       RESIRR=GETLOG (SETDATA,NLINES,'RESIRR        ',IZ,IZ,IZ,IZ,IZ,IZ)
       IRRSR= GETINT (SETDATA,NLINES,'IRRSR         ',IZ,IZ,IZ,IZ,IZ,IZ)
       RIRRR= GETREAL(SETDATA,NLINES,'RIRRR         ',IZ,IZ,IZ,IZ,IZ,IZ)
       IRTIMR=GETREAL(SETDATA,NLINES,'IRTIMR        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
C
C---- Read recreational exposure pathway inclusion flags ---------------------
C
      IF (RECRE) THEN
       DO IR = 1,NRE
         REC(IR)=GETLOG(SETDATA,NLINES,'REC           ',IR,IZ,IZ,IZ,
     .                  IZ,IZ)
       END DO
      ENDIF
C
C---- Read inhalation values -------------------------------------------
C
      IF (INHAL.OR.SLINH) THEN
       IRES  =GETINT (SETDATA,NLINES,'IRES          ',IZ,IZ,IZ,IZ,IZ,IZ)
       AVALSL=GETREAL(SETDATA,NLINES,'AVALSL        ',IZ,IZ,IZ,IZ,IZ,IZ)
       XMLF  =GETREAL(SETDATA,NLINES,'XMLF          ',IZ,IZ,IZ,IZ,IZ,IZ)
       RESFAC=GETREAL(SETDATA,NLINES,'RESFAC        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
C
C---- Read drinking water values -------------------------------------
C
      IF (DRINK) THEN
       DWSRC =GETINT (SETDATA,NLINES,'DWSRC         ',IZ,IZ,IZ,IZ,IZ,IZ)
       DWTRET=GETLOG (SETDATA,NLINES,'DWTRET        ',IZ,IZ,IZ,IZ,IZ,IZ)
       HOLDDW=GETREAL(SETDATA,NLINES,'HOLDDW        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ENDIF
C
C---- Read aquatic foods values -----------------------------------------
C
      IF (AQFOOD) THEN
       DO IQ = 1,NAQ
       AQF(IQ)=GETLOG(SETDATA,NLINES,'AQF           ',IQ,IZ,IZ,IZ,IZ,IZ)
       END DO
       ISALT=GETLOG(SETDATA,NLINES,'ISALT         ',IZ,IZ,IZ,IZ,IZ,IZ)
        DO IQ = 1, NAQ
          IF (AQF(IQ)) THEN
            HLDUP2(IQ)=GETREAL(SETDATA,NLINES,'HLDUP2        ',IQ,
     .                         IZ,IZ,IZ,IZ,IZ)
          ENDIF
        END DO
      ENDIF
C
C---- Read terrestrial food values -------------------------------------
C
      IF (TFOOD) THEN
       DO IT = 1,NTF
       TFD(IT)=GETLOG(SETDATA,NLINES,'TFD           ',IT,IZ,IZ,IZ,IZ,IZ)
       END DO
        DO IT = 1, NTF
          IF (TFD(IT)) THEN
            GRWP(IT)=  GETREAL(SETDATA,NLINES,'GRWP          ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
            IRRST(IT)= GETINT (SETDATA,NLINES,'IRRST         ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
            RIRR(IT)=  GETREAL(SETDATA,NLINES,'RIRR          ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
            IRTIMT(IT)=GETREAL(SETDATA,NLINES,'IRTIMT        ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
            YELD(IT)= GETREAL (SETDATA,NLINES,'YELD          ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
            HLDUP(IT)= GETREAL(SETDATA,NLINES,'HLDUP         ',IT,
     .                         IZ,IZ,IZ,IZ,IZ)
          END IF
        END DO
      ENDIF
C
C---- Read animal product values -------------------------------------
C
      IF (ANFOOD) THEN
       ANMFED=.TRUE.
C       ANMFED=GETLOG(SETDATA,NLINES,'ANMFED        ',IZ,IZ,IZ,IZ,IZ,IZ)
       DO IN = 1,NTF
       ANF(IN)=GETLOG(SETDATA,NLINES,'ANF           ',IN,IZ,IZ,IZ,IZ,IZ)
       END DO
        DO IN = 1, NAN
          IF (ANF(IN)) THEN
            HLDUPA(IN)=GETREAL(SETDATA,NLINES,'HLDUPA        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)           
            DWFACA(IN)=GETREAL(SETDATA,NLINES,'DWFACA        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            DIETFR(IN)=GETREAL(SETDATA,NLINES,'DIETFR        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            GRWPA(IN)= GETREAL(SETDATA,NLINES,'GRWPA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IRRSA(IN)= GETINT (SETDATA,NLINES,'IRRSA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            RIRRA(IN)= GETREAL(SETDATA,NLINES,'RIRRA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            IRTIMA(IN)=GETREAL(SETDATA,NLINES,'IRTIMA        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            YELDA(IN)= GETREAL(SETDATA,NLINES,'YELDA         ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
            STORTM(IN)=GETREAL(SETDATA,NLINES,'STORTM        ',IN,
     .                         IZ,IZ,IZ,IZ,IZ)
          ENDIF
        END DO
        IF (ANF(1)) THEN
          FRANFR(1)=GETREAL(SETDATA,NLINES,'FRACFR        ',1,
     .                         IZ,IZ,IZ,IZ,IZ)
          IN = 5
          DIETFR(IN)=GETREAL(SETDATA,NLINES,'DIETFR        ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          GRWPA(IN)= GETREAL(SETDATA,NLINES,'GRWPA         ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          IRRSA(IN)= GETINT (SETDATA,NLINES,'IRRSA         ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          RIRRA(IN)= GETREAL(SETDATA,NLINES,'RIRRA         ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          IRTIMA(IN)=GETREAL(SETDATA,NLINES,'IRTIMA        ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          YELDA(IN)= GETREAL(SETDATA,NLINES,'YELDA         ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          STORTM(IN)=GETREAL(SETDATA,NLINES,'STORTM        ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
        ENDIF
        IF (ANF(3)) THEN
          FRANFR(3)=GETREAL(SETDATA,NLINES,'FRACFR        ',3,
     .                         IZ,IZ,IZ,IZ,IZ)
          IN = 6
          DIETFR(IN)=GETREAL(SETDATA,NLINES,'DIETFR        ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          GRWPA(IN)= GETREAL(SETDATA,NLINES,'GRWPA         ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          IRRSA(IN)= GETINT (SETDATA,NLINES,'IRRSA         ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          RIRRA(IN)= GETREAL(SETDATA,NLINES,'RIRRA         ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          IRTIMA(IN)=GETREAL(SETDATA,NLINES,'IRTIMA        ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          YELDA(IN)= GETREAL(SETDATA,NLINES,'YELDA         ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
          STORTM(IN)=GETREAL(SETDATA,NLINES,'STORTM        ',IN,
     .                       IZ,IZ,IZ,IZ,IZ)
        ENDIF
      ENDIF
C
C---- Read default parameter values -----------------------------------
C
      IUNIT =GETINT (SETDATA,NLINES,'IUNIT         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SOLUNT=GETINT (SETDATA,NLINES,'SOLUNT        ',IZ,IZ,IZ,IZ,IZ,IZ)
      HARVST=GETLOG (SETDATA,NLINES,'HARVST        ',IZ,IZ,IZ,IZ,IZ,IZ)
      DPVRES=GETREAL(SETDATA,NLINES,'DPVRES        ',IZ,IZ,IZ,IZ,IZ,IZ)
      LEAFRS=GETREAL(SETDATA,NLINES,'LEAFRS        ',IZ,IZ,IZ,IZ,IZ,IZ)
      RAIN  =GETREAL(SETDATA,NLINES,'RAIN          ',IZ,IZ,IZ,IZ,IZ,IZ)
      DRYSET=GETLOG (SETDATA,NLINES,'DRYSET        ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(DRYSET) 
     .DEPFR1=GETREAL(SETDATA,NLINES,'DEPFR1        ',IZ,IZ,IZ,IZ,IZ,IZ)
      WETSET=GETLOG (SETDATA,NLINES,'WETSET        ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(WETSET) 
     .DEPFR2=GETREAL(SETDATA,NLINES,'DEPFR2        ',IZ,IZ,IZ,IZ,IZ,IZ)
c these need to be dimensioned by radionuclide
      ANDKRN=GETREAL(SETDATA,NLINES,'ANDKRN        ',IZ,IZ,IZ,IZ,IZ,IZ)
      ANDKR =GETREAL(SETDATA,NLINES,'ANDKR         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SURCM =GETREAL(SETDATA,NLINES,'SURCM         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SLDN  =GETREAL(SETDATA,NLINES,'SLDN          ',IZ,IZ,IZ,IZ,IZ,IZ)
      SSLDN =GETREAL(SETDATA,NLINES,'SSLDN         ',IZ,IZ,IZ,IZ,IZ,IZ)
      SEDDN =GETREAL(SETDATA,NLINES,'SEDDN         ',IZ,IZ,IZ,IZ,IZ,IZ)
      WTIM  =GETREAL(SETDATA,NLINES,'WTIM          ',IZ,IZ,IZ,IZ,IZ,IZ)
c      TCWS  =GETREAL(SETDATA,NLINES,'TCWS          ',IZ,IZ,IZ,IZ,IZ,IZ)
      ABSHUM=GETREAL(SETDATA,NLINES,'ABSHUM        ',IZ,IZ,IZ,IZ,IZ,IZ)
      DO IX = 1,4
       TRANS(IX) =GETREAL(SETDATA,NLINES,'TRANS         ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
       DWATER(IX)=GETREAL(SETDATA,NLINES,'DWATER        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
       BIOMAS(IX)=GETREAL(SETDATA,NLINES,'BIOMAS        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
       DRYFAC(IX)=GETREAL(SETDATA,NLINES,'DRYFAC        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
       SLCONA(IX)=GETREAL(SETDATA,NLINES,'SLCONA        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
      END DO
      DO IX = 1,6
       TRANSA(IX)=GETREAL(SETDATA,NLINES,'TRANSA        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
       CONSUM(IX)=GETREAL(SETDATA,NLINES,'CONSUM        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
       BIOMA2(IX)=GETREAL(SETDATA,NLINES,'BIOMA2        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
       DRYFA2(IX)=GETREAL(SETDATA,NLINES,'DRYFA2        ',IX,IZ,IZ,IZ,
     .  IZ,IZ)
        IF(IX.LE.5) NVU(IX)=GETREAL(SETDATA,NLINES,'NVU           ',IX,
     .   IZ,IZ,IZ,IZ,IZ)
      END DO
      DO IX = 1,3
       YELDBT(IX)=GETREAL(SETDATA,NLINES,'YELDBT        ',IX,IZ,IZ,IZ,
     .                    IZ,IZ)
       SVU(IX)=GETREAL(SETDATA,NLINES,'SVU           ',IX,IZ,IZ,IZ,IZ,
     .  IZ)
      END DO
C
C----- End of Input ----------------------------------------------------------
C
      RETURN
      END      
C
C---- End of Module REDCAS ---------------------------------------------------
C
