C  INTAKE:                Version Date:  21-JuL-1997
C----------------------------------------------------------------------
C
      SUBROUTINE REDCAS (SETDATA,NLINES)
C
C     This module reads the receptor input data set from the GID file.
C     Pacific Northwest Laboratory Environmental Dosimetry System 
C
C     Creation Date:     09-Jun-97  DL Strenge
C     Last Modification: 11-Oct-01  BA Napier
C-----------------------------------------------------------------------     
C     10/11/01  BAN  COnverted to Lower and Upper bound ages
C     19 Dec 2001 BAN  Added Rn model reads
C     16 Sept 2002 BAN  Added FRINH, for radon
C-----------------------------------------------------------------------
C
C  SETDATA(1000)  REAL U  Array of input images from the GID file
C  NLINES         INT  U  Number of lines in SETDATA
C
C------------------------------------------------------------------------
C
      INCLUDE 'AGES.CMN'
      INCLUDE 'ALLPAR.CMN'
      INCLUDE 'DEVICE.CMN'
      INCLUDE 'OPT.CMN'
C
      INTEGER GETINT
      REAL GETREAL
      LOGICAL GETLOG, RNmod
      CHARACTER*80 SETDATA(1000)
	DIMENSION EQf(6), RNmod(6)
C
C---- Set default conditions -------------------------------------------
C     Far-field root fractions (surface)  
C
      RF1 = 1.0
C
C---- Read input options and general parameters ------------------------
C
      DEBUG =  GETLOG(SETDATA,NLINES,'DEBUG         ',IZ,IZ,IZ,IZ,IZ,IZ)
C
C---- Read age group information ---------------------------------------------
C
      NAGES = GETINT(SETDATA,NLINES,'NAGES         ',IZ,IZ,IZ,IZ,IZ,IZ)
      IF(NAGES.LE.0.OR.NAGES.GT.6) THEN
        WRITE(NERR,100) NAGES
 100    FORMAT(' Error in specification of number of age groups. '/
     .         '   Value input = ',I5,',  valid range is 1 to 6.')
        NERROR = NERROR + 1
        NAGES = 1
      ELSE
        DO IA = 1,NAGES
           LOWAGE(IA) = GETREAL(SETDATA,NLINES,'LOWAGE         ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          UPAGE(IA) = GETREAL(SETDATA,NLINES,'UPAGE         ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
        END DO
      ENDIF
C
C---- Get parameters for external exposure to air ----------------------------
C
        DO IA = 1,NAGES
          UEXAIR(IA) = GETREAL(SETDATA,NLINES,'UEXAIR        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TEXAIR(IA) = GETREAL(SETDATA,NLINES,'TEXAIR        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for external ground exposure ----------------------------
C
        DO IA = 1,NAGES
          UEXGRD(IA) = GETREAL(SETDATA,NLINES,'UEXGRD        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TEXGRD(IA) = GETREAL(SETDATA,NLINES,'TEXGRD        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          FTIN(IA)   = GETREAL(SETDATA,NLINES,'FTIN          ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          FTOUT(IA)  = GETREAL(SETDATA,NLINES,'FTOUT         ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
        END DO
        SHIN  = GETREAL(SETDATA,NLINES,'SHIN          ',IZ,IZ,
     .                 IZ,IZ,IZ,IZ)
        SHOUT = GETREAL(SETDATA,NLINES,'SHOUT         ',IZ,IZ,
     .                 IZ,IZ,IZ,IZ)

C---- Get parameters for external exposure while swimming --------------------
C
        DO IA = 1,NAGES
          EVSWIM(IA) = GETREAL(SETDATA,NLINES,'EVSWIM        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TESWIM(IA) = GETREAL(SETDATA,NLINES,'TESWIM        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TSWIM(IA)  = GETREAL(SETDATA,NLINES,'TSWIM         ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for external exposure while boating ---------------------
C
        DO IA = 1,NAGES
          EVBOAT(IA) = GETREAL(SETDATA,NLINES,'EVBOAT        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TEBOAT(IA) = GETREAL(SETDATA,NLINES,'TEBOAT        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TBOAT(IA)  = GETREAL(SETDATA,NLINES,'TBOAT         ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
        END DO
        SFBOAT = GETREAL(SETDATA,NLINES,'SFBOAT        ',IZ,IZ,
     .                 IZ,IZ,IZ,IZ)
C
C---- Get parameters for external exposure to shoreline ----------------------
C
        DO IA = 1,NAGES
          EVSHOR(IA) = GETREAL(SETDATA,NLINES,'EVSHOR        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TESHOR(IA) = GETREAL(SETDATA,NLINES,'TESHOR        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TSHOR(IA)  = GETREAL(SETDATA,NLINES,'TSHOR         ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
        END DO
        SWFAC  = GETREAL(SETDATA,NLINES,'SWFAC         ',IZ,IZ,
     .                 IZ,IZ,IZ,IZ)
C
C---- Get parameters for food crop ingestion pathways ------------------------
C
       NTF = 4
       DO IT = 1,NTF
             DO IA = 1,NAGES
                 UCRP(IT,IA) = GETREAL(SETDATA,NLINES,'UCRP          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
                 TCRP(IT,IA) = GETREAL(SETDATA,NLINES,'TCRP          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
             END DO
       END DO
C
C---- Get parameters for animal product ingestion pathways -------------------
C
       NAM = 4   
       DO IT = 1,NAM
             DO IA = 1,NAGES
                 UANM(IT,IA) = GETREAL(SETDATA,NLINES,'UANM          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
                 TANM(IT,IA) = GETREAL(SETDATA,NLINES,'TANM          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
             END DO
       END DO
C
C---- Get parameters for aquatic food ingestion pathways ---------------------
C
       NAQ = 4   
       DO IT = 1,NAQ
             DO IA = 1,NAGES
                 UAQU(IT,IA) = GETREAL(SETDATA,NLINES,'UAQU          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
                 TAQU(IT,IA) = GETREAL(SETDATA,NLINES,'TAQU          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
             END DO
       END DO
C
C---- Get parameters for drinking water ingestion pathway --------------------
C
        DO IA = 1,NAGES
          UDW(IA) = GETREAL(SETDATA,NLINES,'UDW           ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TDW(IA) = GETREAL(SETDATA,NLINES,'TDW           ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for inadvertent ingestion while swimming ---------------
C
        DO IA = 1,NAGES
          USWIM(IA) = GETREAL(SETDATA,NLINES,'USWIM         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          EVSWIM(IA) = GETREAL(SETDATA,NLINES,'EVSWIM        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TESWIM(IA) = GETREAL(SETDATA,NLINES,'TESWIM        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TSWIM(IA) = GETREAL(SETDATA,NLINES,'TSWIM         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for inadvertent water ingestion while showering ---------
C
        DO IA = 1,NAGES
          USHIN(IA) = GETREAL(SETDATA,NLINES,'USHIN         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          EVSHWR(IA) = GETREAL(SETDATA,NLINES,'EVSHWR        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TESHWR(IA) = GETREAL(SETDATA,NLINES,'TESHWR        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TSHWR(IA) = GETREAL(SETDATA,NLINES,'TSHWR         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for soil ingestion pathway ------------------------------
C
        DO IA = 1,NAGES
          USOIL(IA) = GETREAL(SETDATA,NLINES,'USOIL         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TSOIL(IA) = GETREAL(SETDATA,NLINES,'TSOIL         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for inhalation of air pathway ---------------------------
C
        DO IA = 1,NAGES
          UINH(IA) = GETREAL(SETDATA,NLINES,'UINH          ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TINH(IA) = GETREAL(SETDATA,NLINES,'TINH          ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          FRINH(IA) = GETREAL(SETDATA,NLINES,'FRINH         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for inhalation of resuspended soil pathway --------------
C
        DO IA = 1,NAGES
          UINHR(IA) = GETREAL(SETDATA,NLINES,'UINHR         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TINHR(IA) = GETREAL(SETDATA,NLINES,'TINHR         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          FRINHR(IA) = GETREAL(SETDATA,NLINES,'FRINHR        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for inhalation of indoor air pathway --------------------
C
        DO IA = 1,NAGES
          UINDRH(IA) = GETREAL(SETDATA,NLINES,'UINDRH        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TINDRH(IA) = GETREAL(SETDATA,NLINES,'TINDRH        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          FRINDR(IA) = GETREAL(SETDATA,NLINES,'FRINDR        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
	    RNmod(IA) = GETLOG(SETDATA, NLINES,'RNMODL        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          EQf(IA) = GETREAL(SETDATA,NLINES,'EQFRAC        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
	    EQFRAC(IA) = 1.0
	    IF (RNmod(IA)) EQFRAC(IA) = EQf(IA)
        END DO
C
C---- Get parameters for dermal contact while showering pathway --------------
C
        DO IA = 1,NAGES
          TESHWR(IA) = GETREAL(SETDATA,NLINES,'TESHWR        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          EVSHWR(IA) = GETREAL(SETDATA,NLINES,'EVSHWR        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TSHWR(IA) = GETREAL(SETDATA,NLINES,'TSHWR         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          ASKINSH(IA) = GETREAL(SETDATA,NLINES,'ASKINSH       ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for dermal contact while swimming pathway ---------------
C
        DO IA = 1,NAGES
          TESWIM(IA) =  GETREAL(SETDATA,NLINES,'TESWIM        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          EVSWIM(IA) =  GETREAL(SETDATA,NLINES,'EVSWIM        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TSWIM(IA) =   GETREAL(SETDATA,NLINES,'TSWIM         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          ASKINSW(IA) = GETREAL(SETDATA,NLINES,'ASKINSW       ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for dermal contact with soil pathway --------------------
C
        DO IA = 1,NAGES
          ADHSOL(IA) = GETREAL(SETDATA,NLINES,'ADHSOL        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          EVSOIL(IA) = GETREAL(SETDATA,NLINES,'EVSOIL        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TSOILD(IA) = GETREAL(SETDATA,NLINES,'TSOILD        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          ASKINSL(IA) = GETREAL(SETDATA,NLINES,'ASKINSL       ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C---- Get parameters for dermal contact with sediment pathway ----------------
C
        DO IA = 1,NAGES
          ADHSED(IA) = GETREAL(SETDATA,NLINES,'ADHSED        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          ASKINSD(IA) = GETREAL(SETDATA,NLINES,'ASKINSD       ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
        END DO
C
C----- end of input ----------------------------------------------------------
C
      RETURN
C
C---- End of Module REDCAS ---------------------------------------------------
C
      END

