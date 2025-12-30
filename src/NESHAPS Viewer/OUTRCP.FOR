C   NESHAPS:     OUTRCP.FOR             Version Date: 2 Feb-03               
C   Copyright 2003 by Battelle Memorial Institute.  
C                            All rights reserved.
C*****************************************************************************
C                                                                            *
C                           SUBROUTINE OUTRCP                                *
C                                                                            *
C  Subroutine OUTRCP Writes the input RECEPTOR CONTROLS to the *.EPA file    *
C                                                                            *
C  Written by:       Bruce Napier                                            *
C                    Battelle Pacific Northwest Laboratories                 *
C                    P.O. Box 999                                            *
C                    Richland, WA 99352                                      *
C                                                                            *
C  Creation Date:    2 Feb 03     BAN                                        *
C  Last Modified:    13 Mar 03    BAN                                        *
C                                                                            *
C*****************************************************************************
C   Change history
C        13 Mar 03  BAN  Minor formatting changes
c
C*****************************************************************************
C
C---- Modular Organization --------------------------------------------------
C
C     Module of: GENII/NESHAPS
C     Called by: NESHAPS
C     Calls: NONE
C     Common blocks referenced: DATABLKS
C
      SUBROUTINE OUTRCP
      INCLUDE 'DATABLKS.CMN'
	INCLUDE 'NESHAPS.CMN'
	INCLUDE 'DATARCP.CMN'
	LOGICAL FILTER, RNMOD, GETLOG
	CHARACTER*80 GETSTR
	CHARACTER*120 SETDATA(5000)
	INTEGER NLINES, GETINT
	REAL LOWAGE(6), UPAGE(6)
C
      DIMENSION UEXAIR(6),  TEXAIR(6),  UEXGRD(6),  TEXGRD(6),
     .               FTIN(6),    FTOUT(6),
     .                    UCRP(4,6),
     .              TCRP(4,6),  UANM(4,6),  TANM(4,6),  
     .              USOIL(6),   TSOIL(6),   UINH(6),    TINH(6),
     .              UINHR(6),   FRINHR(6),  TINHR(6),   UINDRH(6),
     .              FRINDR(6),  TINDRH(6),  
     .              EQFRAC(6),  FRINH(6),  RNMOD(6),    EQF(6)
C
C==== Variable Declarations ==================================================
C
      REAL   UEXAIR,  TEXAIR,  UEXGRD,  TEXGRD,
     .       SHIN,    SHOUT,   FTIN,    FTOUT,
     .       UCRP,
     .       TCRP,    UANM,    TANM,   
     .       USOIL,   TSOIL,   UINH,    TINH,
     .       UINHR,   FRINHR,  TINHR,   UINDRH,
     .       FRINDR,  TINDRH,  EQF,  FRINH
C
	CHARACTER*10 TFNAME(4), ANMNAME(4)
      REAL BEFORE, RELEND, NTKEND
	DATA TFNAME/'LEAFY VEG','OTHER VEG','FRUIT','GRAIN'/
	DATA ANMNAME/'MEAT','POULTRY','MILK','EGGS'/
C
C----- Get data from GID file -----------------------     
C
      FILTER = .FALSE.
      CALL GETSET(NGID,NERR,rcpNAME,NLINES,SETDATA,NSITE,FILTER)
C
C----- Was the RCP data set found? -------------------------------------------
C
      IF(NLINES.LE.0) THEN
        WRITE(NERR,2211) NLINES
 2211   FORMAT(' The src data set was not found, NLINES =',I5)
        NERROR = NERROR + 1
        GO TO 999
      ENDIF
C
      WRITE (NEPA,2) rcpNAME
2     FORMAT('---- SUMMARY OF INPUT DATA FROM RECEPTOR MODULE ',A32,
     .'-------------------------')
C     .,/,'*** NOTE: FOR GENII NESHAPS VERSION,
C     . ALL PARAMETERS SHOULD BE FIXED TO EPA DEFAULTS ************')
C
      IS=1
	IZ=0
CCCC  INSERT DATA ACQUISITION LOGIC FROM GENRCP-----------------------
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
	  WRITE (NEPA,3) IA, LOWAGE(IA), UPAGE(IA)
3       FORMAT('  FOR AGE GROUP ',I1,' THE LOWER AGE LIMIT IS ',
     .         F4.1,' AND THE UPPER AGE LIMIT IS ',F4.1,' YEARS.')

        END DO
      ENDIF
C
C---- Get parameters for external exposure to air ----------------------------
C
      IF (AIREXT) THEN
        DO IA = 1,NAGES
          UEXAIR(IA) = GETREAL(SETDATA,NLINES,'UEXAIR        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TEXAIR(IA) = GETREAL(SETDATA,NLINES,'TEXAIR        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
	  WRITE(NEPA,4) IA, UEXAIR(IA), TEXAIR(IA)
4       FORMAT('  FOR AGE GROUP ',I1,' DAILY PLUME IMMERSION EXPOSURE ',
     .  'TIME IS ',F4.1,' HOURS/DAY FOR ',F5.1,' DAYS/YEAR')
        END DO
	END IF
C
C---- Get parameters for external ground exposure ----------------------------
C
      IF (GROUND) THEN
        DO IA = 1,NAGES
          UEXGRD(IA) = GETREAL(SETDATA,NLINES,'UEXGRD        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          TEXGRD(IA) = GETREAL(SETDATA,NLINES,'TEXGRD        ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          FTIN(IA)   = GETREAL(SETDATA,NLINES,'FTIN          ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
          FTOUT(IA)  = GETREAL(SETDATA,NLINES,'FTOUT         ',IA,IZ,
     .                 IZ,IZ,IZ,IZ)
	    WRITE(NEPA,5)IA, UEXGRD(IA),TEXGRD(IA),FTIN(IA),FTOUT(IA)
5         FORMAT('  FOR AGE GROUP ',I1,' DAILY SOIL EXTERNAL EXPOSURE ',
     .    'TIME IS ',F4.1,' HOURS/DAY FOR ',F5.1,' DAYS/YEAR,',/,
     .    '     WITH ',
     .    'FRACTIONS OF TIME INDOORS/OUTDOORS OF ',F5.3,'/',F5.3)
        END DO
        SHIN  = GETREAL(SETDATA,NLINES,'SHIN          ',IZ,IZ,
     .                 IZ,IZ,IZ,IZ)
        SHOUT = GETREAL(SETDATA,NLINES,'SHOUT         ',IZ,IZ,
     .                 IZ,IZ,IZ,IZ)
	  WRITE(NEPA,6)SHIN, SHOUT
6       FORMAT('     THE SHIELDING FACTORS FOR INDOOR AND OUTDOOR ',
     .         'EXPOSURE ARE ',F5.3, ' AND ',F5.3)
	END IF
C---- Get parameters for food crop ingestion pathways ------------------------
C
      IF (TFOOD) THEN
       NTF = 4
       DO IT = 1,NTF
             DO IA = 1,NAGES
	           IF (TFD(IT)) THEN
                 UCRP(IT,IA) = GETREAL(SETDATA,NLINES,'UCRP          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
                 TCRP(IT,IA) = GETREAL(SETDATA,NLINES,'TCRP          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
	           WRITE(NEPA,7)IA,TFNAME(IT),UCRP(IT,IA), TCRP(IT,IA)
7                FORMAT('  FOR AGE GROUP ',I1,' CONSUMPTION OF ',A10,
     .           ' IS ',F5.3,' KG/DAY FOR ',F5.1,' DAYS/YEAR')
	           indconsC(it,ia)=ucrp(it,ia)*tcrp(it,ia)
                 END IF 
             END DO
       END DO
	END IF
C
C---- Get parameters for animal product ingestion pathways -------------------
C
      IF (ANFOOD) THEN
       NAM = 4   
       DO IT = 1,NAM
             DO IA = 1,NAGES
	         IF (ANF(IT)) THEN
                 UANM(IT,IA) = GETREAL(SETDATA,NLINES,'UANM          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
                 TANM(IT,IA) = GETREAL(SETDATA,NLINES,'TANM          ',
     .                         IA,IT,IZ,IZ,IZ,IZ)
	           WRITE(NEPA,8)IA,ANMNAME(IT),UANM(IT,IA), TANM(IT,IA)
8                FORMAT('  FOR AGE GROUP ',I1,' CONSUMPTION OF ',A10,
     .           ' IS ',F5.3,' KG/DAY FOR ',F5.1,' DAYS/YEAR')
	           indconsA(it,ia) = uanm(it,ia)*tanm(it,ia)
	         END IF
             END DO
       END DO
	END IF
C
C---- Get parameters for soil ingestion pathway ------------------------------
C
      IF (SLING) THEN
        DO IA = 1,NAGES
          USOIL(IA) = GETREAL(SETDATA,NLINES,'USOIL         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TSOIL(IA) = GETREAL(SETDATA,NLINES,'TSOIL         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
	    WRITE(NEPA,9) IA, USOIL(IA), TSOIL(IA)
9         FORMAT('  FOR AGE GROUP ',I1,' CONSUMPTION OF SOIL',
     .    ' IS ',1PE10.2,' MILLIGRAMS/DAY FOR ',0pF5.1,' DAYS/YEAR')
        END DO
	END IF
C
C---- Get parameters for inhalation of air pathway ---------------------------
C
      IF (INHAL) THEN
        DO IA = 1,NAGES
          UINH(IA) = GETREAL(SETDATA,NLINES,'UINH          ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TINH(IA) = GETREAL(SETDATA,NLINES,'TINH          ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          FRINH(IA) = GETREAL(SETDATA,NLINES,'FRINH         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
	    WRITE(NEPA,10)IA, UINH(IA),TINH(IA),FRINH(IA)
10        FORMAT('  FOR AGE GROUP ',I1,' OUTDOOR INHALATION RATE IS ',
     .    F5.2,' M3/DAY FOR ',F5.1,' DAYS/YEAR,',/,'     WITH A FRACTION
     .',    ' OF A DAY OUTDOORS OF ',F5.3)
        END DO
	END IF
C
C---- Get parameters for inhalation of resuspended soil pathway --------------
C
      IF (IRES .NE. 0) THEN
        DO IA = 1,NAGES
          UINHR(IA) = GETREAL(SETDATA,NLINES,'UINHR         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          TINHR(IA) = GETREAL(SETDATA,NLINES,'TINHR         ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
          FRINHR(IA) = GETREAL(SETDATA,NLINES,'FRINHR        ',
     .                   IA,IZ,IZ,IZ,IZ,IZ)
	    WRITE(NEPA,11)IA, UINHR(IA),TINHR(IA),FRINHR(IA)
11        FORMAT('  FOR AGE GROUP ',I1,' INHALATION RATE FOR RESUSPENDED
     . MATERIAL IS ',
     .    F5.2,' M3/DAY FOR ',F5.1,' DAYS/YEAR,',/,'     WITH A FRACTION
     .',    ' OF A DAY EXPOSED TO RESUSPENSION OF ',F5.3)
        END DO
	END IF
C
C---- Get parameters for inhalation of indoor air pathway --------------------
C
      IF (SHINDR) THEN
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
	    WRITE(NEPA,12)IA, UINDRH(IA),TINDRH(IA),FRINDR(IA)
12        FORMAT('  FOR AGE GROUP ',I1,' INDOOR INHALATION RATE IS ',
     .    F5.2,' M3/DAY FOR ',F5.1,' DAYS/YEAR,',/,'     WITH A FRACTION
     .',    ' OF A DAY INDOORS OF ',F5.3)
	    IF (RNMOD(IA)) THEN
	    WRITE(NEPA,13)EQFRAC(IA)
13        FORMAT('  THE RADON PROGENY EQUILIBRIUM FRACTION IS ',F5.3)
          END IF
        END DO
	END IF

C
C----- end of input ----------------------------------------------------------
C
C
999   CONTINUE
      RETURN
	END